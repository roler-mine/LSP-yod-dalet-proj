(* bin/Main.ml *)

module T = Lsp.Types
module Handlers = Jovial_lsp_lib.Handlers
module Analyze = Jovial_lsp_lib.Analyze

let params_yojson (req : Jsonrpc.Request.t) : Yojson.Safe.t =
  match req.params with
  | None -> `Assoc []
  | Some p -> (Jsonrpc.Structured.yojson_of_t p :> Yojson.Safe.t)

let notif_params_yojson (n : Jsonrpc.Notification.t) : Yojson.Safe.t =
  match n.params with
  | None -> `Assoc []
  | Some p -> (Jsonrpc.Structured.yojson_of_t p :> Yojson.Safe.t)

(* ---------------- Logging (stderr only) ---------------- *)
module Log = struct
  let info fmt =
    Printf.ksprintf
      (fun s ->
        prerr_endline ("[jovial-lsp] " ^ s);
        flush stderr)
      fmt
end

(* ---------------- LSP stdio framing ---------------- *)
module Stdio = struct
  exception Protocol_error of string

  let strip_trailing_cr (s : string) =
    let n = String.length s in
    if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

  let read_headers (ic : in_channel) : (string * string) list =
    let rec loop acc =
      match input_line ic with
      | exception End_of_file -> raise End_of_file
      | line ->
          let line = strip_trailing_cr line in
          if line = "" then List.rev acc
          else
            match String.index_opt line ':' with
            | None -> raise (Protocol_error ("bad header line: " ^ line))
            | Some i ->
                let k = String.sub line 0 i |> String.lowercase_ascii |> String.trim in
                let v =
                  String.sub line (i + 1) (String.length line - i - 1)
                  |> String.trim
                in
                loop ((k, v) :: acc)
    in
    loop []

  let content_length (headers : (string * string) list) : int =
    match List.assoc_opt "content-length" headers with
    | None -> raise (Protocol_error "missing Content-Length header")
    | Some v ->
        (try int_of_string v with _ -> raise (Protocol_error ("bad Content-Length: " ^ v)))

  let read_exactly (ic : in_channel) (n : int) : string =
    really_input_string ic n

  let read_packet (ic : in_channel) : Jsonrpc.Packet.t =
    let headers = read_headers ic in
    let len = content_length headers in
    let body = read_exactly ic len in
    let json = Yojson.Safe.from_string body in
    Jsonrpc.Packet.t_of_yojson json

  let write_packet (oc : out_channel) (pkt : Jsonrpc.Packet.t) : unit =
    let body = Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t pkt) in
    output_string oc (Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body));
    output_string oc body;
    flush oc
end

(* ---------------- State ---------------- *)
type state =
  { handlers : Handlers.t
  ; mutable initialized : bool
  ; mutable shutdown_requested : bool
  ; mutable roots : string list
  }

(* ---------------- Response helpers ---------------- *)
let send_server_notification (oc : out_channel) (n : Lsp.Server_notification.t) : unit =
  let notif = Lsp.Server_notification.to_jsonrpc n in
  Stdio.write_packet oc (Jsonrpc.Packet.Notification notif)

let respond_ok (oc : out_channel) ~(id : Jsonrpc.Id.t) ~(result_json : Jsonrpc.Json.t) : unit =
  let resp = Jsonrpc.Response.ok id result_json in
  Stdio.write_packet oc (Jsonrpc.Packet.Response resp)

let respond_error
    (oc : out_channel)
    ~(id : Jsonrpc.Id.t)
    ~(code : Jsonrpc.Response.Error.Code.t)
    ~(message : string)
  : unit =
  let err = Jsonrpc.Response.Error.make ~code ~message () in
  let resp = Jsonrpc.Response.error id err in
  Stdio.write_packet oc (Jsonrpc.Packet.Response resp)

let server_not_initialized (oc : out_channel) (req : Jsonrpc.Request.t) : unit =
  respond_error oc ~id:req.id ~code:Jsonrpc.Response.Error.Code.ServerNotInitialized
    ~message:"Server not initialized"

(* ---------------- workspace roots from initialize ---------------- *)
let roots_from_initialize (req : Jsonrpc.Request.t) : string list =
  let open Yojson.Safe.Util in
  let p = params_yojson req in
  (* prefer workspaceFolders, fall back to rootUri *)
  let wf =
    match p |> member "workspaceFolders" with
    | `Null -> []
    | `List xs ->
        xs
        |> List.filter_map (fun j ->
               match j |> member "uri" with
               | `String u -> Some u
               | _ -> None)
    | _ -> []
  in
  if wf <> [] then wf
  else
    match p |> member "rootUri" with
    | `String u -> [ u ]
    | _ -> []

(* ---------------- Custom request: jovial/parse ---------------- *)
let handle_custom_request (oc : out_channel) (req : Jsonrpc.Request.t) : bool =
  match req.method_ with
  | "jovial/parse" ->
      let open Yojson.Safe.Util in
      let params = params_yojson req in
      let uri_str =
        match params |> member "uri" with
        | `String s -> s
        | _ -> "inmemory:///parse.jovial"
      in
      let text =
        match params |> member "text" with
        | `String s -> s
        | _ ->
            respond_error oc ~id:req.id
              ~code:Jsonrpc.Response.Error.Code.InvalidParams
              ~message:"jovial/parse expects params { uri?: string, text: string }";
            ""
      in
      if text = "" then true
      else (
        let uri = Lsp.Uri.of_string uri_str in
        let a = Analyze.analyze ~uri ~text in
        let diags : Yojson.Safe.t =
          `List (List.map T.Diagnostic.yojson_of_t a.diagnostics)
        in
        let result =
          `Assoc [ ("status", `String "ok"); ("diagnostics", diags) ]
        in
        respond_ok oc ~id:req.id ~result_json:result;
        true
      )
  | _ -> false

(* ---------------- LSP request handling ---------------- *)
let handle_request (st : state) (oc : out_channel) (req : Jsonrpc.Request.t) : unit =
  if (not st.initialized) && not (String.equal req.method_ "initialize") then
    server_not_initialized oc req
  else
    match Lsp.Client_request.of_jsonrpc req with
    | Ok (Lsp.Client_request.E r) -> (
        match r with
        | Lsp.Client_request.Initialize _params ->
            st.initialized <- true;

            (* Capture workspace roots for cross-file index *)
            let roots = roots_from_initialize req in
            st.roots <- roots;
            (try Handlers.set_workspace_roots st.handlers roots with _ -> ());

            let caps =
              T.ServerCapabilities.create
                ~textDocumentSync:(`TextDocumentSyncKind T.TextDocumentSyncKind.Incremental)

                (* navigation + UI *)
                ~hoverProvider:(`Bool true)
                ~definitionProvider:(`Bool true)
                ~implementationProvider:(`Bool true)
                ~referencesProvider:(`Bool true)
                ~documentSymbolProvider:(`Bool true)
                ~workspaceSymbolProvider:(`Bool true)
                ~renameProvider:(`Bool true)

                (* goodies *)
                ~completionProvider:(T.CompletionOptions.create ())
                ~codeActionProvider:(`Bool true)
                ~inlayHintProvider:(`Bool true)
                ()
            in
            let init_res = T.InitializeResult.create ~capabilities:caps () in
            respond_ok oc ~id:req.id ~result_json:(Lsp.Client_request.yojson_of_result r init_res)

        | Lsp.Client_request.Shutdown ->
            st.shutdown_requested <- true;
            respond_ok oc ~id:req.id ~result_json:(Lsp.Client_request.yojson_of_result r ())

        | Lsp.Client_request.TextDocumentHover p ->
            let res = Handlers.hover st.handlers p in
            respond_ok oc ~id:req.id ~result_json:(Lsp.Client_request.yojson_of_result r res)

        | Lsp.Client_request.TextDocumentDefinition p ->
            let res = Handlers.definition st.handlers p in
            respond_ok oc ~id:req.id ~result_json:(Lsp.Client_request.yojson_of_result r res)

        | Lsp.Client_request.DocumentSymbol p ->
            let res = Handlers.document_symbols st.handlers p in
            respond_ok oc ~id:req.id ~result_json:(Lsp.Client_request.yojson_of_result r res)

        | Lsp.Client_request.TextDocumentCompletion p ->
            let res = Handlers.completion st.handlers p in
            respond_ok oc ~id:req.id ~result_json:(Lsp.Client_request.yojson_of_result r res)

        (* ---- Extended features: we route by method string too, because some
           LSP method variants may not be present in the Client_request sum
           depending on the exact generated bindings. ---- *)
        | _ -> (
            match req.method_ with
            | "textDocument/references" ->
                (try
                   let p = T.ReferenceParams.t_of_yojson (params_yojson req) in
                   let res = Handlers.references st.handlers p in
                   let json =
                     match res with
                     | None -> `Null
                     | Some locs -> `List (List.map T.Location.yojson_of_t locs)
                   in
                   respond_ok oc ~id:req.id ~result_json:json
                 with exn ->
                   respond_error oc ~id:req.id
                     ~code:Jsonrpc.Response.Error.Code.InternalError
                     ~message:(Printexc.to_string exn)
                )

            | "textDocument/implementation" ->
                (try
                   (* ImplementationParams is structurally like DefinitionParams in practice. *)
                   let p = T.DefinitionParams.t_of_yojson (params_yojson req) in
                   let res = Handlers.implementation st.handlers p in
                   let json =
                     match res with
                     | None -> `Null
                     | Some locs -> T.Locations.yojson_of_t locs
                   in
                   respond_ok oc ~id:req.id ~result_json:json
                 with exn ->
                   respond_error oc ~id:req.id
                     ~code:Jsonrpc.Response.Error.Code.InternalError
                     ~message:(Printexc.to_string exn)
                )

            | "workspace/symbol" ->
                (try
                   let p = T.WorkspaceSymbolParams.t_of_yojson (params_yojson req) in
                   let res = Handlers.workspace_symbol st.handlers p in
                   let json =
                     match res with
                     | None -> `Null
                     | Some syms -> `List (List.map T.SymbolInformation.yojson_of_t syms)
                   in
                   respond_ok oc ~id:req.id ~result_json:json
                 with exn ->
                   respond_error oc ~id:req.id
                     ~code:Jsonrpc.Response.Error.Code.InternalError
                     ~message:(Printexc.to_string exn)
                )

            | "textDocument/prepareRename" ->
                (try
                   let p = T.PrepareRenameParams.t_of_yojson (params_yojson req) in
                   let res = Handlers.prepare_rename st.handlers p in
                   let json =
                     match res with
                     | None -> `Null
                     | Some pr -> T.PrepareRenameResult.yojson_of_t pr
                   in
                   respond_ok oc ~id:req.id ~result_json:json
                 with exn ->
                   respond_error oc ~id:req.id
                     ~code:Jsonrpc.Response.Error.Code.InternalError
                     ~message:(Printexc.to_string exn)
                )

            | "textDocument/rename" ->
                (try
                   let p = T.RenameParams.t_of_yojson (params_yojson req) in
                   let res = Handlers.rename st.handlers p in
                   let json =
                     match res with
                     | None -> `Null
                     | Some we -> T.WorkspaceEdit.yojson_of_t we
                   in
                   respond_ok oc ~id:req.id ~result_json:json
                 with exn ->
                   respond_error oc ~id:req.id
                     ~code:Jsonrpc.Response.Error.Code.InternalError
                     ~message:(Printexc.to_string exn)
                )

            | "textDocument/inlayHint" ->
                (try
                   let p = T.InlayHintParams.t_of_yojson (params_yojson req) in
                   let res = Handlers.inlay_hints st.handlers p in
                   let json =
                     match res with
                     | None -> `Null
                     | Some hs -> `List (List.map T.InlayHint.yojson_of_t hs)
                   in
                   respond_ok oc ~id:req.id ~result_json:json
                 with exn ->
                   respond_error oc ~id:req.id
                     ~code:Jsonrpc.Response.Error.Code.InternalError
                     ~message:(Printexc.to_string exn)
                )

            | "textDocument/codeAction" ->
                (try
                   let p = T.CodeActionParams.t_of_yojson (params_yojson req) in
                   let res = Handlers.code_actions st.handlers p in
                   let json =
                     match res with
                     | None -> `Null
                     | Some items ->
                         `List
                           (List.map
                              (function
                                | `Command c -> T.Command.yojson_of_t c
                                | `CodeAction a -> T.CodeAction.yojson_of_t a)
                              items)
                   in
                   respond_ok oc ~id:req.id ~result_json:json
                 with exn ->
                   respond_error oc ~id:req.id
                     ~code:Jsonrpc.Response.Error.Code.InternalError
                     ~message:(Printexc.to_string exn)
                )

            | _ ->
                (* Not a built-in request we handle here: try custom methods. *)
                if not (handle_custom_request oc req) then
                  respond_error oc ~id:req.id ~code:Jsonrpc.Response.Error.Code.MethodNotFound
                    ~message:("Method not implemented: " ^ req.method_)
          )
      )
    | Error _msg ->
        if not (handle_custom_request oc req) then
          respond_error oc ~id:req.id ~code:Jsonrpc.Response.Error.Code.MethodNotFound
            ~message:("Unknown method: " ^ req.method_)

(* ---------------- LSP notifications ---------------- *)
let handle_notification (st : state) (oc : out_channel) (n : Jsonrpc.Notification.t) : unit =
  (* handle workspace folder changes even if bindings don't expose a variant *)
  if String.equal n.method_ "workspace/didChangeWorkspaceFolders" then (
    (* safest move: re-read all roots isn’t available in delta notification;
       we’ll just trigger a reindex on current roots. *)
    (try Handlers.set_workspace_roots st.handlers st.roots with _ -> ());
    ()
  ) else
    match Lsp.Client_notification.of_jsonrpc n with
    | Error _ ->
        ()
    | Ok notif ->
        if not st.initialized then (
          match notif with
          | Lsp.Client_notification.Exit ->
              if st.shutdown_requested then exit 0 else exit 1
          | _ -> ()
        ) else (
          match notif with
          | Lsp.Client_notification.Initialized -> ()
          | Lsp.Client_notification.Exit ->
              if st.shutdown_requested then exit 0 else exit 1

          | Lsp.Client_notification.TextDocumentDidOpen p ->
              let pd = Handlers.did_open st.handlers p in
              send_server_notification oc (Lsp.Server_notification.PublishDiagnostics pd)

          | Lsp.Client_notification.TextDocumentDidChange p ->
              let pd = Handlers.did_change st.handlers p in
              send_server_notification oc (Lsp.Server_notification.PublishDiagnostics pd)

          | Lsp.Client_notification.TextDocumentDidClose p ->
              let pd = Handlers.did_close st.handlers p in
              send_server_notification oc (Lsp.Server_notification.PublishDiagnostics pd)

          | _ -> ()
        )

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug)

(* ---------------- Main loop ---------------- *)
let rec loop (st : state) (ic : in_channel) (oc : out_channel) : unit =
  match Stdio.read_packet ic with
  | exception End_of_file ->
      Log.info "stdin closed; exiting"
  | exception Stdio.Protocol_error msg ->
      Log.info "protocol error: %s" msg
  | exception exn ->
      Log.info "read error: %s" (Printexc.to_string exn)
  | pkt ->
      (try
         match pkt with
         | Jsonrpc.Packet.Request req -> handle_request st oc req
         | Jsonrpc.Packet.Notification n -> handle_notification st oc n
         | Jsonrpc.Packet.Batch_call calls ->
             List.iter
               (function
                 | `Request r -> handle_request st oc r
                 | `Notification n -> handle_notification st oc n)
               calls
         | Jsonrpc.Packet.Response _ -> ()
         | Jsonrpc.Packet.Batch_response _ -> ()
       with exn ->
         Log.info "handler crash: %s" (Printexc.to_string exn));
      loop st ic oc

let () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;

  Printexc.record_backtrace true;
  Log.info "starting (stdio)";
  let st =
    { handlers = Handlers.create ()
    ; initialized = false
    ; shutdown_requested = false
    ; roots = []
    }
  in
  loop st stdin stdout
