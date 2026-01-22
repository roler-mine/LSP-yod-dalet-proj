(* bin/Main.ml *)

module T = Lsp.Types
module Handlers = Jovial_lsp_lib.Handlers
module Analyze = Jovial_lsp_lib.Analyze


let params_yojson (req : Jsonrpc.Request.t) : Yojson.Safe.t =
  match req.params with
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

(* ---------------- LSP stdio framing ----------------
   LSP uses JSON-RPC messages framed with Content-Length headers. :contentReference[oaicite:6]{index=6}
*)
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
            let v = String.sub line (i + 1) (String.length line - i - 1) |> String.trim in
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
  (* LSP says: if request received before initialize -> error -32002. :contentReference[oaicite:7]{index=7} *)
  respond_error oc ~id:req.id ~code:Jsonrpc.Response.Error.Code.ServerNotInitialized
    ~message:"Server not initialized"

(* ---------------- Custom request: jovial/parse ---------------- *)
let diag_message_to_string (m : [ `String of string | `MarkupContent of T.MarkupContent.t ]) : string =
  match m with
  | `String s -> s
  | `MarkupContent mc -> mc.value

let yojson_of_position (p : T.Position.t) =
  `Assoc [ ("line", `Int p.line); ("character", `Int p.character) ]

let yojson_of_range (r : T.Range.t) =
  `Assoc [ ("start", yojson_of_position r.start); ("end", yojson_of_position r.end_) ]

let yojson_of_severity (s : T.DiagnosticSeverity.t) =
  (* LSP severity is 1..4 *)
  match s with
  | T.DiagnosticSeverity.Error -> `Int 1
  | T.DiagnosticSeverity.Warning -> `Int 2
  | T.DiagnosticSeverity.Information -> `Int 3
  | T.DiagnosticSeverity.Hint -> `Int 4

  let handle_custom_request (oc : out_channel) (req : Jsonrpc.Request.t) : bool =
  match req.method_ with
  | "jovial/parse" ->
      let open Yojson.Safe.Util in
      let params = params_yojson (req) in

      let uri_str =
        match params |> member "uri" with
        | `String s -> s
        | _ -> "inmemory:///parse.jovial"
      in

      let text =
        match params |> member "text" with
        | `String s -> s
        | _ ->
            respond_error oc
              ~id:req.id
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
  (* Enforce init gating per spec. :contentReference[oaicite:8]{index=8} *)
  if (not st.initialized) && not (String.equal req.method_ "initialize") then (
    server_not_initialized oc req
  ) else (
    match Lsp.Client_request.of_jsonrpc req with
    | Ok (Lsp.Client_request.E r) -> (
        match r with
        | Lsp.Client_request.Initialize _params ->
          st.initialized <- true;

          let caps =
            T.ServerCapabilities.create
              ~textDocumentSync:(`TextDocumentSyncKind T.TextDocumentSyncKind.Incremental)
              ~hoverProvider:(`Bool true)
              ~definitionProvider:(`Bool true)
              ~documentSymbolProvider:(`Bool true)
              ~completionProvider:(T.CompletionOptions.create ())
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

        | _ ->
          respond_error oc ~id:req.id ~code:Jsonrpc.Response.Error.Code.MethodNotFound
            ~message:"Method not implemented"
      )
    | Error _msg ->
      (* Not a built-in LSP request: try custom methods. *)
      if not (handle_custom_request oc req) then
        respond_error oc ~id:req.id ~code:Jsonrpc.Response.Error.Code.MethodNotFound
          ~message:("Unknown method: " ^ req.method_)
  )

(* ---------------- LSP notifications ---------------- *)
let handle_notification (st : state) (oc : out_channel) (n : Jsonrpc.Notification.t) : unit =
  match Lsp.Client_notification.of_jsonrpc n with
  | Error _ ->
    (* Unknown notification: ignore *)
    ()
  | Ok notif ->
    (* Per spec: notifications before initialize are dropped (except exit). :contentReference[oaicite:9]{index=9} *)
    if (not st.initialized) then (
      match notif with
      | Lsp.Client_notification.Exit -> if st.shutdown_requested then exit 0 else exit 1
      | _ -> ()
    ) else (
      match notif with
      | Lsp.Client_notification.Initialized -> ()
      | Lsp.Client_notification.Exit -> if st.shutdown_requested then exit 0 else exit 1

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
  (* Avoid Windows newline translation messing with Content-Length framing. *)
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;

  Printexc.record_backtrace true;
  Log.info "starting (stdio)";
  let st = { handlers = Handlers.create (); initialized = false; shutdown_requested = false } in
  loop st stdin stdout
