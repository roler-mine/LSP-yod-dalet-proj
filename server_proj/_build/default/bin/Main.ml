(* bin/Main.ml *)

module T = Lsp.Types
module Handlers = Jovial_lsp_lib.Handlers

(* Log to stderr ONLY; stdout is reserved for the protocol. *)
module Log = struct
  let info fmt =
    Printf.ksprintf
      (fun s ->
        prerr_endline ("[jovial-lsp] " ^ s);
        flush stderr)
      fmt
end

(* ----- LSP stdio framing (Content-Length headers) ----- *)
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
          else (
            match String.index_opt line ':' with
            | None -> raise (Protocol_error ("bad header line: " ^ line))
            | Some i ->
                let k = String.sub line 0 i |> String.lowercase_ascii |> String.trim in
                let v =
                  String.sub line (i + 1) (String.length line - i - 1) |> String.trim
                in
                loop ((k, v) :: acc))
    in
    loop []

  let content_length (headers : (string * string) list) : int =
    match List.assoc_opt "content-length" headers with
    | None -> raise (Protocol_error "missing Content-Length header")
    | Some v -> (
        try int_of_string v
        with _ -> raise (Protocol_error ("bad Content-Length: " ^ v)))

  let read_exactly (ic : in_channel) (n : int) : string =
    really_input_string ic n

  let read_packet (ic : in_channel) : Jsonrpc.Packet.t =
    let headers = read_headers ic in
    let len = content_length headers in
    let body = read_exactly ic len in
    Jsonrpc.Packet.t_of_yojson (Yojson.Safe.from_string body)

  let write_packet (oc : out_channel) (pkt : Jsonrpc.Packet.t) : unit =
    let body = Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t pkt) in
    output_string oc (Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body));
    output_string oc body;
    flush oc
end

type state =
  { handlers : Handlers.t
  ; mutable shutdown_requested : bool
  }

let send_server_notification (oc : out_channel) (n : Lsp.Server_notification.t) : unit =
  let notif = Lsp.Server_notification.to_jsonrpc n in
  Stdio.write_packet oc (Jsonrpc.Packet.Notification notif)

let respond_ok (oc : out_channel) ~(id : Jsonrpc.Id.t) ~(result_json : Jsonrpc.Json.t) : unit =
  let resp = Jsonrpc.Response.ok id result_json in
  Stdio.write_packet oc (Jsonrpc.Packet.Response resp)

let respond_error (oc : out_channel) ~(id : Jsonrpc.Id.t) ~(code : Jsonrpc.Response.Error.Code.t) ~(message : string) : unit =
  let err = Jsonrpc.Response.Error.make ~code ~message () in
  let resp = Jsonrpc.Response.error id err in
  Stdio.write_packet oc (Jsonrpc.Packet.Response resp)

let handle_request (st : state) (oc : out_channel) (req : Jsonrpc.Request.t) : unit =
  match Lsp.Client_request.of_jsonrpc req with
  | Error msg ->
      respond_error oc ~id:req.id ~code:(Jsonrpc.Response.Error.Code.InvalidRequest) ~message:("Bad request: " ^ msg)
  | Ok (Lsp.Client_request.E r) -> (
      match r with
      | Lsp.Client_request.Initialize _params ->
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
          let res =
            Handlers.document_symbols st.handlers p
          in
          respond_ok oc ~id:req.id ~result_json:(Lsp.Client_request.yojson_of_result r res)

      | Lsp.Client_request.TextDocumentCompletion p ->
          let res = 
            Handlers.completion st.handlers p
          in
          respond_ok oc ~id:req.id ~result_json:(Lsp.Client_request.yojson_of_result r res)

      | _ ->
          respond_error oc ~id:req.id ~code:(Jsonrpc.Response.Error.Code.MethodNotFound) ~message:"Method not implemented")

let handle_notification (st : state) (oc : out_channel) (n : Jsonrpc.Notification.t) : unit =
  match Lsp.Client_notification.of_jsonrpc n with
  | Error _ -> ()
  | Ok notif -> (
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

      | _ -> ())

let rec loop (st : state) (ic : in_channel) (oc : out_channel) : unit =
  match Stdio.read_packet ic with
  | exception End_of_file ->
      Log.info "stdin closed; exiting"
  | exception Stdio.Protocol_error msg ->
      Log.info "protocol error: %s" msg
  | pkt -> (
      match pkt with
      | Jsonrpc.Packet.Request req ->
          handle_request st oc req;
          loop st ic oc
      | Jsonrpc.Packet.Notification n ->
          handle_notification st oc n;
          loop st ic oc
      | Jsonrpc.Packet.Batch_call calls ->
          List.iter
            (function
              | `Request r -> handle_request st oc r
              | `Notification n -> handle_notification st oc n)
            calls;
          loop st ic oc
      | Jsonrpc.Packet.Response _ ->
          loop st ic oc
      | Jsonrpc.Packet.Batch_response _ ->
          loop st ic oc)

let () =
  Log.info "starting (stdio)";
  let st = { handlers = Handlers.create (); shutdown_requested = false } in
  loop st stdin stdout
