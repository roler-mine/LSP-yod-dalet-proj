module T = Lsp.Types

let json_obj (fields : (string * Yojson.Safe.t) list) : Yojson.Safe.t =
  `Assoc fields

let get_assoc (j : Yojson.Safe.t) : (string * Yojson.Safe.t) list option =
  match j with
  | `Assoc xs -> Some xs
  | _ -> None

let find_field (k : string) (xs : (string * Yojson.Safe.t) list) =
  List.assoc_opt k xs

let method_of_msg (j : Yojson.Safe.t) : string option =
  match get_assoc j with
  | None -> None
  | Some xs ->
      (match find_field "method" xs with
       | Some (`String m) -> Some m
       | _ -> None)

let id_of_msg (j : Yojson.Safe.t) : Yojson.Safe.t option =
  match get_assoc j with
  | None -> None
  | Some xs -> find_field "id" xs

let params_of_msg (j : Yojson.Safe.t) : Yojson.Safe.t =
  match get_assoc j with
  | None -> `Null
  | Some xs -> (match find_field "params" xs with Some p -> p | None -> `Null)

let is_request (j : Yojson.Safe.t) : bool =
  match id_of_msg j with Some _ -> true | None -> false

let respond (oc : out_channel) ~(id:Yojson.Safe.t) ~(result:Yojson.Safe.t) : unit =
  Lsp_io.write_message oc (json_obj [ "jsonrpc", `String "2.0"; "id", id; "result", result ])

let respond_error (oc : out_channel) ~(id:Yojson.Safe.t) ~(code:int) ~(message:string) : unit =
  let err = json_obj [ "code", `Int code; "message", `String message ] in
  Lsp_io.write_message oc (json_obj [ "jsonrpc", `String "2.0"; "id", id; "error", err ])

let notify (oc : out_channel) ~(method_:string) ~(params:Yojson.Safe.t) : unit =
  Lsp_io.write_message oc (json_obj [ "jsonrpc", `String "2.0"; "method", `String method_; "params", params ])

let docuri_to_string (u:T.DocumentUri.t) : string =
  try
    match T.DocumentUri.yojson_of_t u with
    | `String s -> s
    | _ -> ""
  with _ -> ""

let yojson_of_diagnostics (ds : T.Diagnostic.t list) : Yojson.Safe.t =
  `List (List.map T.Diagnostic.yojson_of_t ds)

let publish_diagnostics (oc : out_channel) ~(uri:T.DocumentUri.t) ~(diags:T.Diagnostic.t list) : unit =
  let params =
    json_obj
      [
        "uri", `String (docuri_to_string uri);
        "diagnostics", yojson_of_diagnostics diags;
      ]
  in
  notify oc ~method_:"textDocument/publishDiagnostics" ~params

let initialize_result_json : Yojson.Safe.t =
  json_obj
    [
      "capabilities",
      json_obj
        [
          "textDocumentSync",
          json_obj [ "openClose", `Bool true; "change", `Int 2 (* Incremental *) ];

          "executeCommandProvider",
          json_obj [ "commands",
                     `List [
                       `String "jovial.dumpAst";
                       `String "jovial.debugReport";
                       `String "jovial.rescanWorkspace";
                     ]
                   ];
        ];
    ]

let file_of_uri (u : T.DocumentUri.t) : string option =
  let s = docuri_to_string u in
  let prefix = "file://" in
  if String.length s >= String.length prefix
     && String.sub s 0 (String.length prefix) = prefix
  then Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else None

let parse_uri_arg (arg:Yojson.Safe.t) : T.DocumentUri.t option =
  match arg with
  | `String s ->
      (try Some (T.DocumentUri.t_of_yojson (`String s)) with _ -> None)
  | `Assoc xs ->
      (match List.assoc_opt "uri" xs with
       | Some (`String s) -> (try Some (T.DocumentUri.t_of_yojson (`String s)) with _ -> None)
       | _ -> None)
  | _ -> None

let parse_int_arg (arg:Yojson.Safe.t) : int option =
  match arg with
  | `Int n -> Some n
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let handle_initialize (ws:Workspace.t) (oc:out_channel) (id:Yojson.Safe.t) (params:Yojson.Safe.t) =
  (* set workspace root if provided, then scan *)
  (try
     let p = T.InitializeParams.t_of_yojson params in
     Workspace.set_root_uri ws p.rootUri;
     Workspace.rescan ws
   with _ -> ());
  respond oc ~id ~result:initialize_result_json

let handle_shutdown oc id =
  respond oc ~id ~result:`Null

let handle_execute_command (ws : Workspace.t) (oc : out_channel) (id : Yojson.Safe.t) (params : Yojson.Safe.t) =
  try
    let p = T.ExecuteCommandParams.t_of_yojson params in
    match p.command with
    | "jovial.dumpAst" ->
        let uri_opt =
          match p.arguments with
          | None -> None
          | Some (a0 :: _) -> parse_uri_arg a0
          | Some [] -> None
        in
        (match uri_opt with
         | None -> respond_error oc ~id ~code:(-32602) ~message:"dumpAst: missing uri argument"
         | Some uri ->
             (match Workspace.ast_dump_for ws ~uri with
              | None -> respond oc ~id ~result:`Null
              | Some s -> respond oc ~id ~result:(`String s)))

    | "jovial.rescanWorkspace" ->
        Workspace.rescan ws;
        respond oc ~id ~result:(`Assoc [ "compoolCount", `Int (Workspace.compool_count ws) ])

    | "jovial.debugReport" ->
        let uri_opt, max_tokens =
          match p.arguments with
          | None -> (None, 200)
          | Some [] -> (None, 200)
          | Some (a0 :: rest) ->
              let uri = parse_uri_arg a0 in
              let mt =
                match rest with
                | a1 :: _ -> (match parse_int_arg a1 with Some n -> n | None -> 200)
                | [] -> 200
              in
              (uri, mt)
        in
        (match uri_opt with
         | None -> respond_error oc ~id ~code:(-32602) ~message:"debugReport: missing uri argument"
         | Some uri ->
             let j = Workspace.debug_report_for ws ~uri ~max_tokens in
             respond oc ~id ~result:j)

    | _ ->
        respond_error oc ~id ~code:(-32601) ~message:"Unknown command"
  with _ ->
    respond_error oc ~id ~code:(-32602) ~message:"Invalid executeCommand params"

let handle_notification (ws : Workspace.t) (oc : out_channel) (method_ : string) (params : Yojson.Safe.t) =
  match method_ with
  | "initialized" -> ()
  | "textDocument/didOpen" ->
      (try
         let p = T.DidOpenTextDocumentParams.t_of_yojson params in
         let td = p.textDocument in
         let uri = td.uri in
         let file = file_of_uri uri in
         Workspace.open_doc ws ~uri ~file ~text:td.text;
         publish_diagnostics oc ~uri ~diags:(Workspace.diagnostics_for ws ~uri)
       with _ -> ())
  | "textDocument/didChange" ->
      (try
         let p = T.DidChangeTextDocumentParams.t_of_yojson params in
         let uri = p.textDocument.uri in
         Workspace.change_doc ws ~uri ~changes:p.contentChanges;
         publish_diagnostics oc ~uri ~diags:(Workspace.diagnostics_for ws ~uri)
       with _ -> ())
  | "textDocument/didClose" ->
      (try
         let p = T.DidCloseTextDocumentParams.t_of_yojson params in
         let uri = p.textDocument.uri in
         Workspace.close_doc ws ~uri;
         publish_diagnostics oc ~uri ~diags:[]
       with _ -> ())
  | "exit" -> exit 0
  | _ -> ()

let handle_request (ws : Workspace.t) (oc : out_channel) (method_ : string) (id : Yojson.Safe.t) (params : Yojson.Safe.t) =
  match method_ with
  | "initialize" -> handle_initialize ws oc id params
  | "shutdown" -> handle_shutdown oc id
  | "workspace/executeCommand" -> handle_execute_command ws oc id params
  | _ -> respond_error oc ~id ~code:(-32601) ~message:("Method not found: " ^ method_)

let run (ic : in_channel) (oc : out_channel) : unit =
  (* critical on Windows: LSP framing expects binary-accurate reads/writes *)
  if Sys.win32 then (
    set_binary_mode_in ic true;
    set_binary_mode_out oc true;
  );

  let ws = Workspace.create () in

  let rec loop () =
    match Lsp_io.read_message ic with
    | None -> ()
    | Some msg ->
        let json =
          try Yojson.Safe.from_string msg
          with _ -> `Null
        in
        (match method_of_msg json with
         | None -> ()
         | Some m ->
             if is_request json then
               (match id_of_msg json with
                | None -> ()
                | Some id -> handle_request ws oc m id (params_of_msg json))
             else
               handle_notification ws oc m (params_of_msg json));
        loop ()
  in
  loop ()
