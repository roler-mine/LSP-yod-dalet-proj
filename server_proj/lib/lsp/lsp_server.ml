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

let yojson_of_diagnostics (ds : T.Diagnostic.t list) : Yojson.Safe.t =
  `List (List.map T.Diagnostic.yojson_of_t ds)

let publish_diagnostics (oc : out_channel) ~(uri:T.DocumentUri.t) ~(diags:T.Diagnostic.t list) : unit =
  let params =
    json_obj
      [
        "uri", `String (Uri_path.docuri_to_string uri);
        "diagnostics", yojson_of_diagnostics diags;
      ]
  in
  notify oc ~method_:"textDocument/publishDiagnostics" ~params

let semantic_token_types_legend : Yojson.Safe.t =
  `List [
    `String "namespace";
    `String "type";
    `String "function";
    `String "variable";
    `String "property";
    `String "keyword";
    `String "string";
    `String "number";
    `String "operator";
    `String "macro";
  ]

let semantic_token_modifiers_legend : Yojson.Safe.t =
  `List [
    `String "declaration";
    `String "readonly";
  ]

let initialize_result_json : Yojson.Safe.t =
  json_obj
    [
      "capabilities",
      json_obj
        [
          "textDocumentSync",
          json_obj [ "openClose", `Bool true; "change", `Int 2 (* Incremental *) ];

          "definitionProvider", `Bool true;
          "implementationProvider", `Bool true;
          "referencesProvider", `Bool true;
          "documentSymbolProvider", `Bool true;
          "hoverProvider", `Bool true;
          "renameProvider", json_obj [ "prepareProvider", `Bool true ];
          "completionProvider",
          json_obj [
            "triggerCharacters", `List [ `String "."; `String "!"; `String "'"; `String "\"" ];
            "resolveProvider", `Bool false;
          ];
          "codeActionProvider", `Bool true;
          "inlayHintProvider", `Bool true;
          "semanticTokensProvider",
          json_obj [
            "legend",
            json_obj [
              "tokenTypes", semantic_token_types_legend;
              "tokenModifiers", semantic_token_modifiers_legend;
            ];
            "full", `Bool true;
            "range", `Bool true;
          ];

          "executeCommandProvider",
          json_obj [ "commands",
                     `List [
                       `String "jovial.dumpAst";
                       `String "jovial.dumpCst";
                       `String "jovial.debugReport";
                       `String "jovial.rescanWorkspace";
                      ]
                    ];

          "workspace",
          json_obj [
            "didChangeWatchedFiles",
            json_obj [ "dynamicRegistration", `Bool false ];
          ];
        ];
    ]

let file_of_uri (u : T.DocumentUri.t) : string option =
  Uri_path.file_path_of_uri u

let parse_uri_arg (arg:Yojson.Safe.t) : T.DocumentUri.t option =
  match arg with
  | `String s ->
      Uri_path.docuri_of_string s
  | `Assoc xs ->
      (match List.assoc_opt "uri" xs with
       | Some (`String s) -> Uri_path.docuri_of_string s
       | _ -> None)
  | _ -> None

let parse_int_arg (arg:Yojson.Safe.t) : int option =
  match arg with
  | `Int n -> Some n
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let parse_text_document_uri (params:Yojson.Safe.t) : T.DocumentUri.t option =
  match get_assoc params with
  | None -> None
  | Some xs ->
      (match find_field "textDocument" xs with
       | Some (`Assoc tdxs) ->
           (match find_field "uri" tdxs with
            | Some (`String s) -> Uri_path.docuri_of_string s
            | _ -> None)
       | _ -> None)

let parse_position (params:Yojson.Safe.t) : T.Position.t option =
  let int_field key xs =
    match find_field key xs with
    | Some (`Int n) -> Some n
    | Some (`Intlit s) -> (try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  match get_assoc params with
  | None -> None
  | Some xs ->
      (match find_field "position" xs with
       | Some (`Assoc pxs) ->
           (match int_field "line" pxs, int_field "character" pxs with
            | Some line, Some character -> Some { T.Position.line; character }
            | _ -> None)
       | _ -> None)

let parse_position_obj (j:Yojson.Safe.t) : T.Position.t option =
  let int_field key xs =
    match find_field key xs with
    | Some (`Int n) -> Some n
    | Some (`Intlit s) -> (try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  match get_assoc j with
  | None -> None
  | Some xs ->
      (match int_field "line" xs, int_field "character" xs with
       | Some line, Some character -> Some { T.Position.line; character }
       | _ -> None)

let parse_range (params:Yojson.Safe.t) : T.Range.t option =
  match get_assoc params with
  | None -> None
  | Some xs ->
      (match find_field "range" xs with
       | Some (`Assoc rxs) ->
           (match find_field "start" rxs, find_field "end" rxs with
            | Some s, Some e ->
                (match parse_position_obj s, parse_position_obj e with
                 | Some start, Some end_ -> Some { T.Range.start; end_ }
                 | _ -> None)
            | _ -> None)
       | _ -> None)

let parse_new_name (params:Yojson.Safe.t) : string option =
  match get_assoc params with
  | None -> None
  | Some xs ->
      (match find_field "newName" xs with
       | Some (`String s) -> Some s
       | _ -> None)

let parse_include_declaration (params:Yojson.Safe.t) : bool =
  match get_assoc params with
  | None -> true
  | Some xs ->
      (match find_field "context" xs with
       | Some (`Assoc cxs) ->
           (match find_field "includeDeclaration" cxs with
            | Some (`Bool b) -> b
            | _ -> true)
       | _ -> true)

let parse_root_uri (params:Yojson.Safe.t) : T.DocumentUri.t option =
  match get_assoc params with
  | None -> None
  | Some xs ->
      (match find_field "rootUri" xs with
       | Some (`String s) when s <> "" -> Uri_path.docuri_of_string s
       | _ ->
           (match find_field "workspaceFolders" xs with
            | Some (`List ((`Assoc f0) :: _)) ->
                (match find_field "uri" f0 with
                 | Some (`String s) when s <> "" -> Uri_path.docuri_of_string s
                 | _ -> None)
            | _ -> None))

let parse_root_path (params:Yojson.Safe.t) : string option =
  match get_assoc params with
  | None -> None
  | Some xs ->
      (match find_field "rootPath" xs with
       | Some (`String s) when s <> "" -> Some s
       | _ -> None)

let parse_semantic_tokens_refresh_support (params:Yojson.Safe.t) : bool =
  match get_assoc params with
  | None -> false
  | Some xs ->
      (match find_field "capabilities" xs with
       | Some (`Assoc cxs) ->
           (match find_field "workspace" cxs with
            | Some (`Assoc wxs) ->
                (match find_field "semanticTokens" wxs with
                 | Some (`Assoc stxs) ->
                     (match find_field "refreshSupport" stxs with
                      | Some (`Bool b) -> b
                      | _ -> false)
                 | _ -> false)
            | _ -> false)
       | _ -> false)

let send_request (oc:out_channel) ~(id:Yojson.Safe.t) ~(method_:string) ~(params:Yojson.Safe.t) : unit =
  Lsp_io.write_message oc
    (json_obj [ "jsonrpc", `String "2.0"; "id", id; "method", `String method_; "params", params ])

let request_semantic_tokens_refresh
    (oc:out_channel)
    ~(refresh_supported:bool)
    (next_id:int ref)
  : unit =
  if refresh_supported then (
    let id = !next_id in
    incr next_id;
    send_request oc ~id:(`Int id) ~method_:"workspace/semanticTokens/refresh" ~params:`Null
  )

let handle_initialize
    (ws:Workspace.t)
    ~(semantic_refresh_supported:bool ref)
    (oc:out_channel)
    (id:Yojson.Safe.t)
    (params:Yojson.Safe.t) =
  (* Set workspace root from rootUri/workspaceFolders/rootPath, then scan. *)
  semantic_refresh_supported := parse_semantic_tokens_refresh_support params;
  (match parse_root_uri params with
   | Some ru -> Workspace.set_root_uri ws (Some ru)
   | None -> Workspace.set_root_path ws (parse_root_path params));
  Workspace.rescan ws;
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

    | "jovial.dumpCst" ->
        let uri_opt =
          match p.arguments with
          | None -> None
          | Some (a0 :: _) -> parse_uri_arg a0
          | Some [] -> None
        in
        (match uri_opt with
         | None -> respond_error oc ~id ~code:(-32602) ~message:"dumpCst: missing uri argument"
         | Some uri ->
             (match Workspace.cst_dump_for ws ~uri with
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

let handle_notification
    (ws : Workspace.t)
    ~(semantic_refresh_supported:bool ref)
    ~(next_server_request_id:int ref)
    (oc : out_channel)
    (method_ : string)
    (params : Yojson.Safe.t) =
  match method_ with
  | "initialized" -> ()
  | "textDocument/didOpen" ->
      (try
         let p = T.DidOpenTextDocumentParams.t_of_yojson params in
         let td = p.textDocument in
         let uri = td.uri in
         let file = file_of_uri uri in
         Workspace.open_doc ws ~uri ~file ~text:td.text;
         publish_diagnostics oc ~uri ~diags:(Workspace.diagnostics_for ws ~uri);
         request_semantic_tokens_refresh oc
           ~refresh_supported:!semantic_refresh_supported
           next_server_request_id
       with _ -> ())
  | "textDocument/didChange" ->
      (try
         let p = T.DidChangeTextDocumentParams.t_of_yojson params in
         let uri = p.textDocument.uri in
         Workspace.change_doc ws ~uri ~changes:p.contentChanges;
         publish_diagnostics oc ~uri ~diags:(Workspace.diagnostics_for ws ~uri);
         request_semantic_tokens_refresh oc
           ~refresh_supported:!semantic_refresh_supported
           next_server_request_id
       with _ -> ())
  | "textDocument/didClose" ->
      (try
         let p = T.DidCloseTextDocumentParams.t_of_yojson params in
         let uri = p.textDocument.uri in
         Workspace.close_doc ws ~uri;
         publish_diagnostics oc ~uri ~diags:[];
         request_semantic_tokens_refresh oc
           ~refresh_supported:!semantic_refresh_supported
           next_server_request_id
       with _ -> ())
  | "workspace/didChangeWatchedFiles" ->
      Workspace.rescan ws;
      Workspace.revalidate_all ws
      |> List.iter (fun uri ->
           publish_diagnostics oc ~uri ~diags:(Workspace.diagnostics_for ws ~uri));
      request_semantic_tokens_refresh oc
        ~refresh_supported:!semantic_refresh_supported
        next_server_request_id
  | "exit" -> exit 0
  | _ -> ()

let handle_request
    (ws : Workspace.t)
    ~(semantic_refresh_supported:bool ref)
    (oc : out_channel)
    (method_ : string)
    (id : Yojson.Safe.t)
    (params : Yojson.Safe.t) =
  match method_ with
  | "initialize" -> handle_initialize ws ~semantic_refresh_supported oc id params
  | "shutdown" -> handle_shutdown oc id
  | "workspace/executeCommand" -> handle_execute_command ws oc id params
  | "textDocument/documentSymbol" ->
      (match parse_text_document_uri params with
       | None -> respond_error oc ~id ~code:(-32602) ~message:"documentSymbol: missing textDocument.uri"
       | Some uri ->
           let j = Workspace.document_symbols_json_for ws ~uri in
           respond oc ~id ~result:j)
  | "textDocument/definition" ->
      (match parse_text_document_uri params, parse_position params with
       | Some uri, Some pos ->
           let j = Workspace.definition_json_for ws ~uri ~pos in
           respond oc ~id ~result:j
       | _ ->
           respond_error oc ~id ~code:(-32602) ~message:"definition: invalid textDocument or position")
  | "textDocument/implementation" ->
      (match parse_text_document_uri params, parse_position params with
       | Some uri, Some pos ->
           let j = Workspace.implementation_json_for ws ~uri ~pos in
           respond oc ~id ~result:j
       | _ ->
           respond_error oc ~id ~code:(-32602) ~message:"implementation: invalid textDocument or position")
  | "textDocument/references" ->
      (match parse_text_document_uri params, parse_position params with
       | Some uri, Some pos ->
           let include_decl = parse_include_declaration params in
           let j = Workspace.references_json_for ws ~uri ~pos ~include_decl in
           respond oc ~id ~result:j
       | _ ->
           respond_error oc ~id ~code:(-32602) ~message:"references: invalid textDocument or position")
  | "textDocument/hover" ->
      (match parse_text_document_uri params, parse_position params with
       | Some uri, Some pos ->
           let j = Workspace.hover_json_for ws ~uri ~pos in
           respond oc ~id ~result:j
       | _ ->
           respond_error oc ~id ~code:(-32602) ~message:"hover: invalid textDocument or position")
  | "textDocument/prepareRename" ->
      (match parse_text_document_uri params, parse_position params with
       | Some uri, Some pos ->
           let j = Workspace.prepare_rename_json_for ws ~uri ~pos in
           respond oc ~id ~result:j
       | _ ->
           respond_error oc ~id ~code:(-32602) ~message:"prepareRename: invalid textDocument or position")
  | "textDocument/rename" ->
      (match parse_text_document_uri params, parse_position params, parse_new_name params with
       | Some uri, Some pos, Some new_name ->
           let j = Workspace.rename_json_for ws ~uri ~pos ~new_name in
           respond oc ~id ~result:j
       | _ ->
           respond_error oc ~id ~code:(-32602) ~message:"rename: invalid textDocument, position, or newName")
  | "textDocument/completion" ->
      (match parse_text_document_uri params, parse_position params with
       | Some uri, Some pos ->
           let j = Workspace.completion_json_for ws ~uri ~pos in
           respond oc ~id ~result:j
       | _ ->
           respond_error oc ~id ~code:(-32602) ~message:"completion: invalid textDocument or position")
  | "textDocument/codeAction" ->
      (match parse_text_document_uri params, parse_range params with
       | Some uri, Some range ->
           let j = Workspace.code_actions_json_for ws ~uri ~range in
           respond oc ~id ~result:j
       | _ ->
           respond_error oc ~id ~code:(-32602) ~message:"codeAction: invalid textDocument or range")
  | "textDocument/inlayHint" ->
      (match parse_text_document_uri params, parse_range params with
       | Some uri, Some range ->
           let j = Workspace.inlay_hints_json_for ws ~uri ~range in
           respond oc ~id ~result:j
       | _ ->
           respond_error oc ~id ~code:(-32602) ~message:"inlayHint: invalid textDocument or range")
  | "textDocument/semanticTokens/full" ->
      (match parse_text_document_uri params with
       | Some uri ->
           let j = Workspace.semantic_tokens_full_json_for ws ~uri in
           respond oc ~id ~result:j
       | _ ->
           respond_error oc ~id ~code:(-32602) ~message:"semanticTokens/full: invalid textDocument")
  | "textDocument/semanticTokens/range" ->
      (match parse_text_document_uri params, parse_range params with
       | Some uri, Some range ->
           let j = Workspace.semantic_tokens_range_json_for ws ~uri ~range in
           respond oc ~id ~result:j
       | _ ->
           respond_error oc ~id ~code:(-32602) ~message:"semanticTokens/range: invalid textDocument or range")
  | _ -> respond_error oc ~id ~code:(-32601) ~message:("Method not found: " ^ method_)

let run (ic : in_channel) (oc : out_channel) : unit =
  (* critical on Windows: LSP framing expects binary-accurate reads/writes *)
  if Sys.win32 then (
    set_binary_mode_in ic true;
    set_binary_mode_out oc true;
  );

  let ws = Workspace.create () in
  let semantic_refresh_supported = ref false in
  let next_server_request_id = ref 1 in

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
                | Some id ->
                    handle_request ws ~semantic_refresh_supported oc m id (params_of_msg json))
             else
               handle_notification ws
                 ~semantic_refresh_supported
                 ~next_server_request_id
                 oc
                 m
                 (params_of_msg json));
        loop ()
  in
  loop ()
