module T = Lsp.Types

let json_obj (fields : (string * Yojson.Safe.t) list) : Yojson.Safe.t =
  `Assoc fields

let respond (oc : out_channel) ~(id:Yojson.Safe.t) ~(result:Yojson.Safe.t) : unit =
  Lsp_io.write_message oc (json_obj [ "jsonrpc", `String "2.0"; "id", id; "result", result ])

let respond_error (oc : out_channel) ~(id:Yojson.Safe.t) ~(code:int) ~(message:string) : unit =
  let err = json_obj [ "code", `Int code; "message", `String message ] in
  Lsp_io.write_message oc (json_obj [ "jsonrpc", `String "2.0"; "id", id; "error", err ])

let notify (oc : out_channel) ~(method_:string) ~(params:Yojson.Safe.t) : unit =
  Lsp_io.write_message oc (json_obj [ "jsonrpc", `String "2.0"; "method", `String method_; "params", params ])

let yojson_of_diagnostics (ds : T.Diagnostic.t list) : Yojson.Safe.t =
  `List (List.map T.Diagnostic.yojson_of_t ds)

let yojson_of_locations (xs:T.Location.t list) : Yojson.Safe.t =
  `List (List.map T.Location.yojson_of_t xs)

let yojson_of_document_symbols
    (xs:[< `DocumentSymbol of T.DocumentSymbol.t | `SymbolInformation of T.SymbolInformation.t ] list)
  : Yojson.Safe.t =
  `List (
    List.map (function
      | `DocumentSymbol symbol -> T.DocumentSymbol.yojson_of_t symbol
      | `SymbolInformation symbol -> T.SymbolInformation.yojson_of_t symbol
    ) xs
  )

let yojson_of_symbol_infos (xs:T.SymbolInformation.t list) : Yojson.Safe.t =
  `List (List.map T.SymbolInformation.yojson_of_t xs)

let yojson_of_hover_opt = function
  | None -> `Null
  | Some hover -> T.Hover.yojson_of_t hover

let yojson_of_signature_help_opt = function
  | None -> `Null
  | Some payload -> T.SignatureHelp.yojson_of_t payload

let yojson_of_prepare_rename_opt
    (value:[< `Range of T.Range.t | `RangeWithPlaceholder of T.Range.t * string ] option)
  : Yojson.Safe.t =
  match value with
  | None -> `Null
  | Some (`Range range) -> T.Range.yojson_of_t range
  | Some (`RangeWithPlaceholder (range, placeholder)) ->
      json_obj
        [
          "range", T.Range.yojson_of_t range;
          "placeholder", `String placeholder;
        ]

let yojson_of_workspace_edit_opt = function
  | None -> `Null
  | Some edit -> T.WorkspaceEdit.yojson_of_t edit

let yojson_of_completion_items (xs:T.CompletionItem.t list) : Yojson.Safe.t =
  `List (List.map T.CompletionItem.yojson_of_t xs)

let yojson_of_code_actions (xs:T.CodeAction.t list) : Yojson.Safe.t =
  `List (List.map T.CodeAction.yojson_of_t xs)

let yojson_of_inlay_hints (xs:T.InlayHint.t list) : Yojson.Safe.t =
  `List (List.map T.InlayHint.yojson_of_t xs)

let yojson_of_semantic_tokens_opt = function
  | None -> `Null
  | Some payload -> T.SemanticTokens.yojson_of_t payload

let publish_diagnostics (oc : out_channel) ~(uri:T.DocumentUri.t) ~(diags:T.Diagnostic.t list) : unit =
  let params =
    json_obj
      [
        "uri", `String (Uri_path.docuri_to_string uri);
        "diagnostics", yojson_of_diagnostics diags;
      ]
  in
  notify oc ~method_:"textDocument/publishDiagnostics" ~params

let diagnostics_digest (diags:T.Diagnostic.t list) : string =
  Digest.(to_hex (string (Yojson.Safe.to_string (yojson_of_diagnostics diags))))

let publish_diagnostics_if_changed
    (published_diags:(string, string) Hashtbl.t)
    (oc:out_channel)
    ~(uri:T.DocumentUri.t)
    ~(diags:T.Diagnostic.t list)
  : bool =
  let uri_s = Uri_path.docuri_to_string uri in
  let digest = diagnostics_digest diags in
  match Hashtbl.find_opt published_diags uri_s with
  | Some prev when prev = digest -> false
  | _ ->
      Hashtbl.replace published_diags uri_s digest;
      publish_diagnostics oc ~uri ~diags;
      true

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
          json_obj [ "openClose", `Bool true; "change", `Int 2 ];
          "definitionProvider", `Bool true;
          "declarationProvider", `Bool true;
          "typeDefinitionProvider", `Bool true;
          "implementationProvider", `Bool true;
          "referencesProvider", `Bool true;
          "workspaceSymbolProvider", `Bool true;
          "documentSymbolProvider", `Bool true;
          "hoverProvider", `Bool true;
          "signatureHelpProvider",
          json_obj [
            "triggerCharacters", `List [ `String "("; `String "," ];
            "retriggerCharacters", `List [ `String "," ];
          ];
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
                       `String "jovial.dumpLsifIndex";
                       `String "jovial.dumpLsifDelta";
                       `String "jovial.debugReport";
                       `String "jovial.debugPerfStats";
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
