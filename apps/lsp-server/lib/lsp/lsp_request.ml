module T = Lsp.Types

let get_assoc (j : Yojson.Safe.t) : (string * Yojson.Safe.t) list option =
  match j with `Assoc xs -> Some xs | _ -> None

let find_field (k : string) (xs : (string * Yojson.Safe.t) list) =
  List.assoc_opt k xs

let method_of_msg (j : Yojson.Safe.t) : string option =
  match get_assoc j with
  | None -> None
  | Some xs -> (
      match find_field "method" xs with Some (`String m) -> Some m | _ -> None)

let id_of_msg (j : Yojson.Safe.t) : Yojson.Safe.t option =
  match get_assoc j with None -> None | Some xs -> find_field "id" xs

let params_of_msg (j : Yojson.Safe.t) : Yojson.Safe.t =
  match get_assoc j with
  | None -> `Null
  | Some xs -> (
      match find_field "params" xs with Some p -> p | None -> `Null)

let is_request (j : Yojson.Safe.t) : bool =
  match id_of_msg j with Some _ -> true | None -> false

let parse_uri_arg (arg : Yojson.Safe.t) : T.DocumentUri.t option =
  match arg with
  | `String s -> Uri_path.docuri_of_string s
  | `Assoc xs -> (
      match List.assoc_opt "uri" xs with
      | Some (`String s) -> Uri_path.docuri_of_string s
      | _ -> None)
  | _ -> None

let parse_int_arg (arg : Yojson.Safe.t) : int option =
  match arg with
  | `Int n -> Some n
  | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
  | _ -> None

let parse_text_document_uri (params : Yojson.Safe.t) : T.DocumentUri.t option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "textDocument" xs with
      | Some (`Assoc tdxs) -> (
          match find_field "uri" tdxs with
          | Some (`String s) -> Uri_path.docuri_of_string s
          | _ -> None)
      | _ -> None)

let parse_position (params : Yojson.Safe.t) : T.Position.t option =
  let int_field key xs =
    match find_field key xs with
    | Some (`Int n) -> Some n
    | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "position" xs with
      | Some (`Assoc pxs) -> (
          match (int_field "line" pxs, int_field "character" pxs) with
          | Some line, Some character -> Some { T.Position.line; character }
          | _ -> None)
      | _ -> None)

let parse_position_obj (j : Yojson.Safe.t) : T.Position.t option =
  let int_field key xs =
    match find_field key xs with
    | Some (`Int n) -> Some n
    | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  match get_assoc j with
  | None -> None
  | Some xs -> (
      match (int_field "line" xs, int_field "character" xs) with
      | Some line, Some character -> Some { T.Position.line; character }
      | _ -> None)

let parse_range (params : Yojson.Safe.t) : T.Range.t option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "range" xs with
      | Some (`Assoc rxs) -> (
          match (find_field "start" rxs, find_field "end" rxs) with
          | Some s, Some e -> (
              match (parse_position_obj s, parse_position_obj e) with
              | Some start, Some end_ -> Some { T.Range.start; end_ }
              | _ -> None)
          | _ -> None)
      | _ -> None)

let parse_new_name (params : Yojson.Safe.t) : string option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "newName" xs with
      | Some (`String s) -> Some s
      | _ -> None)

let parse_include_declaration (params : Yojson.Safe.t) : bool =
  match get_assoc params with
  | None -> true
  | Some xs -> (
      match find_field "context" xs with
      | Some (`Assoc cxs) -> (
          match find_field "includeDeclaration" cxs with
          | Some (`Bool b) -> b
          | _ -> true)
      | _ -> true)

let parse_workspace_symbol_query (params : Yojson.Safe.t) : string =
  match get_assoc params with
  | None -> ""
  | Some xs -> (
      match find_field "query" xs with Some (`String s) -> s | _ -> "")

let parse_cancel_request_id (params : Yojson.Safe.t) : Yojson.Safe.t option =
  match get_assoc params with None -> None | Some xs -> find_field "id" xs

let parse_root_uri (params : Yojson.Safe.t) : T.DocumentUri.t option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "rootUri" xs with
      | Some (`String s) when s <> "" -> Uri_path.docuri_of_string s
      | _ -> (
          match find_field "workspaceFolders" xs with
          | Some (`List (`Assoc f0 :: _)) -> (
              match find_field "uri" f0 with
              | Some (`String s) when s <> "" -> Uri_path.docuri_of_string s
              | _ -> None)
          | _ -> None))

let parse_workspace_folder_roots (params : Yojson.Safe.t) : T.DocumentUri.t list
    =
  match get_assoc params with
  | None -> []
  | Some xs -> (
      match find_field "workspaceFolders" xs with
      | Some (`List folders) ->
          folders
          |> List.filter_map (function
            | `Assoc fxs -> (
                match find_field "uri" fxs with
                | Some (`String s) when s <> "" -> Uri_path.docuri_of_string s
                | _ -> None)
            | _ -> None)
      | _ -> [])

let parse_root_uris (params : Yojson.Safe.t) : T.DocumentUri.t list =
  let roots = parse_workspace_folder_roots params in
  if roots <> [] then roots
  else match parse_root_uri params with Some u -> [ u ] | None -> []

let parse_root_path (params : Yojson.Safe.t) : string option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "rootPath" xs with
      | Some (`String s) when s <> "" -> Some s
      | _ -> None)

let parse_semantic_tokens_refresh_support (params : Yojson.Safe.t) : bool =
  match get_assoc params with
  | None -> false
  | Some xs -> (
      match find_field "capabilities" xs with
      | Some (`Assoc cxs) -> (
          match find_field "workspace" cxs with
          | Some (`Assoc wxs) -> (
              match find_field "semanticTokens" wxs with
              | Some (`Assoc stxs) -> (
                  match find_field "refreshSupport" stxs with
                  | Some (`Bool b) -> b
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)

let parse_watched_file_changes (params : Yojson.Safe.t) :
    bool * int * (string * [ `Created | `Changed | `Deleted ]) list =
  let parse_kind = function
    | `Int 1 | `Intlit "1" -> Some `Created
    | `Int 2 | `Intlit "2" -> Some `Changed
    | `Int 3 | `Intlit "3" -> Some `Deleted
    | _ -> None
  in
  match get_assoc params with
  | None -> (false, 0, [])
  | Some xs -> (
      match find_field "changes" xs with
      | Some (`List changes) ->
          let parsed =
            changes
            |> List.filter_map (function
              | `Assoc cxs -> (
                  match (find_field "uri" cxs, find_field "type" cxs) with
                  | Some (`String u), Some kind_json -> (
                      match
                        (Uri_path.docuri_of_string u, parse_kind kind_json)
                      with
                      | Some uri, Some kind -> (
                          match Uri_path.file_path_of_uri uri with
                          | Some path -> Some (path, kind)
                          | None -> None)
                      | _ -> None)
                  | _ -> None)
              | _ -> None)
          in
          (true, List.length changes, parsed)
      | _ -> (false, 0, []))

let assoc_field (name : string) (json : Yojson.Safe.t) : Yojson.Safe.t option =
  match get_assoc json with None -> None | Some xs -> find_field name xs

type source_file_set = {
  source_root_uri : T.DocumentUri.t;
  source_file_uris : T.DocumentUri.t list;
  source_search_truncated : bool;
}

let bool_field_default ~(default : bool) = function
  | Some (`Bool b) -> b
  | _ -> default

let parse_uri_list = function
  | `List xs ->
      xs
      |> List.filter_map (function
        | `String s -> Uri_path.docuri_of_string s
        | _ -> None)
  | _ -> []

let parse_source_file_sets_json = function
  | `List sets ->
      sets
      |> List.filter_map (function
        | `Assoc fields -> (
            match List.assoc_opt "rootUri" fields with
            | Some (`String root_s) -> (
                match Uri_path.docuri_of_string root_s with
                | None -> None
                | Some source_root_uri ->
                    let source_file_uris =
                      match List.assoc_opt "fileUris" fields with
                      | Some uris -> parse_uri_list uris
                      | None -> []
                    in
                    let source_search_truncated =
                      bool_field_default ~default:false
                        (List.assoc_opt "searchTruncated" fields)
                    in
                    Some
                      {
                        source_root_uri;
                        source_file_uris;
                        source_search_truncated;
                      })
            | _ -> None)
        | _ -> None)
  | _ -> []

let parse_source_file_sets (params : Yojson.Safe.t) : source_file_set list =
  let jovial =
    Option.bind (assoc_field "initializationOptions" params) (fun opts ->
        assoc_field "jovial" opts)
  in
  let workspace = Option.bind jovial (assoc_field "workspace") in
  match Option.bind workspace (assoc_field "sourceFileSets") with
  | Some sets -> parse_source_file_sets_json sets
  | _ -> []

let parse_source_file_sets_notification (params : Yojson.Safe.t) :
    source_file_set list =
  match params with
  | `Assoc fields -> (
      match List.assoc_opt "sets" fields with
      | Some sets -> parse_source_file_sets_json sets
      | None -> parse_source_file_sets_json params)
  | _ -> parse_source_file_sets_json params

let source_set_roots (sets : source_file_set list) : T.DocumentUri.t list =
  List.map (fun set -> set.source_root_uri) sets

let same_uri_path (a : T.DocumentUri.t) (b : T.DocumentUri.t) : bool =
  match (Uri_path.file_path_of_uri a, Uri_path.file_path_of_uri b) with
  | Some pa, Some pb -> Uri_path.same_path pa pb
  | _ -> Uri_path.docuri_to_string a = Uri_path.docuri_to_string b

let source_paths_for_root (sets : source_file_set list) (root_uri : T.DocumentUri.t)
    : string list =
  sets
  |> List.find_opt (fun s -> same_uri_path s.source_root_uri root_uri)
  |> Option.map (fun s -> s.source_file_uris |> List.filter_map Uri_path.file_path_of_uri)
  |> Option.value ~default:[]

let parse_client_overrides (params : Yojson.Safe.t) :
    Lsp_runtime_settings.client_overrides =
  let int_field (json : Yojson.Safe.t option) : int option =
    match json with
    | Some (`Int n) -> Some n
    | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  let bool_field (json : Yojson.Safe.t option) : bool option =
    match json with Some (`Bool b) -> Some b | _ -> None
  in
  let jovial =
    assoc_field "initializationOptions" params |> fun value ->
    Option.bind value (assoc_field "jovial")
  in
  let workspace = Option.bind jovial (assoc_field "workspace") in
  let background = Option.bind jovial (assoc_field "background") in
  let features = Option.bind jovial (assoc_field "features") in
  let feature_overrides_json = Option.bind features (assoc_field "overrides") in
  let server = Option.bind jovial (assoc_field "server") in
  let startup = Option.bind jovial (assoc_field "startup") in
  let performance = Option.bind jovial (assoc_field "performance") in
  let workspace_diag_mode =
    Option.bind workspace (assoc_field "diagnosticsMode") |> function
    | Some (`String raw) -> Workspace_settings.workspace_diag_mode_of_string raw
    | _ -> None
  in
  let workspace_profile_mode =
    Option.bind workspace (assoc_field "profileMode") |> function
    | Some (`String raw) ->
        Workspace_settings.workspace_profile_mode_of_string raw
    | _ -> None
  in
  let root_model =
    Option.bind workspace (assoc_field "rootModel") |> function
    | Some (`String raw) -> Workspace_settings.root_model_of_string raw
    | _ -> None
  in
  let root_manual_files =
    Option.bind workspace (assoc_field "manualRootFiles") |> function
    | Some (`List xs) ->
        Some
          (xs
          |> List.filter_map (function
            | `String s ->
                let trimmed = String.trim s in
                if trimmed = "" then None else Some trimmed
            | _ -> None))
    | _ -> None
  in
  let source_extensions =
    Option.bind workspace (assoc_field "extraSourceFileExtensions") |> function
    | Some (`List xs) ->
        Some
          (xs
          |> List.filter_map (function
            | `String s -> (
                match Source_file.normalize_extension s with
                | Some ext -> Some ext
                | None -> None)
            | _ -> None))
    | _ -> None
  in
  let feature_profile =
    Option.bind features (assoc_field "profile") |> function
    | Some (`String raw) -> Workspace_settings.feature_profile_of_string raw
    | _ -> None
  in
  let feature_bool name =
    match bool_field (Option.bind feature_overrides_json (assoc_field name)) with
    | Some _ as hit -> hit
    | None -> bool_field (Option.bind features (assoc_field name))
  in
  let feature_flags : Workspace_settings.feature_overrides =
    {
      Workspace_settings.diagnostics =
        feature_bool "diagnostics";
      definition = feature_bool "definition";
      declaration = feature_bool "declaration";
      type_definition = feature_bool "typeDefinition";
      implementation = feature_bool "implementation";
      references = feature_bool "references";
      document_symbols = feature_bool "documentSymbols";
      workspace_symbols = feature_bool "workspaceSymbols";
      hover = feature_bool "hover";
      signature_help = feature_bool "signatureHelp";
      rename = feature_bool "rename";
      completion = feature_bool "completion";
      code_actions = feature_bool "codeActions";
      inlay_hints = feature_bool "inlayHints";
      semantic_tokens = feature_bool "semanticTokens";
    }
  in
  let startup_priority_mode =
    (match Option.bind performance (assoc_field "priorityMode") with
    | Some _ as value -> value
    | None -> Option.bind startup (assoc_field "priorityMode"))
    |> function
    | Some (`String raw) -> Workspace_settings.startup_priority_mode_of_string raw
    | _ -> None
  in
  {
    workspace_diag_mode;
    workspace_profile_mode;
    root_model;
    root_manual_files;
    source_extensions;
    feature_profile;
    feature_flags;
    parse_file_max_bytes =
      int_field (Option.bind server (assoc_field "parseMaxFileBytes"));
    large_file_threshold_bytes =
      int_field (Option.bind performance (assoc_field "largeFileThresholdBytes"));
    huge_file_threshold_bytes =
      int_field (Option.bind performance (assoc_field "hugeFileThresholdBytes"));
    full_semantic_tokens_max_bytes =
      int_field
        (Option.bind performance (assoc_field "fullSemanticTokensMaxBytes"));
    full_parse_max_bytes =
      (match int_field (Option.bind performance (assoc_field "fullParseMaxBytes")) with
      | Some _ as hit -> hit
      | None -> int_field (Option.bind server (assoc_field "parseMaxFileBytes")));
    enable_huge_file_full_parse =
      bool_field
        (Option.bind performance (assoc_field "enableHugeFileFullParse"));
    background_parse_worker_count =
      int_field
        (Option.bind performance (assoc_field "backgroundParseWorkerCount"));
    pressure_soft_mb =
      int_field (Option.bind server (assoc_field "pressureSoftMb"));
    pressure_critical_mb =
      int_field (Option.bind server (assoc_field "pressureCriticalMb"));
    startup_priority_mode;
    bg_tick_budget_ms =
      int_field (Option.bind background (assoc_field "indexBudgetMs"));
    bg_diag_batch_size =
      int_field (Option.bind background (assoc_field "diagBatchSize"));
  }
