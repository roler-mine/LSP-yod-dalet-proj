(* Module overview: Disk cache helpers for workspace summaries and index reuse. *)

module T = Lsp.Types

open Workspace_foundation

type file_metadata = {
  path : string;
  path_key : string;
  uri : string;
  size : int;
  mtime_ns : int64;
  content_hash : string option;
  source_extensions : string list;
  parser_version : string;
  indexer_version : string;
  schema_version : int;
}

type source_index_load = {
  index : Workspace_index.t;
  loaded_from_cache : bool;
  changed_paths : string list;
  pruned_paths : string list;
}

type skeleton_cache = {
  entries_by_path : (string, quick_nav_entry list) Hashtbl.t;
}

type module_summary_cache = {
  summary_entries : module_summary_cache_entry list;
}

type source_payload = {
  index : Workspace_index.t;
  metadata_by_path : (string, file_metadata) Hashtbl.t;
}

type skeleton_payload = {
  skeleton_entries : (string, file_metadata * quick_nav_entry list) Hashtbl.t;
}

type module_summary_payload = {
  module_summaries : (string, file_metadata * Module_summary.t) Hashtbl.t;
}

type source_binary_payload = {
  source_index : Workspace_index.t;
  source_metadata : file_metadata list;
}

type skeleton_binary_payload = {
  skeleton_max_bytes : int;
  skeleton_items : (file_metadata * quick_nav_entry list) list;
}

type module_summary_binary_payload = {
  module_summary_items : (file_metadata * Module_summary.t) list;
}

type 'a binary_envelope = {
  be_schema_version : int;
  be_parser_version : string;
  be_indexer_version : string;
  be_implementation_config_version : string;
  be_source_extensions : string list;
  be_payload : 'a;
}

let schema_version = 3
let parser_version = Workspace_persistent_index.parser_version
let indexer_version = Workspace_persistent_index.indexer_version
let implementation_config_version = "jovial-persistent-cache-v2-binary"

let cache_dir ~(root : string) = Workspace_storage_layout.cache_dir ~root
let json_path ~(root : string) name = Filename.concat (cache_dir ~root) name
let bin_path ~(root : string) name = Filename.concat (cache_dir ~root) name
let cache_version_json_path ~(root : string) = json_path ~root "cache-version.json"
let source_index_json_path ~(root : string) = json_path ~root "source-index.json"
let source_index_bin_path ~(root : string) = bin_path ~root "source-index.bin"
let skeleton_index_json_path ~(root : string) = json_path ~root "skeleton-index.json"
let skeleton_index_bin_path ~(root : string) = bin_path ~root "skeleton-index.bin"
let module_summary_json_path ~(root : string) =
  json_path ~root "module-summaries.json"
let module_summary_bin_path ~(root : string) =
  bin_path ~root "module-summaries.bin"

let binary_magic = "JOVIAL_LSP_CACHE_BIN_V2\n"

let normalized_source_extensions (source_extensions : string list) =
  Source_file.normalize_extensions source_extensions |> List.sort_uniq String.compare

let ensure_dir path =
  let rec loop path =
    if path = "" || Sys.file_exists path then ()
    else (
      loop (Filename.dirname path);
      try Unix.mkdir path 0o755 with _ -> ())
  in
  loop path

let read_json path = try Some (Yojson.Safe.from_file path) with _ -> None

let header_matches_values ~(source_extensions : string list) ~schema ~parser
    ~indexer ~implementation ~extensions =
  schema = schema_version
  && parser = parser_version
  && indexer = indexer_version
  && implementation = implementation_config_version
  && List.sort_uniq String.compare extensions
     = normalized_source_extensions source_extensions

let binary_header_matches ~(source_extensions : string list)
    (envelope : 'a binary_envelope) : bool =
  header_matches_values ~source_extensions ~schema:envelope.be_schema_version
    ~parser:envelope.be_parser_version ~indexer:envelope.be_indexer_version
    ~implementation:envelope.be_implementation_config_version
    ~extensions:envelope.be_source_extensions

let read_binary_payload ~(source_extensions : string list) path : 'a option =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let magic = really_input_string ic (String.length binary_magic) in
        if magic <> binary_magic then None
        else
          let envelope : 'a binary_envelope = Marshal.from_channel ic in
          if binary_header_matches ~source_extensions envelope then
            Some envelope.be_payload
          else None)
  with _ -> None

let save_binary_payload ~(source_extensions : string list) path payload =
  ensure_dir (Filename.dirname path);
  let tmp = path ^ ".tmp" in
  let oc = open_out_bin tmp in
  try
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc binary_magic;
        Marshal.to_channel oc
          {
            be_schema_version = schema_version;
            be_parser_version = parser_version;
            be_indexer_version = indexer_version;
            be_implementation_config_version = implementation_config_version;
            be_source_extensions =
              normalized_source_extensions source_extensions;
            be_payload = payload;
          }
          [ Marshal.No_sharing ]);
    Sys.rename tmp path
  with exn ->
    close_out_noerr oc;
    (try Sys.remove tmp with _ -> ());
    raise exn

let content_hash_of_path path : string option =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        Some (Digest.to_hex (Digest.string (really_input_string ic len))))
  with _ -> None

let save_json path json =
  ensure_dir (Filename.dirname path);
  let tmp = path ^ ".tmp" in
  let oc = open_out_bin tmp in
  try
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> Yojson.Safe.to_channel oc json);
    Sys.rename tmp path
  with exn ->
    close_out_noerr oc;
    (try Sys.remove tmp with _ -> ());
    raise exn

let json_of_string_list values =
  `List (List.map (fun value -> `String value) values)

let string_list_of_json = function
  | `List values ->
      values |> List.filter_map (function `String s -> Some s | _ -> None)
  | _ -> []

let json_of_option f = function None -> `Null | Some value -> f value

let option_string_of_json = function
  | `Null -> Some None
  | `String s -> Some (Some s)
  | _ -> None

let field name fields = List.assoc_opt name fields

let int_of_json = function
  | `Int i -> Some i
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let int64_of_json = function
  | `Int i -> Some (Int64.of_int i)
  | `Intlit s -> (try Some (Int64.of_string s) with _ -> None)
  | `String s -> (try Some (Int64.of_string s) with _ -> None)
  | _ -> None

let string_of_json = function `String s -> Some s | _ -> None
let bool_of_json = function `Bool b -> Some b | _ -> None
let field_bind name fields f = match field name fields with None -> None | Some v -> f v

let file_uri path =
  match Uri_path.docuri_of_path path with
  | Some uri -> Uri_path.docuri_to_string uri
  | None -> Uri_path.file_uri_of_path path

let fingerprint_path (path : string) : file_metadata option =
  try
    let st = Unix.stat path in
    if st.Unix.st_kind = Unix.S_DIR then None
    else
      Some
        {
          path;
          path_key = Uri_path.normalize_path_key path;
          uri = file_uri path;
          size = st.Unix.st_size;
          mtime_ns = Int64.of_float (st.Unix.st_mtime *. 1_000_000_000.0);
          content_hash = None;
          source_extensions = [];
          parser_version;
          indexer_version;
          schema_version;
        }
  with _ -> None

let metadata_for_path ?content_hash ~(source_extensions : string list) path :
    file_metadata option =
  match fingerprint_path path with
  | None -> None
  | Some meta ->
      Some
        {
          meta with
          content_hash;
          source_extensions = normalized_source_extensions source_extensions;
        }

let json_of_file_metadata (m : file_metadata) =
  `Assoc
    [
      ("schemaVersion", `Int m.schema_version);
      ("parserVersion", `String m.parser_version);
      ("indexerVersion", `String m.indexer_version);
      ("sourceExtensions", json_of_string_list m.source_extensions);
      ("path", `String m.path);
      ("pathKey", `String m.path_key);
      ("uri", `String m.uri);
      ("size", `Int m.size);
      ("mtimeNs", `Intlit (Int64.to_string m.mtime_ns));
      ("contentHash", json_of_option (fun s -> `String s) m.content_hash);
    ]

let file_metadata_of_json = function
  | `Assoc fields -> (
      match
        ( field_bind "path" fields string_of_json,
          field_bind "pathKey" fields string_of_json,
          field_bind "uri" fields string_of_json,
          field_bind "size" fields int_of_json,
          field_bind "mtimeNs" fields int64_of_json,
          field_bind "parserVersion" fields string_of_json,
          field_bind "indexerVersion" fields string_of_json,
          field_bind "schemaVersion" fields int_of_json )
      with
      | ( Some path,
          Some path_key,
          Some uri,
          Some size,
          Some mtime_ns,
          Some parser_version,
          Some indexer_version,
          Some schema_version ) ->
          let source_extensions =
            field "sourceExtensions" fields
            |> Option.map string_list_of_json
            |> Option.value ~default:[]
            |> List.sort_uniq String.compare
          in
          let content_hash =
            field_bind "contentHash" fields option_string_of_json
            |> Option.value ~default:None
          in
          Some
            {
              path;
              path_key;
              uri;
              size;
              mtime_ns;
              content_hash;
              source_extensions;
              parser_version;
              indexer_version;
              schema_version;
            }
      | _ -> None)
  | _ -> None

let metadata_matches ~(source_extensions : string list) (meta : file_metadata)
    ~(path : string) : bool =
  match metadata_for_path ~source_extensions path with
  | None -> false
  | Some current ->
      current.path_key = meta.path_key
      && current.size = meta.size
      && current.mtime_ns = meta.mtime_ns
      && (match meta.content_hash with
         | None -> true
         | Some expected -> content_hash_of_path path = Some expected)
      && current.source_extensions = meta.source_extensions
      && meta.schema_version = schema_version
      && meta.parser_version = parser_version
      && meta.indexer_version = indexer_version

let regular_source ~(source_extensions : string list) path =
  Source_file.has_extension
    ~extensions:(Source_file.normalize_extensions source_extensions)
    (Filename.basename path)
  && Sys.file_exists path
  && try not (Sys.is_directory path) with _ -> false

let unique_regular_paths ~(source_extensions : string list) paths =
  let seen = Hashtbl.create (max 16 (List.length paths)) in
  paths
  |> List.filter_map (fun path ->
         let key = Uri_path.normalize_path_key path in
         if key = "" || Hashtbl.mem seen key then None
         else if not (regular_source ~source_extensions path) then None
         else (
           Hashtbl.replace seen key true;
           Some path))

let cache_header ~(source_extensions : string list) extra_fields =
  `Assoc
    ([
       ("schemaVersion", `Int schema_version);
       ("parserVersion", `String parser_version);
       ("indexerVersion", `String indexer_version);
       ("implementationConfigVersion", `String implementation_config_version);
       ( "sourceExtensions",
         json_of_string_list (normalized_source_extensions source_extensions) );
     ]
    @ extra_fields)

let header_matches ~(source_extensions : string list) fields =
  let got_schema = field_bind "schemaVersion" fields int_of_json in
  let got_parser = field_bind "parserVersion" fields string_of_json in
  let got_indexer = field_bind "indexerVersion" fields string_of_json in
  let got_impl =
    field_bind "implementationConfigVersion" fields string_of_json
  in
  let got_extensions =
    field "sourceExtensions" fields
    |> Option.map string_list_of_json
    |> Option.value ~default:[]
    |> List.sort_uniq String.compare
  in
  got_schema = Some schema_version
  && got_parser = Some parser_version
  && got_indexer = Some indexer_version
  && got_impl = Some implementation_config_version
  && got_extensions = normalized_source_extensions source_extensions

let save_cache_version ~(root : string) ~(source_extensions : string list) :
    unit =
  try save_json (cache_version_json_path ~root) (cache_header ~source_extensions [])
  with _ -> Perf_stats.tick "persistent_cache.write_failed"

let source_payload_of_binary (payload : source_binary_payload) : source_payload =
  let metadata_by_path = Hashtbl.create 512 in
  List.iter
    (fun meta ->
      if meta.path_key <> "" then
        Hashtbl.replace metadata_by_path meta.path_key meta)
    payload.source_metadata;
  { index = payload.source_index; metadata_by_path }

let read_source_json_payload ~(source_extensions : string list) ~(root : string)
    : source_payload option =
  match read_json (source_index_json_path ~root) with
  | None -> None
  | Some (`Assoc fields) when header_matches ~source_extensions fields -> (
      match field_bind "index" fields Workspace_index.of_yojson with
      | None -> None
      | Some index ->
          let metadata_by_path = Hashtbl.create 512 in
          (match field "files" fields with
          | Some (`List files) ->
              List.iter
                (fun json ->
                  match file_metadata_of_json json with
                  | None -> ()
                  | Some meta ->
                      if meta.path_key <> "" then
                        Hashtbl.replace metadata_by_path meta.path_key meta)
                files
          | _ -> ());
          Some { index; metadata_by_path })
  | Some _ ->
      Perf_stats.tick "persistent_cache.source_ignored";
      None

let read_source_payload ~(source_extensions : string list) ~(root : string) :
    source_payload option =
  let bin = source_index_bin_path ~root in
  if Sys.file_exists bin then
    match read_binary_payload ~source_extensions bin with
    | Some payload ->
        Perf_stats.tick "persistent_cache.source_binary_hit";
        Some (source_payload_of_binary payload)
    | None ->
        Perf_stats.tick "persistent_cache.source_binary_ignored";
        None
  else read_source_json_payload ~source_extensions ~root

let index_source_extensions_match ~(source_extensions : string list)
    (idx : Workspace_index.t) =
  match Workspace_index.to_yojson idx with
  | `Assoc fields ->
      let got =
        field "sourceExtensions" fields
        |> Option.map string_list_of_json
        |> Option.value ~default:[]
        |> List.sort_uniq String.compare
      in
      got = normalized_source_extensions source_extensions
  | _ -> false

let legacy_source_payload ~(source_extensions : string list) ~(root : string) :
    Workspace_index.t option =
  match
    Workspace_persistent_index.load_workspace_index ~source_extensions ~root
  with
  | Some idx when index_source_extensions_match ~source_extensions idx ->
      Perf_stats.tick "persistent_cache.source_legacy_hit";
      Some idx
  | _ -> None

let reconcile_source_index ~(source_extensions : string list)
    ~(paths : string list) ~(payload : source_payload option)
    ~(legacy_index : Workspace_index.t option) ~(root : string) :
    source_index_load =
  let current_paths = unique_regular_paths ~source_extensions paths in
  match (payload, legacy_index) with
  | None, None ->
      Perf_stats.tick "persistent_cache.source_miss";
      {
        index =
          Workspace_index.of_source_files ~source_extensions ~root
            ~paths:current_paths;
        loaded_from_cache = false;
        changed_paths = current_paths;
        pruned_paths = [];
      }
  | Some payload, _ ->
      let idx = payload.index in
      let wanted = Hashtbl.create (max 16 (List.length current_paths)) in
      List.iter
        (fun path -> Hashtbl.replace wanted (Uri_path.normalize_path_key path) path)
        current_paths;
      let pruned_paths = ref [] in
      Workspace_index.all_source_paths idx
      |> List.iter (fun path ->
             let key = Uri_path.normalize_path_key path in
             if key <> "" && not (Hashtbl.mem wanted key) then (
               if Workspace_index.apply_file_change idx ~path ~kind:Deleted then
                 pruned_paths := path :: !pruned_paths));
      let changed_paths = ref [] in
      List.iter
        (fun path ->
          let key = Uri_path.normalize_path_key path in
          let fresh =
            match Hashtbl.find_opt payload.metadata_by_path key with
            | Some meta -> metadata_matches ~source_extensions meta ~path
            | None -> false
          in
          if not fresh then (
            ignore
              (Workspace_index.apply_file_change idx ~path ~kind:Changed);
            changed_paths := path :: !changed_paths))
        current_paths;
      if !changed_paths = [] && !pruned_paths = [] then
        Perf_stats.tick "persistent_cache.source_hit"
      else Perf_stats.tick "persistent_cache.source_reconciled";
      {
        index = idx;
        loaded_from_cache = true;
        changed_paths = List.rev !changed_paths;
        pruned_paths = List.rev !pruned_paths;
      }
  | None, Some idx ->
      let changed_paths = ref [] in
      let pruned_paths = ref [] in
      let wanted = Hashtbl.create (max 16 (List.length current_paths)) in
      List.iter
        (fun path -> Hashtbl.replace wanted (Uri_path.normalize_path_key path) path)
        current_paths;
      Workspace_index.all_source_paths idx
      |> List.iter (fun path ->
             let key = Uri_path.normalize_path_key path in
             if key <> "" && not (Hashtbl.mem wanted key) then (
               if Workspace_index.apply_file_change idx ~path ~kind:Deleted then
                 pruned_paths := path :: !pruned_paths));
      List.iter
        (fun path ->
          ignore (Workspace_index.apply_file_change idx ~path ~kind:Changed);
          changed_paths := path :: !changed_paths)
        current_paths;
      {
        index = idx;
        loaded_from_cache = true;
        changed_paths = List.rev !changed_paths;
        pruned_paths = List.rev !pruned_paths;
      }

let load_or_build_source_index ~(root : string)
    ~(source_extensions : string list) ~(paths : string list) :
    source_index_load =
  let payload = read_source_payload ~source_extensions ~root in
  let legacy_index =
    match payload with
    | Some _ -> None
    | None -> legacy_source_payload ~source_extensions ~root
  in
  reconcile_source_index ~source_extensions ~paths ~payload ~legacy_index ~root

let save_source_index ~(root : string) ~(source_extensions : string list)
    (idx : Workspace_index.t) : unit =
  try
    let files =
      Workspace_index.all_source_paths idx
      |> List.filter_map (metadata_for_path ~source_extensions)
      |> List.sort (fun a b -> String.compare a.path_key b.path_key)
    in
    save_cache_version ~root ~source_extensions;
    save_binary_payload ~source_extensions (source_index_bin_path ~root)
      { source_index = idx; source_metadata = files }
  with _ -> Perf_stats.tick "persistent_cache.write_failed"

let json_of_pos (p : Ast.Loc.pos) =
  `Assoc
    [
      ("line", `Int p.line);
      ("col", `Int p.col);
      ("offset", `Int p.offset);
    ]

let pos_of_json = function
  | `Assoc fields -> (
      match
        ( field_bind "line" fields int_of_json,
          field_bind "col" fields int_of_json,
          field_bind "offset" fields int_of_json )
      with
      | Some line, Some col, Some offset -> Some { Ast.Loc.line; col; offset }
      | _ -> None)
  | _ -> None

let json_of_loc (loc : Ast.Loc.t) =
  `Assoc
    [
      ("file", json_of_option (fun s -> `String s) loc.file);
      ("start", json_of_pos loc.start_pos);
      ("end", json_of_pos loc.end_pos);
    ]

let loc_of_json = function
  | `Assoc fields -> (
      match
        ( field_bind "start" fields pos_of_json,
          field_bind "end" fields pos_of_json )
      with
      | Some start_pos, Some end_pos ->
          let file =
            field_bind "file" fields option_string_of_json
            |> Option.value ~default:None
          in
          Some { Ast.Loc.file; start_pos; end_pos }
      | _ -> None)
  | _ -> None

let string_of_external_kind = function
  | Workspace_symbol_metadata.ExternalLocal -> "local"
  | Workspace_symbol_metadata.ExternalDef -> "def"
  | Workspace_symbol_metadata.ExternalRef -> "ref"
  | Workspace_symbol_metadata.ExternalSystem -> "system"

let external_kind_of_string = function
  | "local" -> Some Workspace_symbol_metadata.ExternalLocal
  | "def" -> Some Workspace_symbol_metadata.ExternalDef
  | "ref" -> Some Workspace_symbol_metadata.ExternalRef
  | "system" -> Some Workspace_symbol_metadata.ExternalSystem
  | _ -> None

let string_of_decl_role = function
  | Workspace_symbol_metadata.RealDeclaration -> "realDeclaration"
  | Workspace_symbol_metadata.ExternalDefinition -> "externalDefinition"
  | Workspace_symbol_metadata.ExternalReferenceImport -> "externalReferenceImport"
  | Workspace_symbol_metadata.CompoolImport -> "compoolImport"
  | Workspace_symbol_metadata.UsageReference -> "usageReference"
  | Workspace_symbol_metadata.TypeUse -> "typeUse"
  | Workspace_symbol_metadata.MacroDefinition -> "macroDefinition"
  | Workspace_symbol_metadata.MacroUse -> "macroUse"

let decl_role_of_string = function
  | "realDeclaration" -> Some Workspace_symbol_metadata.RealDeclaration
  | "externalDefinition" -> Some Workspace_symbol_metadata.ExternalDefinition
  | "externalReferenceImport" ->
      Some Workspace_symbol_metadata.ExternalReferenceImport
  | "compoolImport" -> Some Workspace_symbol_metadata.CompoolImport
  | "usageReference" -> Some Workspace_symbol_metadata.UsageReference
  | "typeUse" -> Some Workspace_symbol_metadata.TypeUse
  | "macroDefinition" -> Some Workspace_symbol_metadata.MacroDefinition
  | "macroUse" -> Some Workspace_symbol_metadata.MacroUse
  | _ -> None

let string_of_jovial_kind = function
  | Workspace_symbol_metadata.JovialProgram -> "program"
  | Workspace_symbol_metadata.JovialModule -> "module"
  | Workspace_symbol_metadata.JovialCompool -> "compool"
  | Workspace_symbol_metadata.JovialCompoolImport -> "compoolImport"
  | Workspace_symbol_metadata.JovialItem -> "item"
  | Workspace_symbol_metadata.JovialTable -> "table"
  | Workspace_symbol_metadata.JovialBlock -> "block"
  | Workspace_symbol_metadata.JovialOverlay -> "overlay"
  | Workspace_symbol_metadata.JovialType -> "type"
  | Workspace_symbol_metadata.JovialProcedure -> "procedure"
  | Workspace_symbol_metadata.JovialFunction -> "function"
  | Workspace_symbol_metadata.JovialParameter -> "parameter"
  | Workspace_symbol_metadata.JovialField -> "field"
  | Workspace_symbol_metadata.JovialLabel -> "label"
  | Workspace_symbol_metadata.JovialDefine -> "define"
  | Workspace_symbol_metadata.JovialConstantItem -> "constantItem"
  | Workspace_symbol_metadata.JovialConstantTable -> "constantTable"
  | Workspace_symbol_metadata.JovialStatusConstant -> "statusConstant"
  | Workspace_symbol_metadata.JovialBuiltinType -> "builtinType"
  | Workspace_symbol_metadata.JovialUnknownSymbol -> "unknown"

let jovial_kind_of_string = function
  | "program" -> Some Workspace_symbol_metadata.JovialProgram
  | "module" -> Some Workspace_symbol_metadata.JovialModule
  | "compool" -> Some Workspace_symbol_metadata.JovialCompool
  | "compoolImport" -> Some Workspace_symbol_metadata.JovialCompoolImport
  | "item" -> Some Workspace_symbol_metadata.JovialItem
  | "table" -> Some Workspace_symbol_metadata.JovialTable
  | "block" -> Some Workspace_symbol_metadata.JovialBlock
  | "overlay" -> Some Workspace_symbol_metadata.JovialOverlay
  | "type" -> Some Workspace_symbol_metadata.JovialType
  | "procedure" -> Some Workspace_symbol_metadata.JovialProcedure
  | "function" -> Some Workspace_symbol_metadata.JovialFunction
  | "parameter" -> Some Workspace_symbol_metadata.JovialParameter
  | "field" -> Some Workspace_symbol_metadata.JovialField
  | "label" -> Some Workspace_symbol_metadata.JovialLabel
  | "define" -> Some Workspace_symbol_metadata.JovialDefine
  | "constantItem" -> Some Workspace_symbol_metadata.JovialConstantItem
  | "constantTable" -> Some Workspace_symbol_metadata.JovialConstantTable
  | "statusConstant" -> Some Workspace_symbol_metadata.JovialStatusConstant
  | "builtinType" -> Some Workspace_symbol_metadata.JovialBuiltinType
  | "unknown" -> Some Workspace_symbol_metadata.JovialUnknownSymbol
  | _ -> None

let json_of_metadata (m : Workspace_symbol_metadata.jovial_symbol_metadata) =
  `Assoc
    [
      ("jovialKind", `String (string_of_jovial_kind m.jovial_kind));
      ("externalKind", `String (string_of_external_kind m.external_kind));
      ("declRole", `String (string_of_decl_role m.decl_role));
      ("isConstant", `Bool m.is_constant);
      ("isReadonly", `Bool m.is_readonly);
      ("isInline", `Bool m.is_inline);
      ("isImported", `Bool m.is_imported);
      ("isExported", `Bool m.is_exported);
      ("sourceKeyword", json_of_option (fun s -> `String s) m.source_keyword);
      ("hasBody", json_of_option (fun b -> `Bool b) m.has_body);
    ]

let metadata_of_json = function
  | `Assoc fields -> (
      match
        ( Option.bind (field_bind "jovialKind" fields string_of_json)
            jovial_kind_of_string,
          Option.bind (field_bind "externalKind" fields string_of_json)
            external_kind_of_string,
          Option.bind (field_bind "declRole" fields string_of_json)
            decl_role_of_string )
      with
      | Some jovial_kind, Some external_kind, Some decl_role ->
          let is_constant =
            field_bind "isConstant" fields bool_of_json
            |> Option.value ~default:false
          in
          let is_readonly =
            field_bind "isReadonly" fields bool_of_json
            |> Option.value ~default:is_constant
          in
          let is_inline =
            field_bind "isInline" fields bool_of_json
            |> Option.value ~default:false
          in
          let is_imported =
            field_bind "isImported" fields bool_of_json
            |> Option.value ~default:false
          in
          let is_exported =
            field_bind "isExported" fields bool_of_json
            |> Option.value ~default:false
          in
          let source_keyword =
            field_bind "sourceKeyword" fields option_string_of_json
            |> Option.value ~default:None
          in
          let has_body =
            match field "hasBody" fields with
            | Some `Null | None -> None
            | Some json -> bool_of_json json
          in
          Some
            {
              Workspace_symbol_metadata.default_metadata with
              jovial_kind;
              external_kind;
              decl_role;
              is_constant;
              is_readonly;
              is_inline;
              is_imported;
              is_exported;
              source_keyword;
              has_body;
            }
      | _ -> None)
  | _ -> None

let json_of_quick_nav_entry (entry : quick_nav_entry) =
  `Assoc
    [
      ("uri", `String (Uri_path.docuri_to_string entry.qn_uri));
      ("name", `String entry.qn_name);
      ("key", `String entry.qn_key);
      ("loc", json_of_loc entry.qn_loc);
      ("kind", `Int entry.qn_kind);
      ("container", json_of_option (fun s -> `String s) entry.qn_container);
      ("metadata", json_of_metadata entry.qn_metadata);
    ]

let quick_nav_entry_of_json = function
  | `Assoc fields -> (
      match
        ( Option.bind (field_bind "uri" fields string_of_json)
            Uri_path.docuri_of_string,
          field_bind "name" fields string_of_json,
          field_bind "key" fields string_of_json,
          field_bind "loc" fields loc_of_json,
          field_bind "kind" fields int_of_json )
      with
      | Some qn_uri, Some qn_name, Some qn_key, Some qn_loc, Some qn_kind ->
          let qn_container =
            field_bind "container" fields option_string_of_json
            |> Option.value ~default:None
          in
          let qn_metadata =
            field_bind "metadata" fields metadata_of_json
            |> Option.value ~default:Workspace_symbol_metadata.default_metadata
          in
          Some
            {
              qn_uri;
              qn_name;
              qn_key;
              qn_loc;
              qn_kind;
              qn_container;
              qn_metadata;
            }
      | _ -> None)
  | _ -> None

let skeleton_payload_of_binary (payload : skeleton_binary_payload) :
    skeleton_payload option =
  if payload.skeleton_max_bytes < 0 then None
  else
    let skeleton_entries = Hashtbl.create 512 in
    List.iter
      (fun (meta, entries) ->
        if meta.path_key <> "" then
          Hashtbl.replace skeleton_entries meta.path_key (meta, entries))
      payload.skeleton_items;
    Some { skeleton_entries }

let read_skeleton_json_payload ~(source_extensions : string list)
    ~(max_bytes : int) ~(root : string) : skeleton_payload option =
  match read_json (skeleton_index_json_path ~root) with
  | None -> None
  | Some (`Assoc fields)
    when header_matches ~source_extensions fields
         && field_bind "maxBytes" fields int_of_json = Some max_bytes
    ->
      let skeleton_entries = Hashtbl.create 512 in
      (match field "entries" fields with
      | Some (`List entries) ->
          List.iter
            (function
              | `Assoc entry_fields -> (
                  match
                    ( field_bind "file" entry_fields file_metadata_of_json,
                      field "symbols" entry_fields )
                  with
                  | Some meta, Some (`List symbols) ->
                      let symbols =
                        symbols |> List.filter_map quick_nav_entry_of_json
                      in
                      if meta.path_key <> "" then
                        Hashtbl.replace skeleton_entries meta.path_key
                          (meta, symbols)
                  | _ -> ())
              | _ -> ())
            entries
      | _ -> ());
      Some { skeleton_entries }
  | Some _ ->
      Perf_stats.tick "persistent_cache.skeleton_ignored";
      None

let read_skeleton_payload ~(source_extensions : string list) ~(max_bytes : int)
    ~(root : string) : skeleton_payload option =
  let bin = skeleton_index_bin_path ~root in
  if Sys.file_exists bin then
    match read_binary_payload ~source_extensions bin with
    | Some payload when payload.skeleton_max_bytes = max_bytes -> (
        match skeleton_payload_of_binary payload with
        | Some payload ->
            Perf_stats.tick "persistent_cache.skeleton_binary_hit";
            Some payload
        | None ->
            Perf_stats.tick "persistent_cache.skeleton_binary_ignored";
            None)
    | Some _ ->
        Perf_stats.tick "persistent_cache.skeleton_binary_ignored";
        None
    | None ->
        Perf_stats.tick "persistent_cache.skeleton_binary_ignored";
        None
  else read_skeleton_json_payload ~source_extensions ~max_bytes ~root

let skeleton_entries (cache : skeleton_cache) ~(path : string) :
    quick_nav_entry list option =
  Hashtbl.find_opt cache.entries_by_path (Uri_path.normalize_path_key path)

let load_skeleton_cache ~(root : string) ~(source_extensions : string list)
    ~(max_bytes : int) ~(paths : string list) : skeleton_cache =
  let entries_by_path = Hashtbl.create 512 in
  let wanted = Hashtbl.create (max 16 (List.length paths)) in
  List.iter
    (fun path -> Hashtbl.replace wanted (Uri_path.normalize_path_key path) path)
    paths;
  (match read_skeleton_payload ~source_extensions ~max_bytes ~root with
  | None -> Perf_stats.tick "persistent_cache.skeleton_miss"
  | Some payload ->
      Hashtbl.iter
        (fun key (meta, entries) ->
          match Hashtbl.find_opt wanted key with
          | Some path when metadata_matches ~source_extensions meta ~path ->
              Hashtbl.replace entries_by_path key entries;
              Perf_stats.tick "persistent_cache.skeleton_entry_hit"
          | _ -> ())
        payload.skeleton_entries);
  { entries_by_path }

let _json_of_skeleton_payload ~(source_extensions : string list)
    ~(max_bytes : int) (payload : skeleton_payload) =
  let entries =
    Hashtbl.fold
      (fun _ (meta, entries) acc ->
        `Assoc
          [
            ("file", json_of_file_metadata meta);
            ( "symbols",
              `List (List.map json_of_quick_nav_entry entries) );
          ]
        :: acc)
      payload.skeleton_entries []
    |> List.sort (fun a b ->
           let path_key = function
             | `Assoc fields -> (
                 match field_bind "file" fields file_metadata_of_json with
                 | Some meta -> meta.path_key
                 | None -> "")
             | _ -> ""
           in
           String.compare (path_key a) (path_key b))
  in
  cache_header ~source_extensions
    [ ("maxBytes", `Int max_bytes); ("entries", `List entries) ]

let retained_skeleton_payload ~(source_extensions : string list)
    (payload : skeleton_payload) : skeleton_payload =
  let retained = Hashtbl.create (Hashtbl.length payload.skeleton_entries) in
  Hashtbl.iter
    (fun key (meta, entries) ->
      if metadata_matches ~source_extensions meta ~path:meta.path then
        Hashtbl.replace retained key (meta, entries))
    payload.skeleton_entries;
  { skeleton_entries = retained }

let load_retained_skeleton_payload ~(root : string)
    ~(source_extensions : string list) ~(max_bytes : int) : skeleton_payload =
  match read_skeleton_payload ~source_extensions ~max_bytes ~root with
  | Some payload -> retained_skeleton_payload ~source_extensions payload
  | None -> { skeleton_entries = Hashtbl.create 512 }

let apply_skeleton_entries ~(source_extensions : string list)
    (payload : skeleton_payload)
    (entries_by_path : (string * quick_nav_entry list) list) : unit =
  List.iter
    (fun (path, entries) ->
      match metadata_for_path ~source_extensions path with
      | None -> Hashtbl.remove payload.skeleton_entries (Uri_path.normalize_path_key path)
      | Some meta -> Hashtbl.replace payload.skeleton_entries meta.path_key (meta, entries))
    entries_by_path

let write_skeleton_payload ~(root : string) ~(source_extensions : string list)
    ~(max_bytes : int) (payload : skeleton_payload) : unit =
  let entries =
    Hashtbl.fold
      (fun _ (meta, entries) acc -> (meta, entries) :: acc)
      payload.skeleton_entries []
    |> List.sort (fun (a, _) (b, _) -> String.compare a.path_key b.path_key)
  in
  save_cache_version ~root ~source_extensions;
  save_binary_payload ~source_extensions (skeleton_index_bin_path ~root)
    { skeleton_max_bytes = max_bytes; skeleton_items = entries }

type skeleton_write_buffer = {
  swb_root : string;
  swb_source_extensions : string list;
  swb_max_bytes : int;
  swb_payload : skeleton_payload;
  mutable swb_dirty_count : int;
}

let skeleton_write_buffers : (string, skeleton_write_buffer) Hashtbl.t =
  Hashtbl.create 8

let skeleton_write_batch_size = 64

let skeleton_buffer_key ~(root : string) ~(source_extensions : string list)
    ~(max_bytes : int) : string =
  String.concat "\000"
    (root :: string_of_int max_bytes :: normalized_source_extensions source_extensions)

let flush_skeleton_buffer (buffer : skeleton_write_buffer) : unit =
  if buffer.swb_dirty_count <= 0 then ()
  else (
    write_skeleton_payload ~root:buffer.swb_root
      ~source_extensions:buffer.swb_source_extensions
      ~max_bytes:buffer.swb_max_bytes buffer.swb_payload;
    buffer.swb_dirty_count <- 0;
    Perf_stats.tick "persistent_cache.skeleton_batch_flush")

let skeleton_write_buffer ~(root : string) ~(source_extensions : string list)
    ~(max_bytes : int) : skeleton_write_buffer =
  let key = skeleton_buffer_key ~root ~source_extensions ~max_bytes in
  match Hashtbl.find_opt skeleton_write_buffers key with
  | Some buffer -> buffer
  | None ->
      let payload =
        load_retained_skeleton_payload ~root ~source_extensions ~max_bytes
      in
      let buffer =
        {
          swb_root = root;
          swb_source_extensions = normalized_source_extensions source_extensions;
          swb_max_bytes = max_bytes;
          swb_payload = payload;
          swb_dirty_count = 0;
        }
      in
      Hashtbl.replace skeleton_write_buffers key buffer;
      buffer

let save_skeleton_entries_immediate ~(root : string)
    ~(source_extensions : string list) ~(max_bytes : int)
    ~(entries_by_path : (string * quick_nav_entry list) list) : unit =
  if entries_by_path = [] then ()
  else
    let key = skeleton_buffer_key ~root ~source_extensions ~max_bytes in
    Hashtbl.remove skeleton_write_buffers key;
    let payload =
      load_retained_skeleton_payload ~root ~source_extensions ~max_bytes
    in
    apply_skeleton_entries ~source_extensions payload entries_by_path;
    write_skeleton_payload ~root ~source_extensions ~max_bytes payload

let save_skeleton_entry ~(root : string) ~(source_extensions : string list)
    ~(max_bytes : int) ~(path : string) ~(entries : quick_nav_entry list) :
    unit =
  try
    save_skeleton_entries_immediate ~root ~source_extensions ~max_bytes
      ~entries_by_path:[ (path, entries) ]
  with _ -> Perf_stats.tick "persistent_cache.write_failed"

let save_skeleton_entries_buffered ~(root : string)
    ~(source_extensions : string list) ~(max_bytes : int)
    ~(entries_by_path : (string * quick_nav_entry list) list) : unit =
  if entries_by_path = [] then ()
  else
    try
      let buffer = skeleton_write_buffer ~root ~source_extensions ~max_bytes in
      apply_skeleton_entries ~source_extensions buffer.swb_payload entries_by_path;
      buffer.swb_dirty_count <-
        buffer.swb_dirty_count + List.length entries_by_path;
      if buffer.swb_dirty_count >= skeleton_write_batch_size then
        flush_skeleton_buffer buffer
    with _ -> Perf_stats.tick "persistent_cache.write_failed"

let flush_skeleton_entries ~(root : string) ~(source_extensions : string list)
    ~(max_bytes : int) : unit =
  try
    let key = skeleton_buffer_key ~root ~source_extensions ~max_bytes in
    match Hashtbl.find_opt skeleton_write_buffers key with
    | None -> ()
    | Some buffer -> flush_skeleton_buffer buffer
  with _ -> Perf_stats.tick "persistent_cache.write_failed"

let module_summary_payload_of_binary
    (payload : module_summary_binary_payload) : module_summary_payload =
  let module_summaries = Hashtbl.create 512 in
  List.iter
    (fun (meta, summary) ->
      if meta.path_key <> "" then
        Hashtbl.replace module_summaries meta.path_key (meta, summary))
    payload.module_summary_items;
  { module_summaries }

let read_module_summary_json_payload ~(source_extensions : string list)
    ~(root : string) : module_summary_payload option =
  match read_json (module_summary_json_path ~root) with
  | None -> None
  | Some (`Assoc fields) when header_matches ~source_extensions fields ->
      let module_summaries = Hashtbl.create 512 in
      (match field "entries" fields with
      | Some (`List entries) ->
          List.iter
            (function
              | `Assoc entry_fields -> (
                  match
                    ( field_bind "file" entry_fields file_metadata_of_json,
                      field_bind "summary" entry_fields Module_summary.of_yojson )
                  with
                  | Some meta, Some summary when meta.path_key <> "" ->
                      Hashtbl.replace module_summaries meta.path_key
                        (meta, summary)
                  | _ -> ())
              | _ -> ())
            entries
      | _ -> ());
      Some { module_summaries }
  | Some _ ->
      Perf_stats.tick "persistent_cache.summary_ignored";
      None

let read_module_summary_payload ~(source_extensions : string list)
    ~(root : string) : module_summary_payload option =
  let bin = module_summary_bin_path ~root in
  if Sys.file_exists bin then
    match read_binary_payload ~source_extensions bin with
    | Some payload ->
        Perf_stats.tick "persistent_cache.summary_binary_hit";
        Some (module_summary_payload_of_binary payload)
    | None ->
        Perf_stats.tick "persistent_cache.summary_binary_ignored";
        None
  else read_module_summary_json_payload ~source_extensions ~root

let _reverse_importers_json_of_entries
    (entries : (string, file_metadata * Module_summary.t) Hashtbl.t) :
    Yojson.Safe.t =
  let reverse = Hashtbl.create 128 in
  let add compool path_key =
    let key = String.uppercase_ascii (String.trim compool) in
    if key <> "" then
      let prev = Option.value (Hashtbl.find_opt reverse key) ~default:[] in
      if not (List.mem path_key prev) then
        Hashtbl.replace reverse key (path_key :: prev)
  in
  Hashtbl.iter
    (fun path_key (_meta, summary) ->
      List.iter
        (fun compool -> add compool path_key)
        summary.Module_summary.imported_compools)
    entries;
  let items =
    Hashtbl.fold
      (fun compool importers acc ->
        `Assoc
          [
            ("compool", `String compool);
            ( "importers",
              `List
                (List.map
                   (fun path_key -> `String path_key)
                   (List.sort_uniq String.compare importers)) );
          ]
        :: acc)
      reverse []
    |> List.sort (fun a b ->
           let compool = function
             | `Assoc fields -> (
                 match field_bind "compool" fields string_of_json with
                 | Some s -> s
                 | None -> "")
             | _ -> ""
           in
           String.compare (compool a) (compool b))
  in
  `List items

let _json_of_module_summary_payload ~(source_extensions : string list)
    (payload : module_summary_payload) =
  let entries =
    Hashtbl.fold
      (fun _ (meta, summary) acc ->
        `Assoc
          [
            ("file", json_of_file_metadata meta);
            ("summary", Module_summary.to_yojson summary);
          ]
        :: acc)
      payload.module_summaries []
    |> List.sort (fun a b ->
           let path_key = function
             | `Assoc fields -> (
                 match field_bind "file" fields file_metadata_of_json with
                 | Some meta -> meta.path_key
                 | None -> "")
             | _ -> ""
           in
           String.compare (path_key a) (path_key b))
  in
  cache_header ~source_extensions
    [
      ("entries", `List entries);
      ("reverseImporters", _reverse_importers_json_of_entries payload.module_summaries);
    ]

let load_module_summary_cache ~(root : string)
    ~(source_extensions : string list) ~(paths : string list) :
    module_summary_cache =
  let wanted = Hashtbl.create (max 16 (List.length paths)) in
  List.iter
    (fun path -> Hashtbl.replace wanted (Uri_path.normalize_path_key path) path)
    paths;
  let module_summary_entries =
    match read_module_summary_payload ~source_extensions ~root with
    | None ->
        Perf_stats.tick "persistent_cache.summary_miss";
        []
    | Some payload ->
        Hashtbl.fold
          (fun path_key (meta, summary) acc ->
            match Hashtbl.find_opt wanted path_key with
            | Some path when metadata_matches ~source_extensions meta ~path ->
                Perf_stats.tick "persistent_cache.summary_entry_hit";
                {
                  msc_path = path;
                  msc_path_key = path_key;
                  msc_summary = summary;
                  msc_authority = ModuleSummaryMetadataValidated;
                }
                :: acc
            | _ -> acc)
          payload.module_summaries []
  in
  { summary_entries = module_summary_entries }

let module_summary_entries (cache : module_summary_cache) :
    module_summary_cache_entry list =
  cache.summary_entries

let save_module_summary_entry ~(root : string)
    ~(source_extensions : string list) ~(path : string)
    ~(summary : Module_summary.t) : unit =
  try
    let payload =
      match read_module_summary_payload ~source_extensions ~root with
      | Some payload -> payload
      | None -> { module_summaries = Hashtbl.create 512 }
    in
    let retained = Hashtbl.create (Hashtbl.length payload.module_summaries + 1) in
    Hashtbl.iter
      (fun key (meta, summary) ->
        if metadata_matches ~source_extensions meta ~path:meta.path then
          Hashtbl.replace retained key (meta, summary))
      payload.module_summaries;
    (match
       metadata_for_path ~content_hash:summary.Module_summary.content_hash
         ~source_extensions path
     with
    | None -> Hashtbl.remove retained (Uri_path.normalize_path_key path)
    | Some meta -> Hashtbl.replace retained meta.path_key (meta, summary));
    let payload = { module_summaries = retained } in
    let entries =
      Hashtbl.fold
        (fun _ (meta, summary) acc -> (meta, summary) :: acc)
        payload.module_summaries []
      |> List.sort (fun (a, _) (b, _) -> String.compare a.path_key b.path_key)
    in
    save_cache_version ~root ~source_extensions;
    save_binary_payload ~source_extensions (module_summary_bin_path ~root)
      { module_summary_items = entries }
  with _ -> Perf_stats.tick "persistent_cache.write_failed"
