(* Module overview: Loads and saves persistent workspace index snapshots between server runs. *)

type file_fingerprint = {
  uri : string;
  size : int;
  mtime_ns : int64;
  content_hash : string option;
  parser_version : string;
  indexer_version : string;
}

type 'a binary_envelope = {
  be_parser_version : string;
  be_indexer_version : string;
  be_source_extensions : string list;
  be_payload : 'a;
}

let parser_version = "jovial-menhir-v1"
let indexer_version = "jovial-indexer-v1"

let cache_dir ~(root : string) = Workspace_storage_layout.index_dir ~root

let json_path ~(root : string) name = Filename.concat (cache_dir ~root) name
let bin_path ~(root : string) name = Filename.concat (cache_dir ~root) name
let files_json_path ~(root : string) = json_path ~root "files.json"
let files_bin_path ~(root : string) = bin_path ~root "files.bin"
let symbols_json_path ~(root : string) = json_path ~root "symbols.json"
let symbols_bin_path ~(root : string) = bin_path ~root "symbols.bin"
let refs_json_path ~(root : string) = json_path ~root "refs.json"
let refs_bin_path ~(root : string) = bin_path ~root "refs.bin"
let scopes_json_path ~(root : string) = json_path ~root "scopes.json"
let scopes_bin_path ~(root : string) = bin_path ~root "scopes.bin"
let deps_json_path ~(root : string) = json_path ~root "deps.json"
let deps_bin_path ~(root : string) = bin_path ~root "deps.bin"
let macros_json_path ~(root : string) = json_path ~root "macros.json"
let macros_bin_path ~(root : string) = bin_path ~root "macros.bin"
let diagnostics_json_path ~(root : string) = json_path ~root "diagnostics.json"
let diagnostics_bin_path ~(root : string) = bin_path ~root "diagnostics.bin"
let snapshot_bin_path ~(root : string) = bin_path ~root "snapshot.bin"

let binary_magic = "JOVIAL_LSP_INDEX_BIN_V2\n"

let ensure_dir path =
  let rec loop path =
    if path = "" || Sys.file_exists path then ()
    else (
      loop (Filename.dirname path);
      try Unix.mkdir path 0o755 with _ -> ())
  in
  loop path

let read_json path =
  try Some (Yojson.Safe.from_file path) with _ -> None

let normalized_source_extensions (source_extensions : string list) =
  Source_file.normalize_extensions source_extensions |> List.sort_uniq String.compare

let source_extensions_of_index (idx : Workspace_index.t) : string list =
  match Workspace_index.to_yojson idx with
  | `Assoc fields -> (
      match List.assoc_opt "sourceExtensions" fields with
      | Some (`List values) ->
          values
          |> List.filter_map (function `String s -> Some s | _ -> None)
          |> normalized_source_extensions
      | _ -> [])
  | _ -> []

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
          if
            envelope.be_parser_version = parser_version
            && envelope.be_indexer_version = indexer_version
            && envelope.be_source_extensions
               = normalized_source_extensions source_extensions
          then Some envelope.be_payload
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
            be_parser_version = parser_version;
            be_indexer_version = indexer_version;
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

let raw_binary_magic = "JOVIAL_LSP_INDEX_RAW_V1\n"

let save_raw_binary path payload =
  ensure_dir (Filename.dirname path);
  let tmp = path ^ ".tmp" in
  let oc = open_out_bin tmp in
  try
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc raw_binary_magic;
        Marshal.to_channel oc payload [ Marshal.No_sharing ]);
    Sys.rename tmp path
  with exn ->
    close_out_noerr oc;
    (try Sys.remove tmp with _ -> ());
    raise exn

let debug_json_enabled () =
  Env_utils.flag "JOVIAL_PERSISTENT_INDEX_DEBUG_JSON" ~default:false

let save_json path json =
  ensure_dir (Filename.dirname path);
  let tmp = path ^ ".tmp" in
  let oc = open_out_bin tmp in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> Yojson.Safe.to_channel oc json);
  Sys.rename tmp path

let fingerprint_path path : file_fingerprint option =
  try
    let st = Unix.stat path in
    let uri =
      match Uri_path.docuri_of_path path with
      | Some u -> Uri_path.docuri_to_string u
      | None -> Uri_path.file_uri_of_path path
    in
    Some
      {
        uri;
        size = st.Unix.st_size;
        mtime_ns = Int64.of_float (st.Unix.st_mtime *. 1_000_000_000.0);
        content_hash = None;
        parser_version;
        indexer_version;
      }
  with _ -> None

let load_workspace_index ~(source_extensions : string list) ~(root : string) :
    Workspace_index.t option =
  let bin = files_bin_path ~root in
  if Sys.file_exists bin then read_binary_payload ~source_extensions bin
  else
    match read_json (files_json_path ~root) with
    | None -> None
    | Some json -> (
        match Workspace_index.of_yojson json with
        | None -> None
        | Some idx ->
            ignore source_extensions;
            Some idx)

let save_workspace_index ~(root : string) (idx : Workspace_index.t) : unit =
  try
    let source_extensions = source_extensions_of_index idx in
    save_binary_payload ~source_extensions (files_bin_path ~root) idx
  with _ -> ()

let json_of_option f = function None -> `Null | Some v -> f v

let json_of_pos (p : Ast.Loc.pos) =
  `Assoc
    [
      ("line", `Int p.line);
      ("col", `Int p.col);
      ("offset", `Int p.offset);
    ]

let json_of_loc (loc : Ast.Loc.t) =
  `Assoc
    [
      ("file", json_of_option (fun s -> `String s) loc.file);
      ("start", json_of_pos loc.start_pos);
      ("end", json_of_pos loc.end_pos);
    ]

let string_of_symbol_kind = function
  | Skeleton_index.Program -> "Program"
  | Skeleton_index.Module -> "Module"
  | Skeleton_index.Compool -> "Compool"
  | Skeleton_index.Procedure -> "Procedure"
  | Skeleton_index.Function -> "Function"
  | Skeleton_index.Item -> "Item"
  | Skeleton_index.Table -> "Table"
  | Skeleton_index.Block -> "Block"
  | Skeleton_index.Type -> "Type"
  | Skeleton_index.Label -> "Label"
  | Skeleton_index.Define -> "Define"
  | Skeleton_index.ExternalDef -> "ExternalDef"
  | Skeleton_index.ExternalRef -> "ExternalRef"

let string_of_external_kind = function
  | `Def -> "Def"
  | `Ref -> "Ref"
  | `Local -> "Local"
  | `System -> "System"

let json_of_symbol_record (s : Symbol_index.symbol_record) =
  `Assoc
    [
      ("id", `Int (Symbol_id.to_int s.id));
      ("name", `String s.name);
      ("normalizedName", `String s.normalized_name);
      ("kind", `String (string_of_symbol_kind s.kind));
      ("declaration", json_of_loc s.declaration);
      ("definition", json_of_option json_of_loc s.definition);
      ("containerScope", `Int s.container_scope);
      ("exported", `Bool s.exported);
      ("externalKind", `String (string_of_external_kind s.external_kind));
      ( "jovialClassification",
        `String
          (Workspace_symbol_metadata.symbol_kind_label
             s.metadata.jovial_kind) );
      ( "declarationRole",
        `String
          (Workspace_symbol_metadata.decl_role_label s.metadata.decl_role) );
      ( "typeInfo",
        json_of_option
          (fun (info : Symbol_index.type_info) ->
            `Assoc [ ("display", `String info.display) ])
          s.type_info );
      ("docs", json_of_option (fun docs -> `String docs) s.docs);
    ]

let string_of_occurrence_kind = function
  | Reference_index.Declaration -> "Declaration"
  | Reference_index.Definition -> "Definition"
  | Reference_index.Reference -> "Reference"
  | Reference_index.Read -> "Read"
  | Reference_index.Write -> "Write"
  | Reference_index.Call -> "Call"
  | Reference_index.TypeUse -> "TypeUse"
  | Reference_index.ImportUse -> "ImportUse"
  | Reference_index.MacroUse -> "MacroUse"

let string_of_confidence = function
  | `Exact -> "Exact"
  | `Likely -> "Likely"
  | `Unresolved -> "Unresolved"

let json_of_occurrence (o : Reference_index.occurrence) =
  `Assoc
    [
      ("symbolId", json_of_option (fun id -> `Int (Symbol_id.to_int id)) o.symbol_id);
      ("name", `String o.name);
      ("normalizedName", `String o.normalized_name);
      ("loc", json_of_loc o.loc);
      ("scopeId", `Int o.scope_id);
      ("kind", `String (string_of_occurrence_kind o.kind));
      ("confidence", `String (string_of_confidence o.confidence));
    ]

let string_of_scope_kind = function
  | Scope_graph.SystemScope -> "SystemScope"
  | Scope_graph.CompoolScope -> "CompoolScope"
  | Scope_graph.ModuleScope -> "ModuleScope"
  | Scope_graph.ModuleBodyScope -> "ModuleBodyScope"
  | Scope_graph.ProcedureScope -> "ProcedureScope"
  | Scope_graph.FunctionScope -> "FunctionScope"
  | Scope_graph.BlockScope -> "BlockScope"
  | Scope_graph.TableScope -> "TableScope"
  | Scope_graph.TypeScope -> "TypeScope"
  | Scope_graph.LoopScope -> "LoopScope"
  | Scope_graph.MacroScope -> "MacroScope"

let json_of_scope (s : Scope_graph.scope) =
  let json_of_symbol_binding (binding : Scope_graph.symbol_binding) =
    `Assoc
      [
        ("symbolId", `Int (Symbol_id.to_int binding.symbol_id));
        ("normalizedName", `String binding.normalized_name);
      ]
  in
  `Assoc
    [
      ("id", `Int s.id);
      ("parent", json_of_option (fun id -> `Int id) s.parent);
      ("kind", `String (string_of_scope_kind s.kind));
      ("name", json_of_option (fun name -> `String name) s.name);
      ("loc", json_of_loc s.loc);
      ("symbols", `List (List.map (fun id -> `Int (Symbol_id.to_int id)) s.symbols));
      ("symbolBindings", `List (List.map json_of_symbol_binding s.symbol_bindings));
      ("imports", `List (List.map (fun id -> `Int id) s.imports));
    ]

let string_of_edge_kind = function
  | Dependency_graph.ICompoolImport -> "ICompoolImport"
  | Dependency_graph.ICopyInclude -> "ICopyInclude"
  | Dependency_graph.DefExport -> "DefExport"
  | Dependency_graph.RefImport -> "RefImport"
  | Dependency_graph.DefineUse -> "DefineUse"
  | Dependency_graph.TypeUse -> "TypeUse"
  | Dependency_graph.ProcedureCall -> "ProcedureCall"
  | Dependency_graph.TableFieldUse -> "TableFieldUse"

let json_of_edge (e : Dependency_graph.edge) =
  `Assoc
    [
      ("sourceUri", `String e.source_uri);
      ("target", `String e.target);
      ("targetUri", json_of_option (fun uri -> `String uri) e.target_uri);
      ("kind", `String (string_of_edge_kind e.kind));
    ]

let json_of_define ~(uri : string) (d : Skeleton_index.symbol_decl) =
  `Assoc
    [
      ("uri", `String uri);
      ("name", `String d.name);
      ("normalizedName", `String d.normalized_name);
      ("loc", json_of_loc d.loc);
      ("scopeId", `Int d.scope_id);
    ]

let json_of_file_diagnostics (uri, state) =
  `Assoc
    [
      ("uri", `String uri);
      ( "version",
        json_of_option (fun version -> `Int version) state.Workspace_snapshot.lsp_version
      );
      ("rev", `Int state.rev);
      ( "diagnostics",
        `List (List.map Lsp.Types.Diagnostic.yojson_of_t state.diagnostics) );
    ]

let save_snapshot_index ~(root : string) (snap : Workspace_snapshot.snapshot) :
    unit =
  try
    let files = Workspace_snapshot.UriMap.bindings snap.files in
    let symbols = Symbol_index.all snap.symbols in
    let refs = Reference_index.all snap.refs in
    let scopes = Scope_graph.scopes snap.scopes in
    let deps = Dependency_graph.edges snap.deps in
    let macro_entries =
      files
      |> List.concat_map (fun (uri, state) ->
             match state.Workspace_snapshot.skeleton with
             | None -> []
             | Some sk -> List.map (json_of_define ~uri) sk.defines)
    in
    save_raw_binary (snapshot_bin_path ~root) snap;
    save_raw_binary (symbols_bin_path ~root) (snap.generation, symbols);
    save_raw_binary (refs_bin_path ~root) (snap.generation, refs);
    save_raw_binary (scopes_bin_path ~root) (snap.generation, scopes);
    save_raw_binary (deps_bin_path ~root) (snap.generation, deps);
    save_raw_binary (macros_bin_path ~root) (snap.generation, macro_entries);
    save_raw_binary (diagnostics_bin_path ~root) (snap.generation, files);
    if debug_json_enabled () then (
      save_json (symbols_json_path ~root)
        (`Assoc
          [
            ("generation", `Int snap.generation);
            ("symbols", `List (List.map json_of_symbol_record symbols));
          ]);
      save_json (refs_json_path ~root)
        (`Assoc
          [
            ("generation", `Int snap.generation);
            ("refs", `List (List.map json_of_occurrence refs));
          ]);
      save_json (scopes_json_path ~root)
        (`Assoc
          [
            ("generation", `Int snap.generation);
            ("scopes", `List (List.map json_of_scope scopes));
          ]);
      save_json (deps_json_path ~root)
        (`Assoc
          [
            ("generation", `Int snap.generation);
            ("deps", `List (List.map json_of_edge deps));
          ]);
      save_json (macros_json_path ~root)
        (`Assoc
          [ ("generation", `Int snap.generation); ("macros", `List macro_entries) ]);
      save_json (diagnostics_json_path ~root)
        (`Assoc
          [
            ("generation", `Int snap.generation);
            ("files", `List (List.map json_of_file_diagnostics files));
          ]))
  with _ -> ()
