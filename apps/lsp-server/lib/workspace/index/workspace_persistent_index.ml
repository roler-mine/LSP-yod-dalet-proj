(* Module overview: Loads and saves persistent workspace index snapshots between server runs. *)

type file_fingerprint = {
  uri : string;
  size : int;
  mtime_ns : int64;
  content_hash : string option;
  parser_version : string;
  indexer_version : string;
}

let parser_version = "jovial-menhir-v1"
let indexer_version = "jovial-indexer-v1"

let cache_dir ~(root : string) =
  Filename.concat (Filename.concat root ".jovial-lsp") "index"

let json_path ~(root : string) name = Filename.concat (cache_dir ~root) name
let files_json_path ~(root : string) = json_path ~root "files.json"
let symbols_json_path ~(root : string) = json_path ~root "symbols.json"
let refs_json_path ~(root : string) = json_path ~root "refs.json"
let scopes_json_path ~(root : string) = json_path ~root "scopes.json"
let deps_json_path ~(root : string) = json_path ~root "deps.json"
let macros_json_path ~(root : string) = json_path ~root "macros.json"
let diagnostics_json_path ~(root : string) = json_path ~root "diagnostics.json"

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
  match read_json (files_json_path ~root) with
  | None -> None
  | Some json -> (
      match Workspace_index.of_yojson json with
      | None -> None
      | Some idx ->
          ignore source_extensions;
          Some idx)

let save_workspace_index ~(root : string) (idx : Workspace_index.t) : unit =
  try save_json (files_json_path ~root) (Workspace_index.to_yojson idx)
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
      ("id", `String s.id);
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
      ("symbolId", json_of_option (fun id -> `String id) o.symbol_id);
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
  `Assoc
    [
      ("id", `Int s.id);
      ("parent", json_of_option (fun id -> `Int id) s.parent);
      ("kind", `String (string_of_scope_kind s.kind));
      ("name", json_of_option (fun name -> `String name) s.name);
      ("loc", json_of_loc s.loc);
      ("symbols", `List (List.map (fun id -> `String id) s.symbols));
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
    let macro_entries =
      files
      |> List.concat_map (fun (uri, state) ->
             match state.Workspace_snapshot.skeleton with
             | None -> []
             | Some sk -> List.map (json_of_define ~uri) sk.defines)
    in
    save_json (symbols_json_path ~root)
      (`Assoc
        [
          ("generation", `Int snap.generation);
          ( "symbols",
            `List
              (List.map json_of_symbol_record
                 (Symbol_index.all snap.symbols)) );
        ]);
    save_json (refs_json_path ~root)
      (`Assoc
        [
          ("generation", `Int snap.generation);
          ( "refs",
            `List
              (List.map json_of_occurrence
                 (Reference_index.all snap.refs)) );
        ]);
    save_json (scopes_json_path ~root)
      (`Assoc
        [
          ("generation", `Int snap.generation);
          ( "scopes",
            `List (List.map json_of_scope (Scope_graph.scopes snap.scopes)) );
        ]);
    save_json (deps_json_path ~root)
      (`Assoc
        [
          ("generation", `Int snap.generation);
          ( "deps",
            `List
              (List.map json_of_edge (Dependency_graph.edges snap.deps)) );
        ]);
    save_json (macros_json_path ~root)
      (`Assoc [ ("generation", `Int snap.generation); ("macros", `List macro_entries) ]);
    save_json (diagnostics_json_path ~root)
      (`Assoc
        [
          ("generation", `Int snap.generation);
          ( "files",
            `List (List.map json_of_file_diagnostics files) );
        ])
  with _ -> ()
