(* Module overview: Immutable workspace snapshot model used by persistent indexes and LSIF export. *)

module UriMap = Map.Make (String)

type file_state = {
  uri : string;
  rev : int;
  lsp_version : int option;
  mode : Workspace_tuning.file_mode;
  text_size : int;
  tokens : Token_cache.t option;
  skeleton : Skeleton_index.skeleton_file option;
  ast : Ast.program option;
  semantic : Semantic_overlay.t option;
  diagnostics : Lsp.Types.Diagnostic.t list;
}

type snapshot = {
  generation : int;
  files : file_state UriMap.t;
  symbols : Symbol_index.t;
  scopes : Scope_graph.t;
  refs : Reference_index.t;
  deps : Dependency_graph.t;
}

let empty () =
  {
    generation = 0;
    files = UriMap.empty;
    symbols = Symbol_index.empty ();
    scopes = Scope_graph.empty ();
    refs = Reference_index.empty ();
    deps = Dependency_graph.empty ();
  }

let latest_mtx = Mutex.create ()
let latest = ref (empty ())
let workspace_cache : (int, snapshot) Hashtbl.t = Hashtbl.create 16

let current () =
  Mutex.lock latest_mtx;
  let snap = !latest in
  Mutex.unlock latest_mtx;
  snap

let publish snap =
  Mutex.lock latest_mtx;
  if snap.generation >= (!latest).generation then latest := snap;
  Mutex.unlock latest_mtx

let cached_for_workspace (ws : Workspace_foundation.t) : snapshot option =
  if ws.Workspace_foundation.ide_snapshot_dirty then None
  else (
    Mutex.lock latest_mtx;
    let snap = Hashtbl.find_opt workspace_cache ws.workspace_id in
    Mutex.unlock latest_mtx;
    snap)

let publish_for_workspace (ws : Workspace_foundation.t) (snap : snapshot) =
  Mutex.lock latest_mtx;
  Hashtbl.replace workspace_cache ws.workspace_id snap;
  if snap.generation >= (!latest).generation then latest := snap;
  Mutex.unlock latest_mtx;
  ws.ide_snapshot_generation <- snap.generation;
  ws.ide_snapshot_dirty <- false

let skeleton_of_doc (doc : Document.t) : Skeleton_index.skeleton_file option =
  Skeleton_index.of_document doc

let file_state_of_doc (ws : Workspace_foundation.t) (doc : Document.t) :
    file_state =
  let syntax =
    match Document.current_parse doc with
    | Some { Document.parsed_syntax = Some syntax; _ } -> Some syntax
    | _ -> None
  in
  let skeleton = skeleton_of_doc doc in
  let semantic =
    match skeleton with
    | None -> None
    | Some sk ->
        let symbols = Symbol_index.of_skeleton sk in
        Some (Semantic_overlay.of_skeleton symbols sk)
  in
  {
    uri = Uri_path.docuri_to_string doc.Document.uri;
    rev = doc.Document.rev;
    lsp_version = doc.Document.lsp_version;
    mode = Workspace_tuning.file_mode_of_doc doc ws;
    text_size = String.length doc.Document.text;
    tokens = Option.map (fun syntax -> syntax.Syntax_cache.token_cache) syntax;
    skeleton;
    ast = Document.current_ast doc;
    semantic;
    diagnostics = Document.diagnostics doc;
  }

let max_scope_id graph =
  Scope_graph.scopes graph
  |> List.fold_left (fun acc (scope : Scope_graph.scope) -> max acc scope.id) 0

let merge_scope_into target source =
  let offset = max_scope_id target + 1 in
  Scope_graph.scopes source
  |> List.iter (fun (scope : Scope_graph.scope) ->
         if scope.id = 0 then ()
         else
           let shifted =
             {
               scope with
               id = scope.id + offset;
               parent =
                 (match scope.parent with
                 | None -> None
                 | Some 0 -> Some 0
                 | Some parent -> Some (parent + offset));
             }
           in
           ignore (Scope_graph.add_scope target shifted))

let merge_deps_into target source =
  Dependency_graph.edges source |> List.iter (Dependency_graph.add_edge target)

let ensure_system_scope scopes =
  ignore
    (Scope_graph.add_scope scopes
       {
         Scope_graph.id = 0;
         parent = None;
         kind = Scope_graph.SystemScope;
         name = None;
         loc = Ast.Loc.none;
         symbols = [];
         symbol_bindings = [];
         imports = [];
       })

let of_workspace (ws : Workspace_foundation.t) : snapshot =
  Workspace_foundation.Perf_stats.time "snapshot.of_workspace_ms" (fun () ->
      let symbols = Symbol_index.empty () in
      let scopes = Scope_graph.empty () in
      let refs = Reference_index.empty () in
      let deps = Dependency_graph.empty () in
      ensure_system_scope scopes;
      let files = ref UriMap.empty in
      let seen = Hashtbl.create 128 in
      let add_doc doc =
        let state = file_state_of_doc ws doc in
        if not (Hashtbl.mem seen state.uri) then (
          Hashtbl.replace seen state.uri true;
          files := UriMap.add state.uri state !files;
          match state.skeleton with
          | None -> ()
          | Some sk ->
              let file_symbols = Symbol_index.of_skeleton sk in
              Symbol_index.all file_symbols
              |> List.iter (Symbol_index.add symbols);
              let file_refs = Reference_index.of_skeleton file_symbols sk in
              Reference_index.all file_refs |> List.iter (Reference_index.add refs);
              merge_scope_into scopes (Scope_graph.of_skeleton sk);
              merge_deps_into deps (Dependency_graph.of_skeleton sk))
      in
      Hashtbl.iter (fun _ doc -> add_doc doc) ws.docs;
      Hashtbl.iter (fun _ doc -> add_doc doc) ws.files;
      {
        generation = Semantic_store.global_rev ws.semantic_store;
        files = !files;
        symbols;
        scopes;
        refs;
        deps;
      })

let file snap ~(uri : string) = UriMap.find_opt uri snap.files
