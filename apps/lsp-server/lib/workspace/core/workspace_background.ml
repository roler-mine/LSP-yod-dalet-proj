module T = Lsp.Types
open Workspace_foundation
open Workspace_state
open Workspace_runtime
open Workspace_index_graph
open Workspace_imports
open Workspace_semantics
open Workspace_tuning

type semantic_validation_mode = SemanticFull | SemanticRangeSemi of T.Range.t

type doc_store_diff = {
  structural_changed : bool;
  rev_changed : bool;
  parse_rev_changed : bool;
  public_signature_changed : bool;
  imports_changed : bool;
  defines_changed : bool;
  icopy_includes_changed : bool;
  declaration_signature_changed : bool;
  compool_key_changed : bool;
  file_changed : bool;
  old_has_compool : bool;
  new_has_compool : bool;
  old_summary : Module_summary.t option;
  new_summary : Module_summary.t;
}

let has_compool_doc (doc : Document.t) : bool =
  match doc.Document.compool_def with
  | None -> false
  | Some name -> normalize_name name <> ""

let import_signature (imports : Preprocess.import list) : string list =
  imports
  |> List.filter_map (fun (imp : Preprocess.import) ->
         match imp.kind with
         | Preprocess.Compool ->
             let key = normalize_name imp.name in
             if key = "" then None else Some ("compool:" ^ key))
  |> List.sort_uniq String.compare

let define_signature (defines : Preprocess.define list) : string list =
  defines
  |> List.map (fun (d : Preprocess.define) ->
         Printf.sprintf "define:%s:%b:%s:%s" (normalize_name d.key)
           d.requires_call
           (String.concat "," (List.map normalize_name d.formals))
           d.body)

let include_signature (doc : Document.t) : string list =
  icopy_include_targets_of_doc doc
  |> List.map normalize_include_target
  |> List.map normalize_name
  |> List.filter (fun key -> key <> "")
  |> List.sort_uniq String.compare

let skeleton_symbol_kind_code = function
  | Syntax_cache.SkModule -> 1
  | Syntax_cache.SkCompool -> 2
  | Syntax_cache.SkProcedure | Syntax_cache.SkFunction -> 3
  | Syntax_cache.SkItem -> 4
  | Syntax_cache.SkTable -> 5
  | Syntax_cache.SkBlock -> 6
  | Syntax_cache.SkType -> 7
  | Syntax_cache.SkLabel -> 8
  | Syntax_cache.SkDefineMacro -> 9

let line_digest_for_loc (doc : Document.t) (loc : Ast.Loc.t) : string =
  let line = max 0 (loc.Ast.Loc.start_pos.line - 1) in
  match
    ( Text_index.line_start_offset doc.Document.index ~line,
      Text_index.line_length doc.Document.index ~line )
  with
  | Some start_off, Some len
    when start_off >= 0 && len >= 0
         && start_off + len <= String.length doc.Document.text ->
      Digest.to_hex (Digest.substring doc.Document.text start_off len)
  | _ -> ""

let declaration_signature (doc : Document.t) : string list =
  match Document.current_parse doc with
  | Some { Document.parsed_syntax = Some syntax; _ } ->
      let declaration_units =
        syntax.Syntax_cache.units
        |> List.filter_map (fun (unit_ : Syntax_cache.syntax_unit) ->
               match unit_.kind with
               | Syntax_cache.Declaration ->
                   Some ("decl:" ^ unit_.digest)
               | _ -> None)
      in
      let skeleton_symbols =
        syntax.Syntax_cache.skeleton.symbols
       |> List.map (fun (symbol : Syntax_cache.skeleton_symbol) ->
               Printf.sprintf "sym:%s:%d:%s:%b:%b:%s"
                 (normalize_name symbol.sk_name)
                 (skeleton_symbol_kind_code symbol.sk_kind)
                 (Option.value symbol.sk_container ~default:""
                 |> normalize_name)
                 symbol.sk_exported symbol.sk_imported
                 (line_digest_for_loc doc symbol.sk_loc))
      in
      declaration_units @ skeleton_symbols
  | _ -> []

let validated_doc_with_diags ?(import_lookup_pump : bool = true)
    ?(semantic_mode : semantic_validation_mode = SemanticFull) (ws : t)
    (doc : Document.t) : Document.t =
  let doc =
    match semantic_mode with
    | SemanticFull -> Document.ensure_parsed doc
    | SemanticRangeSemi _ -> doc
  in
  let import_diags =
    try validate_imports ~pump_lookup:import_lookup_pump ws doc
    with exn -> [ diag_internal_phase_failure ~phase:"import" doc exn ]
  in
  let semantic_diags =
    match Document.current_parse doc with
    | Some { Document.parsed_ast = Some _; parsed_diags = []; _ } -> (
      match semantic_mode with
      | SemanticFull -> (
          try validate_semantics ws doc
          with exn ->
            [ diag_internal_phase_failure ~phase:"semantic" doc exn ])
      | SemanticRangeSemi _ -> [])
    | _ -> []
  in
  Document.with_import_diags (import_diags @ semantic_diags) doc

let doc_store_diff ~(old_doc : Document.t option) ~(new_doc : Document.t) :
    doc_store_diff =
  let new_summary = Module_summary.of_document new_doc in
  match old_doc with
  | None ->
      {
        structural_changed = true;
        rev_changed = true;
        parse_rev_changed = true;
        public_signature_changed = true;
        imports_changed = true;
        defines_changed = true;
        icopy_includes_changed = true;
        declaration_signature_changed = true;
        compool_key_changed = true;
        file_changed = true;
        old_has_compool = false;
        new_has_compool = has_compool_doc new_doc;
        old_summary = None;
        new_summary;
      }
  | Some old_doc ->
      let old_summary = Module_summary.of_document old_doc in
      let rev_changed = old_doc.Document.rev <> new_doc.Document.rev in
      let parse_rev_changed =
        old_doc.Document.parse_rev <> new_doc.Document.parse_rev
      in
      let imports_changed =
        old_summary.Module_summary.imported_compools
        <> new_summary.Module_summary.imported_compools
      in
      let defines_changed =
        old_summary.Module_summary.define_public_macros
        <> new_summary.Module_summary.define_public_macros
      in
      let icopy_includes_changed =
        old_summary.Module_summary.icopy_targets
        <> new_summary.Module_summary.icopy_targets
      in
      let declaration_signature_changed =
        declaration_signature old_doc <> declaration_signature new_doc
      in
      let public_signature_changed =
        not
          (Module_summary.public_signature_unchanged old_summary new_summary)
      in
      let old_compool_key = compool_key_of_doc old_doc in
      let new_compool_key = compool_key_of_doc new_doc in
      let compool_key_changed = old_compool_key <> new_compool_key in
      let file_changed = old_doc.Document.file <> new_doc.Document.file in
      let has_compool =
        has_compool_doc old_doc || has_compool_doc new_doc
      in
      {
        structural_changed =
          imports_changed || defines_changed || icopy_includes_changed
          || public_signature_changed || compool_key_changed || file_changed
          || ((not has_compool) && declaration_signature_changed);
        rev_changed;
        parse_rev_changed;
        public_signature_changed;
        imports_changed;
        defines_changed;
        icopy_includes_changed;
        declaration_signature_changed;
        compool_key_changed;
        file_changed;
        old_has_compool = has_compool_doc old_doc;
        new_has_compool = has_compool_doc new_doc;
        old_summary = Some old_summary;
        new_summary;
      }

let replace_doc_storage ?(touch_bg_parsed : bool = true)
    ?(clear_closed_doc_touch : bool = true) (ws : t) ~(uri : T.DocumentUri.t)
    ~(old_doc : Document.t option) (doc : Document.t) : unit =
  let remove_old_path_entry () =
  match old_doc with
  | None -> ()
  | Some old_doc -> (
        match old_doc.Document.file with
        | None -> ()
        | Some path when Some path <> doc.Document.file ->
            let path_key = normalize_path_key path in
            Hashtbl.remove ws.files path_key;
            if touch_bg_parsed then Hashtbl.remove ws.bg_parsed path_key;
            if clear_closed_doc_touch then
              Hashtbl.remove ws.closed_doc_last_touch path_key
        | Some _ -> ())
  in
  remove_old_path_entry ();
  Hashtbl.replace ws.docs uri doc;
  match doc.Document.file with
  | None -> ()
  | Some f ->
      let path_key = normalize_path_key f in
      Hashtbl.replace ws.files path_key doc;
      if touch_bg_parsed then Hashtbl.replace ws.bg_parsed path_key true;
      if clear_closed_doc_touch then
        Hashtbl.remove ws.closed_doc_last_touch path_key

let update_module_summary_cache_for_doc (ws : t)
    ~(old_doc : Document.t option) (doc : Document.t)
    (summary : Module_summary.t) : unit =
  (match old_doc with
  | Some old_doc -> (
      match (old_doc.Document.file, doc.Document.file) with
      | Some old_path, Some new_path
        when normalize_path_key old_path <> normalize_path_key new_path ->
          remove_module_summary_cache_entry ws
            ~path_key:(normalize_path_key old_path)
      | Some old_path, None ->
          remove_module_summary_cache_entry ws
            ~path_key:(normalize_path_key old_path)
      | _ -> ())
  | None -> ());
  match doc.Document.file with
  | None -> ()
  | Some path ->
      let path_key = normalize_path_key path in
      if path_key <> "" then (
        install_module_summary_cache_entry ws
          {
            msc_path = path;
            msc_path_key = path_key;
            msc_summary = summary;
            msc_authority = ModuleSummaryMetadataValidated;
          };
        match ws.root_path with
        | None -> ()
        | Some root ->
            Persistent_cache.save_module_summary_entry
              ~source_extensions:ws.source_extensions ~root ~path ~summary)

let maybe_shed_doc_ast (ws : t) ~(uri : T.DocumentUri.t) (doc : Document.t) :
    bool =
  let dropped = Document.drop_ast doc in
  if dropped == doc then false
  else (
    replace_doc_storage ~touch_bg_parsed:false ~clear_closed_doc_touch:false ws
      ~uri ~old_doc:(Some doc) dropped;
    Perf_stats.tick "mem.doc_ast_shed";
    true)

let maybe_shed_open_doc_parse_state
    ?(prefer_uri : T.DocumentUri.t option = None) (ws : t) : unit =
  let budget =
    match workspace_pressure_mode ws with
    | PressureNormal -> 0
    | PressureSoft -> 16
    | PressureCritical -> max 64 (Hashtbl.length ws.docs)
  in
  if budget > 0 then (
    let remaining = ref budget in
    let preferred_key = Option.map Uri_path.docuri_to_string prefer_uri in
    let try_uri (uri : T.DocumentUri.t) : unit =
      if !remaining > 0 then
        match Hashtbl.find_opt ws.docs uri with
        | None -> ()
        | Some doc -> if maybe_shed_doc_ast ws ~uri doc then decr remaining
    in
    (match prefer_uri with None -> () | Some uri -> try_uri uri);
    if !remaining > 0 then
      Hashtbl.iter
        (fun uri doc ->
          if !remaining > 0 then
            let skip_preferred =
              match preferred_key with
              | Some key -> Uri_path.docuri_to_string uri = key
              | None -> false
            in
            if (not skip_preferred) && maybe_shed_doc_ast ws ~uri doc then
              decr remaining)
        ws.docs)

let revalidate_importers_for_doc_diff (ws : t) ~(old_doc : Document.t option)
    ~(new_doc : Document.t) ~(diff : doc_store_diff) ~(enqueue_open_diag : bool)
    : unit =
  if
    diff.public_signature_changed
    && (diff.old_has_compool || diff.new_has_compool)
  then (
    let seen = Hashtbl.create 4 in
    let revalidate_importers_for_compool (key_opt : string option) : unit =
      match key_opt with
      | None -> ()
      | Some key ->
          let key = normalize_name key in
          if key = "" || Hashtbl.mem seen key then ()
          else (
            Hashtbl.replace seen key true;
            let importers = importer_uris_for_compool_key ws ~compool_key:key in
            if importers <> [] then (
              Perf_stats.tick "dep.invalidate.compool_reverse";
              Perf_stats.observe_ms "dep.invalidate.compool_reverse_uris"
                (float_of_int (List.length importers)));
            invalidate_importer_nav_state_for_compool_key ws ~compool_key:key;
            if enqueue_open_diag then
              List.iter
                (fun importer_uri ->
                  enqueue_open_diag_revalidate ws ~uri:importer_uri
                    ~reason:"compool_change")
                importers)
    in
    revalidate_importers_for_compool (Option.bind old_doc compool_key_of_doc);
    revalidate_importers_for_compool (compool_key_of_doc new_doc);
    if enqueue_open_diag then
      enqueue_all_open_diag_revalidate ws ~reason:"compool_change")

let record_doc_diff_side_effects (ws : t) (diff : doc_store_diff) : unit =
  (match diff.old_summary with
  | None -> Perf_stats.tick "summary.public_hash_changed"
  | Some _ ->
      if diff.public_signature_changed then
        Perf_stats.tick "summary.public_hash_changed"
      else Perf_stats.tick "summary.public_hash_unchanged");
  if
    diff.declaration_signature_changed
    && not diff.public_signature_changed
    && (diff.old_has_compool || diff.new_has_compool)
  then Perf_stats.tick "dep.invalidate.pruned_by_summary";
  if diff.structural_changed then (
    mark_graph_dirty ws;
    Perf_stats.tick "dep.invalidate.graph_dirty";
    if diff.public_signature_changed then
      Perf_stats.tick "dep.invalidate.public_signature";
    if diff.imports_changed then Perf_stats.tick "dep.invalidate.icompools";
    if diff.icopy_includes_changed then Perf_stats.tick "dep.invalidate.icopy";
    if diff.defines_changed then Perf_stats.tick "dep.invalidate.define";
    if diff.declaration_signature_changed && diff.public_signature_changed then
      Perf_stats.tick "dep.invalidate.declaration")
  else if diff.rev_changed || diff.parse_rev_changed then
    Perf_stats.tick "dep.invalidate.current_file_only"
  else Perf_stats.tick "store_doc.side_effects_skipped";
  if
    diff.compool_key_changed
    || diff.structural_changed
       && (diff.old_has_compool || diff.new_has_compool)
  then invalidate_symbol_hints ws

let install_doc_surface ?(touch_bg_parsed : bool = true)
    ?(clear_closed_doc_touch : bool = true)
    ?(enqueue_importer_revalidate : bool = true) ?(allow_ast_shed : bool = true)
    (ws : t) ~(uri : T.DocumentUri.t) ~(old_doc : Document.t option)
    (doc : Document.t) : doc_store_diff =
  let diff = doc_store_diff ~old_doc ~new_doc:doc in
  record_doc_diff_side_effects ws diff;
  replace_doc_storage ~touch_bg_parsed ~clear_closed_doc_touch ws ~uri ~old_doc
    doc;
  update_module_summary_cache_for_doc ws ~old_doc doc diff.new_summary;
  if allow_ast_shed then
    maybe_shed_open_doc_parse_state ws ~prefer_uri:(Some uri);
  revalidate_importers_for_doc_diff ws ~old_doc ~new_doc:doc ~diff
    ~enqueue_open_diag:enqueue_importer_revalidate;
  if ws.sem_store_enabled && (diff.rev_changed || diff.parse_rev_changed) then
    Semantic_store.remove_uri ws.semantic_store ~uri;
  diff

let store_doc ?(import_lookup_pump : bool = true)
    ?(semantic_mode : semantic_validation_mode = SemanticFull) (ws : t)
    (uri : T.DocumentUri.t) (doc : Document.t) : unit =
  let old_doc = Hashtbl.find_opt ws.docs uri in
  let doc =
    validated_doc_with_diags ~import_lookup_pump ~semantic_mode ws doc
  in
  ignore (install_doc_surface ws ~uri ~old_doc doc)

let store_doc_fast (ws : t) (uri : T.DocumentUri.t) (doc : Document.t) : unit =
  let old_doc = Hashtbl.find_opt ws.docs uri in
  ignore
    (install_doc_surface ~touch_bg_parsed:false
       ~enqueue_importer_revalidate:false ws ~uri ~old_doc doc);
  match doc.Document.file with
  | None -> ()
  | Some f ->
      let path_key = normalize_path_key f in
      if doc.Document.parse_rev = doc.Document.rev then
        Hashtbl.replace ws.bg_parsed path_key true
      else Hashtbl.remove ws.bg_parsed path_key

let direct_import_paths (ws : t) (doc : Document.t) : string list =
  let acc = Hashtbl.create 16 in
  let resolve_compool_path (name : string) : string option =
    let key = normalize_name name in
    if key = "" then None
    else
      let from_index =
        match ws.index with
        | Some idx -> Workspace_index.find_compool idx ~name:key
        | None -> None
      in
      match from_index with
      | Some _ as p -> p
      | None ->
          if allow_fallback_scan ws then find_compool_path_fallback ws ~key
          else None
  in
  List.iter
    (fun (imp : Preprocess.import) ->
      match imp.kind with
      | Preprocess.Compool -> (
          match resolve_compool_path imp.name with
          | None -> ()
          | Some p -> Hashtbl.replace acc (normalize_path_key p) p))
    (Document.imports doc);
  Hashtbl.fold (fun _ p xs -> p :: xs) acc []

let enqueue_doc_imports_high (ws : t) (doc : Document.t) : unit =
  direct_import_paths ws doc
  |> List.iter (fun p ->
      enqueue_bg_path ws ~lane:LaneOpen ~reason_group:"open_import" ~high:true p)

let background_doc_with_diags (ws : t) (doc : Document.t) : Document.t =
  validated_doc_with_diags ~import_lookup_pump:false ws doc

let refresh_open_doc_diags ?(import_lookup_pump : bool = false) (ws : t)
    ~(uri : T.DocumentUri.t) : Document.t option =
  match Hashtbl.find_opt ws.docs uri with
  | None -> None
  | Some doc ->
      let refreshed = validated_doc_with_diags ~import_lookup_pump ws doc in
      replace_doc_storage ~touch_bg_parsed:false ~clear_closed_doc_touch:false
        ws ~uri ~old_doc:(Some doc) refreshed;
      maybe_shed_open_doc_parse_state ws ~prefer_uri:(Some uri);
      Perf_stats.tick "diag.open.revalidate_fast_path";
      Perf_stats.tick "store_doc.side_effects_skipped";
      Some refreshed

let queue_workspace_diag_update_for_doc (ws : t) (doc : Document.t) : unit =
  match doc.Document.file with
  | None -> ()
  | Some path -> (
      match find_open_doc_for_path ws ~path with
      | Some _ -> ()
      | None ->
          if bg_diag_allowed ws then
            let uri_s = Uri_path.docuri_to_string doc.Document.uri in
            let filtered =
              filter_workspace_diags ws (Document.diagnostics doc)
            in
            let prev =
              match Hashtbl.find_opt ws.bg_closed_diags uri_s with
              | Some ds -> ds
              | None -> []
            in
            if prev <> filtered then (
              if filtered = [] then Hashtbl.remove ws.bg_closed_diags uri_s
              else Hashtbl.replace ws.bg_closed_diags uri_s filtered;
              enqueue_bg_diag_update ws ~uri:doc.Document.uri ~diags:filtered))

let refresh_closed_doc_diagnostics_now (ws : t) ~(uri : T.DocumentUri.t) : bool
    =
  if Hashtbl.mem ws.docs uri then false
  else
    match Uri_path.file_path_of_uri uri with
    | None -> false
    | Some path ->
        let path_key = normalize_path_key path in
        if
          path_key = ""
          || not
               (Source_file.has_extension ~extensions:ws.source_extensions
                  (Filename.basename path))
        then false
        else if not (Sys.file_exists path) then (
          let uri_s = Uri_path.docuri_to_string uri in
          let had = Hashtbl.mem ws.bg_closed_diags uri_s in
          Hashtbl.remove ws.bg_closed_diags uri_s;
          Hashtbl.remove ws.files path_key;
          Hashtbl.remove ws.bg_parsed path_key;
          Hashtbl.remove ws.closed_doc_last_touch path_key;
          remove_module_summary_cache_entry ws ~path_key;
          if had then enqueue_bg_diag_update ws ~uri ~diags:[];
          had)
        else
          match read_file_text path with
          | None -> false
          | Some text ->
              let doc0 =
                try
                  parse_guarded_document_make ~profile:Parser.Background ws ~uri
                    ~file:(Some path) ~text
                with exn ->
                  let fallback =
                    Document.make_with_profile ~profile:Parser.Background ~uri
                      ~file:(Some path) ~text:""
                  in
                  with_internal_phase_diag fallback ~phase:"closed-diag-refresh"
                    ~exn
              in
              let doc = background_doc_with_diags ws doc0 in
              Hashtbl.replace ws.files path_key doc;
              Hashtbl.replace ws.bg_parsed path_key true;
              touch_closed_doc_path ws ~path_key;
              evict_closed_docs_if_needed ws;
              queue_workspace_diag_update_for_doc ws doc;
              true

let quick_nav_entry_add (ws : t) ~(key : string) (entry : quick_nav_entry) :
    unit =
  let k = normalize_name key in
  if k = "" then ()
  else
    let prev =
      match Hashtbl.find_opt ws.quick_nav_index k with
      | Some xs -> xs
      | None -> []
    in
    if
      List.exists
        (fun x ->
          Uri_path.docuri_to_string x.qn_uri
          = Uri_path.docuri_to_string entry.qn_uri
          && x.qn_loc = entry.qn_loc)
        prev
    then ()
    else Hashtbl.replace ws.quick_nav_index k (entry :: prev)

let refresh_bg_seed_paths (ws : t) : unit =
  ensure_graph_fresh ws;
  match ws.index with
  | None ->
      ws.bg_seed_paths <- [||];
      ws.bg_seed_cursor <- 0;
      Hashtbl.clear ws.quick_nav_index;
      Hashtbl.clear ws.quick_nav_done_set;
      ws.quick_nav_index_done <- 0;
      ws.quick_nav_index_total <- 0;
      Hashtbl.clear ws.quick_nav_pending_set;
      while not (Queue.is_empty ws.quick_nav_pending_paths) do
        ignore (Queue.pop ws.quick_nav_pending_paths)
      done;
      ws.bg_seed_needs_refresh <- false
  | Some idx ->
      ws.bg_seed_paths <- Array.of_list (Workspace_index.all_source_paths idx);
      ws.bg_seed_cursor <- 0;
      let cached_nav =
        match ws.root_path with
        | None -> None
        | Some root ->
            Some
              (Persistent_cache.load_skeleton_cache
                 ~source_extensions:ws.source_extensions ~root
                 ~max_bytes:nav_quick_scan_per_file_bytes
                 ~paths:(Array.to_list ws.bg_seed_paths))
      in
      let total_unique = ref 0 in
      let seen = Hashtbl.create (max 16 (Array.length ws.bg_seed_paths)) in
      Array.iter
        (fun p ->
          let key = normalize_path_key p in
          if key <> "" && not (Hashtbl.mem seen key) then (
            Hashtbl.replace seen key true;
            incr total_unique;
            if
              (not (Hashtbl.mem ws.quick_nav_done_set key))
              && not (Hashtbl.mem ws.quick_nav_pending_set key)
            then
              match cached_nav with
              | Some cache -> (
                  match Persistent_cache.skeleton_entries cache ~path:p with
                  | Some entries ->
                      List.iter
                        (fun e -> quick_nav_entry_add ws ~key:e.qn_key e)
                        entries;
                      Hashtbl.replace ws.quick_nav_done_set key true;
                      Perf_stats.tick "persistent_cache.skeleton_hit"
                  | None ->
                      Hashtbl.replace ws.quick_nav_pending_set key true;
                      Queue.add p ws.quick_nav_pending_paths)
              | None ->
                  Hashtbl.replace ws.quick_nav_pending_set key true;
                  Queue.add p ws.quick_nav_pending_paths))
        ws.bg_seed_paths;
      ws.quick_nav_index_total <- !total_unique;
      ws.quick_nav_index_done <- Hashtbl.length ws.quick_nav_done_set;
      ws.bg_seed_needs_refresh <- false

let seed_bg_paths_from_index (ws : t) : unit =
  ensure_graph_fresh ws;
  if ws.bg_seed_needs_refresh then refresh_bg_seed_paths ws;
  let per_tick =
    match workspace_pressure_mode ws with
    | PressureNormal -> bg_seed_paths_per_tick
    | PressureSoft -> max 1 (bg_seed_paths_per_tick / 4)
    | PressureCritical -> 0
  in
  if per_tick <= 0 then ()
  else
    let added = ref 0 in
    while
      !added < per_tick
      && ws.graph_root_closure_cursor < Array.length ws.graph_root_closure_paths
    do
      let p = ws.graph_root_closure_paths.(ws.graph_root_closure_cursor) in
      ws.graph_root_closure_cursor <- ws.graph_root_closure_cursor + 1;
      enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"root_closure" ~high:false
        p;
      incr added
    done;
    while
      !added < per_tick && ws.bg_seed_cursor < Array.length ws.bg_seed_paths
    do
      let p = ws.bg_seed_paths.(ws.bg_seed_cursor) in
      ws.bg_seed_cursor <- ws.bg_seed_cursor + 1;
      enqueue_bg_path ws ~lane:LaneSweep ~reason_group:"seed_sweep" ~high:false
        p;
      incr added
    done

let quick_nav_proc_kind = 12

let parse_doc_worker ~(max_bytes : int) ~(uri : T.DocumentUri.t)
    ~(file : string option) ~(text : string) : Document.t =
  if is_parse_guard_exceeded ~max_bytes ~text_len:(String.length text) then
    Document.make_parse_skipped ~uri ~file ~text
      ~parse_diags:
        [ diag_parse_guard ~file ~max_bytes ~actual_bytes:(String.length text) ]
  else
    try Document.make_with_profile ~profile:Parser.Background ~uri ~file ~text
    with exn ->
      let fallback =
        Document.make_with_profile ~profile:Parser.Background ~uri ~file ~text:""
      in
      with_internal_phase_diag fallback ~phase:"worker-parse" ~exn

let parse_open_doc_worker ~(max_bytes : int) (doc : Document.t) : Document.t =
  if
    is_parse_guard_exceeded ~max_bytes
      ~text_len:(String.length doc.Document.text)
  then
    Document.with_parse_skipped
      [
        diag_parse_guard ~file:doc.Document.file ~max_bytes
          ~actual_bytes:(String.length doc.Document.text);
      ]
      doc
  else
    try Document.reparse_for_profile ~profile:Parser.Background doc
    with exn -> with_internal_phase_diag doc ~phase:"worker-parse-open" ~exn

let parse_job_path_key (job : parse_job) : string =
  match job.pj_payload with
  | ParseJobOpen x -> x.path_key
  | ParseJobPath x -> x.path_key

let open_doc_generation_for_uri (ws : t) (uri : T.DocumentUri.t) : int =
  match
    Hashtbl.find_opt ws.open_parse_generation (Uri_path.docuri_to_string uri)
  with
  | Some g -> g
  | None -> 0

let parse_job_stale_reason (ws : t) (job : parse_job) : string option =
  if job.pj_epoch <> ws.parse_epoch then Some "epoch"
  else
    match job.pj_payload with
    | ParseJobOpen { uri; generation; doc; _ } -> (
        match Hashtbl.find_opt ws.docs uri with
        | None -> Some "closed"
        | Some latest_doc ->
            let latest_generation = open_doc_generation_for_uri ws uri in
            if latest_generation <> generation then Some "generation"
            else if latest_doc.Document.rev <> doc.Document.rev then Some "rev"
            else if latest_doc.Document.lsp_version <> doc.Document.lsp_version
            then Some "version"
            else None)
    | ParseJobPath { path_key; _ } ->
        if Hashtbl.mem ws.bg_parsed path_key then Some "already_parsed"
        else if has_open_doc_for_path_key ws ~path_key then Some "open_doc"
        else None

let parse_job_is_stale (ws : t) (job : parse_job) : bool =
  parse_job_stale_reason ws job <> None

let stale_result_of_parse_job (job : parse_job) : parse_result =
  ParseResultStale
    { pr_epoch = job.pj_epoch; path_key = parse_job_path_key job }

let parse_job_kind_of_queue_kind = function
  | BgQueueHighLarge -> Some ParseJobHighLarge
  | BgQueueRootLarge -> Some ParseJobRootLarge
  | BgQueueNormalLarge -> Some ParseJobNormalLarge
  | BgQueueHighSmall | BgQueueRootSmall | BgQueueNormalSmall -> None

let parse_worker_loop (ws : t) () : unit =
  let wait_job () =
    Mutex.lock ws.parse_worker_mtx;
    while (not ws.parse_worker_stop) && Queue.is_empty ws.parse_worker_jobs do
      Condition.wait ws.parse_worker_cv ws.parse_worker_mtx
    done;
    let job_opt =
      if ws.parse_worker_stop then None
      else Some (Queue.pop ws.parse_worker_jobs)
    in
    Mutex.unlock ws.parse_worker_mtx;
    job_opt
  in
  let push_result (res : parse_result) =
    Mutex.lock ws.parse_worker_mtx;
    Queue.add res ws.parse_worker_results;
    Mutex.unlock ws.parse_worker_mtx
  in
  let rec loop () =
    match wait_job () with
    | None -> ()
    | Some job ->
        let result =
          if parse_job_is_stale ws job then stale_result_of_parse_job job
          else
            match job.pj_payload with
            | ParseJobOpen { path_key; uri; generation; doc } ->
                let doc =
                  parse_open_doc_worker ~max_bytes:ws.parse_file_max_bytes doc
                in
                ParseResultOpen
                  {
                    pr_kind = job.pj_kind;
                    pr_epoch = job.pj_epoch;
                    path_key;
                    uri;
                    generation;
                    doc;
                  }
            | ParseJobPath { path; path_key } ->
                let uri =
                  match Uri_path.docuri_of_path path with
                  | Some u -> u
                  | None -> (
                      match
                        T.DocumentUri.t_of_yojson
                          (`String (Uri_path.file_uri_of_path path))
                      with
                      | u -> u
                      | exception _ ->
                          T.DocumentUri.t_of_yojson (`String "file:///"))
                in
                let doc_opt =
                  match read_file_text path with
                  | None -> None
                  | Some text ->
                      Some
                        (parse_doc_worker ~max_bytes:ws.parse_file_max_bytes
                           ~uri ~file:(Some path) ~text)
                in
                ParseResultPath
                  {
                    pr_kind = job.pj_kind;
                    pr_epoch = job.pj_epoch;
                    path;
                    path_key;
                    doc_opt;
                  }
        in
        push_result result;
        loop ()
  in
  loop ()

let ensure_parse_worker_started (ws : t) : unit =
  if ws.parse_worker_started then ()
  else (
    ws.parse_worker_started <- true;
    for _i = 1 to max 1 ws.parse_worker_count do
      ignore (Thread.create (parse_worker_loop ws) ())
    done)

let try_submit_large_parse_job (ws : t) ~(path : string)
    ~(queue_kind : bg_queue_kind) : bool =
  match parse_job_kind_of_queue_kind queue_kind with
  | None -> false
  | Some pj_kind ->
      let path_key = normalize_path_key path in
      if path_key = "" then true
      else if Hashtbl.mem ws.parse_worker_inflight path_key then true
      else if
        Hashtbl.length ws.parse_worker_inflight >= ws.parse_worker_max_inflight
      then false
      else
        let payload =
          match find_open_doc_for_path ws ~path with
          | Some doc ->
              let generation =
                match
                  Hashtbl.find_opt ws.open_parse_generation
                    (Uri_path.docuri_to_string doc.Document.uri)
                with
                | Some g -> g
                | None -> 0
              in
              ParseJobOpen
                {
                  path_key;
                  uri = doc.Document.uri;
                  generation;
                  doc;
                }
          | None -> ParseJobPath { path; path_key }
        in
        let job =
          { pj_kind; pj_epoch = ws.parse_epoch; pj_payload = payload }
        in
        Mutex.lock ws.parse_worker_mtx;
        Queue.add job ws.parse_worker_jobs;
        Condition.signal ws.parse_worker_cv;
        Mutex.unlock ws.parse_worker_mtx;
        Hashtbl.replace ws.parse_worker_inflight path_key pj_kind;
        true

let quick_nav_kind_of_skeleton_kind = function
  | Syntax_cache.SkModule | Syntax_cache.SkCompool | Syntax_cache.SkBlock -> 2
  | Syntax_cache.SkType -> 5
  | Syntax_cache.SkProcedure | Syntax_cache.SkFunction -> quick_nav_proc_kind
  | Syntax_cache.SkItem | Syntax_cache.SkTable | Syntax_cache.SkLabel -> 13
  | Syntax_cache.SkDefineMacro -> 14

let quick_nav_metadata_of_skeleton_symbol
    (symbol : Syntax_cache.skeleton_symbol) =
  let external_kind =
    if symbol.sk_exported then Workspace_symbol_metadata.ExternalDef
    else if symbol.sk_imported then Workspace_symbol_metadata.ExternalRef
    else Workspace_symbol_metadata.ExternalLocal
  in
  let jovial_kind =
    match symbol.sk_kind with
    | Syntax_cache.SkModule -> Workspace_symbol_metadata.JovialModule
    | Syntax_cache.SkCompool -> Workspace_symbol_metadata.JovialCompool
    | Syntax_cache.SkProcedure -> Workspace_symbol_metadata.JovialProcedure
    | Syntax_cache.SkFunction -> Workspace_symbol_metadata.JovialFunction
    | Syntax_cache.SkItem -> Workspace_symbol_metadata.JovialItem
    | Syntax_cache.SkTable -> Workspace_symbol_metadata.JovialTable
    | Syntax_cache.SkBlock -> Workspace_symbol_metadata.JovialBlock
    | Syntax_cache.SkType -> Workspace_symbol_metadata.JovialType
    | Syntax_cache.SkLabel -> Workspace_symbol_metadata.JovialLabel
    | Syntax_cache.SkDefineMacro -> Workspace_symbol_metadata.JovialDefine
  in
  {
    Workspace_symbol_metadata.default_metadata with
    jovial_kind;
    external_kind;
    decl_role = Workspace_symbol_metadata.decl_role_of_external_kind external_kind;
    is_imported = symbol.sk_imported;
    is_exported = symbol.sk_exported;
    has_body = Some false;
  }

let quick_nav_entries_of_path_prefix (path : string) ~(max_bytes : int) :
    quick_nav_entry list =
  match read_file_prefix_text path ~max_bytes with
  | None -> []
  | Some text ->
      let uri =
        match Uri_path.docuri_of_path path with
        | Some u -> u
        | None -> (
            match
              T.DocumentUri.t_of_yojson
                (`String (Uri_path.file_uri_of_path path))
            with
            | u -> u
            | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))
      in
      let cache =
        try
          Some
            (Syntax_cache.build_with_profile ~profile:Parser.Batch
               ~file:(Some path) ~text ())
        with _ -> None
      in
      match cache with
      | None -> []
      | Some cache ->
          cache.Syntax_cache.skeleton.symbols
          |> List.filter_map (fun (symbol : Syntax_cache.skeleton_symbol) ->
                 let key = normalize_name symbol.sk_name in
                 if key = "" then None
                 else
                   Some
                     {
                       qn_uri = uri;
                       qn_name = symbol.sk_name;
                       qn_key = key;
                       qn_loc = symbol.sk_loc;
                       qn_kind =
                         quick_nav_kind_of_skeleton_kind symbol.sk_kind;
                       qn_container = symbol.sk_container;
                       qn_metadata =
                         quick_nav_metadata_of_skeleton_symbol symbol;
                     })

let quick_nav_index_step (ws : t) ~(budget_ms : int) : unit =
  if budget_ms <= 0 then ()
  else
    let deadline = Perf_stats.now_ms () +. float_of_int budget_ms in
    let rec loop () =
      if Perf_stats.now_ms () >= deadline then ()
      else if Queue.is_empty ws.quick_nav_pending_paths then ()
      else
        let path = Queue.pop ws.quick_nav_pending_paths in
        let path_key = normalize_path_key path in
        Hashtbl.remove ws.quick_nav_pending_set path_key;
        if path_key <> "" then (
          let entries =
            quick_nav_entries_of_path_prefix path
              ~max_bytes:nav_quick_scan_per_file_bytes
          in
          List.iter (fun e -> quick_nav_entry_add ws ~key:e.qn_key e) entries;
          (match ws.root_path with
          | None -> ()
          | Some root ->
              Persistent_cache.save_skeleton_entry
                ~source_extensions:ws.source_extensions ~root
                ~max_bytes:nav_quick_scan_per_file_bytes ~path ~entries);
          if not (Hashtbl.mem ws.quick_nav_done_set path_key) then (
            Hashtbl.replace ws.quick_nav_done_set path_key true;
            ws.quick_nav_index_done <- ws.quick_nav_index_done + 1));
        loop ()
    in
    loop ()

let apply_parse_result_open (ws : t) ~(pr_kind : parse_job_kind)
    ~(pr_epoch : int) ~(path_key : string) ~(uri : T.DocumentUri.t)
    ~(generation : int) ~(doc : Document.t) : unit =
  Hashtbl.remove ws.parse_worker_inflight path_key;
  if pr_epoch <> ws.parse_epoch then
    Perf_stats.tick "bg.parse.stale_result_drop"
  else
    let latest_generation = open_doc_generation_for_uri ws uri in
    if latest_generation <> generation then
      Perf_stats.tick "diag.open.stale_generation_drop"
    else
      match Hashtbl.find_opt ws.docs uri with
      | None -> Perf_stats.tick "diag.open.stale_generation_drop"
      | Some latest_doc ->
          if
            latest_doc.Document.rev <> doc.Document.rev
            || doc.Document.lsp_version <> latest_doc.Document.lsp_version
          then Perf_stats.tick "diag.open.stale_version_drop"
          else (
            ignore pr_kind;
            let old_doc = Hashtbl.find_opt ws.docs uri in
            let finalize_diag_now =
              doc.Document.parse_rev = doc.Document.rev
              && Hashtbl.length ws.open_parse_generation <= 1
              && xmodule_diag_prereqs_ready ws
            in
            ignore
              (install_doc_surface
                 ~enqueue_importer_revalidate:finalize_diag_now
                 ~allow_ast_shed:false ws ~uri ~old_doc doc);
            let uri_key = Uri_path.docuri_to_string uri in
            if doc.Document.parse_rev = doc.Document.rev then (
              (match Hashtbl.find_opt ws.open_provisional_since_ms uri_key with
              | None -> ()
              | Some t0 ->
                  let lag = max 0.0 (Perf_stats.now_ms () -. t0) in
                  Perf_stats.observe_ms "diag.open.authoritative_lag_ms" lag;
                  Hashtbl.remove ws.open_provisional_since_ms uri_key);
              Hashtbl.remove ws.open_parse_generation uri_key;
              if xmodule_diag_prereqs_ready ws then
                enqueue_open_diag_revalidate ws ~uri ~reason:"open_parse"
              else Perf_stats.tick "diag.open.revalidate_deferred_startup")
            else
              Hashtbl.replace ws.open_provisional_since_ms uri_key
                (Perf_stats.now_ms ());
            (match doc.Document.file with
            | Some p ->
                let pk = normalize_path_key p in
                Hashtbl.replace ws.files pk doc;
                Hashtbl.replace ws.bg_parsed pk true;
                Hashtbl.remove ws.closed_doc_last_touch pk
            | None -> ());
            enqueue_doc_imports_high ws doc;
            invalidate_lsif_snapshot ws)

let apply_parse_result_path (ws : t) ~(pr_kind : parse_job_kind)
    ~(pr_epoch : int) ~(path_key : string) ~(doc_opt : Document.t option) : unit
    =
  Hashtbl.remove ws.parse_worker_inflight path_key;
  if pr_epoch <> ws.parse_epoch then
    Perf_stats.tick "bg.parse.stale_result_drop"
  else if has_open_doc_for_path_key ws ~path_key then
    Perf_stats.tick "bg.parse.stale_result_drop"
  else
    match doc_opt with
    | None -> ()
    | Some doc ->
        let doc =
          match pr_kind with
          | ParseJobHighLarge -> background_doc_with_diags ws doc
          | ParseJobRootLarge -> background_doc_with_diags ws doc
          | ParseJobNormalLarge ->
              if ws.startup_fully_nav_ready_ms <> None then
                background_doc_with_diags ws doc
              else doc
        in
        invalidate_lsif_snapshot ws;
        let old_doc = Hashtbl.find_opt ws.files path_key in
        let diff = doc_store_diff ~old_doc ~new_doc:doc in
        (match old_doc with
        | None -> ()
        | Some _ -> record_doc_diff_side_effects ws diff);
        revalidate_importers_for_doc_diff ws ~old_doc ~new_doc:doc ~diff
          ~enqueue_open_diag:true;
        Hashtbl.replace ws.files path_key doc;
        Hashtbl.replace ws.bg_parsed path_key true;
        touch_closed_doc_path ws ~path_key;
        evict_closed_docs_if_needed ws;
        if ws.sem_store_enabled && (diff.rev_changed || diff.parse_rev_changed)
        then Semantic_store.remove_uri ws.semantic_store ~uri:doc.Document.uri;
        queue_workspace_diag_update_for_doc ws doc

let apply_parse_result_stale (ws : t) ~(pr_epoch : int) ~(path_key : string) :
    unit =
  Hashtbl.remove ws.parse_worker_inflight path_key;
  Perf_log.log_event "background_stale_job_discard" ~uri:path_key;
  if pr_epoch <> ws.parse_epoch then
    Perf_stats.tick "bg.parse.stale_result_drop"
  else Perf_stats.tick "bg.parse.stale_job_drop"

let finish_open_doc_now_if_needed (ws : t) ~(uri : T.DocumentUri.t) : bool =
  match Hashtbl.find_opt ws.docs uri with
  | None -> false
  | Some doc ->
      if doc.Document.parse_rev = doc.Document.rev then false
      else
        match doc.Document.file with
        | None -> false
        | Some path ->
            let generation =
              match
                Hashtbl.find_opt ws.open_parse_generation
                  (Uri_path.docuri_to_string uri)
              with
              | Some g -> g
              | None -> 0
            in
            let parsed_doc =
              try
                Perf_stats.time "parse.open_doc_forced" (fun () ->
                    parse_open_doc_worker ~max_bytes:ws.parse_file_max_bytes doc)
              with exn ->
                with_internal_phase_diag doc ~phase:"open-doc-forced" ~exn
            in
            apply_parse_result_open ws ~pr_kind:ParseJobHighLarge
              ~pr_epoch:ws.parse_epoch ~path_key:(normalize_path_key path) ~uri
              ~generation ~doc:parsed_doc;
            (match Hashtbl.find_opt ws.docs uri with
            | Some latest when latest.Document.parse_rev = latest.Document.rev
              ->
                ignore
                  (refresh_open_doc_diags ~import_lookup_pump:false ws ~uri)
            | _ -> ());
            true

let finish_last_open_doc_now_if_needed (ws : t) : bool =
  if Hashtbl.length ws.open_parse_generation > 1 then false
  else
    let pending =
      Hashtbl.fold
        (fun uri doc acc ->
          if doc.Document.parse_rev = doc.Document.rev then acc
          else (uri, doc) :: acc)
        ws.docs []
    in
    match pending with
    | [] -> false
    | (uri, _) :: _ -> finish_open_doc_now_if_needed ws ~uri

let drain_parse_worker_results ?(open_only : bool = false) (ws : t)
    ~(max_items : int) : unit =
  if max_items <= 0 then ()
  else
    let pop_next_result () =
      Mutex.lock ws.parse_worker_mtx;
      let next =
        if Queue.is_empty ws.parse_worker_results then None
        else if not open_only then Some (Queue.pop ws.parse_worker_results)
        else
          let skipped = Queue.create () in
          let found = ref None in
          while !found = None && not (Queue.is_empty ws.parse_worker_results) do
            let res = Queue.pop ws.parse_worker_results in
            match res with
            | ParseResultOpen _ -> found := Some res
            | ParseResultStale _ -> found := Some res
            | ParseResultPath _ -> Queue.add res skipped
          done;
          while not (Queue.is_empty skipped) do
            Queue.add (Queue.pop skipped) ws.parse_worker_results
          done;
          !found
      in
      Mutex.unlock ws.parse_worker_mtx;
      next
    in
    let rec loop n =
      if n <= 0 then ()
      else (
        let next = pop_next_result () in
        match next with
        | None -> ()
        | Some (ParseResultOpen x) ->
            apply_parse_result_open ws ~pr_kind:x.pr_kind ~pr_epoch:x.pr_epoch
              ~path_key:x.path_key ~uri:x.uri ~generation:x.generation
              ~doc:x.doc;
            loop (n - 1)
        | Some (ParseResultPath x) ->
            apply_parse_result_path ws ~pr_kind:x.pr_kind ~pr_epoch:x.pr_epoch
              ~path_key:x.path_key ~doc_opt:x.doc_opt;
            loop (n - 1)
        | Some (ParseResultStale x) ->
            apply_parse_result_stale ws ~pr_epoch:x.pr_epoch
              ~path_key:x.path_key;
            loop (n - 1))
    in
    loop max_items

let background_parse_path (ws : t) (path : string) : unit =
  let path_key = normalize_path_key path in
  if path_key = "" || Hashtbl.mem ws.bg_parsed path_key then ()
  else
    match find_open_doc_for_path ws ~path with
    | Some open_doc -> (
        let uri = open_doc.Document.uri in
        let uri_key = Uri_path.docuri_to_string uri in
        if
          open_doc.Document.parse_rev = open_doc.Document.rev
          && not (Hashtbl.mem ws.open_parse_generation uri_key)
        then (
          Hashtbl.replace ws.bg_parsed path_key true;
          Hashtbl.remove ws.closed_doc_last_touch path_key)
        else
          let parse_generation =
            match Hashtbl.find_opt ws.open_parse_generation uri_key with
            | Some g -> g
            | None -> 0
          in
          let parsed_doc =
            if open_doc.Document.parse_rev = open_doc.Document.rev then open_doc
            else
              try
                Perf_stats.time "parse.open_doc_deferred" (fun () ->
                    parse_open_doc_worker ~max_bytes:ws.parse_file_max_bytes
                      open_doc)
              with exn ->
                with_internal_phase_diag open_doc ~phase:"open-doc-deferred"
                  ~exn
          in
          let latest_generation =
            match Hashtbl.find_opt ws.open_parse_generation uri_key with
            | Some g -> g
            | None -> 0
          in
          if latest_generation <> parse_generation then
            Perf_stats.tick "diag.open.stale_generation_drop"
          else
            let latest_version =
              match Hashtbl.find_opt ws.docs uri with
              | None -> None
              | Some latest_doc -> latest_doc.Document.lsp_version
            in
            if parsed_doc.Document.lsp_version <> latest_version then
              Perf_stats.tick "diag.open.stale_version_drop"
            else (
              let old_doc = Hashtbl.find_opt ws.docs uri in
              let enqueue_importer_revalidate = xmodule_diag_prereqs_ready ws in
              ignore
                (install_doc_surface ~enqueue_importer_revalidate
                   ~allow_ast_shed:false ws ~uri ~old_doc parsed_doc);
              if parsed_doc.Document.parse_rev = parsed_doc.Document.rev then (
                let key = Uri_path.docuri_to_string uri in
                (match Hashtbl.find_opt ws.open_provisional_since_ms key with
                | None -> ()
                | Some t0 ->
                    let lag = max 0.0 (Perf_stats.now_ms () -. t0) in
                    Perf_stats.observe_ms "diag.open.authoritative_lag_ms" lag;
                    Hashtbl.remove ws.open_provisional_since_ms key);
                Hashtbl.remove ws.open_parse_generation key;
                if xmodule_diag_prereqs_ready ws then
                  enqueue_open_diag_revalidate ws ~uri ~reason:"open_parse"
                else Perf_stats.tick "diag.open.revalidate_deferred_startup")
              else
                Hashtbl.replace ws.open_provisional_since_ms
                  (Uri_path.docuri_to_string uri)
                  (Perf_stats.now_ms ());
              invalidate_lsif_snapshot ws;
              Hashtbl.replace ws.files path_key parsed_doc;
              Hashtbl.replace ws.bg_parsed path_key true;
              Hashtbl.remove ws.closed_doc_last_touch path_key;
              enqueue_doc_imports_high ws parsed_doc))
    | None -> (
        let uri =
          match Uri_path.docuri_of_path path with
          | Some u -> u
          | None -> (
              match
                T.DocumentUri.t_of_yojson
                  (`String (Uri_path.file_uri_of_path path))
              with
              | u -> u
              | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))
        in
        let doc_opt =
          match file_size_bytes path with
          | Some n
            when is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes
                   ~text_len:n ->
              Some
                (make_doc_with_parse_guard ws ~uri ~file:(Some path) ~text:""
                   ~actual_bytes:n)
          | _ -> (
              match read_file_text path with
              | None -> None
              | Some text ->
                  let doc0 =
                    try
                      parse_guarded_document_make ~profile:Parser.Background ws
                        ~uri ~file:(Some path)
                        ~text
                    with exn ->
                      let fallback =
                        Document.make_with_profile ~profile:Parser.Background ~uri
                          ~file:(Some path) ~text:""
                      in
                      with_internal_phase_diag fallback ~phase:"bg-open-doc"
                        ~exn
                  in
                  Some (background_doc_with_diags ws doc0))
        in
        match doc_opt with
        | None -> ()
        | Some doc ->
            invalidate_lsif_snapshot ws;
            let old_doc = Hashtbl.find_opt ws.files path_key in
            let diff = doc_store_diff ~old_doc ~new_doc:doc in
            (match old_doc with
            | None -> ()
            | Some _ -> record_doc_diff_side_effects ws diff);
            revalidate_importers_for_doc_diff ws ~old_doc ~new_doc:doc ~diff
              ~enqueue_open_diag:true;
            Hashtbl.replace ws.files path_key doc;
            Hashtbl.replace ws.bg_parsed path_key true;
            touch_closed_doc_path ws ~path_key;
            evict_closed_docs_if_needed ws;
            if
              ws.sem_store_enabled && (diff.rev_changed || diff.parse_rev_changed)
            then Semantic_store.remove_uri ws.semantic_store ~uri:doc.Document.uri;
            queue_workspace_diag_update_for_doc ws doc)

let revalidate_closed_docs_for_hint_readiness (ws : t) : unit =
  Hashtbl.iter
    (fun path_key doc ->
      if has_open_doc_for_path_key ws ~path_key then ()
      else
        let updated = background_doc_with_diags ws doc in
        if Document.diagnostics updated <> Document.diagnostics doc then (
          Hashtbl.replace ws.files path_key updated;
          queue_workspace_diag_update_for_doc ws updated))
    ws.files

let maybe_build_symbol_hints_background (ws : t) : unit =
  if ws.symbol_hints <> None then ()
  else
    match ws.index with
    | None -> ()
    | Some idx ->
        if
          Queue.is_empty ws.bg_high_small_queue
          && Queue.is_empty ws.bg_norm_small_queue
          && Queue.is_empty ws.bg_high_large_queue
          && Queue.is_empty ws.bg_norm_large_queue
          && Workspace_index.is_complete idx
          && workspace_pressure_mode ws <> PressureCritical
        then (
          let idx =
            Perf_stats.time "bg.hint_build" (fun () -> symbol_hint_index ws)
          in
          ws.symbol_hints <- Some idx;
          revalidate_closed_docs_for_hint_readiness ws;
          enqueue_all_open_diag_revalidate ws ~reason:"hint_ready";
          Perf_stats.tick "diag.warmup_revalidate")

let background_tick (ws : t) ~(budget_ms : int) ~(mode : bg_tick_mode)
    ~(idle_quiet_ms : int) ~(last_message_ms : float) : unit =
  if budget_ms <= 0 then ()
  else (
    ensure_parse_worker_started ws;
    Perf_stats.tick "bg.tick";
    Perf_stats.tick "startup.phase_tick";
    update_pressure_state ws;
    maybe_shed_open_doc_parse_state ws;
    ignore
      (enqueue_pending_open_doc_parses ws
         ~reason_group:"foreground_catchup");
    let foreground_pending_before_results = has_pending_open_parse_work ws in
    let info_first_active = diagnostics_deferred_for_startup ws in
    let diag_stage_pending =
      ws.feature_flags.diagnostics
      && (ws.startup_diag_hover_ready_ms = None
         || Hashtbl.length ws.open_parse_generation > 0
         || not (startup_open_diag_revalidate_empty ws))
    in
    let parse_result_budget =
      match mode with
      | BgTickInteractive ->
          if info_first_active then 4
          else if diag_stage_pending then 10
          else if ws.startup_fully_nav_ready_ms = None then 4
          else 2
      | BgTickIdle -> 6
    in
    drain_parse_worker_results ws
      ~open_only:foreground_pending_before_results
      ~max_items:parse_result_budget;
    let foreground_pending = has_pending_open_parse_work ws in
    if foreground_pending then Perf_stats.tick "sched.open_doc_foreground_gate"
    else (
      pump_index_background ws;
      if workspace_pressure_mode ws <> PressureCritical then
        seed_bg_paths_from_index ws);
    let quick_nav_budget =
      match mode with
      | BgTickInteractive ->
          if info_first_active then max 2 ((budget_ms * 2) / 3)
          else if diag_stage_pending then max 1 (budget_ms / 6)
          else if ws.startup_fully_nav_ready_ms = None then max 2 (budget_ms / 2)
          else max 1 (budget_ms / 4)
      | BgTickIdle -> max 1 (budget_ms / 3)
    in
    if foreground_pending then Perf_stats.tick "quick_nav.deferred_open_doc"
    else quick_nav_index_step ws ~budget_ms:quick_nav_budget;
    let budget =
      match workspace_pressure_mode ws with
      | PressureNormal -> float_of_int budget_ms
      | PressureSoft -> float_of_int (max 1 (budget_ms / 2))
      | PressureCritical -> float_of_int (max 1 (budget_ms / 4))
    in
    let required_idle_quiet_ms =
      if diag_stage_pending then 0
      else max 0 (max idle_quiet_ms ws.bg_large_parse_idle_quiet_ms)
    in
    let allow_normal_large =
      match mode with
      | BgTickInteractive ->
          (not foreground_pending)
          && ws.startup_fully_nav_ready_ms = None
          && Queue.is_empty ws.bg_high_large_queue
          && Hashtbl.length ws.parse_worker_inflight
             < ws.parse_worker_max_inflight
      | BgTickIdle ->
          (not foreground_pending)
          &&
          let since_last_msg = Perf_stats.now_ms () -. last_message_ms in
          since_last_msg >= float_of_int required_idle_quiet_ms
    in
    let allow_root_large =
      match mode with
      | BgTickInteractive ->
          ws.startup_diag_hover_ready_ms = None
          || Hashtbl.length ws.open_parse_generation > 0
      | BgTickIdle ->
          let since_last_msg = Perf_stats.now_ms () -. last_message_ms in
          since_last_msg >= float_of_int required_idle_quiet_ms
    in
    let prefer_open_base =
      foreground_pending
      || ws.startup_diag_hover_ready_ms = None
      || Hashtbl.length ws.open_parse_generation > 0
    in
    let processed_total = ref 0 in
    let processed_open = ref 0 in
    let required_open_share =
      let pct = max 0 (min 100 ws.sched_open_doc_min_share_pct) in
      float_of_int pct /. 100.0
    in
    if
      mode = BgTickIdle && (not allow_normal_large)
      && not (Queue.is_empty ws.bg_norm_large_queue)
    then Perf_stats.tick "bg.parse.large_deferred_busy";
    let deadline = Perf_stats.now_ms () +. budget in
    let keep = ref true in
    while !keep && Perf_stats.now_ms () < deadline do
      let foreground_open_only = has_pending_open_parse_work ws in
      let prefer_open =
        if foreground_open_only then true
        else if not prefer_open_base then false
        else if !processed_total <= 0 then true
        else
          let share =
            float_of_int !processed_open /. float_of_int !processed_total
          in
          share < required_open_share
      in
      match
        dequeue_bg_path ws ~mode ~allow_normal_large ~allow_root_large
          ~open_only:foreground_open_only ~prefer_open
      with
      | None -> keep := false
      | Some (path, prio) ->
          incr processed_total;
          let path_key = normalize_path_key path in
          if has_open_doc_for_path_key ws ~path_key then incr processed_open;
          (match lane_of_bg_queue_kind prio with
          | LaneOpen -> Perf_stats.tick "sched.lane_a_ticks"
          | LaneRoot -> Perf_stats.tick "sched.lane_b_ticks"
          | LaneSweep -> Perf_stats.tick "sched.lane_c_ticks");
          if
            prio = BgQueueNormalSmall
            && workspace_pressure_mode ws = PressureCritical
          then (
            enqueue_bg_path ws ~lane:LaneSweep ~reason_group:"pressure_backoff"
              ~high:false path;
            keep := false)
          else (
            (match prio with
            | BgQueueHighSmall -> Perf_stats.tick "bg.queue_high"
            | BgQueueRootSmall -> Perf_stats.tick "bg.queue_root"
            | BgQueueNormalSmall -> Perf_stats.tick "bg.queue_normal"
            | BgQueueHighLarge | BgQueueRootLarge | BgQueueNormalLarge ->
                Perf_stats.tick "bg.parse.large_started");
            match prio with
            | BgQueueHighLarge | BgQueueRootLarge | BgQueueNormalLarge ->
                if try_submit_large_parse_job ws ~path ~queue_kind:prio then ()
                else
                  let lane =
                    match prio with
                    | BgQueueRootLarge -> LaneRoot
                    | BgQueueHighLarge ->
                        if
                          has_open_doc_for_path_key ws
                            ~path_key:(normalize_path_key path)
                        then LaneOpen
                        else LaneSweep
                    | _ -> LaneSweep
                  in
                  enqueue_bg_path ws ~lane
                    ~high:(prio = BgQueueHighLarge || prio = BgQueueRootLarge)
                    path;
                  keep := false
            | BgQueueHighSmall | BgQueueRootSmall | BgQueueNormalSmall ->
                ignore
                  (Perf_stats.time "bg.parse_doc" (fun () ->
                       background_parse_path ws path)))
    done;
    if has_pending_open_parse_work ws then
      Perf_stats.tick "bg.hint_build_deferred_open_doc"
    else maybe_build_symbol_hints_background ws;
    update_startup_ready_state ws)

let drain_pending_diag_updates (ws : t) ~(max_items : int) :
    (T.DocumentUri.t * int option * T.Diagnostic.t list) list =
  if max_items <= 0 then []
  else if not (bg_diag_allowed ws) then []
  else
    let rec loop n acc =
      if n <= 0 || Queue.is_empty ws.bg_pending_diag_updates then List.rev acc
      else
        let key = Queue.pop ws.bg_pending_diag_updates in
        Hashtbl.remove ws.bg_pending_diag_set key;
        match Hashtbl.find_opt ws.bg_pending_diag_payloads key with
        | None -> loop n acc
        | Some (uri, version, diags) ->
            Hashtbl.remove ws.bg_pending_diag_payloads key;
            if Hashtbl.mem ws.docs uri then (
              Perf_stats.tick "diag.open.stale_generation_drop";
              loop n acc)
            else loop (n - 1) ((uri, version, diags) :: acc)
    in
    loop max_items []

let drain_open_diag_revalidate_uris (ws : t) ~(max_items : int) :
    T.DocumentUri.t list =
  if max_items <= 0 then []
  else
    let rec loop n acc =
      if n <= 0 || Queue.is_empty ws.open_diag_revalidate_updates then
        List.rev acc
      else
        let key = Queue.pop ws.open_diag_revalidate_updates in
        Hashtbl.remove ws.open_diag_revalidate_set key;
        match Hashtbl.find_opt ws.open_diag_revalidate_payloads key with
        | None -> loop n acc
        | Some (uri, _reason) -> (
            Hashtbl.remove ws.open_diag_revalidate_payloads key;
            match refresh_open_doc_diags ~import_lookup_pump:false ws ~uri with
            | None -> loop n acc
            | Some _ ->
                Perf_stats.tick "diag.open.revalidate_drained";
                loop (n - 1) (uri :: acc))
    in
    let out = loop max_items [] in
    if out <> [] then update_startup_ready_state ws;
    out

let startup_diag_hover_ready_now (ws : t) : bool =
  update_startup_ready_state ws;
  ws.startup_diag_hover_ready_ms <> None

let startup_is_ready_now (ws : t) : bool =
  update_startup_ready_state ws;
  ws.startup_ready_ms <> None

let startup_readiness_json_for_report (ws : t) : Yojson.Safe.t =
  update_startup_ready_state ws;
  startup_readiness_json ws
