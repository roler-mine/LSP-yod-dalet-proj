module T = Lsp.Types
open Workspace_foundation
open Workspace_state
open Workspace_runtime
open Workspace_index_graph
open Workspace_imports
open Workspace_semantics
open Workspace_background
open Workspace_tuning

let compare_pos (a:T.Position.t) (b:T.Position.t) : int =
  if a.line <> b.line then compare a.line b.line
  else compare a.character b.character

let normalize_lsp_range (r:T.Range.t) : T.Range.t =
  if compare_pos r.start r.end_ <= 0 then r
  else { T.Range.start = r.end_; end_ = r.start }

let merge_lsp_range (a:T.Range.t) (b:T.Range.t) : T.Range.t =
  let a = normalize_lsp_range a in
  let b = normalize_lsp_range b in
  let start = if compare_pos a.start b.start <= 0 then a.start else b.start in
  let end_ = if compare_pos a.end_ b.end_ >= 0 then a.end_ else b.end_ in
  { T.Range.start; end_ }

let line_span_of_lsp_range (r:T.Range.t) : int =
  let r = normalize_lsp_range r in
  max 1 (r.end_.line - r.start.line + 1)

let didchange_semi_range (changes:T.TextDocumentContentChangeEvent.t list) : T.Range.t option =
  if not didchange_semi_check_enabled then None
  else if changes = [] || List.length changes > didchange_semi_check_max_changes then None
  else
    let merged = ref None in
    let total_text_chars = ref 0 in
    let all_ranged = ref true in
    List.iter
      (fun (ch:T.TextDocumentContentChangeEvent.t) ->
        total_text_chars := !total_text_chars + String.length ch.text;
        if !total_text_chars > didchange_semi_check_max_text_chars then
          all_ranged := false;
        match ch.range with
        | None -> all_ranged := false
        | Some r ->
            let r = normalize_lsp_range r in
            merged :=
              Some
                (match !merged with
                 | None -> r
                 | Some acc -> merge_lsp_range acc r))
      changes;
    if not !all_ranged then None
    else
      match !merged with
      | None -> None
      | Some r ->
          if line_span_of_lsp_range r > didchange_semi_check_max_lines then None
          else Some r

let offset_of_position_in_index (idx:Text_index.t) (p:T.Position.t) : int option =
  let clamp lo hi x =
    if x < lo then lo else if x > hi then hi else x
  in
  let line_count = Text_index.line_count idx in
  if line_count <= 0 then Some 0
  else
    let line = clamp 0 (line_count - 1) p.line in
    match Text_index.line_start_offset idx ~line with
    | None -> Some 0
    | Some start ->
        let line_len =
          match Text_index.line_length idx ~line with
          | Some n -> n
          | None -> 0
        in
        let col = clamp 0 line_len p.character in
        (match Text_index.offset_of_line_col idx ~line ~col with
         | Some off -> Some off
         | None -> Some (start + col))

let slice_of_range_for_text_index (text:string) (idx:Text_index.t) (r:T.Range.t) : string option =
  let r = normalize_lsp_range r in
  match offset_of_position_in_index idx r.start, offset_of_position_in_index idx r.end_ with
  | Some a, Some b ->
      let a, b = if a <= b then (a, b) else (b, a) in
      if a < 0 || b < a || b > String.length text then None
      else Some (String.sub text a (b - a))
  | _ ->
      None

let apply_text_change_only
    (text:string)
    (idx:Text_index.t)
    (ch:T.TextDocumentContentChangeEvent.t)
  : string * Text_index.t =
  match ch.range with
  | None ->
      let text' = ch.text in
      (text', Text_index.of_string text')
  | Some r ->
      (match offset_of_position_in_index idx r.start, offset_of_position_in_index idx r.end_ with
       | Some a, Some b ->
           let a, b = if a <= b then (a, b) else (b, a) in
           if a < 0 || b < a || b > String.length text then
             (text, idx)
           else
             let before = String.sub text 0 a in
             let after_len = String.length text - b in
             let after = if after_len <= 0 then "" else String.sub text b after_len in
             let text' = before ^ ch.text ^ after in
             (text', Text_index.of_string text')
       | _ ->
           (text, idx))

let touched_ident_keys_for_changes
    ~(old_doc:Document.t option)
    ~(changes:T.TextDocumentContentChangeEvent.t list)
  : string list =
  let out = Hashtbl.create 32 in
  let add_keys (keys:string list) : unit =
    List.iter (fun k ->
      let kk = normalize_name k in
      if kk <> "" then Hashtbl.replace out kk true
    ) keys
  in
  (match old_doc with
   | None ->
       List.iter (fun (ch:T.TextDocumentContentChangeEvent.t) ->
         add_keys (ident_keys_of_text ch.text)
       ) changes
   | Some doc ->
       let text_ref = ref doc.Document.text in
       let idx_ref = ref doc.Document.index in
       List.iter (fun (ch:T.TextDocumentContentChangeEvent.t) ->
         add_keys (ident_keys_of_text ch.text);
         (match ch.range with
          | None -> ()
          | Some r ->
              (match slice_of_range_for_text_index !text_ref !idx_ref r with
               | None -> ()
               | Some removed_text -> add_keys (ident_keys_of_text removed_text)));
         let text', idx' = apply_text_change_only !text_ref !idx_ref ch in
         text_ref := text';
         idx_ref := idx'
       ) changes);
  Hashtbl.fold (fun k _ acc -> k :: acc) out []

let should_defer_reparse_for_change
    (doc:Document.t)
    ~(changes:T.TextDocumentContentChangeEvent.t list)
    ~(next_rev:int)
  : bool =
  if not didchange_defer_parse_enabled then false
  else if next_rev mod didchange_defer_parse_force_full_every = 0 then false
  else if String.length doc.Document.text < didchange_defer_parse_min_doc_chars then false
  else if List.length changes = 0 || List.length changes > didchange_defer_parse_max_changes then false
  else if List.exists (fun (ch:T.TextDocumentContentChangeEvent.t) -> ch.range = None) changes then false
  else
    let inserted_chars =
      List.fold_left (fun acc (ch:T.TextDocumentContentChangeEvent.t) ->
        acc + String.length ch.text
      ) 0 changes
    in
    inserted_chars <= didchange_defer_parse_max_inserted_chars

let open_doc_converged (ws:t) ~(uri:T.DocumentUri.t) : bool =
  match Hashtbl.find_opt ws.docs uri with
  | None -> false
  | Some doc -> doc.Document.parse_rev = doc.Document.rev

let open_doc_generation_key (uri:T.DocumentUri.t) : string =
  Uri_path.docuri_to_string uri

let bump_open_parse_generation (ws:t) ~(uri:T.DocumentUri.t) : int =
  let key = open_doc_generation_key uri in
  let next =
    match Hashtbl.find_opt ws.open_parse_generation key with
    | Some n -> n + 1
    | None -> 1
  in
  Hashtbl.replace ws.open_parse_generation key next;
  next

let mark_open_doc_provisional (ws:t) ~(uri:T.DocumentUri.t) : unit =
  let key = open_doc_generation_key uri in
  Hashtbl.replace ws.open_provisional_since_ms key (Perf_stats.now_ms ())

let mark_open_doc_authoritative (ws:t) ~(uri:T.DocumentUri.t) : unit =
  let key = open_doc_generation_key uri in
  (match Hashtbl.find_opt ws.open_provisional_since_ms key with
   | None -> ()
   | Some t0 ->
       let lag = max 0.0 (Perf_stats.now_ms () -. t0) in
       Perf_stats.observe_ms "diag.open.authoritative_lag_ms" lag;
       Hashtbl.remove ws.open_provisional_since_ms key);
  Hashtbl.remove ws.open_parse_generation key

let open_doc
    ?(force_provisional:bool=false)
    (ws:t)
    ~(uri:T.DocumentUri.t)
    ~(file:string option)
    ~(text:string)
  : unit =
  invalidate_lsif_snapshot ws;
  startup_mark_started ws;
  (* If no workspace root was set, fall back to the first opened file's directory. *)
  (match ws.root_path, file with
   | None, Some f ->
       set_root_path ws (Some (Filename.dirname f));
       rescan ws
   | _ -> ());
  clear_nav_response_cache_for_uri ws ~uri;
  let should_defer_parse =
    force_provisional
    || didopen_always_provisional
    || (didopen_defer_parse_enabled
        && String.length text >= didopen_defer_parse_min_doc_chars)
  in
  ignore (bump_open_parse_generation ws ~uri);
  if should_defer_parse then mark_open_doc_provisional ws ~uri
  else mark_open_doc_authoritative ws ~uri;
  if should_defer_parse then Perf_stats.tick "open.parse_deferred";
  let doc =
    if should_defer_parse then
      if is_parse_guard_exceeded
           ~max_bytes:ws.parse_file_max_bytes
           ~text_len:(String.length text)
      then
        make_doc_with_parse_guard ws ~uri ~file ~text ~actual_bytes:(String.length text)
      else
        Document.make_unparsed ~uri ~file ~text ~parse_diags:[]
    else
      (try
         Perf_stats.time "parse.open_doc" (fun () -> parse_guarded_document_make ws ~uri ~file ~text)
       with exn ->
         let fallback = Document.make ~uri ~file ~text:"" in
         with_internal_phase_diag fallback ~phase:"open-doc" ~exn)
  in
  if should_defer_parse then
    store_doc_fast ws uri doc
  else
    store_doc ws uri doc;
  (match file with
   | Some p ->
       let path_key = normalize_path_key p in
      if should_defer_parse then (
        Hashtbl.remove ws.bg_parsed path_key;
         enqueue_bg_path ws ~lane:LaneOpen ~reason_group:"did_open_deferred" ~high:true p
       ) else
         Hashtbl.replace ws.bg_parsed path_key true
   | None -> ());
  if not should_defer_parse then
    enqueue_doc_imports_high ws doc;
  if not (should_defer_parse && didopen_disable_foreground_tick) then
    background_tick ws
      ~budget_ms:(if should_defer_parse then 40 else 120)
      ~mode:BgTickInteractive
      ~idle_quiet_ms:ws.bg_large_parse_idle_quiet_ms
      ~last_message_ms:(Perf_stats.now_ms ());
  if not force_provisional then
    pump_index_background ws;
  ignore (maybe_escalate_index_reconcile ws ~doc:(Some doc) ~reason:"didOpen");
  update_startup_ready_state ws

let change_doc (ws:t) ~(uri:T.DocumentUri.t) ~(changes:T.TextDocumentContentChangeEvent.t list) : unit =
  if changes = [] then ()
  else
  mark_graph_dirty ws;
  invalidate_lsif_snapshot ws;
  ignore (bump_open_parse_generation ws ~uri);
  let old_doc = Hashtbl.find_opt ws.docs uri in
  let clear_cache_for_full_sync =
    List.exists (fun (ch:T.TextDocumentContentChangeEvent.t) -> ch.range = None) changes
  in
  if clear_cache_for_full_sync then
    clear_nav_response_cache_for_uri ws ~uri
  else
    let touched = touched_ident_keys_for_changes ~old_doc ~changes in
    invalidate_nav_response_cache_for_keys ws ~uri ~keys:touched;
  let semantic_mode_for_rev (rev:int) : semantic_validation_mode =
    let mode =
      if rev mod didchange_semi_force_full_every = 0 then SemanticFull
      else
        match didchange_semi_range changes with
        | Some r -> SemanticRangeSemi r
        | None -> SemanticFull
    in
    (match mode with
     | SemanticRangeSemi _ -> Perf_stats.tick "change.semantic_semi"
     | SemanticFull -> Perf_stats.tick "change.semantic_full");
    mode
  in
  match old_doc with
  | None ->
      let file = Uri_path.file_path_of_uri uri in
      let base = Document.make ~uri ~file ~text:"" in
      let draft =
        Perf_stats.time "apply.change_doc_fast" (fun () ->
          Document.apply_changes_no_reparse ~changes base)
      in
      if is_parse_guard_exceeded
           ~max_bytes:ws.parse_file_max_bytes
           ~text_len:(String.length draft.Document.text)
      then (
        let guarded =
          Document.with_parse_diags
            [diag_parse_guard
               ~file:draft.Document.file
               ~max_bytes:ws.parse_file_max_bytes
               ~actual_bytes:(String.length draft.Document.text)]
            draft
        in
        Perf_stats.tick "parse.large_file_guard";
        store_doc_fast ws uri guarded
      ) else (
        let doc =
          try
            Perf_stats.time "parse.change_doc" (fun () ->
              Document.apply_changes_and_reparse ~changes base)
          with exn ->
            with_internal_phase_diag base ~phase:"apply-changes" ~exn
        in
        let semantic_mode = semantic_mode_for_rev doc.rev in
        store_doc ~import_lookup_pump:false ~semantic_mode ws uri doc
      );
      pump_index_background ws
  | Some doc ->
      let doc_fast =
        Perf_stats.time "apply.change_doc_fast" (fun () ->
          Document.apply_changes_no_reparse ~changes doc)
      in
      let next_rev = doc_fast.Document.rev in
      if is_parse_guard_exceeded
           ~max_bytes:ws.parse_file_max_bytes
           ~text_len:(String.length doc_fast.Document.text)
      then (
        let guarded =
          Document.with_parse_diags
            [diag_parse_guard
               ~file:doc_fast.Document.file
               ~max_bytes:ws.parse_file_max_bytes
               ~actual_bytes:(String.length doc_fast.Document.text)]
            doc_fast
        in
        Perf_stats.tick "parse.large_file_guard";
        store_doc_fast ws uri guarded;
        pump_index_background ws
      ) else if should_defer_reparse_for_change doc ~changes ~next_rev then (
        Perf_stats.tick "change.parse_deferred";
        store_doc_fast ws uri doc_fast;
        pump_index_background ws
      ) else (
        let doc' =
          try
            Perf_stats.time "parse.change_doc" (fun () ->
              Document.apply_changes_and_reparse ~changes doc)
          with exn ->
            with_internal_phase_diag doc ~phase:"apply-changes" ~exn
        in
        let semantic_mode = semantic_mode_for_rev doc'.rev in
        store_doc ~import_lookup_pump:false ~semantic_mode ws uri doc';
        pump_index_background ws
      );
  (match Hashtbl.find_opt ws.docs uri with
   | Some latest ->
       if latest.Document.parse_rev = latest.Document.rev then
         mark_open_doc_authoritative ws ~uri
       else
         mark_open_doc_provisional ws ~uri;
       ignore (maybe_escalate_index_reconcile ws ~doc:(Some latest) ~reason:"didChange")
   | None -> ());
  update_startup_ready_state ws

let close_doc (ws:t) ~(uri:T.DocumentUri.t) : unit =
  mark_graph_dirty ws;
  invalidate_lsif_snapshot ws;
  clear_nav_response_cache_for_uri ws ~uri;
  let closed_path_key : string option ref = ref None in
  (match Hashtbl.find_opt ws.docs uri with
   | Some d ->
        (match d.Document.file with
         | Some p -> closed_path_key := Some (normalize_path_key p)
         | None -> ());
        (match d.Document.compool_def with
        | Some name when normalize_name name <> "" -> invalidate_symbol_hints ws
        | _ -> ());
        enqueue_doc_imports_high ws d
   | None -> ());
  Hashtbl.remove ws.docs uri;
  Hashtbl.remove ws.open_parse_generation (open_doc_generation_key uri);
  Hashtbl.remove ws.open_provisional_since_ms (open_doc_generation_key uri);
  Hashtbl.remove ws.open_diag_revalidate_payloads (open_doc_generation_key uri);
  Hashtbl.remove ws.open_diag_revalidate_set (open_doc_generation_key uri);
  (match !closed_path_key with
   | Some key ->
       touch_closed_doc_path ws ~path_key:key;
       evict_closed_docs_if_needed ws
   | None -> ());
  if ws.sem_store_enabled then
    (match Semantic_store.snapshot_for_uri ws.semantic_store ~uri with
     | Some snap when snap.Doc_snapshot.path_key <> None -> ()
     | _ -> Semantic_store.remove_uri ws.semantic_store ~uri);
  pump_index_background ws;
  update_startup_ready_state ws

let apply_watched_file_changes
    (ws:t)
    ~(changes:(string * [ `Created | `Changed | `Deleted ]) list)
  : unit =
  if changes <> [] then mark_graph_dirty ws;
  if changes <> [] then invalidate_lsif_snapshot ws;
  if changes <> [] then Hashtbl.clear ws.nav_response_cache;
  ensure_index_started ws;
  let hints_dirty = ref false in
  let sem_dirty = ref false in
  (match ws.index with
   | None -> ()
   | Some idx ->
       List.iter (fun (path, kind) ->
         try
           let mapped_kind =
             match kind with
             | `Created -> Workspace_index.Created
             | `Changed -> Workspace_index.Changed
             | `Deleted -> Workspace_index.Deleted
           in
           if Workspace_index.apply_file_change idx ~path ~kind:mapped_kind then
             hints_dirty := true;
           let path_key = normalize_path_key path in
           Hashtbl.remove ws.files path_key;
           Hashtbl.remove ws.bg_parsed path_key;
           Hashtbl.remove ws.closed_doc_last_touch path_key;
           (match kind with
            | `Deleted ->
                (match Uri_path.docuri_of_path path with
                 | Some uri ->
                     Hashtbl.remove ws.bg_closed_diags (Uri_path.docuri_to_string uri);
                     enqueue_bg_diag_update ws ~uri ~diags:[]
                 | None -> ())
            | `Created | `Changed ->
                enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"watch_change" ~high:true path)
         with _ -> ()
       ) changes;
       let max_dirs, max_files =
         if is_network_root ws then
           (index_background_dirs_network, index_background_files_network)
         else
           (index_background_dirs, index_bootstrap_files)
       in
        (try
          ignore
            (Workspace_index.scan_step idx
               ~max_dirs
               ~max_files)
        with _ -> ()));
  if ws.sem_store_enabled then (
    List.iter (fun (path, _kind) ->
      let path_key = normalize_path_key path in
      if path_key <> "" then
        let removed = Semantic_store.invalidate_path_and_dependents ws.semantic_store ~path_key in
        if removed <> [] then sem_dirty := true
    ) changes
  );
  ws.bg_seed_cursor <- 0;
  ws.bg_seed_needs_refresh <- true;
  if !hints_dirty then invalidate_symbol_hints ws
  else if !sem_dirty then invalidate_symbol_hints ws;
  update_startup_ready_state ws

let revalidate_all (ws:t) : T.DocumentUri.t list =
  let uris = Hashtbl.fold (fun uri _ acc -> uri :: acc) ws.docs [] in
  List.iter (fun uri ->
    match Hashtbl.find_opt ws.docs uri with
    | None -> ()
    | Some doc -> store_doc ws uri doc
  ) uris;
  update_startup_ready_state ws;
  uris
