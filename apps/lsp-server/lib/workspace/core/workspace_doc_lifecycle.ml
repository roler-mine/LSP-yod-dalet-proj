(* Module overview: Handles document open, change, close, and watched-file lifecycle transitions. *)

module T = Lsp.Types
open Workspace_foundation
open Workspace_state
open Workspace_runtime
open Workspace_index_graph
open Workspace_semantics
open Workspace_background
open Workspace_imports
open Workspace_tuning

let normalize_lsp_range (r : T.Range.t) : T.Range.t =
  if
    (if r.start.line <> r.end_.line then compare r.start.line r.end_.line
     else compare r.start.character r.end_.character)
    <= 0
  then r
  else { T.Range.start = r.end_; end_ = r.start }

let merge_lsp_range (a : T.Range.t) (b : T.Range.t) : T.Range.t =
  let a = normalize_lsp_range a in
  let b = normalize_lsp_range b in
  let cmp_pos (x : T.Position.t) (y : T.Position.t) =
    if x.line <> y.line then compare x.line y.line
    else compare x.character y.character
  in
  let start = if cmp_pos a.start b.start <= 0 then a.start else b.start in
  let end_ = if cmp_pos a.end_ b.end_ >= 0 then a.end_ else b.end_ in
  { T.Range.start; end_ }

let line_span_of_lsp_range (r : T.Range.t) : int =
  let r = normalize_lsp_range r in
  max 1 (r.end_.line - r.start.line + 1)

let didchange_semi_range (changes : T.TextDocumentContentChangeEvent.t list) :
    T.Range.t option =
  if not didchange_semi_check_enabled then None
  else if changes = [] || List.length changes > didchange_semi_check_max_changes
  then None
  else
    let merged = ref None in
    let total_text_chars = ref 0 in
    let all_ranged = ref true in
    List.iter
      (fun (ch : T.TextDocumentContentChangeEvent.t) ->
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

let touched_ident_keys_for_changes ~(old_doc : Document.t option)
    ~(changes : T.TextDocumentContentChangeEvent.t list) : string list =
  let out = Hashtbl.create 32 in
  let add_keys (keys : string list) : unit =
    List.iter
      (fun k ->
        let kk = normalize_name k in
        if kk <> "" then Hashtbl.replace out kk true)
      keys
  in
  (match old_doc with
  | None ->
      List.iter
        (fun (ch : T.TextDocumentContentChangeEvent.t) ->
          add_keys (ident_keys_of_text ch.text))
        changes
  | Some doc ->
      let text_ref = ref doc.Document.text in
      let idx_ref = ref doc.Document.index in
      List.iter
        (fun (ch : T.TextDocumentContentChangeEvent.t) ->
          add_keys (ident_keys_of_text ch.text);
          (match ch.range with
          | None -> ()
          | Some r -> (
              match
                Document.slice_of_range ~text:!text_ref ~index:!idx_ref r
              with
              | None -> ()
              | Some removed_text -> add_keys (ident_keys_of_text removed_text)));
          let text', idx' =
            Document.apply_content_change ~text:!text_ref ~index:!idx_ref ch
          in
          text_ref := text';
          idx_ref := idx')
        changes);
  Hashtbl.fold (fun k _ acc -> k :: acc) out []

let should_defer_reparse_for_change (doc : Document.t)
    ~(changes : T.TextDocumentContentChangeEvent.t list) ~(next_rev : int) :
    bool =
  if not didchange_defer_parse_enabled then false
  else if next_rev mod didchange_defer_parse_force_full_every = 0 then false
  else if String.length doc.Document.text < didchange_defer_parse_min_doc_chars
  then false
  else if
    List.length changes = 0
    || List.length changes > didchange_defer_parse_max_changes
  then false
  else if
    List.exists
      (fun (ch : T.TextDocumentContentChangeEvent.t) -> ch.range = None)
      changes
  then false
  else
    let inserted_chars =
      List.fold_left
        (fun acc (ch : T.TextDocumentContentChangeEvent.t) ->
          acc + String.length ch.text)
        0 changes
    in
    inserted_chars <= didchange_defer_parse_max_inserted_chars

let open_doc_converged (ws : t) ~(uri : T.DocumentUri.t) : bool =
  match Hashtbl.find_opt ws.docs uri with
  | None -> false
  | Some doc -> doc.Document.parse_rev = doc.Document.rev

let open_doc_generation_key (uri : T.DocumentUri.t) : string =
  Uri_path.docuri_to_string uri

let bump_open_parse_generation (ws : t) ~(uri : T.DocumentUri.t) : int =
  let key = open_doc_generation_key uri in
  let next =
    match Hashtbl.find_opt ws.open_parse_generation key with
    | Some n -> n + 1
    | None -> 1
  in
  Hashtbl.replace ws.open_parse_generation key next;
  next

let mark_open_doc_provisional (ws : t) ~(uri : T.DocumentUri.t) : unit =
  let key = open_doc_generation_key uri in
  Hashtbl.replace ws.open_provisional_since_ms key (Perf_stats.now_ms ())

let mark_open_doc_authoritative (ws : t) ~(uri : T.DocumentUri.t) : unit =
  let key = open_doc_generation_key uri in
  (match Hashtbl.find_opt ws.open_provisional_since_ms key with
  | None -> ()
  | Some t0 ->
      let lag = max 0.0 (Perf_stats.now_ms () -. t0) in
      Perf_stats.observe_ms "diag.open.authoritative_lag_ms" lag;
      Hashtbl.remove ws.open_provisional_since_ms key);
  Hashtbl.remove ws.open_parse_generation key

let should_defer_open_doc ?(force_provisional : bool = false) (ws : t)
    ~(text : string) : bool =
  let open_doc_count = Hashtbl.length ws.docs in
  let workspace_forces_provisional = open_doc_count >= 8 in
  force_provisional || didopen_always_provisional
  || didopen_defer_parse_enabled
     && String.length text >= didopen_defer_parse_min_doc_chars
  || String.length text >= ws.bg_large_file_bytes
  || workspace_forces_provisional
  || workspace_pressure_mode ws <> PressureNormal

let startup_open_doc_preview_enabled (ws : t) : bool =
  ws.startup_diag_hover_ready_ms = None
  && ws.workspace_diag_mode = WorkspaceDiagsOff

let preview_open_doc_diags ?(force_provisional : bool = false) (ws : t)
    ~(uri : T.DocumentUri.t) ~(file : string option) ~(text : string) :
    T.Diagnostic.t list option =
  let _ = uri in
  if
    is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes
      ~text_len:(String.length text)
  then
    Some
      [
        diag_parse_guard ~file ~max_bytes:ws.parse_file_max_bytes
          ~actual_bytes:(String.length text);
      ]
  else if startup_open_doc_preview_enabled ws then Some []
  else if should_defer_open_doc ~force_provisional ws ~text then Some []
  else None

let zero_lsp_range : T.Range.t =
  let pos = { T.Position.line = 0; character = 0 } in
  { T.Range.start = pos; end_ = pos }

let starts_with ~prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let first_significant_line ?(max_scan_bytes : int = 8192) (text : string) :
    (int * int * string) option =
  let n = String.length text in
  let rec leading_col i stop =
    if i >= stop then 0
    else
      match text.[i] with ' ' | '\t' -> 1 + leading_col (i + 1) stop | _ -> 0
  in
  let rec loop line start =
    if start >= n || start >= max_scan_bytes then None
    else
      let line_end =
        match String.index_from_opt text start '\n' with
        | Some i -> min i max_scan_bytes
        | None -> min n max_scan_bytes
      in
      let raw = String.sub text start (line_end - start) in
      let trimmed = String.trim raw in
      if
        trimmed = ""
        || starts_with ~prefix:"%" trimmed
        || starts_with ~prefix:"\"" trimmed
      then loop (line + 1) (line_end + 1)
      else Some (line, leading_col start line_end, trimmed)
  in
  loop 0 0

let first_word_upper (s : string) : string =
  let n = String.length s in
  let rec stop i =
    if i >= n then i
    else
      match s.[i] with
      | ' ' | '\t' | '\r' | '\n' | ';' | '(' | ')' -> i
      | _ -> stop (i + 1)
  in
  String.uppercase_ascii (String.sub s 0 (stop 0))

let diagnostic_at_line ~line ~character ~message : T.Diagnostic.t =
  let start = { T.Position.line; character } in
  let end_ = { T.Position.line; character = character + 1 } in
  {
    T.Diagnostic.range = { T.Range.start; end_ };
    severity = Some T.DiagnosticSeverity.Error;
    code = None;
    codeDescription = None;
    source = Some "parse";
    message = `String message;
    tags = None;
    relatedInformation = None;
    data = None;
  }

(* Large-file didChange can intentionally defer full parsing; this bounded
   check catches obvious source-shape breakage so diagnostics still move. *)
let provisional_live_edit_parse_diags (doc : Document.t) : T.Diagnostic.t list =
  match first_significant_line doc.Document.text with
  | Some (line, character, text) when first_word_upper text <> "START" ->
      [
        diagnostic_at_line ~line ~character
          ~message:
            "Expected START before source text; full diagnostics are updating \
             in the background.";
      ]
  | _ -> []

let with_provisional_live_edit_diags (doc : Document.t) : Document.t =
  match provisional_live_edit_parse_diags doc with
  | [] -> doc
  | diags ->
      Perf_stats.tick "diag.open.provisional_live_edit";
      Document.with_parse_diags diags doc

let doc_has_compool_import (doc : Document.t) : bool =
  Document.imports doc
  |> List.exists (fun (imp : Preprocess.import) ->
      match imp.kind with Preprocess.Compool -> true)

let cached_authoritative_doc_for_open ?lsp_version (ws : t)
    ~(uri : T.DocumentUri.t) ~(file : string option) ~(text : string) :
    Document.t option =
  match file with
  | None -> None
  | Some path -> (
      match doc_from_path_cached_only ws path with
      | None -> None
      | Some cached ->
          if
            cached.Document.text = text && Document.current_parse cached <> None
          then Some (Document.with_identity ?lsp_version ~uri ~file cached)
          else None)

type open_doc_install_result = {
  doc : Document.t;
  should_defer_parse : bool;
  workspace_forces_provisional : bool;
}

let open_doc_install ?lsp_version ?(force_provisional : bool = false)
    ?(defer_cross_module_semantics : bool = false) (ws : t)
    ~(uri : T.DocumentUri.t) ~(file : string option) ~(text : string) :
    open_doc_install_result =
  (* If no workspace root was set, fall back to the first opened file's directory. *)
  (match (ws.root_path, file) with
  | None, Some f ->
      set_root_path ws (Some (Filename.dirname f));
      rescan ws
  | _ -> ());
  let open_doc_count = Hashtbl.length ws.docs in
  let workspace_forces_provisional = open_doc_count >= 8 in
  let cached_doc =
    cached_authoritative_doc_for_open ?lsp_version ws ~uri ~file ~text
  in
  if cached_doc = None then invalidate_lsif_snapshot ws
  else Perf_stats.tick "snapshot.invalidate_skipped_cache_hit";
  if cached_doc = None then startup_mark_open_doc ws ~bytes:(String.length text)
  else Perf_stats.tick "open.startup_window_preserved_cache_hit";
  let should_defer_parse =
    cached_doc = None && should_defer_open_doc ~force_provisional ws ~text
  in
  ignore (bump_open_parse_generation ws ~uri);
  if should_defer_parse then mark_open_doc_provisional ws ~uri
  else mark_open_doc_authoritative ws ~uri;
  if should_defer_parse then Perf_stats.tick "open.parse_deferred";
  let doc =
    match cached_doc with
    | Some doc ->
        Perf_stats.tick "open.cache_hit";
        doc
    | None -> (
        if should_defer_parse then
          if
            is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes
              ~text_len:(String.length text)
          then
            make_doc_with_parse_guard ?lsp_version ws ~uri ~file ~text
              ~actual_bytes:(String.length text)
          else
            Document.make_unparsed_versioned ~lsp_version ~uri ~file ~text
              ~parse_diags:[]
        else
          try
            Perf_stats.time "parse.open_doc" (fun () ->
                parse_guarded_document_make ?lsp_version ws ~uri ~file ~text)
          with exn ->
            let fallback =
              Document.make_versioned ~lsp_version ~uri ~file ~text:""
            in
            with_internal_phase_diag fallback ~phase:"open-doc" ~exn)
  in
  if cached_doc <> None then
    store_doc_fast ~preserve_current_semantic_snapshot:true ws uri doc
  else if should_defer_parse then store_doc_fast ws uri doc
  else if defer_cross_module_semantics && doc_has_compool_import doc then (
    Perf_stats.tick "open.semantic_deferred";
    store_doc ~import_lookup_pump:false
      ~semantic_mode:(SemanticRangeSemi zero_lsp_range) ws uri doc)
  else store_doc ws uri doc;
  (match file with
  | Some p ->
      let path_key = normalize_path_key p in
      if should_defer_parse then (
        Hashtbl.remove ws.bg_parsed path_key;
        enqueue_bg_path ws ~lane:LaneOpen ~reason_group:"did_open_deferred"
          ~high:true p)
      else Hashtbl.replace ws.bg_parsed path_key true
  | None -> ());
  if not should_defer_parse then (
    if defer_cross_module_semantics then
      Perf_stats.tick "open.import_parse_deferred";
    enqueue_doc_imports_high ws doc);
  update_startup_ready_state ws;
  { doc; should_defer_parse; workspace_forces_provisional }

let open_doc ?lsp_version ?(force_provisional : bool = false)
    ?(inline_catch_up : bool = true)
    ?(defer_cross_module_semantics : bool = false) (ws : t)
    ~(uri : T.DocumentUri.t) ~(file : string option) ~(text : string) : unit =
  let { doc; should_defer_parse; workspace_forces_provisional } =
    open_doc_install ?lsp_version ~force_provisional
      ~defer_cross_module_semantics ws ~uri ~file ~text
  in
  if not inline_catch_up then Perf_stats.tick "diag.open.inline_work_deferred"
  else (
    if not (should_defer_parse && didopen_disable_foreground_tick) then
      background_tick ws
        ~budget_ms:(if should_defer_parse then 40 else 120)
        ~mode:BgTickInteractive ~idle_quiet_ms:ws.bg_large_parse_idle_quiet_ms
        ~last_message_ms:(Perf_stats.now_ms ());
    let allow_inline_index_work =
      (not force_provisional)
      && (not workspace_forces_provisional)
      && workspace_pressure_mode ws = PressureNormal
    in
    if allow_inline_index_work then pump_index_background ws;
    if allow_inline_index_work then
      ignore
        (maybe_escalate_index_reconcile ws ~doc:(Some doc) ~reason:"didOpen");
    if allow_inline_index_work && not should_defer_parse then
      update_startup_ready_state ws)

let change_doc ?lsp_version (ws : t) ~(uri : T.DocumentUri.t)
    ~(changes : T.TextDocumentContentChangeEvent.t list) : unit =
  if changes = [] then ()
  else
    let old_doc = Hashtbl.find_opt ws.docs uri in
    let existing_doc_fast =
      match old_doc with
      | Some doc ->
          Some
            ( doc,
              Perf_stats.time "apply.change_doc_fast" (fun () ->
                  Document.apply_changes_no_reparse ?lsp_version ~changes doc)
            )
      | None -> None
    in
    match existing_doc_fast with
    | Some (doc, doc_fast) when doc_fast == doc -> Perf_stats.tick "change.noop"
    | _ ->
        invalidate_lsif_snapshot ws;
        ignore (bump_open_parse_generation ws ~uri);
        let semantic_mode_for_rev (rev : int) : semantic_validation_mode =
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
        (match existing_doc_fast with
        | None ->
            let file = Uri_path.file_path_of_uri uri in
            let base =
              Document.make_versioned ~lsp_version ~uri ~file ~text:""
            in
            let draft =
              Perf_stats.time "apply.change_doc_fast" (fun () ->
                  Document.apply_changes_no_reparse ?lsp_version ~changes base)
            in
            if
              is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes
                ~text_len:(String.length draft.Document.text)
            then (
              let guarded =
                Document.with_parse_skipped
                  [
                    diag_parse_guard ~file:draft.Document.file
                      ~max_bytes:ws.parse_file_max_bytes
                      ~actual_bytes:(String.length draft.Document.text);
                  ]
                  draft
              in
              Perf_stats.tick "parse.large_file_guard";
              store_doc_fast ws uri guarded)
            else
              let doc =
                try
                  Perf_stats.time "parse.change_doc" (fun () ->
                      Document.apply_changes_and_reparse ?lsp_version ~changes
                        base)
                with exn ->
                  with_internal_phase_diag base ~phase:"apply-changes" ~exn
              in
              let semantic_mode = semantic_mode_for_rev doc.rev in
              store_doc ~import_lookup_pump:false ~semantic_mode ws uri doc;
              pump_index_background ws
        | Some (doc, doc_fast) ->
            let next_rev = doc_fast.Document.rev in
            if
              is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes
                ~text_len:(String.length doc_fast.Document.text)
            then (
              let guarded =
                Document.with_parse_skipped
                  [
                    diag_parse_guard ~file:doc_fast.Document.file
                      ~max_bytes:ws.parse_file_max_bytes
                      ~actual_bytes:(String.length doc_fast.Document.text);
                  ]
                  doc_fast
              in
              Perf_stats.tick "parse.large_file_guard";
              store_doc_fast ws uri guarded;
              pump_index_background ws)
            else if should_defer_reparse_for_change doc ~changes ~next_rev then (
              Perf_stats.tick "change.parse_deferred";
              store_doc_fast ws uri (with_provisional_live_edit_diags doc_fast);
              pump_index_background ws)
            else
              let doc' =
                try
                  Perf_stats.time "parse.change_doc" (fun () ->
                      Document.apply_changes_and_reparse ?lsp_version ~changes
                        doc)
                with exn ->
                  with_internal_phase_diag doc ~phase:"apply-changes" ~exn
              in
              let semantic_mode = semantic_mode_for_rev doc'.rev in
              store_doc ~import_lookup_pump:false ~semantic_mode ws uri doc';
              pump_index_background ws);
        (match Hashtbl.find_opt ws.docs uri with
        | Some latest ->
            ignore
              (enqueue_open_doc_parse_if_pending
                 ~reason_group:"did_change_deferred" ws latest);
            if latest.Document.parse_rev = latest.Document.rev then
              mark_open_doc_authoritative ws ~uri
            else mark_open_doc_provisional ws ~uri;
            ignore
              (maybe_escalate_index_reconcile ws ~doc:(Some latest)
                 ~reason:"didChange")
        | None -> ());
        update_startup_ready_state ws

let close_doc (ws : t) ~(uri : T.DocumentUri.t) : unit =
  mark_graph_dirty ws;
  invalidate_lsif_snapshot ws;
  let closed_path_key : string option ref = ref None in
  (match Hashtbl.find_opt ws.docs uri with
  | Some d ->
      let uri_s = Uri_path.docuri_to_string uri in
      let diags = Document.diagnostics d in
      if diags = [] then Hashtbl.remove ws.bg_closed_diags uri_s
      else Hashtbl.replace ws.bg_closed_diags uri_s diags;
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
  (if ws.sem_store_enabled then
     match Semantic_store.snapshot_for_uri ws.semantic_store ~uri with
     | Some snap when snap.Semantic_store.Snapshot.path_key <> None -> ()
     | _ ->
         Perf_stats.tick "query.cache.invalidate_uri";
         Semantic_store.remove_uri ws.semantic_store ~uri);
  pump_index_background ws;
  update_startup_ready_state ws

let sync_source_file_paths_from_watch (ws : t)
    (changes : (string * [ `Created | `Changed | `Deleted ]) list) : bool =
  let paths = Hashtbl.create (max 16 (List.length ws.source_file_paths)) in
  List.iter
    (fun path ->
      let key = normalize_path_key path in
      if key <> "" then Hashtbl.replace paths key path)
    ws.source_file_paths;
  let changed = ref false in
  List.iter
    (fun (path, kind) ->
      let key = normalize_path_key path in
      if key <> "" then
        match kind with
        | `Deleted ->
            if Hashtbl.mem paths key then (
              Hashtbl.remove paths key;
              changed := true)
        | `Created | `Changed ->
            if not (Hashtbl.mem paths key) then (
              Hashtbl.replace paths key path;
              changed := true))
    changes;
  if !changed then
    ws.source_file_paths <-
      Hashtbl.fold (fun _ path acc -> path :: acc) paths []
      |> List.sort (fun a b ->
          compare (normalize_path_key a) (normalize_path_key b));
  !changed

let apply_watched_file_changes (ws : t)
    ~(changes : (string * [ `Created | `Changed | `Deleted ]) list) : unit =
  let changes =
    changes
    |> List.filter (fun (path, _kind) ->
        Source_file.has_extension ~extensions:ws.source_extensions
          (Filename.basename path))
  in
  let source_set_changed = sync_source_file_paths_from_watch ws changes in
  let changed_path_keys =
    changes
    |> List.filter_map (fun (path, _kind) ->
        let key = normalize_path_key path in
        if key = "" then None else Some key)
    |> List.sort_uniq String.compare
  in
  let dependent_paths =
    if changed_path_keys = [] then []
    else graph_reverse_dependency_closure_paths ws ~path_keys:changed_path_keys
  in
  if changes <> [] then mark_graph_dirty ws;
  if changes <> [] then invalidate_lsif_snapshot ws;
  ensure_index_started ws;
  let hints_dirty = ref false in
  let sem_dirty = ref false in
  (match ws.index with
  | None -> ()
  | Some idx ->
      List.iter
        (fun (path, kind) ->
          try
            let mapped_kind =
              match kind with
              | `Created -> Workspace_index.Created
              | `Changed -> Workspace_index.Changed
              | `Deleted -> Workspace_index.Deleted
            in
            if Workspace_index.apply_file_change idx ~path ~kind:mapped_kind
            then hints_dirty := true;
            let path_key = normalize_path_key path in
            Hashtbl.remove ws.files path_key;
            Hashtbl.remove ws.bg_parsed path_key;
            Hashtbl.remove ws.closed_doc_last_touch path_key;
            remove_module_summary_cache_entry ws ~path_key;
            match kind with
            | `Deleted -> (
                match Uri_path.docuri_of_path path with
                | Some uri ->
                    Hashtbl.remove ws.bg_closed_diags
                      (Uri_path.docuri_to_string uri);
                    enqueue_bg_diag_update ws ~uri ~diags:[]
                | None -> ())
            | `Created | `Changed ->
                enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"watch_change"
                  ~high:true path
          with _ -> ())
        changes);
  (if changes <> [] then
     match ws.index with
     | None -> ()
     | Some idx -> save_index_checkpoint_if_possible ws idx);
  if dependent_paths <> [] then (
    Perf_stats.tick "dep.invalidate.file_reverse";
    Perf_stats.observe_ms "dep.invalidate.file_reverse_paths"
      (float_of_int (List.length dependent_paths));
    List.iter
      (fun path ->
        enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"dep_file_change"
          ~high:true path)
      dependent_paths);
  if ws.sem_store_enabled then
    List.iter
      (fun (path, _kind) ->
        let path_key = normalize_path_key path in
        if path_key <> "" then (
          Perf_stats.tick "query.cache.invalidate_path";
          let removed =
            Semantic_store.invalidate_path_and_dependents ws.semantic_store
              ~path_key
          in
          if removed <> [] then
            Perf_stats.observe_ms "query.cache.invalidate_path_removed"
              (float_of_int (List.length removed));
          if removed <> [] then sem_dirty := true))
      changes;
  ws.bg_seed_cursor <- 0;
  if changes <> [] || source_set_changed then ws.bg_seed_needs_refresh <- true;
  if !hints_dirty then invalidate_symbol_hints ws
  else if !sem_dirty then invalidate_symbol_hints ws;
  update_startup_ready_state ws

let revalidate_all (ws : t) : T.DocumentUri.t list =
  let uris = Hashtbl.fold (fun uri _ acc -> uri :: acc) ws.docs [] in
  List.iter
    (fun uri ->
      ignore (refresh_open_doc_diags ~import_lookup_pump:true ws ~uri))
    uris;
  update_startup_ready_state ws;
  uris
