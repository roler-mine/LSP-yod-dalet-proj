module T = Lsp.Types
open Workspace_foundation
open Workspace_tuning

type t = Workspace_foundation.t

let profile_small_max_bytes = 10 * 1024 * 1024
let profile_medium_max_bytes = 40 * 1024 * 1024

let create ?(settings = Workspace_settings.from_env ()) () : t =
  let now = Perf_stats.now_ms () in
  {
    docs = Hashtbl.create 32;
    files = Hashtbl.create 64;
    root_path = None;
    source_file_paths = [];
    index = None;
    index_checkpoint_loaded = false;
    symbol_hints = None;
    semantic_store = Semantic_store.create ();
    semantic_tokens_cache = Hashtbl.create 32;
    sem_store_enabled = settings.sem_store_enabled;
    lsif_delta_enabled = settings.lsif_delta_enabled;
    lsif_delta_state = Lsif_delta.create ();
    lsif_snapshot_revision = 0;
    lsif_snapshot_payload = None;
    lsif_snapshot_symbols = None;
    workspace_diag_mode = settings.workspace_diag_mode;
    feature_flags = settings.feature_flags;
    bg_high_small_queue = Queue.create ();
    bg_norm_small_queue = Queue.create ();
    bg_root_small_queue = Queue.create ();
    bg_high_large_queue = Queue.create ();
    bg_root_large_queue = Queue.create ();
    bg_norm_large_queue = Queue.create ();
    bg_enqueued = Hashtbl.create 4096;
    bg_enqueue_recent_ms = Hashtbl.create 8192;
    bg_parsed = Hashtbl.create 4096;
    bg_seed_paths = [||];
    bg_seed_cursor = 0;
    graph_root_closure_paths = [||];
    graph_root_closure_cursor = 0;
    graph_nodes = Hashtbl.create 4096;
    graph_root_reason = Hashtbl.create 256;
    graph_root_closure_set = Hashtbl.create 4096;
    graph_needs_refresh = true;
    graph_epoch = 0;
    graph_scc_count = 0;
    bg_closed_diags = Hashtbl.create 1024;
    bg_pending_diag_updates = Queue.create ();
    bg_pending_diag_payloads = Hashtbl.create 1024;
    bg_pending_diag_set = Hashtbl.create 1024;
    bg_seed_needs_refresh = true;
    closed_doc_lru_clock = 0;
    closed_doc_last_touch = Hashtbl.create 4096;
    closed_doc_lru_max = settings.closed_doc_lru_max;
    parse_file_max_bytes = settings.parse_file_max_bytes;
    large_file_threshold_bytes = settings.large_file_threshold_bytes;
    huge_file_threshold_bytes = settings.huge_file_threshold_bytes;
    full_semantic_tokens_max_bytes = settings.full_semantic_tokens_max_bytes;
    full_parse_max_bytes = settings.full_parse_max_bytes;
    enable_huge_file_full_parse = settings.enable_huge_file_full_parse;
    bg_large_file_bytes = settings.bg_large_file_bytes;
    bg_large_parse_idle_quiet_ms = settings.bg_large_parse_idle_quiet_ms;
    pressure_soft_mb = settings.pressure_soft_mb;
    pressure_critical_mb = settings.pressure_critical_mb;
    pressure_mode = PressureNormal;
    pressure_live_mb = 0;
    pressure_last_check_ms = 0.0;
    startup_started_ms = now;
    startup_diag_hover_target_ms = settings.startup_diag_hover_target_ms;
    startup_nav_target_ms = settings.startup_nav_target_ms;
    startup_diag_hover_ready_ms = None;
    startup_fully_nav_ready_ms = None;
    startup_diag_hover_notified = false;
    startup_fully_nav_notified = false;
    startup_diag_hover_miss_notified = false;
    startup_nav_miss_notified = false;
    startup_diag_hover_miss_emitted = false;
    startup_nav_miss_emitted = false;
    startup_ready_ms = None;
    startup_ready_notified = false;
    startup_miss_notified = false;
    startup_miss_emitted = false;
    startup_phase = StartupCold;
    startup_phase_notified = None;
    startup_target_ms = settings.startup_target_ms;
    startup_priority_mode = settings.startup_priority_mode;
    startup_aggressive_window_ms = settings.startup_aggressive_window_ms;
    startup_aggressive_bg_budget_ms = settings.startup_aggressive_bg_budget_ms;
    open_diag_revalidate_updates = Queue.create ();
    open_diag_revalidate_payloads = Hashtbl.create 256;
    open_diag_revalidate_set = Hashtbl.create 256;
    index_reconcile_escalate_last_ms = 0.0;
    index_reconcile_escalations = 0;
    open_parse_generation = Hashtbl.create 128;
    open_provisional_since_ms = Hashtbl.create 128;
    xmodule_diag_ready_prev = false;
    source_bytes_estimate = None;
    source_bytes_estimate_count = -1;
    workspace_profile_mode = settings.workspace_profile_mode;
    root_model = settings.root_model;
    root_heuristic_fallback = settings.root_heuristic_fallback;
    root_manual_files = settings.root_manual_files;
    source_extensions = settings.source_extensions;
    graph_requeue_cooldown_ms = settings.graph_requeue_cooldown_ms;
    root_closure_max_depth = settings.root_closure_max_depth;
    root_closure_target_files = settings.root_closure_target_files;
    skeleton_prefix_bytes = settings.skeleton_prefix_bytes;
    sched_open_doc_min_share_pct = settings.sched_open_doc_min_share_pct;
    quick_nav_index = Hashtbl.create 2048;
    module_summary_cache = Hashtbl.create 2048;
    module_summary_compool_index = Hashtbl.create 512;
    module_summary_reverse_importers = Hashtbl.create 512;
    module_summary_cache_loaded = false;
    quick_nav_pending_paths = Queue.create ();
    quick_nav_pending_set = Hashtbl.create 4096;
    quick_nav_done_set = Hashtbl.create 4096;
    nav_quick_scan_offset_by_path = Hashtbl.create 4096;
    quick_nav_index_done = 0;
    quick_nav_index_total = 0;
    parse_worker_jobs = Queue.create ();
    parse_worker_results = Queue.create ();
    parse_worker_mtx = Mutex.create ();
    parse_worker_cv = Condition.create ();
    parse_worker_inflight = Hashtbl.create 4096;
    parse_worker_count = settings.parse_worker_count;
    parse_worker_max_inflight = settings.parse_worker_max_inflight;
    parse_worker_started = false;
    parse_worker_stop = false;
    bg_high_large_budget_ms = settings.bg_high_large_budget_ms;
    parse_epoch = 0;
    request_cancel_checker = None;
  }

let invalidate_lsif_snapshot (ws : t) : unit =
  ws.lsif_snapshot_revision <- ws.lsif_snapshot_revision + 1

let normalize_name (s : string) : string =
  String.uppercase_ascii (String.trim s)

let lane_of_bg_queue_kind = function
  | BgQueueHighSmall | BgQueueHighLarge -> LaneOpen
  | BgQueueRootSmall | BgQueueRootLarge -> LaneRoot
  | BgQueueNormalSmall | BgQueueNormalLarge -> LaneSweep

let string_of_lane = function
  | LaneOpen -> "open"
  | LaneRoot -> "root"
  | LaneSweep -> "sweep"

let queue_kind_priority = function
  | BgQueueNormalSmall | BgQueueNormalLarge -> 0
  | BgQueueRootSmall | BgQueueRootLarge -> 1
  | BgQueueHighSmall | BgQueueHighLarge -> 2

let size_class_of_path (ws : t) (path : string) : file_size_class =
  match try Some (Unix.stat path).Unix.st_size with _ -> None with
  | Some n when n >= ws.bg_large_file_bytes -> FileSizeLarge
  | _ -> FileSizeSmall

let file_class_rank = function
  | FileClassOpen -> 0
  | FileClassEntry -> 1
  | FileClassNormal -> 2

let basename_upper (path : string) : string =
  Filename.basename path |> String.uppercase_ascii

let is_main_boot_heuristic (path : string) : bool =
  let b = basename_upper path in
  String.length b >= 4
  && (String.sub b 0 4 = "MAIN" || String.sub b 0 4 = "BOOT")

let mark_graph_dirty (ws : t) : unit = ws.graph_needs_refresh <- true

let normalize_include_target (s : string) : string =
  s |> String.trim |> String.map (fun c -> if c = '\\' then '/' else c)

let icopy_include_targets_of_text ~(file : string option) ~(text : string) :
    string list =
  Workspace_include_model.include_targets_of_text ~file ~text
  |> List.map (fun (target : Workspace_include_model.include_target) ->
         target.target)

let icopy_include_targets_of_doc (doc : Document.t) : string list =
  icopy_include_targets_of_text ~file:doc.Document.file
    ~text:doc.Document.text

let is_nav_ident_start_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' | '$' -> true
  | _ -> false

let is_nav_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' | '\'' -> true
  | _ -> false

let ident_keys_of_text (text : string) : string list =
  let n = String.length text in
  let out = Hashtbl.create 16 in
  let rec loop i =
    if i >= n then ()
    else
      let c = text.[i] in
      if
        is_nav_ident_start_char c
        && (i = 0 || not (is_nav_ident_char text.[i - 1]))
      then (
        let j = ref (i + 1) in
        while !j < n && is_nav_ident_char text.[!j] do
          incr j
        done;
        let key = normalize_name (String.sub text i (!j - i)) in
        if key <> "" then Hashtbl.replace out key true;
        loop !j)
      else loop (i + 1)
  in
  loop 0;
  Hashtbl.fold (fun k _ acc -> k :: acc) out []

let compool_key_of_doc (doc : Document.t) : string option =
  match doc.Document.compool_def with
  | None -> None
  | Some raw ->
      let key = normalize_name raw in
      if key = "" then None else Some key

let invalidate_importer_nav_state_for_compool_key (ws : t)
    ~(compool_key : string) : unit =
  if not ws.sem_store_enabled then ()
  else
    let key = normalize_name compool_key in
    if key = "" then ()
    else
      Semantic_store.uris_importing_compool ws.semantic_store ~compool_key:key
      |> List.iter (fun uri ->
          Perf_stats.tick "query.cache.invalidate_uri";
          Semantic_store.remove_uri ws.semantic_store ~uri)

let importer_uris_for_compool_key (ws : t) ~(compool_key : string) :
    T.DocumentUri.t list =
  let key = normalize_name compool_key in
  if key = "" then []
  else
    let seen : (string, bool) Hashtbl.t = Hashtbl.create 64 in
    let out = ref [] in
    let add_uri (uri : T.DocumentUri.t) =
      let uri_key = Uri_path.docuri_to_string uri in
      if not (Hashtbl.mem seen uri_key) then (
        Hashtbl.replace seen uri_key true;
        out := uri :: !out)
    in
    if ws.sem_store_enabled then
      Semantic_store.uris_importing_compool ws.semantic_store ~compool_key:key
      |> List.iter add_uri;
    (match Hashtbl.find_opt ws.module_summary_reverse_importers key with
    | Some path_keys ->
        List.iter
          (fun path_key ->
            match Hashtbl.find_opt ws.module_summary_cache path_key with
            | Some entry -> (
                match
                  T.DocumentUri.t_of_yojson
                    (`String entry.msc_summary.Module_summary.source_uri)
                with
                | uri -> add_uri uri
                | exception _ -> ())
            | None -> ())
          path_keys
    | None -> ());
    Hashtbl.iter
      (fun uri doc ->
        if
          List.exists
            (fun (imp : Preprocess.import) ->
              imp.kind = Preprocess.Compool && normalize_name imp.name = key)
            (Document.imports doc)
        then add_uri uri)
      ws.docs;
    List.rev !out

let normalize_path_key = Uri_path.normalize_path_key
let same_path = Uri_path.same_path

let file_size_bytes (path : string) : int option =
  try Some Unix.(stat path).st_size with _ -> None

let is_parse_guard_exceeded ~(max_bytes : int) ~(text_len : int) : bool =
  max_bytes > 0 && text_len > max_bytes

let diag_parse_guard ~(file : string option) ~(max_bytes : int)
    ~(actual_bytes : int) : T.Diagnostic.t =
  let z = { Ast.Loc.line = 1; col = 0; offset = 0 } in
  let loc = Ast.Loc.make ~file ~start_pos:z ~end_pos:z in
  Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"parse"
    ~message:
      (Printf.sprintf "File parse skipped (%d bytes exceeds guard %d bytes)."
         actual_bytes max_bytes)
    loc

let make_doc_with_parse_guard ?lsp_version (ws : t)
    ~(uri : T.DocumentUri.t) ~(file : string option) ~(text : string)
    ~(actual_bytes : int) : Document.t =
  Perf_stats.tick "parse.large_file_guard";
  Document.make_parse_skipped_versioned ~lsp_version ~uri ~file ~text
    ~parse_diags:
      [
        diag_parse_guard ~file ~max_bytes:ws.parse_file_max_bytes ~actual_bytes;
      ]

let parse_guarded_document_make ?lsp_version
    ?(profile = Parser.Interactive) (ws : t) ~(uri : T.DocumentUri.t)
    ~(file : string option) ~(text : string) : Document.t =
  if
    is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes
      ~text_len:(String.length text)
  then
    make_doc_with_parse_guard ?lsp_version ws ~uri ~file ~text
      ~actual_bytes:(String.length text)
  else Document.make_with_profile_versioned ~lsp_version ~profile ~uri ~file ~text

let has_open_doc_for_path_key (ws : t) ~(path_key : string) : bool =
  Hashtbl.fold
    (fun _uri doc acc ->
      acc
      ||
      match doc.Document.file with
      | None -> false
      | Some p -> normalize_path_key p = path_key)
    ws.docs false

let is_schedulable_source_path (ws : t) (path : string) : bool =
  Source_file.has_extension ~extensions:ws.source_extensions
    (Filename.basename path)

let open_doc_parse_pending_for_path_key (ws : t) ~(path_key : string) : bool =
  path_key <> ""
  && (not (Hashtbl.mem ws.bg_parsed path_key))
  && Hashtbl.fold
       (fun _uri doc acc ->
         acc
         ||
         (doc.Document.parse_rev <> doc.Document.rev
         &&
         match doc.Document.file with
         | Some p ->
             normalize_path_key p = path_key && is_schedulable_source_path ws p
         | None -> false))
       ws.docs false

let has_pending_open_parse_work (ws : t) : bool =
  Hashtbl.fold
    (fun _uri doc acc ->
      acc
      ||
      (doc.Document.parse_rev <> doc.Document.rev
      &&
      match doc.Document.file with
      | Some p ->
          let path_key = normalize_path_key p in
          path_key <> ""
          && is_schedulable_source_path ws p
          && not (Hashtbl.mem ws.bg_parsed path_key)
      | None -> false))
    ws.docs false

let touch_closed_doc_path (ws : t) ~(path_key : string) : unit =
  if path_key = "" then ()
  else if has_open_doc_for_path_key ws ~path_key then
    Hashtbl.remove ws.closed_doc_last_touch path_key
  else (
    ws.closed_doc_lru_clock <- ws.closed_doc_lru_clock + 1;
    Hashtbl.replace ws.closed_doc_last_touch path_key ws.closed_doc_lru_clock)

let evict_closed_docs_if_needed (ws : t) : unit =
  let max_closed = max 1 ws.closed_doc_lru_max in
  let closed =
    Hashtbl.fold
      (fun path_key doc acc ->
        if has_open_doc_for_path_key ws ~path_key then acc
        else
          let touch =
            match Hashtbl.find_opt ws.closed_doc_last_touch path_key with
            | Some t -> t
            | None -> 0
          in
          (path_key, touch, doc) :: acc)
      ws.files []
  in
  Perf_stats.tick "mem.closed_doc_count";
  let closed_count = List.length closed in
  if closed_count <= max_closed then ()
  else
    let to_drop = closed_count - max_closed in
    let sorted =
      List.sort (fun (_, ta, _) (_, tb, _) -> compare ta tb) closed
    in
    let rec drop n xs =
      if n <= 0 then ()
      else
        match xs with
        | [] -> ()
        | (path_key, _, doc) :: tl ->
            Hashtbl.remove ws.files path_key;
            Hashtbl.remove ws.bg_parsed path_key;
            Hashtbl.remove ws.closed_doc_last_touch path_key;
            if ws.sem_store_enabled then (
              Perf_stats.tick "query.cache.invalidate_uri";
              Semantic_store.remove_uri ws.semantic_store ~uri:doc.Document.uri);
            Perf_stats.tick "mem.closed_doc_evict";
            drop (n - 1) tl
    in
    drop to_drop sorted

let find_open_doc_for_path (ws : t) ~(path : string) : Document.t option =
  let found_open = ref None in
  Hashtbl.iter
    (fun _uri doc ->
      match doc.Document.file with
      | Some p when same_path p path -> found_open := Some doc
      | _ -> ())
    ws.docs;
  !found_open

let classify_bg_queue_kind (ws : t) ~(lane : schedule_lane) ~(high : bool)
    ~(path : string) : bg_queue_kind =
  let size_opt = file_size_bytes path in
  let is_large =
    match size_opt with Some n -> n >= ws.bg_large_file_bytes | None -> false
  in
  match (lane, is_large) with
  | LaneOpen, false -> BgQueueHighSmall
  | LaneOpen, true -> BgQueueHighLarge
  | LaneRoot, false -> BgQueueRootSmall
  | LaneRoot, true -> BgQueueRootLarge
  | LaneSweep, false -> if high then BgQueueHighSmall else BgQueueNormalSmall
  | LaneSweep, true -> if high then BgQueueHighLarge else BgQueueNormalLarge

let enqueue_bg_path ?(lane : schedule_lane = LaneSweep)
    ?(reason_group : string = "generic") (ws : t) ~(high : bool) (path : string)
    : unit =
  if not (is_schedulable_source_path ws path) then
    Perf_stats.tick "sched.non_source_path_ignored"
  else
  let path_key = normalize_path_key path in
  if path_key <> "" then
    if Hashtbl.mem ws.parse_worker_inflight path_key then ()
    else
      let requested = classify_bg_queue_kind ws ~lane ~high ~path in
      let doc_generation =
        match find_open_doc_for_path ws ~path with
        | Some doc -> (
            match
              Hashtbl.find_opt ws.open_parse_generation
                (Uri_path.docuri_to_string doc.Document.uri)
            with
            | Some g -> g
            | None -> 0)
        | None -> 0
      in
      let work_key =
        Printf.sprintf "%s|%s|%s|g%d|d%d" path_key (string_of_lane lane)
          reason_group ws.graph_epoch doc_generation
      in
      let now = Perf_stats.now_ms () in
      let is_cooldown_hit =
        match Hashtbl.find_opt ws.bg_enqueue_recent_ms work_key with
        | Some last_ms ->
            ws.graph_requeue_cooldown_ms > 0
            && lane <> LaneOpen
            && now -. last_ms < float_of_int ws.graph_requeue_cooldown_ms
        | None -> false
      in
      if is_cooldown_hit then Perf_stats.tick "sched.requeue_cooldown_hit"
      else (
        Hashtbl.replace ws.bg_enqueue_recent_ms work_key now;
        match Hashtbl.find_opt ws.bg_enqueued path_key with
        | None -> (
            Hashtbl.replace ws.bg_enqueued path_key requested;
            match requested with
            | BgQueueHighSmall -> Queue.add path ws.bg_high_small_queue
            | BgQueueRootSmall -> Queue.add path ws.bg_root_small_queue
            | BgQueueNormalSmall -> Queue.add path ws.bg_norm_small_queue
            | BgQueueHighLarge -> Queue.add path ws.bg_high_large_queue
            | BgQueueRootLarge -> Queue.add path ws.bg_root_large_queue
            | BgQueueNormalLarge -> Queue.add path ws.bg_norm_large_queue)
        | Some current ->
            if queue_kind_priority requested > queue_kind_priority current then (
              Hashtbl.replace ws.bg_enqueued path_key requested;
              (match requested with
              | BgQueueHighSmall -> Queue.add path ws.bg_high_small_queue
              | BgQueueRootSmall -> Queue.add path ws.bg_root_small_queue
              | BgQueueNormalSmall -> Queue.add path ws.bg_norm_small_queue
              | BgQueueHighLarge -> Queue.add path ws.bg_high_large_queue
              | BgQueueRootLarge -> Queue.add path ws.bg_root_large_queue
              | BgQueueNormalLarge -> Queue.add path ws.bg_norm_large_queue);
              Perf_stats.tick "bg.queue_promoted")
            else Perf_stats.tick "sched.duplicate_work_dropped")

let enqueue_open_doc_parse_if_pending ?(reason_group : string = "open_doc")
    (ws : t) (doc : Document.t) : bool =
  if doc.Document.parse_rev = doc.Document.rev then false
  else
    match doc.Document.file with
    | None -> false
    | Some path ->
        if not (is_schedulable_source_path ws path) then false
        else
          let path_key = normalize_path_key path in
          if path_key = "" || Hashtbl.mem ws.bg_parsed path_key then false
          else if Hashtbl.mem ws.parse_worker_inflight path_key then false
          else if
            (match Hashtbl.find_opt ws.bg_enqueued path_key with
            | Some BgQueueHighSmall | Some BgQueueHighLarge -> true
            | _ -> false)
          then false
          else (
            enqueue_bg_path ws ~lane:LaneOpen ~reason_group ~high:true path;
            true)

let enqueue_pending_open_doc_parses ?(reason_group : string = "open_doc")
    (ws : t) : int =
  Hashtbl.fold
    (fun _uri doc acc ->
      if enqueue_open_doc_parse_if_pending ~reason_group ws doc then acc + 1
      else acc)
    ws.docs 0

let dequeue_bg_path (ws : t) ~(mode : bg_tick_mode) ~(allow_normal_large : bool)
    ~(allow_root_large : bool) ~(open_only : bool) ~(prefer_open : bool) :
    (string * bg_queue_kind) option =
  let pop_from_queue (q : string Queue.t) ~(expect : bg_queue_kind) :
      (string * bg_queue_kind) option =
    let skipped = Queue.create () in
    let restore () =
      while not (Queue.is_empty skipped) do
        Queue.add (Queue.pop skipped) q
      done
    in
    let rec loop () =
      if Queue.is_empty q then (
        restore ();
        None)
      else
        let p = Queue.pop q in
        let key = normalize_path_key p in
        match Hashtbl.find_opt ws.bg_enqueued key with
        | Some kind when kind = expect ->
            if (not open_only) || open_doc_parse_pending_for_path_key ws ~path_key:key
            then (
              Hashtbl.remove ws.bg_enqueued key;
              restore ();
              Some (p, kind))
            else (
              Queue.add p skipped;
              loop ())
        | _ -> loop ()
    in
    loop ()
  in
  let pop_high_small () =
    pop_from_queue ws.bg_high_small_queue ~expect:BgQueueHighSmall
  in
  let pop_high_large () =
    pop_from_queue ws.bg_high_large_queue ~expect:BgQueueHighLarge
  in
  let pop_root_small () =
    pop_from_queue ws.bg_root_small_queue ~expect:BgQueueRootSmall
  in
  let pop_root_large () =
    if not allow_root_large then None
    else pop_from_queue ws.bg_root_large_queue ~expect:BgQueueRootLarge
  in
  let pop_norm_small () =
    pop_from_queue ws.bg_norm_small_queue ~expect:BgQueueNormalSmall
  in
  let pop_norm_large () =
    match mode with
    | BgTickInteractive | BgTickIdle ->
        if not allow_normal_large then None
        else pop_from_queue ws.bg_norm_large_queue ~expect:BgQueueNormalLarge
  in
  let prioritize_open_large =
    ws.startup_diag_hover_ready_ms = None
    && Hashtbl.length ws.open_parse_generation > 0
    && Queue.is_empty ws.bg_high_small_queue
  in
  let pops =
    if prefer_open then
      if prioritize_open_large then
        [
          pop_high_large;
          pop_high_small;
          pop_root_small;
          pop_root_large;
          pop_norm_small;
          pop_norm_large;
        ]
      else
        [
          pop_high_small;
          pop_high_large;
          pop_root_small;
          pop_root_large;
          pop_norm_small;
          pop_norm_large;
        ]
    else if prioritize_open_large then
      [
        pop_root_small;
        pop_root_large;
        pop_high_large;
        pop_high_small;
        pop_norm_small;
        pop_norm_large;
      ]
    else
      [
        pop_root_small;
        pop_root_large;
        pop_high_small;
        pop_high_large;
        pop_norm_small;
        pop_norm_large;
      ]
  in
  let rec pick = function
    | [] -> None
    | f :: tl -> ( match f () with Some _ as hit -> hit | None -> pick tl)
  in
  pick pops

let enqueue_bg_diag_update ?version (ws : t) ~(uri : T.DocumentUri.t)
    ~(diags : T.Diagnostic.t list) : unit =
  let key = Uri_path.docuri_to_string uri in
  if Hashtbl.mem ws.bg_pending_diag_payloads key then
    Perf_stats.tick "bg.diag_pending_overwrite";
  Hashtbl.replace ws.bg_pending_diag_payloads key (uri, version, diags);
  if not (Hashtbl.mem ws.bg_pending_diag_set key) then (
    Hashtbl.replace ws.bg_pending_diag_set key true;
    Queue.add key ws.bg_pending_diag_updates)

let tick_open_diag_revalidate_reason (reason : string) : unit =
  match String.lowercase_ascii (String.trim reason) with
  | "compool" | "compool_change" ->
      Perf_stats.tick "diag.open.revalidate_reason_compool"
  | "reconcile" | "index_reconcile" ->
      Perf_stats.tick "diag.open.revalidate_reason_reconcile"
  | "hint" | "hint_ready" | "xmodule_ready" ->
      Perf_stats.tick "diag.open.revalidate_reason_hint_ready"
  | _ -> ()

let enqueue_open_diag_revalidate (ws : t) ~(uri : T.DocumentUri.t)
    ~(reason : string) : unit =
  if not ws.feature_flags.diagnostics then
    Perf_stats.tick "diag.open.revalidate_skipped_feature_off"
  else if Hashtbl.mem ws.docs uri then (
    let key = Uri_path.docuri_to_string uri in
    Hashtbl.replace ws.open_diag_revalidate_payloads key (uri, reason);
    if not (Hashtbl.mem ws.open_diag_revalidate_set key) then (
      Hashtbl.replace ws.open_diag_revalidate_set key true;
      Queue.add key ws.open_diag_revalidate_updates);
    Perf_stats.tick "diag.open.revalidate_enqueued";
    tick_open_diag_revalidate_reason reason)

let enqueue_all_open_diag_revalidate (ws : t) ~(reason : string) : unit =
  Hashtbl.iter
    (fun uri _ -> enqueue_open_diag_revalidate ws ~uri ~reason)
    ws.docs

let filter_workspace_diags (ws : t) (diags : T.Diagnostic.t list) :
    T.Diagnostic.t list =
  match ws.workspace_diag_mode with
  | WorkspaceDiagsOff -> []
  | WorkspaceDiagsAll -> diags
  | WorkspaceDiagsErrors ->
      List.filter
        (fun (d : T.Diagnostic.t) ->
          match d.severity with
          | Some T.DiagnosticSeverity.Error -> true
          | _ -> false)
        diags

let is_probably_network_path (p : string) : bool =
  let n = String.length p in
  n >= 2 && ((p.[0] = '\\' && p.[1] = '\\') || (p.[0] = '/' && p.[1] = '/'))

let module_summary_authority_label = function
  | ModuleSummaryProvisional -> "provisional"
  | ModuleSummaryMetadataValidated -> "metadataValidated"

let rebuild_module_summary_indexes (ws : t) : unit =
  Hashtbl.clear ws.module_summary_compool_index;
  Hashtbl.clear ws.module_summary_reverse_importers;
  let add_reverse ~compool path_key =
    let key = normalize_name compool in
    if key <> "" then
      let prev =
        match Hashtbl.find_opt ws.module_summary_reverse_importers key with
        | Some xs -> xs
        | None -> []
      in
      if not (List.mem path_key prev) then
        Hashtbl.replace ws.module_summary_reverse_importers key (path_key :: prev)
  in
  Hashtbl.iter
    (fun path_key entry ->
      (match entry.msc_summary.Module_summary.compool_name with
      | Some key when normalize_name key <> "" ->
          Hashtbl.replace ws.module_summary_compool_index (normalize_name key)
            path_key
      | _ -> ());
      List.iter
        (fun compool -> add_reverse ~compool path_key)
        entry.msc_summary.Module_summary.imported_compools)
    ws.module_summary_cache;
  Hashtbl.iter
    (fun key values ->
      Hashtbl.replace ws.module_summary_reverse_importers key
        (List.sort_uniq String.compare values))
    ws.module_summary_reverse_importers

let clear_module_summary_cache (ws : t) : unit =
  Hashtbl.clear ws.module_summary_cache;
  Hashtbl.clear ws.module_summary_compool_index;
  Hashtbl.clear ws.module_summary_reverse_importers;
  ws.module_summary_cache_loaded <- false

let install_module_summary_cache_entry (ws : t)
    (entry : module_summary_cache_entry) : unit =
  if entry.msc_path_key <> "" then (
    Hashtbl.replace ws.module_summary_cache entry.msc_path_key entry;
    rebuild_module_summary_indexes ws)

let remove_module_summary_cache_entry (ws : t) ~(path_key : string) : unit =
  if path_key <> "" then (
    Hashtbl.remove ws.module_summary_cache path_key;
    rebuild_module_summary_indexes ws)

let module_summary_entry_for_path (ws : t) ~(path : string) :
    module_summary_cache_entry option =
  Hashtbl.find_opt ws.module_summary_cache (normalize_path_key path)

let module_summary_entry_for_compool_key (ws : t) ~(compool_key : string) :
    module_summary_cache_entry option =
  let key = normalize_name compool_key in
  match Hashtbl.find_opt ws.module_summary_compool_index key with
  | None -> None
  | Some path_key -> Hashtbl.find_opt ws.module_summary_cache path_key

let module_summary_entry_for_uri (ws : t) ~(uri : T.DocumentUri.t) :
    module_summary_cache_entry option =
  let uri_s = Uri_path.docuri_to_string uri in
  match Uri_path.file_path_of_uri uri with
  | Some path -> module_summary_entry_for_path ws ~path
  | None ->
      Hashtbl.fold
        (fun _ entry acc ->
          match acc with
          | Some _ -> acc
          | None ->
              if entry.msc_summary.Module_summary.source_uri = uri_s then
                Some entry
              else None)
        ws.module_summary_cache None

let module_summary_cache_counts (ws : t) : int * int * int =
  Hashtbl.fold
    (fun _ entry (total, provisional, validated) ->
      match entry.msc_authority with
      | ModuleSummaryProvisional -> (total + 1, provisional + 1, validated)
      | ModuleSummaryMetadataValidated ->
          (total + 1, provisional, validated + 1))
    ws.module_summary_cache (0, 0, 0)

let set_root_path (ws : t) (root : string option) : unit =
  if ws.root_path <> root then (
    mark_graph_dirty ws;
    clear_module_summary_cache ws;
    ws.root_path <- root;
    ws.startup_started_ms <- Perf_stats.now_ms ();
    ws.startup_diag_hover_ready_ms <- None;
    ws.startup_fully_nav_ready_ms <- None;
    ws.startup_diag_hover_notified <- false;
    ws.startup_fully_nav_notified <- false;
    ws.startup_diag_hover_miss_notified <- false;
    ws.startup_nav_miss_notified <- false;
    ws.startup_diag_hover_miss_emitted <- false;
    ws.startup_nav_miss_emitted <- false;
    ws.startup_ready_ms <- None;
    ws.startup_ready_notified <- false;
    ws.startup_miss_notified <- false;
    ws.startup_miss_emitted <- false;
    ws.xmodule_diag_ready_prev <- false;
    Hashtbl.clear ws.open_diag_revalidate_payloads;
    Hashtbl.clear ws.open_diag_revalidate_set;
    while not (Queue.is_empty ws.open_diag_revalidate_updates) do
      ignore (Queue.pop ws.open_diag_revalidate_updates)
    done)

let set_root_uri (ws : t) (root_uri : T.DocumentUri.t option) : unit =
  set_root_path ws
    (match root_uri with None -> None | Some u -> Uri_path.file_path_of_uri u)

let set_source_files (ws : t) (paths : string list) : bool =
  let seen = Hashtbl.create (max 16 (List.length paths)) in
  let source_paths =
    paths
    |> List.filter_map (fun path ->
           let key = normalize_path_key path in
           if
             key = ""
             || Hashtbl.mem seen key
             || not
                  (Source_file.has_extension ~extensions:ws.source_extensions
                     (Filename.basename path))
           then None
           else (
             Hashtbl.replace seen key true;
             Some path))
    |> List.sort (fun a b -> compare (normalize_path_key a) (normalize_path_key b))
  in
  if source_paths <> ws.source_file_paths then (
    ws.source_file_paths <- source_paths;
    ws.index <- None;
    clear_module_summary_cache ws;
    mark_graph_dirty ws;
    ws.bg_seed_needs_refresh <- true;
    true)
  else false

let is_import_word_char = function
  | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let tokenize_upper_words_prefix ~(max_chars : int) (text : string) : string list
    =
  let n = min (String.length text) (max 0 max_chars) in
  let upper =
    if n = String.length text then String.uppercase_ascii text
    else String.uppercase_ascii (String.sub text 0 n)
  in
  let out = ref [] in
  let rec scan i =
    if i >= n then ()
    else if is_import_word_char upper.[i] then (
      let j = ref (i + 1) in
      while !j < n && is_import_word_char upper.[!j] do
        incr j
      done;
      out := String.sub upper i (!j - i) :: !out;
      scan !j)
    else scan (i + 1)
  in
  scan 0;
  List.rev !out

let quick_imports_from_deferred_doc_text (doc : Document.t) :
    Preprocess.import list =
  let tokens =
    tokenize_upper_words_prefix ~max_chars:nav_miss_import_scan_max_chars
      (Document.text doc)
  in
  let seen = Hashtbl.create 16 in
  let out = ref [] in
  let loc = Ast.Loc.none in
  let push_name (raw : string) =
    let name = normalize_name raw in
    if name <> "" && not (Hashtbl.mem seen name) then (
      Hashtbl.replace seen name true;
      out := { Preprocess.kind = Preprocess.Compool; name; loc } :: !out)
  in
  let rec collect = function
    | ("COMPOOL" | "ICOMPOOL") :: name :: tl ->
        push_name name;
        collect tl
    | _ :: tl -> collect tl
    | [] -> ()
  in
  collect tokens;
  List.rev !out

let best_effort_doc_imports_for_scheduling (doc : Document.t) :
    Preprocess.import list =
  let imports = Document.imports doc in
  if imports <> [] then imports
  else if doc.Document.parse_rev = doc.Document.rev then []
  else quick_imports_from_deferred_doc_text doc

let uniq_norm_strings (xs : string list) : string list =
  let seen = Hashtbl.create (max 16 (List.length xs)) in
  let out = ref [] in
  List.iter
    (fun x ->
      let k = normalize_name x in
      if k <> "" && not (Hashtbl.mem seen k) then (
        Hashtbl.replace seen k true;
        out := k :: !out))
    xs;
  List.rev !out

let diagnostics_for (ws : t) ~(uri : T.DocumentUri.t) : T.Diagnostic.t list =
  match Hashtbl.find_opt ws.docs uri with
  | Some doc -> Document.diagnostics doc
  | None ->
      let key = Uri_path.docuri_to_string uri in
      Option.value (Hashtbl.find_opt ws.bg_closed_diags key) ~default:[]

let document_version (ws : t) ~(uri : T.DocumentUri.t) : int option =
  match Hashtbl.find_opt ws.docs uri with
  | None -> None
  | Some doc -> Document.lsp_version doc

let diagnostics_snapshot_for (ws : t) ~(uri : T.DocumentUri.t) :
    int option * T.Diagnostic.t list =
  match Hashtbl.find_opt ws.docs uri with
  | Some doc -> (Document.lsp_version doc, Document.diagnostics doc)
  | None ->
      let key = Uri_path.docuri_to_string uri in
      (None, Option.value (Hashtbl.find_opt ws.bg_closed_diags key) ~default:[])

let ast_dump_for (ws : t) ~(uri : T.DocumentUri.t) : string option =
  match Hashtbl.find_opt ws.docs uri with
  | None -> None
  | Some doc -> Document.ast_dump ~max_depth:64 ~max_nodes:4000 doc

let cst_dump_for (ws : t) ~(uri : T.DocumentUri.t) : string option =
  match Hashtbl.find_opt ws.docs uri with
  | None -> None
  | Some doc ->
      let max_tokens = 2000 in
      let tokens =
        match Document.current_parse doc with
        | Some { Document.parsed_syntax = Some syntax; _ } -> (
            match syntax.Syntax_cache.raw_tokens with
            | Some toks -> toks
            | None ->
                Preprocess.lex_all_tokens_with_lexemes
                  ~file:doc.Document.file ~text:doc.Document.text)
        | _ ->
            Preprocess.lex_all_tokens_with_lexemes ~file:doc.Document.file
              ~text:doc.Document.text
      in
      let safe_substring text start_off end_off =
        let n = String.length text in
        let a = max 0 (min n start_off) in
        let b = max a (min n end_off) in
        String.sub text a (b - a)
      in
      let b = Buffer.create 4096 in
      Buffer.add_string b "CST (token stream)\n";
      let add_token_row idx (span : Preprocess.lex_tok) =
        let tok = span.Parser.tok in
        let line0 = max 1 span.start_line in
        let line1 = max 1 span.end_line in
        let col0 = max 0 span.start_col in
        let col1 = max 0 span.end_col in
        let tok_s = Parser.Debug.string_of_token tok in
        let raw =
          match span.lexeme with
          | Some s -> s
          | None -> safe_substring doc.Document.text span.start_off span.end_off
        in
        let lex = String.escaped raw in
        Buffer.add_string b
          (Printf.sprintf "%5d  %-14s %-28s @ %d:%d-%d:%d\n" idx tok_s
             ("\"" ^ lex ^ "\"")
             line0 col0 line1 col1)
      in
      let len = Array.length tokens in
      let stop = min len max_tokens in
      for i = 0 to stop - 1 do
        add_token_row (i + 1) tokens.(i)
      done;
      if len > max_tokens then
        Buffer.add_string b
          (Printf.sprintf "\n<truncated after %d tokens>\n" max_tokens);
      Some (Buffer.contents b)
