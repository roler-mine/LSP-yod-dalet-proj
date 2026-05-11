module T = Lsp.Types
open Workspace_foundation
open Workspace_state
open Workspace_tuning

let pressure_mode_to_string = function
  | PressureNormal -> "normal"
  | PressureSoft -> "soft"
  | PressureCritical -> "critical"

let startup_phase_to_string = function
  | StartupCold -> "cold"
  | StartupWarming -> "warming"
  | StartupAggressiveCatchUp -> "aggressiveCatchUp"
  | StartupReady -> "ready"

let word_bytes : int = max 1 (Sys.word_size / 8)

let words_to_mb (words : int) : int =
  let bytes = max 0 (words * word_bytes) in
  let mb = 1024 * 1024 in
  if bytes <= 0 then 0 else (bytes + mb - 1) / mb

let update_pressure_state (ws : t) : unit =
  let now = Perf_stats.now_ms () in
  if now -. ws.pressure_last_check_ms >= float_of_int pressure_check_interval_ms
  then (
    ws.pressure_last_check_ms <- now;
    let live_words, spike_words =
      try
        let s = Gc.quick_stat () in
        let live_words = max s.live_words s.heap_words in
        let spike_words = max live_words s.top_heap_words in
        (live_words, spike_words)
      with _ -> (0, 0)
    in
    let live_mb = words_to_mb live_words in
    let spike_mb = words_to_mb spike_words in
    ws.pressure_live_mb <- live_mb;
    let soft_mb = max 1 ws.pressure_soft_mb in
    let critical_mb = max soft_mb ws.pressure_critical_mb in
    let next_mode =
      match ws.pressure_mode with
      | PressureNormal ->
          if spike_mb >= critical_mb then PressureCritical
          else if spike_mb >= soft_mb then PressureSoft
          else PressureNormal
      | PressureSoft | PressureCritical ->
          if live_mb >= critical_mb then PressureCritical
          else if live_mb >= soft_mb then PressureSoft
          else PressureNormal
    in
    if next_mode <> ws.pressure_mode then (
      (match next_mode with
      | PressureSoft ->
          Perf_stats.tick "pressure.enter_soft";
          Perf_stats.tick "mem.pressure_soft"
      | PressureCritical ->
          Perf_stats.tick "pressure.enter_critical";
          Perf_stats.tick "mem.pressure_critical"
      | PressureNormal -> Perf_stats.tick "pressure.exit_to_normal");
      ws.pressure_mode <- next_mode))

let workspace_pressure_mode (ws : t) : pressure_mode =
  update_pressure_state ws;
  ws.pressure_mode

let workspace_pressure_live_mb (ws : t) : int =
  update_pressure_state ws;
  ws.pressure_live_mb

let bg_diag_allowed (ws : t) : bool =
  if not ws.feature_flags.diagnostics then false
  else
    match workspace_pressure_mode ws with
    | PressureCritical ->
        Perf_stats.tick "bg.diag_paused_critical";
        false
    | PressureNormal | PressureSoft -> true

let lsif_doc_load_budget_for_pressure (ws : t) : int =
  match workspace_pressure_mode ws with
  | PressureNormal -> lsif_doc_load_budget_normal
  | PressureSoft ->
      Perf_stats.tick "lsif.throttle_soft";
      lsif_doc_load_budget_soft
  | PressureCritical ->
      Perf_stats.tick "lsif.defer_critical";
      0

let request_cancelled (ws : t) : bool =
  match ws.request_cancel_checker with
  | None -> false
  | Some f -> ( try f () with _ -> false)

let with_request_cancel_checker (ws : t) (is_cancelled : unit -> bool)
    (f : unit -> 'a) : 'a =
  let prev = ws.request_cancel_checker in
  ws.request_cancel_checker <- Some is_cancelled;
  Fun.protect ~finally:(fun () -> ws.request_cancel_checker <- prev) f

let startup_mark_started (ws : t) : unit =
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
  ws.startup_phase <- StartupWarming;
  ws.startup_phase_notified <- None;
  Perf_stats.tick "startup.phase_warming";
  ws.xmodule_diag_ready_prev <- false;
  Hashtbl.clear ws.open_diag_revalidate_payloads;
  Hashtbl.clear ws.open_diag_revalidate_set;
  while not (Queue.is_empty ws.open_diag_revalidate_updates) do
    ignore (Queue.pop ws.open_diag_revalidate_updates)
  done;
  ws.index_reconcile_escalate_last_ms <- 0.0;
  ws.index_reconcile_escalations <- 0

let startup_open_docs_converged (ws : t) : bool =
  Hashtbl.fold
    (fun _ doc acc -> acc && doc.Document.parse_rev = doc.Document.rev)
    ws.docs true

let startup_open_docs_pending_count (ws : t) : int =
  Hashtbl.fold
    (fun _ doc acc ->
      if doc.Document.parse_rev = doc.Document.rev then acc else acc + 1)
    ws.docs 0

let startup_index_complete (ws : t) : bool =
  match ws.index with
  | None -> false
  | Some idx -> Workspace_index.is_complete idx

let startup_index_reconcile_pending (_ws : t) : bool = false

let startup_seed_complete (ws : t) : bool =
  ws.graph_root_closure_cursor >= Array.length ws.graph_root_closure_paths
  && (not ws.graph_needs_refresh)
  && (not ws.bg_seed_needs_refresh)
  && ws.bg_seed_cursor >= Array.length ws.bg_seed_paths

let startup_high_queues_empty (ws : t) : bool =
  Queue.is_empty ws.bg_high_small_queue
  && Queue.is_empty ws.bg_high_large_queue
  && Queue.is_empty ws.bg_root_small_queue
  && Queue.is_empty ws.bg_root_large_queue
  && not
       (Hashtbl.fold
          (fun _ kind acc ->
            acc || kind = ParseJobHighLarge || kind = ParseJobRootLarge)
          ws.parse_worker_inflight false)

let parse_worker_queues_empty (ws : t) : bool =
  Queue.is_empty ws.parse_worker_jobs
  && Queue.is_empty ws.parse_worker_results
  && Hashtbl.length ws.parse_worker_inflight = 0

let startup_queues_empty (ws : t) : bool =
  startup_high_queues_empty ws
  && Queue.is_empty ws.bg_norm_small_queue
  && Queue.is_empty ws.bg_norm_large_queue
  && Queue.is_empty ws.bg_pending_diag_updates
  && Hashtbl.length ws.bg_pending_diag_payloads = 0
  && parse_worker_queues_empty ws

let quick_nav_index_complete (ws : t) : bool =
  let no_pending =
    Queue.is_empty ws.quick_nav_pending_paths
    && Hashtbl.length ws.quick_nav_pending_set = 0
  in
  no_pending
  &&
  match ws.index with
  | Some idx when Workspace_index.source_count idx = 0 ->
      ws.quick_nav_index_total = 0 && ws.quick_nav_index_done = 0
  | _ ->
      ws.quick_nav_index_total > 0
      && ws.quick_nav_index_done >= ws.quick_nav_index_total

let quick_nav_index_ready_for_startup (ws : t) : bool =
  if quick_nav_index_complete ws then true
  else if ws.quick_nav_index_total <= 0 then false
  else
    let goal = min ws.quick_nav_index_total 64 in
    ws.quick_nav_index_done >= goal

let startup_hints_ready (ws : t) : bool =
  ws.symbol_hints <> None
  || ws.workspace_diag_mode = WorkspaceDiagsOff
     && quick_nav_index_ready_for_startup ws
  ||
  match ws.index with
  | Some idx ->
      Workspace_index.is_complete idx
      && Queue.is_empty ws.bg_high_small_queue
      && Queue.is_empty ws.bg_root_small_queue
      && Queue.is_empty ws.bg_norm_small_queue
      && Queue.is_empty ws.bg_high_large_queue
      && Queue.is_empty ws.bg_root_large_queue
      && Queue.is_empty ws.bg_norm_large_queue
  | None -> false

let startup_nav_prereqs_ready (ws : t) : bool =
  ws.graph_root_closure_cursor >= Array.length ws.graph_root_closure_paths
  && (startup_hints_ready ws || quick_nav_index_ready_for_startup ws)

let startup_open_docs_authoritative (ws : t) : bool =
  startup_open_docs_converged ws && Hashtbl.length ws.open_parse_generation = 0

let startup_open_diag_revalidate_empty (ws : t) : bool =
  Queue.is_empty ws.open_diag_revalidate_updates
  && Hashtbl.length ws.open_diag_revalidate_payloads = 0

let xmodule_diag_prereqs_ready (ws : t) : bool =
  startup_index_complete ws
  && (not (startup_index_reconcile_pending ws))
  && startup_hints_ready ws
  && startup_open_docs_authoritative ws

let startup_navigation_prereqs_ready_now (ws : t) : bool =
  xmodule_diag_prereqs_ready ws

let diagnostics_deferred_for_startup (ws : t) : bool =
  ws.feature_flags.diagnostics
  && ws.startup_priority_mode = StartupPriorityInfoFirst
  && not (startup_navigation_prereqs_ready_now ws)

let startup_is_diag_hover_ready (ws : t) : bool =
  (not ws.feature_flags.diagnostics)
  || (xmodule_diag_prereqs_ready ws && startup_open_diag_revalidate_empty ws)

let startup_is_fully_navigable (ws : t) : bool =
  startup_nav_prereqs_ready ws && startup_is_diag_hover_ready ws

let startup_elapsed_ms_float (ws : t) : float =
  let now = Perf_stats.now_ms () in
  max 0.0 (now -. ws.startup_started_ms)

let open_doc_count (ws : t) : int = Hashtbl.length ws.docs

let startup_update_phase (ws : t) : unit =
  let next_phase =
    match ws.startup_fully_nav_ready_ms with
    | Some _ -> StartupReady
    | None ->
        let elapsed_ms = startup_elapsed_ms_float ws in
        let warm_window =
          float_of_int
            (max 0 (ws.startup_nav_target_ms - ws.startup_aggressive_window_ms))
        in
        if elapsed_ms >= warm_window then StartupAggressiveCatchUp
        else StartupWarming
  in
  if ws.startup_phase <> next_phase then (
    ws.startup_phase <- next_phase;
    match next_phase with
    | StartupWarming -> Perf_stats.tick "startup.phase_warming"
    | StartupAggressiveCatchUp -> Perf_stats.tick "startup.phase_aggressive"
    | StartupCold | StartupReady -> ())

let startup_ready_components (ws : t) :
    bool
    * bool
    * bool
    * bool
    * bool
    * bool
    * bool
    * bool
    * bool
    * bool
    * bool
    * bool =
  ( startup_index_complete ws,
    not (startup_index_reconcile_pending ws),
    startup_seed_complete ws,
    startup_high_queues_empty ws,
    startup_queues_empty ws,
    startup_hints_ready ws,
    startup_nav_prereqs_ready ws,
    quick_nav_index_complete ws,
    startup_open_docs_converged ws,
    startup_open_docs_authoritative ws,
    xmodule_diag_prereqs_ready ws,
    startup_open_diag_revalidate_empty ws )

let startup_is_ready (ws : t) : bool =
  let ( index_complete,
        index_reconcile_clear,
        seed_complete,
        _high_queues_empty,
        queues_empty,
        hints_ready,
        nav_prereqs_ready,
        quick_nav_index_complete,
        open_docs_converged,
        open_docs_authoritative,
        xmodule_ready,
        open_diag_revalidate_empty ) =
    startup_ready_components ws
  in
  index_complete && index_reconcile_clear && seed_complete && queues_empty
  && hints_ready && nav_prereqs_ready && quick_nav_index_complete
  && open_docs_converged && open_docs_authoritative && xmodule_ready
  && open_diag_revalidate_empty

let startup_elapsed_ms (ws : t) : int =
  max 0 (int_of_float (startup_elapsed_ms_float ws))

let update_startup_ready_state (ws : t) : unit =
  let elapsed = startup_elapsed_ms ws in
  if
    elapsed > ws.startup_diag_hover_target_ms
    && (not ws.startup_diag_hover_miss_notified)
    && ws.startup_diag_hover_ready_ms = None
  then (
    ws.startup_diag_hover_miss_notified <- true;
    Perf_stats.tick "startup.miss_15s");
  if
    elapsed > ws.startup_nav_target_ms
    && (not ws.startup_nav_miss_notified)
    && ws.startup_fully_nav_ready_ms = None
  then ws.startup_nav_miss_notified <- true;

  let xmodule_ready_now = xmodule_diag_prereqs_ready ws in
  if xmodule_ready_now && not ws.xmodule_diag_ready_prev then (
    ws.xmodule_diag_ready_prev <- true;
    Perf_stats.tick "diag.xmodule_ready_transition";
    if not ws.feature_flags.diagnostics then
      Perf_stats.tick "diag.open.revalidate_skipped_feature_off"
    else enqueue_all_open_diag_revalidate ws ~reason:"xmodule_ready")
  else if not xmodule_ready_now then ws.xmodule_diag_ready_prev <- false;

  (if startup_is_diag_hover_ready ws then
     match ws.startup_diag_hover_ready_ms with
     | Some _ -> ()
     | None ->
         let now = Perf_stats.now_ms () in
         ws.startup_diag_hover_ready_ms <- Some now
   else
     match ws.startup_diag_hover_ready_ms with
     | None -> ()
     | Some _ ->
         ws.startup_diag_hover_ready_ms <- None;
         ws.startup_diag_hover_notified <- false);

  (if startup_is_ready ws then (
     match ws.startup_fully_nav_ready_ms with
     | Some _ -> ()
     | None ->
         let now = Perf_stats.now_ms () in
         ws.startup_fully_nav_ready_ms <- Some now;
         ws.startup_phase <- StartupReady;
         Perf_stats.tick "startup.ready";
         if ws.startup_phase_notified = Some StartupReady then
           ws.startup_phase_notified <- None)
   else
     match ws.startup_fully_nav_ready_ms with
     | None -> ()
     | Some _ ->
         ws.startup_fully_nav_ready_ms <- None;
         ws.startup_fully_nav_notified <- false;
         ws.startup_ready_notified <- false;
         ws.startup_phase_notified <- None;
         Perf_stats.tick "startup.ready_retracted");
  ws.startup_ready_ms <- ws.startup_fully_nav_ready_ms;
  ws.startup_miss_notified <- ws.startup_nav_miss_notified;
  ws.startup_miss_emitted <- ws.startup_nav_miss_emitted;
  startup_update_phase ws

let startup_readiness_json (ws : t) : Yojson.Safe.t =
  let ( index_complete,
        index_reconcile_clear,
        seed_complete,
        high_queues_empty,
        queues_empty,
        hints_ready,
        nav_prereqs_ready,
        quick_nav_ready,
        open_docs_converged,
        open_docs_authoritative,
        xmodule_ready,
        open_diag_revalidate_empty ) =
    startup_ready_components ws
  in
  let index_reconcile_pending = not index_reconcile_clear in
  let pending_open_docs = startup_open_docs_pending_count ws in
  let pending_open_diag_revalidate =
    Queue.length ws.open_diag_revalidate_updates
  in
  let ready_diag_ms =
    match ws.startup_diag_hover_ready_ms with
    | Some ready -> max 0 (int_of_float (ready -. ws.startup_started_ms))
    | None -> startup_elapsed_ms ws
  in
  let ready_nav_ms =
    match ws.startup_fully_nav_ready_ms with
    | Some ready -> max 0 (int_of_float (ready -. ws.startup_started_ms))
    | None -> startup_elapsed_ms ws
  in
  let elapsed_ms =
    match ws.startup_fully_nav_ready_ms with
    | Some ready -> max 0 (int_of_float (ready -. ws.startup_started_ms))
    | None -> startup_elapsed_ms ws
  in
  `Assoc
    [
      ("startedMs", `Float ws.startup_started_ms);
      ( "readyMs",
        match ws.startup_ready_ms with None -> `Null | Some ts -> `Float ts );
      ("elapsedMs", `Int elapsed_ms);
      ("targetMs", `Int ws.startup_nav_target_ms);
      ("readyWithinTarget", `Bool (elapsed_ms <= ws.startup_nav_target_ms));
      ("isReady", `Bool (ws.startup_ready_ms <> None));
      ("phase", `String (startup_phase_to_string ws.startup_phase));
      ("aggressiveWindowMs", `Int ws.startup_aggressive_window_ms);
      ("aggressiveBudgetMs", `Int ws.startup_aggressive_bg_budget_ms);
      ("missNotified", `Bool ws.startup_nav_miss_notified);
      ( "stages",
        `Assoc
          [
            ( "diagHoverReady",
              `Assoc
                [
                  ("targetMs", `Int ws.startup_diag_hover_target_ms);
                  ("elapsedMs", `Int ready_diag_ms);
                  ("isReady", `Bool (ws.startup_diag_hover_ready_ms <> None));
                  ( "readyWithinTarget",
                    `Bool (ready_diag_ms <= ws.startup_diag_hover_target_ms) );
                ] );
            ( "fullyNavigable",
              `Assoc
                [
                  ("targetMs", `Int ws.startup_nav_target_ms);
                  ("elapsedMs", `Int ready_nav_ms);
                  ("isReady", `Bool (ws.startup_fully_nav_ready_ms <> None));
                  ( "readyWithinTarget",
                    `Bool (ready_nav_ms <= ws.startup_nav_target_ms) );
                ] );
          ] );
      ( "components",
        `Assoc
          [
            ("indexComplete", `Bool index_complete);
            ("indexReconcilePending", `Bool index_reconcile_pending);
            ("seedComplete", `Bool seed_complete);
            ("highQueuesEmpty", `Bool high_queues_empty);
            ("queuesEmpty", `Bool queues_empty);
            ("hintsReady", `Bool hints_ready);
            ("navPrereqsReady", `Bool nav_prereqs_ready);
            ("quickNavIndexReady", `Bool quick_nav_ready);
            ("quickNavIndexComplete", `Bool (quick_nav_index_complete ws));
            ("quickNavIndexed", `Int ws.quick_nav_index_done);
            ("quickNavTotal", `Int ws.quick_nav_index_total);
            ("quickNavPending", `Int (Queue.length ws.quick_nav_pending_paths));
            ( "quickNavPendingSet",
              `Int (Hashtbl.length ws.quick_nav_pending_set) );
            ("parseWorkerJobs", `Int (Queue.length ws.parse_worker_jobs));
            ("parseWorkerResults", `Int (Queue.length ws.parse_worker_results));
            ( "parseWorkerInflight",
              `Int (Hashtbl.length ws.parse_worker_inflight) );
            ("openDocsConverged", `Bool open_docs_converged);
            ("openDocsAuthoritative", `Bool open_docs_authoritative);
            ("openDocsPendingParse", `Int pending_open_docs);
            ("xmoduleDiagReady", `Bool xmodule_ready);
            ("openDiagRevalidateQueueEmpty", `Bool open_diag_revalidate_empty);
            ("openDiagRevalidatePending", `Int pending_open_diag_revalidate);
          ] );
    ]

let consume_workspace_ready_event_json (ws : t) : Yojson.Safe.t option =
  update_startup_ready_state ws;
  let root_uri =
    match ws.root_path with
    | None -> None
    | Some p -> Some (`String (Uri_path.file_uri_of_path p))
  in
  match root_uri with
  | None -> None
  | Some root_uri_json ->
      if
        ws.startup_diag_hover_ready_ms <> None
        && not ws.startup_diag_hover_notified
      then (
        ws.startup_diag_hover_notified <- true;
        Some
          (`Assoc
             [
               ("rootUri", root_uri_json);
               ("stage", `String "diagHoverReady");
               ("readiness", startup_readiness_json ws);
             ]))
      else if
        ws.startup_fully_nav_ready_ms <> None
        && not ws.startup_fully_nav_notified
      then (
        ws.startup_fully_nav_notified <- true;
        ws.startup_ready_notified <- true;
        Some
          (`Assoc
             [
               ("rootUri", root_uri_json);
               ("stage", `String "fullyNavigable");
               ("readiness", startup_readiness_json ws);
             ]))
      else None

let consume_startup_phase_event_json (ws : t) : Yojson.Safe.t option =
  update_startup_ready_state ws;
  match (ws.startup_phase, ws.startup_phase_notified) with
  | StartupReady, _ -> None
  | phase, Some already when phase = already -> None
  | phase, _ ->
      ws.startup_phase_notified <- Some phase;
      let root_uri_json =
        match ws.root_path with
        | None -> `Null
        | Some p -> `String (Uri_path.file_uri_of_path p)
      in
      Some
        (`Assoc
           [
             ("rootUri", root_uri_json);
             ("phase", `String (startup_phase_to_string phase));
             ("elapsedMs", `Int (startup_elapsed_ms ws));
             ("targetMs", `Int ws.startup_nav_target_ms);
           ])

let consume_startup_miss_event_json (ws : t) : Yojson.Safe.t option =
  update_startup_ready_state ws;
  let root_uri =
    match ws.root_path with
    | None -> None
    | Some p -> Some (`String (Uri_path.file_uri_of_path p))
  in
  match root_uri with
  | None -> None
  | Some root_uri_json ->
      if
        ws.startup_diag_hover_miss_notified
        && (not ws.startup_diag_hover_miss_emitted)
        && ws.startup_diag_hover_ready_ms = None
      then (
        ws.startup_diag_hover_miss_emitted <- true;
        Some
          (`Assoc
             [
               ("rootUri", root_uri_json);
               ("stage", `String "diagHoverReady");
               ("phase", `String (startup_phase_to_string ws.startup_phase));
               ("elapsedMs", `Int (startup_elapsed_ms ws));
               ("targetMs", `Int ws.startup_diag_hover_target_ms);
             ]))
      else if
        ws.startup_nav_miss_notified
        && (not ws.startup_nav_miss_emitted)
        && ws.startup_fully_nav_ready_ms = None
      then (
        ws.startup_nav_miss_emitted <- true;
        ws.startup_miss_emitted <- true;
        Some
          (`Assoc
             [
               ("rootUri", root_uri_json);
               ("stage", `String "fullyNavigable");
               ("phase", `String (startup_phase_to_string ws.startup_phase));
               ("elapsedMs", `Int (startup_elapsed_ms ws));
               ("targetMs", `Int ws.startup_nav_target_ms);
             ]))
      else None

let startup_background_budget_ms (ws : t) ~(base_budget_ms : int) : int =
  update_startup_ready_state ws;
  let base = max 1 base_budget_ms in
  if ws.startup_fully_nav_ready_ms <> None then base
  else
    match ws.startup_phase with
    | StartupAggressiveCatchUp ->
        max base
          (max ws.startup_aggressive_bg_budget_ms ws.bg_high_large_budget_ms)
    | StartupCold | StartupWarming | StartupReady ->
        max base ws.bg_high_large_budget_ms

let workspace_source_bytes_estimate (ws : t) : int =
  match ws.index with
  | None -> 0
  | Some idx ->
      let bytes = Workspace_index.source_total_bytes idx in
      ws.source_bytes_estimate <- Some bytes;
      ws.source_bytes_estimate_count <- Workspace_index.source_count idx;
      max 0 bytes

let workspace_profile_for_budget (ws : t) : workspace_profile =
  match ws.workspace_profile_mode with
  | ProfileModeSmall -> ProfileSmall
  | ProfileModeMedium -> ProfileMedium
  | ProfileModeLarge -> ProfileLarge
  | ProfileModeAuto ->
      let bytes = workspace_source_bytes_estimate ws in
      if bytes >= profile_medium_max_bytes then ProfileLarge
      else if bytes >= profile_small_max_bytes then ProfileMedium
      else ProfileSmall

let effective_bg_tick_budget_ms (ws : t) ~(base_budget_ms : int) : int =
  let base = max 1 base_budget_ms in
  match workspace_profile_for_budget ws with
  | ProfileSmall -> base
  | ProfileMedium -> max base 16
  | ProfileLarge -> max base 24

let feature_flags (ws : t) : Workspace_settings.feature_flags = ws.feature_flags

let startup_priority_mode (ws : t) : Workspace_settings.startup_priority_mode =
  ws.startup_priority_mode

let startup_navigation_ready_now (ws : t) : bool =
  update_startup_ready_state ws;
  startup_navigation_prereqs_ready_now ws

let startup_diag_hover_ready_now (ws : t) : bool =
  update_startup_ready_state ws;
  ws.startup_diag_hover_ready_ms <> None

let startup_is_ready_now (ws : t) : bool =
  update_startup_ready_state ws;
  ws.startup_ready_ms <> None

let startup_readiness_json_for_report (ws : t) : Yojson.Safe.t =
  update_startup_ready_state ws;
  startup_readiness_json ws

let workspace_ready_event_json (ws : t) : Yojson.Safe.t option =
  consume_workspace_ready_event_json ws

let startup_phase_event_json (ws : t) : Yojson.Safe.t option =
  consume_startup_phase_event_json ws

let startup_miss_event_json (ws : t) : Yojson.Safe.t option =
  consume_startup_miss_event_json ws
