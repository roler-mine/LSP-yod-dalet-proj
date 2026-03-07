module T = Lsp.Types

type workspace_diag_mode = Workspace_settings.workspace_diag_mode =
  | WorkspaceDiagsOff
  | WorkspaceDiagsErrors
  | WorkspaceDiagsAll

type pressure_mode = PressureNormal | PressureSoft | PressureCritical

type startup_phase =
  | StartupCold
  | StartupWarming
  | StartupAggressiveCatchUp
  | StartupReady

type startup_ready_stage =
  | StartupStageDiagHoverReady
  | StartupStageFullyNavigable

type bg_tick_mode = BgTickInteractive | BgTickIdle
type workspace_profile = ProfileSmall | ProfileMedium | ProfileLarge

type workspace_profile_mode = Workspace_settings.workspace_profile_mode =
  | ProfileModeAuto
  | ProfileModeSmall
  | ProfileModeMedium
  | ProfileModeLarge

type root_model = Workspace_settings.root_model =
  | RootModelAuto
  | RootModelHeuristic
  | RootModelManual

type bg_queue_kind =
  | BgQueueHighSmall
  | BgQueueNormalSmall
  | BgQueueRootSmall
  | BgQueueHighLarge
  | BgQueueRootLarge
  | BgQueueNormalLarge

type parse_job_kind =
  | ParseJobHighLarge
  | ParseJobRootLarge
  | ParseJobNormalLarge

type file_class = FileClassOpen | FileClassEntry | FileClassNormal
type file_size_class = FileSizeSmall | FileSizeLarge

type parse_quality =
  | ParseQualityNone
  | ParseQualitySkeleton
  | ParseQualityFull

type schedule_lane = LaneOpen | LaneRoot | LaneSweep

type graph_node = {
  gn_path : string;
  gn_path_key : string;
  mutable gn_import_compools : string list;
  mutable gn_import_paths : string list;
  mutable gn_rev_importers : string list;
  mutable gn_file_class : file_class;
  mutable gn_size_class : file_size_class;
  mutable gn_parse_quality : parse_quality;
  mutable gn_epoch : int;
}

type parse_job_payload =
  | ParseJobOpen of {
      path_key : string;
      uri : T.DocumentUri.t;
      file : string option;
      text : string;
      generation : int;
    }
  | ParseJobPath of { path : string; path_key : string }

type parse_job = {
  pj_kind : parse_job_kind;
  pj_epoch : int;
  pj_payload : parse_job_payload;
}

type parse_result =
  | ParseResultOpen of {
      pr_kind : parse_job_kind;
      pr_epoch : int;
      path_key : string;
      uri : T.DocumentUri.t;
      generation : int;
      doc : Document.t;
    }
  | ParseResultPath of {
      pr_kind : parse_job_kind;
      pr_epoch : int;
      path : string;
      path_key : string;
      doc_opt : Document.t option;
    }

type quick_nav_entry = {
  qn_uri : T.DocumentUri.t;
  qn_name : string;
  qn_key : string;
  qn_loc : Ast.Loc.t;
  qn_kind : int;
  qn_container : string option;
}

type t = {
  docs : (T.DocumentUri.t, Document.t) Hashtbl.t;
  files : (string, Document.t) Hashtbl.t;
  mutable root_path : string option;
  mutable index : Workspace_index.t option;
  mutable symbol_hints :
    ((string, string list) Hashtbl.t * (string, string list) Hashtbl.t) option;
  nav_response_cache : (string, Yojson.Safe.t) Hashtbl.t;
  semantic_store : Semantic_store.t;
  sem_store_enabled : bool;
  lsif_delta_enabled : bool;
  lsif_delta_state : Lsif_delta.t;
  mutable lsif_snapshot_revision : int;
  mutable lsif_snapshot_payload : Yojson.Safe.t option;
  mutable lsif_snapshot_symbols : (string, Yojson.Safe.t) Hashtbl.t option;
  workspace_diag_mode : workspace_diag_mode;
  bg_high_small_queue : string Queue.t;
  bg_norm_small_queue : string Queue.t;
  bg_root_small_queue : string Queue.t;
  bg_high_large_queue : string Queue.t;
  bg_root_large_queue : string Queue.t;
  bg_norm_large_queue : string Queue.t;
  bg_enqueued : (string, bg_queue_kind) Hashtbl.t;
  bg_enqueue_recent_ms : (string, float) Hashtbl.t;
  bg_parsed : (string, bool) Hashtbl.t;
  mutable bg_seed_paths : string array;
  mutable bg_seed_cursor : int;
  mutable graph_root_closure_paths : string array;
  mutable graph_root_closure_cursor : int;
  graph_nodes : (string, graph_node) Hashtbl.t;
  graph_root_reason : (string, string) Hashtbl.t;
  graph_root_closure_set : (string, bool) Hashtbl.t;
  mutable graph_needs_refresh : bool;
  mutable graph_epoch : int;
  mutable graph_scc_count : int;
  bg_closed_diags : (string, T.Diagnostic.t list) Hashtbl.t;
  bg_pending_diag_updates : string Queue.t;
  bg_pending_diag_payloads :
    (string, T.DocumentUri.t * T.Diagnostic.t list) Hashtbl.t;
  bg_pending_diag_set : (string, bool) Hashtbl.t;
  mutable bg_seed_needs_refresh : bool;
  mutable closed_doc_lru_clock : int;
  closed_doc_last_touch : (string, int) Hashtbl.t;
  closed_doc_lru_max : int;
  parse_file_max_bytes : int;
  bg_large_file_bytes : int;
  bg_large_parse_idle_quiet_ms : int;
  pressure_soft_mb : int;
  pressure_critical_mb : int;
  mutable pressure_mode : pressure_mode;
  mutable pressure_live_mb : int;
  mutable pressure_last_check_ms : float;
  mutable startup_started_ms : float;
  startup_diag_hover_target_ms : int;
  startup_nav_target_ms : int;
  mutable startup_diag_hover_ready_ms : float option;
  mutable startup_fully_nav_ready_ms : float option;
  mutable startup_diag_hover_notified : bool;
  mutable startup_fully_nav_notified : bool;
  mutable startup_diag_hover_miss_notified : bool;
  mutable startup_nav_miss_notified : bool;
  mutable startup_diag_hover_miss_emitted : bool;
  mutable startup_nav_miss_emitted : bool;
  mutable startup_ready_ms : float option;
  mutable startup_ready_notified : bool;
  mutable startup_miss_notified : bool;
  mutable startup_miss_emitted : bool;
  mutable startup_phase : startup_phase;
  mutable startup_phase_notified : startup_phase option;
  startup_target_ms : int;
  startup_aggressive_window_ms : int;
  startup_aggressive_bg_budget_ms : int;
  open_diag_revalidate_updates : string Queue.t;
  open_diag_revalidate_payloads : (string, T.DocumentUri.t * string) Hashtbl.t;
  open_diag_revalidate_set : (string, bool) Hashtbl.t;
  mutable index_reconcile_escalate_last_ms : float;
  mutable index_reconcile_escalations : int;
  open_parse_generation : (string, int) Hashtbl.t;
  open_provisional_since_ms : (string, float) Hashtbl.t;
  mutable xmodule_diag_ready_prev : bool;
  mutable source_bytes_estimate : int option;
  mutable source_bytes_estimate_count : int;
  workspace_profile_mode : workspace_profile_mode;
  root_model : root_model;
  root_heuristic_fallback : bool;
  root_manual_files : string list;
  graph_requeue_cooldown_ms : int;
  root_closure_max_depth : int;
  root_closure_target_files : int;
  skeleton_prefix_bytes : int;
  sched_open_doc_min_share_pct : int;
  quick_nav_index : (string, quick_nav_entry list) Hashtbl.t;
  quick_nav_pending_paths : string Queue.t;
  quick_nav_pending_set : (string, bool) Hashtbl.t;
  quick_nav_done_set : (string, bool) Hashtbl.t;
  nav_quick_scan_offset_by_path : (string, int) Hashtbl.t;
  mutable quick_nav_index_done : int;
  mutable quick_nav_index_total : int;
  parse_worker_jobs : parse_job Queue.t;
  parse_worker_results : parse_result Queue.t;
  parse_worker_mtx : Mutex.t;
  parse_worker_cv : Condition.t;
  parse_worker_inflight : (string, parse_job_kind) Hashtbl.t;
  parse_worker_max_inflight : int;
  mutable parse_worker_started : bool;
  mutable parse_worker_stop : bool;
  bg_high_large_budget_ms : int;
  mutable parse_epoch : int;
  mutable request_cancel_checker : (unit -> bool) option;
}
