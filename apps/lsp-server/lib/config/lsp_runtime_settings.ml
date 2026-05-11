type t = {
  sem_refresh_every_didchange : int;
  bg_tick_budget_ms : int;
  bg_diag_batch_size : int;
  idle_sleep_ms : int;
  startup_fair_tick_ms : int;
  diag_min_fair_tick_ms : int;
  bg_large_parse_idle_quiet_ms : int;
  open_diag_revalidate_batch_size : int;
  watch_coalesce_ttl_ms : int;
  inbox_max_items : int;
}

type client_overrides = {
  workspace_diag_mode : Workspace_settings.workspace_diag_mode option;
  workspace_profile_mode : Workspace_settings.workspace_profile_mode option;
  root_model : Workspace_settings.root_model option;
  root_manual_files : string list option;
  source_extensions : string list option;
  feature_profile : Workspace_settings.feature_profile option;
  feature_flags : Workspace_settings.feature_overrides;
  parse_file_max_bytes : int option;
  large_file_threshold_bytes : int option;
  huge_file_threshold_bytes : int option;
  full_semantic_tokens_max_bytes : int option;
  full_parse_max_bytes : int option;
  enable_huge_file_full_parse : bool option;
  background_parse_worker_count : int option;
  pressure_soft_mb : int option;
  pressure_critical_mb : int option;
  startup_priority_mode : Workspace_settings.startup_priority_mode option;
  bg_tick_budget_ms : int option;
  bg_diag_batch_size : int option;
}

let empty_client_overrides : client_overrides =
  {
    workspace_diag_mode = None;
    workspace_profile_mode = None;
    root_model = None;
    root_manual_files = None;
    source_extensions = None;
    feature_profile = None;
    feature_flags = Workspace_settings.empty_feature_overrides;
    parse_file_max_bytes = None;
    large_file_threshold_bytes = None;
    huge_file_threshold_bytes = None;
    full_semantic_tokens_max_bytes = None;
    full_parse_max_bytes = None;
    enable_huge_file_full_parse = None;
    background_parse_worker_count = None;
    pressure_soft_mb = None;
    pressure_critical_mb = None;
    startup_priority_mode = None;
    bg_tick_budget_ms = None;
    bg_diag_batch_size = None;
  }

let from_env () : t =
  {
    sem_refresh_every_didchange =
      max 1
        (Env_utils.nonneg_int "JOVIAL_SEM_REFRESH_EVERY_DIDCHANGE" ~default:8);
    bg_tick_budget_ms =
      max 1 (Env_utils.nonneg_int "JOVIAL_BG_TICK_BUDGET_MS" ~default:8);
    bg_diag_batch_size =
      max 1 (Env_utils.nonneg_int "JOVIAL_BG_DIAG_BATCH_SIZE" ~default:64);
    idle_sleep_ms =
      max 1 (Env_utils.nonneg_int "JOVIAL_BG_IDLE_SLEEP_MS" ~default:20);
    startup_fair_tick_ms =
      max 0 (Env_utils.nonneg_int "JOVIAL_STARTUP_FAIR_TICK_MS" ~default:2);
    diag_min_fair_tick_ms =
      max 0 (Env_utils.nonneg_int "JOVIAL_DIAG_MIN_FAIR_TICK_MS" ~default:1);
    bg_large_parse_idle_quiet_ms =
      max 0
        (Env_utils.nonneg_int "JOVIAL_BG_LARGE_PARSE_IDLE_QUIET_MS" ~default:150);
    open_diag_revalidate_batch_size =
      max 1
        (Env_utils.nonneg_int "JOVIAL_OPEN_DIAG_REVALIDATE_BATCH_SIZE"
           ~default:8);
    watch_coalesce_ttl_ms =
      max 0 (Env_utils.nonneg_int "JOVIAL_WATCH_COALESCE_TTL_MS" ~default:250);
    inbox_max_items =
      max 16 (Env_utils.nonneg_int "JOVIAL_INBOX_MAX_ITEMS" ~default:2048);
  }

let apply_client_overrides (settings : t) (overrides : client_overrides) : t =
  {
    settings with
    bg_tick_budget_ms =
      (match overrides.bg_tick_budget_ms with
      | Some n -> max 1 n
      | None -> settings.bg_tick_budget_ms);
    bg_diag_batch_size =
      (match overrides.bg_diag_batch_size with
      | Some n -> max 1 n
      | None -> settings.bg_diag_batch_size);
  }
