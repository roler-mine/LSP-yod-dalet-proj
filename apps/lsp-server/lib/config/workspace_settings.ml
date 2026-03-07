type workspace_diag_mode =
  | WorkspaceDiagsOff
  | WorkspaceDiagsErrors
  | WorkspaceDiagsAll

type workspace_profile_mode =
  | ProfileModeAuto
  | ProfileModeSmall
  | ProfileModeMedium
  | ProfileModeLarge

type root_model = RootModelAuto | RootModelHeuristic | RootModelManual

type t = {
  sem_store_enabled : bool;
  lsif_delta_enabled : bool;
  workspace_diag_mode : workspace_diag_mode;
  closed_doc_lru_max : int;
  parse_file_max_bytes : int;
  bg_large_file_bytes : int;
  bg_large_parse_idle_quiet_ms : int;
  pressure_soft_mb : int;
  pressure_critical_mb : int;
  startup_target_ms : int;
  startup_diag_hover_target_ms : int;
  startup_nav_target_ms : int;
  startup_aggressive_window_ms : int;
  startup_aggressive_bg_budget_ms : int;
  parse_worker_max_inflight : int;
  bg_high_large_budget_ms : int;
  workspace_profile_mode : workspace_profile_mode;
  root_model : root_model;
  root_heuristic_fallback : bool;
  root_manual_files : string list;
  graph_requeue_cooldown_ms : int;
  root_closure_max_depth : int;
  root_closure_target_files : int;
  skeleton_prefix_bytes : int;
  sched_open_doc_min_share_pct : int;
}

type client_overrides = {
  workspace_diag_mode : workspace_diag_mode option;
  workspace_profile_mode : workspace_profile_mode option;
  root_model : root_model option;
  root_manual_files : string list option;
  parse_file_max_bytes : int option;
  pressure_soft_mb : int option;
  pressure_critical_mb : int option;
}

let empty_client_overrides : client_overrides =
  {
    workspace_diag_mode = None;
    workspace_profile_mode = None;
    root_model = None;
    root_manual_files = None;
    parse_file_max_bytes = None;
    pressure_soft_mb = None;
    pressure_critical_mb = None;
  }

let pressure_soft_mb_default = 512
let pressure_critical_mb_default = 768
let startup_target_ms_default = 15000
let startup_diag_hover_target_ms_default = 15000
let startup_nav_target_ms_default = 30000
let startup_aggressive_window_ms_default = 3000
let startup_aggressive_bg_budget_ms_default = 20
let profile_small_max_bytes = 10 * 1024 * 1024
let profile_medium_max_bytes = 40 * 1024 * 1024

let workspace_diag_mode_of_string (raw : string) : workspace_diag_mode option =
  match String.lowercase_ascii (String.trim raw) with
  | "off" -> Some WorkspaceDiagsOff
  | "all" -> Some WorkspaceDiagsAll
  | "errors" | "" -> Some WorkspaceDiagsErrors
  | _ -> None

let workspace_profile_mode_of_string (raw : string) :
    workspace_profile_mode option =
  match String.lowercase_ascii (String.trim raw) with
  | "small" -> Some ProfileModeSmall
  | "medium" -> Some ProfileModeMedium
  | "large" -> Some ProfileModeLarge
  | "auto" | "" -> Some ProfileModeAuto
  | _ -> None

let root_model_of_string (raw : string) : root_model option =
  match String.lowercase_ascii (String.trim raw) with
  | "heuristic" -> Some RootModelHeuristic
  | "manual" -> Some RootModelManual
  | "auto" | "" -> Some RootModelAuto
  | _ -> None

let parse_manual_root_files (raw : string) : string list =
  raw |> String.split_on_char ';'
  |> List.concat_map (fun s -> String.split_on_char ',' s)
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")

let from_env () : t =
  let startup_target_ms =
    max 1000
      (Env_utils.nonneg_int "JOVIAL_STARTUP_TARGET_MS"
         ~default:startup_target_ms_default)
  in
  let startup_diag_hover_target_ms =
    max 1000
      (Env_utils.nonneg_int "JOVIAL_STARTUP_DIAG_HOVER_TARGET_MS"
         ~default:startup_diag_hover_target_ms_default)
  in
  let startup_nav_target_ms =
    max startup_diag_hover_target_ms
      (Env_utils.nonneg_int "JOVIAL_STARTUP_NAV_TARGET_MS"
         ~default:startup_nav_target_ms_default)
  in
  let startup_aggressive_window_ms =
    max 250
      (Env_utils.nonneg_int "JOVIAL_STARTUP_AGGRESSIVE_WINDOW_MS"
         ~default:startup_aggressive_window_ms_default)
  in
  let startup_aggressive_bg_budget_ms =
    max 1
      (Env_utils.nonneg_int "JOVIAL_STARTUP_AGGRESSIVE_BG_BUDGET_MS"
         ~default:startup_aggressive_bg_budget_ms_default)
  in
  let parse_worker_max_inflight =
    max 1
      (Env_utils.nonneg_int "JOVIAL_BG_PARSE_WORKER_MAX_INFLIGHT" ~default:1)
  in
  let bg_high_large_budget_ms =
    max 1 (Env_utils.nonneg_int "JOVIAL_BG_HIGH_LARGE_BUDGET_MS" ~default:8)
  in
  let root_manual_files =
    match Env_utils.nonempty_string "JOVIAL_ROOT_MANUAL_FILES" with
    | None -> []
    | Some raw -> parse_manual_root_files raw
  in
  let sched_open_doc_min_share_pct =
    let n =
      Env_utils.nonneg_int "JOVIAL_SCHED_OPEN_DOC_MIN_SHARE_PCT" ~default:50
    in
    min 100 (max 0 n)
  in
  {
    sem_store_enabled = Env_utils.flag "JOVIAL_SEM_STORE" ~default:true;
    lsif_delta_enabled = Env_utils.flag "JOVIAL_LSIF_DELTA" ~default:true;
    workspace_diag_mode =
      (match Env_utils.nonempty_string "JOVIAL_WORKSPACE_DIAGS_MODE" with
      | None -> WorkspaceDiagsErrors
      | Some raw -> (
          match workspace_diag_mode_of_string raw with
          | Some mode -> mode
          | None -> WorkspaceDiagsErrors));
    closed_doc_lru_max =
      max 1 (Env_utils.nonneg_int "JOVIAL_CLOSED_DOC_LRU_MAX" ~default:256);
    parse_file_max_bytes =
      max 1
        (Env_utils.nonneg_int "JOVIAL_PARSE_FILE_MAX_BYTES" ~default:16777216);
    bg_large_file_bytes =
      max 1 (Env_utils.nonneg_int "JOVIAL_BG_LARGE_FILE_BYTES" ~default:800000);
    bg_large_parse_idle_quiet_ms =
      max 0
        (Env_utils.nonneg_int "JOVIAL_BG_LARGE_PARSE_IDLE_QUIET_MS" ~default:150);
    pressure_soft_mb =
      max 64
        (Env_utils.nonneg_int "JOVIAL_PRESSURE_SOFT_MB"
           ~default:pressure_soft_mb_default);
    pressure_critical_mb =
      max 64
        (Env_utils.nonneg_int "JOVIAL_PRESSURE_CRITICAL_MB"
           ~default:pressure_critical_mb_default);
    startup_target_ms;
    startup_diag_hover_target_ms;
    startup_nav_target_ms;
    startup_aggressive_window_ms;
    startup_aggressive_bg_budget_ms;
    parse_worker_max_inflight;
    bg_high_large_budget_ms;
    workspace_profile_mode =
      (match Env_utils.nonempty_string "JOVIAL_WORKSPACE_PROFILE_MODE" with
      | None -> ProfileModeAuto
      | Some raw -> (
          match workspace_profile_mode_of_string raw with
          | Some mode -> mode
          | None -> ProfileModeAuto));
    root_model =
      (match Env_utils.nonempty_string "JOVIAL_ROOT_MODEL" with
      | None -> RootModelAuto
      | Some raw -> (
          match root_model_of_string raw with
          | Some model -> model
          | None -> RootModelAuto));
    root_heuristic_fallback =
      Env_utils.flag "JOVIAL_ROOT_HEURISTIC_FALLBACK" ~default:true;
    root_manual_files;
    graph_requeue_cooldown_ms =
      max 0
        (Env_utils.nonneg_int "JOVIAL_GRAPH_REQUEUE_COOLDOWN_MS" ~default:400);
    root_closure_max_depth =
      max 1 (Env_utils.nonneg_int "JOVIAL_ROOT_CLOSURE_MAX_DEPTH" ~default:4);
    root_closure_target_files =
      max 8
        (Env_utils.nonneg_int "JOVIAL_ROOT_CLOSURE_TARGET_FILES" ~default:256);
    skeleton_prefix_bytes =
      max 1024
        (Env_utils.nonneg_int "JOVIAL_SKELETON_PREFIX_BYTES" ~default:262144);
    sched_open_doc_min_share_pct;
  }

let apply_client_overrides (settings : t) (overrides : client_overrides) : t =
  {
    settings with
    workspace_diag_mode =
      (match overrides.workspace_diag_mode with
      | None -> settings.workspace_diag_mode
      | Some workspace_diag_mode -> workspace_diag_mode);
    workspace_profile_mode =
      (match overrides.workspace_profile_mode with
      | None -> settings.workspace_profile_mode
      | Some workspace_profile_mode -> workspace_profile_mode);
    root_model =
      (match overrides.root_model with
      | None -> settings.root_model
      | Some root_model -> root_model);
    root_manual_files =
      (match overrides.root_manual_files with
      | None -> settings.root_manual_files
      | Some root_manual_files -> root_manual_files);
    parse_file_max_bytes =
      (match overrides.parse_file_max_bytes with
      | Some n -> max 1 n
      | None -> settings.parse_file_max_bytes);
    pressure_soft_mb =
      (match overrides.pressure_soft_mb with
      | Some n -> max 64 n
      | None -> settings.pressure_soft_mb);
    pressure_critical_mb =
      (match overrides.pressure_critical_mb with
      | Some n -> max 64 n
      | None -> settings.pressure_critical_mb);
  }
