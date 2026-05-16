(* Module overview: Workspace configuration model for startup, diagnostics, feature flags, and limits. *)

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

type startup_priority_mode =
  | StartupPriorityBalanced
  | StartupPriorityInfoFirst

type feature_profile =
  | FeatureProfileFull
  | FeatureProfileResponsive
  | FeatureProfileMinimal
  | FeatureProfileCustom

type feature_flags = {
  diagnostics : bool;
  definition : bool;
  declaration : bool;
  type_definition : bool;
  implementation : bool;
  references : bool;
  document_symbols : bool;
  workspace_symbols : bool;
  hover : bool;
  signature_help : bool;
  rename : bool;
  completion : bool;
  code_actions : bool;
  code_lens : bool;
  inlay_hints : bool;
  formatting : bool;
  semantic_tokens : bool;
}

type feature_overrides = {
  diagnostics : bool option;
  definition : bool option;
  declaration : bool option;
  type_definition : bool option;
  implementation : bool option;
  references : bool option;
  document_symbols : bool option;
  workspace_symbols : bool option;
  hover : bool option;
  signature_help : bool option;
  rename : bool option;
  completion : bool option;
  code_actions : bool option;
  code_lens : bool option;
  inlay_hints : bool option;
  formatting : bool option;
  semantic_tokens : bool option;
}

type t = {
  sem_store_enabled : bool;
  lsif_delta_enabled : bool;
  workspace_diag_mode : workspace_diag_mode;
  feature_profile : feature_profile;
  feature_flags : feature_flags;
  closed_doc_lru_max : int;
  parse_file_max_bytes : int;
  large_file_threshold_bytes : int;
  huge_file_threshold_bytes : int;
  full_semantic_tokens_max_bytes : int;
  full_parse_max_bytes : int;
  enable_huge_file_full_parse : bool;
  bg_large_file_bytes : int;
  bg_large_parse_idle_quiet_ms : int;
  pressure_soft_mb : int;
  pressure_critical_mb : int;
  startup_target_ms : int;
  startup_diag_hover_target_ms : int;
  startup_nav_target_ms : int;
  startup_priority_mode : startup_priority_mode;
  startup_aggressive_window_ms : int;
  startup_aggressive_bg_budget_ms : int;
  parse_worker_count : int;
  parse_worker_max_inflight : int;
  bg_high_large_budget_ms : int;
  workspace_profile_mode : workspace_profile_mode;
  root_model : root_model;
  root_heuristic_fallback : bool;
  root_manual_files : string list;
  source_extensions : string list;
  graph_requeue_cooldown_ms : int;
  root_closure_max_depth : int;
  root_closure_target_files : int;
  skeleton_prefix_bytes : int;
  sched_open_doc_min_share_pct : int;
  allow_slow_query_fallback : bool;
  implementation_config : Implementation_config.t;
}

type client_overrides = {
  workspace_diag_mode : workspace_diag_mode option;
  workspace_profile_mode : workspace_profile_mode option;
  root_model : root_model option;
  root_manual_files : string list option;
  source_extensions : string list option;
  feature_profile : feature_profile option;
  parse_file_max_bytes : int option;
  large_file_threshold_bytes : int option;
  huge_file_threshold_bytes : int option;
  full_semantic_tokens_max_bytes : int option;
  full_parse_max_bytes : int option;
  enable_huge_file_full_parse : bool option;
  background_parse_worker_count : int option;
  feature_flags : feature_overrides;
  pressure_soft_mb : int option;
  pressure_critical_mb : int option;
  startup_priority_mode : startup_priority_mode option;
  implementation_config : Implementation_config.client_overrides;
}

let empty_feature_overrides : feature_overrides =
  {
    diagnostics = None;
    definition = None;
    declaration = None;
    type_definition = None;
    implementation = None;
    references = None;
    document_symbols = None;
    workspace_symbols = None;
    hover = None;
    signature_help = None;
    rename = None;
    completion = None;
    code_actions = None;
    code_lens = None;
    inlay_hints = None;
    formatting = None;
    semantic_tokens = None;
  }

let empty_client_overrides : client_overrides =
  {
    workspace_diag_mode = None;
    workspace_profile_mode = None;
    root_model = None;
    root_manual_files = None;
    source_extensions = None;
    feature_profile = None;
    parse_file_max_bytes = None;
    large_file_threshold_bytes = None;
    huge_file_threshold_bytes = None;
    full_semantic_tokens_max_bytes = None;
    full_parse_max_bytes = None;
    enable_huge_file_full_parse = None;
    background_parse_worker_count = None;
    feature_flags = empty_feature_overrides;
    pressure_soft_mb = None;
    pressure_critical_mb = None;
    startup_priority_mode = None;
    implementation_config = Implementation_config.empty_client_overrides;
  }

let pressure_soft_mb_default = 512
let pressure_critical_mb_default = 768
let startup_target_ms_default = 1500
let startup_diag_hover_target_ms_default = 1500
let startup_nav_target_ms_default = 1500
let startup_aggressive_window_ms_default = 500
let startup_aggressive_bg_budget_ms_default = 20
let profile_small_max_bytes = 10 * 1024 * 1024
let profile_medium_max_bytes = 40 * 1024 * 1024
let large_file_threshold_bytes_default = 128 * 1024
let huge_file_threshold_bytes_default = 15 * 1024 * 1024
let full_semantic_tokens_max_bytes_default = 1024 * 1024
let full_parse_max_bytes_default = 15 * 1024 * 1024

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

let startup_priority_mode_of_string (raw : string) :
    startup_priority_mode option =
  match String.lowercase_ascii (String.trim raw) with
  | "balanced" -> Some StartupPriorityBalanced
  | "navigationfirst" | "navigation_first" | "navigation-first" | "infofirst"
  | "info_first" | "info-first" | "" ->
      Some StartupPriorityInfoFirst
  | _ -> None

let feature_profile_of_string (raw : string) : feature_profile option =
  match String.lowercase_ascii (String.trim raw) with
  | "full" | "" -> Some FeatureProfileFull
  | "responsive" -> Some FeatureProfileResponsive
  | "minimal" -> Some FeatureProfileMinimal
  | "custom" -> Some FeatureProfileCustom
  | _ -> None

let feature_flags_from_env () : feature_flags =
  {
    diagnostics = Env_utils.flag "JOVIAL_FEATURE_DIAGNOSTICS" ~default:true;
    definition = true;
    declaration = true;
    type_definition = true;
    implementation = true;
    references = true;
    document_symbols =
      Env_utils.flag "JOVIAL_FEATURE_DOCUMENT_SYMBOLS" ~default:true;
    workspace_symbols =
      Env_utils.flag "JOVIAL_FEATURE_WORKSPACE_SYMBOLS" ~default:true;
    hover = Env_utils.flag "JOVIAL_FEATURE_HOVER" ~default:true;
    signature_help = Env_utils.flag "JOVIAL_FEATURE_SIGNATURE_HELP" ~default:true;
    rename = true;
    completion = Env_utils.flag "JOVIAL_FEATURE_COMPLETION" ~default:true;
    code_actions = Env_utils.flag "JOVIAL_FEATURE_CODE_ACTIONS" ~default:true;
    code_lens = Env_utils.flag "JOVIAL_FEATURE_CODE_LENS" ~default:true;
    inlay_hints = Env_utils.flag "JOVIAL_FEATURE_INLAY_HINTS" ~default:true;
    formatting = Env_utils.flag "JOVIAL_FEATURE_FORMATTING" ~default:true;
    semantic_tokens =
      Env_utils.flag "JOVIAL_FEATURE_SEMANTIC_TOKENS" ~default:true;
  }

let apply_feature_overrides (base : feature_flags) (overrides : feature_overrides)
    : feature_flags =
  {
    diagnostics =
      (match overrides.diagnostics with
      |Some enabled -> enabled
      |None -> base.diagnostics);
    definition = base.definition;
    declaration = base.declaration;
    type_definition = base.type_definition;
    implementation = base.implementation;
    references = base.references;
    document_symbols =
      (match overrides.document_symbols with
      | Some enabled -> enabled
      | None -> base.document_symbols);
    workspace_symbols =
      (match overrides.workspace_symbols with
      | Some enabled -> enabled
      | None -> base.workspace_symbols);
    hover =
      (match overrides.hover with
      | Some enabled -> enabled
      | None -> base.hover);
    signature_help =
      (match overrides.signature_help with
      | Some enabled -> enabled
      | None -> base.signature_help);
    rename = base.rename;
    completion =
      (match overrides.completion with
      | Some enabled -> enabled
      | None -> base.completion);
    code_actions =
      (match overrides.code_actions with
      | Some enabled -> enabled
      | None -> base.code_actions);
    code_lens =
      (match overrides.code_lens with
      | Some enabled -> enabled
      | None -> base.code_lens);
    inlay_hints =
      (match overrides.inlay_hints with
      | Some enabled -> enabled
      | None -> base.inlay_hints);
    formatting =
      (match overrides.formatting with
      | Some enabled -> enabled
      | None -> base.formatting);
    semantic_tokens =
      (match overrides.semantic_tokens with
      | Some enabled -> enabled
      | None -> base.semantic_tokens);
  }

  let apply_feature_profile (base : feature_flags) (profile : feature_profile) :
    feature_flags =
  match profile with
  | FeatureProfileFull -> base
  | FeatureProfileCustom -> base
  | FeatureProfileResponsive ->
      {
        base with
        code_actions = false;
        code_lens = false;
        inlay_hints = false;
        semantic_tokens = false;
      }
  | FeatureProfileMinimal ->
      {
        base with
        document_symbols = false;
        workspace_symbols = false;
        signature_help = false;
        completion = false;
        code_actions = false;
        code_lens = false;
        inlay_hints = false;
        formatting = false;
        semantic_tokens = false;
      }


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
  let startup_priority_mode =
    match Env_utils.nonempty_string "JOVIAL_STARTUP_PRIORITY_MODE" with
    | None -> StartupPriorityInfoFirst
    | Some raw -> (
        match startup_priority_mode_of_string raw with
        | Some mode -> mode
        | None -> StartupPriorityInfoFirst)
  in
  let feature_profile =
    match Env_utils.nonempty_string "JOVIAL_FEATURE_PROFILE" with
    | None -> FeatureProfileFull
    | Some raw -> (
        match feature_profile_of_string raw with
        | Some profile -> profile
        | None -> FeatureProfileFull)
  in
  let source_extensions =
    let extra =
      match Env_utils.nonempty_string "JOVIAL_SOURCE_EXTENSIONS" with
      | None -> []
      | Some raw -> parse_manual_root_files raw
    in
    Source_file.with_defaults extra
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
  let parse_worker_count =
    max 1 (Env_utils.nonneg_int "JOVIAL_BG_PARSE_WORKER_COUNT" ~default:2)
  in
  let parse_worker_max_inflight =
    max 1
      (Env_utils.nonneg_int "JOVIAL_BG_PARSE_WORKER_MAX_INFLIGHT"
         ~default:parse_worker_count)
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
  let large_file_threshold_bytes =
    max 1
      (Env_utils.nonneg_int "JOVIAL_LARGE_FILE_THRESHOLD_BYTES"
         ~default:large_file_threshold_bytes_default)
  in
  let huge_file_threshold_bytes =
    max large_file_threshold_bytes
      (Env_utils.nonneg_int "JOVIAL_HUGE_FILE_THRESHOLD_BYTES"
         ~default:huge_file_threshold_bytes_default)
  in
  let full_semantic_tokens_max_bytes =
    max 1
      (Env_utils.nonneg_int "JOVIAL_FULL_SEMANTIC_TOKENS_MAX_BYTES"
         ~default:full_semantic_tokens_max_bytes_default)
  in
  let full_parse_max_bytes =
    max 1
      (Env_utils.nonneg_int "JOVIAL_FULL_PARSE_MAX_BYTES"
         ~default:
           (Env_utils.nonneg_int "JOVIAL_PARSE_FILE_MAX_BYTES"
              ~default:full_parse_max_bytes_default))
  in
  let enable_huge_file_full_parse =
    Env_utils.flag "JOVIAL_ENABLE_HUGE_FILE_FULL_PARSE" ~default:false
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
    feature_profile;
    feature_flags = apply_feature_profile (feature_flags_from_env ()) feature_profile;
    closed_doc_lru_max =
      max 1 (Env_utils.nonneg_int "JOVIAL_CLOSED_DOC_LRU_MAX" ~default:256);
    parse_file_max_bytes =
      if enable_huge_file_full_parse then full_parse_max_bytes
      else min full_parse_max_bytes (max 1 huge_file_threshold_bytes);
    large_file_threshold_bytes;
    huge_file_threshold_bytes;
    full_semantic_tokens_max_bytes;
    full_parse_max_bytes;
    enable_huge_file_full_parse;
    bg_large_file_bytes =
      max 1
        (Env_utils.nonneg_int "JOVIAL_BG_LARGE_FILE_BYTES"
           ~default:large_file_threshold_bytes);
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
    startup_priority_mode;
    startup_aggressive_window_ms;
    startup_aggressive_bg_budget_ms;
    parse_worker_count;
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
    source_extensions;
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
    allow_slow_query_fallback =
      Env_utils.flag "JOVIAL_ALLOW_SLOW_QUERY_FALLBACK" ~default:false;
    implementation_config = Implementation_config.from_env ();
  }

let apply_client_overrides (settings : t) (overrides : client_overrides) : t =
  let feature_profile =
    match overrides.feature_profile with
    | Some feature_profile -> feature_profile
    | None -> settings.feature_profile
  in
  let feature_flags =
    let flags =
      match overrides.feature_profile with
      | Some profile -> apply_feature_profile settings.feature_flags profile
      | None -> settings.feature_flags
    in
    apply_feature_overrides flags overrides.feature_flags
  in
  let large_file_threshold_bytes =
    match overrides.large_file_threshold_bytes with
    | Some n -> max 1 n
    | None -> settings.large_file_threshold_bytes
  in
  let huge_file_threshold_bytes =
    match overrides.huge_file_threshold_bytes with
    | Some n -> max large_file_threshold_bytes n
    | None -> max large_file_threshold_bytes settings.huge_file_threshold_bytes
  in
  let full_semantic_tokens_max_bytes =
    match overrides.full_semantic_tokens_max_bytes with
    | Some n -> max 1 n
    | None -> settings.full_semantic_tokens_max_bytes
  in
  let full_parse_max_bytes =
    match (overrides.full_parse_max_bytes, overrides.parse_file_max_bytes) with
    | Some n, _ -> max 1 n
    | None, Some n -> max 1 n
    | None, None -> settings.full_parse_max_bytes
  in
  let enable_huge_file_full_parse =
    match overrides.enable_huge_file_full_parse with
    | Some enabled -> enabled
    | None -> settings.enable_huge_file_full_parse
  in
  let parse_file_max_bytes =
    if enable_huge_file_full_parse then full_parse_max_bytes
    else min full_parse_max_bytes (max 1 huge_file_threshold_bytes)
  in
  {
    settings with
    feature_profile;
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
    source_extensions =
      (match overrides.source_extensions with
      | None -> settings.source_extensions
      | Some source_extensions -> Source_file.with_defaults source_extensions);
    feature_flags;
    parse_file_max_bytes;
    large_file_threshold_bytes;
    huge_file_threshold_bytes;
    full_semantic_tokens_max_bytes;
    full_parse_max_bytes;
    enable_huge_file_full_parse;
    bg_large_file_bytes =
      (match overrides.large_file_threshold_bytes with
      | Some _ -> large_file_threshold_bytes
      | None -> settings.bg_large_file_bytes);
    parse_worker_count =
      (match overrides.background_parse_worker_count with
      | Some n -> max 1 n
      | None -> settings.parse_worker_count);
    parse_worker_max_inflight =
      (match overrides.background_parse_worker_count with
      | Some n -> max 1 n
      | None -> settings.parse_worker_max_inflight);
    pressure_soft_mb =
      (match overrides.pressure_soft_mb with
      | Some n -> max 64 n
      | None -> settings.pressure_soft_mb);
    pressure_critical_mb =
      (match overrides.pressure_critical_mb with
      | Some n -> max 64 n
      | None -> settings.pressure_critical_mb);
    startup_priority_mode =
      (match overrides.startup_priority_mode with
      | Some mode -> mode
      | None -> settings.startup_priority_mode);
    implementation_config =
      Implementation_config.apply_client_overrides settings.implementation_config
        overrides.implementation_config;
  }
