type parse_policy = Perf_log.parse_policy =
  | Forbid_sync_parse
  | Allow_sync_parse_if_small
  | Force_background

type request_kind = Perf_log.request_kind =
  | Hover
  | Completion
  | Definition
  | References
  | DocumentSymbol
  | SemanticTokensRange
  | SemanticTokensFull
  | Diagnostics
  | BackgroundIndex

let request_allows_sync_parse = Perf_log.request_allows_sync_parse
let has_current_parse = Document.has_current_parse

type file_mode = Small | Normal | Large | Huge

let file_mode_of_size (ws : Workspace_foundation.t) ~(bytes : int) : file_mode =
  if bytes >= ws.Workspace_foundation.huge_file_threshold_bytes then Huge
  else if bytes >= ws.Workspace_foundation.full_semantic_tokens_max_bytes then
    Large
  else if bytes >= ws.Workspace_foundation.large_file_threshold_bytes then
    Normal
  else Small

let file_mode_of_doc (doc : Document.t) (ws : Workspace_foundation.t) :
    file_mode =
  file_mode_of_size ws ~bytes:(String.length doc.Document.text)

let is_large_doc (doc : Document.t) (ws : Workspace_foundation.t) : bool =
  match file_mode_of_doc doc ws with Large | Huge -> true | Small | Normal -> false

let is_huge_doc (doc : Document.t) (ws : Workspace_foundation.t) : bool =
  match file_mode_of_doc doc ws with Huge -> true | Small | Normal | Large -> false

let full_parse_allowed_for_size (ws : Workspace_foundation.t) ~(bytes : int) :
    bool =
  bytes <= ws.Workspace_foundation.full_parse_max_bytes
  &&
  (ws.Workspace_foundation.enable_huge_file_full_parse
  || bytes < ws.Workspace_foundation.huge_file_threshold_bytes)

let index_bootstrap_dirs = 64
let index_bootstrap_files = 6000
let index_background_dirs = 4
let index_background_files = 200
let index_lookup_dirs = 12
let index_lookup_files = 600
let index_bootstrap_dirs_network = 2
let index_bootstrap_files_network = 200
let index_background_dirs_network = 1
let index_background_files_network = 64
let index_lookup_dirs_network = 1
let index_lookup_files_network = 64

let index_startup_disable =
  Env_utils.flag "JOVIAL_INDEX_STARTUP_DISABLE" ~default:false

let index_startup_dirs =
  Env_utils.nonneg_int "JOVIAL_INDEX_STARTUP_DIRS" ~default:0

let index_startup_files =
  Env_utils.nonneg_int "JOVIAL_INDEX_STARTUP_FILES" ~default:0

let index_startup_dirs_network =
  Env_utils.nonneg_int "JOVIAL_INDEX_STARTUP_DIRS_NETWORK" ~default:0

let index_startup_files_network =
  Env_utils.nonneg_int "JOVIAL_INDEX_STARTUP_FILES_NETWORK" ~default:0

let index_stale_reconcile_min_interval_ms =
  max 100
    (Env_utils.nonneg_int "JOVIAL_INDEX_STALE_RECONCILE_MIN_INTERVAL_MS"
       ~default:900)

let index_reconcile_escalate_dirs =
  max 1
    (Env_utils.nonneg_int "JOVIAL_INDEX_RECONCILE_ESCALATE_DIRS" ~default:24)

let index_reconcile_escalate_files =
  max 1
    (Env_utils.nonneg_int "JOVIAL_INDEX_RECONCILE_ESCALATE_FILES" ~default:3000)

let didchange_semi_check_enabled =
  Env_utils.flag "JOVIAL_DIDCHANGE_SEMI_CHECK" ~default:true

let didchange_semi_check_max_changes =
  Env_utils.nonneg_int "JOVIAL_DIDCHANGE_SEMI_MAX_CHANGES" ~default:6

let didchange_semi_check_max_lines =
  Env_utils.nonneg_int "JOVIAL_DIDCHANGE_SEMI_MAX_LINES" ~default:30

let didchange_semi_check_max_text_chars =
  Env_utils.nonneg_int "JOVIAL_DIDCHANGE_SEMI_MAX_TEXT_CHARS" ~default:1200

let didchange_semi_force_full_every =
  max 1
    (Env_utils.nonneg_int "JOVIAL_DIDCHANGE_SEMI_FORCE_FULL_EVERY" ~default:20)

let didchange_defer_parse_enabled =
  Env_utils.flag "JOVIAL_DIDCHANGE_DEFER_PARSE" ~default:true

let didchange_defer_parse_min_doc_chars =
  Env_utils.nonneg_int "JOVIAL_DIDCHANGE_DEFER_MIN_DOC_CHARS" ~default:120000

let didchange_defer_parse_max_changes =
  Env_utils.nonneg_int "JOVIAL_DIDCHANGE_DEFER_MAX_CHANGES" ~default:8

let didchange_defer_parse_max_inserted_chars =
  Env_utils.nonneg_int "JOVIAL_DIDCHANGE_DEFER_MAX_INSERTED_CHARS" ~default:1800

let didchange_defer_parse_force_full_every =
  max 1
    (Env_utils.nonneg_int "JOVIAL_DIDCHANGE_DEFER_FORCE_FULL_EVERY" ~default:24)

let didopen_defer_parse_enabled =
  Env_utils.flag "JOVIAL_DIDOPEN_DEFER_PARSE" ~default:true

let didopen_defer_parse_min_doc_chars =
  Env_utils.nonneg_int "JOVIAL_DIDOPEN_DEFER_MIN_DOC_CHARS" ~default:120000

let didopen_always_provisional =
  Env_utils.flag "JOVIAL_DIDOPEN_ALWAYS_PROVISIONAL" ~default:false

let didopen_disable_foreground_tick =
  Env_utils.flag "JOVIAL_DIDOPEN_DISABLE_FOREGROUND_TICK" ~default:true

let warmup_suppress_crossmodule_unresolved =
  Env_utils.flag "JOVIAL_DIAG_WARMUP_SUPPRESS_XMODULE" ~default:true

let bg_seed_paths_per_tick =
  max 1 (Env_utils.nonneg_int "JOVIAL_BG_SEED_PATHS_PER_TICK" ~default:128)

let nav_soft_budget_ms =
  max 1 (Env_utils.nonneg_int "JOVIAL_NAV_SOFT_BUDGET_MS" ~default:1800)

let nav_quick_scan_files =
  match Sys.getenv_opt "JOVIAL_NAV_QUICK_SCAN_FILES" with
  | Some _ -> Env_utils.nonneg_int "JOVIAL_NAV_QUICK_SCAN_FILES" ~default:48
  | None -> Env_utils.nonneg_int "JOVIAL_NAV_SOURCE_SCAN_FILES" ~default:48

let nav_quick_scan_total_bytes =
  Env_utils.nonneg_int "JOVIAL_NAV_QUICK_SCAN_TOTAL_BYTES"
    ~default:(12 * 1024 * 1024)

let nav_quick_scan_per_file_bytes =
  Env_utils.nonneg_int "JOVIAL_NAV_QUICK_SCAN_PER_FILE_BYTES" ~default:262144

let nav_miss_import_scan_max_chars =
  max 4096
    (Env_utils.nonneg_int "JOVIAL_NAV_MISS_IMPORT_SCAN_MAX_CHARS"
       ~default:262144)

let nav_miss_high_enqueue_cap =
  Env_utils.nonneg_int "JOVIAL_NAV_MISS_HIGH_ENQUEUE_CAP" ~default:24

let pressure_check_interval_ms =
  max 50 (Env_utils.nonneg_int "JOVIAL_PRESSURE_CHECK_INTERVAL_MS" ~default:250)

let lsif_doc_load_budget_normal =
  max 1 (Env_utils.nonneg_int "JOVIAL_LSIF_DOC_LOAD_BUDGET" ~default:400)

let lsif_doc_load_budget_soft =
  max 1 (Env_utils.nonneg_int "JOVIAL_LSIF_DOC_LOAD_BUDGET_SOFT" ~default:64)

let startup_scan_budget_for_root ~(network : bool) : int * int =
  if index_startup_disable then (0, 0)
  else
    let default_dirs, default_files, cfg_dirs, cfg_files =
      if network then
        ( index_bootstrap_dirs_network,
          index_bootstrap_files_network,
          index_startup_dirs_network,
          index_startup_files_network )
      else
        ( index_bootstrap_dirs,
          index_bootstrap_files,
          index_startup_dirs,
          index_startup_files )
    in
    let dirs = if cfg_dirs = 0 then default_dirs else cfg_dirs in
    let files = if cfg_files = 0 then default_files else cfg_files in
    (max 0 dirs, max 0 files)
