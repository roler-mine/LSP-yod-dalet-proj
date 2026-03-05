module T = Lsp.Types
open Ast

type workspace_diag_mode =
  | WorkspaceDiagsOff
  | WorkspaceDiagsErrors
  | WorkspaceDiagsAll

type pressure_mode =
  | PressureNormal
  | PressureSoft
  | PressureCritical

type startup_phase =
  | StartupCold
  | StartupWarming
  | StartupAggressiveCatchUp
  | StartupReady

type startup_ready_stage =
  | StartupStageDiagHoverReady
  | StartupStageFullyNavigable

type bg_tick_mode =
  | BgTickInteractive
  | BgTickIdle

type workspace_profile =
  | ProfileSmall
  | ProfileMedium
  | ProfileLarge

type workspace_profile_mode =
  | ProfileModeAuto
  | ProfileModeSmall
  | ProfileModeMedium
  | ProfileModeLarge

type root_model =
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

type file_class =
  | FileClassOpen
  | FileClassEntry
  | FileClassNormal

type file_size_class =
  | FileSizeSmall
  | FileSizeLarge

type parse_quality =
  | ParseQualityNone
  | ParseQualitySkeleton
  | ParseQualityFull

type schedule_lane =
  | LaneOpen
  | LaneRoot
  | LaneSweep

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
  | ParseJobPath of {
      path : string;
      path_key : string;
    }

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
  files : (string, Document.t) Hashtbl.t;  (* normalized file path -> parsed document *)
  mutable root_path : string option;
  mutable index : Workspace_index.t option;
  mutable symbol_hints :
    ((string, string list) Hashtbl.t * (string, string list) Hashtbl.t) option;
  (* values map + types map: symbol key -> candidate compool names *)
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
  bg_pending_diag_payloads : (string, (T.DocumentUri.t * T.Diagnostic.t list)) Hashtbl.t;
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
  open_diag_revalidate_payloads : (string, (T.DocumentUri.t * string)) Hashtbl.t;
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

let env_flag (name:string) ~(default:bool) : bool =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw ->
      (match String.lowercase_ascii (String.trim raw) with
       | "" -> default
       | "1" | "true" | "yes" | "on" -> true
       | "0" | "false" | "no" | "off" -> false
       | _ -> default)

let env_nonneg_int (name:string) ~(default:int) : int =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw ->
      (try
         let n = int_of_string (String.trim raw) in
         if n < 0 then default else n
       with _ ->
         default)

let workspace_diag_mode_of_env () : workspace_diag_mode =
  match Sys.getenv_opt "JOVIAL_WORKSPACE_DIAGS_MODE" with
  | None -> WorkspaceDiagsErrors
  | Some raw ->
      (match String.lowercase_ascii (String.trim raw) with
       | "off" -> WorkspaceDiagsOff
       | "all" -> WorkspaceDiagsAll
       | "errors" | "" -> WorkspaceDiagsErrors
       | _ -> WorkspaceDiagsErrors)

let pressure_soft_mb_default = 512
let pressure_critical_mb_default = 768
let startup_target_ms_default = 15000
let startup_diag_hover_target_ms_default = 15000
let startup_nav_target_ms_default = 30000
let startup_aggressive_window_ms_default = 3000
let startup_aggressive_bg_budget_ms_default = 20
let profile_small_max_bytes = 10 * 1024 * 1024
let profile_medium_max_bytes = 40 * 1024 * 1024

let workspace_profile_mode_of_env () : workspace_profile_mode =
  match Sys.getenv_opt "JOVIAL_WORKSPACE_PROFILE_MODE" with
  | None -> ProfileModeAuto
  | Some raw ->
      (match String.lowercase_ascii (String.trim raw) with
       | "small" -> ProfileModeSmall
       | "medium" -> ProfileModeMedium
       | "large" -> ProfileModeLarge
       | _ -> ProfileModeAuto)

let root_model_of_env () : root_model =
  match Sys.getenv_opt "JOVIAL_ROOT_MODEL" with
  | None -> RootModelAuto
  | Some raw ->
      (match String.lowercase_ascii (String.trim raw) with
       | "heuristic" -> RootModelHeuristic
       | "manual" -> RootModelManual
       | _ -> RootModelAuto)

let parse_manual_root_files_env () : string list =
  match Sys.getenv_opt "JOVIAL_ROOT_MANUAL_FILES" with
  | None -> []
  | Some raw ->
      raw
      |> String.split_on_char ';'
      |> List.concat_map (fun s -> String.split_on_char ',' s)
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")

let create () : t =
  let startup_target_ms =
    max 1000 (env_nonneg_int "JOVIAL_STARTUP_TARGET_MS" ~default:startup_target_ms_default)
  in
  let startup_diag_hover_target_ms =
    max 1000
      (env_nonneg_int
         "JOVIAL_STARTUP_DIAG_HOVER_TARGET_MS"
         ~default:startup_diag_hover_target_ms_default)
  in
  let startup_nav_target_ms =
    max startup_diag_hover_target_ms
      (env_nonneg_int
         "JOVIAL_STARTUP_NAV_TARGET_MS"
         ~default:startup_nav_target_ms_default)
  in
  let startup_aggressive_window_ms =
    max 250
      (env_nonneg_int
         "JOVIAL_STARTUP_AGGRESSIVE_WINDOW_MS"
         ~default:startup_aggressive_window_ms_default)
  in
  let startup_aggressive_bg_budget_ms =
    max 1
      (env_nonneg_int
         "JOVIAL_STARTUP_AGGRESSIVE_BG_BUDGET_MS"
         ~default:startup_aggressive_bg_budget_ms_default)
  in
  let parse_worker_max_inflight =
    max 1
      (env_nonneg_int
         "JOVIAL_BG_PARSE_WORKER_MAX_INFLIGHT"
         ~default:1)
  in
  let bg_high_large_budget_ms =
    max 1 (env_nonneg_int "JOVIAL_BG_HIGH_LARGE_BUDGET_MS" ~default:8)
  in
  let root_model = root_model_of_env () in
  let root_heuristic_fallback =
    env_flag "JOVIAL_ROOT_HEURISTIC_FALLBACK" ~default:true
  in
  let root_manual_files = parse_manual_root_files_env () in
  let graph_requeue_cooldown_ms =
    max 0 (env_nonneg_int "JOVIAL_GRAPH_REQUEUE_COOLDOWN_MS" ~default:400)
  in
  let root_closure_max_depth =
    max 1 (env_nonneg_int "JOVIAL_ROOT_CLOSURE_MAX_DEPTH" ~default:4)
  in
  let root_closure_target_files =
    max 8 (env_nonneg_int "JOVIAL_ROOT_CLOSURE_TARGET_FILES" ~default:256)
  in
  let skeleton_prefix_bytes =
    max 1024 (env_nonneg_int "JOVIAL_SKELETON_PREFIX_BYTES" ~default:262144)
  in
  let sched_open_doc_min_share_pct =
    let n = env_nonneg_int "JOVIAL_SCHED_OPEN_DOC_MIN_SHARE_PCT" ~default:50 in
    min 100 (max 0 n)
  in
  let now = Perf_stats.now_ms () in
  {
    docs = Hashtbl.create 32;
    files = Hashtbl.create 64;
    root_path = None;
    index = None;
    symbol_hints = None;
    nav_response_cache = Hashtbl.create 2048;
    semantic_store = Semantic_store.create ();
    sem_store_enabled = env_flag "JOVIAL_SEM_STORE" ~default:true;
    lsif_delta_enabled = env_flag "JOVIAL_LSIF_DELTA" ~default:true;
    lsif_delta_state = Lsif_delta.create ();
    lsif_snapshot_revision = 0;
    lsif_snapshot_payload = None;
    lsif_snapshot_symbols = None;
    workspace_diag_mode = workspace_diag_mode_of_env ();
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
    closed_doc_lru_max = max 1 (env_nonneg_int "JOVIAL_CLOSED_DOC_LRU_MAX" ~default:256);
    parse_file_max_bytes = max 1 (env_nonneg_int "JOVIAL_PARSE_FILE_MAX_BYTES" ~default:16777216);
    bg_large_file_bytes = max 1 (env_nonneg_int "JOVIAL_BG_LARGE_FILE_BYTES" ~default:800000);
    bg_large_parse_idle_quiet_ms =
      max 0 (env_nonneg_int "JOVIAL_BG_LARGE_PARSE_IDLE_QUIET_MS" ~default:150);
    pressure_soft_mb = max 64 (env_nonneg_int "JOVIAL_PRESSURE_SOFT_MB" ~default:pressure_soft_mb_default);
    pressure_critical_mb =
      max 64
        (env_nonneg_int "JOVIAL_PRESSURE_CRITICAL_MB" ~default:pressure_critical_mb_default);
    pressure_mode = PressureNormal;
    pressure_live_mb = 0;
    pressure_last_check_ms = 0.0;
    startup_started_ms = now;
    startup_diag_hover_target_ms;
    startup_nav_target_ms;
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
    startup_target_ms;
    startup_aggressive_window_ms;
    startup_aggressive_bg_budget_ms;
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
    workspace_profile_mode = workspace_profile_mode_of_env ();
    root_model;
    root_heuristic_fallback;
    root_manual_files;
    graph_requeue_cooldown_ms;
    root_closure_max_depth;
    root_closure_target_files;
    skeleton_prefix_bytes;
    sched_open_doc_min_share_pct;
    quick_nav_index = Hashtbl.create 2048;
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
    parse_worker_max_inflight;
    parse_worker_started = false;
    parse_worker_stop = false;
    bg_high_large_budget_ms;
    parse_epoch = 0;
    request_cancel_checker = None;
  }

let invalidate_lsif_snapshot (ws:t) : unit =
  ws.lsif_snapshot_revision <- ws.lsif_snapshot_revision + 1

let normalize_name (s:string) : string =
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

let size_class_of_path (ws:t) (path:string) : file_size_class =
  match (try Some (Unix.stat path).Unix.st_size with _ -> None) with
  | Some n when n >= ws.bg_large_file_bytes -> FileSizeLarge
  | _ -> FileSizeSmall

let file_class_rank = function
  | FileClassOpen -> 0
  | FileClassEntry -> 1
  | FileClassNormal -> 2

let basename_upper (path:string) : string =
  Filename.basename path |> String.uppercase_ascii

let is_main_boot_heuristic (path:string) : bool =
  let b = basename_upper path in
  String.length b >= 4
  && (String.sub b 0 4 = "MAIN" || String.sub b 0 4 = "BOOT")

let mark_graph_dirty (ws:t) : unit =
  ws.graph_needs_refresh <- true

let is_nav_ident_start_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' | '$' -> true
  | _ -> false

let is_nav_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' | '\'' -> true
  | _ -> false

let ident_keys_of_text (text:string) : string list =
  let n = String.length text in
  let out = Hashtbl.create 16 in
  let rec loop i =
    if i >= n then ()
    else
      let c = text.[i] in
      if is_nav_ident_start_char c
         && (i = 0 || not (is_nav_ident_char text.[i - 1]))
      then
        let j = ref (i + 1) in
        while !j < n && is_nav_ident_char text.[!j] do
          incr j
        done;
        let key = normalize_name (String.sub text i (!j - i)) in
        if key <> "" then Hashtbl.replace out key true;
        loop !j
      else
        loop (i + 1)
  in
  loop 0;
  Hashtbl.fold (fun k _ acc -> k :: acc) out []

let nav_response_cache_key ~(kind:string) ~(uri:T.DocumentUri.t) ~(symbol_key:string) : string =
  Printf.sprintf "%s|%s|%s" kind (Uri_path.docuri_to_string uri) (normalize_name symbol_key)

let nav_response_cache_get
    (ws:t)
    ~(kind:string)
    ~(uri:T.DocumentUri.t)
    ~(symbol_key:string)
  : Yojson.Safe.t option =
  Hashtbl.find_opt ws.nav_response_cache (nav_response_cache_key ~kind ~uri ~symbol_key)

let nav_response_cache_put
    (ws:t)
    ~(kind:string)
    ~(uri:T.DocumentUri.t)
    ~(symbol_key:string)
    ~(payload:Yojson.Safe.t)
  : unit =
  let key = normalize_name symbol_key in
  if key <> "" then
    Hashtbl.replace ws.nav_response_cache
      (nav_response_cache_key ~kind ~uri ~symbol_key:key)
      payload

let parse_nav_response_cache_key (k:string) : (string * string * string) option =
  match String.split_on_char '|' k with
  | kind :: uri_s :: sym :: [] -> Some (kind, uri_s, sym)
  | kind :: uri_s :: sym_parts ->
      Some (kind, uri_s, String.concat "|" sym_parts)
  | _ -> None

let clear_nav_response_cache_for_uri (ws:t) ~(uri:T.DocumentUri.t) : unit =
  let uri_s = Uri_path.docuri_to_string uri in
  let stale =
    Hashtbl.fold (fun k _ acc ->
      match parse_nav_response_cache_key k with
      | Some (_, u, _) when u = uri_s -> k :: acc
      | _ -> acc
    ) ws.nav_response_cache []
  in
  List.iter (fun k -> Hashtbl.remove ws.nav_response_cache k) stale

let invalidate_nav_response_cache_for_keys
    (ws:t)
    ~(uri:T.DocumentUri.t)
    ~(keys:string list)
  : unit =
  if keys = [] then ()
  else
    let uri_s = Uri_path.docuri_to_string uri in
    let keyset = Hashtbl.create (max 16 (List.length keys * 2)) in
    List.iter (fun k ->
      let kk = normalize_name k in
      if kk <> "" then Hashtbl.replace keyset kk true
    ) keys;
    if Hashtbl.length keyset = 0 then ()
    else
      let stale =
        Hashtbl.fold (fun k _ acc ->
          match parse_nav_response_cache_key k with
          | Some (_, u, sym) when u = uri_s && Hashtbl.mem keyset (normalize_name sym) -> k :: acc
          | _ -> acc
        ) ws.nav_response_cache []
      in
      List.iter (fun k -> Hashtbl.remove ws.nav_response_cache k) stale

let compool_key_of_doc (doc:Document.t) : string option =
  match doc.Document.compool_def with
  | None -> None
  | Some raw ->
      let key = normalize_name raw in
      if key = "" then None else Some key

let invalidate_importer_nav_state_for_compool_key
    (ws:t)
    ~(compool_key:string)
  : unit =
  if not ws.sem_store_enabled then ()
  else
    let key = normalize_name compool_key in
    if key = "" then ()
    else
      Semantic_store.uris_importing_compool ws.semantic_store ~compool_key:key
      |> List.iter (fun uri ->
           Semantic_store.remove_uri ws.semantic_store ~uri;
           clear_nav_response_cache_for_uri ws ~uri)

let importer_uris_for_compool_key
    (ws:t)
    ~(compool_key:string)
  : T.DocumentUri.t list =
  let key = normalize_name compool_key in
  if key = "" then []
  else
    let seen : (string, bool) Hashtbl.t = Hashtbl.create 64 in
    let out = ref [] in
    let add_uri (uri:T.DocumentUri.t) =
      let uri_key = Uri_path.docuri_to_string uri in
      if not (Hashtbl.mem seen uri_key) then (
        Hashtbl.replace seen uri_key true;
        out := uri :: !out
      )
    in
    if ws.sem_store_enabled then
      Semantic_store.uris_importing_compool ws.semantic_store ~compool_key:key
      |> List.iter add_uri;
    Hashtbl.iter
      (fun uri doc ->
        if List.exists
             (fun (imp:Preprocess.import) ->
               imp.kind = Preprocess.Compool && normalize_name imp.name = key)
             (Document.imports doc)
        then
          add_uri uri)
      ws.docs;
    List.rev !out

let normalize_path_key (p:string) : string =
  let p = String.map (fun c -> if c = '\\' then '/' else c) p in
  if Sys.win32 then String.lowercase_ascii p else p

let same_path a b =
  normalize_path_key a = normalize_path_key b

let file_size_bytes (path:string) : int option =
  try
    Some (Unix.(stat path).st_size)
  with _ ->
    None

let is_parse_guard_exceeded ~(max_bytes:int) ~(text_len:int) : bool =
  max_bytes > 0 && text_len > max_bytes

let diag_parse_guard ~(file:string option) ~(max_bytes:int) ~(actual_bytes:int) : T.Diagnostic.t =
  let z = { Ast.Loc.line = 1; col = 0; offset = 0 } in
  let loc = Ast.Loc.make ~file ~start_pos:z ~end_pos:z in
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Error
    ~source:"parse"
    ~message:(Printf.sprintf
      "File parse skipped (%d bytes exceeds guard %d bytes)."
      actual_bytes
      max_bytes)
    loc

let make_doc_with_parse_guard
    (ws:t)
    ~(uri:T.DocumentUri.t)
    ~(file:string option)
    ~(text:string)
    ~(actual_bytes:int)
  : Document.t =
  Perf_stats.tick "parse.large_file_guard";
  Document.make_unparsed
    ~uri
    ~file
    ~text
    ~parse_diags:[diag_parse_guard ~file ~max_bytes:ws.parse_file_max_bytes ~actual_bytes]

let parse_guarded_document_make
    (ws:t)
    ~(uri:T.DocumentUri.t)
    ~(file:string option)
    ~(text:string)
  : Document.t =
  if is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes ~text_len:(String.length text) then
    make_doc_with_parse_guard ws ~uri ~file ~text ~actual_bytes:(String.length text)
  else
    Document.make ~uri ~file ~text

let has_open_doc_for_path_key (ws:t) ~(path_key:string) : bool =
  Hashtbl.fold (fun _uri doc acc ->
    acc
    ||
    match doc.Document.file with
    | None -> false
    | Some p -> normalize_path_key p = path_key
  ) ws.docs false

let touch_closed_doc_path (ws:t) ~(path_key:string) : unit =
  if path_key = "" then ()
  else if has_open_doc_for_path_key ws ~path_key then
    Hashtbl.remove ws.closed_doc_last_touch path_key
  else (
    ws.closed_doc_lru_clock <- ws.closed_doc_lru_clock + 1;
    Hashtbl.replace ws.closed_doc_last_touch path_key ws.closed_doc_lru_clock
  )

let evict_closed_docs_if_needed (ws:t) : unit =
  let max_closed = max 1 ws.closed_doc_lru_max in
  let closed =
    Hashtbl.fold (fun path_key doc acc ->
      if has_open_doc_for_path_key ws ~path_key then acc
      else
        let touch =
          match Hashtbl.find_opt ws.closed_doc_last_touch path_key with
          | Some t -> t
          | None -> 0
        in
        (path_key, touch, doc) :: acc
    ) ws.files []
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
            if ws.sem_store_enabled then
              Semantic_store.remove_uri ws.semantic_store ~uri:doc.Document.uri;
            Perf_stats.tick "mem.closed_doc_evict";
            drop (n - 1) tl
    in
    drop to_drop sorted

let find_open_doc_for_path (ws:t) ~(path:string) : Document.t option =
  let found_open = ref None in
  Hashtbl.iter (fun _uri doc ->
    match doc.Document.file with
    | Some p when same_path p path -> found_open := Some doc
    | _ -> ()
  ) ws.docs;
  !found_open

let classify_bg_queue_kind
    (ws:t)
    ~(lane:schedule_lane)
    ~(high:bool)
    ~(path:string)
  : bg_queue_kind =
  let size_opt = file_size_bytes path in
  let is_large =
    match size_opt with
    | Some n -> n >= ws.bg_large_file_bytes
    | None -> false
  in
  match lane, is_large with
  | LaneOpen, false -> BgQueueHighSmall
  | LaneOpen, true -> BgQueueHighLarge
  | LaneRoot, false -> BgQueueRootSmall
  | LaneRoot, true -> BgQueueRootLarge
  | LaneSweep, false ->
      if high then BgQueueHighSmall else BgQueueNormalSmall
  | LaneSweep, true ->
      if high then BgQueueHighLarge else BgQueueNormalLarge

let enqueue_bg_path
    ?(lane:schedule_lane=LaneSweep)
    ?(reason_group:string="generic")
    (ws:t)
    ~(high:bool)
    (path:string)
  : unit =
  let path_key = normalize_path_key path in
  if path_key <> "" then
    if Hashtbl.mem ws.parse_worker_inflight path_key then
      ()
    else
      let requested = classify_bg_queue_kind ws ~lane ~high ~path in
      let doc_generation =
        match find_open_doc_for_path ws ~path with
        | Some doc ->
            (match Hashtbl.find_opt ws.open_parse_generation (Uri_path.docuri_to_string doc.Document.uri) with
             | Some g -> g
             | None -> 0)
        | None -> 0
      in
      let work_key =
        Printf.sprintf "%s|%s|%s|g%d|d%d"
          path_key
          (string_of_lane lane)
          reason_group
          ws.graph_epoch
          doc_generation
      in
      let now = Perf_stats.now_ms () in
      let is_cooldown_hit =
        match Hashtbl.find_opt ws.bg_enqueue_recent_ms work_key with
        | Some last_ms ->
            ws.graph_requeue_cooldown_ms > 0
            && lane <> LaneOpen
            && now -. last_ms < float_of_int ws.graph_requeue_cooldown_ms
        | None ->
            false
      in
      if is_cooldown_hit then
        Perf_stats.tick "sched.requeue_cooldown_hit"
      else (
        Hashtbl.replace ws.bg_enqueue_recent_ms work_key now;
        match Hashtbl.find_opt ws.bg_enqueued path_key with
        | None ->
            Hashtbl.replace ws.bg_enqueued path_key requested;
            (match requested with
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
              Perf_stats.tick "bg.queue_promoted"
            ) else
              Perf_stats.tick "sched.duplicate_work_dropped"
      )

let dequeue_bg_path
    (ws:t)
    ~(mode:bg_tick_mode)
    ~(allow_normal_large:bool)
    ~(allow_root_large:bool)
    ~(prefer_open:bool)
  : (string * bg_queue_kind) option =
  let rec pop_from_queue
      (q:string Queue.t)
      ~(expect:bg_queue_kind)
    : (string * bg_queue_kind) option =
    if Queue.is_empty q then None
    else
      let p = Queue.pop q in
      let key = normalize_path_key p in
      match Hashtbl.find_opt ws.bg_enqueued key with
      | Some kind when kind = expect ->
          Hashtbl.remove ws.bg_enqueued key;
          Some (p, kind)
      | _ ->
          pop_from_queue q ~expect
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
    | BgTickInteractive
    | BgTickIdle ->
        if not allow_normal_large then None
        else pop_from_queue ws.bg_norm_large_queue ~expect:BgQueueNormalLarge
  in
  let prioritize_open_large =
    ws.startup_diag_hover_ready_ms = None
    && Hashtbl.length ws.open_parse_generation > 0
  in
  let pops =
    if prefer_open then
      if prioritize_open_large then
        [ pop_high_large; pop_high_small; pop_root_small; pop_root_large; pop_norm_small; pop_norm_large ]
      else
        [ pop_high_small; pop_high_large; pop_root_small; pop_root_large; pop_norm_small; pop_norm_large ]
    else
      if prioritize_open_large then
        [ pop_root_small; pop_root_large; pop_high_large; pop_high_small; pop_norm_small; pop_norm_large ]
      else
        [ pop_root_small; pop_root_large; pop_high_small; pop_high_large; pop_norm_small; pop_norm_large ]
  in
  let rec pick = function
    | [] -> None
    | f :: tl ->
        (match f () with
         | Some _ as hit -> hit
         | None -> pick tl)
  in
  pick pops

let enqueue_bg_diag_update
    (ws:t)
    ~(uri:T.DocumentUri.t)
    ~(diags:T.Diagnostic.t list)
  : unit =
  let key = Uri_path.docuri_to_string uri in
  if Hashtbl.mem ws.bg_pending_diag_payloads key then
    Perf_stats.tick "bg.diag_pending_overwrite";
  Hashtbl.replace ws.bg_pending_diag_payloads key (uri, diags);
  if not (Hashtbl.mem ws.bg_pending_diag_set key) then (
    Hashtbl.replace ws.bg_pending_diag_set key true;
    Queue.add key ws.bg_pending_diag_updates
  )

let tick_open_diag_revalidate_reason (reason:string) : unit =
  match String.lowercase_ascii (String.trim reason) with
  | "compool" | "compool_change" ->
      Perf_stats.tick "diag.open.revalidate_reason_compool"
  | "reconcile" | "index_reconcile" ->
      Perf_stats.tick "diag.open.revalidate_reason_reconcile"
  | "hint" | "hint_ready" | "xmodule_ready" ->
      Perf_stats.tick "diag.open.revalidate_reason_hint_ready"
  | _ ->
      ()

let enqueue_open_diag_revalidate
    (ws:t)
    ~(uri:T.DocumentUri.t)
    ~(reason:string)
  : unit =
  if Hashtbl.mem ws.docs uri then (
    let key = Uri_path.docuri_to_string uri in
    Hashtbl.replace ws.open_diag_revalidate_payloads key (uri, reason);
    if not (Hashtbl.mem ws.open_diag_revalidate_set key) then (
      Hashtbl.replace ws.open_diag_revalidate_set key true;
      Queue.add key ws.open_diag_revalidate_updates
    );
    Perf_stats.tick "diag.open.revalidate_enqueued";
    tick_open_diag_revalidate_reason reason
  )

let enqueue_all_open_diag_revalidate
    (ws:t)
    ~(reason:string)
  : unit =
  Hashtbl.iter
    (fun uri _ -> enqueue_open_diag_revalidate ws ~uri ~reason)
    ws.docs

let filter_workspace_diags (ws:t) (diags:T.Diagnostic.t list) : T.Diagnostic.t list =
  match ws.workspace_diag_mode with
  | WorkspaceDiagsOff -> []
  | WorkspaceDiagsAll -> diags
  | WorkspaceDiagsErrors ->
      List.filter (fun (d:T.Diagnostic.t) ->
        match d.severity with
        | Some T.DiagnosticSeverity.Error -> true
        | _ -> false
      ) diags

let is_probably_network_path (p:string) : bool =
  let n = String.length p in
  n >= 2
  && ((p.[0] = '\\' && p.[1] = '\\')
      || (p.[0] = '/' && p.[1] = '/'))

let set_root_path (ws:t) (root:string option) : unit =
  if ws.root_path <> root then (
    mark_graph_dirty ws;
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
    done
  )

let set_root_uri (ws:t) (root_uri:T.DocumentUri.t option) : unit =
  set_root_path ws
    (match root_uri with
     | None -> None
     | Some u -> Uri_path.file_path_of_uri u)

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
let index_startup_disable = env_flag "JOVIAL_INDEX_STARTUP_DISABLE" ~default:false
let index_startup_dirs = env_nonneg_int "JOVIAL_INDEX_STARTUP_DIRS" ~default:0
let index_startup_files = env_nonneg_int "JOVIAL_INDEX_STARTUP_FILES" ~default:0
let index_startup_dirs_network = env_nonneg_int "JOVIAL_INDEX_STARTUP_DIRS_NETWORK" ~default:0
let index_startup_files_network = env_nonneg_int "JOVIAL_INDEX_STARTUP_FILES_NETWORK" ~default:0
let index_stale_reconcile_min_interval_ms =
  max 100 (env_nonneg_int "JOVIAL_INDEX_STALE_RECONCILE_MIN_INTERVAL_MS" ~default:900)
let index_reconcile_escalate_dirs =
  max 1 (env_nonneg_int "JOVIAL_INDEX_RECONCILE_ESCALATE_DIRS" ~default:24)
let index_reconcile_escalate_files =
  max 1 (env_nonneg_int "JOVIAL_INDEX_RECONCILE_ESCALATE_FILES" ~default:3000)
let didchange_semi_check_enabled = env_flag "JOVIAL_DIDCHANGE_SEMI_CHECK" ~default:true
let didchange_semi_check_max_changes =
  env_nonneg_int "JOVIAL_DIDCHANGE_SEMI_MAX_CHANGES" ~default:6
let didchange_semi_check_max_lines =
  env_nonneg_int "JOVIAL_DIDCHANGE_SEMI_MAX_LINES" ~default:30
let didchange_semi_check_max_text_chars =
  env_nonneg_int "JOVIAL_DIDCHANGE_SEMI_MAX_TEXT_CHARS" ~default:1200
let didchange_semi_force_full_every =
  max 1 (env_nonneg_int "JOVIAL_DIDCHANGE_SEMI_FORCE_FULL_EVERY" ~default:20)
let didchange_defer_parse_enabled =
  env_flag "JOVIAL_DIDCHANGE_DEFER_PARSE" ~default:true
let didchange_defer_parse_min_doc_chars =
  env_nonneg_int "JOVIAL_DIDCHANGE_DEFER_MIN_DOC_CHARS" ~default:120000
let didchange_defer_parse_max_changes =
  env_nonneg_int "JOVIAL_DIDCHANGE_DEFER_MAX_CHANGES" ~default:8
let didchange_defer_parse_max_inserted_chars =
  env_nonneg_int "JOVIAL_DIDCHANGE_DEFER_MAX_INSERTED_CHARS" ~default:1800
let didchange_defer_parse_force_full_every =
  max 1 (env_nonneg_int "JOVIAL_DIDCHANGE_DEFER_FORCE_FULL_EVERY" ~default:24)
let didopen_defer_parse_enabled =
  env_flag "JOVIAL_DIDOPEN_DEFER_PARSE" ~default:true
let didopen_defer_parse_min_doc_chars =
  env_nonneg_int "JOVIAL_DIDOPEN_DEFER_MIN_DOC_CHARS" ~default:120000
let didopen_always_provisional =
  env_flag "JOVIAL_DIDOPEN_ALWAYS_PROVISIONAL" ~default:false
let didopen_disable_foreground_tick =
  env_flag "JOVIAL_DIDOPEN_DISABLE_FOREGROUND_TICK" ~default:true
let warmup_suppress_crossmodule_unresolved =
  env_flag "JOVIAL_DIAG_WARMUP_SUPPRESS_XMODULE" ~default:true
let bg_seed_paths_per_tick =
  max 1 (env_nonneg_int "JOVIAL_BG_SEED_PATHS_PER_TICK" ~default:128)
let nav_soft_budget_ms =
  max 1 (env_nonneg_int "JOVIAL_NAV_SOFT_BUDGET_MS" ~default:1800)
let nav_quick_scan_files =
  match Sys.getenv_opt "JOVIAL_NAV_QUICK_SCAN_FILES" with
  | Some _ -> env_nonneg_int "JOVIAL_NAV_QUICK_SCAN_FILES" ~default:48
  | None -> env_nonneg_int "JOVIAL_NAV_SOURCE_SCAN_FILES" ~default:48
let nav_quick_scan_total_bytes =
  env_nonneg_int "JOVIAL_NAV_QUICK_SCAN_TOTAL_BYTES" ~default:(12 * 1024 * 1024)
let nav_quick_scan_per_file_bytes =
  env_nonneg_int "JOVIAL_NAV_QUICK_SCAN_PER_FILE_BYTES" ~default:262144
let nav_miss_import_scan_max_chars =
  max 4096 (env_nonneg_int "JOVIAL_NAV_MISS_IMPORT_SCAN_MAX_CHARS" ~default:262144)
let nav_miss_high_enqueue_cap =
  env_nonneg_int "JOVIAL_NAV_MISS_HIGH_ENQUEUE_CAP" ~default:24
let pressure_check_interval_ms =
  max 50 (env_nonneg_int "JOVIAL_PRESSURE_CHECK_INTERVAL_MS" ~default:250)
let lsif_doc_load_budget_normal =
  max 1 (env_nonneg_int "JOVIAL_LSIF_DOC_LOAD_BUDGET" ~default:400)
let lsif_doc_load_budget_soft =
  max 1 (env_nonneg_int "JOVIAL_LSIF_DOC_LOAD_BUDGET_SOFT" ~default:64)

let startup_scan_budget_for_root ~(network:bool) : int * int =
  if index_startup_disable then
    (0, 0)
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

let pressure_mode_to_string = function
  | PressureNormal -> "normal"
  | PressureSoft -> "soft"
  | PressureCritical -> "critical"

let startup_phase_to_string = function
  | StartupCold -> "cold"
  | StartupWarming -> "warming"
  | StartupAggressiveCatchUp -> "aggressiveCatchUp"
  | StartupReady -> "ready"

let word_bytes : int =
  max 1 (Sys.word_size / 8)

let words_to_mb (words:int) : int =
  let bytes = max 0 (words * word_bytes) in
  let mb = 1024 * 1024 in
  if bytes <= 0 then 0 else (bytes + mb - 1) / mb

let update_pressure_state (ws:t) : unit =
  let now = Perf_stats.now_ms () in
  if now -. ws.pressure_last_check_ms >= float_of_int pressure_check_interval_ms then (
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
       | PressureSoft -> Perf_stats.tick "pressure.enter_soft"; Perf_stats.tick "mem.pressure_soft"
       | PressureCritical ->
           Perf_stats.tick "pressure.enter_critical";
           Perf_stats.tick "mem.pressure_critical"
       | PressureNormal -> Perf_stats.tick "pressure.exit_to_normal");
      ws.pressure_mode <- next_mode
    )
  )

let workspace_pressure_mode (ws:t) : pressure_mode =
  update_pressure_state ws;
  ws.pressure_mode

let workspace_pressure_live_mb (ws:t) : int =
  update_pressure_state ws;
  ws.pressure_live_mb

let bg_diag_allowed (ws:t) : bool =
  match workspace_pressure_mode ws with
  | PressureCritical ->
      Perf_stats.tick "bg.diag_paused_critical";
      false
  | PressureNormal | PressureSoft ->
      true

let lsif_doc_load_budget_for_pressure (ws:t) : int =
  match workspace_pressure_mode ws with
  | PressureNormal -> lsif_doc_load_budget_normal
  | PressureSoft ->
      Perf_stats.tick "lsif.throttle_soft";
      lsif_doc_load_budget_soft
  | PressureCritical ->
      Perf_stats.tick "lsif.defer_critical";
      0

let request_cancelled (ws:t) : bool =
  match ws.request_cancel_checker with
  | None -> false
  | Some f ->
      (try f () with _ -> false)

let with_request_cancel_checker (ws:t) (is_cancelled:unit -> bool) (f:unit -> 'a) : 'a =
  let prev = ws.request_cancel_checker in
  ws.request_cancel_checker <- Some is_cancelled;
  Fun.protect
    ~finally:(fun () -> ws.request_cancel_checker <- prev)
    f

let startup_mark_started (ws:t) : unit =
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

let startup_open_docs_converged (ws:t) : bool =
  Hashtbl.fold (fun _ doc acc ->
    acc && doc.Document.parse_rev = doc.Document.rev
  ) ws.docs true

let startup_open_docs_pending_count (ws:t) : int =
  Hashtbl.fold (fun _ doc acc ->
    if doc.Document.parse_rev = doc.Document.rev then acc else acc + 1
  ) ws.docs 0

let startup_index_complete (ws:t) : bool =
  match ws.index with
  | None -> false
  | Some idx -> Workspace_index.is_complete idx

let startup_index_reconcile_pending (ws:t) : bool =
  match ws.index with
  | None -> false
  | Some idx -> Workspace_index.reconcile_pending idx

let startup_seed_complete (ws:t) : bool =
  ws.graph_root_closure_cursor >= Array.length ws.graph_root_closure_paths
  && (not ws.graph_needs_refresh)
  && (not ws.bg_seed_needs_refresh)
  && ws.bg_seed_cursor >= Array.length ws.bg_seed_paths

let startup_high_queues_empty (ws:t) : bool =
  Queue.is_empty ws.bg_high_small_queue
  && Queue.is_empty ws.bg_high_large_queue
  && Queue.is_empty ws.bg_root_small_queue
  && Queue.is_empty ws.bg_root_large_queue
  && not (Hashtbl.fold (fun _ kind acc ->
      acc || kind = ParseJobHighLarge || kind = ParseJobRootLarge) ws.parse_worker_inflight false)

let startup_queues_empty (ws:t) : bool =
  startup_high_queues_empty ws
  && Queue.is_empty ws.bg_norm_small_queue
  && Queue.is_empty ws.bg_norm_large_queue
  && Queue.is_empty ws.bg_pending_diag_updates
  && Hashtbl.length ws.bg_pending_diag_payloads = 0
  && Hashtbl.length ws.parse_worker_inflight = 0

let startup_hints_ready (ws:t) : bool =
  ws.symbol_hints <> None
  || (
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
     )

let quick_nav_index_complete (ws:t) : bool =
  ws.quick_nav_index_total > 0
  && ws.quick_nav_index_done >= ws.quick_nav_index_total
  && Queue.is_empty ws.quick_nav_pending_paths
  && Hashtbl.length ws.quick_nav_pending_set = 0

let quick_nav_index_ready_for_startup (ws:t) : bool =
  if quick_nav_index_complete ws then true
  else if ws.quick_nav_index_total <= 0 then false
  else
    let goal = min ws.quick_nav_index_total 64 in
    ws.quick_nav_index_done >= goal

let startup_nav_prereqs_ready (ws:t) : bool =
  ws.graph_root_closure_cursor >= Array.length ws.graph_root_closure_paths
  && (startup_hints_ready ws || quick_nav_index_ready_for_startup ws)

let startup_open_docs_authoritative (ws:t) : bool =
  startup_open_docs_converged ws
  && Hashtbl.length ws.open_parse_generation = 0

let startup_open_diag_revalidate_empty (ws:t) : bool =
  Queue.is_empty ws.open_diag_revalidate_updates
  && Hashtbl.length ws.open_diag_revalidate_payloads = 0

let xmodule_diag_prereqs_ready (ws:t) : bool =
  startup_index_complete ws
  && not (startup_index_reconcile_pending ws)
  && startup_hints_ready ws
  && startup_open_docs_authoritative ws

let startup_is_diag_hover_ready (ws:t) : bool =
  xmodule_diag_prereqs_ready ws
  && startup_open_diag_revalidate_empty ws

let startup_is_fully_navigable (ws:t) : bool =
  startup_nav_prereqs_ready ws
  && startup_is_diag_hover_ready ws

let startup_elapsed_ms_float (ws:t) : float =
  let now = Perf_stats.now_ms () in
  max 0.0 (now -. ws.startup_started_ms)

let startup_update_phase (ws:t) : unit =
  let next_phase =
    match ws.startup_fully_nav_ready_ms with
    | Some _ ->
        StartupReady
    | None ->
        let elapsed_ms = startup_elapsed_ms_float ws in
        let warm_window =
          float_of_int (max 0 (ws.startup_nav_target_ms - ws.startup_aggressive_window_ms))
        in
        if elapsed_ms >= warm_window then StartupAggressiveCatchUp
        else StartupWarming
  in
  if ws.startup_phase <> next_phase then (
    ws.startup_phase <- next_phase;
    (match next_phase with
     | StartupWarming -> Perf_stats.tick "startup.phase_warming"
     | StartupAggressiveCatchUp -> Perf_stats.tick "startup.phase_aggressive"
     | StartupCold | StartupReady -> ())
  )

let startup_ready_components (ws:t)
  : bool * bool * bool * bool * bool * bool * bool * bool * bool * bool * bool * bool =
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

let startup_is_ready (ws:t) : bool =
  let index_complete, index_reconcile_clear, seed_complete, _high_queues_empty, queues_empty, hints_ready, nav_prereqs_ready, _quick_nav_index_complete, open_docs_converged, open_docs_authoritative, xmodule_ready, open_diag_revalidate_empty =
    startup_ready_components ws
  in
  index_complete && index_reconcile_clear && seed_complete
  && queues_empty && hints_ready && nav_prereqs_ready
  && open_docs_converged && open_docs_authoritative
  && xmodule_ready && open_diag_revalidate_empty

let startup_elapsed_ms (ws:t) : int =
  max 0 (int_of_float (startup_elapsed_ms_float ws))

let update_startup_ready_state (ws:t) : unit =
  let elapsed = startup_elapsed_ms ws in
  if elapsed > ws.startup_diag_hover_target_ms
     && not ws.startup_diag_hover_miss_notified
     && ws.startup_diag_hover_ready_ms = None
  then (
    ws.startup_diag_hover_miss_notified <- true;
    Perf_stats.tick "startup.miss_15s"
  );
  if elapsed > ws.startup_nav_target_ms
     && not ws.startup_nav_miss_notified
     && ws.startup_fully_nav_ready_ms = None
  then
    ws.startup_nav_miss_notified <- true;

  let xmodule_ready_now = xmodule_diag_prereqs_ready ws in
  if xmodule_ready_now && not ws.xmodule_diag_ready_prev then (
    ws.xmodule_diag_ready_prev <- true;
    Perf_stats.tick "diag.xmodule_ready_transition";
    enqueue_all_open_diag_revalidate ws ~reason:"xmodule_ready"
  ) else if not xmodule_ready_now then
    ws.xmodule_diag_ready_prev <- false;

  if startup_is_diag_hover_ready ws then (
    match ws.startup_diag_hover_ready_ms with
    | Some _ -> ()
    | None ->
        let now = Perf_stats.now_ms () in
        ws.startup_diag_hover_ready_ms <- Some now
  ) else (
    match ws.startup_diag_hover_ready_ms with
    | None -> ()
    | Some _ ->
        ws.startup_diag_hover_ready_ms <- None;
        ws.startup_diag_hover_notified <- false
  );

  if startup_is_fully_navigable ws then (
    match ws.startup_fully_nav_ready_ms with
    | Some _ -> ()
    | None ->
        let now = Perf_stats.now_ms () in
        ws.startup_fully_nav_ready_ms <- Some now;
        ws.startup_phase <- StartupReady;
        Perf_stats.tick "startup.ready";
        if ws.startup_phase_notified = Some StartupReady then
          ws.startup_phase_notified <- None
  ) else (
    match ws.startup_fully_nav_ready_ms with
    | None -> ()
    | Some _ ->
        ws.startup_fully_nav_ready_ms <- None;
        ws.startup_fully_nav_notified <- false;
        ws.startup_ready_notified <- false;
        ws.startup_phase_notified <- None;
        Perf_stats.tick "startup.ready_retracted"
  );
  ws.startup_ready_ms <- ws.startup_fully_nav_ready_ms;
  ws.startup_miss_notified <- ws.startup_nav_miss_notified;
  ws.startup_miss_emitted <- ws.startup_nav_miss_emitted;
  startup_update_phase ws

let startup_readiness_json (ws:t) : Yojson.Safe.t =
  let index_complete, index_reconcile_clear, seed_complete, high_queues_empty, queues_empty, hints_ready, nav_prereqs_ready, quick_nav_ready, open_docs_converged, open_docs_authoritative, xmodule_ready, open_diag_revalidate_empty =
    startup_ready_components ws
  in
  let index_reconcile_pending = not index_reconcile_clear in
  let pending_open_docs = startup_open_docs_pending_count ws in
  let pending_open_diag_revalidate = Queue.length ws.open_diag_revalidate_updates in
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
      "startedMs", `Float ws.startup_started_ms;
      ( "readyMs",
        match ws.startup_ready_ms with
        | None -> `Null
        | Some ts -> `Float ts );
      "elapsedMs", `Int elapsed_ms;
      "targetMs", `Int ws.startup_nav_target_ms;
      "readyWithinTarget", `Bool (elapsed_ms <= ws.startup_nav_target_ms);
      "isReady", `Bool (ws.startup_ready_ms <> None);
      "phase", `String (startup_phase_to_string ws.startup_phase);
      "aggressiveWindowMs", `Int ws.startup_aggressive_window_ms;
      "aggressiveBudgetMs", `Int ws.startup_aggressive_bg_budget_ms;
      "missNotified", `Bool ws.startup_nav_miss_notified;
      ( "stages",
        `Assoc
          [
            ( "diagHoverReady",
              `Assoc
                [
                  "targetMs", `Int ws.startup_diag_hover_target_ms;
                  "elapsedMs", `Int ready_diag_ms;
                  "isReady", `Bool (ws.startup_diag_hover_ready_ms <> None);
                  "readyWithinTarget", `Bool (ready_diag_ms <= ws.startup_diag_hover_target_ms);
                ] );
            ( "fullyNavigable",
              `Assoc
                [
                  "targetMs", `Int ws.startup_nav_target_ms;
                  "elapsedMs", `Int ready_nav_ms;
                  "isReady", `Bool (ws.startup_fully_nav_ready_ms <> None);
                  "readyWithinTarget", `Bool (ready_nav_ms <= ws.startup_nav_target_ms);
                ] );
          ] );
      ( "components",
        `Assoc
          [
            "indexComplete", `Bool index_complete;
            "indexReconcilePending", `Bool index_reconcile_pending;
            "seedComplete", `Bool seed_complete;
            "highQueuesEmpty", `Bool high_queues_empty;
            "queuesEmpty", `Bool queues_empty;
            "hintsReady", `Bool hints_ready;
            "navPrereqsReady", `Bool nav_prereqs_ready;
            "quickNavIndexReady", `Bool quick_nav_ready;
            "openDocsConverged", `Bool open_docs_converged;
            "openDocsAuthoritative", `Bool open_docs_authoritative;
            "openDocsPendingParse", `Int pending_open_docs;
            "xmoduleDiagReady", `Bool xmodule_ready;
            "openDiagRevalidateQueueEmpty", `Bool open_diag_revalidate_empty;
            "openDiagRevalidatePending", `Int pending_open_diag_revalidate;
          ] );
    ]

let consume_workspace_ready_event_json (ws:t) : Yojson.Safe.t option =
  update_startup_ready_state ws;
  let root_uri =
    match ws.root_path with
    | None -> None
    | Some p -> Some (`String (Uri_path.file_uri_of_path p))
  in
  match root_uri with
  | None -> None
  | Some root_uri_json ->
      if ws.startup_diag_hover_ready_ms <> None && not ws.startup_diag_hover_notified then (
        ws.startup_diag_hover_notified <- true;
        Some
          (`Assoc
             [
               "rootUri", root_uri_json;
               "stage", `String "diagHoverReady";
               ("readiness", startup_readiness_json ws);
             ])
      ) else if ws.startup_fully_nav_ready_ms <> None && not ws.startup_fully_nav_notified then (
        ws.startup_fully_nav_notified <- true;
        ws.startup_ready_notified <- true;
        Some
          (`Assoc
             [
               "rootUri", root_uri_json;
               "stage", `String "fullyNavigable";
               ("readiness", startup_readiness_json ws);
             ])
      ) else
        None

let consume_startup_phase_event_json (ws:t) : Yojson.Safe.t option =
  update_startup_ready_state ws;
  match ws.startup_phase, ws.startup_phase_notified with
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
             "rootUri", root_uri_json;
             "phase", `String (startup_phase_to_string phase);
             "elapsedMs", `Int (startup_elapsed_ms ws);
             "targetMs", `Int ws.startup_nav_target_ms;
           ])

let consume_startup_miss_event_json (ws:t) : Yojson.Safe.t option =
  update_startup_ready_state ws;
  let root_uri =
    match ws.root_path with
    | None -> None
    | Some p -> Some (`String (Uri_path.file_uri_of_path p))
  in
  match root_uri with
  | None -> None
  | Some root_uri_json ->
      if ws.startup_diag_hover_miss_notified
         && not ws.startup_diag_hover_miss_emitted
         && ws.startup_diag_hover_ready_ms = None
      then (
        ws.startup_diag_hover_miss_emitted <- true;
        Some
          (`Assoc
             [
               "rootUri", root_uri_json;
               "stage", `String "diagHoverReady";
               "phase", `String (startup_phase_to_string ws.startup_phase);
               "elapsedMs", `Int (startup_elapsed_ms ws);
               "targetMs", `Int ws.startup_diag_hover_target_ms;
             ])
      ) else if ws.startup_nav_miss_notified
                && not ws.startup_nav_miss_emitted
                && ws.startup_fully_nav_ready_ms = None
      then (
        ws.startup_nav_miss_emitted <- true;
        ws.startup_miss_emitted <- true;
        Some
          (`Assoc
             [
               "rootUri", root_uri_json;
               "stage", `String "fullyNavigable";
               "phase", `String (startup_phase_to_string ws.startup_phase);
               "elapsedMs", `Int (startup_elapsed_ms ws);
               "targetMs", `Int ws.startup_nav_target_ms;
             ])
      ) else
        None

let startup_background_budget_ms (ws:t) ~(base_budget_ms:int) : int =
  update_startup_ready_state ws;
  let base = max 1 base_budget_ms in
  if ws.startup_fully_nav_ready_ms <> None then base
  else
    match ws.startup_phase with
    | StartupAggressiveCatchUp ->
        max base (max ws.startup_aggressive_bg_budget_ms ws.bg_high_large_budget_ms)
    | StartupCold | StartupWarming | StartupReady ->
        max base ws.bg_high_large_budget_ms

let workspace_source_bytes_estimate (ws:t) : int =
  match ws.index with
  | None ->
      0
  | Some idx ->
      let source_count = Workspace_index.source_count idx in
      if ws.source_bytes_estimate_count = source_count then
        (match ws.source_bytes_estimate with
         | Some bytes -> max 0 bytes
         | None -> 0)
      else
        let bytes =
          Workspace_index.all_source_paths idx
          |> List.fold_left (fun acc path ->
               match file_size_bytes path with
               | Some n when n > 0 -> acc + n
               | _ -> acc)
               0
        in
        ws.source_bytes_estimate <- Some bytes;
        ws.source_bytes_estimate_count <- source_count;
        max 0 bytes

let workspace_profile_for_budget (ws:t) : workspace_profile =
  match ws.workspace_profile_mode with
  | ProfileModeSmall -> ProfileSmall
  | ProfileModeMedium -> ProfileMedium
  | ProfileModeLarge -> ProfileLarge
  | ProfileModeAuto ->
      let bytes = workspace_source_bytes_estimate ws in
      if bytes >= profile_medium_max_bytes then ProfileLarge
      else if bytes >= profile_small_max_bytes then ProfileMedium
      else ProfileSmall

let effective_bg_tick_budget_ms (ws:t) ~(base_budget_ms:int) : int =
  let base = max 1 base_budget_ms in
  match workspace_profile_for_budget ws with
  | ProfileSmall -> base
  | ProfileMedium -> max base 16
  | ProfileLarge -> max base 24

type semantic_validation_mode =
  | SemanticFull
  | SemanticRangeSemi of T.Range.t

let is_network_root (ws:t) : bool =
  match ws.root_path with
  | Some p -> is_probably_network_path p
  | None -> false

let allow_fallback_scan (ws:t) : bool =
  not (is_network_root ws)

let lookup_scan_budget (ws:t) : int * int =
  if is_network_root ws then
    (index_lookup_dirs_network, index_lookup_files_network)
  else
    (index_lookup_dirs, index_lookup_files)

let ensure_index_started (ws:t) : unit =
  match ws.root_path, ws.index with
  | Some root, None ->
      let idx = Workspace_index.start ~root in
      if Workspace_index.checkpoint_loaded idx then
        Perf_stats.tick "index.checkpoint_loaded";
      ws.index <- Some idx
  | _ ->
      ()

let pump_index (ws:t) ~(max_dirs:int) ~(max_files:int) : unit =
  ensure_index_started ws;
  match ws.index with
  | None -> ()
  | Some idx ->
      (try
         let was_reconciling = Workspace_index.reconcile_pending idx in
         let stale_before = Workspace_index.reconcile_stale_pruned idx in
         let _dirs, files = Workspace_index.scan_step idx ~max_dirs ~max_files in
         if files > 0 then ws.bg_seed_needs_refresh <- true;
         if was_reconciling && not (Workspace_index.reconcile_pending idx) then (
           Perf_stats.tick "index.reconcile_completed";
           if Workspace_index.reconcile_stale_pruned idx > stale_before then
             Perf_stats.tick "index.reconcile_pruned_stale";
           ws.bg_seed_needs_refresh <- true;
           enqueue_all_open_diag_revalidate ws ~reason:"reconcile"
         )
       with _ -> ())

let pump_index_background (ws:t) : unit =
  if is_network_root ws then
    pump_index ws ~max_dirs:index_background_dirs_network ~max_files:index_background_files_network
  else
    pump_index ws ~max_dirs:index_background_dirs ~max_files:index_background_files

let pump_index_lookup (ws:t) : unit =
  let max_dirs, max_files = lookup_scan_budget ws in
  pump_index ws ~max_dirs ~max_files

let index_checkpoint_loaded_for_report (ws:t) : bool =
  match ws.index with
  | None -> false
  | Some idx -> Workspace_index.checkpoint_loaded idx

let index_reconcile_pending_for_report (ws:t) : bool =
  match ws.index with
  | None -> false
  | Some idx -> Workspace_index.reconcile_pending idx

let index_reconcile_epoch_for_report (ws:t) : int =
  match ws.index with
  | None -> 0
  | Some idx -> Workspace_index.reconcile_epoch idx

let index_reconcile_sources_before_for_report (ws:t) : int =
  match ws.index with
  | None -> 0
  | Some idx -> Workspace_index.reconcile_sources_before idx

let index_reconcile_sources_after_for_report (ws:t) : int =
  match ws.index with
  | None -> 0
  | Some idx -> Workspace_index.reconcile_sources_after idx

let index_reconcile_stale_pruned_for_report (ws:t) : int =
  match ws.index with
  | None -> 0
  | Some idx -> Workspace_index.reconcile_stale_pruned idx

let is_import_word_char = function
  | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let tokenize_upper_words_prefix ~(max_chars:int) (text:string) : string list =
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
      scan !j
    ) else
      scan (i + 1)
  in
  scan 0;
  List.rev !out

let quick_imports_from_deferred_doc_text (doc:Document.t) : Preprocess.import list =
  let tokens =
    tokenize_upper_words_prefix
      ~max_chars:nav_miss_import_scan_max_chars
      (Document.text doc)
  in
  let seen = Hashtbl.create 16 in
  let out = ref [] in
  let loc = Ast.Loc.none in
  let push_name (raw:string) =
    let name = normalize_name raw in
    if name <> "" && not (Hashtbl.mem seen name) then (
      Hashtbl.replace seen name true;
      out :=
        { Preprocess.kind = Preprocess.Compool; name; loc } :: !out
    )
  in
  let rec collect = function
    | ("COMPOOL" | "ICOMPOOL") :: name :: tl ->
        push_name name;
        collect tl
    | _ :: tl ->
        collect tl
    | [] ->
        ()
  in
  collect tokens;
  List.rev !out

let best_effort_doc_imports_for_scheduling (doc:Document.t) : Preprocess.import list =
  let imports = Document.imports doc in
  if imports <> [] then
    imports
  else if doc.Document.parse_rev = doc.Document.rev then
    []
  else
    quick_imports_from_deferred_doc_text doc

let uniq_norm_strings (xs:string list) : string list =
  let seen = Hashtbl.create (max 16 (List.length xs)) in
  let out = ref [] in
  List.iter
    (fun x ->
      let k = normalize_name x in
      if k <> "" && not (Hashtbl.mem seen k) then (
        Hashtbl.replace seen k true;
        out := k :: !out
      ))
    xs;
  List.rev !out

let resolve_manual_root_file (ws:t) (raw:string) : string =
  if raw = "" then raw
  else if Filename.is_relative raw then
    match ws.root_path with
    | Some root -> Filename.concat root raw
    | None -> raw
  else
    raw

let graph_entry_hint_for_path (ws:t) ~(path:string) : bool =
  match ws.index with
  | None -> false
  | Some idx ->
      Workspace_index.source_entry_hint idx ~path

let graph_import_hints_for_path (ws:t) ~(path:string) ~(doc_opt:Document.t option) : string list =
  match doc_opt with
  | Some doc ->
      best_effort_doc_imports_for_scheduling doc
      |> List.filter_map (fun (imp:Preprocess.import) ->
           match imp.kind with
           | Preprocess.Compool ->
               let key = normalize_name imp.name in
               if key = "" then None else Some key)
      |> uniq_norm_strings
  | None ->
      (match ws.index with
       | None -> []
       | Some idx -> Workspace_index.source_import_hints idx ~path)

let graph_parse_quality_for_path (ws:t) ~(path_key:string) ~(doc_opt:Document.t option) : parse_quality =
  match doc_opt with
  | Some doc when doc.Document.parse_rev = doc.Document.rev -> ParseQualityFull
  | _ ->
      if Hashtbl.mem ws.quick_nav_done_set path_key || Hashtbl.mem ws.bg_parsed path_key then
        ParseQualitySkeleton
      else
        ParseQualityNone

let graph_file_class_for_path (ws:t) ~(path:string) ~(doc_opt:Document.t option) : file_class =
  match doc_opt with
  | Some _ -> FileClassOpen
  | None ->
      if graph_entry_hint_for_path ws ~path then FileClassEntry
      else FileClassNormal

let graph_queue_empty (ws:t) : bool =
  Queue.is_empty ws.bg_root_small_queue
  && Queue.is_empty ws.bg_root_large_queue

let graph_closure_seed_complete (ws:t) : bool =
  ws.graph_root_closure_cursor >= Array.length ws.graph_root_closure_paths

let graph_path_of_key (ws:t) (path_key:string) : string option =
  match Hashtbl.find_opt ws.graph_nodes path_key with
  | Some node -> Some node.gn_path
  | None -> None

let graph_neighbors_for_key
    (ws:t)
    ~(closure:(string, bool) Hashtbl.t)
    (path_key:string)
  : string list =
  match Hashtbl.find_opt ws.graph_nodes path_key with
  | None -> []
  | Some node ->
      let out = ref [] in
      let seen = Hashtbl.create 16 in
      let push_key (k:string) =
        if k <> "" && Hashtbl.mem closure k && not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := k :: !out
        )
      in
      List.iter (fun p -> push_key (normalize_path_key p)) node.gn_import_paths;
      List.iter push_key node.gn_rev_importers;
      List.rev !out

let graph_refresh (ws:t) : unit =
  if not ws.graph_needs_refresh then ()
  else (
    ws.graph_epoch <- ws.graph_epoch + 1;
    Hashtbl.clear ws.graph_nodes;
    Hashtbl.clear ws.graph_root_reason;
    Hashtbl.clear ws.graph_root_closure_set;
    ws.graph_root_closure_paths <- [||];
    ws.graph_root_closure_cursor <- 0;
    ws.graph_scc_count <- 0;

    let path_seen = Hashtbl.create 8192 in
    let add_path (acc:string list ref) (path:string) =
      let key = normalize_path_key path in
      if key <> "" && not (Hashtbl.mem path_seen key) then (
        Hashtbl.replace path_seen key true;
        acc := path :: !acc
      )
    in
    let candidates = ref [] in
    (match ws.index with
     | None -> ()
     | Some idx -> Workspace_index.all_source_paths idx |> List.iter (add_path candidates));
    Hashtbl.iter
      (fun _uri doc ->
        match doc.Document.file with
        | Some path -> add_path candidates path
        | None -> ())
      ws.docs;
    ws.root_manual_files
    |> List.iter (fun raw ->
         let path = resolve_manual_root_file ws raw in
         if path <> "" then add_path candidates path);
    let paths = List.rev !candidates in

    List.iter
      (fun path ->
        let path_key = normalize_path_key path in
        if path_key <> "" then (
          let doc_opt =
            match find_open_doc_for_path ws ~path with
            | Some d -> Some d
            | None -> Hashtbl.find_opt ws.files path_key
          in
          let import_compools = graph_import_hints_for_path ws ~path ~doc_opt in
          let import_paths =
            import_compools
            |> List.filter_map (fun compool ->
                 match ws.index with
                 | Some idx -> Workspace_index.find_compool idx ~name:compool
                 | None -> None)
            |> List.sort_uniq String.compare
          in
          let node =
            {
              gn_path = path;
              gn_path_key = path_key;
              gn_import_compools = import_compools;
              gn_import_paths = import_paths;
              gn_rev_importers = [];
              gn_file_class = graph_file_class_for_path ws ~path ~doc_opt;
              gn_size_class = size_class_of_path ws path;
              gn_parse_quality = graph_parse_quality_for_path ws ~path_key ~doc_opt;
              gn_epoch = ws.graph_epoch;
            }
          in
          Hashtbl.replace ws.graph_nodes path_key node))
      paths;

    let reverse_sets : (string, (string, bool) Hashtbl.t) Hashtbl.t = Hashtbl.create 1024 in
    Hashtbl.iter
      (fun importer_key node ->
        node.gn_import_paths
        |> List.iter (fun provider_path ->
             let provider_key = normalize_path_key provider_path in
             if provider_key <> "" then
               let set =
                 match Hashtbl.find_opt reverse_sets provider_key with
                 | Some s -> s
                 | None ->
                     let s = Hashtbl.create 8 in
                     Hashtbl.replace reverse_sets provider_key s;
                     s
               in
               Hashtbl.replace set importer_key true))
      ws.graph_nodes;
    Hashtbl.iter
      (fun path_key node ->
        let revs =
          match Hashtbl.find_opt reverse_sets path_key with
          | None -> []
          | Some set -> Hashtbl.fold (fun k _ acc -> k :: acc) set []
        in
        node.gn_rev_importers <- revs)
      ws.graph_nodes;

    let open_roots =
      Hashtbl.fold
        (fun path_key node acc ->
          if node.gn_file_class = FileClassOpen then path_key :: acc else acc)
        ws.graph_nodes
        []
    in
    let entry_roots =
      Hashtbl.fold
        (fun path_key node acc ->
          if node.gn_file_class = FileClassEntry then path_key :: acc else acc)
        ws.graph_nodes
        []
    in
    let heuristic_roots =
      Hashtbl.fold
        (fun path_key node acc ->
          if is_main_boot_heuristic node.gn_path then path_key :: acc else acc)
        ws.graph_nodes
        []
    in
    let manual_roots =
      ws.root_manual_files
      |> List.filter_map (fun raw ->
           let path = resolve_manual_root_file ws raw in
           let key = normalize_path_key path in
           if key <> "" && Hashtbl.mem ws.graph_nodes key then Some key else None)
    in

    let roots =
      match ws.root_model with
      | RootModelManual ->
          if manual_roots <> [] then open_roots @ manual_roots
          else open_roots @ entry_roots @ heuristic_roots
      | RootModelHeuristic ->
          open_roots @ heuristic_roots
      | RootModelAuto ->
          if entry_roots <> [] then open_roots @ entry_roots
          else if ws.root_heuristic_fallback then open_roots @ heuristic_roots
          else open_roots
    in
    let roots =
      roots
      |> List.sort_uniq String.compare
      |> List.sort (fun a b ->
           match Hashtbl.find_opt ws.graph_nodes a, Hashtbl.find_opt ws.graph_nodes b with
           | Some na, Some nb ->
               compare (file_class_rank na.gn_file_class) (file_class_rank nb.gn_file_class)
           | _ -> String.compare a b)
    in
    List.iter
      (fun root_key ->
        let reason =
          if List.mem root_key open_roots then "open"
          else if List.mem root_key entry_roots then "entry"
          else if List.mem root_key manual_roots then "manual"
          else if List.mem root_key heuristic_roots then "heuristic"
          else "fallback"
        in
        Hashtbl.replace ws.graph_root_reason root_key reason)
      roots;

    let closure = Hashtbl.create (max 256 (ws.root_closure_target_files * 2)) in
    let q : (string * int) Queue.t = Queue.create () in
    roots |> List.iter (fun key -> Queue.add (key, 0) q);
    while not (Queue.is_empty q)
          && Hashtbl.length closure < ws.root_closure_target_files
    do
      let key, depth = Queue.pop q in
      if key <> "" && not (Hashtbl.mem closure key) then (
        Hashtbl.replace closure key true;
        if depth < ws.root_closure_max_depth then
          match Hashtbl.find_opt ws.graph_nodes key with
          | None -> ()
          | Some node ->
              node.gn_import_paths
              |> List.iter (fun p ->
                   let k = normalize_path_key p in
                   if k <> "" && Hashtbl.mem ws.graph_nodes k then
                     Queue.add (k, depth + 1) q);
              node.gn_rev_importers
              |> List.iter (fun k ->
                   if k <> "" && Hashtbl.mem ws.graph_nodes k then
                     Queue.add (k, depth + 1) q)
      )
    done;
    Hashtbl.iter (fun key _ -> Hashtbl.replace ws.graph_root_closure_set key true) closure;

    let closure_paths =
      Hashtbl.fold
        (fun key _ acc ->
          match graph_path_of_key ws key with
          | Some path -> path :: acc
          | None -> acc)
        closure
        []
      |> List.sort String.compare
      |> Array.of_list
    in
    ws.graph_root_closure_paths <- closure_paths;
    ws.graph_root_closure_cursor <- 0;

    let index_tbl : (string, int) Hashtbl.t = Hashtbl.create 1024 in
    let low_tbl : (string, int) Hashtbl.t = Hashtbl.create 1024 in
    let on_stack : (string, bool) Hashtbl.t = Hashtbl.create 1024 in
    let stack : string Stack.t = Stack.create () in
    let next_index = ref 0 in
    let scc_count = ref 0 in
    let rec strongconnect (v:string) : unit =
      let idx = !next_index in
      incr next_index;
      Hashtbl.replace index_tbl v idx;
      Hashtbl.replace low_tbl v idx;
      Stack.push v stack;
      Hashtbl.replace on_stack v true;
      graph_neighbors_for_key ws ~closure v
      |> List.iter (fun w ->
           if not (Hashtbl.mem index_tbl w) then (
             strongconnect w;
             let low_v = Option.value (Hashtbl.find_opt low_tbl v) ~default:idx in
             let low_w = Option.value (Hashtbl.find_opt low_tbl w) ~default:idx in
             Hashtbl.replace low_tbl v (min low_v low_w)
           ) else if Hashtbl.mem on_stack w then (
             let low_v = Option.value (Hashtbl.find_opt low_tbl v) ~default:idx in
             let idx_w = Option.value (Hashtbl.find_opt index_tbl w) ~default:idx in
             Hashtbl.replace low_tbl v (min low_v idx_w)
           ));
      let low_v = Option.value (Hashtbl.find_opt low_tbl v) ~default:idx in
      let idx_v = Option.value (Hashtbl.find_opt index_tbl v) ~default:idx in
      if low_v = idx_v then (
        incr scc_count;
        let continue = ref true in
        while !continue && not (Stack.is_empty stack) do
          let w = Stack.pop stack in
          Hashtbl.remove on_stack w;
          if w = v then continue := false
        done
      )
    in
    Hashtbl.iter
      (fun key _ ->
        if not (Hashtbl.mem index_tbl key) then strongconnect key)
      closure;
    ws.graph_scc_count <- !scc_count;
    ws.graph_needs_refresh <- false;
    Perf_stats.tick "graph.refreshed";
    Perf_stats.observe_ms "graph.root_candidates" (float_of_int (List.length roots));
    Perf_stats.observe_ms "graph.root_closure_size" (float_of_int (Array.length ws.graph_root_closure_paths));
    Perf_stats.observe_ms "graph.scc_count" (float_of_int ws.graph_scc_count)
  )

let ensure_graph_fresh (ws:t) : unit =
  if ws.graph_needs_refresh then
    graph_refresh ws

let maybe_escalate_index_reconcile
    ?(reason:string="unknown")
    ?(has_imports_override:bool option)
    (ws:t)
    ~(doc:Document.t option)
  : bool =
  let has_imports =
    match has_imports_override, doc with
    | Some b, _ -> b
    | None, None -> false
    | None, Some d ->
        best_effort_doc_imports_for_scheduling d <> []
  in
  match ws.index with
  | None -> false
  | Some idx ->
      let compools = Workspace_index.compool_count idx in
      let sources = Workspace_index.source_count idx in
      if (not has_imports) || compools > 0 || sources <= 0 then false
      else
        let now = Perf_stats.now_ms () in
        if now -. ws.index_reconcile_escalate_last_ms < float_of_int index_stale_reconcile_min_interval_ms then
          false
        else (
          ws.index_reconcile_escalate_last_ms <- now;
          ws.index_reconcile_escalations <- ws.index_reconcile_escalations + 1;
          if Workspace_index.force_reconcile idx then
            Perf_stats.tick "index.reconcile_started";
          ws.bg_seed_needs_refresh <- true;
          pump_index
            ws
            ~max_dirs:index_reconcile_escalate_dirs
            ~max_files:index_reconcile_escalate_files;
          ignore reason;
          true
        )

let schedule_nav_miss_reconcile
    (ws:t)
    ~(doc:Document.t)
    ~(symbol_key:string)
  : unit =
  let key = normalize_name symbol_key in
  if key = "" then ()
  else (
    Perf_stats.tick "nav.miss_trigger_reconcile";
    let imports = best_effort_doc_imports_for_scheduling doc in
    let profile = workspace_profile_for_budget ws in
    let nav_miss_high_cap =
      match profile with
      | ProfileLarge -> max nav_miss_high_enqueue_cap 64
      | ProfileMedium -> max nav_miss_high_enqueue_cap 36
      | ProfileSmall -> nav_miss_high_enqueue_cap
    in
    let quick_scan_files_budget =
      match profile with
      | ProfileLarge -> max nav_quick_scan_files 128
      | ProfileMedium -> max nav_quick_scan_files 72
      | ProfileSmall -> nav_quick_scan_files
    in
    let quick_scan_total_budget =
      match profile with
      | ProfileLarge -> max nav_quick_scan_total_bytes 4_194_304
      | ProfileMedium -> max nav_quick_scan_total_bytes 2_359_296
      | ProfileSmall -> nav_quick_scan_total_bytes
    in
    let high_budget =
      if ws.startup_diag_hover_ready_ms <> None then max_int
      else max 0 nav_miss_high_cap
    in
    let high_used = ref 0 in
    let scheduled_paths : (string, bool) Hashtbl.t = Hashtbl.create 16 in
    let enqueue_path_once (path:string) : unit =
      let path_key = normalize_path_key path in
      if path_key <> "" && not (Hashtbl.mem scheduled_paths path_key) then (
        Hashtbl.replace scheduled_paths path_key true;
        let use_high =
          if !high_used < high_budget then (
            incr high_used;
            true
          ) else
            false
        in
        let lane = if use_high then LaneOpen else LaneRoot in
        enqueue_bg_path ws ~lane ~reason_group:"nav_miss" ~high:use_high path
      )
    in
    let enqueue_compool_path (name:string) : unit =
      match ws.index with
      | None -> ()
      | Some idx ->
          (match Workspace_index.find_compool idx ~name with
           | None -> ()
           | Some p -> enqueue_path_once p)
    in
    (match Hashtbl.find_opt ws.quick_nav_index key with
     | None -> ()
     | Some entries ->
         List.iter
           (fun (e:quick_nav_entry) ->
             match Uri_path.file_path_of_uri e.qn_uri with
             | Some p -> enqueue_path_once p
             | None -> ())
           entries);
    imports
    |> List.iter (fun (imp:Preprocess.import) ->
         enqueue_compool_path imp.name);
    (match ws.symbol_hints with
     | None -> ()
     | Some (values, types) ->
         let add_candidates (tbl:(string, string list) Hashtbl.t) =
           match Hashtbl.find_opt tbl key with
           | None -> ()
           | Some compools ->
               List.iter enqueue_compool_path compools
         in
         add_candidates values;
         add_candidates types);
    if Hashtbl.length scheduled_paths = 0 then (
      let pat = "PROC " ^ key in
      let contains_pat (s:string) : bool =
        let n = String.length s in
        let m = String.length pat in
        let rec loop i =
          if i + m > n then false
          else if String.sub s i m = pat then true
          else loop (i + 1)
        in
        m > 0 && loop 0
      in
      let read_prefix (path:string) ~(max_bytes:int) : string option =
        try
          let ic = open_in_bin path in
          Fun.protect
            ~finally:(fun () -> close_in_noerr ic)
            (fun () ->
              let size =
                try in_channel_length ic with _ -> max_bytes
              in
              let n = max 0 (min max_bytes size) in
              Some (really_input_string ic n))
        with _ ->
          None
      in
      match ws.index with
      | None -> ()
      | Some idx ->
          let rec scan scanned scanned_bytes = function
            | [] -> ()
            | _ when scanned >= quick_scan_files_budget -> ()
            | _ when scanned_bytes >= quick_scan_total_budget -> ()
            | path :: tl ->
                (match read_prefix path ~max_bytes:nav_quick_scan_per_file_bytes with
                 | None ->
                     scan (scanned + 1) scanned_bytes tl
                 | Some text ->
                     let bytes = String.length text in
                     let next_bytes = scanned_bytes + bytes in
                     if next_bytes > quick_scan_total_budget then ()
                     else (
                       let upper = String.uppercase_ascii text in
                       if contains_pat upper then enqueue_path_once path;
                       scan (scanned + 1) next_bytes tl))
          in
          scan 0 0 (Workspace_index.all_source_paths idx);
          if Hashtbl.length scheduled_paths = 0 then (
            ensure_graph_fresh ws;
            let rec enqueue_closure i remaining =
              if remaining <= 0 || i >= Array.length ws.graph_root_closure_paths then ()
              else (
                enqueue_path_once ws.graph_root_closure_paths.(i);
                enqueue_closure (i + 1) (remaining - 1)
              )
            in
            enqueue_closure 0 (min 48 (Array.length ws.graph_root_closure_paths))
          )
    );
    ignore
      (maybe_escalate_index_reconcile
         ws
         ~doc:(Some doc)
         ~reason:"nav_miss"
         ~has_imports_override:(imports <> []))
  )

let invalidate_symbol_hints (ws:t) : unit =
  ws.symbol_hints <- None

let rescan (ws:t) : unit =
  startup_mark_started ws;
  mark_graph_dirty ws;
  invalidate_lsif_snapshot ws;
  ws.parse_epoch <- ws.parse_epoch + 1;
  ws.lsif_snapshot_revision <- 0;
  Hashtbl.clear ws.files;
  Hashtbl.clear ws.nav_response_cache;
  Hashtbl.clear ws.bg_enqueued;
  Hashtbl.clear ws.bg_parsed;
  Hashtbl.clear ws.closed_doc_last_touch;
  Hashtbl.clear ws.bg_closed_diags;
  Hashtbl.clear ws.bg_pending_diag_payloads;
  Hashtbl.clear ws.bg_pending_diag_set;
  Hashtbl.clear ws.open_parse_generation;
  Hashtbl.clear ws.open_provisional_since_ms;
  Hashtbl.clear ws.quick_nav_index;
  Hashtbl.clear ws.quick_nav_pending_set;
  Hashtbl.clear ws.quick_nav_done_set;
  Hashtbl.clear ws.nav_quick_scan_offset_by_path;
  ws.quick_nav_index_done <- 0;
  ws.quick_nav_index_total <- 0;
  Hashtbl.clear ws.parse_worker_inflight;
  while not (Queue.is_empty ws.bg_high_small_queue) do ignore (Queue.pop ws.bg_high_small_queue) done;
  while not (Queue.is_empty ws.bg_norm_small_queue) do ignore (Queue.pop ws.bg_norm_small_queue) done;
  while not (Queue.is_empty ws.bg_high_large_queue) do ignore (Queue.pop ws.bg_high_large_queue) done;
  while not (Queue.is_empty ws.bg_norm_large_queue) do ignore (Queue.pop ws.bg_norm_large_queue) done;
  while not (Queue.is_empty ws.quick_nav_pending_paths) do ignore (Queue.pop ws.quick_nav_pending_paths) done;
  while not (Queue.is_empty ws.parse_worker_jobs) do ignore (Queue.pop ws.parse_worker_jobs) done;
  while not (Queue.is_empty ws.parse_worker_results) do ignore (Queue.pop ws.parse_worker_results) done;
  while not (Queue.is_empty ws.bg_pending_diag_updates) do ignore (Queue.pop ws.bg_pending_diag_updates) done;
  ws.bg_seed_paths <- [||];
  ws.bg_seed_cursor <- 0;
  ws.bg_seed_needs_refresh <- true;
  invalidate_symbol_hints ws;
  if ws.sem_store_enabled then Semantic_store.reset ws.semantic_store;
  if ws.lsif_delta_enabled then Lsif_delta.reset ws.lsif_delta_state;
  match ws.root_path with
  | None -> ws.index <- None
   | Some root ->
       let idx = Workspace_index.start ~root in
       ws.index <- Some idx;
       let max_dirs, max_files =
         startup_scan_budget_for_root
           ~network:(is_probably_network_path root)
       in
       (try
          ignore
            (Workspace_index.scan_step idx
              ~max_dirs
              ~max_files)
        with _ -> ())

let compool_count (ws:t) : int =
  pump_index_background ws;
  match ws.index with None -> 0 | Some idx -> Workspace_index.compool_count idx

let diag_missing_compool (loc:Ast.Loc.t) (name:string) : T.Diagnostic.t =
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Error
    ~source:"import"
    ~message:("Missing COMPOOL: " ^ name)
    loc

let has_known_source_ext_name (name:string) : bool =
  let lower = String.lowercase_ascii name in
  let ends_with ext =
    let n = String.length lower in
    let m = String.length ext in
    n >= m && String.sub lower (n - m) m = ext
  in
  ends_with ".jov" || ends_with ".j73" || ends_with ".jvl" || ends_with ".j"

let source_stem_of_filename (name:string) : string option =
  if not (has_known_source_ext_name name) then None
  else
    let n = String.length name in
    let rec find_dot i =
      if i < 0 then None
      else if name.[i] = '.' then Some i
      else find_dot (i - 1)
    in
    match find_dot (n - 1) with
    | None -> None
    | Some i when i <= 0 -> None
    | Some i -> Some (String.sub name 0 i)

let is_ignored_lookup_dir (name:string) : bool =
  name = ".git" || name = "_build" || name = "node_modules" || name = ".vscode"

let find_compool_in_dir_tree ~(key:string) ~(root:string) : string option =
  let rec walk (dir:string) : string option =
    let entries =
      try Sys.readdir dir |> Array.to_list
      with _ -> []
    in
    let rec loop = function
      | [] -> None
      | name :: tl ->
          let full = Filename.concat dir name in
          (try
             if Sys.is_directory full then
               if is_ignored_lookup_dir name then loop tl
               else
                 (match walk full with
                  | Some _ as hit -> hit
                  | None -> loop tl)
             else
               match source_stem_of_filename name with
               | Some stem when normalize_name stem = key -> Some full
               | _ -> loop tl
           with _ -> loop tl)
    in
    loop entries
  in
  walk root

let find_compool_path_fallback (ws:t) ~(key:string) : string option =
  if key = "" then None
  else
    let dirs = Hashtbl.create 16 in
    let add_dir (d:string) =
      let k = normalize_path_key d in
      if k <> "" then Hashtbl.replace dirs k d
    in
    (match ws.root_path with
     | None -> ()
     | Some root -> add_dir root);
    Hashtbl.iter (fun _ doc ->
      match doc.Document.file with
      | None -> ()
      | Some p -> add_dir (Filename.dirname p)
    ) ws.docs;
    let roots = Hashtbl.fold (fun _ d acc -> d :: acc) dirs [] in
    let rec loop = function
      | [] -> None
      | root :: tl ->
          (match find_compool_in_dir_tree ~key ~root with
           | Some _ as hit -> hit
           | None -> loop tl)
    in
    loop roots

let find_open_compool_doc_by_key (ws:t) (key:string) : Document.t option =
  let found = ref None in
  Hashtbl.iter (fun _ doc ->
    match !found, doc.Document.compool_def with
    | Some _, _ -> ()
    | None, Some nm when normalize_name nm = key -> found := Some doc
    | None, _ -> ()
  ) ws.docs;
  !found

let has_compool_target (ws:t) (name:string) : bool =
  let key = normalize_name name in
  match find_open_compool_doc_by_key ws key with
  | Some _ -> true
  | None ->
      (match ws.index with
       | Some idx ->
           (match Workspace_index.find_compool idx ~name:key with
            | Some _ -> true
            | None ->
                if allow_fallback_scan ws then
                  (match find_compool_path_fallback ws ~key with
                   | Some _ -> true
                   | None -> false)
                else
                  false)
       | None ->
           (match if allow_fallback_scan ws then find_compool_path_fallback ws ~key else None with
            | Some _ -> true
            | None -> false))

type compool_import_dir = {
  compool : string;
  selected : (string * Ast.Loc.t) list;  (* imported element name + location *)
}

let diag_missing_imported_type ~(loc:Ast.Loc.t) ~(item:string) ~(typ:string) : T.Diagnostic.t =
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Error
    ~source:"import"
    ~message:(Printf.sprintf "Imported item %S requires explicit import of type %S." item typ)
    loc

let diag_missing_import_hint ~(loc:Ast.Loc.t) ~(kind:string) ~(symbol:string) ~(compools:string list) : T.Diagnostic.t =
  let targets =
    match compools with
    | [] -> ""
    | [c] -> c
    | xs -> String.concat ", " xs
  in
  let msg =
    if targets = "" then
      Printf.sprintf "%s %S may require a COMPOOL import." kind symbol
    else
      Printf.sprintf
        "%s %S is available in COMPOOL %s. Import it with !COMPOOL '%s' (or selective import)."
        kind
        symbol
        targets
        (List.hd compools)
  in
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Warning
    ~source:"import"
    ~message:msg
    loc

let is_builtin_type (k:string) : bool =
  match k with
  | "A" | "B" | "U" | "S" | "F" | "C" | "P" | "W" | "V" | "STATUS" -> true
  | _ -> false

let is_builtin_function_name (name:string) : bool =
  match normalize_name name with
  | "LOC" | "NEXT" | "BIT" | "BYTE"
  | "SHIFTL" | "SHIFTR" | "ABS" | "SGN"
  | "BITSIZE" | "BYTESIZE" | "WORDSIZE"
  | "LBOUND" | "UBOUND" | "NWDSEN"
  | "FIRST" | "LAST" | "REP" | "V" ->
      true
  | _ ->
      false

let is_control_stmt_keyword (name:string) : bool =
  match normalize_name name with
  | "EXIT" | "ABORT" | "STOP" -> true
  | _ -> false

let is_reserved_keyword (name:string) : bool =
  match normalize_name name with
  | "START" | "TERM" | "BEGIN" | "END"
  | "DEF" | "REF" | "STATIC" | "CONSTANT" | "PROC" | "ITEM" | "TABLE" | "TYPE"
  | "IF" | "THEN" | "ELSE" | "WHILE" | "FOR" | "BY"
  | "CASE" | "DEFAULT" | "FALLTHRU"
  | "EXIT" | "GOTO" | "RETURN" | "ABORT" | "STOP"
  | "TRUE" | "FALSE"
  | "NOT" | "AND" | "OR" | "XOR" | "EQV" | "MOD"
  | "PROGRAM" | "COMPOOL" | "ICOMPOOL" | "DEFINE" | "BLOCK"
  | "ICOPY" | "ISKIP" | "IBEGIN" | "IEND" | "ILINKAGE" | "ITRACE"
  | "IINTERFERENCE" | "IREDUCIBLE" | "ILIST" | "INOLIST" | "IEJECT"
  | "IBASE" | "IISBASE" | "IDROP" | "ILEFTRIGHT" | "IREARRANGE"
  | "IINITIALIZE" | "IORDER"
  | "REC" | "RENT"
  | "LISTEXP" | "LISTINV" | "LISTBOTH"
  | "INLINE" | "INSTANCE" | "LABEL" | "LIKE"
  | "OVERLAY" | "PARALLEL" | "POS" | "NULL" ->
      true
  | x when is_builtin_function_name x ->
      true
  | _ ->
      false

let extract_compool_import_dirs (doc:Document.t) : compool_import_dir list =
  match doc.Document.ast with
  | None -> []
  | Some prog ->
      let from_decl (d:Ast.decl Ast.node) : compool_import_dir option =
        match d.v with
        | Ast.DDirective { name; args = first :: rest } ->
            let dn = normalize_name name.v in
            if dn = "COMPOOL" || dn = "ICOMPOOL" then
              let compool = normalize_name first.v in
              if compool = "" then None
              else
                let selected =
                  rest
                  |> List.filter_map (fun arg ->
                       let k = normalize_name arg.v in
                       if k = "" then None else Some (k, arg.loc))
                in
                Some { compool; selected }
            else
              None
        | _ -> None
      in
      prog
      |> List.filter_map (function
           | Ast.TopDecl d -> from_decl d
           | Ast.TopStmt _ -> None)

type dep_info = {
  types : (string, bool) Hashtbl.t;            (* explicit type declarations *)
  item_deps : (string, string list) Hashtbl.t; (* item/table name -> required type keys *)
}

let dep_info_create () : dep_info =
  { types = Hashtbl.create 64; item_deps = Hashtbl.create 128 }

let rec type_keys_of_type_expr (t:Ast.type_expr Ast.node) (acc:(string, bool) Hashtbl.t) : unit =
  match t.v with
  | Ast.TName id ->
      let k = normalize_name id.v in
      if k <> "" && not (is_builtin_type k) then Hashtbl.replace acc k true
  | Ast.TPointer inner ->
      type_keys_of_type_expr inner acc
  | Ast.TArray { elem; _ } ->
      type_keys_of_type_expr elem acc
  | Ast.TRecord fields ->
      List.iter (fun f -> type_keys_of_type_expr f.v.ftype acc) fields
  | Ast.TFunc { params; returns } ->
      List.iter (fun p -> type_keys_of_type_expr p.v.Ast.ptype acc) params;
      (match returns with None -> () | Some r -> type_keys_of_type_expr r acc)

let keys_of_type_expr (t:Ast.type_expr Ast.node) : string list =
  let h = Hashtbl.create 8 in
  type_keys_of_type_expr t h;
  Hashtbl.fold (fun k _ xs -> k :: xs) h []

let rec dep_info_add_stmt (info:dep_info) (s:Ast.stmt Ast.node) : unit =
  match s.v with
  | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _ | Ast.SGoto _ -> ()
  | Ast.SDecl d -> dep_info_add_decl info d
  | Ast.SBlock xs -> List.iter (dep_info_add_stmt info) xs
  | Ast.SIf { then_; else_; _ } ->
      dep_info_add_stmt info then_;
      (match else_ with None -> () | Some e -> dep_info_add_stmt info e)
  | Ast.SWhile { body; _ } -> dep_info_add_stmt info body
  | Ast.SFor { init; step; body; _ } ->
      (match init with None -> () | Some i -> dep_info_add_stmt info i);
      (match step with None -> () | Some st -> dep_info_add_stmt info st);
      dep_info_add_stmt info body
  | Ast.SLabel { body; _ } -> dep_info_add_stmt info body

and dep_info_add_decl (info:dep_info) (d:Ast.decl Ast.node) : unit =
  match d.v with
  | Ast.DType { name; defn = _ } ->
      let k = normalize_name name.v in
      if k <> "" then Hashtbl.replace info.types k true
  | Ast.DVar { name; dtype; _ } ->
      let n = normalize_name name.v in
      if n <> "" then Hashtbl.replace info.item_deps n (keys_of_type_expr dtype)
  | Ast.DConst _ -> ()
  | Ast.DDirective _ -> ()
  | Ast.DProc p ->
      List.iter (dep_info_add_decl info) p.v.locals;
      dep_info_add_stmt info p.v.body

let dep_info_of_doc (doc:Document.t) : dep_info =
  let info = dep_info_create () in
  (match doc.Document.ast with
   | None -> ()
   | Some prog ->
       List.iter (function
         | Ast.TopDecl d -> dep_info_add_decl info d
         | Ast.TopStmt s -> dep_info_add_stmt info s
       ) prog);
  info

let read_file_text (path:string) : string option =
  try
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let txt = really_input_string ic len in
    close_in_noerr ic;
    Some txt
  with _ -> None

let read_file_prefix_text (path:string) ~(max_bytes:int) : string option =
  if max_bytes <= 0 then None
  else
    try
      let ic = open_in_bin path in
      let len = in_channel_length ic in
      let take = min len max_bytes in
      let txt = really_input_string ic take in
      close_in_noerr ic;
      Some txt
    with _ -> None

let read_file_window_text
    (path:string)
    ~(offset:int)
    ~(max_bytes:int)
  : (string * int) option =
  if max_bytes <= 0 then None
  else
    try
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let len = in_channel_length ic in
          let start = max 0 (min offset len) in
          seek_in ic start;
          let take = min max_bytes (len - start) in
          if take <= 0 then
            Some ("", 0)
          else
            let txt = really_input_string ic take in
            let next =
              if start + take >= len then 0
              else start + take
            in
            Some (txt, next))
    with _ ->
      None

let doc_from_path_cached_only (ws:t) (path:string) : Document.t option =
  let key = normalize_path_key path in
  match Hashtbl.find_opt ws.files key with
  | Some d ->
      touch_closed_doc_path ws ~path_key:key;
      Some d
  | None ->
      (match find_open_doc_for_path ws ~path with
       | Some d ->
           Hashtbl.replace ws.files key d;
           touch_closed_doc_path ws ~path_key:key;
           evict_closed_docs_if_needed ws;
           Some d
       | None ->
           None)

let doc_from_path_cached (ws:t) (path:string) : Document.t option =
  match doc_from_path_cached_only ws path with
  | Some d -> Some d
  | None ->
      let key = normalize_path_key path in
      let uri =
        match Uri_path.docuri_of_path path with
        | Some u -> u
        | None ->
            (match T.DocumentUri.t_of_yojson (`String (Uri_path.file_uri_of_path path)) with
             | u -> u
             | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))
      in
      let d_opt =
        match file_size_bytes path with
        | Some n when is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes ~text_len:n ->
            Some (make_doc_with_parse_guard ws ~uri ~file:(Some path) ~text:"" ~actual_bytes:n)
        | _ ->
            (match read_file_text path with
             | None -> None
             | Some txt ->
                 let d =
                   try parse_guarded_document_make ws ~uri ~file:(Some path) ~text:txt
                   with exn ->
                     ignore exn;
                     Document.make ~uri ~file:(Some path) ~text:""
                 in
                 Some d)
      in
      (match d_opt with
       | None -> None
       | Some d ->
           Hashtbl.replace ws.files key d;
           touch_closed_doc_path ws ~path_key:key;
           evict_closed_docs_if_needed ws;
           Some d)

let resolve_compool_doc_uncached (ws:t) ~(name:string) : Document.t option =
  let key = normalize_name name in
  match find_open_compool_doc_by_key ws key with
  | Some d -> Some d
  | None ->
      let path_opt =
        match ws.index with
        | Some idx ->
            (match Workspace_index.find_compool idx ~name:key with
             | Some p -> Some p
             | None ->
                 if allow_fallback_scan ws then
                   find_compool_path_fallback ws ~key
                 else
                   None)
        | None ->
            if allow_fallback_scan ws then find_compool_path_fallback ws ~key else None
      in
      (match path_opt with
       | None -> None
       | Some path -> doc_from_path_cached ws path)

let validate_imports ?(pump_lookup:bool=true) (ws:t) (doc:Document.t) : T.Diagnostic.t list =
  let pre_imports = Document.imports doc in
  let has_compool_import = pre_imports <> [] in
  if has_compool_import && pump_lookup then pump_index_lookup ws;
  let missing_compools =
    pre_imports
    |> List.filter_map (fun (imp:Preprocess.import) ->
         match imp.kind with
         | Preprocess.Compool ->
             if has_compool_target ws imp.name then None
             else Some (diag_missing_compool imp.loc imp.name))
  in
  let imports = extract_compool_import_dirs doc in
  let missing_type_imports =
    if imports = [] then []
    else
      let doc_cache : (string, Document.t option) Hashtbl.t = Hashtbl.create 16 in
      let info_cache : (string, dep_info option) Hashtbl.t = Hashtbl.create 16 in

      let get_doc_for_compool (name:string) : Document.t option =
        let key = normalize_name name in
        match Hashtbl.find_opt doc_cache key with
        | Some x -> x
        | None ->
            let x = resolve_compool_doc_uncached ws ~name:key in
            Hashtbl.replace doc_cache key x;
            x
      in

      let get_info_for_compool (name:string) : dep_info option =
        let key = normalize_name name in
        match Hashtbl.find_opt info_cache key with
        | Some x -> x
        | None ->
            let x =
              match get_doc_for_compool key with
              | None -> None
              | Some d -> Some (dep_info_of_doc d)
            in
            Hashtbl.replace info_cache key x;
            x
      in

      let available_types : (string, bool) Hashtbl.t = Hashtbl.create 64 in
      let add_available_type k =
        if k <> "" then Hashtbl.replace available_types (normalize_name k) true
      in

      let self_info = dep_info_of_doc doc in
      Hashtbl.iter (fun tk _ -> add_available_type tk) self_info.types;

      (* Pass 1: collect explicitly imported type names. *)
      List.iter (fun imp ->
        match get_info_for_compool imp.compool with
        | None -> ()
        | Some info ->
            if imp.selected = [] then
              Hashtbl.iter (fun tk _ -> add_available_type tk) info.types
            else
              List.iter (fun (nm, _loc) ->
                if Hashtbl.mem info.types nm then add_available_type nm
              ) imp.selected
      ) imports;

      (* Pass 2: for selectively imported items, require explicit import of their types. *)
      let seen : (string, bool) Hashtbl.t = Hashtbl.create 64 in
      let hint_seen : (string, bool) Hashtbl.t = Hashtbl.create 64 in
      let out = ref [] in
      let add_diag_once (loc:Ast.Loc.t) ~(item:string) ~(typ:string) =
        let k =
          Printf.sprintf "%s|%d|%d|%s|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line
            loc.start_pos.col
            item
            typ
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := diag_missing_imported_type ~loc ~item ~typ :: !out
        )
      in
      let add_type_hint_once (loc:Ast.Loc.t) ~(typ:string) =
        let k =
          Printf.sprintf "%s|%d|%d|type-hint|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line
            loc.start_pos.col
            typ
        in
        if not (Hashtbl.mem hint_seen k) then (
          Hashtbl.replace hint_seen k true;
          out :=
            diag_missing_import_hint
              ~loc
              ~kind:"Type"
              ~symbol:typ
              ~compools:[]
            :: !out
        )
      in
      List.iter (fun imp ->
        if imp.selected <> [] then
          match get_info_for_compool imp.compool with
          | None -> ()
          | Some info ->
              List.iter (fun (sel_name, sel_loc) ->
                match Hashtbl.find_opt info.item_deps sel_name with
                | None -> ()
                | Some deps ->
                    List.iter (fun dep ->
                      let dep = normalize_name dep in
                      if dep <> "" && not (Hashtbl.mem available_types dep) then (
                        add_diag_once sel_loc ~item:sel_name ~typ:dep;
                        add_type_hint_once sel_loc ~typ:dep
                      )
                    ) deps
              ) imp.selected
      ) imports;
      List.rev !out
  in
  missing_compools @ missing_type_imports

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

type sem_ty =
  | TyUnknown
  | TyInt
  | TyFloat
  | TyBit
  | TyChar
  | TyString
  | TyStatus
  | TyPointer of sem_ty option
  | TyArray of sem_ty
  | TyRecord of (string * sem_ty) list

type sem_proc_sig = {
  param_tys : sem_ty list option;
  ret_ty : sem_ty option;
  use_attr : Ast.proc_use;
}

type sem_value =
  | SVVar of sem_ty
  | SVConst of sem_ty
  | SVProc of sem_proc_sig

type sem_exports = {
  values : (string, sem_value) Hashtbl.t;
  types : (string, Ast.type_expr Ast.node) Hashtbl.t;
}

type sem_scope = sem_exports

type sem_proc_ctx = {
  proc_key : string;
  proc_name : string;
  proc_ret_ty : sem_ty option;
}

let diag_semantic (loc:Ast.Loc.t) (msg:string) : T.Diagnostic.t =
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Error
    ~source:"semantic"
    ~message:msg
    loc

let doc_zero_loc (doc:Document.t) : Ast.Loc.t =
  let z = { Ast.Loc.line = 1; col = 0; offset = 0 } in
  Ast.Loc.make ~file:doc.Document.file ~start_pos:z ~end_pos:z

let diag_internal_phase_failure ~(phase:string) (doc:Document.t) (exn:exn) : T.Diagnostic.t =
  let msg =
    Printf.sprintf
      "Internal %s failure: %s. Showing partial diagnostics from completed phases."
      phase
      (Printexc.to_string exn)
  in
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Error
    ~source:"internal"
    ~message:msg
    (doc_zero_loc doc)

let with_internal_phase_diag (doc:Document.t) ~(phase:string) ~(exn:exn) : Document.t =
  let d = diag_internal_phase_failure ~phase doc exn in
  Document.with_import_diags (doc.Document.import_diags @ [d]) doc

let copy_tbl (tbl:('k, 'v) Hashtbl.t) : ('k, 'v) Hashtbl.t =
  let out = Hashtbl.create (max 16 (Hashtbl.length tbl * 2)) in
  Hashtbl.iter (fun k v -> Hashtbl.replace out k v) tbl;
  out

let sem_scope_copy (s:sem_scope) : sem_scope =
  { values = copy_tbl s.values; types = copy_tbl s.types }

let sem_scope_empty () : sem_scope =
  { values = Hashtbl.create 64; types = Hashtbl.create 64 }

let sem_add_value ?(overwrite=true) (s:sem_scope) (name:string) (v:sem_value) : unit =
  let k = normalize_name name in
  if k <> "" && (overwrite || not (Hashtbl.mem s.values k)) then
    Hashtbl.replace s.values k v

let sem_add_type ?(overwrite=true) (s:sem_scope) (name:string) (t:Ast.type_expr Ast.node) : unit =
  let k = normalize_name name in
  if k <> "" && (overwrite || not (Hashtbl.mem s.types k)) then
    Hashtbl.replace s.types k t

let sem_find_record_field (fields:(string * sem_ty) list) (name:string) : sem_ty option =
  let key = normalize_name name in
  fields
  |> List.find_opt (fun (nm, _) -> normalize_name nm = key)
  |> Option.map snd

let sem_is_builtin_type (k:string) : bool =
  match k with
  | "A" | "B" | "U" | "S" | "F" | "C" | "P" | "W" | "V" | "STATUS" -> true
  | _ -> false

let is_single_letter_loop_control (name:string) : bool =
  String.length name = 1
  && match name.[0] with
     | 'A' .. 'Z' | 'a' .. 'z' -> true
     | _ -> false

let rec sem_ty_of_type_expr ?(seen:string list=[]) (types:(string, Ast.type_expr Ast.node) Hashtbl.t) (t:Ast.type_expr Ast.node) : sem_ty =
  match t.v with
  | Ast.TName id ->
      let k = normalize_name id.v in
      (match k with
       | "B" -> TyBit
       | "U" | "S" | "W" -> TyInt
       | "F" | "A" -> TyFloat
       | "C" -> TyChar
       | "P" -> TyPointer None
       | "STATUS" | "V" -> TyStatus
       | "" -> TyUnknown
       | _ ->
           if List.mem k seen || sem_is_builtin_type k then TyUnknown
           else
             match Hashtbl.find_opt types k with
             | None -> TyUnknown
             | Some defn -> sem_ty_of_type_expr ~seen:(k :: seen) types defn)
  | Ast.TArray { elem; _ } ->
      TyArray (sem_ty_of_type_expr ~seen types elem)
  | Ast.TPointer inner ->
      TyPointer (Some (sem_ty_of_type_expr ~seen types inner))
  | Ast.TRecord fields ->
      TyRecord
        (fields
         |> List.map (fun f ->
              let fv = f.v in
              (fv.fname.v, sem_ty_of_type_expr ~seen types fv.ftype)))
  | Ast.TFunc _ ->
      TyUnknown

let sem_proc_sig_of_proc (types:(string, Ast.type_expr Ast.node) Hashtbl.t) (p:Ast.proc Ast.node) : sem_proc_sig =
  let local_var_tys : (string, sem_ty) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun d ->
    match d.v with
    | Ast.DVar { name; dtype; _ } ->
        Hashtbl.replace local_var_tys (normalize_name name.v) (sem_ty_of_type_expr types dtype)
    | _ -> ()
  ) p.v.locals;
  let param_tys =
    if p.v.params = [] then Some []
    else
      Some
        (p.v.params
         |> List.map (fun prm ->
              let pn = normalize_name prm.v.pname.v in
              let direct = sem_ty_of_type_expr types prm.v.ptype in
              match direct with
              | TyUnknown ->
                  (match Hashtbl.find_opt local_var_tys pn with
                   | Some ty -> ty
                   | None -> TyUnknown)
               | ty -> ty))
  in
  let ret_ty =
    match p.v.returns with
    | None -> None
    | Some r -> Some (sem_ty_of_type_expr types r)
  in
  { param_tys; ret_ty; use_attr = p.v.use_attr }

let block_proc_names_of_program (prog:Ast.program) : (string, bool) Hashtbl.t =
  let out = Hashtbl.create 32 in
  List.iter (function
    | Ast.TopDecl d ->
        (match d.v with
         | Ast.DDirective { name; args = nm :: _ } when normalize_name name.v = "BLOCK" ->
             let k = normalize_name nm.v in
             if k <> "" then Hashtbl.replace out k true
         | _ -> ())
    | Ast.TopStmt _ -> ()
  ) prog;
  out

let sem_exports_of_program (prog:Ast.program) : sem_exports =
  let out = sem_scope_empty () in
  let block_names = block_proc_names_of_program prog in
  let is_block_proc (p:Ast.proc Ast.node) : bool =
    Hashtbl.mem block_names (normalize_name p.v.name.v)
  in
  let rec collect_types_decl ~(in_block:bool) (d:Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DType { name; defn } ->
        sem_add_type out name.v defn
    | Ast.DProc p ->
        if in_block || is_block_proc p then
          List.iter (collect_types_decl ~in_block:true) p.v.locals
    | Ast.DVar _ | Ast.DConst _ | Ast.DDirective _ -> ()
  in
  let rec collect_values_decl ~(in_block:bool) (d:Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DVar { name; dtype; _ } ->
        sem_add_value out name.v (SVVar (sem_ty_of_type_expr out.types dtype))
    | Ast.DConst { name; dtype; value = _ } ->
        let ty =
          match dtype with
          | Some t -> sem_ty_of_type_expr out.types t
          | None -> TyUnknown
        in
        sem_add_value out name.v (SVConst ty)
    | Ast.DType _ ->
        ()
    | Ast.DProc p ->
        if not in_block then
          sem_add_value out p.v.name.v (SVProc (sem_proc_sig_of_proc out.types p));
        if in_block || is_block_proc p then
          List.iter (collect_values_decl ~in_block:true) p.v.locals
    | Ast.DDirective _ ->
        ()
  in
  List.iter (function
    | Ast.TopDecl d -> collect_types_decl ~in_block:false d
    | Ast.TopStmt _ -> ()
  ) prog;
  List.iter (function
    | Ast.TopDecl d -> collect_values_decl ~in_block:false d
    | Ast.TopStmt _ -> ()
  ) prog;
  out

let sem_exports_of_doc (doc:Document.t) : sem_exports =
  match doc.Document.ast with
  | None -> sem_scope_empty ()
  | Some prog -> sem_exports_of_program prog

let add_compool_hint
    (tbl:(string, string list) Hashtbl.t)
    ~(symbol_key:string)
    ~(compool_key:string)
    : unit =
  let key = normalize_name symbol_key in
  let compool = normalize_name compool_key in
  if key <> "" && compool <> "" then
    let prev =
      match Hashtbl.find_opt tbl key with
      | None -> []
      | Some xs -> xs
    in
    if not (List.mem compool prev) then
      Hashtbl.replace tbl key (compool :: prev)

let symbol_hint_max_file_count = 1500
let symbol_hint_max_chars = 20_000_000

let build_symbol_hint_index (ws:t) : (string, string list) Hashtbl.t * (string, string list) Hashtbl.t =
  let values = Hashtbl.create 1024 in
  let types = Hashtbl.create 1024 in
  let seen_paths = Hashtbl.create 512 in
  let parsed_files = ref 0 in
  let parsed_chars = ref 0 in
  let network_root = is_network_root ws in

  let hint_compool_key_of_doc (doc:Document.t) : string option =
    match doc.Document.compool_def with
    | Some compool ->
        let k = normalize_name compool in
        if k = "" then None else Some k
    | None ->
        (match doc.Document.file with
         | None -> None
         | Some path ->
             (match source_stem_of_filename (Filename.basename path) with
              | None -> None
              | Some stem ->
                  let k = normalize_name stem in
                  if k = "" then None else Some k))
  in

  let add_doc_hints (doc:Document.t) : unit =
    match hint_compool_key_of_doc doc with
    | None -> ()
    | Some compool ->
        let exp = sem_exports_of_doc doc in
        Hashtbl.iter (fun sym v ->
          match v with
          | SVProc _ -> ()
          | _ -> add_compool_hint values ~symbol_key:sym ~compool_key:compool
        ) exp.values;
        Hashtbl.iter (fun sym _ -> add_compool_hint types ~symbol_key:sym ~compool_key:compool) exp.types
  in

  let add_path_hints (p:string) : unit =
    let pk = normalize_path_key p in
    if not (Hashtbl.mem seen_paths pk)
       && !parsed_files < symbol_hint_max_file_count
       && !parsed_chars < symbol_hint_max_chars
    then (
      Hashtbl.replace seen_paths pk true;
      match doc_from_path_cached_only ws p with
      | None ->
          enqueue_bg_path ws ~high:false p
      | Some d ->
          let txt_len = String.length d.Document.text in
          if !parsed_chars + txt_len <= symbol_hint_max_chars then (
            incr parsed_files;
            parsed_chars := !parsed_chars + txt_len;
            add_doc_hints d
          )
    )
  in

  Hashtbl.iter (fun _ doc ->
    (match doc.Document.file with
     | None -> ()
     | Some p -> Hashtbl.replace seen_paths (normalize_path_key p) true);
    add_doc_hints doc
  ) ws.docs;

  Hashtbl.iter (fun _ doc ->
    match doc.Document.file with
    | None -> ()
    | Some p ->
        let pk = normalize_path_key p in
        if not (Hashtbl.mem seen_paths pk) then (
          Hashtbl.replace seen_paths pk true;
          add_doc_hints doc
        )
  ) ws.files;

  (match ws.index with
   | None -> ()
   | Some idx ->
       if not network_root then
         Workspace_index.all_paths idx |> List.iter add_path_hints);

  (values, types)

let symbol_hint_index (ws:t) : (string, string list) Hashtbl.t * (string, string list) Hashtbl.t =
  pump_index_lookup ws;
  match ws.symbol_hints with
  | Some idx -> idx
  | None ->
      let idx = build_symbol_hint_index ws in
      ws.symbol_hints <- Some idx;
      enqueue_all_open_diag_revalidate ws ~reason:"hint_ready";
      idx

let sem_import_dirs (doc:Document.t) : compool_import_dir list =
  let ast_dirs = extract_compool_import_dirs doc in
  let ast_compools : (string, bool) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun d -> Hashtbl.replace ast_compools (normalize_name d.compool) true) ast_dirs;
  let pre_dirs =
    Document.imports doc
    |> List.filter_map (fun (imp:Preprocess.import) ->
         match imp.kind with
         | Preprocess.Compool ->
             let k = normalize_name imp.name in
             if k = "" || Hashtbl.mem ast_compools k then None
             else Some { compool = k; selected = [] })
  in
  ast_dirs @ pre_dirs

let sem_ty_of_literal (lit:Ast.literal) : sem_ty =
  match lit with
  | Ast.LInt s ->
      let u = normalize_name s in
      if String.length u >= 3 && String.contains u '\'' && String.contains u 'B' then TyBit else TyInt
  | Ast.LFloat _ -> TyFloat
  | Ast.LString s -> if String.length s = 1 then TyChar else TyString
  | Ast.LChar _ -> TyChar
  | Ast.LBool _ -> TyBit

let sem_ty_to_string (t:sem_ty) : string =
  let rec is_bit_like = function
    | TyBit -> true
    | TyArray inner -> is_bit_like inner
    | _ -> false
  in
  if is_bit_like t then "bit"
  else
  match t with
  | TyUnknown -> "unknown"
  | TyInt -> "integer"
  | TyFloat -> "float"
  | TyBit -> "bit"
  | TyChar -> "character"
  | TyString -> "string"
  | TyStatus -> "status"
  | TyPointer _ -> "pointer"
  | TyArray _ -> "table/array"
  | TyRecord _ -> "record"

let rec sem_is_primitive = function
  | TyUnknown | TyInt | TyFloat | TyBit | TyChar | TyString | TyStatus | TyPointer _ -> true
  | TyArray inner -> sem_is_primitive inner
  | TyRecord _ -> false

let rec sem_scalarize = function
  | TyArray inner when sem_is_primitive inner -> sem_scalarize inner
  | t -> t

let rec sem_compatible (lhs:sem_ty) (rhs:sem_ty) : bool =
  let lhs = sem_scalarize lhs in
  let rhs = sem_scalarize rhs in
  match lhs, rhs with
  | TyUnknown, _ | _, TyUnknown -> true
  | TyInt, TyInt
  | TyFloat, TyFloat
  | TyBit, TyBit
  | TyChar, TyChar
  | TyString, TyString
  | TyStatus, TyStatus
  | TyPointer _, TyPointer _ -> true
  | TyInt, TyFloat
  | TyFloat, TyInt -> true
  | TyRecord _, TyRecord _ -> true
  | TyArray a, TyArray b -> sem_compatible a b
  | _ -> false

let validate_semantics (ws:t) (doc:Document.t) : T.Diagnostic.t list =
  match doc.Document.ast with
  | None -> []
  | Some prog ->
      let seen = Hashtbl.create 128 in
      let out = ref [] in
      let emit (loc:Ast.Loc.t) (msg:string) =
        let k =
          Printf.sprintf "%s|%d|%d|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line
            loc.start_pos.col
            msg
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := diag_semantic loc msg :: !out
        )
      in
      let emit_import_hint (loc:Ast.Loc.t) ~(kind:string) ~(symbol:string) ~(compools:string list) =
        let k =
          Printf.sprintf "%s|%d|%d|import|%s|%s|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line
            loc.start_pos.col
            kind
            symbol
            (String.concat "," compools)
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := diag_missing_import_hint ~loc ~kind ~symbol ~compools :: !out
        )
      in

      let import_dirs = sem_import_dirs doc in
      let doc_cache : (string, Document.t option) Hashtbl.t = Hashtbl.create 16 in
      let exports_cache : (string, sem_exports option) Hashtbl.t = Hashtbl.create 16 in
      let imported_compools : (string, [ `All | `Selected ]) Hashtbl.t = Hashtbl.create 32 in
      let mark_import_mode (compool:string) (mode:[ `All | `Selected ]) =
        let k = normalize_name compool in
        if k <> "" then
          match Hashtbl.find_opt imported_compools k, mode with
          | Some `All, _ -> ()
          | _, `All -> Hashtbl.replace imported_compools k `All
          | Some `Selected, `Selected -> ()
          | None, `Selected -> Hashtbl.replace imported_compools k `Selected
      in
      List.iter (fun imp ->
        let mode = if imp.selected = [] then `All else `Selected in
        mark_import_mode imp.compool mode
      ) import_dirs;
      (match doc.Document.compool_def with
       | None -> ()
       | Some c -> mark_import_mode c `All);

      let hint_tables
        : ((string, string list) Hashtbl.t * (string, string list) Hashtbl.t) option ref
        = ref ws.symbol_hints
      in
      let hint_compools_for ~(is_type:bool) (name:string) : string list =
        let key = normalize_name name in
        if key = "" then []
        else
          match !hint_tables with
          | None ->
              []
          | Some (hint_values, hint_types) ->
              let tbl = if is_type then hint_types else hint_values in
              match Hashtbl.find_opt tbl key with
              | None -> []
              | Some xs ->
                  xs
                  |> List.filter (fun c ->
                       match Hashtbl.find_opt imported_compools (normalize_name c) with
                       | Some `All -> false
                       | Some `Selected | None -> true)
                  |> List.sort_uniq String.compare
                  |> (fun ys ->
                        let rec take n acc = function
                          | [] -> List.rev acc
                          | _ when n <= 0 -> List.rev acc
                          | x :: tl -> take (n - 1) (x :: acc) tl
                        in
                        take 3 [] ys)
      in
      let suggest_missing_import ~(loc:Ast.Loc.t) ~(kind:string) ~(is_type:bool) ~(symbol:string) : unit =
        let compools = hint_compools_for ~is_type symbol in
        if compools <> [] then
          emit_import_hint loc ~kind ~symbol ~compools
      in

      let has_import_hint ~(is_type:bool) (symbol:string) : bool =
        hint_compools_for ~is_type symbol <> []
      in

      let get_doc_for_compool (name:string) : Document.t option =
        let key = normalize_name name in
        match Hashtbl.find_opt doc_cache key with
        | Some x -> x
        | None ->
            let x = resolve_compool_doc_uncached ws ~name:key in
            Hashtbl.replace doc_cache key x;
            x
      in
      let get_exports_for_compool (name:string) : sem_exports option =
        let key = normalize_name name in
        match Hashtbl.find_opt exports_cache key with
        | Some x -> x
        | None ->
            let x =
              match get_doc_for_compool key with
              | None -> None
              | Some d -> Some (sem_exports_of_doc d)
            in
            Hashtbl.replace exports_cache key x;
            x
      in

      let should_suppress_cross_module_unresolved ~(is_type:bool) ~(name:string) : bool =
        if not warmup_suppress_crossmodule_unresolved then false
        else if ws.startup_diag_hover_ready_ms <> None then false
        else if import_dirs = [] then false
        else
          let key = normalize_name name in
          if key = "" then false
          else
            let qualified_import_match =
              match String.index_opt key '\'' with
              | None -> false
              | Some i ->
                  if i <= 0 || i + 1 >= String.length key then false
                  else
                    let compool = String.sub key (i + 1) (String.length key - i - 1) in
                    compool <> ""
                    && List.exists (fun imp -> imp.compool = compool) import_dirs
            in
            if qualified_import_match then (
              Perf_stats.tick "diag.xmodule_suppressed";
              Perf_stats.tick "diag.warmup_suppressed";
              true
            ) else
            let maybe_external =
              List.exists (fun imp ->
                let selected_match =
                  List.exists (fun (nm, _loc) -> nm = key) imp.selected
                in
                match get_exports_for_compool imp.compool with
                | None ->
                    true
                | Some exp ->
                    let exported =
                      if is_type then Hashtbl.mem exp.types key
                      else Hashtbl.mem exp.values key
                    in
                    if imp.selected = [] then exported
                    else selected_match
              ) import_dirs
            in
            if maybe_external then (
              Perf_stats.tick "diag.xmodule_suppressed";
              Perf_stats.tick "diag.warmup_suppressed";
              true
            ) else
              false
      in

      let scope = sem_scope_copy (sem_exports_of_program prog) in
      List.iter (fun imp ->
        match get_exports_for_compool imp.compool with
        | None -> ()
        | Some exp ->
            if imp.selected = [] then (
              Hashtbl.iter (fun k v -> sem_add_value ~overwrite:false scope k v) exp.values;
              Hashtbl.iter (fun k v -> sem_add_type ~overwrite:false scope k v) exp.types
            ) else (
              List.iter (fun (nm, _loc) ->
                match Hashtbl.find_opt exp.values nm with
                | Some v -> sem_add_value ~overwrite:false scope nm v
                | None -> ()
              ) imp.selected;
              List.iter (fun (nm, _loc) ->
                match Hashtbl.find_opt exp.types nm with
                | Some t -> sem_add_type ~overwrite:false scope nm t
                | None -> ()
              ) imp.selected
            )
      ) import_dirs;

      let sem_lookup_value (scp:sem_scope) (name:string) : sem_value option =
        Hashtbl.find_opt scp.values (normalize_name name)
      in

      let sem_is_builtin_call (name:string) : bool =
        let k = normalize_name name in
        k = "__CONV__" || k = "__PRESET__" || k = "__POW__" || k = "__RANGE__"
        || is_builtin_function_name k
      in

      let rec check_type_import_hints (scp:sem_scope) (t:Ast.type_expr Ast.node) : unit =
        match t.v with
        | Ast.TName id ->
            let k = normalize_name id.v in
            if k <> "" && not (sem_is_builtin_type k) && not (Hashtbl.mem scp.types k) then
              suggest_missing_import ~loc:id.loc ~kind:"Type" ~is_type:true ~symbol:id.v
        | Ast.TPointer inner ->
            check_type_import_hints scp inner
        | Ast.TArray { elem; _ } ->
            check_type_import_hints scp elem
        | Ast.TRecord fields ->
            List.iter (fun f -> check_type_import_hints scp f.v.ftype) fields
        | Ast.TFunc { params; returns } ->
            List.iter (fun p -> check_type_import_hints scp p.v.ptype) params;
            (match returns with None -> () | Some r -> check_type_import_hints scp r)
      in

      let rec sem_subscript_array (ty:sem_ty) (count:int) : sem_ty =
        if count <= 0 then ty
        else
          match ty with
          | TyArray elem -> sem_subscript_array elem (count - 1)
          | _ -> TyUnknown
      in

      let sem_subscript_pointer (ty:sem_ty) (count:int) : sem_ty =
        match ty with
        | TyPointer None ->
            TyPointer None
        | TyPointer (Some target) ->
            let inner = sem_subscript_array target count in
            if inner = TyUnknown then TyUnknown else TyPointer (Some inner)
        | _ ->
            TyUnknown
      in

      let sem_subscript_value (ty:sem_ty) (count:int) : sem_ty =
        match ty with
        | TyArray _ -> sem_subscript_array ty count
        | TyPointer _ -> sem_subscript_pointer ty count
        | _ -> TyUnknown
      in

      let sem_deref_target ~(ptr_loc:Ast.Loc.t) (ty:sem_ty) : sem_ty =
        match ty with
        | TyPointer (Some inner) ->
            inner
        | TyPointer None ->
            emit ptr_loc "Dereference requires a typed pointer.";
            TyUnknown
        | other ->
            (* Keep existing table-qualified behavior for compatibility. *)
            other
      in

      let sem_field_ty_in (field_name:string) (ty:sem_ty) : sem_ty option =
        match ty with
        | TyRecord fields -> sem_find_record_field fields field_name
        | TyArray (TyRecord fields) -> sem_find_record_field fields field_name
        | TyArray inner -> (
            match inner with
            | TyRecord fields -> sem_find_record_field fields field_name
            | _ -> None)
        | _ -> None
      in

      let rec ty_of_expr (scp:sem_scope) (current_proc:sem_proc_ctx option) ?(status_atom=false) (e:Ast.expr Ast.node) : sem_ty =
        match e.v with
        | Ast.ELit lit ->
            sem_ty_of_literal lit
        | Ast.EName id ->
            if status_atom then TyStatus
            else
              (match sem_lookup_value scp id.v with
               | Some (SVVar ty) | Some (SVConst ty) -> ty
               | Some (SVProc _) ->
                   (match current_proc with
                    | Some cp when normalize_name id.v = cp.proc_key ->
                        (match cp.proc_ret_ty with
                         | Some ty -> ty
                         | None ->
                             emit id.loc (Printf.sprintf "%S is a procedure and cannot be used as a value." id.v);
                             TyUnknown)
                    | _ ->
                        emit id.loc (Printf.sprintf "%S is a procedure and cannot be used as a value." id.v);
                        TyUnknown)
                | None ->
                     if should_suppress_cross_module_unresolved ~is_type:false ~name:id.v then
                       ()
                     else if has_import_hint ~is_type:false id.v then
                       suggest_missing_import ~loc:id.loc ~kind:"Identifier" ~is_type:false ~symbol:id.v
                     else
                       emit id.loc (Printf.sprintf "Undefined identifier %S." id.v);
                     TyUnknown)
        | Ast.EUnop { rhs; _ } ->
            ty_of_expr scp current_proc rhs
        | Ast.EBinop { lhs; rhs; _ } ->
            let l = ty_of_expr scp current_proc lhs in
            let r = ty_of_expr scp current_proc rhs in
            if l = TyFloat || r = TyFloat then TyFloat else l
        | Ast.ECall { callee; args } ->
            let ck = normalize_name callee.v in
            if ck = "V" then (
              List.iter (fun a -> ignore (ty_of_expr scp current_proc ~status_atom:true a)) args;
              TyStatus
            ) else if sem_is_builtin_call callee.v then (
              match ck, args with
              | "LOC", a0 :: rest ->
                  let target_ty = ty_of_expr scp current_proc a0 in
                  List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) rest;
                  TyPointer (Some target_ty)
              | "LOC", [] ->
                  TyPointer None
              | "NEXT", p0 :: rest ->
                  let pty = ty_of_expr scp current_proc p0 in
                  List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) rest;
                  (match pty with
                   | TyPointer _ -> pty
                   | _ -> TyUnknown)
              | _ ->
                  List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) args;
                  TyUnknown
            ) else
              (match sem_lookup_value scp callee.v with
               | Some (SVProc sig_) ->
                   (match current_proc with
                    | Some cp when normalize_name callee.v = cp.proc_key ->
                        (match sig_.use_attr with
                         | Ast.UseRec -> ()
                         | Ast.UseRent ->
                             emit callee.loc
                               (Printf.sprintf
                                  "Recursive call to %S requires REC (RENT alone is not enough)."
                                  cp.proc_name)
                         | Ast.UseNormal ->
                             emit callee.loc
                               (Printf.sprintf "Recursive call to %S requires REC." cp.proc_name))
                    | _ -> ());
                   (match sig_.param_tys with
                    | None -> ()
                    | Some pts ->
                        let rec check_pairs ps xs =
                          match ps, xs with
                          | pty :: pst, arg :: xst ->
                              let aty = ty_of_expr scp current_proc arg in
                              if not (sem_compatible pty aty) then
                                emit arg.loc
                                  (Printf.sprintf
                                     "Argument type mismatch in call to %S: expected %s, got %s."
                                     callee.v
                                     (sem_ty_to_string pty)
                                     (sem_ty_to_string aty));
                              check_pairs pst xst
                          | _, [] -> ()
                          | [], _ :: xst ->
                              (* Extra args: parse them, but avoid noisy count errors for now. *)
                              List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) xst
                        in
                        check_pairs pts args);
                   (match sig_.ret_ty with Some rt -> rt | None -> TyUnknown)
               | Some (SVVar ty) | Some (SVConst ty) ->
                   List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) args;
                   if args = [] then (
                     emit callee.loc (Printf.sprintf "%S is not callable." callee.v);
                     TyUnknown
                   ) else
                     let out_ty = sem_subscript_value ty (List.length args) in
                     if out_ty = TyUnknown then (
                       emit callee.loc (Printf.sprintf "Cannot subscript %S." callee.v);
                       TyUnknown
                     ) else out_ty
                | None ->
                    if not (should_suppress_cross_module_unresolved ~is_type:false ~name:callee.v) then
                      emit callee.loc
                        (Printf.sprintf
                           "Undefined procedure %S. Declare it with REF PROC %S in scope."
                           callee.v
                           callee.v);
                    List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) args;
                    TyUnknown)
        | Ast.EIndex { base; index } ->
            let bt = ty_of_expr scp current_proc base in
            List.iter (fun i -> ignore (ty_of_expr scp current_proc i)) index;
            sem_subscript_value bt (List.length index)
        | Ast.EField { base; field } ->
            let bt = ty_of_expr scp current_proc base in
            (match bt with
             | TyRecord fields ->
                 (match sem_find_record_field fields field.v with
                  | Some t -> t
                  | None ->
                      emit field.loc (Printf.sprintf "Unknown field %S." field.v);
                      TyUnknown)
             | TyArray (TyRecord fields) ->
                 (match sem_find_record_field fields field.v with
                  | Some t -> t
                  | None ->
                      emit field.loc (Printf.sprintf "Unknown field %S." field.v);
                      TyUnknown)
             | _ -> TyUnknown)
        | Ast.EAt { field; ptr } ->
            let pt = ty_of_expr scp current_proc ptr in
            let target_ty = sem_deref_target ~ptr_loc:ptr.loc pt in
            let field_ref =
              match field.v with
              | Ast.EName id ->
                  Some (id, [])
              | Ast.EIndex { base; index } -> (
                  match base.v with
                  | Ast.EName id -> Some (id, index)
                  | _ -> None)
              | _ ->
                  ignore (ty_of_expr scp current_proc field);
                  None
            in
            (match field_ref with
             | None -> TyUnknown
             | Some (id, indexes) ->
                 List.iter (fun i -> ignore (ty_of_expr scp current_proc i)) indexes;
                 let qualified_target = sem_subscript_array target_ty (List.length indexes) in
                 (match sem_field_ty_in id.v qualified_target with
                  | Some ty -> ty
                  | None ->
                       emit id.loc (Printf.sprintf "Unknown field %S for @ access." id.v);
                       TyUnknown))
        | Ast.EDeref { ptr } ->
            let pt = ty_of_expr scp current_proc ptr in
            sem_deref_target ~ptr_loc:ptr.loc pt
        | Ast.EParen inner ->
            ty_of_expr scp current_proc inner
      in

      let ty_of_lvalue (scp:sem_scope) (current_proc:sem_proc_ctx option) (e:Ast.expr Ast.node) : sem_ty option =
        match e.v with
        | Ast.EName id ->
            (match sem_lookup_value scp id.v with
             | Some (SVVar ty) | Some (SVConst ty) -> Some ty
             | Some (SVProc _) ->
                 (match current_proc with
                  | Some cp when normalize_name id.v = cp.proc_key ->
                      (match cp.proc_ret_ty with
                       | Some ty -> Some ty
                       | None ->
                           emit id.loc (Printf.sprintf "Cannot assign to procedure %S." id.v);
                           None)
                  | _ ->
                      emit id.loc (Printf.sprintf "Cannot assign to procedure %S." id.v);
                      None)
              | None ->
                   if should_suppress_cross_module_unresolved ~is_type:false ~name:id.v then
                     ()
                   else if has_import_hint ~is_type:false id.v then
                     suggest_missing_import ~loc:id.loc ~kind:"Item" ~is_type:false ~symbol:id.v
                   else
                     emit id.loc (Printf.sprintf "Undefined item %S." id.v);
                   None)
        | Ast.EField _ | Ast.EAt _ | Ast.EDeref _ | Ast.EIndex _ ->
            Some (ty_of_expr scp current_proc e)
        | _ ->
            ignore (ty_of_expr scp current_proc e);
            None
      in

      let add_decl_symbol (scp:sem_scope) (d:Ast.decl Ast.node) : unit =
        match d.v with
        | Ast.DType { name; defn } ->
            sem_add_type scp name.v defn
        | Ast.DVar { name; dtype; _ } ->
            sem_add_value scp name.v (SVVar (sem_ty_of_type_expr scp.types dtype))
        | Ast.DConst { name; dtype; _ } ->
            let ty =
              match dtype with
              | None -> TyUnknown
              | Some t -> sem_ty_of_type_expr scp.types t
            in
            sem_add_value scp name.v (SVConst ty)
        | Ast.DProc p ->
            sem_add_value scp p.v.name.v (SVProc (sem_proc_sig_of_proc scp.types p))
        | Ast.DDirective _ -> ()
      in

      let rec collect_label_depths_for_stmt
          (out:(string, int) Hashtbl.t)
          ~(loop_depth:int)
          (s:Ast.stmt Ast.node)
        : unit =
        match s.v with
        | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _ | Ast.SGoto _ ->
            ()
        | Ast.SDecl _ ->
            ()
        | Ast.SBlock xs ->
            List.iter (collect_label_depths_for_stmt out ~loop_depth) xs
        | Ast.SIf { then_; else_; _ } ->
            collect_label_depths_for_stmt out ~loop_depth then_;
            (match else_ with
             | None -> ()
             | Some e -> collect_label_depths_for_stmt out ~loop_depth e)
        | Ast.SWhile { body; _ } ->
            collect_label_depths_for_stmt out ~loop_depth:(loop_depth + 1) body
        | Ast.SFor { init; step; body; _ } ->
            (match init with
             | None -> ()
             | Some i -> collect_label_depths_for_stmt out ~loop_depth i);
            (match step with
             | None -> ()
             | Some st -> collect_label_depths_for_stmt out ~loop_depth st);
            collect_label_depths_for_stmt out ~loop_depth:(loop_depth + 1) body
        | Ast.SLabel { label; body } ->
            let key = normalize_name label.v in
            if key <> "" && not (Hashtbl.mem out key) then
              Hashtbl.add out key loop_depth;
            collect_label_depths_for_stmt out ~loop_depth body
      in

      let collect_label_depths (s:Ast.stmt Ast.node) : (string, int) Hashtbl.t =
        let out = Hashtbl.create 64 in
        collect_label_depths_for_stmt out ~loop_depth:0 s;
        out
      in

      let top_level_label_depths : (string, int) Hashtbl.t =
        let out = Hashtbl.create 64 in
        List.iter (function
          | Ast.TopStmt s ->
              collect_label_depths_for_stmt out ~loop_depth:0 s
          | Ast.TopDecl _ ->
              ()
        ) prog;
        out
      in

      let rec check_stmt
          (scp:sem_scope)
          (current_proc:sem_proc_ctx option)
          ~(loop_depth:int)
          ~(label_depths:(string, int) Hashtbl.t)
          (s:Ast.stmt Ast.node)
        : unit =
        match s.v with
        | Ast.SEmpty -> ()
        | Ast.SDecl d ->
            add_decl_symbol scp d;
            check_decl scp current_proc ~loop_depth ~label_depths d
        | Ast.SBlock xs ->
            List.iter (fun st -> check_stmt scp current_proc ~loop_depth ~label_depths st) xs
        | Ast.SAssign { lhs; rhs } ->
            let lhs_ty = ty_of_lvalue scp current_proc lhs in
            let rhs_ty = ty_of_expr scp current_proc rhs in
            (match lhs_ty with
             | None -> ()
             | Some lt ->
                 if not (sem_compatible lt rhs_ty) then
                   emit rhs.loc
                     (Printf.sprintf
                        "Type mismatch in assignment: left is %s, right is %s."
                        (sem_ty_to_string lt)
                         (sem_ty_to_string rhs_ty)))
        | Ast.SCallStmt { callee; args; abort_label } ->
            let ck = normalize_name callee.v in
            if is_control_stmt_keyword ck then (
              if ck = "EXIT" && loop_depth <= 0 then
                emit callee.loc "EXIT is only valid inside a loop.";
              if ck = "ABORT" && current_proc = None then
                emit callee.loc "ABORT is only valid inside a procedure.";
              if ck = "STOP" then
                List.iter (fun arg -> ignore (ty_of_expr scp current_proc arg)) args;
              if ck = "STOP" && List.length args > 1 then
                emit callee.loc "STOP accepts at most one optional stop code expression."
            ) else (
              let diag_count_before = List.length !out in
              ignore (ty_of_expr scp current_proc (Ast.node ~loc:s.loc (Ast.ECall { callee; args })));
              let diag_count_after = List.length !out in
              if diag_count_after = diag_count_before then
                match sem_lookup_value scp callee.v with
                | Some _ -> ()
                | None ->
                    if (not (sem_is_builtin_call callee.v))
                       && not (should_suppress_cross_module_unresolved ~is_type:false ~name:callee.v)
                    then
                      emit callee.loc
                        (Printf.sprintf
                           "Undefined procedure %S. Declare it with REF PROC %S in scope."
                           callee.v
                           callee.v)
            );
            (match abort_label with
             | None -> ()
             | Some lab ->
                 if current_proc = None then
                   emit lab.loc "ABORT label phrase is only valid inside a procedure call statement."
                 else
                   let lk = normalize_name lab.v in
                   if lk = "" || not (Hashtbl.mem label_depths lk) then
                     emit lab.loc
                       (Printf.sprintf "Undefined ABORT target label %S." lab.v))
        | Ast.SIf { cond; then_; else_ } ->
            ignore (ty_of_expr scp current_proc cond);
            check_stmt scp current_proc ~loop_depth ~label_depths then_;
            (match else_ with
             | None -> ()
             | Some e -> check_stmt scp current_proc ~loop_depth ~label_depths e)
        | Ast.SWhile { cond; body } ->
            ignore (ty_of_expr scp current_proc cond);
            check_stmt scp current_proc ~loop_depth:(loop_depth + 1) ~label_depths body
        | Ast.SFor { init; cond; step; body } ->
            let for_scope =
              match init with
              | Some ({ v = Ast.SAssign { lhs = { v = Ast.EName lc; _ }; rhs }; _ })
                when is_single_letter_loop_control lc.v ->
                  let scp2 = sem_scope_copy scp in
                  let lty = ty_of_expr scp current_proc rhs in
                  sem_add_value scp2 lc.v (SVVar lty);
                  scp2
              | _ ->
                  scp
            in
            (match init with
             | None -> ()
             | Some i -> check_stmt for_scope current_proc ~loop_depth ~label_depths i);
            (match cond with None -> () | Some c -> ignore (ty_of_expr for_scope current_proc c));
            (match step with
             | None -> ()
             | Some st -> check_stmt for_scope current_proc ~loop_depth ~label_depths st);
            check_stmt for_scope current_proc ~loop_depth:(loop_depth + 1) ~label_depths body
        | Ast.SReturn eo ->
            if current_proc = None then
              emit s.loc "RETURN is only valid inside a procedure.";
            (match eo with None -> () | Some e -> ignore (ty_of_expr scp current_proc e))
        | Ast.SLabel { body; _ } ->
            check_stmt scp current_proc ~loop_depth ~label_depths body
        | Ast.SGoto id ->
            let key = normalize_name id.v in
            (match Hashtbl.find_opt label_depths key with
             | None ->
                 emit id.loc (Printf.sprintf "Undefined target label %S." id.v)
             | Some target_depth ->
                 if target_depth > loop_depth then
                   emit id.loc
                     (Printf.sprintf
                        "GOTO to %S enters a deeper loop body, which is not allowed."
                        id.v))

      and check_decl
          (scp:sem_scope)
          (current_proc:sem_proc_ctx option)
          ~(loop_depth:int)
          ~label_depths:(_label_depths:(string, int) Hashtbl.t)
          (d:Ast.decl Ast.node)
        : unit =
        let _ = loop_depth in
        match d.v with
        | Ast.DVar { dtype; init; _ } ->
            check_type_import_hints scp dtype;
            (match init with
             | None -> ()
             | Some rhs ->
                 let lty = sem_ty_of_type_expr scp.types dtype in
                 let rty = ty_of_expr scp current_proc rhs in
                 if not (sem_compatible lty rty) then
                   emit rhs.loc
                     (Printf.sprintf
                        "Type mismatch in initializer: expected %s, got %s."
                        (sem_ty_to_string lty)
                        (sem_ty_to_string rty)))
        | Ast.DConst { dtype; value; _ } ->
            (match dtype with
             | None -> ()
             | Some t -> check_type_import_hints scp t);
            ignore (ty_of_expr scp current_proc value)
        | Ast.DType { defn; _ } ->
            check_type_import_hints scp defn
        | Ast.DDirective _ ->
            ()
        | Ast.DProc p ->
            let proc_scope = sem_scope_copy scp in
            let proc_label_depths = collect_label_depths p.v.body in
            List.iter (add_decl_symbol proc_scope) p.v.locals;
            let local_var_tys : (string, sem_ty) Hashtbl.t = Hashtbl.create 32 in
            List.iter (fun dlocal ->
              match dlocal.v with
              | Ast.DVar { name; dtype; _ } ->
                  Hashtbl.replace
                    local_var_tys
                    (normalize_name name.v)
                    (sem_ty_of_type_expr proc_scope.types dtype)
              | _ -> ()
            ) p.v.locals;
            List.iter (fun prm -> check_type_import_hints proc_scope prm.v.ptype) p.v.params;
            (match p.v.returns with
             | None -> ()
             | Some r -> check_type_import_hints proc_scope r);
            let proc_ret_ty =
              match p.v.returns with
              | None -> None
              | Some r -> Some (sem_ty_of_type_expr proc_scope.types r)
            in
            let proc_ctx =
              Some
                {
                  proc_key = normalize_name p.v.name.v;
                  proc_name = p.v.name.v;
                  proc_ret_ty;
                }
            in
            List.iter (fun prm ->
              let pname = prm.v.pname.v in
              let direct_ty = sem_ty_of_type_expr proc_scope.types prm.v.ptype in
              let inferred_ty =
                match direct_ty with
                | TyUnknown ->
                    (match Hashtbl.find_opt local_var_tys (normalize_name pname) with
                     | Some ty -> ty
                     | _ -> TyUnknown)
                | ty -> ty
              in
              sem_add_value proc_scope pname (SVVar inferred_ty)
            ) p.v.params;
            List.iter
              (fun pd ->
                check_decl proc_scope proc_ctx ~loop_depth:0 ~label_depths:proc_label_depths pd)
              p.v.locals;
            check_stmt proc_scope proc_ctx ~loop_depth:0 ~label_depths:proc_label_depths p.v.body
      in

      List.iter (function
        | Ast.TopDecl d -> check_decl scope None ~loop_depth:0 ~label_depths:top_level_label_depths d
        | Ast.TopStmt s -> check_stmt scope None ~loop_depth:0 ~label_depths:top_level_label_depths s
      ) prog;
      List.rev !out

let store_doc
    ?(import_lookup_pump:bool=true)
    ?(semantic_mode:semantic_validation_mode=SemanticFull)
    (ws:t)
    (uri:T.DocumentUri.t)
    (doc:Document.t)
  : unit =
  let old_doc = Hashtbl.find_opt ws.docs uri in
  let old_compool_key = Option.bind old_doc compool_key_of_doc in
  let import_diags =
    try
      validate_imports ~pump_lookup:import_lookup_pump ws doc
    with exn ->
      [diag_internal_phase_failure ~phase:"import" doc exn]
  in
  let semantic_diags =
    if doc.Document.ast = None || doc.Document.parse_diags <> [] then
      []
    else
      (match semantic_mode with
       | SemanticFull ->
           (try
              validate_semantics ws doc
            with exn ->
              [diag_internal_phase_failure ~phase:"semantic" doc exn])
       | SemanticRangeSemi _ ->
           [])
  in
  let doc = Document.with_import_diags (import_diags @ semantic_diags) doc in
  mark_graph_dirty ws;
  let new_compool_key = compool_key_of_doc doc in
  let has_compool (d:Document.t) =
    match d.Document.compool_def with
    | None -> false
    | Some name -> normalize_name name <> ""
  in
  if has_compool doc || Option.fold ~none:false ~some:has_compool old_doc then
    invalidate_symbol_hints ws;
  Hashtbl.replace ws.docs uri doc;
  let revalidate_importers_for_compool (key:string option) : unit =
    match key with
    | None -> ()
    | Some key ->
        let importers = importer_uris_for_compool_key ws ~compool_key:key in
        invalidate_importer_nav_state_for_compool_key ws ~compool_key:key;
        List.iter
          (fun importer_uri ->
            enqueue_open_diag_revalidate ws ~uri:importer_uri ~reason:"compool_change")
          importers
  in
  revalidate_importers_for_compool old_compool_key;
  revalidate_importers_for_compool new_compool_key;
  if ws.sem_store_enabled then (
    Semantic_store.remove_uri ws.semantic_store ~uri;
  );
  match doc.Document.file with
  | None -> ()
  | Some f ->
      let path_key = normalize_path_key f in
      Hashtbl.replace ws.files path_key doc;
      Hashtbl.replace ws.bg_parsed path_key true;
      Hashtbl.remove ws.closed_doc_last_touch path_key

let store_doc_fast (ws:t) (uri:T.DocumentUri.t) (doc:Document.t) : unit =
  mark_graph_dirty ws;
  Hashtbl.replace ws.docs uri doc;
  match doc.Document.file with
  | None -> ()
  | Some f ->
      let path_key = normalize_path_key f in
      Hashtbl.replace ws.files path_key doc;
      if doc.Document.parse_rev = doc.Document.rev then
        Hashtbl.replace ws.bg_parsed path_key true
      else
        Hashtbl.remove ws.bg_parsed path_key;
      Hashtbl.remove ws.closed_doc_last_touch path_key

let direct_import_paths (ws:t) (doc:Document.t) : string list =
  let acc = Hashtbl.create 16 in
  let resolve_compool_path (name:string) : string option =
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
          if allow_fallback_scan ws then find_compool_path_fallback ws ~key else None
  in
  List.iter (fun (imp:Preprocess.import) ->
    match imp.kind with
    | Preprocess.Compool ->
        (match resolve_compool_path imp.name with
         | None -> ()
         | Some p -> Hashtbl.replace acc (normalize_path_key p) p)
  ) (Document.imports doc);
  Hashtbl.fold (fun _ p xs -> p :: xs) acc []

let enqueue_doc_imports_high (ws:t) (doc:Document.t) : unit =
  direct_import_paths ws doc
  |> List.iter (fun p -> enqueue_bg_path ws ~lane:LaneOpen ~reason_group:"open_import" ~high:true p)

let background_doc_with_diags (ws:t) (doc:Document.t) : Document.t =
  let import_diags =
    try
      validate_imports ~pump_lookup:false ws doc
    with exn ->
      [diag_internal_phase_failure ~phase:"import" doc exn]
  in
  let semantic_diags =
    if doc.Document.ast = None || doc.Document.parse_diags <> [] then
      []
    else
      (try
         validate_semantics ws doc
       with exn ->
         [diag_internal_phase_failure ~phase:"semantic" doc exn])
  in
  Document.with_import_diags (import_diags @ semantic_diags) doc

let queue_workspace_diag_update_for_doc (ws:t) (doc:Document.t) : unit =
  match doc.Document.file with
  | None -> ()
  | Some path ->
      (match find_open_doc_for_path ws ~path with
      | Some _ ->
          ()
      | None ->
           if bg_diag_allowed ws then (
             let uri_s = Uri_path.docuri_to_string doc.Document.uri in
             let filtered = filter_workspace_diags ws (Document.diagnostics doc) in
             let prev =
               match Hashtbl.find_opt ws.bg_closed_diags uri_s with
               | Some ds -> ds
               | None -> []
             in
             if prev <> filtered then (
               if filtered = [] then Hashtbl.remove ws.bg_closed_diags uri_s
               else Hashtbl.replace ws.bg_closed_diags uri_s filtered;
               enqueue_bg_diag_update ws ~uri:doc.Document.uri ~diags:filtered
             )
           ))

let refresh_bg_seed_paths (ws:t) : unit =
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
      let total_unique = ref 0 in
      let seen = Hashtbl.create (max 16 (Array.length ws.bg_seed_paths)) in
      Array.iter (fun p ->
        let key = normalize_path_key p in
        if key <> "" && not (Hashtbl.mem seen key) then (
          Hashtbl.replace seen key true;
          incr total_unique;
          if not (Hashtbl.mem ws.quick_nav_done_set key)
             && not (Hashtbl.mem ws.quick_nav_pending_set key)
          then (
            Hashtbl.replace ws.quick_nav_pending_set key true;
            Queue.add p ws.quick_nav_pending_paths
          )
        )
      ) ws.bg_seed_paths;
      ws.quick_nav_index_total <- !total_unique;
      ws.quick_nav_index_done <- Hashtbl.length ws.quick_nav_done_set;
      ws.bg_seed_needs_refresh <- false

let seed_bg_paths_from_index (ws:t) : unit =
  ensure_graph_fresh ws;
  if ws.bg_seed_needs_refresh then
    refresh_bg_seed_paths ws;
  let per_tick =
    match workspace_pressure_mode ws with
    | PressureNormal -> bg_seed_paths_per_tick
    | PressureSoft -> max 1 (bg_seed_paths_per_tick / 4)
    | PressureCritical -> 0
  in
  if per_tick <= 0 then ()
  else (
    let added = ref 0 in
    while !added < per_tick
          && ws.graph_root_closure_cursor < Array.length ws.graph_root_closure_paths
    do
      let p = ws.graph_root_closure_paths.(ws.graph_root_closure_cursor) in
      ws.graph_root_closure_cursor <- ws.graph_root_closure_cursor + 1;
      enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"root_closure" ~high:false p;
      incr added
    done;
    while !added < per_tick
          && ws.bg_seed_cursor < Array.length ws.bg_seed_paths
    do
      let p = ws.bg_seed_paths.(ws.bg_seed_cursor) in
      ws.bg_seed_cursor <- ws.bg_seed_cursor + 1;
      enqueue_bg_path ws ~lane:LaneSweep ~reason_group:"seed_sweep" ~high:false p;
      incr added
    done
  )

let quick_nav_proc_kind = 12

let parse_doc_worker
    ~(max_bytes:int)
    ~(uri:T.DocumentUri.t)
    ~(file:string option)
    ~(text:string)
  : Document.t =
  if is_parse_guard_exceeded ~max_bytes ~text_len:(String.length text) then
    Document.make_unparsed
      ~uri
      ~file
      ~text
      ~parse_diags:[diag_parse_guard ~file ~max_bytes ~actual_bytes:(String.length text)]
  else
    try
      Document.make ~uri ~file ~text
    with exn ->
      let fallback = Document.make ~uri ~file ~text:"" in
      with_internal_phase_diag fallback ~phase:"worker-parse" ~exn

let parse_job_path_key (job:parse_job) : string =
  match job.pj_payload with
  | ParseJobOpen x -> x.path_key
  | ParseJobPath x -> x.path_key

let parse_job_kind_of_queue_kind = function
  | BgQueueHighLarge -> Some ParseJobHighLarge
  | BgQueueRootLarge -> Some ParseJobRootLarge
  | BgQueueNormalLarge -> Some ParseJobNormalLarge
  | BgQueueHighSmall | BgQueueRootSmall | BgQueueNormalSmall -> None

let parse_worker_loop (ws:t) () : unit =
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
  let push_result (res:parse_result) =
    Mutex.lock ws.parse_worker_mtx;
    Queue.add res ws.parse_worker_results;
    Mutex.unlock ws.parse_worker_mtx
  in
  let rec loop () =
    match wait_job () with
    | None -> ()
    | Some job ->
        let result =
          match job.pj_payload with
          | ParseJobOpen { path_key; uri; file; text; generation } ->
              let doc =
                parse_doc_worker
                  ~max_bytes:ws.parse_file_max_bytes
                  ~uri
                  ~file
                  ~text
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
                | None ->
                    (match T.DocumentUri.t_of_yojson (`String (Uri_path.file_uri_of_path path)) with
                     | u -> u
                     | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))
              in
              let doc_opt =
                match read_file_text path with
                | None -> None
                | Some text ->
                    Some
                      (parse_doc_worker
                         ~max_bytes:ws.parse_file_max_bytes
                         ~uri
                         ~file:(Some path)
                         ~text)
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

let ensure_parse_worker_started (ws:t) : unit =
  if ws.parse_worker_started then ()
  else (
    ws.parse_worker_started <- true;
    ignore (Thread.create (parse_worker_loop ws) ())
  )

let try_submit_large_parse_job
    (ws:t)
    ~(path:string)
    ~(queue_kind:bg_queue_kind)
  : bool =
  match parse_job_kind_of_queue_kind queue_kind with
  | None -> false
  | Some pj_kind ->
      let path_key = normalize_path_key path in
      if path_key = "" then true
      else if Hashtbl.mem ws.parse_worker_inflight path_key then
        true
      else if Hashtbl.length ws.parse_worker_inflight >= ws.parse_worker_max_inflight then
        false
      else
        let payload =
          match find_open_doc_for_path ws ~path with
          | Some doc ->
              let generation =
                match Hashtbl.find_opt ws.open_parse_generation (Uri_path.docuri_to_string doc.Document.uri) with
                | Some g -> g
                | None -> 0
              in
              ParseJobOpen
                {
                  path_key;
                  uri = doc.Document.uri;
                  file = doc.Document.file;
                  text = doc.Document.text;
                  generation;
                }
          | None ->
              ParseJobPath { path; path_key }
        in
        let job = { pj_kind; pj_epoch = ws.parse_epoch; pj_payload = payload } in
        Mutex.lock ws.parse_worker_mtx;
        Queue.add job ws.parse_worker_jobs;
        Condition.signal ws.parse_worker_cv;
        Mutex.unlock ws.parse_worker_mtx;
        Hashtbl.replace ws.parse_worker_inflight path_key pj_kind;
        true

let quick_nav_entry_add (ws:t) ~(key:string) (entry:quick_nav_entry) : unit =
  let k = normalize_name key in
  if k = "" then ()
  else
    let prev =
      match Hashtbl.find_opt ws.quick_nav_index k with
      | Some xs -> xs
      | None -> []
    in
    if List.exists (fun x ->
         Uri_path.docuri_to_string x.qn_uri = Uri_path.docuri_to_string entry.qn_uri
         && x.qn_loc = entry.qn_loc) prev
    then ()
    else
      Hashtbl.replace ws.quick_nav_index k (entry :: prev)

let quick_nav_entries_of_path_prefix (path:string) ~(max_bytes:int) : quick_nav_entry list =
  match read_file_prefix_text path ~max_bytes with
  | None -> []
  | Some text ->
      let upper = String.uppercase_ascii text in
      let n = String.length upper in
      let idx = Text_index.of_string text in
      let uri =
        match Uri_path.docuri_of_path path with
        | Some u -> u
        | None ->
            (match T.DocumentUri.t_of_yojson (`String (Uri_path.file_uri_of_path path)) with
             | u -> u
             | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))
      in
      let rec scan i acc =
        if i + 5 > n then List.rev acc
        else if String.sub upper i 5 = "PROC " then
          let s = i + 5 in
          let j = ref s in
          while !j < n && is_nav_ident_char upper.[!j] do
            incr j
          done;
          let key = String.sub upper s (!j - s) |> normalize_name in
          let acc =
            if key = "" then acc
            else
              let l0, c0 = Text_index.line_col_of_offset idx s in
              let l1, c1 = Text_index.line_col_of_offset idx !j in
              let loc =
                Ast.Loc.make
                  ~file:(Some path)
                  ~start_pos:{ line = l0 + 1; col = c0; offset = s }
                  ~end_pos:{ line = l1 + 1; col = c1; offset = !j }
              in
              {
                qn_uri = uri;
                qn_name = key;
                qn_key = key;
                qn_loc = loc;
                qn_kind = quick_nav_proc_kind;
                qn_container = None;
              } :: acc
          in
          scan (i + 1) acc
        else
          scan (i + 1) acc
      in
      scan 0 []

let quick_nav_index_step (ws:t) ~(budget_ms:int) : unit =
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
            quick_nav_entries_of_path_prefix path ~max_bytes:nav_quick_scan_per_file_bytes
          in
          List.iter (fun e -> quick_nav_entry_add ws ~key:e.qn_key e) entries;
          if not (Hashtbl.mem ws.quick_nav_done_set path_key) then (
            Hashtbl.replace ws.quick_nav_done_set path_key true;
            ws.quick_nav_index_done <- ws.quick_nav_index_done + 1
          )
        );
        loop ()
    in
    loop ()

let apply_parse_result_open
    (ws:t)
    ~(pr_kind:parse_job_kind)
    ~(pr_epoch:int)
    ~(path_key:string)
    ~(uri:T.DocumentUri.t)
    ~(generation:int)
    ~(doc:Document.t)
  : unit =
  Hashtbl.remove ws.parse_worker_inflight path_key;
  if pr_epoch <> ws.parse_epoch then
    Perf_stats.tick "bg.parse.stale_result_drop"
  else
  let latest_generation =
    match Hashtbl.find_opt ws.open_parse_generation (Uri_path.docuri_to_string uri) with
    | Some g -> g
    | None -> 0
  in
  if latest_generation <> generation then
    Perf_stats.tick "diag.open.stale_generation_drop"
  else (
    ignore pr_kind;
    store_doc ~import_lookup_pump:false ws uri doc;
    let uri_key = Uri_path.docuri_to_string uri in
    if doc.Document.parse_rev = doc.Document.rev then (
      enqueue_open_diag_revalidate ws ~uri ~reason:"open_parse";
      (match Hashtbl.find_opt ws.open_provisional_since_ms uri_key with
       | None -> ()
       | Some t0 ->
           let lag = max 0.0 (Perf_stats.now_ms () -. t0) in
           Perf_stats.observe_ms "diag.open.authoritative_lag_ms" lag;
           Hashtbl.remove ws.open_provisional_since_ms uri_key);
      Hashtbl.remove ws.open_parse_generation uri_key
    ) else
      Hashtbl.replace ws.open_provisional_since_ms uri_key (Perf_stats.now_ms ());
    (match doc.Document.file with
     | Some p ->
         let pk = normalize_path_key p in
         Hashtbl.replace ws.files pk doc;
         Hashtbl.replace ws.bg_parsed pk true;
         Hashtbl.remove ws.closed_doc_last_touch pk
     | None -> ());
    enqueue_doc_imports_high ws doc;
    (match compool_key_of_doc doc with
     | None -> ()
     | Some key ->
         let importers = importer_uris_for_compool_key ws ~compool_key:key in
         invalidate_importer_nav_state_for_compool_key ws ~compool_key:key;
         List.iter
           (fun importer_uri ->
             enqueue_open_diag_revalidate ws ~uri:importer_uri ~reason:"compool_change")
           importers);
    invalidate_lsif_snapshot ws
  )

let apply_parse_result_path
    (ws:t)
    ~(pr_kind:parse_job_kind)
    ~(pr_epoch:int)
    ~(path_key:string)
    ~(doc_opt:Document.t option)
  : unit =
  Hashtbl.remove ws.parse_worker_inflight path_key;
  if pr_epoch <> ws.parse_epoch then
    Perf_stats.tick "bg.parse.stale_result_drop"
  else
    match doc_opt with
    | None -> ()
    | Some doc ->
        let doc =
          match pr_kind with
          | ParseJobHighLarge ->
              background_doc_with_diags ws doc
          | ParseJobRootLarge ->
              background_doc_with_diags ws doc
          | ParseJobNormalLarge ->
              if ws.startup_fully_nav_ready_ms <> None then
                background_doc_with_diags ws doc
              else
                doc
        in
        invalidate_lsif_snapshot ws;
        Hashtbl.replace ws.files path_key doc;
        Hashtbl.replace ws.bg_parsed path_key true;
        touch_closed_doc_path ws ~path_key;
        evict_closed_docs_if_needed ws;
        (match compool_key_of_doc doc with
         | None -> ()
         | Some key ->
             let importers = importer_uris_for_compool_key ws ~compool_key:key in
             invalidate_importer_nav_state_for_compool_key ws ~compool_key:key;
             List.iter
               (fun importer_uri ->
                 enqueue_open_diag_revalidate ws ~uri:importer_uri ~reason:"compool_change")
               importers);
        queue_workspace_diag_update_for_doc ws doc

let drain_parse_worker_results (ws:t) ~(max_items:int) : unit =
  if max_items <= 0 then ()
  else
    let rec loop n =
      if n <= 0 then ()
      else (
        Mutex.lock ws.parse_worker_mtx;
        let next =
          if Queue.is_empty ws.parse_worker_results then None
          else Some (Queue.pop ws.parse_worker_results)
        in
        Mutex.unlock ws.parse_worker_mtx;
        match next with
        | None -> ()
        | Some (ParseResultOpen x) ->
            apply_parse_result_open ws
              ~pr_kind:x.pr_kind
              ~pr_epoch:x.pr_epoch
              ~path_key:x.path_key
              ~uri:x.uri
              ~generation:x.generation
              ~doc:x.doc;
            loop (n - 1)
        | Some (ParseResultPath x) ->
            apply_parse_result_path ws
              ~pr_kind:x.pr_kind
              ~pr_epoch:x.pr_epoch
              ~path_key:x.path_key
              ~doc_opt:x.doc_opt;
            loop (n - 1))
    in
    loop max_items

let background_parse_path (ws:t) (path:string) : unit =
  let path_key = normalize_path_key path in
  if path_key = "" || Hashtbl.mem ws.bg_parsed path_key then ()
  else
    match find_open_doc_for_path ws ~path with
    | Some open_doc ->
        let uri = open_doc.Document.uri in
        let uri_key = Uri_path.docuri_to_string uri in
        if open_doc.Document.parse_rev = open_doc.Document.rev
           && not (Hashtbl.mem ws.open_parse_generation uri_key)
        then (
          Hashtbl.replace ws.bg_parsed path_key true;
          Hashtbl.remove ws.closed_doc_last_touch path_key
        ) else (
          let parse_generation =
            match Hashtbl.find_opt ws.open_parse_generation uri_key with
            | Some g -> g
            | None -> 0
          in
          let parsed_doc =
            if open_doc.Document.parse_rev = open_doc.Document.rev then
              open_doc
            else
              (try
                 Perf_stats.time "parse.open_doc_deferred" (fun () ->
                 parse_guarded_document_make ws
                     ~uri:open_doc.Document.uri
                     ~file:open_doc.Document.file
                     ~text:open_doc.Document.text)
               with exn ->
                 with_internal_phase_diag open_doc ~phase:"open-doc-deferred" ~exn)
          in
          let latest_generation =
            match Hashtbl.find_opt ws.open_parse_generation uri_key with
            | Some g -> g
            | None -> 0
          in
          if latest_generation <> parse_generation then
            Perf_stats.tick "diag.open.stale_generation_drop"
          else (
            store_doc ~import_lookup_pump:false ws uri parsed_doc;
            if parsed_doc.Document.parse_rev = parsed_doc.Document.rev then (
              let key = Uri_path.docuri_to_string uri in
              enqueue_open_diag_revalidate ws ~uri ~reason:"open_parse";
              (match Hashtbl.find_opt ws.open_provisional_since_ms key with
               | None -> ()
               | Some t0 ->
                   let lag = max 0.0 (Perf_stats.now_ms () -. t0) in
                   Perf_stats.observe_ms "diag.open.authoritative_lag_ms" lag;
                   Hashtbl.remove ws.open_provisional_since_ms key);
              Hashtbl.remove ws.open_parse_generation key
            ) else
              Hashtbl.replace ws.open_provisional_since_ms
                (Uri_path.docuri_to_string uri)
                (Perf_stats.now_ms ());
            invalidate_lsif_snapshot ws;
            Hashtbl.replace ws.files path_key parsed_doc;
            Hashtbl.replace ws.bg_parsed path_key true;
            Hashtbl.remove ws.closed_doc_last_touch path_key;
            enqueue_doc_imports_high ws parsed_doc;
            (match compool_key_of_doc parsed_doc with
             | None -> ()
             | Some key ->
                 let importers = importer_uris_for_compool_key ws ~compool_key:key in
                 invalidate_importer_nav_state_for_compool_key ws ~compool_key:key;
                 List.iter
                   (fun importer_uri ->
                     enqueue_open_diag_revalidate ws ~uri:importer_uri ~reason:"compool_change")
                   importers)
          )
        )
    | None ->
        let uri =
          match Uri_path.docuri_of_path path with
          | Some u -> u
          | None ->
              (match T.DocumentUri.t_of_yojson (`String (Uri_path.file_uri_of_path path)) with
               | u -> u
               | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))
        in
        let doc_opt =
          match file_size_bytes path with
          | Some n when is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes ~text_len:n ->
              Some (make_doc_with_parse_guard ws ~uri ~file:(Some path) ~text:"" ~actual_bytes:n)
          | _ ->
              (match read_file_text path with
               | None -> None
               | Some text ->
                   let doc0 =
                     try parse_guarded_document_make ws ~uri ~file:(Some path) ~text
                     with exn ->
                       let fallback = Document.make ~uri ~file:(Some path) ~text:"" in
                       with_internal_phase_diag fallback ~phase:"bg-open-doc" ~exn
                   in
                   Some (background_doc_with_diags ws doc0))
        in
        (match doc_opt with
         | None -> ()
         | Some doc ->
             invalidate_lsif_snapshot ws;
             Hashtbl.replace ws.files path_key doc;
             Hashtbl.replace ws.bg_parsed path_key true;
             touch_closed_doc_path ws ~path_key;
             evict_closed_docs_if_needed ws;
             (match compool_key_of_doc doc with
              | None -> ()
             | Some key ->
                  let importers = importer_uris_for_compool_key ws ~compool_key:key in
                  invalidate_importer_nav_state_for_compool_key ws ~compool_key:key;
                  List.iter
                    (fun importer_uri ->
                      enqueue_open_diag_revalidate ws ~uri:importer_uri ~reason:"compool_change")
                    importers);
             queue_workspace_diag_update_for_doc ws doc)

let revalidate_closed_docs_for_hint_readiness (ws:t) : unit =
  Hashtbl.iter (fun path_key doc ->
    if has_open_doc_for_path_key ws ~path_key then ()
    else
      let updated = background_doc_with_diags ws doc in
      if Document.diagnostics updated <> Document.diagnostics doc then (
        Hashtbl.replace ws.files path_key updated;
        queue_workspace_diag_update_for_doc ws updated
      )
  ) ws.files

let maybe_build_symbol_hints_background (ws:t) : unit =
  if ws.symbol_hints <> None then ()
  else
    match ws.index with
    | None -> ()
    | Some idx ->
        if Queue.is_empty ws.bg_high_small_queue
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
          Perf_stats.tick "diag.warmup_revalidate"
        )

let background_tick
    (ws:t)
    ~(budget_ms:int)
    ~(mode:bg_tick_mode)
    ~(idle_quiet_ms:int)
    ~(last_message_ms:float)
  : unit =
  if budget_ms <= 0 then ()
  else (
    ensure_parse_worker_started ws;
    Perf_stats.tick "bg.tick";
    Perf_stats.tick "startup.phase_tick";
    update_pressure_state ws;
    pump_index_background ws;
    if workspace_pressure_mode ws <> PressureCritical then
      seed_bg_paths_from_index ws;
    let diag_stage_pending =
      ws.startup_diag_hover_ready_ms = None
      || Hashtbl.length ws.open_parse_generation > 0
      || not (startup_open_diag_revalidate_empty ws)
    in
    let parse_result_budget =
      match mode with
      | BgTickInteractive ->
          if diag_stage_pending then 10
          else if ws.startup_fully_nav_ready_ms = None then 4
          else 2
      | BgTickIdle -> 6
    in
    drain_parse_worker_results ws ~max_items:parse_result_budget;
    let quick_nav_budget =
      match mode with
      | BgTickInteractive ->
          if diag_stage_pending then
            max 1 (budget_ms / 6)
          else if ws.startup_fully_nav_ready_ms = None then
            max 2 (budget_ms / 2)
          else
            max 1 (budget_ms / 4)
      | BgTickIdle -> max 1 (budget_ms / 3)
    in
    quick_nav_index_step ws ~budget_ms:quick_nav_budget;
    let budget =
      match workspace_pressure_mode ws with
      | PressureNormal -> float_of_int budget_ms
      | PressureSoft -> float_of_int (max 1 (budget_ms / 2))
      | PressureCritical -> float_of_int (max 1 (budget_ms / 4))
    in
    let required_idle_quiet_ms =
      max 0 (max idle_quiet_ms ws.bg_large_parse_idle_quiet_ms)
    in
    let allow_normal_large =
      match mode with
      | BgTickInteractive ->
          ws.startup_fully_nav_ready_ms = None
          && Queue.is_empty ws.bg_high_large_queue
          && Hashtbl.length ws.parse_worker_inflight < ws.parse_worker_max_inflight
      | BgTickIdle ->
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
      ws.startup_diag_hover_ready_ms = None
      || Hashtbl.length ws.open_parse_generation > 0
    in
    let processed_total = ref 0 in
    let processed_open = ref 0 in
    let required_open_share =
      let pct = max 0 (min 100 ws.sched_open_doc_min_share_pct) in
      float_of_int pct /. 100.0
    in
    if mode = BgTickIdle
       && (not allow_normal_large)
       && not (Queue.is_empty ws.bg_norm_large_queue)
    then
      Perf_stats.tick "bg.parse.large_deferred_busy";
    let deadline = Perf_stats.now_ms () +. budget in
    let keep = ref true in
    while !keep && Perf_stats.now_ms () < deadline do
      let prefer_open =
        if not prefer_open_base then false
        else if !processed_total <= 0 then true
        else
          let share = float_of_int !processed_open /. float_of_int !processed_total in
          share < required_open_share
      in
      match dequeue_bg_path ws ~mode ~allow_normal_large ~allow_root_large ~prefer_open with
      | None ->
          keep := false
      | Some (path, prio) ->
          incr processed_total;
          let path_key = normalize_path_key path in
          if has_open_doc_for_path_key ws ~path_key then incr processed_open;
          (match lane_of_bg_queue_kind prio with
           | LaneOpen -> Perf_stats.tick "sched.lane_a_ticks"
           | LaneRoot -> Perf_stats.tick "sched.lane_b_ticks"
           | LaneSweep -> Perf_stats.tick "sched.lane_c_ticks");
          if prio = BgQueueNormalSmall
             && workspace_pressure_mode ws = PressureCritical
          then (
            enqueue_bg_path ws ~lane:LaneSweep ~reason_group:"pressure_backoff" ~high:false path;
            keep := false
          ) else (
            (match prio with
             | BgQueueHighSmall -> Perf_stats.tick "bg.queue_high"
             | BgQueueRootSmall -> Perf_stats.tick "bg.queue_root"
             | BgQueueNormalSmall -> Perf_stats.tick "bg.queue_normal"
             | BgQueueHighLarge | BgQueueRootLarge | BgQueueNormalLarge ->
                 Perf_stats.tick "bg.parse.large_started");
            (match prio with
             | BgQueueHighLarge | BgQueueRootLarge | BgQueueNormalLarge ->
                 if try_submit_large_parse_job ws ~path ~queue_kind:prio then
                   ()
                 else (
                   let lane =
                     match prio with
                     | BgQueueRootLarge -> LaneRoot
                     | BgQueueHighLarge ->
                         if has_open_doc_for_path_key ws ~path_key:(normalize_path_key path) then
                           LaneOpen
                         else
                           LaneSweep
                     | _ -> LaneSweep
                   in
                   enqueue_bg_path ws ~lane ~high:(prio = BgQueueHighLarge || prio = BgQueueRootLarge) path;
                   keep := false
                 )
             | BgQueueHighSmall | BgQueueRootSmall | BgQueueNormalSmall ->
                 ignore
                   (Perf_stats.time "bg.parse_doc" (fun () ->
                      background_parse_path ws path)))
          )
    done
    ;
    maybe_build_symbol_hints_background ws;
    update_startup_ready_state ws
  )

let drain_pending_diag_updates
    (ws:t)
    ~(max_items:int)
  : (T.DocumentUri.t * T.Diagnostic.t list) list =
  if max_items <= 0 then []
  else if not (bg_diag_allowed ws) then []
  else
    let rec loop n acc =
      if n <= 0 || Queue.is_empty ws.bg_pending_diag_updates then
        List.rev acc
      else
        let key = Queue.pop ws.bg_pending_diag_updates in
        Hashtbl.remove ws.bg_pending_diag_set key;
        (match Hashtbl.find_opt ws.bg_pending_diag_payloads key with
         | None ->
             loop n acc
         | Some (uri, diags) ->
             Hashtbl.remove ws.bg_pending_diag_payloads key;
             if Hashtbl.mem ws.docs uri then (
               Perf_stats.tick "diag.open.stale_generation_drop";
               loop n acc
             ) else
               loop (n - 1) ((uri, diags) :: acc))
    in
    loop max_items []

let drain_open_diag_revalidate_uris
    (ws:t)
    ~(max_items:int)
  : T.DocumentUri.t list =
  if max_items <= 0 then []
  else
    let rec loop n acc =
      if n <= 0 || Queue.is_empty ws.open_diag_revalidate_updates then
        List.rev acc
      else
        let key = Queue.pop ws.open_diag_revalidate_updates in
        Hashtbl.remove ws.open_diag_revalidate_set key;
        match Hashtbl.find_opt ws.open_diag_revalidate_payloads key with
        | None ->
            loop n acc
        | Some (uri, _reason) ->
            Hashtbl.remove ws.open_diag_revalidate_payloads key;
            (match Hashtbl.find_opt ws.docs uri with
             | None ->
                 loop n acc
             | Some doc ->
                 store_doc ~import_lookup_pump:false ws uri doc;
                 Perf_stats.tick "diag.open.revalidate_drained";
                 loop (n - 1) (uri :: acc))
    in
    let out = loop max_items [] in
    if out <> [] then update_startup_ready_state ws;
    out

let startup_diag_hover_ready_now (ws:t) : bool =
  update_startup_ready_state ws;
  ws.startup_diag_hover_ready_ms <> None

let startup_is_ready_now (ws:t) : bool =
  update_startup_ready_state ws;
  ws.startup_ready_ms <> None

let startup_readiness_json_for_report (ws:t) : Yojson.Safe.t =
  update_startup_ready_state ws;
  startup_readiness_json ws

let workspace_ready_event_json (ws:t) : Yojson.Safe.t option =
  consume_workspace_ready_event_json ws

let startup_phase_event_json (ws:t) : Yojson.Safe.t option =
  consume_startup_phase_event_json ws

let startup_miss_event_json (ws:t) : Yojson.Safe.t option =
  consume_startup_miss_event_json ws

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

let diagnostics_for (ws:t) ~(uri:T.DocumentUri.t) : T.Diagnostic.t list =
  match Hashtbl.find_opt ws.docs uri with
  | None -> []
  | Some doc -> Document.diagnostics doc

let ast_dump_for (ws:t) ~(uri:T.DocumentUri.t) : string option =
  match Hashtbl.find_opt ws.docs uri with
  | None -> None
  | Some doc -> Document.ast_dump doc

let cst_dump_for (ws:t) ~(uri:T.DocumentUri.t) : string option =
  match Hashtbl.find_opt ws.docs uri with
  | None -> None
  | Some doc ->
      Lexer.with_session_state (fun () ->
        let lexbuf = Lexing.from_string doc.Document.text in
        (match doc.Document.file with
         | None -> ()
         | Some f ->
             lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = f });
        let b = Buffer.create 4096 in
        Buffer.add_string b "CST (token stream)\n";
        let add_token_row idx tok sp ep lexeme =
          let line0 = max 1 sp.Lexing.pos_lnum in
          let line1 = max 1 ep.Lexing.pos_lnum in
          let col0 = max 0 (sp.Lexing.pos_cnum - sp.Lexing.pos_bol) in
          let col1 = max 0 (ep.Lexing.pos_cnum - ep.Lexing.pos_bol) in
          let tok_s = Parse.Debug.string_of_token tok in
          let lex = String.escaped lexeme in
          Buffer.add_string b
            (Printf.sprintf
               "%5d  %-14s %-28s @ %d:%d-%d:%d\n"
               idx
               tok_s
               ("\"" ^ lex ^ "\"")
               line0
               col0
               line1
               col1)
        in
        let rec loop idx =
          let tok = Lexer.token lexbuf in
          let sp = Lexing.lexeme_start_p lexbuf in
          let ep = Lexing.lexeme_end_p lexbuf in
          let lexeme = Lexing.lexeme lexbuf in
          add_token_row idx tok sp ep lexeme;
          match tok with
          | Parser.EOF -> ()
          | _ -> loop (idx + 1)
        in
        (try loop 1 with exn ->
           Buffer.add_string b
             (Printf.sprintf
                "\n<tokenization stopped: %s>\n"
                (Printexc.to_string exn)));
        Some (Buffer.contents b)
      )

let lsp_pos_of_lex (p:Lexing.position) : Yojson.Safe.t =
  let line0 = max 0 (p.pos_lnum - 1) in
  let col0 = max 0 (p.pos_cnum - p.pos_bol) in
  `Assoc [ "line", `Int line0; "character", `Int col0 ]

let lsp_range_of_lex (sp:Lexing.position) (ep:Lexing.position) : Yojson.Safe.t =
  `Assoc [ "start", lsp_pos_of_lex sp; "end", lsp_pos_of_lex ep ]

type def = {
  uri : T.DocumentUri.t;
  name : string;
  key : string;
  loc : Ast.Loc.t;
  kind : int;
  container : string option;
}

let def_key (d:def) : string =
  Printf.sprintf "%s|%d|%d|%d|%d|%s"
    (Uri_path.docuri_to_string d.uri)
    d.loc.Ast.Loc.start_pos.line
    d.loc.Ast.Loc.start_pos.col
    d.loc.Ast.Loc.end_pos.line
    d.loc.Ast.Loc.end_pos.col
    d.key

let loc_key ~(uri:T.DocumentUri.t) (loc:Ast.Loc.t) : string =
  Printf.sprintf "%s|%d|%d|%d|%d"
    (Uri_path.docuri_to_string uri)
    loc.Ast.Loc.start_pos.line
    loc.Ast.Loc.start_pos.col
    loc.Ast.Loc.end_pos.line
    loc.Ast.Loc.end_pos.col

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' | '\'' -> true
  | _ -> false

let ast_pos_of_offset (idx:Text_index.t) (off:int) : Ast.Loc.pos =
  let line0, col0 = Text_index.line_col_of_offset idx off in
  { Ast.Loc.line = line0 + 1; col = col0; offset = off }

let loc_of_offsets ~(file:string option) ~(idx:Text_index.t) ~(s:int) ~(e:int) : Ast.Loc.t =
  Ast.Loc.make ~file ~start_pos:(ast_pos_of_offset idx s) ~end_pos:(ast_pos_of_offset idx e)

let add_def_raw (acc:def list) ~(uri:T.DocumentUri.t) ~(name:string) ~(loc:Ast.Loc.t) ~(kind:int) ~(container:string option) : def list =
  let key = normalize_name name in
  if key = "" then acc else { uri; name; key; loc; kind; container } :: acc

let add_ident_def (acc:def list) ~(uri:T.DocumentUri.t) ~(id:Ast.ident) ~(kind:int) ~(container:string option) : def list =
  add_def_raw acc ~uri ~name:id.v ~loc:id.loc ~kind ~container

let sym_kind_module = 2
let sym_kind_type = 5
let sym_kind_field = 8
let sym_kind_func = 12
let sym_kind_var = 13
let sym_kind_const = 14

let def_of_preprocess_define (doc:Document.t) (d:Preprocess.define) : def =
  {
    uri = doc.Document.uri;
    name = d.name;
    key = d.key;
    loc = d.loc;
    kind = sym_kind_const;
    container = None;
  }

let nth_opt (xs:'a list) (n:int) : 'a option =
  let rec go i = function
    | [] -> None
    | x :: tl ->
        if i = 0 then Some x else go (i - 1) tl
  in
  if n < 0 then None else go n xs

let tokenize_ident_words (line:string) : (string * int * int) list =
  let n = String.length line in
  let rec scan i acc =
    if i >= n then List.rev acc
    else if is_ident_char line.[i] then
      let j = ref (i + 1) in
      while !j < n && is_ident_char line.[!j] do incr j done;
      let tok = String.sub line i (!j - i) in
      scan !j ((tok, i, !j) :: acc)
    else
      scan (i + 1) acc
  in
  scan 0 []

let preceded_by_bang (line:string) (col:int) : bool =
  col > 0 && line.[col - 1] = '!'

let classify_fallback_decl ~(line:string) (tokens:(string * int * int) list) : (int * int) option =
  let classify kw_idx kw kw_col =
    match kw with
    | "ITEM" | "TABLE" -> Some (kw_idx, sym_kind_var)
    | "TYPE" -> Some (kw_idx, sym_kind_type)
    | "PROC" -> Some (kw_idx, sym_kind_func)
    | "DEFINE" -> Some (kw_idx, sym_kind_const)
    | "BLOCK" -> Some (kw_idx, sym_kind_module)
    | "COMPOOL" ->
        if preceded_by_bang line kw_col then None else Some (kw_idx, sym_kind_module)
    | _ -> None
  in
  let token_upper i =
    match nth_opt tokens i with
    | None -> None
    | Some (w, col, _) -> Some (normalize_name w, col)
  in
  match token_upper 0 with
  | None -> None
  | Some (kw0, col0) ->
      (match classify 0 kw0 col0 with
       | Some _ as hit -> hit
       | None ->
           if kw0 = "DEF" || kw0 = "REF" || kw0 = "STATIC" || kw0 = "CONSTANT" then
             (match token_upper 1 with
              | None -> None
              | Some (kw1, col1) -> classify 1 kw1 col1)
           else
             None)

let fallback_line_defs (doc:Document.t) : def list =
  let uri = doc.Document.uri in
  let idx = doc.Document.index in
  let file = doc.Document.file in
  let lines = String.split_on_char '\n' doc.Document.text in
  let defs_rev =
    lines
    |> List.mapi (fun line0 line -> (line0, line))
    |> List.fold_left (fun acc (line0, line) ->
         let tokens = tokenize_ident_words line in
         match classify_fallback_decl ~line tokens with
         | None -> acc
         | Some (kw_idx, kind) ->
             (match nth_opt tokens (kw_idx + 1), Text_index.line_start_offset idx ~line:line0 with
              | Some (name, c0, c1), Some base ->
                  let s = base + c0 in
                  let e = base + c1 in
                  let loc = loc_of_offsets ~file ~idx ~s ~e in
                  add_def_raw acc ~uri ~name ~loc ~kind ~container:None
              | _ -> acc)
       ) []
  in
  List.rev defs_rev

let max_fallback_scan_lines = 75_000

let rec collect_type_defs ~(uri:T.DocumentUri.t) ~(container:string option) (acc:def list) (t:Ast.type_expr Ast.node) : def list =
  match t.v with
  | Ast.TName _ -> acc
  | Ast.TPointer inner -> collect_type_defs ~uri ~container acc inner
  | Ast.TArray { elem; _ } -> collect_type_defs ~uri ~container acc elem
  | Ast.TRecord fields ->
      List.fold_left (fun a f ->
        let fv = f.v in
        let a = add_ident_def a ~uri ~id:fv.fname ~kind:sym_kind_field ~container in
        collect_type_defs ~uri ~container a fv.ftype
      ) acc fields
  | Ast.TFunc { params; returns } ->
      let acc =
        List.fold_left (fun a p ->
          let pv = p.v in
          let a = add_ident_def a ~uri ~id:pv.pname ~kind:sym_kind_var ~container in
          collect_type_defs ~uri ~container a pv.ptype
        ) acc params
      in
      (match returns with None -> acc | Some r -> collect_type_defs ~uri ~container acc r)

let rec collect_stmt_defs ~(uri:T.DocumentUri.t) ~(container:string option) (acc:def list) (s:Ast.stmt Ast.node) : def list =
  match s.v with
  | Ast.SEmpty -> acc
  | Ast.SDecl d -> collect_decl_defs ~uri ~container acc d
  | Ast.SBlock xs ->
      List.fold_left (collect_stmt_defs ~uri ~container) acc xs
  | Ast.SAssign _ -> acc
  | Ast.SCallStmt _ -> acc
  | Ast.SIf { then_; else_; _ } ->
      let acc = collect_stmt_defs ~uri ~container acc then_ in
      (match else_ with None -> acc | Some e -> collect_stmt_defs ~uri ~container acc e)
  | Ast.SWhile { body; _ } ->
      collect_stmt_defs ~uri ~container acc body
  | Ast.SFor { init; step; body; _ } ->
      let acc = (match init with None -> acc | Some i -> collect_stmt_defs ~uri ~container acc i) in
      let acc = (match step with None -> acc | Some st -> collect_stmt_defs ~uri ~container acc st) in
      collect_stmt_defs ~uri ~container acc body
  | Ast.SReturn _ -> acc
  | Ast.SLabel { label; body } ->
      let acc = add_ident_def acc ~uri ~id:label ~kind:sym_kind_var ~container in
      collect_stmt_defs ~uri ~container acc body
  | Ast.SGoto _ -> acc

and collect_decl_defs ~(uri:T.DocumentUri.t) ~(container:string option) (acc:def list) (d:Ast.decl Ast.node) : def list =
  match d.v with
  | Ast.DVar { name; dtype; _ } ->
      let acc = add_ident_def acc ~uri ~id:name ~kind:sym_kind_var ~container in
      collect_type_defs ~uri ~container acc dtype
  | Ast.DConst { name; dtype; _ } ->
      let acc = add_ident_def acc ~uri ~id:name ~kind:sym_kind_const ~container in
      (match dtype with None -> acc | Some ty -> collect_type_defs ~uri ~container acc ty)
  | Ast.DType { name; defn } ->
      let acc = add_ident_def acc ~uri ~id:name ~kind:sym_kind_type ~container in
      collect_type_defs ~uri ~container:(Some name.v) acc defn
  | Ast.DProc p ->
      let pv = p.v in
      let proc_name = pv.name.v in
      let acc = add_ident_def acc ~uri ~id:pv.name ~kind:sym_kind_func ~container in
      let in_proc = Some proc_name in
      let acc =
        List.fold_left (fun a prm ->
          let prm_v = prm.v in
          let a = add_ident_def a ~uri ~id:prm_v.pname ~kind:sym_kind_var ~container:in_proc in
          collect_type_defs ~uri ~container:in_proc a prm_v.ptype
        ) acc pv.params
      in
      let acc = List.fold_left (collect_decl_defs ~uri ~container:in_proc) acc pv.locals in
      collect_stmt_defs ~uri ~container:in_proc acc pv.body
  | Ast.DDirective _ -> acc

let find_compool_loc_in_doc (doc:Document.t) (key:string) : Ast.Loc.t option =
  let match_directive (d:Ast.decl Ast.node) =
    match d.v with
    | Ast.DDirective { name; args = first :: _ } ->
        let dir = normalize_name name.v in
        if (dir = "COMPOOL" || dir = "ICOMPOOL") && normalize_name first.v = key then
          Some first.loc
        else
          None
    | _ -> None
  in
  match doc.Document.ast with
  | None -> None
  | Some prog ->
      let rec go = function
        | [] -> None
        | Ast.TopDecl d :: tl ->
            (match match_directive d with
             | Some _ as hit -> hit
             | None -> go tl)
        | _ :: tl -> go tl
      in
      go prog

let collect_doc_defs (doc:Document.t) : def list =
  let uri = doc.Document.uri in
  let defs0 =
    match doc.Document.ast with
    | None -> []
    | Some prog ->
        List.fold_left (fun acc top ->
          match top with
            | Ast.TopDecl d -> collect_decl_defs ~uri ~container:None acc d
            | Ast.TopStmt s -> collect_stmt_defs ~uri ~container:None acc s
        ) [] prog
  in
  let defs =
    List.fold_left (fun acc (dm:Preprocess.define) ->
      add_def_raw
        acc
        ~uri
        ~name:dm.name
        ~loc:dm.loc
        ~kind:sym_kind_const
        ~container:None
    ) defs0 doc.Document.defines
  in
  let use_fallback_scan =
    let broken_or_partial = doc.Document.ast = None || doc.Document.parse_diags <> [] in
    broken_or_partial
    && Text_index.line_count doc.Document.index <= max_fallback_scan_lines
  in
  let defs =
    if use_fallback_scan then
      fallback_line_defs doc
      |> List.fold_left (fun acc d -> d :: acc) defs
    else
      defs
  in
  match doc.Document.compool_def with
  | None -> List.rev defs
  | Some name ->
      let k = normalize_name name in
      let defs =
        match find_compool_loc_in_doc doc k with
        | None -> defs
        | Some loc ->
            add_def_raw defs ~uri ~name:k ~loc ~kind:sym_kind_module ~container:None
      in
      List.rev defs

let position_in_loc (pos:T.Position.t) (loc:Ast.Loc.t) : bool =
  let line = pos.T.Position.line + 1 in
  let col = pos.T.Position.character in
  let sp = loc.Ast.Loc.start_pos in
  let ep = loc.Ast.Loc.end_pos in
  (line > sp.line || (line = sp.line && col >= sp.col))
  && (line < ep.line || (line = ep.line && col <= ep.col))

let word_at_position (doc:Document.t) (pos:T.Position.t) : (string * Ast.Loc.t) option =
  match Text_index.offset_of_line_col doc.Document.index ~line:pos.T.Position.line ~col:pos.T.Position.character with
  | None -> None
  | Some off ->
      let text = doc.Document.text in
      let n = String.length text in
      if n = 0 then None
      else
        let pivot =
          if off < n && is_ident_char text.[off] then Some off
          else if off > 0 && off - 1 < n && is_ident_char text.[off - 1] then Some (off - 1)
          else None
        in
        match pivot with
        | None -> None
        | Some i ->
            let a = ref i in
            while !a > 0 && is_ident_char text.[!a - 1] do decr a done;
            let b = ref (i + 1) in
            while !b < n && is_ident_char text.[!b] do incr b done;
            if !b <= !a then None
            else
              let name = String.sub text !a (!b - !a) in
              let loc = loc_of_offsets ~file:doc.Document.file ~idx:doc.Document.index ~s:!a ~e:!b in
              Some (name, loc)

let nav_word_at_position (doc:Document.t) (pos:T.Position.t) : (string * Ast.Loc.t) option =
  match word_at_position doc pos with
  | None -> None
  | Some (name, _) when is_reserved_keyword name -> None
  | Some x -> Some x

let has_define_key (doc:Document.t) (key:string) : bool =
  key <> "" && List.exists (fun (d:Preprocess.define) -> d.key = key) doc.Document.defines

let find_define_key_in_word
    (doc:Document.t)
    ~(word:string)
    ~(word_loc:Ast.Loc.t)
    ~(cursor_col:int)
  : string option =
  let direct = normalize_name word in
  if has_define_key doc direct then Some direct
  else
    let upper_word = String.uppercase_ascii word in
    let n = String.length upper_word in
    if n = 0 then None
    else
      let rel =
        let r = cursor_col - word_loc.start_pos.col in
        if r < 0 then 0 else if r >= n then n - 1 else r
      in
      let uniq_keys_tbl = Hashtbl.create 16 in
      List.iter (fun (d:Preprocess.define) -> Hashtbl.replace uniq_keys_tbl d.key true) doc.Document.defines;
      let keys = Hashtbl.fold (fun k _ acc -> k :: acc) uniq_keys_tbl [] in
      let best : (string * int * int) option ref = ref None in
      let consider key start_idx len =
        match !best with
        | None -> best := Some (key, len, start_idx)
        | Some (_, best_len, best_start) ->
            if len > best_len || (len = best_len && start_idx >= best_start) then
              best := Some (key, len, start_idx)
      in
      List.iter (fun key ->
        let m = String.length key in
        if m > 0 && m <= n then (
          let rec scan i =
            if i + m > n then ()
            else (
              let rec eq j =
                j = m || (upper_word.[i + j] = key.[j] && eq (j + 1))
              in
              if eq 0 && rel >= i && rel < i + m then consider key i m;
              scan (i + 1)
            )
          in
          scan 0
        )
      ) keys;
      match !best with
      | None -> None
      | Some (key, _, _) -> Some key

let is_ws_char = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let skip_ws_forward (s:string) (i:int) : int =
  let n = String.length s in
  let rec go j =
    if j < n && is_ws_char s.[j] then go (j + 1) else j
  in
  go i

let parse_call_arg_count ~(text:string) ~(open_idx:int) : int option =
  let n = String.length text in
  if open_idx < 0 || open_idx >= n || text.[open_idx] <> '(' then None
  else
    let depth = ref 1 in
    let in_single = ref false in
    let in_double = ref false in
    let comma_count = ref 0 in
    let seen_non_ws = ref false in
    let i = ref (open_idx + 1) in
    let done_ = ref false in
    while not !done_ && !i < n do
      let c = text.[!i] in
      if !in_single then (
        if c = '\'' then
          if !i + 1 < n && text.[!i + 1] = '\'' then
            i := !i + 1
          else
            in_single := false
      ) else if !in_double then (
        if c = '"' then
          if !i + 1 < n && text.[!i + 1] = '"' then
            i := !i + 1
          else
            in_double := false
      ) else (
        match c with
        | '\'' ->
            if !depth = 1 then seen_non_ws := true;
            in_single := true
        | '"' ->
            if !depth = 1 then seen_non_ws := true;
            in_double := true
        | '(' ->
            if !depth = 1 then seen_non_ws := true;
            incr depth
        | ')' ->
            decr depth;
            if !depth = 0 then done_ := true
        | ',' when !depth = 1 ->
            incr comma_count
        | _ ->
            if !depth = 1 && not (is_ws_char c) then seen_non_ws := true
      );
      incr i
    done;
    if !depth <> 0 then None
    else if not !seen_non_ws then Some 0
    else Some (!comma_count + 1)

let select_define_decl
    (doc:Document.t)
    ~(key:string)
    ~(call_ctx:bool)
    ~(arg_count:int option)
    ~(cursor_off:int option)
  : Preprocess.define option =
  let defs0 =
    doc.Document.defines
    |> List.filter (fun (d:Preprocess.define) -> d.key = key)
  in
  let defs1 =
    match cursor_off with
    | None -> defs0
    | Some off ->
        let before =
          defs0
          |> List.filter (fun (d:Preprocess.define) -> d.decl_start_off <= off)
        in
        if before = [] then defs0 else before
  in
  let defs2 =
    let same_call =
      defs1
      |> List.filter (fun (d:Preprocess.define) -> d.requires_call = call_ctx)
    in
    if same_call = [] then defs1 else same_call
  in
  let defs3 =
    match arg_count with
    | None -> defs2
    | Some n ->
        let same_arity =
          defs2
          |> List.filter (fun (d:Preprocess.define) -> List.length d.formals = n)
        in
        if same_arity = [] then defs2 else same_arity
  in
  defs3
  |> List.fold_left (fun best (d:Preprocess.define) ->
       match best with
       | None -> Some d
       | Some (cur:Preprocess.define) ->
           if d.decl_start_off >= cur.decl_start_off then Some d else Some cur
     ) None

let define_under_cursor (doc:Document.t) (pos:T.Position.t)
  : (Preprocess.define * Ast.Loc.t) option =
  match word_at_position doc pos with
  | None -> None
  | Some (word, word_loc) ->
      let key_opt =
        find_define_key_in_word doc ~word ~word_loc ~cursor_col:pos.character
      in
      (match key_opt with
       | None -> None
       | Some key ->
           let cursor_off =
             Text_index.offset_of_line_col doc.Document.index ~line:pos.line ~col:pos.character
           in
           let end_line0 = max 0 (word_loc.end_pos.line - 1) in
           let after_word_off =
             Text_index.offset_of_line_col
               doc.Document.index
               ~line:end_line0
               ~col:word_loc.end_pos.col
           in
           let call_ctx, arg_count =
             match after_word_off with
             | None -> (false, None)
             | Some off ->
                 let open_idx = skip_ws_forward doc.Document.text off in
                 if open_idx < String.length doc.Document.text && doc.Document.text.[open_idx] = '('
                 then
                   let argc = parse_call_arg_count ~text:doc.Document.text ~open_idx in
                   (true, argc)
                 else
                   (false, None)
           in
           (match select_define_decl doc ~key ~call_ctx ~arg_count ~cursor_off with
            | None -> None
            | Some d -> Some (d, word_loc)))

let location_json ~(uri:T.DocumentUri.t) (loc:Ast.Loc.t) : Yojson.Safe.t =
  T.Location.yojson_of_t (Lsp_conv.location_of_loc ~uri loc)

let range_json_of_loc (loc:Ast.Loc.t) : Yojson.Safe.t =
  T.Range.yojson_of_t (Lsp_conv.range_of_loc loc)

let symbol_json (d:def) : Yojson.Safe.t =
  let base =
    [
      ("name", `String d.name);
      ("kind", `Int d.kind);
      ("location", location_json ~uri:d.uri d.loc);
    ]
  in
  let base =
    match d.container with
    | None -> base
    | Some c -> ("containerName", `String c) :: base
  in
  `Assoc base

let doc_at_path (ws:t) (path:string) : Document.t option =
  doc_from_path_cached ws path

let doc_at_path_cached (ws:t) (path:string) : Document.t option =
  doc_from_path_cached_only ws path

let resolve_import_paths (ws:t) (doc:Document.t) : string list =
  match ws.index with
  | None -> []
  | Some idx ->
      let acc = Hashtbl.create 16 in
      List.iter (fun (imp:Preprocess.import) ->
        match imp.kind with
        | Preprocess.Compool ->
            (match Workspace_index.find_compool idx ~name:imp.name with
             | None -> ()
             | Some p -> Hashtbl.replace acc (normalize_path_key p) p)
      ) (Document.imports doc);
      Hashtbl.fold (fun _ p xs -> p :: xs) acc []

let docs_for_lookup (ws:t) (doc:Document.t) : Document.t list =
  let seen = Hashtbl.create 32 in
  let out = ref [] in
  let add_doc (d:Document.t) =
    let u = Uri_path.docuri_to_string d.Document.uri in
    if not (Hashtbl.mem seen u) then (
      Hashtbl.add seen u true;
      out := d :: !out
    )
  in
  add_doc doc;
  resolve_import_paths ws doc
  |> List.iter (fun p ->
       match doc_at_path_cached ws p with
       | None ->
           enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"lookup_import" ~high:true p
       | Some d ->
           add_doc d);
  List.rev !out

let has_unscoped_fallback_context (doc:Document.t) : bool =
  doc.Document.ast = None
  || doc.Document.parse_diags <> []
  || Document.imports doc = []

let docs_for_rename (ws:t) (doc:Document.t) : Document.t list =
  let seen = Hashtbl.create 64 in
  let out = ref [] in
  let add_doc (d:Document.t) =
    let u = Uri_path.docuri_to_string d.Document.uri in
    if not (Hashtbl.mem seen u) then (
      Hashtbl.add seen u true;
      out := d :: !out
    )
  in
  docs_for_lookup ws doc |> List.iter add_doc;
  if has_unscoped_fallback_context doc then
    Hashtbl.iter (fun _ d -> add_doc d) ws.files;
  Hashtbl.iter (fun _ d -> add_doc d) ws.docs;
  List.rev !out

let compare_pos (a:T.Position.t) (b:T.Position.t) : int =
  if a.line < b.line then -1
  else if a.line > b.line then 1
  else if a.character < b.character then -1
  else if a.character > b.character then 1
  else 0

let pos_in_range (p:T.Position.t) (r:T.Range.t) : bool =
  compare_pos r.start p <= 0 && compare_pos p r.end_ <= 0

let kind_name (k:int) : string =
  if k = sym_kind_module then "module"
  else if k = sym_kind_type then "type"
  else if k = sym_kind_field then "field"
  else if k = sym_kind_func then "procedure"
  else if k = sym_kind_var then "item"
  else if k = sym_kind_const then "constant"
  else "symbol"

type nav_binding = {
  sym_id : string;
  decl : def;
}

type nav_scope = {
  values : (string, nav_binding) Hashtbl.t;
  types : (string, nav_binding) Hashtbl.t;
  labels : (string, nav_binding) Hashtbl.t;
  fields : (string, nav_binding) Hashtbl.t;
}

type doc_nav = {
  defs_by_id : (string, def) Hashtbl.t;
  occs_by_id : (string, (T.DocumentUri.t * Ast.Loc.t) list) Hashtbl.t;
  seen_occ : (string, bool) Hashtbl.t;
}

let nav_scope_empty () : nav_scope =
  {
    values = Hashtbl.create 64;
    types = Hashtbl.create 32;
    labels = Hashtbl.create 32;
    fields = Hashtbl.create 64;
  }

let nav_scope_copy (s:nav_scope) : nav_scope =
  {
    values = copy_tbl s.values;
    types = copy_tbl s.types;
    labels = copy_tbl s.labels;
    fields = copy_tbl s.fields;
  }

let doc_nav_create () : doc_nav =
  {
    defs_by_id = Hashtbl.create 128;
    occs_by_id = Hashtbl.create 256;
    seen_occ = Hashtbl.create 512;
  }

let def_symbol_id (d:def) : string =
  def_key d

let snapshot_def_of_def ~(sym_id:string) (d:def) : Doc_snapshot.nav_def =
  {
    Doc_snapshot.sym_id = sym_id;
    uri = d.uri;
    name = d.name;
    key = d.key;
    loc = d.loc;
    kind = d.kind;
    container = d.container;
  }

let def_of_snapshot_def (d:Doc_snapshot.nav_def) : def =
  {
    uri = d.uri;
    name = d.name;
    key = d.key;
    loc = d.loc;
    kind = d.kind;
    container = d.container;
  }

let doc_nav_of_snapshot (snap:Doc_snapshot.t) : doc_nav =
  let nav = doc_nav_create () in
  List.iter (fun (sym_id, d) ->
    Hashtbl.replace nav.defs_by_id sym_id (def_of_snapshot_def d)
  ) snap.nav_defs;
  List.iter (fun (sym_id, occs) ->
    Hashtbl.replace nav.occs_by_id sym_id occs;
    List.iter (fun (u, loc) ->
      let k = Printf.sprintf "%s|%s" sym_id (loc_key ~uri:u loc) in
      Hashtbl.replace nav.seen_occ k true
    ) occs
  ) snap.nav_occs;
  nav

let snapshot_for_doc (_ws:t) (doc:Document.t) (nav:doc_nav) : Doc_snapshot.t =
  let path_key =
    match doc.Document.file with
    | None -> None
    | Some p -> Some (normalize_path_key p)
  in
  let nav_defs =
    Hashtbl.fold (fun sym_id d acc ->
      (sym_id, snapshot_def_of_def ~sym_id d) :: acc
    ) nav.defs_by_id []
  in
  let nav_occs =
    Hashtbl.fold (fun sym_id occs acc ->
      (sym_id, occs) :: acc
    ) nav.occs_by_id []
  in
  let symbol_keys_touched =
    nav_defs
    |> List.filter_map (fun (_, d) ->
         let k = normalize_name d.Doc_snapshot.key in
         if k = "" then None else Some k)
    |> List.sort_uniq String.compare
  in
  Doc_snapshot.build
    ~uri:doc.Document.uri
    ~path_key
    ~doc_rev:doc.Document.parse_rev
    ~text:doc.Document.text
    ~imports:doc.Document.imports
    ~defines:doc.Document.defines
    ~compool_def:doc.Document.compool_def
    ~nav_defs
    ~nav_occs
    ~proc_param_map:[]
    ~symbol_keys_touched

let upsert_semantic_snapshot_for_doc_with_nav (ws:t) (doc:Document.t) (nav:doc_nav) : unit =
  if ws.sem_store_enabled then
    Perf_stats.time "snapshot.build" (fun () ->
      let snap = snapshot_for_doc ws doc nav in
      Semantic_store.upsert_snapshot ws.semantic_store snap
    )

let nav_add_occurrence (nav:doc_nav) ~(sym_id:string) ~(uri:T.DocumentUri.t) ~(loc:Ast.Loc.t) : unit =
  let k = Printf.sprintf "%s|%s" sym_id (loc_key ~uri loc) in
  if not (Hashtbl.mem nav.seen_occ k) then (
    Hashtbl.replace nav.seen_occ k true;
    let prev =
      match Hashtbl.find_opt nav.occs_by_id sym_id with
      | None -> []
      | Some xs -> xs
    in
    Hashtbl.replace nav.occs_by_id sym_id ((uri, loc) :: prev)
  )

let nav_add_decl (nav:doc_nav) (d:def) : nav_binding option =
  if d.key = "" then None
  else
    let sym_id = def_symbol_id d in
    Hashtbl.replace nav.defs_by_id sym_id d;
    nav_add_occurrence nav ~sym_id ~uri:d.uri ~loc:d.loc;
    Some { sym_id; decl = d }

let nav_bind_value (scope:nav_scope) (b:nav_binding) : unit =
  Hashtbl.replace scope.values b.decl.key b

let nav_bind_type (scope:nav_scope) (b:nav_binding) : unit =
  Hashtbl.replace scope.types b.decl.key b

let nav_bind_label (scope:nav_scope) (b:nav_binding) : unit =
  Hashtbl.replace scope.labels b.decl.key b

let nav_bind_field (scope:nav_scope) (b:nav_binding) : unit =
  Hashtbl.replace scope.fields b.decl.key b

let nav_bind_decl_default (scope:nav_scope) (b:nav_binding) : unit =
  if b.decl.kind = sym_kind_type then nav_bind_type scope b
  else if b.decl.kind = sym_kind_field then nav_bind_field scope b
  else nav_bind_value scope b

let nav_find_value (scope:nav_scope) (name:string) : nav_binding option =
  Hashtbl.find_opt scope.values (normalize_name name)

let nav_find_type (scope:nav_scope) (name:string) : nav_binding option =
  let k = normalize_name name in
  match Hashtbl.find_opt scope.types k with
  | Some _ as x -> x
  | None -> Hashtbl.find_opt scope.values k

let nav_find_label (scope:nav_scope) (name:string) : nav_binding option =
  let k = normalize_name name in
  match Hashtbl.find_opt scope.labels k with
  | Some _ as x -> x
  | None -> Hashtbl.find_opt scope.values k

let nav_find_field (scope:nav_scope) (name:string) : nav_binding option =
  let k = normalize_name name in
  match Hashtbl.find_opt scope.fields k with
  | Some _ as x -> x
  | None -> Hashtbl.find_opt scope.values k

let def_of_ident ~(uri:T.DocumentUri.t) ~(id:Ast.ident) ~(kind:int) ~(container:string option) : def option =
  let key = normalize_name id.v in
  if key = "" then None
  else Some { uri; name = id.v; key; loc = id.loc; kind; container }

let uniq_defs (xs:def list) : def list =
  let seen = Hashtbl.create 64 in
  let acc = ref [] in
  List.iter (fun d ->
    let k = def_key d in
    if not (Hashtbl.mem seen k) then (
      Hashtbl.add seen k true;
      acc := d :: !acc
    )
  ) xs;
  List.rev !acc

let exported_defs_for_import_scope (doc:Document.t) : def list =
  let block_containers =
    match doc.Document.ast with
    | None -> Hashtbl.create 1
    | Some prog -> block_proc_names_of_program prog
  in
  let is_exported (d:def) : bool =
    if d.kind = sym_kind_field then false
    else
      match d.container with
      | None -> true
      | Some c -> Hashtbl.mem block_containers (normalize_name c)
  in
  collect_doc_defs doc
  |> uniq_defs
  |> List.filter is_exported

let same_uri (a:T.DocumentUri.t) (b:T.DocumentUri.t) : bool =
  Uri_path.docuri_to_string a = Uri_path.docuri_to_string b

let loc_span_weight (loc:Ast.Loc.t) : int =
  let sp = loc.Ast.Loc.start_pos in
  let ep = loc.Ast.Loc.end_pos in
  let line_span = max 0 (ep.line - sp.line) in
  if line_span = 0 then max 0 (ep.col - sp.col)
  else (line_span * 10000) + max 0 ep.col

let symbol_at_position_in_nav (nav:doc_nav) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t)
  : (string * Ast.Loc.t) option =
  let best : (string * Ast.Loc.t * int) option ref = ref None in
  let consider (sym_id:string) (loc:Ast.Loc.t) =
    let span = loc_span_weight loc in
    match !best with
    | None -> best := Some (sym_id, loc, span)
    | Some (_, _, cur) when span < cur -> best := Some (sym_id, loc, span)
    | _ -> ()
  in
  Hashtbl.iter (fun sym_id occs ->
    List.iter (fun (u, loc) ->
      if same_uri u uri && position_in_loc pos loc then consider sym_id loc
    ) occs
  ) nav.occs_by_id;
  match !best with
  | None -> None
  | Some (sym_id, loc, _) -> Some (sym_id, loc)

let build_doc_nav (ws:t) (doc:Document.t) : doc_nav =
  let nav = doc_nav_create () in
  let uri = doc.Document.uri in
  let root_scope = nav_scope_empty () in

  let add_decl_binding
      (scope:nav_scope)
      ~(id:Ast.ident)
      ~(kind:int)
      ~(container:string option)
      (binder:nav_scope -> nav_binding -> unit)
      : unit =
    match def_of_ident ~uri ~id ~kind ~container with
    | None -> ()
    | Some d ->
        (match nav_add_decl nav d with
         | None -> ()
         | Some b -> binder scope b)
  in

  let add_decl_default scope ~id ~kind ~container =
    add_decl_binding scope ~id ~kind ~container nav_bind_decl_default
  in

  let add_decl_value scope ~id ~kind ~container =
    add_decl_binding scope ~id ~kind ~container nav_bind_value
  in

  let add_decl_type scope ~id ~kind ~container =
    add_decl_binding scope ~id ~kind ~container nav_bind_type
  in

  let add_decl_label scope ~id ~kind ~container =
    add_decl_binding scope ~id ~kind ~container nav_bind_label
  in

  let add_decl_field scope ~id ~kind ~container =
    add_decl_binding scope ~id ~kind ~container nav_bind_field
  in

  let bind_external_def (scope:nav_scope) (d:def) : unit =
    if d.key <> "" then (
      let sym_id = def_symbol_id d in
      Hashtbl.replace nav.defs_by_id sym_id d;
      nav_bind_decl_default scope { sym_id; decl = d }
    )
  in

  let add_usage (b:nav_binding option) (id:Ast.ident) : unit =
    match b with
    | None -> ()
    | Some hit ->
        nav_add_occurrence nav ~sym_id:hit.sym_id ~uri ~loc:id.loc
  in

  let use_value (scope:nav_scope) (id:Ast.ident) : unit =
    add_usage (nav_find_value scope id.v) id
  in
  let use_type (scope:nav_scope) (id:Ast.ident) : unit =
    add_usage (nav_find_type scope id.v) id
  in
  let use_label (scope:nav_scope) (id:Ast.ident) : unit =
    add_usage (nav_find_label scope id.v) id
  in
  let use_field (scope:nav_scope) (id:Ast.ident) : unit =
    add_usage (nav_find_field scope id.v) id
  in

  let rec prebind_decl (scope:nav_scope) ~(container:string option) (d:Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DVar { name; _ } ->
        add_decl_default scope ~id:name ~kind:sym_kind_var ~container
    | Ast.DConst { name; _ } ->
        add_decl_default scope ~id:name ~kind:sym_kind_const ~container
    | Ast.DType { name; _ } ->
        add_decl_type scope ~id:name ~kind:sym_kind_type ~container
    | Ast.DProc p ->
        add_decl_value scope ~id:p.v.name ~kind:sym_kind_func ~container
    | Ast.DDirective _ ->
        ()

  and prebind_stmt (scope:nav_scope) ~(container:string option) (s:Ast.stmt Ast.node) : unit =
    match s.v with
    | Ast.SDecl d ->
        prebind_decl scope ~container d
    | Ast.SLabel { label; _ } ->
        add_decl_label scope ~id:label ~kind:sym_kind_var ~container
    | _ ->
        ()

  and walk_type (scope:nav_scope) ~(container:string option) (t:Ast.type_expr Ast.node) : unit =
    match t.v with
    | Ast.TName id ->
        use_type scope id
    | Ast.TPointer inner ->
        walk_type scope ~container inner
    | Ast.TArray { elem; dims } ->
        walk_type scope ~container elem;
        List.iter (walk_expr scope ~container) dims
    | Ast.TRecord fields ->
        List.iter (fun f ->
          let fv = f.v in
          add_decl_field scope ~id:fv.fname ~kind:sym_kind_field ~container;
          walk_type scope ~container fv.ftype
        ) fields
    | Ast.TFunc { params; returns } ->
        let fn_scope = nav_scope_copy scope in
        List.iter (fun prm ->
          add_decl_value fn_scope ~id:prm.v.pname ~kind:sym_kind_var ~container;
          walk_type fn_scope ~container prm.v.ptype
        ) params;
        (match returns with
         | None -> ()
         | Some r -> walk_type fn_scope ~container r)

  and walk_expr (scope:nav_scope) ~(container:string option) (e:Ast.expr Ast.node) : unit =
    match e.v with
    | Ast.EName id ->
        use_value scope id
    | Ast.ELit _ ->
        ()
    | Ast.EUnop { rhs; _ } ->
        walk_expr scope ~container rhs
    | Ast.EBinop { lhs; rhs; _ } ->
        walk_expr scope ~container lhs;
        walk_expr scope ~container rhs
    | Ast.ECall { callee; args } ->
        use_value scope callee;
        List.iter (walk_expr scope ~container) args
    | Ast.EIndex { base; index } ->
        walk_expr scope ~container base;
        List.iter (walk_expr scope ~container) index
    | Ast.EField { base; field } ->
        walk_expr scope ~container base;
        use_field scope field
    | Ast.EAt { field; ptr } ->
        (match field.v with
         | Ast.EName id ->
             use_field scope id
         | Ast.EIndex { base; index } ->
             (match base.v with
              | Ast.EName id ->
                  use_field scope id
              | _ ->
                  walk_expr scope ~container base);
             List.iter (walk_expr scope ~container) index
         | _ ->
             walk_expr scope ~container field);
        walk_expr scope ~container ptr
    | Ast.EDeref { ptr } ->
        walk_expr scope ~container ptr
    | Ast.EParen x ->
        walk_expr scope ~container x

  and walk_decl (scope:nav_scope) ~(container:string option) (d:Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DVar { dtype; init; _ } ->
        walk_type scope ~container dtype;
        (match init with None -> () | Some e -> walk_expr scope ~container e)
    | Ast.DConst { dtype; value; _ } ->
        (match dtype with None -> () | Some t -> walk_type scope ~container t);
        walk_expr scope ~container value
    | Ast.DType { name; defn } ->
        walk_type scope ~container:(Some name.v) defn
    | Ast.DProc p ->
        let proc_container = Some p.v.name.v in
        let proc_scope = nav_scope_copy scope in
        (if nav_find_value proc_scope p.v.name.v = None then
           add_decl_value proc_scope ~id:p.v.name ~kind:sym_kind_func ~container);
        List.iter (fun prm ->
          add_decl_value proc_scope ~id:prm.v.pname ~kind:sym_kind_var ~container:proc_container
        ) p.v.params;
        List.iter (prebind_decl proc_scope ~container:proc_container) p.v.locals;
        List.iter (fun prm ->
          walk_type proc_scope ~container:proc_container prm.v.ptype
        ) p.v.params;
        (match p.v.returns with
         | None -> ()
         | Some r -> walk_type proc_scope ~container:proc_container r);
        List.iter (walk_decl proc_scope ~container:proc_container) p.v.locals;
        walk_stmt proc_scope ~container:proc_container p.v.body
    | Ast.DDirective _ ->
        ()

  and walk_stmt (scope:nav_scope) ~(container:string option) (s:Ast.stmt Ast.node) : unit =
    match s.v with
    | Ast.SEmpty ->
        ()
    | Ast.SDecl d ->
        prebind_decl scope ~container d;
        walk_decl scope ~container d
    | Ast.SBlock xs ->
        let block_scope = nav_scope_copy scope in
        walk_stmt_list block_scope ~container xs
    | Ast.SAssign { lhs; rhs } ->
        walk_expr scope ~container lhs;
        walk_expr scope ~container rhs
    | Ast.SCallStmt { callee; args; _ } ->
        use_value scope callee;
        List.iter (walk_expr scope ~container) args
    | Ast.SIf { cond; then_; else_ } ->
        walk_expr scope ~container cond;
        let then_scope = nav_scope_copy scope in
        walk_stmt then_scope ~container then_;
        (match else_ with
         | None -> ()
         | Some e ->
             let else_scope = nav_scope_copy scope in
             walk_stmt else_scope ~container e)
    | Ast.SWhile { cond; body } ->
        walk_expr scope ~container cond;
        let body_scope = nav_scope_copy scope in
        walk_stmt body_scope ~container body
    | Ast.SFor { init; cond; step; body } ->
        let loop_scope = nav_scope_copy scope in
        (match init with
         | None -> ()
         | Some i ->
             prebind_stmt loop_scope ~container i;
             walk_stmt loop_scope ~container i);
        (match cond with None -> () | Some e -> walk_expr loop_scope ~container e);
        (match step with
         | None -> ()
         | Some st ->
             prebind_stmt loop_scope ~container st;
             walk_stmt loop_scope ~container st);
        let body_scope = nav_scope_copy loop_scope in
        walk_stmt body_scope ~container body
    | Ast.SReturn eo ->
        (match eo with None -> () | Some e -> walk_expr scope ~container e)
    | Ast.SLabel { body; _ } ->
        walk_stmt scope ~container body
    | Ast.SGoto id ->
        use_label scope id

  and walk_stmt_list (scope:nav_scope) ~(container:string option) (xs:Ast.stmt Ast.node list) : unit =
    List.iter (prebind_stmt scope ~container) xs;
    List.iter (walk_stmt scope ~container) xs
  in

  let bind_imports () =
    let is_importable_def (d:def) : bool =
      d.kind <> sym_kind_module && d.kind <> sym_kind_func
    in
    sem_import_dirs doc
    |> List.iter (fun (imp:compool_import_dir) ->
         match resolve_compool_doc_uncached ws ~name:imp.compool with
         | None -> ()
         | Some target ->
             let defs = exported_defs_for_import_scope target in
             if imp.selected = [] then
               List.iter (fun d ->
                 if is_importable_def d then bind_external_def root_scope d
               ) defs
             else
               let selected = Hashtbl.create 32 in
               List.iter (fun (nm, _loc) ->
                 Hashtbl.replace selected (normalize_name nm) true
               ) imp.selected;
               List.iter (fun d ->
                 if is_importable_def d && Hashtbl.mem selected d.key then
                   bind_external_def root_scope d
                ) defs)
  in

  let bind_defines () =
    List.iter (fun (dm:Preprocess.define) ->
      let d = def_of_preprocess_define doc dm in
      match nav_add_decl nav d with
      | None -> ()
      | Some b -> nav_bind_value root_scope b
    ) doc.Document.defines
  in

  bind_imports ();
  bind_defines ();
  (match doc.Document.ast with
   | None -> ()
   | Some prog ->
       List.iter (function
         | Ast.TopDecl d -> prebind_decl root_scope ~container:None d
         | Ast.TopStmt s -> prebind_stmt root_scope ~container:None s
       ) prog;
       List.iter (function
         | Ast.TopDecl d -> walk_decl root_scope ~container:None d
         | Ast.TopStmt s -> walk_stmt root_scope ~container:None s
       ) prog);
  nav

let display_path_of_def (d:def) : string =
  match d.loc.Ast.Loc.file with
  | Some p -> p
  | None ->
      (match Uri_path.file_path_of_uri d.uri with
       | Some p -> p
       | None -> Uri_path.docuri_to_string d.uri)

let file_line_of_def (d:def) : string =
  let f = Filename.basename (display_path_of_def d) in
  Printf.sprintf "%s:%d" f d.loc.Ast.Loc.start_pos.line

let is_name_start_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '$' | '_' -> true
  | _ -> false

let is_valid_rename_name (s:string) : bool =
  let n = String.length s in
  if n = 0 || not (is_name_start_char s.[0]) then false
  else
    let rec loop i =
      if i >= n then true
      else if is_ident_char s.[i] then loop (i + 1)
      else false
    in
    loop 1

let truncate_text (max_len:int) (s:string) : string =
  let n = String.length s in
  if n <= max_len || max_len < 4 then s
  else String.sub s 0 (max_len - 3) ^ "..."

let import_under_cursor (doc:Document.t) (pos:T.Position.t) : Preprocess.import option =
  Document.imports doc
  |> List.find_opt (fun (imp:Preprocess.import) -> position_in_loc pos imp.loc)

let def_to_loc_json (d:def) : Yojson.Safe.t =
  location_json ~uri:d.uri d.loc

let doc_of_uri (ws:t) (uri:T.DocumentUri.t) : Document.t option =
  Hashtbl.find_opt ws.docs uri

let line_text_in_doc (doc:Document.t) ~(line1:int) : string option =
  let line0 = line1 - 1 in
  match Text_index.line_start_offset doc.Document.index ~line:line0 with
  | None -> None
  | Some start ->
      let stop =
        match Text_index.line_start_offset doc.Document.index ~line:(line0 + 1) with
        | Some s -> s
        | None -> String.length doc.Document.text
      in
      let len = max 0 (stop - start) in
      if len = 0 then Some ""
      else
        let raw = String.sub doc.Document.text start len in
        let n = String.length raw in
        let n = if n > 0 && raw.[n - 1] = '\n' then n - 1 else n in
        let n = if n > 0 && raw.[n - 1] = '\r' then n - 1 else n in
        Some (String.trim (if n <= 0 then "" else String.sub raw 0 n))

let line_text_in_file ~(path:string) ~(line1:int) : string option =
  if line1 <= 0 then None
  else
    try
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop cur =
            if cur >= line1 then
              Some (String.trim (input_line ic))
            else (
              ignore (input_line ic);
              loop (cur + 1)
            )
          in
          loop 1)
    with _ -> None

let source_line_for_def (ws:t) (d:def) : string option =
  let line1 = d.loc.Ast.Loc.start_pos.line in
  match doc_of_uri ws d.uri with
  | Some d0 ->
      line_text_in_doc d0 ~line1
  | None ->
      (match d.loc.Ast.Loc.file with
       | None -> None
       | Some p ->
           (match doc_at_path_cached ws p with
            | Some d0 ->
                line_text_in_doc d0 ~line1
            | None ->
                enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"source_line" ~high:true p;
                line_text_in_file ~path:p ~line1))

let literal_to_string = function
  | Ast.LInt s -> s
  | Ast.LFloat s -> s
  | Ast.LString s -> Printf.sprintf "'%s'" s
  | Ast.LChar c -> Printf.sprintf "'%c'" c
  | Ast.LBool b -> if b then "TRUE" else "FALSE"

let rec expr_to_compact_string (e:Ast.expr Ast.node) : string =
  match e.v with
  | Ast.EName id -> id.v
  | Ast.ELit lit -> literal_to_string lit
  | Ast.EParen x -> "(" ^ expr_to_compact_string x ^ ")"
  | Ast.EUnop { rhs; _ } -> expr_to_compact_string rhs
  | Ast.EBinop { lhs; rhs; _ } ->
      expr_to_compact_string lhs ^ "..." ^ expr_to_compact_string rhs
  | Ast.ECall { callee; args } ->
      let xs = args |> List.map expr_to_compact_string |> String.concat ", " in
      callee.v ^ "(" ^ xs ^ ")"
  | Ast.EIndex { base; index } ->
      let xs = index |> List.map expr_to_compact_string |> String.concat ", " in
      expr_to_compact_string base ^ "(" ^ xs ^ ")"
  | Ast.EField { base; field } ->
      expr_to_compact_string base ^ "." ^ field.v
  | Ast.EAt { field; ptr } ->
      expr_to_compact_string field ^ " @ " ^ expr_to_compact_string ptr
  | Ast.EDeref { ptr } ->
      "@ " ^ expr_to_compact_string ptr

let rec type_expr_to_compact_string (t:Ast.type_expr Ast.node) : string =
  match t.v with
  | Ast.TName id -> id.v
  | Ast.TPointer inner -> "P " ^ type_expr_to_compact_string inner
  | Ast.TArray { elem; dims } ->
      let ds = dims |> List.map expr_to_compact_string |> String.concat "," in
      type_expr_to_compact_string elem ^ "(" ^ ds ^ ")"
  | Ast.TRecord _ -> "RECORD"
  | Ast.TFunc _ -> "FUNC"

let param_to_signature_piece (p:Ast.param Ast.node) : string =
  let name = p.v.pname.v in
  let ty =
    match p.v.ptype.v with
    | Ast.TName id when normalize_name id.v = "__IMPLICIT__" -> None
    | _ -> Some (type_expr_to_compact_string p.v.ptype)
  in
  let mode =
    match p.v.pmode with
    | Ast.In -> ""
    | Ast.Out -> "OUT "
    | Ast.InOut -> "INOUT "
  in
  match ty with
  | None -> mode ^ name
  | Some t -> mode ^ name ^ " " ^ t

let proc_use_suffix (u:Ast.proc_use) : string =
  match u with
  | Ast.UseNormal -> ""
  | Ast.UseRec -> " REC"
  | Ast.UseRent -> " RENT"

let proc_signature_of_proc (p:Ast.proc Ast.node) : string =
  let params = p.v.params |> List.map param_to_signature_piece |> String.concat ", " in
  let ret =
    match p.v.returns with
    | None -> ""
    | Some r -> " " ^ type_expr_to_compact_string r
  in
  Printf.sprintf "PROC %s(%s)%s%s;" p.v.name.v params ret (proc_use_suffix p.v.use_attr)

let proc_signature_for_def (ws:t) (d:def) : string option =
  if d.kind <> sym_kind_func then None
  else
    let doc_opt =
      match doc_of_uri ws d.uri with
      | Some d0 -> Some d0
      | None ->
          (match d.loc.Ast.Loc.file with
           | Some p ->
               (match doc_at_path_cached ws p with
                | Some d0 -> Some d0
                | None ->
                    enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"signature" ~high:true p;
                    None)
           | None -> None)
    in
    let same_ident_loc (id:Ast.ident) =
      id.loc.start_pos.line = d.loc.start_pos.line
      && id.loc.start_pos.col = d.loc.start_pos.col
      && normalize_name id.v = d.key
    in
    let rec find_in_decl (decl:Ast.decl Ast.node) : Ast.proc Ast.node option =
      match decl.v with
      | Ast.DProc p when same_ident_loc p.v.name -> Some p
      | Ast.DProc p ->
          find_in_decls p.v.locals
      | _ -> None
    and find_in_decls (decls:Ast.decl Ast.node list) : Ast.proc Ast.node option =
      match decls with
      | [] -> None
      | d0 :: tl ->
          (match find_in_decl d0 with
           | Some _ as x -> x
           | None -> find_in_decls tl)
    in
    match doc_opt with
    | None -> None
    | Some d0 ->
        (match d0.Document.ast with
         | None -> None
         | Some prog ->
             let rec find_top = function
               | [] -> None
               | Ast.TopDecl dcl :: tl ->
                   (match find_in_decl dcl with
                    | Some p -> Some (proc_signature_of_proc p)
                    | None -> find_top tl)
               | Ast.TopStmt _ :: tl -> find_top tl
             in
             find_top prog)

let find_compool_target (ws:t) ~(name:string) : Document.t option =
  pump_index_lookup ws;
  let key = normalize_name name in
  match find_open_compool_doc_by_key ws key with
  | Some d -> Some d
  | None ->
      let path_opt =
        match ws.index with
        | Some idx ->
            (match Workspace_index.find_compool idx ~name:key with
             | Some p -> Some p
             | None ->
                 if allow_fallback_scan ws then
                   find_compool_path_fallback ws ~key
                 else
                   None)
        | None ->
            if allow_fallback_scan ws then find_compool_path_fallback ws ~key else None
      in
      (match path_opt with
       | None -> None
       | Some path ->
           (match doc_at_path_cached ws path with
            | Some d -> Some d
            | None ->
                enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"compool_target" ~high:true path;
                None))

let nav_for_doc_cached
    (ws:t)
    (cache:(string, doc_nav) Hashtbl.t)
    (doc:Document.t)
    : doc_nav =
  let k = Uri_path.docuri_to_string doc.Document.uri in
  match Hashtbl.find_opt cache k with
  | Some nav -> nav
  | None ->
      let nav =
        if ws.sem_store_enabled then
          match Semantic_store.snapshot_for_uri ws.semantic_store ~uri:doc.Document.uri with
          | Some snap when snap.Doc_snapshot.doc_rev = doc.Document.parse_rev ->
              Perf_stats.tick "nav.cache_hit";
              doc_nav_of_snapshot snap
          | _ ->
              Perf_stats.tick "nav.cache_miss";
              let nav = Perf_stats.time "nav.build" (fun () -> build_doc_nav ws doc) in
              if doc.Document.parse_rev = doc.Document.rev then
                upsert_semantic_snapshot_for_doc_with_nav ws doc nav;
              nav
        else
          Perf_stats.time "nav.build" (fun () -> build_doc_nav ws doc)
      in
      Hashtbl.replace cache k nav;
      nav

let find_def_for_sym_id
    (ws:t)
    (cache:(string, doc_nav) Hashtbl.t)
    ~(docs:Document.t list)
    ~(sym_id:string)
    : def option =
  let rec loop = function
    | [] -> None
    | d :: tl ->
        let nav = nav_for_doc_cached ws cache d in
        (match Hashtbl.find_opt nav.defs_by_id sym_id with
         | Some _ as hit -> hit
         | None -> loop tl)
  in
  loop docs

let defs_for_import_cursor (ws:t) (imp:Preprocess.import) : def list =
  match find_compool_target ws ~name:imp.name with
  | None -> []
  | Some d ->
      let defs = collect_doc_defs d in
      let key = normalize_name imp.name in
      let hits = List.filter (fun x -> x.key = key && x.kind = sym_kind_module) defs in
      if hits <> [] then hits
      else
        let loc =
          let z = { Ast.Loc.line = 1; col = 0; offset = 0 } in
          Ast.Loc.make ~file:d.Document.file ~start_pos:z ~end_pos:z
        in
        [ { uri = d.Document.uri; name = imp.name; key; loc; kind = sym_kind_module; container = None } ]

let fallback_defs_by_name (ws:t) (doc:Document.t) (key:string) : def list =
  if key = "" || is_reserved_keyword key then []
  else
    let collect (docs:Document.t list) : def list =
      docs
      |> List.concat_map (fun d ->
           collect_doc_defs d |> List.filter (fun x -> x.key = key))
      |> uniq_defs
    in
    let from_semantic_store () : def list =
      if not ws.sem_store_enabled then []
      else
        Semantic_store.sym_ids_for_key ws.semantic_store ~key
        |> List.concat_map (fun sym_id -> Semantic_store.defs_for_sym_id ws.semantic_store sym_id)
        |> List.map def_of_snapshot_def
        |> List.filter (fun d -> d.key = key)
        |> uniq_defs
    in
    let local_hits = collect (docs_for_lookup ws doc) in
    if local_hits <> [] then local_hits
    else
      let sem_hits = from_semantic_store () in
      if sem_hits <> [] then sem_hits
      else collect (docs_for_rename ws doc)

let allow_unscoped_fallback (doc:Document.t) : bool =
  has_unscoped_fallback_context doc

let is_ref_proc_decl_line (line:string) : bool =
  let toks =
    tokenize_ident_words (normalize_name line)
    |> List.map (fun (w, _, _) -> w)
  in
  let rec has_ref_proc = function
    | "REF" :: "PROC" :: _ -> true
    | _ :: tl -> has_ref_proc tl
    | [] -> false
  in
  has_ref_proc toks

let is_likely_proc_implementation (ws:t) (d:def) : bool =
  if d.kind <> sym_kind_func then true
  else
    match source_line_for_def ws d with
    | None -> true
    | Some line -> not (is_ref_proc_decl_line line)

let substring_matches_at (s:string) ~(sub:string) ~(at:int) : bool =
  let n = String.length s in
  let m = String.length sub in
  if at < 0 || m <= 0 || at + m > n then false
  else
    let rec loop i =
      if i >= m then true
      else if s.[at + i] <> sub.[i] then false
      else loop (i + 1)
    in
    loop 0

let find_substring_from (s:string) ~(sub:string) ~(start:int) : int option =
  let n = String.length s in
  let m = String.length sub in
  if m <= 0 then Some (max 0 start)
  else
    let rec loop i =
      if i + m > n then None
      else if substring_matches_at s ~sub ~at:i then Some i
      else loop (i + 1)
    in
    loop (max 0 start)

let find_proc_def_name_offsets ~(upper_text:string) ~(key:string) : (int * int) option =
  if key = "" then None
  else
    let pat = "DEF PROC " ^ key in
    let key_len = String.length key in
    let rec loop from =
      match find_substring_from upper_text ~sub:pat ~start:from with
      | None -> None
      | Some i ->
          let name_s = i + 9 in
          let name_e = name_s + key_len in
          let before_ok = i = 0 || not (is_ident_char upper_text.[i - 1]) in
          let after_ok =
            name_e >= String.length upper_text || not (is_ident_char upper_text.[name_e])
          in
          if before_ok && after_ok then Some (name_s, name_e)
          else loop (i + 1)
    in
    loop 0

let find_proc_decl_name_offsets ~(upper_text:string) ~(key:string) : (int * int) option =
  if key = "" then None
  else
    let pat = "PROC " ^ key in
    let key_len = String.length key in
    let rec loop from =
      match find_substring_from upper_text ~sub:pat ~start:from with
      | None -> None
      | Some i ->
          let name_s = i + 5 in
          let name_e = name_s + key_len in
          let before_ok = i = 0 || not (is_ident_char upper_text.[i - 1]) in
          let after_ok =
            name_e >= String.length upper_text || not (is_ident_char upper_text.[name_e])
          in
          if before_ok && after_ok then Some (name_s, name_e)
          else loop (i + 1)
    in
    loop 0

let docuri_of_path_unsafe (path:string) : T.DocumentUri.t =
  match Uri_path.docuri_of_path path with
  | Some u -> u
  | None ->
      (match T.DocumentUri.t_of_yojson (`String (Uri_path.file_uri_of_path path)) with
       | u -> u
       | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))

let quick_proc_defs_from_nav_index (ws:t) (doc:Document.t) ~(key:string) : def list =
  if key = "" then []
  else
    let current_path_key =
      match doc.Document.file with
      | None -> None
      | Some p -> Some (normalize_path_key p)
    in
    let entries =
      match Hashtbl.find_opt ws.quick_nav_index key with
      | None -> []
      | Some xs -> xs
    in
    entries
    |> List.filter_map (fun e ->
         let same_doc =
           match Uri_path.file_path_of_uri e.qn_uri, current_path_key with
           | Some p, Some cur -> normalize_path_key p = cur
           | _ -> false
         in
         if same_doc then None
         else (
           (match Uri_path.file_path_of_uri e.qn_uri with
            | Some p -> enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"quick_nav_hit" ~high:true p
            | None -> ());
           Some
             {
               uri = e.qn_uri;
               name = e.qn_name;
               key = e.qn_key;
               loc = e.qn_loc;
               kind = e.qn_kind;
               container = e.qn_container;
             }))
    |> uniq_defs

let quick_proc_defs_from_index_sources (ws:t) (doc:Document.t) ~(key:string) : def list =
  if key = ""
     || nav_quick_scan_files <= 0
     || nav_quick_scan_total_bytes <= 0
     || nav_quick_scan_per_file_bytes <= 0
  then
    []
  else
    let indexed_hits = quick_proc_defs_from_nav_index ws doc ~key in
    if indexed_hits <> [] then indexed_hits
    else
      match ws.index with
      | None -> []
      | Some idx ->
        let profile = workspace_profile_for_budget ws in
        let scan_files_budget =
          match profile with
          | ProfileLarge -> max nav_quick_scan_files 128
          | ProfileMedium -> max nav_quick_scan_files 72
          | ProfileSmall -> nav_quick_scan_files
        in
        let scan_total_budget =
          match profile with
          | ProfileLarge -> max nav_quick_scan_total_bytes 4_194_304
          | ProfileMedium -> max nav_quick_scan_total_bytes 2_359_296
          | ProfileSmall -> nav_quick_scan_total_bytes
        in
        let scan_per_file_budget =
          match profile with
          | ProfileLarge -> max nav_quick_scan_per_file_bytes 393_216
          | ProfileMedium -> nav_quick_scan_per_file_bytes
          | ProfileSmall -> nav_quick_scan_per_file_bytes
        in
        let current_path_key =
          match doc.Document.file with
          | None -> None
          | Some p -> Some (normalize_path_key p)
        in
        ensure_graph_fresh ws;
        let seen_paths = Hashtbl.create 512 in
        let candidate_paths_rev = ref [] in
        let push_path (path:string) =
          let key = normalize_path_key path in
          if key <> "" && not (Hashtbl.mem seen_paths key) then (
            Hashtbl.replace seen_paths key true;
            candidate_paths_rev := path :: !candidate_paths_rev
          )
        in
        Array.iter push_path ws.graph_root_closure_paths;
        Workspace_index.all_source_paths idx |> List.iter push_path;
        let candidate_paths = List.rev !candidate_paths_rev in
        let mk_quick_hit ~(path:string) ~(text:string) ~(s:int) ~(e:int) : def =
          let uri = docuri_of_path_unsafe path in
          let idx = Text_index.of_string text in
          let loc = loc_of_offsets ~file:(Some path) ~idx ~s ~e in
          { uri; name = key; key; loc; kind = sym_kind_func; container = None }
        in
        let rec scan scanned scanned_bytes (fallback:def option) = function
          | [] ->
              (match fallback with
               | None -> []
               | Some d -> [ d ])
          | _ when scanned >= scan_files_budget -> []
          | _ when scanned_bytes >= scan_total_budget -> []
          | path :: tl ->
              let path_key = normalize_path_key path in
              let is_current =
                match current_path_key with
                | Some k -> k = path_key
                | None -> false
              in
              if is_current then scan scanned scanned_bytes fallback tl
              else (
                enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"quick_nav_scan" ~high:true path;
                if Hashtbl.mem ws.files path_key then scan scanned scanned_bytes fallback tl
                else
                  let offset =
                    match Hashtbl.find_opt ws.nav_quick_scan_offset_by_path path_key with
                    | Some n when n >= 0 -> n
                    | _ -> 0
                  in
                  match read_file_window_text path ~offset ~max_bytes:scan_per_file_budget with
                  | None ->
                      scan (scanned + 1) scanned_bytes fallback tl
                  | Some (text, next_offset) ->
                      Hashtbl.replace ws.nav_quick_scan_offset_by_path path_key next_offset;
                      let text_len = String.length text in
                      if text_len = 0 then
                        scan (scanned + 1) scanned_bytes fallback tl
                      else
                      let next_bytes = scanned_bytes + text_len in
                      if next_bytes > scan_total_budget then
                        (match fallback with
                         | None -> []
                         | Some d -> [ d ])
                      else
                        let upper_text = String.uppercase_ascii text in
                        (match find_proc_def_name_offsets ~upper_text ~key with
                         | Some (s, e) ->
                             [ mk_quick_hit ~path ~text ~s ~e ]
                         | None ->
                             let fallback =
                               match fallback with
                               | Some _ -> fallback
                               | None ->
                                   (match find_proc_decl_name_offsets ~upper_text ~key with
                                    | None -> None
                                    | Some (s, e) -> Some (mk_quick_hit ~path ~text ~s ~e))
                             in
                             scan (scanned + 1) next_bytes fallback tl)
              )
        in
        scan 0 0 None candidate_paths

let proc_defs_by_key (ws:t) (doc:Document.t) ~(key:string) : def list =
  if key = "" then []
  else (
    pump_index_lookup ws;
    let from_local_docs () : def list =
      docs_for_rename ws doc
      |> List.concat_map collect_doc_defs
      |> List.filter (fun d -> d.kind = sym_kind_func && d.key = key)
      |> uniq_defs
    in
    let from_semantic_store () : def list =
      if not ws.sem_store_enabled then []
      else
        Semantic_store.sym_ids_for_key ws.semantic_store ~key
        |> List.concat_map (fun sym_id -> Semantic_store.defs_for_sym_id ws.semantic_store sym_id)
        |> List.map def_of_snapshot_def
        |> List.filter (fun d -> d.kind = sym_kind_func && d.key = key)
        |> uniq_defs
    in
    let sem_hits = from_semantic_store () in
    let sem_has_impl = List.exists (is_likely_proc_implementation ws) sem_hits in
    if sem_has_impl then sem_hits
    else
      let local_hits = from_local_docs () in
      let local_has_impl = List.exists (is_likely_proc_implementation ws) local_hits in
      if local_has_impl then local_hits
      else
      let quick_hits =
        quick_proc_defs_from_index_sources ws doc ~key
      in
      let combined = uniq_defs (sem_hits @ local_hits @ quick_hits) in
      combined
  )

let proc_impl_defs_by_key (ws:t) (doc:Document.t) ~(key:string) : def list =
  let defs = proc_defs_by_key ws doc ~key in
  let impls = List.filter (is_likely_proc_implementation ws) defs in
  if impls = [] then defs else impls

let perf_stats_json (_ws:t) : Yojson.Safe.t =
  Perf_stats.snapshot_json ()
