(* Module overview: Shared workspace state types, queues, caches, and performance counters. *)

module T = Lsp.Types

module Perf_stats = struct
  type metric_mut = {
    mutable calls : int;
    mutable total_ms : float;
    mutable max_ms : float;
    mutable last_ms : float;
    samples : float array;
    mutable sample_count : int;
    mutable sample_pos : int;
  }

  type metric = {
    calls : int;
    total_ms : float;
    max_ms : float;
    last_ms : float;
  }

  let metrics : (string, metric_mut) Hashtbl.t = Hashtbl.create 64
  let sample_window = 128

  let metric_for (name : string) : metric_mut =
    match Hashtbl.find_opt metrics name with
    | Some m -> m
    | None ->
        let m : metric_mut =
          {
            calls = 0;
            total_ms = 0.0;
            max_ms = 0.0;
            last_ms = 0.0;
            samples = Array.make sample_window 0.0;
            sample_count = 0;
            sample_pos = 0;
          }
        in
        Hashtbl.add metrics name m;
        m

  let observe_ms (name : string) (elapsed_ms : float) : unit =
    let m = metric_for name in
    m.calls <- m.calls + 1;
    m.total_ms <- m.total_ms +. elapsed_ms;
    m.last_ms <- elapsed_ms;
    if elapsed_ms > m.max_ms then m.max_ms <- elapsed_ms;
    if sample_window > 0 then (
      m.samples.(m.sample_pos) <- elapsed_ms;
      m.sample_pos <- (m.sample_pos + 1) mod sample_window;
      if m.sample_count < sample_window then
        m.sample_count <- m.sample_count + 1)

  let now_ms () : float = Unix.gettimeofday () *. 1000.0

  let time (name : string) (f : unit -> 'a) : 'a =
    let t0 = now_ms () in
    try
      let out = f () in
      observe_ms name (max 0.0 (now_ms () -. t0));
      out
    with exn ->
      observe_ms name (max 0.0 (now_ms () -. t0));
      raise exn

  let tick (name : string) : unit = observe_ms name 0.0

  let metric_snapshot (m : metric_mut) : metric =
    {
      calls = m.calls;
      total_ms = m.total_ms;
      max_ms = m.max_ms;
      last_ms = m.last_ms;
    }

  let percentile_from_samples (m : metric_mut) (pct : float) : float =
    if m.sample_count <= 0 then 0.0
    else
      let samples = Array.sub m.samples 0 m.sample_count in
      Array.sort Float.compare samples;
      let pct = if pct < 0.0 then 0.0 else if pct > 1.0 then 1.0 else pct in
      let idx =
        int_of_float (Float.ceil (pct *. float_of_int m.sample_count)) - 1
      in
      samples.(max 0 (min idx (m.sample_count - 1)))

  let snapshot_json () : Yojson.Safe.t =
    let entry name m =
      let snap = metric_snapshot m in
      let avg_ms =
        if snap.calls <= 0 then 0.0
        else snap.total_ms /. float_of_int snap.calls
      in
      `Assoc
        [
          ("name", `String name);
          ("calls", `Int snap.calls);
          ("totalMs", `Float snap.total_ms);
          ("avgMs", `Float avg_ms);
          ("maxMs", `Float snap.max_ms);
          ("lastMs", `Float snap.last_ms);
          ("sampleCount", `Int m.sample_count);
          ("p50Ms", `Float (percentile_from_samples m 0.50));
          ("p95Ms", `Float (percentile_from_samples m 0.95));
          ("p99Ms", `Float (percentile_from_samples m 0.99));
        ]
    in
    let name_of_entry = function
      | `Assoc fields -> (
          match List.assoc_opt "name" fields with
          | Some (`String s) -> s
          | _ -> "")
      | _ -> ""
    in
    let entries =
      Hashtbl.fold (fun name m acc -> entry name m :: acc) metrics []
      |> List.sort (fun a b -> String.compare (name_of_entry a) (name_of_entry b))
    in
    `Assoc [ ("metrics", `List entries) ]

  let reset () : unit = Hashtbl.clear metrics
end

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

type startup_priority_mode = Workspace_settings.startup_priority_mode =
  | StartupPriorityBalanced
  | StartupPriorityInfoFirst

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

type dependency_edge_kind =
  | ICompoolImport
  | ICopyInclude
  | DefExport
  | RefImport
  | DefineUse
  | TypeUse
  | ProcedureCall
  | TableFieldUse

type dependency_edge = {
  de_kind : dependency_edge_kind;
  de_target : string;
  de_path_key : string option;
}

type graph_node = {
  gn_path : string;
  gn_path_key : string;
  mutable gn_import_compools : string list;
  mutable gn_import_paths : string list;
  mutable gn_include_targets : Workspace_include_model.include_target list;
  mutable gn_rev_importers : string list;
  mutable gn_dependency_edges : dependency_edge list;
  mutable gn_file_class : file_class;
  mutable gn_size_class : file_size_class;
  mutable gn_parse_quality : parse_quality;
  mutable gn_epoch : int;
}

type parse_job_payload =
  | ParseJobOpen of {
      path_key : string;
      uri : T.DocumentUri.t;
      generation : int;
      text_hash : string;
      parse_profile : string;
      started_ms : float;
      doc : Document.t;
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
      text_hash : string;
      parse_profile : string;
      started_ms : float;
      doc : Document.t;
    }
  | ParseResultPath of {
      pr_kind : parse_job_kind;
      pr_epoch : int;
      path : string;
      path_key : string;
      doc_opt : Document.t option;
    }
  | ParseResultStale of { pr_epoch : int; path_key : string }

type quick_nav_entry = {
  qn_uri : T.DocumentUri.t;
  qn_name : string;
  qn_key : string;
  qn_loc : Ast.Loc.t;
  qn_kind : int;
  qn_container : string option;
  qn_metadata : Workspace_symbol_metadata.jovial_symbol_metadata;
}

type module_summary_authority =
  | ModuleSummaryProvisional
  | ModuleSummaryMetadataValidated

type module_summary_cache_entry = {
  msc_path : string;
  msc_path_key : string;
  msc_summary : Module_summary.t;
  mutable msc_authority : module_summary_authority;
}

type asm_label_source = AsmConcreteLabel | AsmExportDirective

type asm_label_hit = {
  label_name : string;
  label_key : string;
  label_path : string;
  label_loc : Ast.Loc.t;
  label_source : asm_label_source;
}

type asm_file_index = {
  asm_path : string;
  asm_path_key : string;
  asm_import_keys : string list;
  asm_labels : asm_label_hit list;
  asm_export_directives : asm_label_hit list;
  asm_content_hash : string;
}

module Lsif_delta = struct
  type t = {
    mutable revision : int;
    symbols : (string, Yojson.Safe.t) Hashtbl.t;
  }

  type delta = {
    base_revision : int;
    revision : int;
    reset : bool;
    deletes : string list;
    upserts : Yojson.Safe.t list;
  }

  let create () : t = { revision = 0; symbols = Hashtbl.create 2048 }
  let revision (t : t) : int = t.revision

  let reset (t : t) : unit =
    t.revision <- 0;
    Hashtbl.clear t.symbols

  let symbols_of_index_json (j : Yojson.Safe.t) :
      (string, Yojson.Safe.t) Hashtbl.t =
    let out = Hashtbl.create 2048 in
    let fields = match j with `Assoc xs -> xs | _ -> [] in
    let symbols =
      match List.assoc_opt "symbols" fields with
      | Some (`List xs) -> xs
      | _ -> []
    in
    List.iter
      (function
        | `Assoc sfields as item -> (
            match List.assoc_opt "id" sfields with
            | Some (`String sym_id) when String.trim sym_id <> "" ->
                Hashtbl.replace out sym_id item
            | _ -> ())
        | _ -> ())
      symbols;
    out

  let update_full (t : t) ~(revision : int)
      ~(symbols : (string, Yojson.Safe.t) Hashtbl.t) : unit =
    t.revision <- revision;
    Hashtbl.clear t.symbols;
    Hashtbl.iter (fun k v -> Hashtbl.replace t.symbols k v) symbols

  let diff (t : t) ~(base_revision : int) ~(current_revision : int)
      ~(current_symbols : (string, Yojson.Safe.t) Hashtbl.t) : delta =
    if base_revision <> t.revision then
      {
        base_revision;
        revision = current_revision;
        reset = true;
        deletes = [];
        upserts = [];
      }
    else
      let deletes = ref [] in
      let upserts = ref [] in
      Hashtbl.iter
        (fun key _ ->
          if not (Hashtbl.mem current_symbols key) then
            deletes := key :: !deletes)
        t.symbols;
      Hashtbl.iter
        (fun key cur ->
          match Hashtbl.find_opt t.symbols key with
          | None -> upserts := cur :: !upserts
          | Some prev ->
              if Yojson.Safe.to_string prev <> Yojson.Safe.to_string cur then
                upserts := cur :: !upserts)
        current_symbols;
      {
        base_revision;
        revision = current_revision;
        reset = false;
        deletes = List.sort String.compare !deletes;
        upserts = List.rev !upserts;
      }

  let delta_json (d : delta) : Yojson.Safe.t =
    `Assoc
      [
        ("format", `String "jovial-lsif-lite");
        ("baseRevision", `Int d.base_revision);
        ("revision", `Int d.revision);
        ("reset", `Bool d.reset);
        ("deletes", `List (List.map (fun k -> `String k) d.deletes));
        ("upserts", `List d.upserts);
      ]
end

type t = {
  workspace_id : int;
  docs : (T.DocumentUri.t, Document.t) Hashtbl.t;
  files : (string, Document.t) Hashtbl.t;
  mutable root_path : string option;
  mutable source_file_paths : string list;
  mutable assembly_file_paths : string list;
  mutable index : Workspace_index.t option;
  mutable index_checkpoint_loaded : bool;
  mutable symbol_hints :
    ((string, string list) Hashtbl.t * (string, string list) Hashtbl.t) option;
  semantic_store : Semantic_store.t;
  cross_file_index : Cross_file_index.t;
  semantic_tokens_cache : (string, int array) Hashtbl.t;
  sem_store_enabled : bool;
  lsif_delta_enabled : bool;
  lsif_delta_state : Lsif_delta.t;
  mutable lsif_snapshot_revision : int;
  mutable lsif_snapshot_payload : Yojson.Safe.t option;
  mutable lsif_snapshot_symbols : (string, Yojson.Safe.t) Hashtbl.t option;
  mutable ide_snapshot_generation : int;
  mutable ide_snapshot_dirty : bool;
  hover_body_cache : (string, string) Hashtbl.t;
  workspace_diag_mode : workspace_diag_mode;
  feature_flags : Workspace_settings.feature_flags;
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
    (string, T.DocumentUri.t * int option * T.Diagnostic.t list) Hashtbl.t;
  bg_pending_diag_set : (string, bool) Hashtbl.t;
  mutable bg_seed_needs_refresh : bool;
  mutable closed_doc_lru_clock : int;
  closed_doc_last_touch : (string, int) Hashtbl.t;
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
  mutable pressure_mode : pressure_mode;
  mutable pressure_live_mb : int;
  mutable pressure_last_check_ms : float;
  mutable startup_started_ms : float;
  startup_diag_hover_default_target_ms : int;
  startup_nav_default_target_ms : int;
  mutable startup_diag_hover_target_ms : int;
  mutable startup_nav_target_ms : int;
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
  startup_priority_mode : startup_priority_mode;
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
  asm_index_by_path : (string, asm_file_index) Hashtbl.t;
  asm_label_hits_by_key : (string, asm_label_hit list) Hashtbl.t;
  mutable asm_index_paths_key : string list;
  mutable asm_index_dirty : bool;
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
  quick_nav_index : (string, quick_nav_entry list) Hashtbl.t;
  module_summary_cache : (string, module_summary_cache_entry) Hashtbl.t;
  module_summary_compool_index : (string, string) Hashtbl.t;
  module_summary_reverse_importers : (string, string list) Hashtbl.t;
  mutable module_summary_cache_loaded : bool;
  quick_nav_pending_paths : string Queue.t;
  quick_nav_pending_set : (string, bool) Hashtbl.t;
  quick_nav_done_set : (string, bool) Hashtbl.t;
  nav_quick_scan_offset_by_path : (string, int) Hashtbl.t;
  mutable quick_nav_index_done : int;
  mutable quick_nav_index_total : int;
  parse_worker_jobs : bytes Queue.t;
  parse_worker_results : bytes Queue.t;
  parse_worker_doc_slots : (int, Document.t) Hashtbl.t;
  mutable parse_worker_next_doc_slot : int;
  parse_worker_mtx : Mutex.t;
  parse_worker_cv : Condition.t;
  parse_worker_inflight : (string, parse_job_kind) Hashtbl.t;
  parse_worker_count : int;
  parse_worker_max_inflight : int;
  mutable parse_worker_started : bool;
  mutable parse_worker_stop : bool;
  bg_high_large_budget_ms : int;
  mutable parse_epoch : int;
  mutable request_cancel_checker : (unit -> bool) option;
}
