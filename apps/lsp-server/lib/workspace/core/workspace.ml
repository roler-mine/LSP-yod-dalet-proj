module T = Lsp.Types

type t = Workspace_foundation.t

type bg_tick_mode = Workspace_foundation.bg_tick_mode =
  | BgTickInteractive
  | BgTickIdle

let create = Workspace_state.create
let set_root_uri = Workspace_state.set_root_uri
let set_root_path = Workspace_state.set_root_path
let set_source_files = Workspace_state.set_source_files
let rescan = Workspace_index_graph.rescan
let ensure_index_health = Workspace_index_graph.ensure_index_health
let revalidate_all = Workspace_doc_lifecycle.revalidate_all
let compool_count = Workspace_index_graph.compool_count
let open_doc = Workspace_doc_lifecycle.open_doc
let preview_open_doc_diags = Workspace_doc_lifecycle.preview_open_doc_diags
let change_doc = Workspace_doc_lifecycle.change_doc
let close_doc = Workspace_doc_lifecycle.close_doc
let apply_watched_file_changes = Workspace_doc_lifecycle.apply_watched_file_changes
let background_tick = Workspace_background.background_tick
let effective_bg_tick_budget_ms = Workspace_runtime.effective_bg_tick_budget_ms
let drain_pending_diag_updates = Workspace_background.drain_pending_diag_updates
let drain_open_diag_revalidate_uris =
  Workspace_background.drain_open_diag_revalidate_uris

let finish_open_doc_now_if_needed =
  Workspace_background.finish_open_doc_now_if_needed

let refresh_closed_doc_diagnostics_now =
  Workspace_background.refresh_closed_doc_diagnostics_now

let finish_last_open_doc_now_if_needed =
  Workspace_background.finish_last_open_doc_now_if_needed

let startup_background_budget_ms = Workspace_runtime.startup_background_budget_ms
let feature_flags = Workspace_runtime.feature_flags
let startup_priority_mode = Workspace_runtime.startup_priority_mode
let startup_navigation_ready_now = Workspace_runtime.startup_navigation_ready_now
let startup_diag_hover_ready_now = Workspace_runtime.startup_diag_hover_ready_now
let startup_is_ready_now = Workspace_runtime.startup_is_ready_now
let open_doc_count = Workspace_runtime.open_doc_count
let startup_readiness_json_for_report =
  Workspace_runtime.startup_readiness_json_for_report

let workspace_ready_event_json = Workspace_runtime.workspace_ready_event_json
let startup_phase_event_json = Workspace_runtime.startup_phase_event_json
let startup_miss_event_json = Workspace_runtime.startup_miss_event_json
let open_doc_converged = Workspace_doc_lifecycle.open_doc_converged
let request_cancelled = Workspace_runtime.request_cancelled
let with_request_cancel_checker = Workspace_runtime.with_request_cancel_checker
let diagnostics_for = Workspace_state.diagnostics_for
let document_version = Workspace_state.document_version
let diagnostics_snapshot_for = Workspace_state.diagnostics_snapshot_for
let ast_dump_for = Workspace_state.ast_dump_for
let cst_dump_for = Workspace_state.cst_dump_for
let definition_locations_for = Workspace_navigation.definition_locations_for
let declaration_locations_for = Workspace_navigation.declaration_locations_for
let type_definition_locations_for =
  Workspace_navigation.type_definition_locations_for

let implementation_locations_for =
  Workspace_navigation.implementation_locations_for

let references_locations_for = Workspace_navigation.references_locations_for
let references_locations_stream =
  Workspace_navigation.references_locations_stream

let document_symbols_for = Workspace_reporting.document_symbols_for
let workspace_symbols_for = Workspace_navigation.workspace_symbols_for
let workspace_symbols_stream = Workspace_navigation.workspace_symbols_stream
let hover_for = Workspace_navigation.hover_for
let signature_help_for = Workspace_navigation.signature_help_for
let prepare_rename_for = Workspace_navigation.prepare_rename_for
let rename_for = Workspace_navigation.rename_for
let completion_items_for = Workspace_navigation.completion_items_for
let code_actions_for = Workspace_navigation.code_actions_for
let semantic_tokens_full_for = Workspace_reporting.semantic_tokens_full_for
let semantic_tokens_range_for = Workspace_reporting.semantic_tokens_range_for
let semantic_tokens_delta_for = Workspace_reporting.semantic_tokens_delta_for
let lsif_index_json = Workspace_reporting.lsif_index_json
let lsif_delta_json = Workspace_reporting.lsif_delta_json
let perf_stats_json = Workspace_nav_lookup.perf_stats_json
let debug_report_for = Workspace_reporting.debug_report_for
let snapshot = Workspace_snapshot.of_workspace

let publish_snapshot ws =
  let snap = Workspace_snapshot.of_workspace ws in
  Workspace_snapshot.publish snap;
  match ws.Workspace_foundation.root_path with
  | None -> ()
  | Some root -> Workspace_persistent_index.save_snapshot_index ~root snap
