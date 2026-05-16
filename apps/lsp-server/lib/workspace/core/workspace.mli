(** Module overview: Facade that exposes the workspace API used by the LSP server. *)

module T = Lsp.Types

type t
type bg_tick_mode = BgTickInteractive | BgTickIdle

val create : ?settings:Workspace_settings.t -> unit -> t
val set_root_uri : t -> T.DocumentUri.t option -> unit
val set_root_path : t -> string option -> unit
val set_source_files : t -> string list -> bool
val rescan : t -> unit
val ensure_index_health : t -> bool
val revalidate_all : t -> T.DocumentUri.t list
val compool_count : t -> int

val open_doc :
  ?lsp_version:int ->
  ?force_provisional:bool ->
  ?inline_catch_up:bool ->
  ?defer_cross_module_semantics:bool ->
  t ->
  uri:T.DocumentUri.t ->
  file:string option ->
  text:string ->
  unit

val preview_open_doc_diags :
  ?force_provisional:bool ->
  t ->
  uri:T.DocumentUri.t ->
  file:string option ->
  text:string ->
  T.Diagnostic.t list option

val change_doc :
  ?lsp_version:int ->
  t ->
  uri:T.DocumentUri.t ->
  changes:T.TextDocumentContentChangeEvent.t list ->
  unit

val close_doc : t -> uri:T.DocumentUri.t -> unit

val apply_watched_file_changes :
  t -> changes:(string * [ `Created | `Changed | `Deleted ]) list -> unit

val background_tick :
  ?should_stop:(unit -> bool) ->
  t ->
  budget_ms:int ->
  mode:bg_tick_mode ->
  idle_quiet_ms:int ->
  last_message_ms:float ->
  unit

val effective_bg_tick_budget_ms : t -> base_budget_ms:int -> int

val drain_pending_diag_updates :
  t -> max_items:int -> (T.DocumentUri.t * int option * T.Diagnostic.t list) list

val drain_open_diag_revalidate_uris : t -> max_items:int -> T.DocumentUri.t list
val drain_open_parse_results_now : t -> unit
val finish_open_doc_now_if_needed : t -> uri:T.DocumentUri.t -> bool
val refresh_closed_doc_diagnostics_now : t -> uri:T.DocumentUri.t -> bool
val finish_last_open_doc_now_if_needed : t -> bool
val startup_background_budget_ms : t -> base_budget_ms:int -> int
val feature_flags : t -> Workspace_settings.feature_flags
val startup_priority_mode : t -> Workspace_settings.startup_priority_mode
val startup_navigation_ready_now : t -> bool
val quick_nav_index_complete : t -> bool
val startup_diag_hover_ready_now : t -> bool
val startup_is_ready_now : t -> bool
val open_doc_count : t -> int
val background_work_pending : t -> bool
val startup_readiness_json_for_report : t -> Yojson.Safe.t
val workspace_ready_event_json : t -> Yojson.Safe.t option
val startup_phase_event_json : t -> Yojson.Safe.t option
val startup_miss_event_json : t -> Yojson.Safe.t option
val open_doc_converged : t -> uri:T.DocumentUri.t -> bool
val request_cancelled : t -> bool
val with_request_cancel_checker : t -> (unit -> bool) -> (unit -> 'a) -> 'a
val diagnostics_for : t -> uri:T.DocumentUri.t -> T.Diagnostic.t list
val document_version : t -> uri:T.DocumentUri.t -> int option
val document_text_length : t -> uri:T.DocumentUri.t -> int option
val diagnostics_snapshot_for :
  t -> uri:T.DocumentUri.t -> int option * T.Diagnostic.t list
val ast_dump_for : t -> uri:T.DocumentUri.t -> string option
val cst_dump_for : t -> uri:T.DocumentUri.t -> string option

val definition_locations_for :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list

val declaration_locations_for :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list

val type_definition_locations_for :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list

val implementation_locations_for :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list

val references_locations_for :
  t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  include_decl:bool ->
  T.Location.t list

val references_locations_stream :
  t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  include_decl:bool ->
  emit:(T.Location.t list -> unit) ->
  T.Location.t list

val document_symbols_for :
  t ->
  uri:T.DocumentUri.t ->
  [ `DocumentSymbol of T.DocumentSymbol.t
  | `SymbolInformation of T.SymbolInformation.t ]
  list

val workspace_symbols_for : t -> query:string -> T.SymbolInformation.t list

val workspace_symbols_stream :
  t ->
  query:string ->
  emit:(T.SymbolInformation.t list -> unit) ->
  T.SymbolInformation.t list

val hover_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Hover.t option

val explain_symbol_resolution_json :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t

val signature_help_for :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.SignatureHelp.t option

val prepare_rename_for :
  t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  [ `Range of T.Range.t | `RangeWithPlaceholder of T.Range.t * string ] option

val rename_for :
  t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  new_name:string ->
  T.WorkspaceEdit.t option

val completion_items_for :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.CompletionItem.t list

val code_actions_for :
  t -> uri:T.DocumentUri.t -> range:T.Range.t -> T.CodeAction.t list

val code_lenses_for : t -> uri:T.DocumentUri.t -> T.CodeLens.t list

val resolve_code_lens : t -> T.CodeLens.t -> T.CodeLens.t

val inlay_hints_for :
  t -> uri:T.DocumentUri.t -> range:T.Range.t -> T.InlayHint.t list

val formatting_edits_for :
  t ->
  uri:T.DocumentUri.t ->
  options:Workspace_formatting.options ->
  T.TextEdit.t list

val range_formatting_edits_for :
  t ->
  uri:T.DocumentUri.t ->
  range:T.Range.t ->
  options:Workspace_formatting.options ->
  T.TextEdit.t list

val semantic_tokens_full_for :
  t -> uri:T.DocumentUri.t -> T.SemanticTokens.t option

val semantic_tokens_range_for :
  t -> uri:T.DocumentUri.t -> range:T.Range.t -> T.SemanticTokens.t option

val semantic_tokens_delta_for :
  t ->
  uri:T.DocumentUri.t ->
  previous_result_id:string ->
  [ `SemanticTokens of T.SemanticTokens.t
  | `SemanticTokensDelta of T.SemanticTokensDelta.t ]
  option

val lsif_index_json : t -> Yojson.Safe.t
val lsif_delta_json : t -> base_revision:int -> Yojson.Safe.t
val perf_stats_json : t -> Yojson.Safe.t

val debug_report_for :
  t -> uri:T.DocumentUri.t -> max_tokens:int -> Yojson.Safe.t

val debug_scheduler_json : t -> Yojson.Safe.t
val debug_memory_json : t -> Yojson.Safe.t

val snapshot : t -> Workspace_snapshot.snapshot
val cached_snapshot : t -> Workspace_snapshot.snapshot option
val publish_snapshot : t -> unit
