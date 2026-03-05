module T = Lsp.Types

type t

type bg_tick_mode =
  | BgTickInteractive
  | BgTickIdle

val create : unit -> t

val set_root_uri : t -> T.DocumentUri.t option -> unit
val set_root_path : t -> string option -> unit
val rescan : t -> unit
val revalidate_all : t -> T.DocumentUri.t list
val compool_count : t -> int

val open_doc : ?force_provisional:bool -> t -> uri:T.DocumentUri.t -> file:string option -> text:string -> unit
val change_doc : t -> uri:T.DocumentUri.t -> changes:T.TextDocumentContentChangeEvent.t list -> unit
val close_doc : t -> uri:T.DocumentUri.t -> unit
val apply_watched_file_changes : t -> changes:(string * [ `Created | `Changed | `Deleted ]) list -> unit
val background_tick :
  t ->
  budget_ms:int ->
  mode:bg_tick_mode ->
  idle_quiet_ms:int ->
  last_message_ms:float ->
  unit
val effective_bg_tick_budget_ms : t -> base_budget_ms:int -> int
val drain_pending_diag_updates : t -> max_items:int -> (T.DocumentUri.t * T.Diagnostic.t list) list
val drain_open_diag_revalidate_uris : t -> max_items:int -> T.DocumentUri.t list
val startup_background_budget_ms : t -> base_budget_ms:int -> int
val startup_diag_hover_ready_now : t -> bool
val startup_is_ready_now : t -> bool
val startup_readiness_json_for_report : t -> Yojson.Safe.t
val workspace_ready_event_json : t -> Yojson.Safe.t option
val startup_phase_event_json : t -> Yojson.Safe.t option
val startup_miss_event_json : t -> Yojson.Safe.t option
val open_doc_converged : t -> uri:T.DocumentUri.t -> bool
val request_cancelled : t -> bool
val with_request_cancel_checker : t -> (unit -> bool) -> (unit -> 'a) -> 'a

val diagnostics_for : t -> uri:T.DocumentUri.t -> T.Diagnostic.t list
val ast_dump_for : t -> uri:T.DocumentUri.t -> string option
val cst_dump_for : t -> uri:T.DocumentUri.t -> string option

val document_symbols_json_for : t -> uri:T.DocumentUri.t -> Yojson.Safe.t
val definition_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val declaration_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val type_definition_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val implementation_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val references_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> include_decl:bool -> Yojson.Safe.t
val workspace_symbols_json_for : t -> query:string -> Yojson.Safe.t
val hover_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val signature_help_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val prepare_rename_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val rename_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> new_name:string -> Yojson.Safe.t
val completion_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val code_actions_json_for : t -> uri:T.DocumentUri.t -> range:T.Range.t -> Yojson.Safe.t
val inlay_hints_json_for : t -> uri:T.DocumentUri.t -> range:T.Range.t -> Yojson.Safe.t
val semantic_tokens_full_json_for : t -> uri:T.DocumentUri.t -> Yojson.Safe.t
val semantic_tokens_range_json_for : t -> uri:T.DocumentUri.t -> range:T.Range.t -> Yojson.Safe.t

val lsif_index_json : t -> Yojson.Safe.t
val lsif_delta_json : t -> base_revision:int -> Yojson.Safe.t
val perf_stats_json : t -> Yojson.Safe.t
val debug_report_for : t -> uri:T.DocumentUri.t -> max_tokens:int -> Yojson.Safe.t
