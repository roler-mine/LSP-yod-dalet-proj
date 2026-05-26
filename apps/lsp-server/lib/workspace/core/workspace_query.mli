(** Module overview: High-level query API for hover, definition, references, AST/CST dumps, and debug output. *)

module T = Lsp.Types

type query_context = {
  ws : Workspace_state.t;
  doc : Document.t;
  pos : T.Position.t;
}

type symbol_ref_id =
  | LegacySymbolId of string
  | CrossFileSymbolId of Cross_file_index.symbol_id

type symbol_ref = {
  symbol_id : symbol_ref_id option;
  name : string;
  key : string;
  loc : Ast.Loc.t;
  def : Workspace_nav_model.def option;
  readiness : Workspace_readiness.t;
  authority : Workspace_readiness.authority;
}

type 'a query_result = 'a Workspace_readiness.result
type definition_result = T.Location.t list query_result
type references_result = T.Location.t list query_result
type hover_result = T.Hover.t option query_result

val context :
  Workspace_state.t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  query_context option

val readiness_for_doc :
  Workspace_state.t -> Document.t -> Workspace_readiness.t * Workspace_readiness.reason list

val symbol_at_position : query_context -> symbol_ref option query_result
val hover_target_at_position : query_context -> symbol_ref option query_result
val definition_at_position : query_context -> definition_result
val implementation_at_position : query_context -> definition_result
val type_definition_at_position : query_context -> definition_result
val references_at_position :
  include_declaration:bool -> query_context -> references_result
val hover_at_position : query_context -> hover_result
val completions_at_position :
  query_context -> T.CompletionItem.t list query_result

val diagnostics_for_file :
  Workspace_state.t -> uri:T.DocumentUri.t -> T.Diagnostic.t list

val document_symbols_for_file :
  Workspace_state.t ->
  uri:T.DocumentUri.t ->
  [ `DocumentSymbol of T.DocumentSymbol.t
  | `SymbolInformation of T.SymbolInformation.t ]
  list
  option

val workspace_symbols_for :
  Workspace_state.t -> query:string -> T.SymbolInformation.t list

val workspace_symbols_stream :
  Workspace_state.t ->
  query:string ->
  emit:(T.SymbolInformation.t list -> unit) ->
  T.SymbolInformation.t list

val definition_locations_for :
  Workspace_state.t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list

val type_definition_locations_for :
  Workspace_state.t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list

val implementation_locations_for :
  Workspace_state.t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list

val references_locations_for :
  Workspace_state.t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  include_decl:bool ->
  T.Location.t list

val hover_for :
  Workspace_state.t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Hover.t option

val completion_items_for :
  Workspace_state.t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  T.CompletionItem.t list

val prepare_rename_for :
  Workspace_state.t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  [ `Range of T.Range.t | `RangeWithPlaceholder of T.Range.t * string ] option

val rename_for :
  Workspace_state.t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  new_name:string ->
  T.WorkspaceEdit.t option

val explain_symbol_resolution_json :
  Workspace_state.t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  Yojson.Safe.t

val debug_report_json : Workspace_state.t -> Document.t -> Yojson.Safe.t
