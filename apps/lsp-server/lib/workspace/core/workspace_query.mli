module T = Lsp.Types

type query_context = {
  ws : Workspace_state.t;
  doc : Document.t;
  pos : T.Position.t;
}

type symbol_ref = {
  symbol_id : string option;
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
val references_at_position :
  include_declaration:bool -> query_context -> references_result
val hover_at_position : query_context -> hover_result

val definition_locations_for :
  Workspace_state.t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list

val references_locations_for :
  Workspace_state.t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  include_decl:bool ->
  T.Location.t list

val hover_for :
  Workspace_state.t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Hover.t option

val debug_report_json : Workspace_state.t -> Document.t -> Yojson.Safe.t
