(** Module overview: Produces code actions from diagnostics and workspace context. *)

module T = Lsp.Types

val code_actions_for :
  Workspace_state.t -> uri:T.DocumentUri.t -> range:T.Range.t -> T.CodeAction.t list
