(** Module overview: Computes and resolves CodeLens entries for references and change impact. *)

module T = Lsp.Types

val code_lenses_for :
  Workspace_state.t -> uri:T.DocumentUri.t -> T.CodeLens.t list

val resolve_code_lens : Workspace_state.t -> T.CodeLens.t -> T.CodeLens.t
