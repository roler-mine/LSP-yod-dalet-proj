(** Module overview: Inlay hint provider for types, procedure calls, and declaration details. *)

module T = Lsp.Types

val inlay_hints_for :
  Workspace_state.t -> uri:T.DocumentUri.t -> range:T.Range.t -> T.InlayHint.t list
