(** Module overview: Hover provider that combines syntax, semantic, type, and navigation metadata. *)

module T = Lsp.Types

val hover_semantic_for :
  Workspace_state.t -> Document.t -> pos:T.Position.t -> T.Hover.t option

val hover_skeleton_fallback_for :
  Workspace_state.t -> Document.t -> pos:T.Position.t -> T.Hover.t option

val hover_for :
  Workspace_state.t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Hover.t option
