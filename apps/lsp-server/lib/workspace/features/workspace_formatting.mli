(** Module overview: Conservative document and range formatter for Jovial source text. *)

module T = Lsp.Types

type options = { tab_size : int; insert_spaces : bool }

val default_options : options

val document_edits_for :
  Workspace_state.t ->
  uri:T.DocumentUri.t ->
  options:options ->
  T.TextEdit.t list

val range_edits_for :
  Workspace_state.t ->
  uri:T.DocumentUri.t ->
  range:T.Range.t ->
  options:options ->
  T.TextEdit.t list
