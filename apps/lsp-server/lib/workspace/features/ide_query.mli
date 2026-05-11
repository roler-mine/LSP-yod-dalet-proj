module T = Lsp.Types

val hover :
  Workspace_snapshot.snapshot ->
  uri:string ->
  pos:T.Position.t ->
  T.Hover.t option

val completion :
  Workspace_snapshot.snapshot ->
  uri:string ->
  pos:T.Position.t ->
  T.CompletionItem.t list

val definition :
  Workspace_snapshot.snapshot ->
  uri:string ->
  pos:T.Position.t ->
  T.Location.t list

val references :
  Workspace_snapshot.snapshot ->
  uri:string ->
  pos:T.Position.t ->
  T.Location.t Seq.t

val document_symbols :
  Workspace_snapshot.snapshot -> uri:string -> T.DocumentSymbol.t list

val workspace_symbols :
  Workspace_snapshot.snapshot -> query:string -> T.SymbolInformation.t list

val semantic_tokens_range :
  Workspace_snapshot.snapshot -> uri:string -> range:T.Range.t -> int array
