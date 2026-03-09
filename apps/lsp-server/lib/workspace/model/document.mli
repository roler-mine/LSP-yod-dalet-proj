module T = Lsp.Types

type t = private {
  uri : T.DocumentUri.t;
  file : string option;
  rev : int;
  parse_rev : int;
  text : string;
  index : Text_index.t;
  imports : Preprocess.import list;
  compool_def : string option;
  defines : Preprocess.define list;
  pre_diags : T.Diagnostic.t list;
  import_diags : T.Diagnostic.t list;
  parse_diags : T.Diagnostic.t list;
  ast : Ast.program option;
  diags : T.Diagnostic.t list;
}

val make : uri:T.DocumentUri.t -> file:string option -> text:string -> t

val make_unparsed :
  uri:T.DocumentUri.t ->
  file:string option ->
  text:string ->
  parse_diags:T.Diagnostic.t list ->
  t

val apply_changes_and_reparse :
  changes:T.TextDocumentContentChangeEvent.t list -> t -> t

val apply_changes_no_reparse :
  changes:T.TextDocumentContentChangeEvent.t list -> t -> t

val apply_content_change :
  text:string ->
  index:Text_index.t ->
  T.TextDocumentContentChangeEvent.t ->
  string * Text_index.t

val slice_of_range :
  text:string -> index:Text_index.t -> T.Range.t -> string option

val with_import_diags : T.Diagnostic.t list -> t -> t
val with_parse_diags : T.Diagnostic.t list -> t -> t
val diagnostics : t -> T.Diagnostic.t list
val ast_dump : t -> string option
val imports : t -> Preprocess.import list
val text : t -> string
val ensure_parsed : t -> t
val drop_ast : t -> t
