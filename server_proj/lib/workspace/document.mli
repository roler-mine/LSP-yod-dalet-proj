module T = Lsp.Types

type t = private {
  uri : T.DocumentUri.t;
  file : string option;

  text : string;
  index : Text_index.t;

  pre_text : string;
  imports : Preprocess.import list;
  compool_def : string option;

  pre_diags : T.Diagnostic.t list;
  import_diags : T.Diagnostic.t list;
  parse_diags : T.Diagnostic.t list;

  ast : Ast.program option;

  diags : T.Diagnostic.t list;
}

val make : uri:T.DocumentUri.t -> file:string option -> text:string -> t

val apply_changes_and_reparse :
  changes:T.TextDocumentContentChangeEvent.t list ->
  t ->
  t

val with_import_diags : T.Diagnostic.t list -> t -> t

val diagnostics : t -> T.Diagnostic.t list
val ast_dump : t -> string option
val imports : t -> Preprocess.import list
val text : t -> string
