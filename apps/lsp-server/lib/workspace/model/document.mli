module T = Lsp.Types

type t = private {
  uri : T.DocumentUri.t;
  file : string option;
  lsp_version : int option;
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
  syntax : Syntax_cache.t option;
  diags : T.Diagnostic.t list;
}

type parsed_payload = {
  parsed_rev : int;
  parsed_ast : Ast.program option;
  parsed_syntax : Syntax_cache.t option;
  parsed_diags : T.Diagnostic.t list;
}

val make : uri:T.DocumentUri.t -> file:string option -> text:string -> t

val make_versioned :
  lsp_version:int option ->
  uri:T.DocumentUri.t ->
  file:string option ->
  text:string ->
  t

val make_with_profile :
  profile:Parser.profile ->
  uri:T.DocumentUri.t ->
  file:string option ->
  text:string ->
  t

val make_with_profile_versioned :
  lsp_version:int option ->
  profile:Parser.profile ->
  uri:T.DocumentUri.t ->
  file:string option ->
  text:string ->
  t

val make_unparsed :
  uri:T.DocumentUri.t ->
  file:string option ->
  text:string ->
  parse_diags:T.Diagnostic.t list ->
  t

val make_unparsed_versioned :
  lsp_version:int option ->
  uri:T.DocumentUri.t ->
  file:string option ->
  text:string ->
  parse_diags:T.Diagnostic.t list ->
  t

val make_parse_skipped :
  uri:T.DocumentUri.t ->
  file:string option ->
  text:string ->
  parse_diags:T.Diagnostic.t list ->
  t

val make_parse_skipped_versioned :
  lsp_version:int option ->
  uri:T.DocumentUri.t ->
  file:string option ->
  text:string ->
  parse_diags:T.Diagnostic.t list ->
  t

val reparse_for_profile : profile:Parser.profile -> t -> t

val apply_changes_and_reparse :
  ?lsp_version:int ->
  changes:T.TextDocumentContentChangeEvent.t list -> t -> t

val apply_changes_and_reparse_with_profile :
  ?lsp_version:int ->
  profile:Parser.profile -> changes:T.TextDocumentContentChangeEvent.t list -> t -> t

val apply_changes_no_reparse :
  ?lsp_version:int ->
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
val with_parse_skipped : T.Diagnostic.t list -> t -> t
val diagnostics : t -> T.Diagnostic.t list
val lsp_version : t -> int option
val with_lsp_version : int option -> t -> t
val ast_dump : ?max_depth:int -> ?max_nodes:int -> t -> string option
val imports : t -> Preprocess.import list
val text : t -> string
val ensure_parsed : t -> t
val current_parse : t -> parsed_payload option
val current_ast : t -> Ast.program option
val has_current_parse : t -> bool
val has_current_syntax : t -> bool
val drop_ast : t -> t
