(* lib/analyze.mli *)

module T = Lsp.Types

type t = private
  { ast : Ast.compilation_unit option
  ; symbols : Symbols.t
  ; diagnostics : T.Diagnostic.t list
  }

val parse :
  T.DocumentUri.t ->
  string ->
  (Ast.compilation_unit, T.Diagnostic.t list) result

val analyze :
  uri:T.DocumentUri.t ->
  text:string ->
  t
