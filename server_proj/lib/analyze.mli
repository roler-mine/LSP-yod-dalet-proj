module T = Lsp.Types

type t = private
  { ast : Ast.module_ option
  ; symbols : Symbols.t
  ; diagnostics : T.Diagnostic.t list
  }

(* Parse using Menhir/ocamllex; returns either AST or diagnostics. *)
val parse :
  T.DocumentUri.t ->
  string ->
  (Ast.module_, T.Diagnostic.t list) result

(* Main entry: combines parse + token-based analysis fallback. *)
val analyze :
  uri:T.DocumentUri.t ->
  text:string ->
  t

(* Constructors / updaters needed by other modules because [t] is private. *)
val empty : ?diagnostics:T.Diagnostic.t list -> unit -> t
val with_diagnostics : t -> T.Diagnostic.t list -> t
