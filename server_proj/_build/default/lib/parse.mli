module T = Lsp.Types

type output = {
  ast : Ast.program option;
  diags : T.Diagnostic.t list;
  ast_dump : string option;
}

module Debug : sig
  val string_of_token : Parser.token -> string
end

val parse_text :
  file:string option ->
  dump_ast:bool ->
  text:string ->
  output
