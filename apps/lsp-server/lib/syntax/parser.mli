(** Public syntax parser API.

    Menhir owns the implementation in [parser.mly]. This source interface keeps
    the token type available to the lexer while exposing only the parser driver
    that the rest of the server uses. *)

type token =
  | XOR
  | WHILE
  | TYPE
  | TRUE
  | THEN
  | TERM
  | TABLE
  | STRINGLIT of string
  | STOP
  | STATIC
  | START
  | STAR
  | SLASH
  | SEMI
  | RPAREN
  | RETURN
  | REF
  | PROGRAM
  | PROC
  | POW
  | POS
  | PLUS
  | OR
  | NOT
  | NE
  | MOD
  | MINUS
  | LT
  | LPAREN
  | LE
  | ITEM
  | INTLIT of string
  | IF
  | ID of string
  | ICOMPOOL
  | GT
  | GOTO
  | GE
  | FOR
  | FLOATLIT of string
  | FALSE
  | FALLTHRU
  | EXIT
  | EQV
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | DEFINE
  | DEFAULT
  | DEF
  | CONV_R
  | CONV_L
  | CONSTANT
  | COMPOOL
  | COMMA
  | COLON
  | CASE
  | BY
  | BLOCK
  | BEGIN
  | BANG
  | AT
  | AND
  | ABORT

type output = {
  ast : Ast.program option;
  diags : Lsp.Types.Diagnostic.t list;
  ast_dump : string option;
}

type profile = Interactive | Background | Batch | Debug

type checkpoint_cache

type checkpoint_stats = {
  cache_hit : bool;
  checkpoint_count : int;
  checkpoint_reused : bool;
  fallback_reason : string option;
}

type checkpointed_output = {
  output : output;
  checkpoint_cache : checkpoint_cache;
  checkpoint_stats : checkpoint_stats;
}

type token_span = {
  tok : token;
  start_off : int;
  end_off : int;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
  lexeme : string option;
}

module Debug : sig
  val string_of_token : token -> string
end

val token_span_of_lexing_positions :
  ?lexeme:string -> token -> Lexing.position -> Lexing.position -> token_span

val token_span_start_p : file:string option -> token_span -> Lexing.position
val token_span_end_p : file:string option -> token_span -> Lexing.position

val parse_tokens :
  file:string option ->
  dump_ast:bool ->
  profile:profile ->
  tokens:token_span array ->
  output

val parse_tokens_checkpointed :
  ?previous:checkpoint_cache ->
  ?dirty_token:int ->
  file:string option ->
  dump_ast:bool ->
  profile:profile ->
  tokens:token_span array ->
  unit ->
  checkpointed_output
