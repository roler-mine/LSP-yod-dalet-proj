
(* The type of tokens. *)

type token = 
  | XOR
  | WITH
  | WHILE
  | WHEN
  | TYPEATOM of (string)
  | TYPE
  | TRUE
  | TO
  | THEN
  | TERM
  | TABLE
  | STRING of (string)
  | STOP
  | STATUS
  | STATIC
  | START
  | STAR
  | SLASH
  | SEMI
  | RPAREN
  | RETURN
  | REP
  | RENT
  | REF
  | REC
  | RBRACK
  | PROGRAM
  | PROC
  | POS
  | PLUS
  | PARALLEL
  | OVERLAY
  | OTHERWISE
  | OR
  | OF
  | NULL
  | NQ
  | NOT
  | NEQ
  | MOD
  | MINUS
  | LT
  | LS
  | LQ
  | LPAREN
  | LIKE
  | LE
  | LBRACK
  | LABEL
  | ITEM
  | INT of (int)
  | INSTANCE
  | INLINE
  | IF
  | IDENT of (string)
  | ICOMPOOL
  | GT
  | GR
  | GQ
  | GOTO
  | GE
  | FUNCTION
  | FOR
  | FLOAT of (float)
  | FALSE
  | FALLTHRU
  | EXIT
  | EQV
  | EQUAL
  | EQ
  | EOF
  | END
  | ELSIF
  | ELSE
  | DIRECTIVE_NAME of (string)
  | DEFINE
  | DEFAULT
  | DEF
  | CONSTANT
  | COMPOOL
  | COMMA
  | COLON
  | CASE
  | BYVAL
  | BYRES
  | BYREF
  | BY
  | BLOCK
  | BEGIN
  | BEAD of (int * string)
  | BANG
  | AND
  | ABORT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val compilation_unit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (module_)
