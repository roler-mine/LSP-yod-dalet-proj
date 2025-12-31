
(* The type of tokens. *)

type token = 
  | XOR
  | WORDSIZE
  | WHILE
  | UBOUND
  | TYPE
  | TRUE
  | THEN
  | TERM
  | TABLE
  | STOP
  | STATUS
  | STATIC
  | START
  | STAR
  | SLASH
  | SHIFTR
  | SHIFTL
  | SGNL
  | SEMI
  | RPAREN
  | RETURN
  | REP
  | RENT
  | REF
  | REC
  | REAL of (string)
  | RCONV
  | PROGRAM
  | PROC
  | POW
  | POS
  | PLUS
  | PARALLEL
  | OVERLAY
  | OR
  | NULL
  | NOT
  | NEXT
  | NE
  | NAME of (string)
  | MOD
  | MINUS
  | LT
  | LPAREN
  | LOC
  | LIKE
  | LETTER of (char)
  | LE
  | LCONV
  | LBOUND
  | LAST
  | LABEL
  | ITRACE
  | ITEM
  | ISKIP
  | IREDUCIBLE
  | IREARRANGE
  | IORDER
  | INT of (int)
  | INSTANCE
  | INOLIST
  | INLINE
  | ILIST
  | ILINKAGE
  | ILEFTRIGHT
  | IISBASE
  | IINTERFERENCE
  | IINITIALIZE
  | IF
  | IEND
  | IEJECT
  | IDROP
  | ICOPY
  | ICOMPOOL
  | IBEGIN
  | IBASE
  | GT
  | GOTO
  | GE
  | FUNCTION
  | FOR
  | FIRST
  | FALSE
  | FALLTHRU
  | EXIT
  | ERROR of (string)
  | EQV
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | DEFINE_STRING of (string)
  | DEFINE
  | DEFAULT
  | DEF
  | CONSTANT
  | COMPOOL
  | COMMENT of (string)
  | COMMA
  | COLON
  | CHARLIT of (string)
  | CASE
  | BYTESIZE
  | BY
  | BLOCK
  | BITSIZE
  | BITLIT of (int * string)
  | BIT
  | BEGIN
  | AT
  | AS
  | AND
  | ABORT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val compilation_unit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.compilation_unit Ast.located)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val compilation_unit: Lexing.position -> (Ast.compilation_unit Ast.located) MenhirInterpreter.checkpoint
  
end
