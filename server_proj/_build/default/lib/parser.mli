
(* The type of tokens. *)

type token = 
  | WHILE
  | THEN
  | TABLE
  | STRING of (string)
  | STAR
  | SLASH
  | SEMI
  | RPAR
  | RETURN
  | REAL of (float)
  | PROC
  | PLUS
  | NE
  | MINUS
  | LT
  | LPAR
  | LE
  | ITEM
  | INT of (int)
  | IF
  | IDENT of (string)
  | GT
  | GE
  | EQ
  | EOF
  | END
  | ELSE
  | DO
  | COMMA
  | COLON
  | CHAR of (char)
  | BOOL of (bool)
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val compilation_unit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.compilation_unit)
