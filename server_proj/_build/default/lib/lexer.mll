(* lib/lexer.mll *)

{
  open Parser

  exception Lexing_error of string * Lexing.position * Lexing.position

  let error (lexbuf : Lexing.lexbuf) (msg : string) =
    raise (Lexing_error (msg, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p))

  let unescape_char = function
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'r' -> '\r'
    | '\\' -> '\\'
    | '"' -> '"'
    | '\'' -> '\''
    | c -> c

  let kw (s : string) =
    match String.uppercase_ascii s with
    | "ITEM"   -> ITEM
    | "TABLE"  -> TABLE
    | "PROC"   -> PROC
    | "END"    -> END
    | "IF"     -> IF
    | "THEN"   -> THEN
    | "ELSE"   -> ELSE
    | "WHILE"  -> WHILE
    | "DO"     -> DO
    | "RETURN" -> RETURN
    | "TRUE"   -> BOOL true
    | "FALSE"  -> BOOL false
    | _        -> IDENT s
}

let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = ['A'-'Z' 'a'-'z' '0'-'9' '_']

rule token = parse
  | [' ' '\t' '\r']                 { token lexbuf }
  | '\n'                            { Lsp_convert.new_line lexbuf; token lexbuf }

  | "/*"                            { comment lexbuf; token lexbuf }
  | "(*"                            { comment_ml lexbuf; token lexbuf }

  | '!' [^ '\n']*                   { token lexbuf }
  | "--" [' ' '\t'] [^ '\n']*       { token lexbuf }  (* avoids eating A--B *)

  | '('                             { LPAR }
  | ')'                             { RPAR }
  | ','                             { COMMA }
  | ';'                             { SEMI }
  | ':' '='                         { ASSIGN }  (* := *)
  | ':'                             { COLON }

  | "<="                            { LE }
  | ">="                            { GE }
  | "<>"                            { NE }
  | '='                             { EQ }
  | '<'                             { LT }
  | '>'                             { GT }

  | '+'                             { PLUS }
  | '-'                             { MINUS }
  | '*'                             { STAR }
  | '/'                             { SLASH }

  | '"'                             { let b = Buffer.create 32 in STRING (string_lit b lexbuf) }
  | '\''                            { CHAR (char_lit lexbuf) }

  | digit+ '.' digit* (['e''E'] ['+''-']? digit+)? as f
                                    { REAL (float_of_string f) }
  | digit+ (['e''E'] ['+''-']? digit+)? as s
                                    {
                                      if String.contains s 'e' || String.contains s 'E'
                                      then REAL (float_of_string s)
                                      else INT (int_of_string s)
                                    }

  | alpha alnum* as id              { kw id }

  | eof                             { EOF }
  | _                               { error lexbuf ("Unexpected character: " ^ Lexing.lexeme lexbuf) }

and comment = parse
  | "*/"                            { () }
  | '\n'                            { Lsp_convert.new_line lexbuf; comment lexbuf }
  | eof                             { error lexbuf "Unterminated block comment." }
  | _                               { comment lexbuf }

and comment_ml = parse
  | "*)"                            { () }
  | '\n'                            { Lsp_convert.new_line lexbuf; comment_ml lexbuf }
  | eof                             { error lexbuf "Unterminated block comment." }
  | _                               { comment_ml lexbuf }

and string_lit b = parse
  | '"'                             { Buffer.contents b }
  | '\\' (_ as c)                   { Buffer.add_char b (unescape_char c); string_lit b lexbuf }
  | '\n'                            { error lexbuf "Unterminated string literal (newline)." }
  | eof                             { error lexbuf "Unterminated string literal (EOF)." }
  | (_ as c)                        { Buffer.add_char b c; string_lit b lexbuf }

and char_lit = parse
  | '\\' (_ as c) '\''              { unescape_char c }
  | (_ as c) '\''                   { c }
  | '\n'                            { error lexbuf "Unterminated char literal (newline)." }
  | eof                             { error lexbuf "Unterminated char literal (EOF)." }
  | _                               { error lexbuf "Bad char literal." }
