{
open Lexing
open Context
open Parser
open Options

let init _filename channel : Lexing.lexbuf =
  Lexing.from_channel channel

}

let digit = ['0'-'9']
let hexadecimal_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let nondigit = ['_' 'a'-'z' 'A'-'Z']

let hex_quad = hexadecimal_digit hexadecimal_digit
                 hexadecimal_digit hexadecimal_digit
let universal_character_name =
    "\\u" hex_quad
  | "\\U" hex_quad hex_quad

let identifier_nondigit =
    nondigit
  | universal_character_name

let identifier = identifier_nondigit (identifier_nondigit|digit)*

let whitespace_char_no_newline = [' ' '\t' '\011' '\012' '\r']

(* Integer constants *)
let nonzero_digit = ['1'-'9']
let decimal_constant = nonzero_digit digit*

let octal_digit = ['0'-'7']
let octal_constant = '0' octal_digit*

let hexadecimal_prefix = "0x" | "0X"
let hexadecimal_constant =
  hexadecimal_prefix hexadecimal_digit+

let unsigned_suffix = ['u' 'U']
let long_suffix = ['l' 'L']
let long_long_suffix = "ll" | "LL"
let integer_suffix =
    unsigned_suffix long_suffix?
  | unsigned_suffix long_long_suffix
  | long_suffix unsigned_suffix?
  | long_long_suffix unsigned_suffix?

let integer_constant =
    decimal_constant integer_suffix?
  | octal_constant integer_suffix?
  | hexadecimal_constant integer_suffix?

let sign = ['-' '+']
let digit_sequence = digit+
let floating_suffix = ['f' 'l' 'F' 'L']

let fractional_constant =
    digit_sequence? '.' digit_sequence
  | digit_sequence '.'
let exponent_part =
    ['e' 'E'] sign? digit_sequence
let decimal_floating_constant =
    fractional_constant exponent_part? floating_suffix?
  | digit_sequence exponent_part floating_suffix?

let hexadecimal_digit_sequence = hexadecimal_digit+
let hexadecimal_fractional_constant =
    hexadecimal_digit_sequence? '.' hexadecimal_digit_sequence
  | hexadecimal_digit_sequence '.'
let binary_exponent_part =
    ['p' 'P'] sign? digit_sequence
let hexadecimal_floating_constant =
    hexadecimal_prefix hexadecimal_fractional_constant
        binary_exponent_part floating_suffix?
  | hexadecimal_prefix hexadecimal_digit_sequence
        binary_exponent_part floating_suffix?

let preprocessing_number =
  '.'? ['0'-'9']
  (['0'-'9' 'A'-'Z' 'a'-'z' '_' '.'] | ['e' 'E' 'p' 'P']['+' '-'])*

let simple_escape_sequence =
  '\\' ['\''  '\"'  '?'  '\\'  'a'  'b'  'f'  'n'  'r'  't'  'v']
let octal_escape_sequence =
  '\\' (octal_digit
         | octal_digit octal_digit
         | octal_digit octal_digit octal_digit)
let hexadecimal_escape_sequence = "\\x" hexadecimal_digit+
let escape_sequence =
    simple_escape_sequence
  | octal_escape_sequence
  | hexadecimal_escape_sequence
  | universal_character_name

rule initial = parse
  | whitespace_char_no_newline+   { initial lexbuf }
  | '\n'                          { new_line lexbuf; initial_linebegin lexbuf }
  | "/*"                          { multiline_comment lexbuf; initial lexbuf }
  | "//"                          { singleline_comment lexbuf; initial_linebegin lexbuf }
  | integer_constant              { CONSTANT }
  | decimal_floating_constant     { CONSTANT }
  | hexadecimal_floating_constant { CONSTANT }
  | preprocessing_number          { failwith "These characters form a preprocessor number, but not a constant" }
  | (['L' 'u' 'U']|"") "'"        { char lexbuf; char_literal_end lexbuf; CONSTANT }
  | (['L' 'u' 'U']|""|"u8") "\""  { string_literal lexbuf; STRING_LITERAL }
  | "..."                         { ELLIPSIS }
  | "+="                          { ADD_ASSIGN }
  | "-="                          { SUB_ASSIGN }
  | "*="                          { MUL_ASSIGN }
  | "/="                          { DIV_ASSIGN }
  | "%="                          { MOD_ASSIGN }
  | "|="                          { OR_ASSIGN }
  | "&="                          { AND_ASSIGN }
  | "^="                          { XOR_ASSIGN }
  | "<<="                         { LEFT_ASSIGN }
  | ">>="                         { RIGHT_ASSIGN }
  | "<<"                          { LEFT }
  | ">>"                          { RIGHT }
  | "=="                          { EQEQ }
  | "!="                          { NEQ }
  | "<="                          { LEQ }
  | ">="                          { GEQ }
  | "="                           { EQ }
  | "<"                           { LT }
  | ">"                           { GT }
  | "++"                          { INC }
  | "--"                          { DEC }
  | "->"                          { PTR }
  | "+"                           { PLUS }
  | "-"                           { MINUS }
  | "*"                           { STAR }
  | "/"                           { SLASH }
  | "%"                           { PERCENT }
  | "!"                           { BANG }
  | "&&"                          { ANDAND }
  | "||"                          { BARBAR }
  | "&"                           { AND }
  | "|"                           { BAR }
  | "^"                           { HAT }
  | "?"                           { QUESTION }
  | ":"                           { COLON }
  | "~"                           { TILDE }
  | "{"|"<%"                      { LBRACE }
  | "}"|"%>"                      { RBRACE }
  | "["|"<:"                      { LBRACK }
  | "]"|":>"                      { RBRACK }
  | "("                           { LPAREN }
  | ")"                           { RPAREN }
  | ";"                           { SEMICOLON }
  | ","                           { COMMA }
  | "."                           { DOT }
  | "_Alignas"                    { ALIGNAS }
  | "_Alignof"                    { ALIGNOF }
  | "_Atomic"                     { ATOMIC }
  | "_Bool"                       { BOOL }
  | "_Complex"                    { COMPLEX }
  | "_Generic"                    { GENERIC }
  | "_Imaginary"                  { IMAGINARY }
  | "_Noreturn"                   { NORETURN }
  | "_Static_assert"              { STATIC_ASSERT }
  | "_Thread_local"               { THREAD_LOCAL }
  | "auto"                        { AUTO }
  | "break"                       { BREAK }
  | "case"                        { CASE }
  | "char"                        { CHAR }
  | "const"                       { CONST }
  | "continue"                    { CONTINUE }
  | "default"                     { DEFAULT }
  | "do"                          { DO }
  | "double"                      { DOUBLE }
  | "else"                        { ELSE }
  | "enum"                        { ENUM }
  | "extern"                      { EXTERN }
  | "float"                       { FLOAT }
  | "for"                         { FOR }
  | "goto"                        { GOTO }
  | "if"                          { IF }
  | "inline"                      { INLINE }
  | "int"                         { INT }
  | "long"                        { LONG }
  | "register"                    { REGISTER }
  | "restrict"                    { RESTRICT }
  | "return"                      { RETURN }
  | "short"                       { SHORT }
  | "signed"                      { SIGNED }
  | "sizeof"                      { SIZEOF }
  | "static"                      { STATIC }
  | "struct"                      { STRUCT }
  | "switch"                      { SWITCH }
  | "typedef"                     { TYPEDEF }
  | "union"                       { UNION }
  | "unsigned"                    { UNSIGNED }
  | "void"                        { VOID }
  | "volatile"                    { VOLATILE }
  | "while"                       { WHILE }
  | identifier as id              { NAME id }
  | eof                           { EOF }
  | _                             { failwith "Lexer error" }

and initial_linebegin = parse
  | '\n'                          { new_line lexbuf; initial_linebegin lexbuf }
  | whitespace_char_no_newline    { initial_linebegin lexbuf }
  | '#' | "%:"                    { hash lexbuf }
  | ""                            { initial lexbuf }

and char = parse
  | simple_escape_sequence        { }
  | octal_escape_sequence         { }
  | hexadecimal_escape_sequence   { }
  | universal_character_name      { }
  | '\\' _                        { failwith "incorrect escape sequence" }
  | _                             { }

and char_literal_end = parse
  | '\''       { }
  | '\n' | eof { failwith "missing terminating \"'\" character" }
  | ""         { char lexbuf; char_literal_end lexbuf }

and string_literal = parse
  | '\"'       { }
  | '\n' | eof { failwith "missing terminating '\"' character" }
  | ""         { char lexbuf; string_literal lexbuf }

and hash = parse
  | whitespace_char_no_newline+ digit* whitespace_char_no_newline*
    "\"" [^ '\n' '\"']* "\"" [^ '\n']* '\n'
  | whitespace_char_no_newline* "pragma"
    whitespace_char_no_newline+ [^ '\n']* '\n'
      { new_line lexbuf; initial_linebegin lexbuf }
  | [^ '\n']* eof
      { failwith "unexpected end of file" }
  | _
      { failwith "Lexer error" }

and multiline_comment = parse
  | "*/"   { () }
  | eof    { failwith "unterminated comment" }
  | '\n'   { new_line lexbuf; multiline_comment lexbuf }
  | _      { multiline_comment lexbuf }

and singleline_comment = parse
  | '\n'   { new_line lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }

{

  let lexer : lexbuf -> token =
    fun lexbuf ->
      if lexbuf.lex_curr_p.pos_cnum = lexbuf.lex_curr_p.pos_bol then
        initial_linebegin lexbuf
      else
        initial lexbuf

  type lexer_state =
    | SRegular          
    | SIdent of string  

  let lexer : lexbuf -> token =
    let st = ref SRegular in
    fun lexbuf ->
      match !st with

      | SIdent id ->
          st := SRegular;
          if is_typedefname id then TYPE else VARIABLE

      | SAtomic
      | SRegular ->
          let token = lexer lexbuf in
          match !st, token with
          | _, NAME id ->
              st := SIdent id;
              token

          | SAtomic, LPAREN ->
              st := SRegular;
              ATOMIC_LPAREN

          | _, ATOMIC ->
              st := (if !atomic_strict_syntax then SAtomic else SRegular);
              token

          | _, _ ->
              st := SRegular;
              token

}