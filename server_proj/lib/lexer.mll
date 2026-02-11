{
  (* lib/lexer.mll *)

  open Parser

  exception Lex_error of string * Lexing.position * Lexing.position

  let error lexbuf msg =
    raise (Lex_error (msg, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf))

  (* Case-insensitive keyword lookup. Keep this list aligned with %token keywords in parser.mly. *)
  let kw s : Parser.token option =
    match String.uppercase_ascii s with
    | "START"   -> Some START
    | "TERM"    -> Some TERM
    | "BEGIN"   -> Some BEGIN
    | "END"     -> Some END

    | "DEF"     -> Some DEF
    | "REF"     -> Some REF
    | "PROC"    -> Some PROC

    | "ITEM"    -> Some ITEM
    | "TABLE"   -> Some TABLE

    | "IF"      -> Some IF
    | "ELSE"    -> Some ELSE
    | "WHILE"   -> Some WHILE
    | "FOR"     -> Some FOR
    | "BY"      -> Some BY

    | "CASE"    -> Some CASE
    | "DEFAULT" -> Some DEFAULT

    | "EXIT"    -> Some EXIT
    | "GOTO"    -> Some GOTO
    | "RETURN"  -> Some RETURN
    | "ABORT"   -> Some ABORT
    | "STOP"    -> Some STOP

    | "TRUE"    -> Some TRUE
    | "FALSE"   -> Some FALSE

    | "NOT"     -> Some NOT
    | "AND"     -> Some AND
    | "OR"      -> Some OR
    | "XOR"     -> Some XOR
    | "EQV"     -> Some EQV

    | "MOD"     -> Some MOD
    | _         -> None

  let mk_id s =
    match kw s with
    | Some t -> t
    | None -> ID s
}

let digit = ['0'-'9']
let alpha = ['A'-'Z''a'-'z']

(* J73-ish names:
   start: letter or '$' (allow '_' as extension)
   body : letters digits '$' '_' and PRIME '
*)
let name_start = alpha | '$' | '_'
let name_char  = alpha | digit | '$' | '_' | '\''

let ws = [' ' '\t' '\r']
let nl = '\n'

let exp = ['e''E'] ['+''-']? digit+
let float1 = digit+ '.' digit* exp?
let float2 = '.' digit+ exp?
let float3 = digit+ exp

rule token = parse
  | ws+                 { token lexbuf }
  | nl                  { Lexing.new_line lexbuf; token lexbuf }

  (* JOVIAL comment forms *)
  | '"'                 { dq_comment lexbuf; token lexbuf }
  | '%'                 { pct_comment lexbuf; token lexbuf }

  (* extra comment forms *)
  | "/*"                { c_comment 1 lexbuf; token lexbuf }
  | "(*"                { p_comment 1 lexbuf; token lexbuf }
  | "//" [^ '\n']*      { token lexbuf }

  (* punctuation *)
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | ','                 { COMMA }
  | ';'                 { SEMI }
  | ':'                 { COLON }
  | '.'                 { DOT }
  | '!'                 { BANG }
  | '@'                 { AT }

  (* multi-char ops *)
  | "<>"                { NE }
  | "<="                { LE }
  | ">="                { GE }
  | "**"                { POW }

  (* single-char ops *)
  | '='                 { EQ }
  | '<'                 { LT }
  | '>'                 { GT }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { STAR }
  | '/'                 { SLASH }
  | '^'                 { POW }

  (* numbers *)
  | float1 as s         { FLOATLIT s }
  | float2 as s         { FLOATLIT s }
  | float3 as s         { FLOATLIT s }
  | digit+ as s         { INTLIT s }

  (* JOVIAL '...' literal:
     - '' inside becomes '
     - if length==1 => CHARLIT
     - else => STRINGLIT
  *)
  | '\''                {
                           let s = read_squoted (Buffer.create 32) lexbuf in
                           if String.length s = 1 then CHARLIT s.[0] else STRINGLIT s
                         }

  (* identifiers/keywords *)
  | name_start name_char* as s { mk_id s }

  | eof                 { EOF }
  | _ as c              { error lexbuf (Printf.sprintf "unexpected character: %C" c) }

and dq_comment = parse
  | '"'                 { () }
  | nl                  { Lexing.new_line lexbuf; dq_comment lexbuf }
  | eof                 { error lexbuf "unterminated \"...\" comment" }
  | _                   { dq_comment lexbuf }

and pct_comment = parse
  | '%'                 { () }
  | nl                  { Lexing.new_line lexbuf; pct_comment lexbuf }
  | eof                 { error lexbuf "unterminated %...% comment" }
  | _                   { pct_comment lexbuf }

and c_comment depth = parse
  | "/*"                { c_comment (depth + 1) lexbuf }
  | "*/"                { if depth = 1 then () else c_comment (depth - 1) lexbuf }
  | nl                  { Lexing.new_line lexbuf; c_comment depth lexbuf }
  | eof                 { error lexbuf "unterminated /* ... */ comment" }
  | _                   { c_comment depth lexbuf }

and p_comment depth = parse
  | "(*"                { p_comment (depth + 1) lexbuf }
  | "*)"                { if depth = 1 then () else p_comment (depth - 1) lexbuf }
  | nl                  { Lexing.new_line lexbuf; p_comment depth lexbuf }
  | eof                 { error lexbuf "unterminated (* ... *) comment" }
  | _                   { p_comment depth lexbuf }

and read_squoted buf = parse
  | "''"                { Buffer.add_char buf '\''; read_squoted buf lexbuf }
  | '\''                { Buffer.contents buf }
  | nl                  { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_squoted buf lexbuf }
  | eof                 { error lexbuf "unterminated '...' literal" }
  | _ as c              { Buffer.add_char buf c; read_squoted buf lexbuf }
