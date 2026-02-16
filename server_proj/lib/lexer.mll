{
  open Parser

  exception Lex_error of string * Lexing.position * Lexing.position

  let error lexbuf msg =
    raise (Lex_error (msg, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf))

  (* --- DEFINE handling (JOVIAL quirk) ---------------------------------
     Double-quotes are used for:
       1) Comments: " ... "
       2) DEFINE string: DEFINE NAME "define-string" "comment" ... ;
     Rule: the FIRST quoted string after DEFINE is meaningful; later quoted
     strings on the same DEFINE statement are comments.
  ---------------------------------------------------------------------- *)
  let define_active     = ref false
  let define_got_string = ref false
  let compool_active     = ref false
  let compool_got_string = ref false

  let define_enter () =
    define_active := true;
    define_got_string := false

  let define_reset () =
    define_active := false;
    define_got_string := false

  let compool_enter () =
    compool_active := true;
    compool_got_string := false

  let compool_reset () =
    compool_active := false;
    compool_got_string := false

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
    | "THEN"    -> Some THEN
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

    (* JOVIAL-ish extras; safe even if you parse them as directives *)
    | "PROGRAM" -> Some PROGRAM
    | "COMPOOL" -> Some COMPOOL
    | "ICOMPOOL"-> Some ICOMPOOL
    | "DEFINE"  -> Some DEFINE
    | "TYPE"    -> Some TYPE
    | "BLOCK"   -> Some BLOCK

    | _         -> None

  let mk_id s =
    match kw s with
    | Some DEFINE -> define_enter (); compool_reset (); DEFINE
    | Some COMPOOL -> compool_enter (); COMPOOL
    | Some ICOMPOOL -> compool_enter (); ICOMPOOL
    | Some t -> t
    | None -> ID s
}

let digit = ['0'-'9']
let alpha = ['A'-'Z''a'-'z']

(* JOVIAL identifiers can contain apostrophes, and '$' is common. *)
let name_start = alpha | '$' | '_'
let name_char  = alpha | digit | '$' | '_' | '\''

let ws = [' ' '\t' '\r']
let nl = '\n'

let exp = ['e''E'] ['+''-']? digit+
let float1 = digit+ '.' digit* exp?
let float2 = '.' digit+ exp?
let float3 = digit+ exp
let based_int = digit+ ['A'-'Z''a'-'z'] '\'' [^ '\'' '\n']+ '\''

rule token = parse
  | ws+                 { token lexbuf }
  | nl                  { Lexing.new_line lexbuf; compool_reset (); token lexbuf }

  (* %...% comments (JOVIAL style) *)
  | '%'                 { pct_comment lexbuf; token lexbuf }

  (* C-style comments are useful in test inputs; keep them. *)
  | "/*"                { c_comment 1 lexbuf; token lexbuf }

  (* JOVIAL conversion brackets: (* ... *) are NOT comments. *)
  | "(*"                { CONV_L }
  | "*)"                { CONV_R }

  (* Double-quote: DEFINE-string / COMPOOL-string (first one only) OR a comment. *)
  | '"'                 {
                          if !define_active && not !define_got_string then (
                            define_got_string := true;
                            STRINGLIT (read_dquoted (Buffer.create 64) lexbuf)
                          ) else if !compool_active && not !compool_got_string then (
                            compool_got_string := true;
                            STRINGLIT (read_dquoted (Buffer.create 64) lexbuf)
                          ) else (
                            dq_comment lexbuf;
                            token lexbuf
                          )
                        }

  (* punctuation / terminators *)
  | '('                 { LPAREN }
  | ')'                 { compool_reset (); RPAREN }
  | ','                 { COMMA }
  | ';'                 { define_reset (); compool_reset (); SEMI }
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
  | based_int as s      { INTLIT s }
  | float1 as s         { FLOATLIT s }
  | float2 as s         { FLOATLIT s }
  | float3 as s         { FLOATLIT s }
  | digit+ as s         { INTLIT s }

  (* JOVIAL character literal: '....' ('' inside means a single ') *)
  | '\''                { STRINGLIT (read_squoted (Buffer.create 64) lexbuf) }

  (* identifiers / keywords *)
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
  | eof                 { error lexbuf "unterminated /*...*/ comment" }
  | _                   { c_comment depth lexbuf }

and read_squoted buf = parse
  | "''"                { Buffer.add_char buf '\''; read_squoted buf lexbuf }
  | '\''                { Buffer.contents buf }
  | nl                  { error lexbuf "newline not allowed in '...'" }
  | eof                 { error lexbuf "unterminated '...'" }
  | _ as c              { Buffer.add_char buf c; read_squoted buf lexbuf }

and read_dquoted buf = parse
  | "\"\""              { Buffer.add_char buf '"'; read_dquoted buf lexbuf }
  | '"'                 { Buffer.contents buf }
  | nl                  { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_dquoted buf lexbuf }
  | eof                 { error lexbuf "unterminated \"...\" define-string" }
  | _ as c              { Buffer.add_char buf c; read_dquoted buf lexbuf }
