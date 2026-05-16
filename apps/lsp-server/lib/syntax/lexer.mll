{
  (* Module overview: OCamllex scanner for Jovial tokens, comments, and DEFINE/COMPOOL quirks. *)
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
  type session = {
    mutable define_active : bool;
    mutable define_got_string : bool;
    mutable compool_active : bool;
    mutable compool_got_string : bool;
  }

  let create_session () =
    {
      define_active = false;
      define_got_string = false;
      compool_active = false;
      compool_got_string = false;
    }

  let define_enter session =
    session.define_active <- true;
    session.define_got_string <- false

  let define_reset session =
    session.define_active <- false;
    session.define_got_string <- false

  let compool_enter session =
    session.compool_active <- true;
    session.compool_got_string <- false

  let compool_reset session =
    session.compool_active <- false;
    session.compool_got_string <- false

  let with_session_state (f: session -> 'a) : 'a =
    f (create_session ())

  let mk_id session s =
    match Keyword.classify s with
    | Some DEFINE -> define_enter session; compool_reset session; DEFINE
    | Some COMPOOL -> compool_enter session; COMPOOL
    | Some ICOMPOOL -> compool_enter session; ICOMPOOL
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

rule token session = parse
  | ws+                 { token session lexbuf }
  | nl                  { Lexing.new_line lexbuf; compool_reset session; token session lexbuf }

  (* %...% comments (JOVIAL style) *)
  | '%'                 { pct_comment lexbuf; token session lexbuf }

  (* JOVIAL conversion brackets: (* ... *) are NOT comments. *)
  | "(*"                { CONV_L }
  | "*)"                { CONV_R }

  (* Double-quote: DEFINE-string / COMPOOL-string (first one only) OR a comment. *)
  | '"'                 {
                          if session.define_active && not session.define_got_string then (
                            session.define_got_string <- true;
                            STRINGLIT (read_dquoted (Buffer.create 64) lexbuf)
                          ) else if session.compool_active && not session.compool_got_string then (
                            session.compool_got_string <- true;
                            STRINGLIT (read_dquoted (Buffer.create 64) lexbuf)
                          ) else (
                            dq_comment lexbuf;
                            token session lexbuf
                          )
                        }

  (* punctuation / terminators *)
  | '('                 { LPAREN }
  | ')'                 { compool_reset session; RPAREN }
  | ','                 { COMMA }
  | ';'                 { define_reset session; compool_reset session; SEMI }
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
  | name_start name_char* as s { mk_id session s }

  | eof                 { EOF }
  | _ as c              { error lexbuf (Printf.sprintf "unexpected character: %C" c) }

and dq_comment = parse
  | '"'                 { () }
  | nl                  { Lexing.new_line lexbuf; dq_comment lexbuf }
  | [^ '"' '\n']+       { dq_comment lexbuf }
  | eof                 { error lexbuf "unterminated \"...\" comment" }
  | _                   { dq_comment lexbuf }

and pct_comment = parse
  | '%'                 { () }
  | nl                  { Lexing.new_line lexbuf; pct_comment lexbuf }
  | [^ '%' '\n']+       { pct_comment lexbuf }
  | eof                 { error lexbuf "unterminated %...% comment" }
  | _                   { pct_comment lexbuf }

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
