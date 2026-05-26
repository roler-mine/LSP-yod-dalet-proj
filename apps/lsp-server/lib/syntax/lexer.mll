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

  type quoted_result =
    | Quoted_ok of string
    | Quoted_bad of string

  let parse_bit_literal (raw : string) : int * string * string =
    let len = String.length raw in
    let rec find_b i =
      if i >= len then None
      else match raw.[i] with
      | 'B' | 'b' -> Some i
      | _ -> find_b (i + 1)
    in
    match find_b 0 with
    | None -> (1, raw, raw)
    | Some bpos ->
        let bead_size =
          try int_of_string (String.sub raw 0 bpos) with Failure _ -> 1
        in
        let first_quote = bpos + 1 in
        let beads =
          if first_quote < len && raw.[first_quote] = '\'' && len >= first_quote + 2 then
            String.sub raw (first_quote + 1) (len - first_quote - 2)
          else raw
        in
        (bead_size, String.uppercase_ascii beads, raw)

  let mk_id session s =
    if Keyword.equal_upper_ascii s "NULL" then NULL
    else if Keyword.equal_upper_ascii s "A" then FIXED_A s
    else match Keyword.classify s with
    | Some DEFINE -> define_enter session; compool_reset session; DEFINE
    | Some COMPOOL -> compool_enter session; COMPOOL
    | Some ICOMPOOL -> compool_enter session; ICOMPOOL
    | Some t -> t
    | None -> ID s
}

let digit = ['0'-'9']
let alpha = ['A'-'Z''a'-'z']

(* JOVIAL identifiers can contain apostrophes, and '$' is common. *)
(* Manual J73 names start with a letter or '$' and then letters/digits/'$'/prime.
   '_' is kept as a recovery/vendor-extension character so existing workspaces keep parsing.
   The AST exposes Ast.is_manual_name for diagnostics. *)
let name_start = alpha | '$' | '_'
let name_char  = alpha | digit | '$' | '_' | '\''

let ws = [' ' '\t' '\r' '\012']
let nl = '\n'

let exp = ['e''E'] ['+''-']? digit+
let float1 = digit+ '.' digit* exp?
let float2 = '.' digit+ exp?
let float3 = digit+ exp
let bit_lit = ['1'-'5'] ['B''b'] '\'' ['0'-'9''A'-'V''a'-'v']+ '\''
let based_int = digit+ ['A'-'Z''a'-'z'] '\'' [^ '\'' '\n']+ '\''

rule token session = parse
  | "\239\187\191"      { token session lexbuf }
  | ws+                 { token session lexbuf }
  | nl                  { Lexing.new_line lexbuf; compool_reset session; token session lexbuf }
  | '\026'              { EOF }

  (* %...% comments (JOVIAL style) *)
  | '%'                 {
                          if pct_comment lexbuf then token session lexbuf
                          else BAD_COMMENT "unterminated percent comment"
                        }

  (* JOVIAL conversion brackets: (* ... *) are NOT comments. *)
  | "(*"                { CONV_L }
  | "*)"                { CONV_R }

  (* Double-quote: DEFINE-string / COMPOOL-string (first one only) OR a comment. *)
  | '"'                 {
                          if session.define_active && not session.define_got_string then (
                            session.define_got_string <- true;
                            match read_dquoted (Buffer.create 64) lexbuf with
                            | Quoted_ok s -> STRINGLIT s
                            | Quoted_bad reason -> BAD_STRING reason
                          ) else if session.compool_active && not session.compool_got_string then (
                            session.compool_got_string <- true;
                            match read_dquoted (Buffer.create 64) lexbuf with
                            | Quoted_ok s -> STRINGLIT s
                            | Quoted_bad reason -> BAD_STRING reason
                          ) else (
                            if dq_comment lexbuf then token session lexbuf
                            else BAD_COMMENT "unterminated double-quote comment"
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
  | bit_lit as s        { let b, beads, raw = parse_bit_literal s in BITLIT (b, beads, raw) }
  | digit+ ['B''b'] '\'' [^ '\'' '\n']* as s
                        { BAD_LITERAL ("malformed bit literal: " ^ s) }
  | based_int as s      { INTLIT s }
  | float1 as s         { FLOATLIT s }
  | float2 as s         { FLOATLIT s }
  | float3 as s         { FLOATLIT s }
  | digit+ as s         { INTLIT s }

  (* JOVIAL character literal: '....' ('' inside means a single ') *)
  | '\''                {
                          match read_squoted (Buffer.create 64) lexbuf with
                          | Quoted_ok s -> STRINGLIT s
                          | Quoted_bad reason -> BAD_STRING reason
                        }

  (* identifiers / keywords *)
  | name_start name_char* as s { mk_id session s }

  | eof                 { EOF }
  | _ as c              { BAD_CHAR (String.make 1 c) }

and dq_comment = parse
  | '"'                 { true }
  | nl                  { Lexing.new_line lexbuf; dq_comment lexbuf }
  | [^ '"' '\n']+       { dq_comment lexbuf }
  | eof                 { false }
  | _                   { dq_comment lexbuf }

and pct_comment = parse
  | '%'                 { true }
  | nl                  { Lexing.new_line lexbuf; pct_comment lexbuf }
  | [^ '%' '\n']+       { pct_comment lexbuf }
  | eof                 { false }
  | _                   { pct_comment lexbuf }

and read_squoted buf = parse
  | "''"                { Buffer.add_char buf '\''; read_squoted buf lexbuf }
  | '\''                { Quoted_ok (Buffer.contents buf) }
  | nl                  { Lexing.new_line lexbuf; Quoted_bad "unterminated character literal" }
  | eof                 { Quoted_bad "unterminated character literal" }
  | _ as c              { Buffer.add_char buf c; read_squoted buf lexbuf }

and read_dquoted buf = parse
  | "\"\""              { Buffer.add_char buf '"'; read_dquoted buf lexbuf }
  | '"'                 { Quoted_ok (Buffer.contents buf) }
  | nl                  { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_dquoted buf lexbuf }
  | eof                 { Quoted_bad "unterminated string literal" }
  | _ as c              { Buffer.add_char buf c; read_dquoted buf lexbuf }
