{
 

  open Parser

  (* === LSP support: spans === *)

  type span = Lexing.position * Lexing.position

  let last_span_ref : span ref =
    ref (Lexing.dummy_pos, Lexing.dummy_pos)

  let last_span () : span = !last_span_ref

  let set_span (s:Lexing.position) (e:Lexing.position) : unit =
    last_span_ref := (s, e)

  (* Pending tokens: lets us emit ERROR then EOF, etc. *)
  let pending : token list ref = ref []

  let enqueue (toks: token list) : unit =
    pending := toks @ !pending

  let dequeue () : token option =
    match !pending with
    | [] -> None
    | t :: ts -> pending := ts; Some t


  type dq_mode =
    | NormalDQ
    | AfterDefineDQ

  let dq_state : dq_mode ref = ref NormalDQ

  let uppercase = String.uppercase_ascii

  exception Internal_lexer_bug of string

  (* Keyword table (case-insensitive). *)
  let kw : (string, token) Hashtbl.t = Hashtbl.create 401

  let add k v = Hashtbl.replace kw k v

  let () =
    (* Module/unit *)
    add "START" START;
    add "TERM" TERM;
    add "PROGRAM" PROGRAM;
    add "COMPOOL" COMPOOL;


    add "PROC" PROC;
    add "PROCEDURE" PROC;
    add "FUNCTION" FUNCTION;

    add "BY" BY;
    add "POS" POS;
    add "SGNL" SGNL;


    add "ICOMPOOL" ICOMPOOL;
    add "ICOPY" ICOPY;
    add "ISKIP" ISKIP;
    add "IBEGIN" IBEGIN;
    add "IEND" IEND;
    add "ILINKAGE" ILINKAGE;
    add "ITRACE" ITRACE;
    add "IINTERFERENCE" IINTERFERENCE;
    add "IREDUCIBLE" IREDUCIBLE;
    add "INOLIST" INOLIST;
    add "ILIST" ILIST;
    add "IEJECT" IEJECT;
    add "IINITIALIZE" IINITIALIZE;
    add "IORDER" IORDER;
    add "ILEFTRIGHT" ILEFTRIGHT;
    add "IREARRANGE" IREARRANGE;
    add "IBASE" IBASE;
    add "IISBASE" IISBASE;
    add "IDROP" IDROP;

    add "DEFINE" DEFINE;

    add "DEF" DEF;
    add "REF" REF;
    add "RENT" RENT;
    add "REC" REC;
    add "REP" REP;
    add "INLINE" INLINE;
    add "LABEL" LABEL;
    add "INSTANCE" INSTANCE;
    add "OVERLAY" OVERLAY;
    add "PARALLEL" PARALLEL;
    add "STATIC" STATIC;

    add "TYPE" TYPE;
    add "ITEM" ITEM;
    add "TABLE" TABLE;
    add "BLOCK" BLOCK;
    add "CONSTANT" CONSTANT;
    add "AS" AS;
    add "LIKE" LIKE;

    add "BIT" BIT;
    add "BITSIZE" BITSIZE;
    add "BYTESIZE" BYTESIZE;
    add "WORDSIZE" WORDSIZE;
    add "STATUS" STATUS;

    add "IF" IF;
    add "THEN" THEN;
    add "ELSE" ELSE;
    add "CASE" CASE;
    add "DEFAULT" DEFAULT;
    add "FALLTHRU" FALLTHRU;
    add "FOR" FOR;
    add "WHILE" WHILE;
    add "EXIT" EXIT;
    add "GOTO" GOTO;
    add "RETURN" RETURN;
    add "STOP" STOP;
    add "ABORT" ABORT;
    add "BEGIN" BEGIN;
    add "END" END;

    add "TRUE" TRUE;
    add "FALSE" FALSE;
    add "NULL" NULL;

    add "LOC" LOC;
    add "NEXT" NEXT;
    add "FIRST" FIRST;
    add "LAST" LAST;
    add "LBOUND" LBOUND;
    add "UBOUND" UBOUND;

    add "MOD" MOD;
    add "NOT" NOT;
    add "AND" AND;
    add "OR" OR;
    add "XOR" XOR;
    add "EQV" EQV;
    add "SHIFTL" SHIFTL;
    add "SHIFTR" SHIFTR;
    ()

  let classify_ident (s:string) : token =
    let u = uppercase s in
    match Hashtbl.find_opt kw u with
    | Some tok ->
        (match tok with
         | DEFINE -> dq_state := AfterDefineDQ; tok
         | _ -> tok);
    | None ->
        if String.length u = 1 then LETTER u.[0] else NAME u

  let emit (lexbuf:Lexing.lexbuf) (t:token) : token =
    set_span (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf);
    t

  let on_stmt_terminator () =
    dq_state := NormalDQ

  let emit_error (startp:Lexing.position) (endp:Lexing.position) (msg:string) : token =
    set_span startp endp;
    ERROR msg

  let int_of_lexeme lexbuf =
    let s = Lexing.lexeme lexbuf in
    try INT (int_of_string s)
    with _ ->
      (* Keep going: return ERROR + EOF to stop parser cleanly. *)
      let sp = Lexing.lexeme_start_p lexbuf and ep = Lexing.lexeme_end_p lexbuf in
      enqueue [EOF];
      emit_error sp ep ("bad integer literal: " ^ s)

}

let wsp   = [' ' '\t' '\r']
let nl    = '\n' | "\r\n"

let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let ident_start = alpha | '$'
let ident_char  = alpha | digit | '$' | '\''

let ident = ident_start ident_char*

let int_lit = digit+
let exp = ['E' 'e'] ['+' '-']? digit+
let real_lit =
  (digit+ '.' digit* exp?)
| ('.' digit+ exp?)
| (digit+ exp)

let bit_size = ['1'-'5']
let bit_digits = ['0'-'3']+

rule raw_token =
  parse
  | wsp+         { raw_token lexbuf }
  | nl           { Lexing.new_line lexbuf; raw_token lexbuf }

  | '%'          {
                   let startp = Lexing.lexeme_start_p lexbuf in
                   percent_comment startp (Buffer.create 64) lexbuf
                 }

  (* double-quote: either COMMENT or DEFINE_STRING, depending on dq_state *)
  | '"'          {
                   let startp = Lexing.lexeme_start_p lexbuf in
                   match !dq_state with
                   | AfterDefineDQ ->
                       dq_state := NormalDQ;
                       define_string startp (Buffer.create 128) lexbuf
                   | NormalDQ ->
                       dq_comment startp (Buffer.create 64) lexbuf
                 }

  (* conversion brackets (not comments) *)
  | "(*"         { emit lexbuf LCONV }
  | "*)"         { emit lexbuf RCONV }

  (* exponentiation operator ** *)
  | "**"         { emit lexbuf POW }

  (* dereference *)
  | '@'          { emit lexbuf AT }

  (* punctuation *)
  | '('          { emit lexbuf LPAREN }
  | ')'          { emit lexbuf RPAREN }
  | ','          { emit lexbuf COMMA }
  | ';'          { on_stmt_terminator (); emit lexbuf SEMI }
  | ':'          { emit lexbuf COLON }
  | '.'          { emit lexbuf DOT }

  (* relational operators (order matters) *)
  | "<="         { emit lexbuf LE }
  | ">="         { emit lexbuf GE }
  | "<>"         { emit lexbuf NE }
  | '<'          { emit lexbuf LT }
  | '>'          { emit lexbuf GT }
  | '='          { emit lexbuf EQ }

  (* arithmetic *)
  | '+'          { emit lexbuf PLUS }
  | '-'          { emit lexbuf MINUS }
  | '*'          { emit lexbuf STAR }
  | '/'          { emit lexbuf SLASH }

  (* bit literal: e.g. 3B'0123' *)
  | (bit_size as sz) ['B' 'b'] '\'' (bit_digits as bits) '\'' {
      let startp = Lexing.lexeme_start_p lexbuf in
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      let size = (Char.code sz) - (Char.code '0') in
      BITLIT (size, bits)
    }

  (* character literal: '...'
     - doubled '' means a single quote character
     - newlines are NOT allowed (spec)
  *)
  | '\''         {
      let startp = Lexing.lexeme_start_p lexbuf in
      char_literal startp (Buffer.create 32) lexbuf
    }

  (* numeric literals: real before int *)
  | real_lit     { emit lexbuf (REAL (Lexing.lexeme lexbuf)) }
  | int_lit      {
      let tok = int_of_lexeme lexbuf in
      (* int_of_lexeme may return ERROR; ensure span already set. *)
      (match tok with
       | ERROR _ -> tok
       | _ -> emit lexbuf tok)
    }

  (* identifiers / keywords *)
  | ident        { emit lexbuf (classify_ident (Lexing.lexeme lexbuf)) }

  | eof          { emit lexbuf EOF }

  | _            {
      let startp = Lexing.lexeme_start_p lexbuf in
      let endp = Lexing.lexeme_end_p lexbuf in
      let s = Lexing.lexeme lexbuf in
      emit_error startp endp ("unexpected character: " ^ s)
    }

(* === COMMENTS / STRINGS === *)

and percent_comment startp buf =
  parse
  | '%' {
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      COMMENT (Buffer.contents buf)
    }
  | nl  {
      Lexing.new_line lexbuf;
      Buffer.add_char buf '\n';
      percent_comment startp buf lexbuf
    }
  | eof {
      let endp = Lexing.lexeme_end_p lexbuf in
      enqueue [EOF];
      emit_error startp endp "unterminated %...% comment"
    }
  | _ as c {
      Buffer.add_char buf c;
      percent_comment startp buf lexbuf
    }

and dq_comment startp buf =
  parse
  | '"' {
      (* Quote-delimited comment: " ... " *)
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      COMMENT (Buffer.contents buf)
    }
  | nl  {
      (* Many JOVIAL codebases use a single '"' to start a line comment.
         Treat newline as a valid terminator too. We do NOT include the newline
         in the comment contents; we consume it and advance line tracking. *)
      let endp = Lexing.lexeme_start_p lexbuf in
      set_span startp endp;
      Lexing.new_line lexbuf;
      COMMENT (Buffer.contents buf)
    }
  | eof {
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      COMMENT (Buffer.contents buf)
    }
  | _ as c {
      Buffer.add_char buf c;
      dq_comment startp buf lexbuf
    }

(* DEFINE string:
   - ends at an un-doubled quote
   - doubled quotes "" represent a single quote character inside the string
   - can contain newlines
*)
and define_string startp buf =
  parse
  | "\"\"" {
      Buffer.add_char buf '"';
      define_string startp buf lexbuf
    }
  | '"' {
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      DEFINE_STRING (Buffer.contents buf)
    }
  | nl {
      Lexing.new_line lexbuf;
      Buffer.add_char buf '\n';
      define_string startp buf lexbuf
    }
  | eof {
      let endp = Lexing.lexeme_end_p lexbuf in
      enqueue [EOF];
      emit_error startp endp "unterminated DEFINE string"
    }
  | _ as c {
      Buffer.add_char buf c;
      define_string startp buf lexbuf
    }

(* Character literal:
   - '' inside means a single quote char
   - cannot contain newlines
*)
and char_literal startp buf =
  parse
  | "''" {
      Buffer.add_char buf '\'';
      char_literal startp buf lexbuf
    }
  | '\'' {
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      CHARLIT (Buffer.contents buf)
    }
  | nl {
      (* newline not allowed in character literals *)
      Lexing.new_line lexbuf;
      let endp = Lexing.lexeme_end_p lexbuf in
      enqueue [EOF];
      emit_error startp endp "newline in character literal"
    }
  | eof {
      let endp = Lexing.lexeme_end_p lexbuf in
      enqueue [EOF];
      emit_error startp endp "unterminated character literal"
    }
  | _ as c {
      Buffer.add_char buf c;
      char_literal startp buf lexbuf
    }

{
  (* === Public entrypoints ===

     - token_with_trivia: returns COMMENT tokens (useful for LSP tokenization).
     - token: skips COMMENT tokens (useful for Menhir parser input).

     Both maintain last_span().
  *)

  let token_with_trivia (lexbuf:Lexing.lexbuf) : token =
    match dequeue () with
    | Some t -> t
    | None -> raw_token lexbuf

  let rec token (lexbuf:Lexing.lexbuf) : token =
    match token_with_trivia lexbuf with
    | COMMENT _ -> token lexbuf
    | t -> t

  (* Tokenize an entire buffer (useful for LSP semantic tokenization). *)
  let tokenize_lexbuf ?(with_trivia=false) (lexbuf:Lexing.lexbuf)
    : (token * span) list =
    let next = if with_trivia then token_with_trivia else token in
    let rec loop acc =
      let t = next lexbuf in
      let sp = last_span () in
      let acc = (t, sp) :: acc in
      match t with
      | EOF -> List.rev acc
      | _ -> loop acc
    in
    loop []

  let tokenize_string ?(with_trivia=false) (s:string) : (token * span) list =
    let lexbuf = Lexing.from_string s in
    (* Initialize position bookkeeping. *)
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_lnum = 1; Lexing.pos_bol = 0 };
    tokenize_lexbuf ~with_trivia lexbuf

}
