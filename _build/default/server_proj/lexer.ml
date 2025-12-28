# 1 "server_proj/lexer.mll"
 
 

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


# 180 "server_proj/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\225\255\226\255\087\000\164\000\230\255\186\000\232\255\
    \234\255\235\255\236\255\002\000\030\000\174\000\243\255\244\255\
    \245\255\246\255\248\255\052\000\053\000\252\255\253\255\254\255\
    \003\000\115\000\251\255\249\255\250\255\219\000\246\000\000\001\
    \017\001\239\255\241\255\240\255\032\001\042\001\057\000\081\000\
    \209\000\231\255\064\001\074\001\084\001\096\001\106\001\240\000\
    \252\255\253\255\001\000\254\255\255\255\251\000\252\255\253\255\
    \004\000\254\255\255\255\024\001\251\255\252\255\005\000\253\255\
    \004\000\255\255\241\000\251\255\252\255\007\000\253\255\088\000\
    \255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\028\000\027\000\255\255\027\000\255\255\
    \255\255\255\255\255\255\018\000\017\000\013\000\255\255\255\255\
    \255\255\255\255\255\255\022\000\008\000\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\026\000\255\255\026\000\
    \255\255\255\255\255\255\255\255\255\255\026\000\255\255\255\255\
    \255\255\255\255\255\255\026\000\255\255\026\000\255\255\255\255\
    \255\255\255\255\003\000\255\255\255\255\255\255\255\255\255\255\
    \003\000\255\255\255\255\255\255\255\255\255\255\004\000\255\255\
    \001\000\255\255\255\255\255\255\255\255\004\000\255\255\001\000\
    \255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\000\000\000\000\000\000\
    \255\255\255\255\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\048\000\
    \000\000\000\000\255\255\000\000\000\000\054\000\000\000\000\000\
    \255\255\000\000\000\000\060\000\000\000\000\000\255\255\000\000\
    \255\255\000\000\067\000\000\000\000\000\255\255\000\000\255\255\
    \000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\024\000\023\000\051\000\024\000\025\000\057\000\063\000\
    \024\000\070\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \024\000\000\000\021\000\024\000\003\000\022\000\065\000\005\000\
    \020\000\017\000\019\000\009\000\016\000\008\000\013\000\007\000\
    \004\000\006\000\006\000\006\000\006\000\006\000\004\000\004\000\
    \004\000\004\000\014\000\015\000\012\000\010\000\011\000\035\000\
    \018\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\034\000\033\000\028\000\027\000\026\000\
    \039\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\024\000\023\000\003\000\072\000\
    \024\000\040\000\040\000\040\000\040\000\000\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\024\000\000\000\000\000\000\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\037\000\000\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \037\000\036\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\000\000\000\000\000\000\000\000\
    \041\000\000\000\051\000\070\000\038\000\050\000\069\000\036\000\
    \002\000\040\000\040\000\040\000\040\000\057\000\000\000\000\000\
    \056\000\036\000\000\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\052\000\000\000\000\000\
    \071\000\000\000\000\000\000\000\038\000\058\000\000\000\036\000\
    \030\000\032\000\063\000\032\000\000\000\062\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\064\000\000\000\000\000\000\000\000\000\000\000\
    \030\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\046\000\000\000\046\000\000\000\000\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\044\000\000\000\044\000\000\000\042\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\000\000\042\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \049\000\068\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \061\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\050\000\024\000\000\000\056\000\062\000\
    \024\000\069\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\024\000\000\000\000\000\064\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\012\000\012\000\019\000\019\000\020\000\
    \038\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\025\000\025\000\003\000\071\000\
    \025\000\039\000\039\000\039\000\039\000\255\255\255\255\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\255\255\255\255\025\000\255\255\255\255\255\255\255\255\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\004\000\255\255\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \006\000\004\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\255\255\255\255\255\255\255\255\
    \040\000\255\255\047\000\066\000\006\000\047\000\066\000\006\000\
    \000\000\040\000\040\000\040\000\040\000\053\000\255\255\255\255\
    \053\000\004\000\255\255\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\047\000\255\255\255\255\
    \066\000\255\255\255\255\255\255\006\000\053\000\255\255\006\000\
    \029\000\030\000\059\000\030\000\255\255\059\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\059\000\255\255\255\255\255\255\255\255\255\255\
    \029\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\036\000\255\255\036\000\255\255\255\255\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\042\000\255\255\042\000\255\255\037\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\255\255\037\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \047\000\066\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\053\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \059\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec raw_token lexbuf =
   __ocaml_lex_raw_token_rec lexbuf 0
and __ocaml_lex_raw_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 201 "server_proj/lexer.mll"
                 ( raw_token lexbuf )
# 394 "server_proj/lexer.ml"

  | 1 ->
# 202 "server_proj/lexer.mll"
                 ( Lexing.new_line lexbuf; raw_token lexbuf )
# 399 "server_proj/lexer.ml"

  | 2 ->
# 204 "server_proj/lexer.mll"
                 (
                   let startp = Lexing.lexeme_start_p lexbuf in
                   percent_comment startp (Buffer.create 64) lexbuf
                 )
# 407 "server_proj/lexer.ml"

  | 3 ->
# 210 "server_proj/lexer.mll"
                 (
                   let startp = Lexing.lexeme_start_p lexbuf in
                   match !dq_state with
                   | AfterDefineDQ ->
                       dq_state := NormalDQ;
                       define_string startp (Buffer.create 128) lexbuf
                   | NormalDQ ->
                       dq_comment startp (Buffer.create 64) lexbuf
                 )
# 420 "server_proj/lexer.ml"

  | 4 ->
# 221 "server_proj/lexer.mll"
                 ( emit lexbuf LCONV )
# 425 "server_proj/lexer.ml"

  | 5 ->
# 222 "server_proj/lexer.mll"
                 ( emit lexbuf RCONV )
# 430 "server_proj/lexer.ml"

  | 6 ->
# 225 "server_proj/lexer.mll"
                 ( emit lexbuf POW )
# 435 "server_proj/lexer.ml"

  | 7 ->
# 228 "server_proj/lexer.mll"
                 ( emit lexbuf AT )
# 440 "server_proj/lexer.ml"

  | 8 ->
# 231 "server_proj/lexer.mll"
                 ( emit lexbuf LPAREN )
# 445 "server_proj/lexer.ml"

  | 9 ->
# 232 "server_proj/lexer.mll"
                 ( emit lexbuf RPAREN )
# 450 "server_proj/lexer.ml"

  | 10 ->
# 233 "server_proj/lexer.mll"
                 ( emit lexbuf COMMA )
# 455 "server_proj/lexer.ml"

  | 11 ->
# 234 "server_proj/lexer.mll"
                 ( on_stmt_terminator (); emit lexbuf SEMI )
# 460 "server_proj/lexer.ml"

  | 12 ->
# 235 "server_proj/lexer.mll"
                 ( emit lexbuf COLON )
# 465 "server_proj/lexer.ml"

  | 13 ->
# 236 "server_proj/lexer.mll"
                 ( emit lexbuf DOT )
# 470 "server_proj/lexer.ml"

  | 14 ->
# 239 "server_proj/lexer.mll"
                 ( emit lexbuf LE )
# 475 "server_proj/lexer.ml"

  | 15 ->
# 240 "server_proj/lexer.mll"
                 ( emit lexbuf GE )
# 480 "server_proj/lexer.ml"

  | 16 ->
# 241 "server_proj/lexer.mll"
                 ( emit lexbuf NE )
# 485 "server_proj/lexer.ml"

  | 17 ->
# 242 "server_proj/lexer.mll"
                 ( emit lexbuf LT )
# 490 "server_proj/lexer.ml"

  | 18 ->
# 243 "server_proj/lexer.mll"
                 ( emit lexbuf GT )
# 495 "server_proj/lexer.ml"

  | 19 ->
# 244 "server_proj/lexer.mll"
                 ( emit lexbuf EQ )
# 500 "server_proj/lexer.ml"

  | 20 ->
# 247 "server_proj/lexer.mll"
                 ( emit lexbuf PLUS )
# 505 "server_proj/lexer.ml"

  | 21 ->
# 248 "server_proj/lexer.mll"
                 ( emit lexbuf MINUS )
# 510 "server_proj/lexer.ml"

  | 22 ->
# 249 "server_proj/lexer.mll"
                 ( emit lexbuf STAR )
# 515 "server_proj/lexer.ml"

  | 23 ->
# 250 "server_proj/lexer.mll"
                 ( emit lexbuf SLASH )
# 520 "server_proj/lexer.ml"

  | 24 ->
let
# 253 "server_proj/lexer.mll"
                 sz
# 526 "server_proj/lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos
and
# 253 "server_proj/lexer.mll"
                                                   bits
# 531 "server_proj/lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 3) (lexbuf.Lexing.lex_curr_pos + -1) in
# 253 "server_proj/lexer.mll"
                                                              (
      let startp = Lexing.lexeme_start_p lexbuf in
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      let size = (Char.code sz) - (Char.code '0') in
      BITLIT (size, bits)
    )
# 541 "server_proj/lexer.ml"

  | 25 ->
# 265 "server_proj/lexer.mll"
                 (
      let startp = Lexing.lexeme_start_p lexbuf in
      char_literal startp (Buffer.create 32) lexbuf
    )
# 549 "server_proj/lexer.ml"

  | 26 ->
# 271 "server_proj/lexer.mll"
                 ( emit lexbuf (REAL (Lexing.lexeme lexbuf)) )
# 554 "server_proj/lexer.ml"

  | 27 ->
# 272 "server_proj/lexer.mll"
                 (
      let tok = int_of_lexeme lexbuf in
      (* int_of_lexeme may return ERROR; ensure span already set. *)
      (match tok with
       | ERROR _ -> tok
       | _ -> emit lexbuf tok)
    )
# 565 "server_proj/lexer.ml"

  | 28 ->
# 281 "server_proj/lexer.mll"
                 ( emit lexbuf (classify_ident (Lexing.lexeme lexbuf)) )
# 570 "server_proj/lexer.ml"

  | 29 ->
# 283 "server_proj/lexer.mll"
                 ( emit lexbuf EOF )
# 575 "server_proj/lexer.ml"

  | 30 ->
# 285 "server_proj/lexer.mll"
                 (
      let startp = Lexing.lexeme_start_p lexbuf in
      let endp = Lexing.lexeme_end_p lexbuf in
      let s = Lexing.lexeme lexbuf in
      emit_error startp endp ("unexpected character: " ^ s)
    )
# 585 "server_proj/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_raw_token_rec lexbuf __ocaml_lex_state

and percent_comment startp buf lexbuf =
   __ocaml_lex_percent_comment_rec startp buf lexbuf 47
and __ocaml_lex_percent_comment_rec startp buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 296 "server_proj/lexer.mll"
        (
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      COMMENT (Buffer.contents buf)
    )
# 601 "server_proj/lexer.ml"

  | 1 ->
# 301 "server_proj/lexer.mll"
        (
      Lexing.new_line lexbuf;
      Buffer.add_char buf '\n';
      percent_comment startp buf lexbuf
    )
# 610 "server_proj/lexer.ml"

  | 2 ->
# 306 "server_proj/lexer.mll"
        (
      let endp = Lexing.lexeme_end_p lexbuf in
      enqueue [EOF];
      emit_error startp endp "unterminated %...% comment"
    )
# 619 "server_proj/lexer.ml"

  | 3 ->
let
# 311 "server_proj/lexer.mll"
         c
# 625 "server_proj/lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 311 "server_proj/lexer.mll"
           (
      Buffer.add_char buf c;
      percent_comment startp buf lexbuf
    )
# 632 "server_proj/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_percent_comment_rec startp buf lexbuf __ocaml_lex_state

and dq_comment startp buf lexbuf =
   __ocaml_lex_dq_comment_rec startp buf lexbuf 53
and __ocaml_lex_dq_comment_rec startp buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 318 "server_proj/lexer.mll"
        (
      (* Quote-delimited comment: " ... " *)
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      COMMENT (Buffer.contents buf)
    )
# 649 "server_proj/lexer.ml"

  | 1 ->
# 324 "server_proj/lexer.mll"
        (
      (* Many JOVIAL codebases use a single '"' to start a line comment.
         Treat newline as a valid terminator too. We do NOT include the newline
         in the comment contents; we consume it and advance line tracking. *)
      let endp = Lexing.lexeme_start_p lexbuf in
      set_span startp endp;
      Lexing.new_line lexbuf;
      COMMENT (Buffer.contents buf)
    )
# 662 "server_proj/lexer.ml"

  | 2 ->
# 333 "server_proj/lexer.mll"
        (
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      COMMENT (Buffer.contents buf)
    )
# 671 "server_proj/lexer.ml"

  | 3 ->
let
# 338 "server_proj/lexer.mll"
         c
# 677 "server_proj/lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 338 "server_proj/lexer.mll"
           (
      Buffer.add_char buf c;
      dq_comment startp buf lexbuf
    )
# 684 "server_proj/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_dq_comment_rec startp buf lexbuf __ocaml_lex_state

and define_string startp buf lexbuf =
   __ocaml_lex_define_string_rec startp buf lexbuf 59
and __ocaml_lex_define_string_rec startp buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 350 "server_proj/lexer.mll"
           (
      Buffer.add_char buf '"';
      define_string startp buf lexbuf
    )
# 699 "server_proj/lexer.ml"

  | 1 ->
# 354 "server_proj/lexer.mll"
        (
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      DEFINE_STRING (Buffer.contents buf)
    )
# 708 "server_proj/lexer.ml"

  | 2 ->
# 359 "server_proj/lexer.mll"
       (
      Lexing.new_line lexbuf;
      Buffer.add_char buf '\n';
      define_string startp buf lexbuf
    )
# 717 "server_proj/lexer.ml"

  | 3 ->
# 364 "server_proj/lexer.mll"
        (
      let endp = Lexing.lexeme_end_p lexbuf in
      enqueue [EOF];
      emit_error startp endp "unterminated DEFINE string"
    )
# 726 "server_proj/lexer.ml"

  | 4 ->
let
# 369 "server_proj/lexer.mll"
         c
# 732 "server_proj/lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 369 "server_proj/lexer.mll"
           (
      Buffer.add_char buf c;
      define_string startp buf lexbuf
    )
# 739 "server_proj/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_define_string_rec startp buf lexbuf __ocaml_lex_state

and char_literal startp buf lexbuf =
   __ocaml_lex_char_literal_rec startp buf lexbuf 66
and __ocaml_lex_char_literal_rec startp buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 380 "server_proj/lexer.mll"
         (
      Buffer.add_char buf '\'';
      char_literal startp buf lexbuf
    )
# 754 "server_proj/lexer.ml"

  | 1 ->
# 384 "server_proj/lexer.mll"
         (
      let endp = Lexing.lexeme_end_p lexbuf in
      set_span startp endp;
      CHARLIT (Buffer.contents buf)
    )
# 763 "server_proj/lexer.ml"

  | 2 ->
# 389 "server_proj/lexer.mll"
       (
      (* newline not allowed in character literals *)
      Lexing.new_line lexbuf;
      let endp = Lexing.lexeme_end_p lexbuf in
      enqueue [EOF];
      emit_error startp endp "newline in character literal"
    )
# 774 "server_proj/lexer.ml"

  | 3 ->
# 396 "server_proj/lexer.mll"
        (
      let endp = Lexing.lexeme_end_p lexbuf in
      enqueue [EOF];
      emit_error startp endp "unterminated character literal"
    )
# 783 "server_proj/lexer.ml"

  | 4 ->
let
# 401 "server_proj/lexer.mll"
         c
# 789 "server_proj/lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 401 "server_proj/lexer.mll"
           (
      Buffer.add_char buf c;
      char_literal startp buf lexbuf
    )
# 796 "server_proj/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_char_literal_rec startp buf lexbuf __ocaml_lex_state

;;

# 406 "server_proj/lexer.mll"
 
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


# 844 "server_proj/lexer.ml"
