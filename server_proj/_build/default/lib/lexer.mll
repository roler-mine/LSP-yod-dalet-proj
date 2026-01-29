{
  (* lexer.mll — JOVIAL J73-ish lexer for Menhir *)

  open Parser

  exception Lexing_error of string

  (* After a '!' we treat the next identifier as a directive name. *)
  let in_directive : bool ref = ref false

  (* Keyword table (case-insensitive). *)
  let kw : (string, Parser.token) Hashtbl.t = Hashtbl.create 256
  let add k v = Hashtbl.replace kw k v

  let () =
    (* module framing / kinds *)
    add "START" START;
    add "TERM" TERM;
    add "PROGRAM" PROGRAM;
    add "COMPOOL" COMPOOL;
    add "ICOMPOOL" ICOMPOOL;

    (* linkage / subroutines *)
    add "DEF" DEF;
    add "REF" REF;
    add "PROC" PROC;
    add "FUNCTION" FUNCTION;

    (* blocks / declarations *)
    add "BEGIN" BEGIN;
    add "END" END;
    add "ITEM" ITEM;
    add "TABLE" TABLE;
    add "STATUS" STATUS;
    add "TYPE" TYPE;
    add "BLOCK" BLOCK;

    (* control flow *)
    add "IF" IF;
    add "THEN" THEN;
    add "ELSE" ELSE;
    add "ELSIF" ELSIF;
    add "FOR" FOR;
    add "BY" BY;
    add "WHILE" WHILE;
    add "TO" TO;
    add "CASE" CASE;
    add "OF" OF;
    add "WHEN" WHEN;
    add "OTHERWISE" OTHERWISE;
    add "GOTO" GOTO;
    add "RETURN" RETURN;
    add "EXIT" EXIT;
    add "STOP" STOP;
    add "ABORT" ABORT;
    add "FALLTHRU" FALLTHRU;

    (* decl attributes / misc *)
    add "DEFAULT" DEFAULT;
    add "DEFINE" DEFINE;
    add "INLINE" INLINE;
    add "INSTANCE" INSTANCE;
    add "LABEL" LABEL;
    add "LIKE" LIKE;
    add "OVERLAY" OVERLAY;
    add "POS" POS;
    add "RENT" RENT;
    add "REC" REC;
    add "REP" REP;
    add "WITH" WITH;
    add "STATIC" STATIC;
    add "CONSTANT" CONSTANT;
    add "PARALLEL" PARALLEL;

    (* explicit parameter binding (often dialect/impl-specific) *)
    add "BYREF" BYREF;
    add "BYVAL" BYVAL;
    add "BYRES" BYRES;

    (* literals *)
    add "NULL" NULL;
    add "TRUE" TRUE;
    add "FALSE" FALSE;

    (* boolean / bit ops *)
    add "AND" AND;
    add "OR" OR;
    add "NOT" NOT;
    add "XOR" XOR;
    add "EQV" EQV;
    add "MOD" MOD;

    (* word-relops *)
    add "EQ" EQ;
    add "NQ" NQ;
    add "LS" LS;
    add "LQ" LQ;
    add "GR" GR;
    add "GQ" GQ

  let is_type_atom (u : string) : bool =
    (* JOVIAL type atoms are commonly single-letter codes.
       NOTE: this will tokenize a variable named "S" as TYPEATOM "S". *)
    String.length u = 1 &&
    match u.[0] with
    | 'U' | 'S' | 'F' | 'A' | 'B' | 'C' | 'P' | 'V' -> true
    | _ -> false

  let bump_line (lexbuf : Lexing.lexbuf) = Lexing.new_line lexbuf

  (* Strings: '...' with doubled '' => ' *)
  let unescape_jovial_string (s : string) : string =
    let b = Buffer.create (String.length s) in
    let rec go i =
      if i >= String.length s then ()
      else if s.[i] = '\'' && i + 1 < String.length s && s.[i+1] = '\'' then (
        Buffer.add_char b '\'';
        go (i + 2)
      ) else (
        Buffer.add_char b s.[i];
        go (i + 1)
      )
    in
    go 0; Buffer.contents b
}

let digit      = ['0'-'9']
let alpha      = ['A'-'Z' 'a'-'z']
let id_start   = alpha | ['$']
let id_char    = alpha | digit | ['$' '_' '\'']
let ws         = [' ' '\t' '\r']
let nl         = '\n'

rule token = parse
| ws+ { token lexbuf }
| '\n' { bump_line lexbuf; token lexbuf }

  (* Comments:
     - " ... "  (double-quote delimited)
     - % ... %  (percent delimited) :contentReference[oaicite:2]{index=2} *)
  | '"'            { comment_quote lexbuf; token lexbuf }
  | '%'            { comment_percent lexbuf; token lexbuf }

  (* directives: !NAME ... ; *)
  | '!'            { in_directive := true; BANG }

  (* bead constant: 1B'V' .. 5B'V' *)
  | (['1'-'5'] as n) ('B'|'b') '\'' (['0'-'9' 'A'-'V' 'a'-'v'] as v) '\''
      {
        let bits = (Char.code n) - (Char.code '0') in
        let c =
          if v >= 'a' && v <= 'v' then Char.chr (Char.code v - 32) else v
        in
        BEAD (bits, String.make 1 c)
      }

  (* floats: 123. , 123.45 , .45 , 1e3 , 1.2e-3 *)
  | digit+ '.' digit* (['E''e'] ['+''-']? digit+)? as f
      { FLOAT (float_of_string f) }
  | '.' digit+ (['E''e'] ['+''-']? digit+)? as f
      { FLOAT (float_of_string f) }
  | digit+ ['E''e'] ['+''-']? digit+ as f
      { FLOAT (float_of_string f) }

  (* ints *)
  | digit+ as i
      { INT (int_of_string i) }

  (* strings *)
  | '\''           { let b = Buffer.create 32 in STRING (string_lit b lexbuf) }

  (* punctuation *)
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '['            { LBRACK }
  | ']'            { RBRACK }
  | ','            { COMMA }
  | ';'            { SEMI }
  | ':'            { COLON }

  (* operators / relations *)
  | "<="           { LE }
  | ">="           { GE }
  | "<>"           { NEQ }
  | '<'            { LT }
  | '>'            { GT }
  | '='            { EQUAL }

  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { STAR }
  | '/'            { SLASH }

  (* identifiers / keywords *)
  | id_start id_char* as raw
      {
        let u = String.uppercase_ascii raw in
        if !in_directive then (
          in_directive := false;
          DIRECTIVE_NAME u
        ) else if is_type_atom u then
          TYPEATOM u
        else
          match Hashtbl.find_opt kw u with
          | Some tok -> tok
          | None -> IDENT raw
      }

  | eof            { EOF }

  | _ as c         { raise (Lexing_error (Printf.sprintf "Unexpected char: %C" c)) }

and comment_quote =
  parse
  | '"'            { () }
  | nl             { bump_line lexbuf; comment_quote lexbuf }
  | eof            { () }
  | _              { comment_quote lexbuf }

and comment_percent  =
  parse
  | '%'            { () }
  | nl             { bump_line lexbuf; comment_percent lexbuf }
  | eof            { () }
  | _              { comment_percent lexbuf }

and string_lit buf =
  parse
  | '\''  { Buffer.contents buf }                  (* closing quote *)
  | "''"  { Buffer.add_char buf '\''; string_lit buf lexbuf }  (* escaped quote *)
  | nl    { bump_line lexbuf; raise (Lexing_error "Unterminated string literal") }
  | eof   { raise (Lexing_error "Unterminated string literal") }
  | _ as c { Buffer.add_char buf c; string_lit buf lexbuf }
