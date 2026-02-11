module T = Lsp.Types
module I = Parser.MenhirInterpreter

type output = {
  ast : Ast.program option;
  diags : T.Diagnostic.t list;
  ast_dump : string option;
}

let loc_of_lex (file : string option) (sp : Lexing.position) (ep : Lexing.position) : Ast.Loc.t =
  let mk (p : Lexing.position) : Ast.Loc.pos =
    let col = p.pos_cnum - p.pos_bol in
    { Ast.Loc.line = p.pos_lnum; col; offset = p.pos_cnum }
  in
  let file =
    match file with
    | Some f -> Some f
    | None -> if sp.pos_fname = "" then None else Some sp.pos_fname
  in
  { Ast.Loc.file; start_pos = mk sp; end_pos = mk ep }

let diag_error (loc : Ast.Loc.t) (msg : string) : T.Diagnostic.t =
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Error
    ~source:"parse"
    ~message:msg
    loc

module Debug = struct
  let string_of_token (t : Parser.token) : string =
    match t with
    | Parser.EOF -> "EOF"
    | Parser.ID _ -> "ID"
    | Parser.INTLIT _ -> "INTLIT"
    | Parser.FLOATLIT _ -> "FLOATLIT"
    | Parser.STRINGLIT _ -> "STRINGLIT"

    | Parser.START -> "START" | Parser.TERM -> "TERM" | Parser.BEGIN -> "BEGIN" | Parser.END -> "END"
    | Parser.DEF -> "DEF" | Parser.REF -> "REF" | Parser.PROC -> "PROC" | Parser.ITEM -> "ITEM" | Parser.TABLE -> "TABLE"
    | Parser.IF -> "IF" | Parser.ELSE -> "ELSE" | Parser.WHILE -> "WHILE" | Parser.FOR -> "FOR" | Parser.BY -> "BY"
    | Parser.CASE -> "CASE" | Parser.DEFAULT -> "DEFAULT"
    | Parser.EXIT -> "EXIT" | Parser.GOTO -> "GOTO" | Parser.RETURN -> "RETURN"
    | Parser.ABORT -> "ABORT" | Parser.STOP -> "STOP"
    | Parser.TRUE -> "TRUE" | Parser.FALSE -> "FALSE"
    | Parser.NOT -> "NOT" | Parser.AND -> "AND" | Parser.OR -> "OR" | Parser.XOR -> "XOR"
    | Parser.EQV -> "EQV" | Parser.MOD -> "MOD"

    | Parser.LPAREN -> "(" | Parser.RPAREN -> ")"
    | Parser.COMMA -> "," | Parser.SEMI -> ";" | Parser.COLON -> ":" | Parser.DOT -> "."
    | Parser.BANG -> "!" | Parser.AT -> "@"

    | Parser.EQ -> "=" | Parser.LT -> "<" | Parser.GT -> ">"
    | Parser.LE -> "<=" | Parser.GE -> ">=" | Parser.NE -> "<>"
    | Parser.PLUS -> "+" | Parser.MINUS -> "-" | Parser.STAR -> "*"
    | Parser.SLASH -> "/" | Parser.POW -> "^"

    | _ -> "<token>"
end

let take n xs =
  let rec go i acc = function
    | [] -> List.rev acc
    | _ when i = 0 -> List.rev acc
    | x :: tl -> go (i - 1) (x :: acc) tl
  in
  go n [] xs

let uniq_sorted (xs : string list) : string list =
  let rec go prev acc = function
    | [] -> List.rev acc
    | x :: tl ->
        if Some x = prev then go prev acc tl
        else go (Some x) (x :: acc) tl
  in
  go None [] xs

(* Extend this list as your token set grows. *)
let expected_candidates : Parser.token list =
  [
    Parser.ID "_";
    Parser.INTLIT "0";
    Parser.FLOATLIT "0.0";
    Parser.STRINGLIT "";

    Parser.START; Parser.TERM; Parser.BEGIN; Parser.END;
    Parser.DEF; Parser.REF; Parser.PROC; Parser.ITEM; Parser.TABLE;
    Parser.IF; Parser.ELSE; Parser.WHILE; Parser.FOR; Parser.BY;
    Parser.CASE; Parser.DEFAULT;
    Parser.RETURN; Parser.EXIT; Parser.GOTO; Parser.ABORT; Parser.STOP;
    Parser.TRUE; Parser.FALSE;
    Parser.NOT; Parser.AND; Parser.OR; Parser.XOR; Parser.EQV; Parser.MOD;

    Parser.LPAREN; Parser.RPAREN; Parser.COMMA; Parser.SEMI; Parser.COLON; Parser.DOT; Parser.BANG; Parser.AT;
    Parser.EQ; Parser.LT; Parser.GT; Parser.LE; Parser.GE; Parser.NE;
    Parser.PLUS; Parser.MINUS; Parser.STAR; Parser.SLASH; Parser.POW;

    Parser.EOF;
  ]

(* Correct Menhir signature:
   acceptable : checkpoint -> token -> position -> bool *)
let expected_tokens_hint (chk : 'a I.checkpoint) (pos : Lexing.position) : string list =
  expected_candidates
  |> List.filter (fun tok -> I.acceptable chk tok pos)
  |> List.map Debug.string_of_token
  |> List.sort String.compare
  |> uniq_sorted
  |> take 12

let parse_text ~(file:string option) ~(dump_ast:bool) ~(text:string) : output =
  if String.trim text = "" then
    { ast = None; diags = []; ast_dump = None }
  else
    let lexbuf = Lexing.from_string text in

    (match file with
     | None -> ()
     | Some f ->
         let p = lexbuf.lex_curr_p in
         lexbuf.lex_curr_p <- { p with pos_fname = f });

    let last_tok : Parser.token option ref = ref None in
    let last_sp  : Lexing.position ref = ref lexbuf.lex_curr_p in
    let last_ep  : Lexing.position ref = ref lexbuf.lex_curr_p in
    let last_lex : string ref = ref "" in
    let last_expected : string list ref = ref [] in

    let next_token () : Parser.token * Lexing.position * Lexing.position =
      let t = Lexer.token lexbuf in
      let sp = Lexing.lexeme_start_p lexbuf in
      let ep = Lexing.lexeme_end_p lexbuf in
      last_tok := Some t;
      last_sp := sp;
      last_ep := ep;
      last_lex := Lexing.lexeme lexbuf;
      (t, sp, ep)
    in

    let mk_error_diag () : T.Diagnostic.t =
      let loc = loc_of_lex file !last_sp !last_ep in
      let expected_hint =
        match !last_expected with
        | [] -> ""
        | xs -> " Expected: " ^ String.concat ", " xs
      in
      match !last_tok with
      | None ->
          diag_error loc ("Parse error (no token info)." ^ expected_hint)
      | Some Parser.EOF ->
          diag_error loc ("Parse error: unexpected end of file." ^ expected_hint)
      | Some t ->
          let tok_s = Debug.string_of_token t in
          let base =
            if !last_lex = "" then
              Printf.sprintf "Parse error near token %s" tok_s
            else
              Printf.sprintf "Parse error near token %s (lexeme: %S)" tok_s !last_lex
          in
          diag_error loc (base ^ expected_hint)
    in

    (* IMPORTANT: adjust if your start symbol is not 'program' *)
    let checkpoint = Parser.Incremental.program lexbuf.lex_curr_p in

    let rec drive (chk : Ast.program I.checkpoint) : (Ast.program, T.Diagnostic.t) result =
      match chk with
      | I.InputNeeded _env ->
          (* Compute hints *before* consuming the next token. *)
          last_expected := expected_tokens_hint chk lexbuf.lex_curr_p;
          let (t, sp, ep) = next_token () in
          drive (I.offer chk (t, sp, ep))

      | I.Shifting _ | I.AboutToReduce _ ->
          drive (I.resume chk)

      | I.Accepted ast ->
          Ok ast

      | I.HandlingError _env ->
          Error (mk_error_diag ())

      | I.Rejected ->
          Error (mk_error_diag ())
    in

    try
      match drive checkpoint with
      | Ok ast ->
          let ast_dump = if dump_ast then Some (Ast.Debug.to_string ast) else None in
          { ast = Some ast; diags = []; ast_dump }
      | Error d ->
          { ast = None; diags = [d]; ast_dump = None }
    with
    | Lexer.Lex_error (msg, sp, ep) ->
        let loc = loc_of_lex file sp ep in
        { ast = None; diags = [diag_error loc ("Lex error: " ^ msg)]; ast_dump = None }
    | exn ->
        let p = lexbuf.lex_curr_p in
        let loc = loc_of_lex file p p in
        { ast = None; diags = [diag_error loc ("Unhandled exception: " ^ Printexc.to_string exn)]; ast_dump = None }
