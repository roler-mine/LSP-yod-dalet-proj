(* lib/parse.ml *)

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
  { Ast.Loc.file = file; start_pos = mk sp; end_pos = mk ep }

let attach_file (file : string option) (loc : Ast.Loc.t) : Ast.Loc.t =
  match file, loc.Ast.Loc.file with
  | Some f, None -> { loc with Ast.Loc.file = Some f }
  | _ -> loc

let diag ~sev ~source (loc : Ast.Loc.t) (msg : string) : T.Diagnostic.t =
  Lsp_conv.diagnostic
    ~severity:sev
    ~source
    ~message:msg
    loc

let diag_error (loc : Ast.Loc.t) (msg : string) : T.Diagnostic.t =
  diag ~sev:T.DiagnosticSeverity.Error ~source:"parse" loc msg

let diag_warn (loc : Ast.Loc.t) (msg : string) : T.Diagnostic.t =
  diag ~sev:T.DiagnosticSeverity.Warning ~source:"parse" loc msg

module Debug = struct
  let string_of_token (t : Parser.token) : string =
    match t with
    | Parser.EOF -> "EOF"
    | Parser.ID _ -> "ID"
    | Parser.INTLIT _ -> "INTLIT"
    | Parser.FLOATLIT _ -> "FLOATLIT"
    | Parser.STRINGLIT _ -> "STRINGLIT"
    | Parser.TRUE -> "TRUE"
    | Parser.FALSE -> "FALSE"

    | Parser.START -> "START" | Parser.TERM -> "TERM" | Parser.BEGIN -> "BEGIN" | Parser.END -> "END"
    | Parser.DEF -> "DEF" | Parser.REF -> "REF" | Parser.PROC -> "PROC"
    | Parser.ITEM -> "ITEM" | Parser.TABLE -> "TABLE"
    | Parser.STATIC -> "STATIC" | Parser.CONSTANT -> "CONSTANT"
    | Parser.IF -> "IF" | Parser.ELSE -> "ELSE" | Parser.WHILE -> "WHILE" | Parser.FOR -> "FOR" | Parser.BY -> "BY" | Parser.THEN -> "THEN"
    | Parser.CASE -> "CASE" | Parser.DEFAULT -> "DEFAULT" | Parser.FALLTHRU -> "FALLTHRU"
    | Parser.EXIT -> "EXIT" | Parser.GOTO -> "GOTO" | Parser.RETURN -> "RETURN" | Parser.ABORT -> "ABORT" | Parser.STOP -> "STOP"
    | Parser.PROGRAM -> "PROGRAM" | Parser.COMPOOL -> "COMPOOL" | Parser.ICOMPOOL -> "ICOMPOOL"
    | Parser.DEFINE -> "DEFINE" | Parser.TYPE -> "TYPE" | Parser.BLOCK -> "BLOCK"

    | Parser.NOT -> "NOT" | Parser.AND -> "AND" | Parser.OR -> "OR" | Parser.XOR -> "XOR" | Parser.EQV -> "EQV"
    | Parser.MOD -> "MOD"

    | Parser.LPAREN -> "(" | Parser.RPAREN -> ")"
    | Parser.COMMA -> "," | Parser.SEMI -> ";" | Parser.COLON -> ":" | Parser.DOT -> "."
    | Parser.BANG -> "!" | Parser.AT -> "@"
    | Parser.CONV_L -> "(*" | Parser.CONV_R -> "*)"

    | Parser.EQ -> "=" | Parser.LT -> "<" | Parser.GT -> ">"
    | Parser.LE -> "<=" | Parser.GE -> ">=" | Parser.NE -> "<>"
    | Parser.PLUS -> "+" | Parser.MINUS -> "-" | Parser.STAR -> "*"
    | Parser.SLASH -> "/" | Parser.POW -> "^"
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
    Parser.DEF; Parser.REF; Parser.PROC; Parser.ITEM; Parser.TABLE; Parser.STATIC; Parser.CONSTANT;
    Parser.PROGRAM; Parser.COMPOOL; Parser.ICOMPOOL; Parser.DEFINE; Parser.TYPE; Parser.BLOCK;
    Parser.IF; Parser.ELSE; Parser.WHILE; Parser.FOR; Parser.BY; Parser.THEN;
    Parser.CASE; Parser.DEFAULT; Parser.FALLTHRU;
    Parser.RETURN; Parser.EXIT; Parser.GOTO; Parser.ABORT; Parser.STOP;
    Parser.TRUE; Parser.FALSE;
    Parser.NOT; Parser.AND; Parser.OR; Parser.XOR; Parser.EQV; Parser.MOD;

    Parser.LPAREN; Parser.RPAREN; Parser.COMMA; Parser.SEMI; Parser.COLON; Parser.DOT; Parser.BANG; Parser.AT;
    Parser.CONV_L; Parser.CONV_R;
    Parser.EQ; Parser.LT; Parser.GT; Parser.LE; Parser.GE; Parser.NE;
    Parser.PLUS; Parser.MINUS; Parser.STAR; Parser.SLASH; Parser.POW;

    Parser.EOF;
  ]

(* Menhir (table+inspection):
   acceptable : checkpoint -> token -> position -> bool *)
let expected_tokens_hint (chk : 'a I.checkpoint) (pos : Lexing.position) : string list =
  expected_candidates
  |> List.filter (fun tok -> I.acceptable chk tok pos)
  |> List.map Debug.string_of_token
  |> List.sort String.compare
  |> uniq_sorted
  |> take 12

let constant_storage_warning =
  "CONSTANT items should have static or external allocation."

let parse_diags_to_lsp ~(file:string option) : T.Diagnostic.t list =
  let seen_const_storage = ref false in
  Parse_diags.take ()
  |> List.filter_map (fun (loc, msg) ->
       if msg = constant_storage_warning then
         if !seen_const_storage then None
         else (
           seen_const_storage := true;
           Some
             (diag_warn
                (attach_file file loc)
                "CONSTANT item uses automatic allocation; consider STATIC/DEF/REF where required.")
         )
       else
         Some (diag_warn (attach_file file loc) msg))

let parse_text ~(file:string option) ~(dump_ast:bool) ~(text:string) : output =
  (* Clear stale recovery/warn diags from previous runs *)
  Parse_diags.clear ();

  if String.trim text = "" then
    { ast = None; diags = []; ast_dump = None }
  else
    let lexbuf = Lexing.from_string text in

    (match file with
     | None -> ()
     | Some f ->
         let p = lexbuf.lex_curr_p in
         lexbuf.lex_curr_p <- { p with pos_fname = f });

    (* Track the most recent token + span + lexeme so HandlingError can report accurately. *)
    let last_tok : Parser.token option ref = ref None in
    let last_sp  : Lexing.position ref = ref lexbuf.lex_curr_p in
    let last_ep  : Lexing.position ref = ref lexbuf.lex_curr_p in
    let last_lex : string ref = ref "" in
    let last_expected : string list ref = ref [] in
    let pending_tok : (Parser.token * Lexing.position * Lexing.position * string) option ref = ref None in
    let parse_errors : T.Diagnostic.t list ref = ref [] in
    let last_error_span : (int * int) option ref = ref None in

    let remember_token (t : Parser.token) (sp : Lexing.position) (ep : Lexing.position) (lexeme : string) : unit =
      last_tok := Some t;
      last_sp := sp;
      last_ep := ep;
      last_lex := lexeme
    in

    let next_token () : Parser.token * Lexing.position * Lexing.position =
      match !pending_tok with
      | Some (t, sp, ep, lexeme) ->
          pending_tok := None;
          remember_token t sp ep lexeme;
          (t, sp, ep)
      | None ->
          let t = Lexer.token lexbuf in
          let sp = Lexing.lexeme_start_p lexbuf in
          let ep = Lexing.lexeme_end_p lexbuf in
          remember_token t sp ep (Lexing.lexeme lexbuf);
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

    let add_parse_error () : unit =
      let key = (!last_sp).Lexing.pos_cnum, (!last_ep).Lexing.pos_cnum in
      if Some key <> !last_error_span then (
        last_error_span := Some key;
        parse_errors := (mk_error_diag ()) :: !parse_errors
      )
    in

    let is_sync_token = function
      | Parser.SEMI | Parser.COMMA | Parser.END | Parser.TERM | Parser.EOF -> true
      | _ -> false
    in

    let rec resume_to_input_or_done
      (chk : Ast.program I.checkpoint)
      (fuel : int)
      : [ `NeedInput of Ast.program I.checkpoint | `Accepted of Ast.program | `Rejected ] =
      if fuel <= 0 then `Rejected
      else
        match chk with
        | I.InputNeeded _ -> `NeedInput chk
        | I.Accepted ast -> `Accepted ast
        | I.Rejected -> `Rejected
        | I.Shifting _ | I.AboutToReduce _ | I.HandlingError _ ->
            resume_to_input_or_done (I.resume chk) (fuel - 1)
    in

    let rec skip_to_sync (fuel : int) : bool =
      if fuel <= 0 then false
      else
        let t = Lexer.token lexbuf in
        let sp = Lexing.lexeme_start_p lexbuf in
        let ep = Lexing.lexeme_end_p lexbuf in
        let lexeme = Lexing.lexeme lexbuf in
        if is_sync_token t then (
          pending_tok := Some (t, sp, ep, lexeme);
          true
        ) else
          skip_to_sync (fuel - 1)
    in

    (* IMPORTANT: adjust if your start symbol is not 'program' *)
    let checkpoint = Parser.Incremental.program lexbuf.lex_curr_p in

    let rec drive (chk : Ast.program I.checkpoint) : Ast.program option =
      match chk with
      | I.InputNeeded _env ->
        (* Avoid expensive expected-token probing on every token;
           compute hints only when an error is actually encountered. *)
        last_expected := [];
        let (t, sp, ep) = next_token () in
        drive (I.offer chk (t, sp, ep))

      | I.Shifting _ | I.AboutToReduce _ ->
        drive (I.resume chk)

      | I.Accepted ast ->
        Some ast

      | I.HandlingError _env ->
        let resumed = resume_to_input_or_done chk 1024 in
        last_expected :=
          (match resumed with
           | `NeedInput chk' -> expected_tokens_hint chk' !last_ep
           | `Accepted _ | `Rejected -> []);
        add_parse_error ();
        begin
          match resumed with
          | `Accepted ast -> Some ast
          | `NeedInput chk' ->
              if skip_to_sync 4096 then drive chk' else None
          | `Rejected -> None
        end

      | I.Rejected ->
        None
    in

    try
      let ast_opt = drive checkpoint in
      let extra = parse_diags_to_lsp ~file in
      let errs = List.rev !parse_errors in
      match ast_opt with
      | Some ast ->
          let ast_dump = if dump_ast then Some (Ast.Debug.to_string ast) else None in
          { ast = Some ast; diags = errs @ extra; ast_dump }
      | None ->
          let errs =
            match errs with
            | [] -> [mk_error_diag ()]
            | xs -> xs
          in
          { ast = None; diags = errs @ extra; ast_dump = None }

    with
    | Lexer.Lex_error (msg, sp, ep) ->
        let extra = parse_diags_to_lsp ~file in
        let errs = List.rev !parse_errors in
        let loc = loc_of_lex file sp ep in
        { ast = None; diags = errs @ extra @ [diag_error loc ("Lex error: " ^ msg)]; ast_dump = None }

    | exn ->
        let extra = parse_diags_to_lsp ~file in
        let errs = List.rev !parse_errors in
        let p = lexbuf.lex_curr_p in
        let loc = loc_of_lex file p p in
        { ast = None; diags = errs @ extra @ [diag_error loc ("Unhandled exception: " ^ Printexc.to_string exn)]; ast_dump = None }
