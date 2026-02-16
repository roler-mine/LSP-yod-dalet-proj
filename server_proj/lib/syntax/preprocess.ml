module T = Lsp.Types

type import_kind = Compool

type import = {
  kind : import_kind;
  name : string;
  loc  : Ast.Loc.t;
}

type result = {
  text : string;
  imports : import list;
  compool_def : string option;
  diags : T.Diagnostic.t list;
}

let uppercase = String.uppercase_ascii
let known_exts = [ ".jov"; ".j73"; ".jvl"; ".j" ]

let basename_any (s:string) : string =
  let u = String.map (fun c -> if c = '\\' then '/' else c) s in
  match String.rindex_opt u '/' with
  | Some i -> String.sub u (i + 1) (String.length u - i - 1)
  | None -> u

let strip_known_ext (s:string) : string =
  let lower = String.lowercase_ascii s in
  let rec go = function
    | [] -> s
    | ext :: tl ->
        let n = String.length lower in
        let m = String.length ext in
        if n >= m && String.sub lower (n - m) m = ext
        then String.sub s 0 (n - m)
        else go tl
  in
  go known_exts

let normalize_compool_name (s:string) : string =
  s
  |> String.trim
  |> basename_any
  |> strip_known_ext
  |> String.trim
  |> uppercase

let mk_loc_of_lex ~(file:string option) (sp:Lexing.position) (ep:Lexing.position) : Ast.Loc.t =
  Ast.Loc.of_lexing_positions sp ep ~file

let diag_error (loc:Ast.Loc.t) (msg:string) : T.Diagnostic.t =
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Error
    ~source:"preprocess"
    ~message:msg
    loc

type scanned_name = {
  raw : string;
  norm : string;
  sp : Lexing.position;
  ep : Lexing.position;
}

let scan_from_tokens ~(file:string option) ~(text:string)
  : (import list * (string * int * int * int) option) =
  let lexbuf = Lexing.from_string text in
  (match file with
   | None -> ()
   | Some f ->
       lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = f });
  let rec gather acc =
    try
      let tok = Lexer.token lexbuf in
      let sp = Lexing.lexeme_start_p lexbuf in
      let ep = Lexing.lexeme_end_p lexbuf in
      let acc = (tok, sp, ep) :: acc in
      match tok with
      | Parser.EOF -> List.rev acc
      | _ -> gather acc
    with _ ->
      List.rev acc
  in
  let toks = gather [] in
  let arr = Array.of_list toks in
  let len = Array.length arr in
  let tok_at i = let (t, _, _) = arr.(i) in t in
  let prev_is_bang i =
    i > 0
    && match tok_at (i - 1) with
       | Parser.BANG -> true
       | _ -> false
  in
  let find_name_after start : scanned_name option =
    let rec go j steps =
      if j >= len || steps > 12 then None
      else
        match arr.(j) with
        | (Parser.LPAREN | Parser.RPAREN | Parser.COMMA), _, _ ->
            go (j + 1) (steps + 1)
        | Parser.ID raw, sp, ep
        | Parser.STRINGLIT raw, sp, ep ->
            let norm = normalize_compool_name raw in
            if norm = "" then go (j + 1) (steps + 1)
            else Some { raw; norm; sp; ep }
        | _ ->
            None
    in
    go start 0
  in
  let imports_rev = ref [] in
  let compool_hit = ref None in
  for i = 0 to len - 1 do
    match tok_at i with
    | Parser.BANG ->
        if i + 1 < len then
          (match tok_at (i + 1) with
           | Parser.COMPOOL | Parser.ICOMPOOL ->
               (match find_name_after (i + 2) with
                | None -> ()
                | Some nm ->
                    imports_rev :=
                      { kind = Compool; name = nm.norm; loc = mk_loc_of_lex ~file nm.sp nm.ep }
                      :: !imports_rev)
           | _ -> ())
    | Parser.ICOMPOOL when not (prev_is_bang i) ->
        (match find_name_after (i + 1) with
         | None -> ()
         | Some nm ->
             imports_rev :=
               { kind = Compool; name = nm.norm; loc = mk_loc_of_lex ~file nm.sp nm.ep }
               :: !imports_rev)
    | Parser.COMPOOL when not (prev_is_bang i) && !compool_hit = None ->
        (match find_name_after (i + 1) with
         | None -> ()
         | Some nm ->
             let col0 = nm.sp.pos_cnum - nm.sp.pos_bol in
             let col1 = nm.ep.pos_cnum - nm.ep.pos_bol in
             compool_hit := Some (nm.raw, nm.sp.pos_lnum, col0, col1))
    | _ ->
        ()
  done;
  (List.rev !imports_rev, !compool_hit)

let scan_compool_def ~(text:string) : string option =
  let _, compool_hit = scan_from_tokens ~file:None ~text in
  match compool_hit with
  | None -> None
  | Some (nm, _, _, _) ->
      let k = normalize_compool_name nm in
      if k = "" then None else Some k

let run ~(file:string option) ~(text:string) : result =
  let imports, compool_hit = scan_from_tokens ~file ~text in
  let compool_def =
    match compool_hit with
    | None -> None
    | Some (nm, _, _, _) ->
        let k = normalize_compool_name nm in
        if k = "" then None else Some k
  in

  let mismatch_diags =
    match file, compool_hit with
    | Some path, Some (def_raw, line_no, c0, c1) ->
        let expected = normalize_compool_name (Filename.basename path) in
        let found = normalize_compool_name def_raw in
        if expected <> "" && found <> "" && expected <> found then
          let sp : Ast.Loc.pos = { line = line_no; col = c0; offset = 0 } in
          let ep : Ast.Loc.pos = { line = line_no; col = c1; offset = 0 } in
          let loc = Ast.Loc.make ~file:(Some path) ~start_pos:sp ~end_pos:ep in
          [diag_error loc
             (Printf.sprintf
                "COMPOOL name %S must match file name %S (extension omitted)."
                found expected)]
        else
          []
    | _ -> []
  in

  { text; imports; compool_def; diags = mismatch_diags }
