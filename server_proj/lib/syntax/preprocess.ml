module T = Lsp.Types

type import_kind = Compool

type import = {
  kind : import_kind;
  name : string;
  loc  : Ast.Loc.t;
}

type define = {
  name : string;
  key : string;
  formals : string list;
  requires_call : bool;
  body : string;
  loc : Ast.Loc.t;
  decl_start_off : int;
}

type result = {
  text : string;
  imports : import list;
  compool_def : string option;
  defines : define list;
  diags : T.Diagnostic.t list;
}

let uppercase = String.uppercase_ascii
let known_exts = [ ".jov"; ".j73"; ".jvl"; ".j" ]

type lex_tok = Parser.token * Lexing.position * Lexing.position

type define_macro = {
  key : string;
  name : string;
  requires_call : bool;
  formals : string list;
  body : string;
  loc : Ast.Loc.t;
  start_off : int;
  end_off : int;
}

let max_macro_expand_passes = 24
let max_macro_expand_chars = 2_000_000

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

let lex_all_tokens ~(file:string option) ~(text:string) : lex_tok array =
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
  gather [] |> Array.of_list

let tok_at (arr:lex_tok array) (i:int) : Parser.token =
  let (t, _, _) = arr.(i) in
  t

type scanned_name = {
  raw : string;
  norm : string;
  sp : Lexing.position;
  ep : Lexing.position;
}

let scan_from_token_array ~(file:string option) (arr:lex_tok array)
  : (import list * (string * int * int * int) option) =
  let len = Array.length arr in
  let prev_is_bang i =
    i > 0
    && match tok_at arr (i - 1) with
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
    match tok_at arr i with
    | Parser.BANG ->
        if i + 1 < len then
          (match tok_at arr (i + 1) with
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

let scan_from_tokens ~(file:string option) ~(text:string)
  : (import list * (string * int * int * int) option) =
  let arr = lex_all_tokens ~file ~text in
  scan_from_token_array ~file arr

let is_define_list_option (s:string) : bool =
  let k = uppercase s in
  k = "LISTEXP" || k = "LISTINV" || k = "LISTBOTH"

let macro_signature (m:define_macro) : string =
  if m.requires_call then
    Printf.sprintf "%s#CALL#%d" m.key (List.length m.formals)
  else
    m.key ^ "#OBJ"

let parse_formals_list (arr:lex_tok array) ~(start:int) : (string list * int) option =
  let len = Array.length arr in
  if start >= len then None
  else
    match tok_at arr start with
    | Parser.RPAREN ->
        Some ([], start + 1)
    | _ ->
        let rec loop (acc_rev:string list) (j:int) : (string list * int) option =
          if j >= len then None
          else
            match tok_at arr j with
            | Parser.ID raw ->
                let acc_rev = uppercase raw :: acc_rev in
                let j = j + 1 in
                if j >= len then None
                else
                  (match tok_at arr j with
                   | Parser.COMMA ->
                       loop acc_rev (j + 1)
                   | Parser.RPAREN ->
                       Some (List.rev acc_rev, j + 1)
                   | _ ->
                       None)
            | _ ->
                None
        in
        loop [] start

let parse_define_at ~(file:string option) (arr:lex_tok array) (i:int) : (define_macro * int) option =
  let len = Array.length arr in
  if i < 0 || i >= len then None
  else
    match tok_at arr i with
    | Parser.DEFINE ->
        let j = ref (i + 1) in
        let next_id () =
          if !j >= len then None
          else
            match arr.(!j) with
            | Parser.ID raw, sp, ep ->
                incr j;
                Some (raw, sp, ep)
            | _ ->
                None
        in
        (match next_id () with
         | None -> None
         | Some (name_raw, sp_name, ep_name) ->
             let requires_call = ref false in
             let malformed = ref false in
             let formals = ref [] in
             if !j < len then
                (match tok_at arr !j with
                 | Parser.LPAREN ->
                     requires_call := true;
                     (match parse_formals_list arr ~start:(!j + 1) with
                     | None ->
                         malformed := true
                     | Some (xs, j_after) ->
                         formals := xs;
                         j := j_after)
                 | _ -> ());
             if (not !malformed) && !j < len then
               (match tok_at arr !j with
                | Parser.ID raw when is_define_list_option raw ->
                    incr j
                | _ -> ());
             if !malformed || !j >= len then None
             else
               match arr.(!j) with
               | Parser.STRINGLIT body, _, ep_string ->
                   incr j;
                   let end_off =
                     if !j < len then
                       match arr.(!j) with
                       | (Parser.SEMI | Parser.COMMA), _, ep_term ->
                           incr j;
                           ep_term.pos_cnum
                       | _ ->
                           ep_string.pos_cnum
                     else
                       ep_string.pos_cnum
                   in
                   let _, sp_def, _ = arr.(i) in
                   let m =
                     {
                       key = uppercase name_raw;
                       name = name_raw;
                       requires_call = !requires_call;
                       formals = !formals;
                       body;
                       loc = mk_loc_of_lex ~file sp_name ep_name;
                       start_off = sp_def.pos_cnum;
                       end_off;
                     }
                   in
                   Some (m, !j)
               | _ ->
                   None)
    | _ ->
        None

let latest_define_macros (defs:define_macro list) : define_macro list =
  let signature_last_idx = Hashtbl.create 32 in
  List.iteri (fun idx m -> Hashtbl.replace signature_last_idx (macro_signature m) idx) defs;
  defs
  |> List.mapi (fun idx m -> (idx, m))
  |> List.filter_map (fun (idx, m) ->
       match Hashtbl.find_opt signature_last_idx (macro_signature m) with
       | Some last_idx when last_idx = idx -> Some m
       | _ -> None)

let collect_define_macros ~(file:string option) (arr:lex_tok array)
  : (define_macro list * (int * int) list) =
  let len = Array.length arr in
  let defs_rev = ref [] in
  let spans_rev = ref [] in
  let i = ref 0 in
  while !i < len do
    match tok_at arr !i with
    | Parser.DEFINE ->
        (match parse_define_at ~file arr !i with
         | Some (m, next_i) ->
             defs_rev := m :: !defs_rev;
             spans_rev := (m.start_off, m.end_off) :: !spans_rev;
             if next_i > !i then i := next_i else incr i
         | None ->
             incr i)
    | _ ->
        incr i
  done;
  (List.rev !defs_rev, List.rev !spans_rev)

let starts_with_ci (s:string) ~(pos:int) (pat:string) : bool =
  let n = String.length s in
  let m = String.length pat in
  if m = 0 || pos < 0 || pos + m > n then false
  else
    let rec loop k =
      if k = m then true
      else
        let a = Char.uppercase_ascii s.[pos + k] in
        let b = Char.uppercase_ascii pat.[k] in
        if a = b then loop (k + 1) else false
    in
    loop 0

let is_ws_char = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let skip_ws (s:string) (i:int) : int =
  let rec go j =
    if j < String.length s && is_ws_char s.[j] then go (j + 1) else j
  in
  go i

let parse_call_arguments (s:string) ~(open_idx:int) : (string list * int) option =
  let n = String.length s in
  if open_idx < 0 || open_idx >= n || s.[open_idx] <> '(' then None
  else
    let depth = ref 1 in
    let in_single = ref false in
    let in_double = ref false in
    let k = ref (open_idx + 1) in
    let arg_start = ref (open_idx + 1) in
    let args_rev = ref [] in
    let push_arg stop =
      let len = stop - !arg_start in
      let arg = if len <= 0 then "" else String.sub s !arg_start len in
      args_rev := arg :: !args_rev
    in
    let continue = ref true in
    while !continue && !k < n do
      let c = s.[!k] in
      if !in_single then (
        if c = '\'' then
          if !k + 1 < n && s.[!k + 1] = '\'' then
            k := !k + 1
          else
            in_single := false
      ) else if !in_double then (
        if c = '"' then
          if !k + 1 < n && s.[!k + 1] = '"' then
            k := !k + 1
          else
            in_double := false
      ) else (
        match c with
        | '\'' ->
            in_single := true
        | '"' ->
            in_double := true
        | '(' ->
            incr depth
        | ')' ->
            decr depth;
            if !depth = 0 then (
              push_arg !k;
              continue := false
            )
        | ',' when !depth = 1 ->
            push_arg !k;
            arg_start := !k + 1
        | _ ->
            ()
      );
      incr k
    done;
    if !depth <> 0 then None
    else
      let args = List.rev !args_rev in
      let args =
        if args = [ "" ] && !k = open_idx + 2 then [] else args
      in
      Some (args, !k)

let replace_formals_in_body ~(body:string) ~(pairs:(string * string) list) : string =
  if pairs = [] then body
  else
    let pairs =
      List.sort
        (fun (a, _) (b, _) -> compare (String.length b) (String.length a))
        pairs
    in
    let n = String.length body in
    let buf = Buffer.create (max 16 n) in
    let rec loop i =
      if i >= n then ()
      else if body.[i] = '$' then
        let rec try_pairs = function
          | [] ->
              Buffer.add_char buf '$';
              loop (i + 1)
          | (formal, arg) :: tl ->
              if starts_with_ci body ~pos:(i + 1) formal then (
                Buffer.add_string buf arg;
                loop (i + 1 + String.length formal)
              ) else
                try_pairs tl
        in
        try_pairs pairs
      else (
        Buffer.add_char buf body.[i];
        loop (i + 1)
      )
    in
    loop 0;
    Buffer.contents buf

let macro_name_len_desc (a:define_macro) (b:define_macro) : int =
  compare (String.length b.name) (String.length a.name)

let find_function_macro_match
    ~(s:string)
    ~(pos:int)
    ~(function_macros:define_macro list)
  : (int * string) option =
  let rec loop = function
    | [] -> None
    | m :: tl ->
        if starts_with_ci s ~pos m.name then
          let after_name = pos + String.length m.name in
          let open_idx = skip_ws s after_name in
          (match parse_call_arguments s ~open_idx with
           | Some (args, end_pos) when List.length args = List.length m.formals ->
               let repl =
                 if m.formals = [] then
                   m.body
                 else
                   let pairs =
                     List.combine m.formals args
                   in
                   replace_formals_in_body ~body:m.body ~pairs
               in
               Some (end_pos - pos, repl)
           | _ ->
               loop tl)
        else
          loop tl
  in
  loop function_macros

let find_object_macro_match
    ~(s:string)
    ~(pos:int)
    ~(object_macros:define_macro list)
  : (int * string) option =
  let rec loop = function
    | [] -> None
    | m :: tl ->
        if starts_with_ci s ~pos m.name then
          Some (String.length m.name, m.body)
        else
          loop tl
  in
  loop object_macros

let expand_macros_once
    ~(s:string)
    ~(object_macros:define_macro list)
    ~(function_macros:define_macro list)
  : string =
  let n = String.length s in
  let buf = Buffer.create (max 16 n) in
  let rec loop i =
    if i >= n then ()
    else
      match find_function_macro_match ~s ~pos:i ~function_macros with
      | Some (consumed, repl) when consumed > 0 ->
          Buffer.add_string buf repl;
          loop (i + consumed)
      | _ ->
          (match find_object_macro_match ~s ~pos:i ~object_macros with
           | Some (consumed, repl) when consumed > 0 ->
               Buffer.add_string buf repl;
               loop (i + consumed)
           | _ ->
               Buffer.add_char buf s.[i];
               loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let expand_macros_recursive
    ~(s:string)
    ~(object_macros:define_macro list)
    ~(function_macros:define_macro list)
  : string =
  let rec pass n current =
    if n >= max_macro_expand_passes then current
    else
      let next =
        expand_macros_once ~s:current ~object_macros ~function_macros
      in
      if next = current then current
      else if String.length next > max_macro_expand_chars then next
      else pass (n + 1) next
  in
  pass 0 s

let merge_spans (spans:(int * int) list) : (int * int) list =
  let spans =
    spans
    |> List.filter_map (fun (a, b) ->
         if b <= a then None else Some (a, b))
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  let rec loop current acc rest =
    match current, rest with
    | None, [] ->
        List.rev acc
    | Some seg, [] ->
        List.rev (seg :: acc)
    | None, seg :: tl ->
        loop (Some seg) acc tl
    | Some (a0, b0), (a1, b1) :: tl ->
        if a1 <= b0 then
          loop (Some (a0, max b0 b1)) acc tl
        else
          loop (Some (a1, b1)) ((a0, b0) :: acc) tl
  in
  loop None [] spans

let expand_text_with_macros
    ~(text:string)
    ~(macros:define_macro list)
    ~(define_spans:(int * int) list)
  : string =
  if macros = [] then text
  else
    let object_macros =
      macros
      |> List.filter (fun m -> not m.requires_call)
      |> List.sort macro_name_len_desc
    in
    let function_macros =
      macros
      |> List.filter (fun m -> m.requires_call)
      |> List.sort macro_name_len_desc
    in
    let merged_spans = merge_spans define_spans in
    let n = String.length text in
    let clamp x = if x < 0 then 0 else if x > n then n else x in
    let buf = Buffer.create (max 16 n) in
    let cursor = ref 0 in
    List.iter (fun (a_raw, b_raw) ->
      let a = clamp a_raw in
      let b = clamp b_raw in
      if a > !cursor then (
        let chunk = String.sub text !cursor (a - !cursor) in
        let chunk' =
          expand_macros_recursive ~s:chunk ~object_macros ~function_macros
        in
        Buffer.add_string buf chunk'
      );
      if b > a then Buffer.add_substring buf text a (b - a);
      cursor := max !cursor b
    ) merged_spans;
    if !cursor < n then (
      let chunk = String.sub text !cursor (n - !cursor) in
      let chunk' =
        expand_macros_recursive ~s:chunk ~object_macros ~function_macros
      in
      Buffer.add_string buf chunk'
    );
    Buffer.contents buf

let scan_compool_def ~(text:string) : string option =
  let _, compool_hit = scan_from_tokens ~file:None ~text in
  match compool_hit with
  | None -> None
  | Some (nm, _, _, _) ->
      let k = normalize_compool_name nm in
      if k = "" then None else Some k

let run ~(file:string option) ~(text:string) : result =
  let raw_arr = lex_all_tokens ~file ~text in
  let all_macros, define_spans = collect_define_macros ~file raw_arr in
  let macros = latest_define_macros all_macros in
  let text = expand_text_with_macros ~text ~macros ~define_spans in
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

  let defines =
    List.map (fun m ->
      {
        name = m.name;
        key = m.key;
        formals = m.formals;
        requires_call = m.requires_call;
        body = m.body;
        loc = m.loc;
        decl_start_off = m.start_off;
      }
    ) all_macros
  in
  { text; imports; compool_def; defines; diags = mismatch_diags }
