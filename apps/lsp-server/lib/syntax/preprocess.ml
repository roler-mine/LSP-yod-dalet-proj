module T = Lsp.Types

type import_kind = Compool
type import = { kind : import_kind; name : string; loc : Ast.Loc.t }

type define = {
  name : string;
  key : string;
  formals : string list;
  requires_call : bool;
  body : string;
  loc : Ast.Loc.t;
  decl_start_off : int;
}

type source_span = {
  source_start_off : int;
  source_end_off : int;
  source_loc : Ast.Loc.t;
}

type expansion_origin =
  | Original of source_span
  | MacroExpansion of {
      macro_name : string;
      macro_decl : Ast.Loc.t;
      call_site : Ast.Loc.t;
      original_tokens : source_span list;
    }

type expansion_segment = {
  generated_start_off : int;
  generated_end_off : int;
  origin : expansion_origin;
}

type result = {
  text : string;
  imports : import list;
  compool_def : string option;
  defines : define list;
  source_map : expansion_segment list;
  diags : T.Diagnostic.t list;
}

let loc_through_source_map (source_map : expansion_segment list)
    (loc : Ast.Loc.t) : Ast.Loc.t =
  let off = loc.Ast.Loc.start_pos.offset in
  let hit =
    List.find_opt
      (fun seg ->
        off >= seg.generated_start_off && off <= seg.generated_end_off)
      source_map
  in
  match hit with
  | None -> loc
  | Some { origin = Original span; _ } -> span.source_loc
  | Some { origin = MacroExpansion { call_site; _ }; _ } -> call_site

let offset_of_lsp_position ~(text : string) (pos : T.Position.t) : int option =
  let idx = Text_index.of_string text in
  Text_index.offset_of_line_col idx ~line:pos.line ~col:pos.character

let diagnostic_through_source_map ~(generated_text : string)
    (source_map : expansion_segment list) (diag : T.Diagnostic.t) :
    T.Diagnostic.t =
  let start_pos =
    let offset =
      Option.value
        (offset_of_lsp_position ~text:generated_text diag.T.Diagnostic.range.start)
        ~default:0
    in
    {
      Ast.Loc.line = diag.T.Diagnostic.range.start.line + 1;
      col = diag.T.Diagnostic.range.start.character;
      offset;
    }
  in
  let end_pos =
    let offset =
      Option.value
        (offset_of_lsp_position ~text:generated_text diag.T.Diagnostic.range.end_)
        ~default:start_pos.offset
    in
    {
      Ast.Loc.line = diag.T.Diagnostic.range.end_.line + 1;
      col = diag.T.Diagnostic.range.end_.character;
      offset;
    }
  in
  let mapped =
    loc_through_source_map source_map
      (Ast.Loc.make ~file:None ~start_pos ~end_pos)
  in
  { diag with T.Diagnostic.range = Lsp_conv.range_of_loc mapped }

let diagnostics_through_source_map ~(generated_text : string) source_map diags =
  List.map (diagnostic_through_source_map ~generated_text source_map) diags

let uppercase = String.uppercase_ascii
let known_exts = Source_file.with_defaults [ ".j" ]

type lex_tok = Parser.token_span

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

let basename_any (s : string) : string =
  let u = String.map (fun c -> if c = '\\' then '/' else c) s in
  match String.rindex_opt u '/' with
  | Some i -> String.sub u (i + 1) (String.length u - i - 1)
  | None -> u

let strip_known_ext (s : string) : string =
  Source_file.strip_known_extension ~extensions:known_exts s

let normalize_compool_name (s : string) : string =
  s |> String.trim |> basename_any |> strip_known_ext |> String.trim
  |> uppercase

let mk_loc_of_lex ~(file : string option) (sp : Lexing.position)
    (ep : Lexing.position) : Ast.Loc.t =
  Ast.Loc.of_lexing_positions sp ep ~file

let diag_error (loc : Ast.Loc.t) (msg : string) : T.Diagnostic.t =
  Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"preprocess"
    ~message:msg loc

let pos_of_offset ~(text : string) (off : int) : Ast.Loc.pos =
  let n = String.length text in
  let target = max 0 (min n off) in
  let rec loop i line line_start =
    if i >= target then
      { Ast.Loc.line; col = target - line_start; offset = target }
    else if text.[i] = '\n' then loop (i + 1) (line + 1) (i + 1)
    else loop (i + 1) line line_start
  in
  loop 0 1 0

let loc_of_offsets ~(file : string option) ~(text : string) ~(start_off : int)
    ~(end_off : int) : Ast.Loc.t =
  let n = String.length text in
  let start_off = max 0 (min n start_off) in
  let end_off = max start_off (min n end_off) in
  Ast.Loc.make ~file
    ~start_pos:(pos_of_offset ~text start_off)
    ~end_pos:(pos_of_offset ~text end_off)

let source_span_of_offsets ~(file : string option) ~(text : string)
    ~(start_off : int) ~(end_off : int) : source_span =
  {
    source_start_off = start_off;
    source_end_off = end_off;
    source_loc = loc_of_offsets ~file ~text ~start_off ~end_off;
  }

let dummy_lex_tok : lex_tok =
  {
    Parser.tok = Parser.EOF;
    start_off = 0;
    end_off = 0;
    start_line = 1;
    start_col = 0;
    end_line = 1;
    end_col = 0;
    lexeme = None;
  }

type token_builder = { mutable buf : lex_tok array; mutable len : int }

let token_builder_create () : token_builder =
  { buf = Array.make 256 dummy_lex_tok; len = 0 }

let token_builder_add (b : token_builder) (tok : lex_tok) : unit =
  if b.len >= Array.length b.buf then (
    let next = Array.make (max 1 (Array.length b.buf * 2)) dummy_lex_tok in
    Array.blit b.buf 0 next 0 b.len;
    b.buf <- next);
  b.buf.(b.len) <- tok;
  b.len <- b.len + 1

let token_builder_finish (b : token_builder) : lex_tok array =
  Array.sub b.buf 0 b.len

let lex_all_tokens_impl ~(debug_lexemes : bool) ~(file : string option)
    ~(text : string) ~(start_off : int) ~(start_line : int)
    ~(start_col : int) : lex_tok array =
  Lexer.with_session_state (fun lexer ->
      let text_len = String.length text in
      let start_off = max 0 (min text_len start_off) in
      let slice =
        if start_off = 0 then text
        else String.sub text start_off (text_len - start_off)
      in
      let lexbuf = Lexing.from_string slice in
      let start_line = max 1 start_line in
      let start_col = max 0 start_col in
      let start_pos =
        {
          Lexing.pos_fname = Option.value file ~default:"";
          pos_lnum = start_line;
          pos_bol = start_off - start_col;
          pos_cnum = start_off;
        }
      in
      lexbuf.lex_abs_pos <- start_off;
      lexbuf.lex_start_p <- start_pos;
      lexbuf.lex_curr_p <- start_pos;
      let builder = token_builder_create () in
      let rec gather () =
        try
          let tok = Lexer.token lexer lexbuf in
          let sp = Lexing.lexeme_start_p lexbuf in
          let ep = Lexing.lexeme_end_p lexbuf in
          let lexeme = if debug_lexemes then Some (Lexing.lexeme lexbuf) else None in
          Parser.token_span_of_lexing_positions ?lexeme tok sp ep
          |> token_builder_add builder;
          match tok with Parser.EOF -> token_builder_finish builder | _ -> gather ()
        with _ -> token_builder_finish builder
      in
      gather ())

let lex_all_tokens ~(file : string option) ~(text : string) : lex_tok array =
  lex_all_tokens_impl ~debug_lexemes:false ~file ~text ~start_off:0
    ~start_line:1 ~start_col:0

let lex_all_tokens_with_lexemes ~(file : string option) ~(text : string) :
    lex_tok array =
  lex_all_tokens_impl ~debug_lexemes:true ~file ~text ~start_off:0
    ~start_line:1 ~start_col:0

let lex_all_tokens_from_offset ~(file : string option) ~(text : string)
    ~(start_off : int) ~(start_line : int) ~(start_col : int) : lex_tok array =
  lex_all_tokens_impl ~debug_lexemes:false ~file ~text ~start_off ~start_line
    ~start_col

let tok_at (arr : lex_tok array) (i : int) : Parser.token =
  arr.(i).Parser.tok

let sp_at ~(file : string option) (arr : lex_tok array) (i : int) :
    Lexing.position =
  Parser.token_span_start_p ~file arr.(i)

let ep_at ~(file : string option) (arr : lex_tok array) (i : int) :
    Lexing.position =
  Parser.token_span_end_p ~file arr.(i)

type scanned_name = {
  raw : string;
  norm : string;
  sp : Lexing.position;
  ep : Lexing.position;
}

let scan_from_token_array ~(file : string option) (arr : lex_tok array) :
    import list * (string * int * int * int) option =
  let len = Array.length arr in
  let prev_is_bang i =
    i > 0 && match tok_at arr (i - 1) with Parser.BANG -> true | _ -> false
  in
  let find_name_after start : scanned_name option =
    let rec go j steps =
      if j >= len || steps > 12 then None
      else
        match tok_at arr j with
        | Parser.LPAREN | Parser.RPAREN | Parser.COMMA ->
            go (j + 1) (steps + 1)
        | Parser.ID raw | Parser.STRINGLIT raw ->
            let norm = normalize_compool_name raw in
            if norm = "" then go (j + 1) (steps + 1)
            else
              let sp = sp_at ~file arr j in
              let ep = ep_at ~file arr j in
              Some { raw; norm; sp; ep }
        | _ -> None
    in
    go start 0
  in
  let imports_rev = ref [] in
  let compool_hit = ref None in
  for i = 0 to len - 1 do
    match tok_at arr i with
    | Parser.BANG -> (
        if i + 1 < len then
          match tok_at arr (i + 1) with
          | Parser.COMPOOL | Parser.ICOMPOOL -> (
              match find_name_after (i + 2) with
              | None -> ()
              | Some nm ->
                  imports_rev :=
                    {
                      kind = Compool;
                      name = nm.norm;
                      loc = mk_loc_of_lex ~file nm.sp nm.ep;
                    }
                    :: !imports_rev)
          | _ -> ())
    | Parser.ICOMPOOL when not (prev_is_bang i) -> (
        match find_name_after (i + 1) with
        | None -> ()
        | Some nm ->
            imports_rev :=
              {
                kind = Compool;
                name = nm.norm;
                loc = mk_loc_of_lex ~file nm.sp nm.ep;
              }
              :: !imports_rev)
    | Parser.COMPOOL when (not (prev_is_bang i)) && !compool_hit = None -> (
        match find_name_after (i + 1) with
        | None -> ()
        | Some nm ->
            let col0 = nm.sp.pos_cnum - nm.sp.pos_bol in
            let col1 = nm.ep.pos_cnum - nm.ep.pos_bol in
            compool_hit := Some (nm.raw, nm.sp.pos_lnum, col0, col1))
    | _ -> ()
  done;
  (List.rev !imports_rev, !compool_hit)

let scan_from_tokens ~(file : string option) ~(text : string) :
    import list * (string * int * int * int) option =
  let arr = lex_all_tokens ~file ~text in
  scan_from_token_array ~file arr

let is_define_list_option (s : string) : bool =
  let k = uppercase s in
  k = "LISTEXP" || k = "LISTINV" || k = "LISTBOTH"

let macro_signature (m : define_macro) : string =
  if m.requires_call then
    Printf.sprintf "%s#CALL#%d" m.key (List.length m.formals)
  else m.key ^ "#OBJ"

let parse_formals_list (arr : lex_tok array) ~(start : int) :
    (string list * int) option =
  let len = Array.length arr in
  if start >= len then None
  else
    match tok_at arr start with
    | Parser.RPAREN -> Some ([], start + 1)
    | _ ->
        let rec loop (acc_rev : string list) (j : int) :
            (string list * int) option =
          if j >= len then None
          else
            match tok_at arr j with
            | Parser.ID raw -> (
                let acc_rev = uppercase raw :: acc_rev in
                let j = j + 1 in
                if j >= len then None
                else
                  match tok_at arr j with
                  | Parser.COMMA -> loop acc_rev (j + 1)
                  | Parser.RPAREN -> Some (List.rev acc_rev, j + 1)
                  | _ -> None)
            | _ -> None
        in
        loop [] start

let parse_define_at ~(file : string option) (arr : lex_tok array) (i : int) :
    (define_macro * int) option =
  let len = Array.length arr in
  if i < 0 || i >= len then None
  else
    match tok_at arr i with
    | Parser.DEFINE -> (
        let j = ref (i + 1) in
        let next_id () =
          if !j >= len then None
          else
            match tok_at arr !j with
            | Parser.ID raw ->
                incr j;
                let sp = sp_at ~file arr (!j - 1) in
                let ep = ep_at ~file arr (!j - 1) in
                Some (raw, sp, ep)
            | _ -> None
        in
        match next_id () with
        | None -> None
        | Some (name_raw, sp_name, ep_name) -> (
            let requires_call = ref false in
            let malformed = ref false in
            let formals = ref [] in
            (if !j < len then
               match tok_at arr !j with
               | Parser.LPAREN -> (
                   requires_call := true;
                   match parse_formals_list arr ~start:(!j + 1) with
                   | None -> malformed := true
                   | Some (xs, j_after) ->
                       formals := xs;
                       j := j_after)
               | _ -> ());
            (if (not !malformed) && !j < len then
               match tok_at arr !j with
               | Parser.ID raw when is_define_list_option raw -> incr j
               | _ -> ());
            if !malformed || !j >= len then None
            else
              match tok_at arr !j with
              | Parser.STRINGLIT body ->
                  let ep_string = ep_at ~file arr !j in
                  incr j;
                  let end_off =
                    if !j < len then
                      match tok_at arr !j with
                      | Parser.SEMI | Parser.COMMA ->
                          let ep_term = ep_at ~file arr !j in
                          incr j;
                          ep_term.pos_cnum
                      | _ -> ep_string.pos_cnum
                    else ep_string.pos_cnum
                  in
                  let sp_def = sp_at ~file arr i in
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
              | _ -> None))
    | _ -> None

let latest_define_macros (defs : define_macro list) : define_macro list =
  let signature_last_idx = Hashtbl.create 32 in
  List.iteri
    (fun idx m -> Hashtbl.replace signature_last_idx (macro_signature m) idx)
    defs;
  defs
  |> List.mapi (fun idx m -> (idx, m))
  |> List.filter_map (fun (idx, m) ->
      match Hashtbl.find_opt signature_last_idx (macro_signature m) with
      | Some last_idx when last_idx = idx -> Some m
      | _ -> None)

let collect_define_macros ~(file : string option) (arr : lex_tok array) :
    define_macro list * (int * int) list =
  let len = Array.length arr in
  let defs_rev = ref [] in
  let spans_rev = ref [] in
  let i = ref 0 in
  while !i < len do
    match tok_at arr !i with
    | Parser.DEFINE -> (
        match parse_define_at ~file arr !i with
        | Some (m, next_i) ->
            defs_rev := m :: !defs_rev;
            spans_rev := (m.start_off, m.end_off) :: !spans_rev;
            if next_i > !i then i := next_i else incr i
        | None -> incr i)
    | _ -> incr i
  done;
  (List.rev !defs_rev, List.rev !spans_rev)

let starts_with_ci (s : string) ~(pos : int) (pat : string) : bool =
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

let is_ws_char = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let skip_ws (s : string) (i : int) : int =
  let rec go j =
    if j < String.length s && is_ws_char s.[j] then go (j + 1) else j
  in
  go i

let parse_call_arguments (s : string) ~(open_idx : int) :
    (string list * int) option =
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
      (if !in_single then (
         if c = '\'' then
           if !k + 1 < n && s.[!k + 1] = '\'' then k := !k + 1
           else in_single := false)
       else if !in_double then (
         if c = '"' then
           if !k + 1 < n && s.[!k + 1] = '"' then k := !k + 1
           else in_double := false)
       else
         match c with
         | '\'' -> in_single := true
         | '"' -> in_double := true
         | '(' -> incr depth
         | ')' ->
             decr depth;
             if !depth = 0 then (
               push_arg !k;
               continue := false)
         | ',' when !depth = 1 ->
             push_arg !k;
             arg_start := !k + 1
         | _ -> ());
      incr k
    done;
    if !depth <> 0 then None
    else
      let args = List.rev !args_rev in
      let args = if args = [ "" ] && !k = open_idx + 2 then [] else args in
      Some (args, !k)

let replace_formals_in_body ~(body : string) ~(pairs : (string * string) list) :
    string =
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
                loop (i + 1 + String.length formal))
              else try_pairs tl
        in
        try_pairs pairs
      else (
        Buffer.add_char buf body.[i];
        loop (i + 1))
    in
    loop 0;
    Buffer.contents buf

let macro_name_len_desc (a : define_macro) (b : define_macro) : int =
  compare (String.length b.name) (String.length a.name)

let is_macro_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' | '\'' -> true
  | _ -> false

let left_boundary (s : string) (pos : int) : bool =
  pos <= 0 || not (is_macro_ident_char s.[pos - 1])

let right_boundary (s : string) (pos : int) : bool =
  pos >= String.length s || not (is_macro_ident_char s.[pos])

type macro_match = {
  mm_macro : define_macro;
  mm_consumed : int;
  mm_replacement : string;
}

type working_segment = { ws_text : string; ws_origin : expansion_origin }

let source_spans_of_origin = function
  | Original span -> [ span ]
  | MacroExpansion { original_tokens; _ } -> original_tokens

let call_site_for_origin ~(file : string option) ~(raw_text : string)
    ~(chunk_source_start : int) ~(pos : int) ~(consumed : int) origin :
    Ast.Loc.t =
  match origin with
  | Original _ ->
      loc_of_offsets ~file ~text:raw_text
        ~start_off:(chunk_source_start + pos)
        ~end_off:(chunk_source_start + pos + consumed)
  | MacroExpansion { call_site; _ } -> call_site

let diag_once seen key loc msg =
  if Hashtbl.mem seen key then []
  else (
    Hashtbl.replace seen key true;
    [ diag_error loc msg ])

let find_macro_match ~(file : string option) ~(raw_text : string)
    ~(diag_seen : (string, bool) Hashtbl.t) ~(chunk_source_start : int)
    ~(origin : expansion_origin) ~(s : string) ~(pos : int)
    ~(object_macros : define_macro list)
    ~(function_macros : define_macro list) : macro_match option * T.Diagnostic.t list =
  let function_diag = ref [] in
  let rec try_function = function
    | [] -> None
    | m :: tl ->
        if
          left_boundary s pos && starts_with_ci s ~pos m.name
          && right_boundary s (pos + String.length m.name)
        then
          let after_name = pos + String.length m.name in
          let open_idx = skip_ws s after_name in
          if open_idx >= String.length s || s.[open_idx] <> '(' then (
            let loc =
              call_site_for_origin ~file ~raw_text ~chunk_source_start ~pos
                ~consumed:(String.length m.name) origin
            in
            function_diag :=
              diag_once diag_seen
                (Printf.sprintf "ctx:%s:%d" m.key (chunk_source_start + pos))
                loc
                (Printf.sprintf
                   "DEFINE macro %s requires an argument list at the call site."
                   m.name);
            try_function tl)
          else
            match parse_call_arguments s ~open_idx with
            | Some (args, end_pos) ->
                if List.length args = List.length m.formals then
                  let repl =
                    if m.formals = [] then m.body
                    else
                      let pairs = List.combine m.formals args in
                      replace_formals_in_body ~body:m.body ~pairs
                  in
                  Some
                    {
                      mm_macro = m;
                      mm_consumed = end_pos - pos;
                      mm_replacement = repl;
                    }
                else (
                  let loc =
                    call_site_for_origin ~file ~raw_text ~chunk_source_start
                      ~pos ~consumed:(end_pos - pos) origin
                  in
                  function_diag :=
                    diag_once diag_seen
                      (Printf.sprintf "arity:%s:%d" m.key
                         (chunk_source_start + pos))
                      loc
                      (Printf.sprintf
                         "DEFINE macro %s expects %d argument(s), got %d."
                         m.name (List.length m.formals) (List.length args));
                  try_function tl)
            | None -> try_function tl
        else try_function tl
  in
  match try_function function_macros with
  | Some mm -> (Some mm, !function_diag)
  | None ->
      let rec try_object = function
        | [] -> None
        | m :: tl ->
            if
              left_boundary s pos && starts_with_ci s ~pos m.name
              && right_boundary s (pos + String.length m.name)
            then
              Some
                {
                  mm_macro = m;
                  mm_consumed = String.length m.name;
                  mm_replacement = m.body;
                }
            else try_object tl
      in
      (try_object object_macros, !function_diag)

let expand_segments_once ~(file : string option) ~(raw_text : string)
    ~(diag_seen : (string, bool) Hashtbl.t)
    ~(object_macros : define_macro list)
    ~(function_macros : define_macro list) (segments : working_segment list) :
    working_segment list * bool * T.Diagnostic.t list =
  let changed = ref false in
  let diags = ref [] in
  let expanded = ref [] in
  let add_segment text origin =
    if text <> "" then expanded := { ws_text = text; ws_origin = origin } :: !expanded
  in
  let expand_segment (seg : working_segment) : unit =
    let s = seg.ws_text in
    let n = String.length s in
    let chunk_source_start =
      match seg.ws_origin with
      | Original span -> span.source_start_off
      | MacroExpansion _ -> 0
    in
    let literal_start = ref 0 in
    let flush_literal stop =
      if stop > !literal_start then
        add_segment
          (String.sub s !literal_start (stop - !literal_start))
          seg.ws_origin
    in
    let rec loop i =
      if i >= n then flush_literal n
      else
        let match_opt, match_diags =
          find_macro_match ~file ~raw_text ~diag_seen ~chunk_source_start
            ~origin:seg.ws_origin ~s ~pos:i ~object_macros ~function_macros
        in
        diags := match_diags @ !diags;
        match match_opt with
        | Some mm when mm.mm_consumed > 0 ->
            changed := true;
            flush_literal i;
            let call_site =
              call_site_for_origin ~file ~raw_text ~chunk_source_start ~pos:i
                ~consumed:mm.mm_consumed seg.ws_origin
            in
            let origin =
              MacroExpansion
                {
                  macro_name = mm.mm_macro.name;
                  macro_decl = mm.mm_macro.loc;
                  call_site;
                  original_tokens = source_spans_of_origin seg.ws_origin;
                }
            in
            add_segment mm.mm_replacement origin;
            literal_start := i + mm.mm_consumed;
            loop !literal_start
        | _ -> loop (i + 1)
    in
    loop 0
  in
  List.iter expand_segment segments;
  (List.rev !expanded, !changed, List.rev !diags)

let working_text (segments : working_segment list) : string =
  let b = Buffer.create 128 in
  List.iter (fun seg -> Buffer.add_string b seg.ws_text) segments;
  Buffer.contents b

let expansion_limit_diag ~(file : string option) ~(raw_text : string)
    (segments : working_segment list) (msg : string) : T.Diagnostic.t =
  let loc =
    match segments with
    | { ws_origin = MacroExpansion { call_site; _ }; _ } :: _ -> call_site
    | { ws_origin = Original span; _ } :: _ -> span.source_loc
    | [] -> loc_of_offsets ~file ~text:raw_text ~start_off:0 ~end_off:0
  in
  diag_error loc msg

let expand_segments_recursive ~(file : string option) ~(raw_text : string)
    ~(object_macros : define_macro list)
    ~(function_macros : define_macro list) (segments : working_segment list) :
    working_segment list * T.Diagnostic.t list =
  let diag_seen = Hashtbl.create 32 in
  let rec pass n current diags =
    if n >= max_macro_expand_passes then
      ( current,
        expansion_limit_diag ~file ~raw_text current
          (Printf.sprintf
             "DEFINE expansion stopped after %d passes; possible recursive macro."
             max_macro_expand_passes)
        :: diags )
    else
      let next, changed, pass_diags =
        expand_segments_once ~file ~raw_text ~diag_seen ~object_macros
          ~function_macros current
      in
      let diags = List.rev_append pass_diags diags in
      if not changed then (current, diags)
      else
        let next_text = working_text next in
        if String.length next_text > max_macro_expand_chars then
          ( next,
            expansion_limit_diag ~file ~raw_text next
              (Printf.sprintf
                 "DEFINE expansion exceeded %d characters and was stopped."
                 max_macro_expand_chars)
            :: diags )
        else pass (n + 1) next diags
  in
  let segments, diags = pass 0 segments [] in
  (segments, List.rev diags)

let merge_spans (spans : (int * int) list) : (int * int) list =
  let spans =
    spans
    |> List.filter_map (fun (a, b) -> if b <= a then None else Some (a, b))
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  let rec loop current acc rest =
    match (current, rest) with
    | None, [] -> List.rev acc
    | Some seg, [] -> List.rev (seg :: acc)
    | None, seg :: tl -> loop (Some seg) acc tl
    | Some (a0, b0), (a1, b1) :: tl ->
        if a1 <= b0 then loop (Some (a0, max b0 b1)) acc tl
        else loop (Some (a1, b1)) ((a0, b0) :: acc) tl
  in
  loop None [] spans

let string_lit_spans_from_tokens (tokens : lex_tok array) : (int * int) list =
  Array.to_list tokens
  |> List.filter_map (fun (span : lex_tok) ->
         match span.tok with
         | Parser.STRINGLIT _ -> Some (span.start_off, span.end_off)
         | _ -> None)

let span_mem spans (a, b) = List.exists (fun (x, y) -> x = a && y = b) spans

let dquote_comment_spans ~(text : string) ~(string_spans : (int * int) list) :
    (int * int) list =
  let n = String.length text in
  let rec find_close i =
    if i >= n then n
    else if text.[i] = '"' then
      if i + 1 < n && text.[i + 1] = '"' then find_close (i + 2) else i + 1
    else find_close (i + 1)
  in
  let rec loop i acc =
    if i >= n then List.rev acc
    else if text.[i] = '"' then
      let close = find_close (i + 1) in
      let acc =
        if span_mem string_spans (i, close) then acc else (i, close) :: acc
      in
      loop close acc
    else loop (i + 1) acc
  in
  loop 0 []

let pct_comment_spans ~(text : string) : (int * int) list =
  let n = String.length text in
  let rec find_close i =
    if i >= n then n else if text.[i] = '%' then i + 1 else find_close (i + 1)
  in
  let rec loop i acc =
    if i >= n then List.rev acc
    else if text.[i] = '%' then
      let close = find_close (i + 1) in
      loop close ((i, close) :: acc)
    else loop (i + 1) acc
  in
  loop 0 []

let protected_spans ~(text : string) ~(tokens : lex_tok array)
    ~(define_spans : (int * int) list) : (int * int) list =
  let string_spans = string_lit_spans_from_tokens tokens in
  merge_spans
    (define_spans @ string_spans
    @ dquote_comment_spans ~text ~string_spans
    @ pct_comment_spans ~text)

let source_map_of_segments (segments : working_segment list) :
    expansion_segment list =
  let cursor = ref 0 in
  List.filter_map
    (fun seg ->
      let len = String.length seg.ws_text in
      let start_off = !cursor in
      let end_off = start_off + len in
      cursor := end_off;
      if len = 0 then None
      else
        Some
          {
            generated_start_off = start_off;
            generated_end_off = end_off;
            origin = seg.ws_origin;
          })
    segments

let original_segment ~(file : string option) ~(text : string) ~(start_off : int)
    ~(end_off : int) : working_segment =
  {
    ws_text = String.sub text start_off (end_off - start_off);
    ws_origin =
      Original (source_span_of_offsets ~file ~text ~start_off ~end_off);
  }

let expand_text_with_macros ~(file : string option) ~(text : string)
    ~(tokens : lex_tok array) ~(macros : define_macro list)
    ~(define_spans : (int * int) list) :
    string * expansion_segment list * T.Diagnostic.t list =
  let full_original () =
    let seg = original_segment ~file ~text ~start_off:0 ~end_off:(String.length text) in
    (text, source_map_of_segments [ seg ], [])
  in
  if macros = [] then full_original ()
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
    let spans = protected_spans ~text ~tokens ~define_spans in
    let n = String.length text in
    let clamp x = if x < 0 then 0 else if x > n then n else x in
    let cursor = ref 0 in
    let segments_rev = ref [] in
    let diags_rev = ref [] in
    let add_original a b =
      if b > a then
        segments_rev := original_segment ~file ~text ~start_off:a ~end_off:b :: !segments_rev
    in
    let add_expanded a b =
      if b > a then
        let original = original_segment ~file ~text ~start_off:a ~end_off:b in
        let expanded, diags =
          expand_segments_recursive ~file ~raw_text:text ~object_macros
            ~function_macros [ original ]
        in
        segments_rev := List.rev_append expanded !segments_rev;
        diags_rev := List.rev_append diags !diags_rev
    in
    List.iter
      (fun (a_raw, b_raw) ->
        let a = clamp a_raw in
        let b = clamp b_raw in
        if a > !cursor then add_expanded !cursor a;
        add_original a b;
        cursor := max !cursor b)
      spans;
    if !cursor < n then add_expanded !cursor n;
    let segments = List.rev !segments_rev in
    (working_text segments, source_map_of_segments segments, List.rev !diags_rev)

let scan_compool_def ~(text : string) : string option =
  let _, compool_hit = scan_from_tokens ~file:None ~text in
  match compool_hit with
  | None -> None
  | Some (nm, _, _, _) ->
      let k = normalize_compool_name nm in
      if k = "" then None else Some k

let run_from_tokens ~(file : string option) ~(text : string)
    ~(tokens : lex_tok array) : result * bool =
  let all_macros, define_spans = collect_define_macros ~file tokens in
  let macros = latest_define_macros all_macros in
  let raw_text = text in
  let text, source_map, expansion_diags =
    expand_text_with_macros ~file ~text:raw_text ~tokens ~macros
      ~define_spans
  in
  let expanded_changed = text <> raw_text in
  let imports, compool_hit =
    if expanded_changed then scan_from_tokens ~file ~text
    else scan_from_token_array ~file tokens
  in
  let compool_def =
    match compool_hit with
    | None -> None
    | Some (nm, _, _, _) ->
        let k = normalize_compool_name nm in
        if k = "" then None else Some k
  in

  let mismatch_diags =
    match (file, compool_hit) with
    | Some path, Some (def_raw, line_no, c0, c1) ->
        let expected = normalize_compool_name (Filename.basename path) in
        let found = normalize_compool_name def_raw in
        if expected <> "" && found <> "" && expected <> found then
          let sp : Ast.Loc.pos = { line = line_no; col = c0; offset = 0 } in
          let ep : Ast.Loc.pos = { line = line_no; col = c1; offset = 0 } in
          let loc = Ast.Loc.make ~file:(Some path) ~start_pos:sp ~end_pos:ep in
          [
            diag_error loc
              (Printf.sprintf
                 "COMPOOL name %S must match file name %S (extension omitted)."
                 found expected);
          ]
        else []
    | _ -> []
  in

  let defines =
    List.map
      (fun m ->
        {
          name = m.name;
          key = m.key;
          formals = m.formals;
          requires_call = m.requires_call;
          body = m.body;
          loc = m.loc;
          decl_start_off = m.start_off;
        })
      all_macros
  in
  ( {
      text;
      imports;
      compool_def;
      defines;
      source_map;
      diags = mismatch_diags @ expansion_diags;
    },
    expanded_changed )

let run ~(file : string option) ~(text : string) : result =
  let tokens = lex_all_tokens ~file ~text in
  run_from_tokens ~file ~text ~tokens |> fst
