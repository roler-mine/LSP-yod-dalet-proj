module T = Lsp.Types
open Ast
open Workspace_foundation
open Workspace_state
open Workspace_imports
open Workspace_semantics

let lsp_pos_of_lex (p : Lexing.position) : Yojson.Safe.t =
  let line0 = max 0 (p.pos_lnum - 1) in
  let col0 = max 0 (p.pos_cnum - p.pos_bol) in
  `Assoc [ ("line", `Int line0); ("character", `Int col0) ]

let lsp_range_of_lex (sp : Lexing.position) (ep : Lexing.position) :
    Yojson.Safe.t =
  `Assoc [ ("start", lsp_pos_of_lex sp); ("end", lsp_pos_of_lex ep) ]

type def = {
  uri : T.DocumentUri.t;
  name : string;
  key : string;
  loc : Ast.Loc.t;
  kind : int;
  container : string option;
}

let def_key (d : def) : string =
  Printf.sprintf "%s|%d|%d|%d|%d|%s"
    (Uri_path.docuri_to_string d.uri)
    d.loc.Ast.Loc.start_pos.line d.loc.Ast.Loc.start_pos.col
    d.loc.Ast.Loc.end_pos.line d.loc.Ast.Loc.end_pos.col d.key

let loc_key ~(uri : T.DocumentUri.t) (loc : Ast.Loc.t) : string =
  Printf.sprintf "%s|%d|%d|%d|%d"
    (Uri_path.docuri_to_string uri)
    loc.Ast.Loc.start_pos.line loc.Ast.Loc.start_pos.col
    loc.Ast.Loc.end_pos.line loc.Ast.Loc.end_pos.col

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' | '\'' -> true
  | _ -> false

let ast_pos_of_offset (idx : Text_index.t) (off : int) : Ast.Loc.pos =
  let line0, col0 = Text_index.line_col_of_offset idx off in
  { Ast.Loc.line = line0 + 1; col = col0; offset = off }

let loc_of_offsets ~(file : string option) ~(idx : Text_index.t) ~(s : int)
    ~(e : int) : Ast.Loc.t =
  Ast.Loc.make ~file ~start_pos:(ast_pos_of_offset idx s)
    ~end_pos:(ast_pos_of_offset idx e)

let add_def_raw (acc : def list) ~(uri : T.DocumentUri.t) ~(name : string)
    ~(loc : Ast.Loc.t) ~(kind : int) ~(container : string option) : def list =
  let key = normalize_name name in
  if key = "" then acc else { uri; name; key; loc; kind; container } :: acc

let add_ident_def (acc : def list) ~(uri : T.DocumentUri.t) ~(id : Ast.ident)
    ~(kind : int) ~(container : string option) : def list =
  add_def_raw acc ~uri ~name:id.v ~loc:id.loc ~kind ~container

let sym_kind_module = 2
let sym_kind_type = 5
let sym_kind_field = 8
let sym_kind_func = 12
let sym_kind_var = 13
let sym_kind_const = 14

let def_of_preprocess_define (doc : Document.t) (d : Preprocess.define) : def =
  {
    uri = doc.Document.uri;
    name = d.name;
    key = d.key;
    loc = d.loc;
    kind = sym_kind_const;
    container = None;
  }

let nth_opt (xs : 'a list) (n : int) : 'a option =
  let rec go i = function
    | [] -> None
    | x :: tl -> if i = 0 then Some x else go (i - 1) tl
  in
  if n < 0 then None else go n xs

let tokenize_ident_words (line : string) : (string * int * int) list =
  let n = String.length line in
  let rec scan i acc =
    if i >= n then List.rev acc
    else if is_ident_char line.[i] then (
      let j = ref (i + 1) in
      while !j < n && is_ident_char line.[!j] do
        incr j
      done;
      let tok = String.sub line i (!j - i) in
      scan !j ((tok, i, !j) :: acc))
    else scan (i + 1) acc
  in
  scan 0 []

let preceded_by_bang (line : string) (col : int) : bool =
  col > 0 && line.[col - 1] = '!'

let classify_fallback_decl ~(line : string) (tokens : (string * int * int) list)
    : (int * int) option =
  let classify kw_idx kw kw_col =
    match kw with
    | "ITEM" | "TABLE" -> Some (kw_idx, sym_kind_var)
    | "TYPE" -> Some (kw_idx, sym_kind_type)
    | "PROC" -> Some (kw_idx, sym_kind_func)
    | "DEFINE" -> Some (kw_idx, sym_kind_const)
    | "BLOCK" -> Some (kw_idx, sym_kind_module)
    | "COMPOOL" ->
        if preceded_by_bang line kw_col then None
        else Some (kw_idx, sym_kind_module)
    | _ -> None
  in
  let token_upper i =
    match nth_opt tokens i with
    | None -> None
    | Some (w, col, _) -> Some (normalize_name w, col)
  in
  match token_upper 0 with
  | None -> None
  | Some (kw0, col0) -> (
      match classify 0 kw0 col0 with
      | Some _ as hit -> hit
      | None ->
          if kw0 = "DEF" || kw0 = "REF" || kw0 = "STATIC" || kw0 = "CONSTANT"
          then
            match token_upper 1 with
            | None -> None
            | Some (kw1, col1) -> classify 1 kw1 col1
          else None)

let fallback_line_defs (doc : Document.t) : def list =
  let uri = doc.Document.uri in
  let idx = doc.Document.index in
  let file = doc.Document.file in
  let lines = String.split_on_char '\n' doc.Document.text in
  let defs_rev =
    lines
    |> List.mapi (fun line0 line -> (line0, line))
    |> List.fold_left
         (fun acc (line0, line) ->
           let tokens = tokenize_ident_words line in
           match classify_fallback_decl ~line tokens with
           | None -> acc
           | Some (kw_idx, kind) -> (
               match
                 ( nth_opt tokens (kw_idx + 1),
                   Text_index.line_start_offset idx ~line:line0 )
               with
               | Some (name, c0, c1), Some base ->
                   let s = base + c0 in
                   let e = base + c1 in
                   let loc = loc_of_offsets ~file ~idx ~s ~e in
                   add_def_raw acc ~uri ~name ~loc ~kind ~container:None
               | _ -> acc))
         []
  in
  List.rev defs_rev

let max_fallback_scan_lines = 75_000

let rec collect_type_defs ~(uri : T.DocumentUri.t) ~(container : string option)
    (acc : def list) (t : Ast.type_expr Ast.node) : def list =
  match t.v with
  | Ast.TName _ -> acc
  | Ast.TPointer inner -> collect_type_defs ~uri ~container acc inner
  | Ast.TArray { elem; _ } -> collect_type_defs ~uri ~container acc elem
  | Ast.TRecord fields ->
      List.fold_left
        (fun a f ->
          let fv = f.v in
          let a =
            add_ident_def a ~uri ~id:fv.fname ~kind:sym_kind_field ~container
          in
          collect_type_defs ~uri ~container a fv.ftype)
        acc fields
  | Ast.TFunc { params; returns } -> (
      let acc =
        List.fold_left
          (fun a p ->
            let pv = p.v in
            let a =
              add_ident_def a ~uri ~id:pv.pname ~kind:sym_kind_var ~container
            in
            collect_type_defs ~uri ~container a pv.ptype)
          acc params
      in
      match returns with
      | None -> acc
      | Some r -> collect_type_defs ~uri ~container acc r)

let rec collect_stmt_defs ~(uri : T.DocumentUri.t) ~(container : string option)
    (acc : def list) (s : Ast.stmt Ast.node) : def list =
  match s.v with
  | Ast.SEmpty -> acc
  | Ast.SDecl d -> collect_decl_defs ~uri ~container acc d
  | Ast.SBlock xs -> List.fold_left (collect_stmt_defs ~uri ~container) acc xs
  | Ast.SAssign _ -> acc
  | Ast.SCallStmt _ -> acc
  | Ast.SIf { then_; else_; _ } -> (
      let acc = collect_stmt_defs ~uri ~container acc then_ in
      match else_ with
      | None -> acc
      | Some e -> collect_stmt_defs ~uri ~container acc e)
  | Ast.SWhile { body; _ } -> collect_stmt_defs ~uri ~container acc body
  | Ast.SFor { init; step; body; _ } ->
      let acc =
        match init with
        | None -> acc
        | Some i -> collect_stmt_defs ~uri ~container acc i
      in
      let acc =
        match step with
        | None -> acc
        | Some st -> collect_stmt_defs ~uri ~container acc st
      in
      collect_stmt_defs ~uri ~container acc body
  | Ast.SReturn _ -> acc
  | Ast.SLabel { label; body } ->
      let acc =
        add_ident_def acc ~uri ~id:label ~kind:sym_kind_var ~container
      in
      collect_stmt_defs ~uri ~container acc body
  | Ast.SGoto _ -> acc

and collect_decl_defs ~(uri : T.DocumentUri.t) ~(container : string option)
    (acc : def list) (d : Ast.decl Ast.node) : def list =
  match d.v with
  | Ast.DVar { name; dtype; _ } ->
      let acc = add_ident_def acc ~uri ~id:name ~kind:sym_kind_var ~container in
      collect_type_defs ~uri ~container acc dtype
  | Ast.DConst { name; dtype; _ } -> (
      let acc =
        add_ident_def acc ~uri ~id:name ~kind:sym_kind_const ~container
      in
      match dtype with
      | None -> acc
      | Some ty -> collect_type_defs ~uri ~container acc ty)
  | Ast.DType { name; defn } ->
      let acc =
        add_ident_def acc ~uri ~id:name ~kind:sym_kind_type ~container
      in
      collect_type_defs ~uri ~container:(Some name.v) acc defn
  | Ast.DProc p ->
      let pv = p.v in
      let proc_name = pv.name.v in
      let acc =
        add_ident_def acc ~uri ~id:pv.name ~kind:sym_kind_func ~container
      in
      let in_proc = Some proc_name in
      let acc =
        List.fold_left
          (fun a prm ->
            let prm_v = prm.v in
            let a =
              add_ident_def a ~uri ~id:prm_v.pname ~kind:sym_kind_var
                ~container:in_proc
            in
            collect_type_defs ~uri ~container:in_proc a prm_v.ptype)
          acc pv.params
      in
      let acc =
        List.fold_left (collect_decl_defs ~uri ~container:in_proc) acc pv.locals
      in
      collect_stmt_defs ~uri ~container:in_proc acc pv.body
  | Ast.DDirective _ -> acc

let find_compool_loc_in_doc (doc : Document.t) (key : string) : Ast.Loc.t option
    =
  let match_directive (d : Ast.decl Ast.node) =
    match d.v with
    | Ast.DDirective { name; args = first :: _ } ->
        let dir = normalize_name name.v in
        if (dir = "COMPOOL" || dir = "ICOMPOOL") && normalize_name first.v = key
        then Some first.loc
        else None
    | _ -> None
  in
  match doc.Document.ast with
  | None -> None
  | Some prog ->
      let rec go = function
        | [] -> None
        | Ast.TopDecl d :: tl -> (
            match match_directive d with Some _ as hit -> hit | None -> go tl)
        | _ :: tl -> go tl
      in
      go prog

let collect_doc_defs (doc : Document.t) : def list =
  let uri = doc.Document.uri in
  let defs0 =
    match doc.Document.ast with
    | None -> []
    | Some prog ->
        List.fold_left
          (fun acc top ->
            match top with
            | Ast.TopDecl d -> collect_decl_defs ~uri ~container:None acc d
            | Ast.TopStmt s -> collect_stmt_defs ~uri ~container:None acc s)
          [] prog
  in
  let defs =
    List.fold_left
      (fun acc (dm : Preprocess.define) ->
        add_def_raw acc ~uri ~name:dm.name ~loc:dm.loc ~kind:sym_kind_const
          ~container:None)
      defs0 doc.Document.defines
  in
  let use_fallback_scan =
    let broken_or_partial =
      doc.Document.ast = None || doc.Document.parse_diags <> []
    in
    broken_or_partial
    && Text_index.line_count doc.Document.index <= max_fallback_scan_lines
  in
  let defs =
    if use_fallback_scan then
      fallback_line_defs doc |> List.fold_left (fun acc d -> d :: acc) defs
    else defs
  in
  match doc.Document.compool_def with
  | None -> List.rev defs
  | Some name ->
      let k = normalize_name name in
      let defs =
        match find_compool_loc_in_doc doc k with
        | None -> defs
        | Some loc ->
            add_def_raw defs ~uri ~name:k ~loc ~kind:sym_kind_module
              ~container:None
      in
      List.rev defs

let position_in_loc (pos : T.Position.t) (loc : Ast.Loc.t) : bool =
  let line = pos.T.Position.line + 1 in
  let col = pos.T.Position.character in
  let sp = loc.Ast.Loc.start_pos in
  let ep = loc.Ast.Loc.end_pos in
  (line > sp.line || (line = sp.line && col >= sp.col))
  && (line < ep.line || (line = ep.line && col <= ep.col))

let word_at_position (doc : Document.t) (pos : T.Position.t) :
    (string * Ast.Loc.t) option =
  match
    Text_index.offset_of_line_col doc.Document.index ~line:pos.T.Position.line
      ~col:pos.T.Position.character
  with
  | None -> None
  | Some off -> (
      let text = doc.Document.text in
      let n = String.length text in
      if n = 0 then None
      else
        let pivot =
          if off < n && is_ident_char text.[off] then Some off
          else if off > 0 && off - 1 < n && is_ident_char text.[off - 1] then
            Some (off - 1)
          else None
        in
        match pivot with
        | None -> None
        | Some i ->
            let a = ref i in
            while !a > 0 && is_ident_char text.[!a - 1] do
              decr a
            done;
            let b = ref (i + 1) in
            while !b < n && is_ident_char text.[!b] do
              incr b
            done;
            if !b <= !a then None
            else
              let name = String.sub text !a (!b - !a) in
              let loc =
                loc_of_offsets ~file:doc.Document.file ~idx:doc.Document.index
                  ~s:!a ~e:!b
              in
              Some (name, loc))

let nav_word_at_position (doc : Document.t) (pos : T.Position.t) :
    (string * Ast.Loc.t) option =
  match word_at_position doc pos with
  | None -> None
  | Some (name, _) when is_reserved_keyword name -> None
  | Some x -> Some x

let has_define_key (doc : Document.t) (key : string) : bool =
  key <> ""
  && List.exists
       (fun (d : Preprocess.define) -> d.key = key)
       doc.Document.defines

let find_define_key_in_word (doc : Document.t) ~(word : string)
    ~(word_loc : Ast.Loc.t) ~(cursor_col : int) : string option =
  let direct = normalize_name word in
  if has_define_key doc direct then Some direct
  else
    let upper_word = String.uppercase_ascii word in
    let n = String.length upper_word in
    if n = 0 then None
    else
      let rel =
        let r = cursor_col - word_loc.start_pos.col in
        if r < 0 then 0 else if r >= n then n - 1 else r
      in
      let uniq_keys_tbl = Hashtbl.create 16 in
      List.iter
        (fun (d : Preprocess.define) ->
          Hashtbl.replace uniq_keys_tbl d.key true)
        doc.Document.defines;
      let keys = Hashtbl.fold (fun k _ acc -> k :: acc) uniq_keys_tbl [] in
      let best : (string * int * int) option ref = ref None in
      let consider key start_idx len =
        match !best with
        | None -> best := Some (key, len, start_idx)
        | Some (_, best_len, best_start) ->
            if len > best_len || (len = best_len && start_idx >= best_start)
            then best := Some (key, len, start_idx)
      in
      List.iter
        (fun key ->
          let m = String.length key in
          if m > 0 && m <= n then
            let rec scan i =
              if i + m > n then ()
              else
                let rec eq j =
                  j = m || (upper_word.[i + j] = key.[j] && eq (j + 1))
                in
                if eq 0 && rel >= i && rel < i + m then consider key i m;
                scan (i + 1)
            in
            scan 0)
        keys;
      match !best with None -> None | Some (key, _, _) -> Some key

let is_ws_char = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let skip_ws_forward (s : string) (i : int) : int =
  let n = String.length s in
  let rec go j = if j < n && is_ws_char s.[j] then go (j + 1) else j in
  go i

let parse_call_arg_count ~(text : string) ~(open_idx : int) : int option =
  let n = String.length text in
  if open_idx < 0 || open_idx >= n || text.[open_idx] <> '(' then None
  else
    let depth = ref 1 in
    let in_single = ref false in
    let in_double = ref false in
    let comma_count = ref 0 in
    let seen_non_ws = ref false in
    let i = ref (open_idx + 1) in
    let done_ = ref false in
    while (not !done_) && !i < n do
      let c = text.[!i] in
      (if !in_single then (
         if c = '\'' then
           if !i + 1 < n && text.[!i + 1] = '\'' then i := !i + 1
           else in_single := false)
       else if !in_double then (
         if c = '"' then
           if !i + 1 < n && text.[!i + 1] = '"' then i := !i + 1
           else in_double := false)
       else
         match c with
         | '\'' ->
             if !depth = 1 then seen_non_ws := true;
             in_single := true
         | '"' ->
             if !depth = 1 then seen_non_ws := true;
             in_double := true
         | '(' ->
             if !depth = 1 then seen_non_ws := true;
             incr depth
         | ')' ->
             decr depth;
             if !depth = 0 then done_ := true
         | ',' when !depth = 1 -> incr comma_count
         | _ -> if !depth = 1 && not (is_ws_char c) then seen_non_ws := true);
      incr i
    done;
    if !depth <> 0 then None
    else if not !seen_non_ws then Some 0
    else Some (!comma_count + 1)

let select_define_decl (doc : Document.t) ~(key : string) ~(call_ctx : bool)
    ~(arg_count : int option) ~(cursor_off : int option) :
    Preprocess.define option =
  let defs0 =
    doc.Document.defines
    |> List.filter (fun (d : Preprocess.define) -> d.key = key)
  in
  let defs1 =
    match cursor_off with
    | None -> defs0
    | Some off ->
        let before =
          defs0
          |> List.filter (fun (d : Preprocess.define) ->
              d.decl_start_off <= off)
        in
        if before = [] then defs0 else before
  in
  let defs2 =
    let same_call =
      defs1
      |> List.filter (fun (d : Preprocess.define) -> d.requires_call = call_ctx)
    in
    if same_call = [] then defs1 else same_call
  in
  let defs3 =
    match arg_count with
    | None -> defs2
    | Some n ->
        let same_arity =
          defs2
          |> List.filter (fun (d : Preprocess.define) ->
              List.length d.formals = n)
        in
        if same_arity = [] then defs2 else same_arity
  in
  defs3
  |> List.fold_left
       (fun best (d : Preprocess.define) ->
         match best with
         | None -> Some d
         | Some (cur : Preprocess.define) ->
             if d.decl_start_off >= cur.decl_start_off then Some d else Some cur)
       None

let define_under_cursor (doc : Document.t) (pos : T.Position.t) :
    (Preprocess.define * Ast.Loc.t) option =
  match word_at_position doc pos with
  | None -> None
  | Some (word, word_loc) -> (
      let key_opt =
        find_define_key_in_word doc ~word ~word_loc ~cursor_col:pos.character
      in
      match key_opt with
      | None -> None
      | Some key -> (
          let cursor_off =
            Text_index.offset_of_line_col doc.Document.index ~line:pos.line
              ~col:pos.character
          in
          let end_line0 = max 0 (word_loc.end_pos.line - 1) in
          let after_word_off =
            Text_index.offset_of_line_col doc.Document.index ~line:end_line0
              ~col:word_loc.end_pos.col
          in
          let call_ctx, arg_count =
            match after_word_off with
            | None -> (false, None)
            | Some off ->
                let open_idx = skip_ws_forward doc.Document.text off in
                if
                  open_idx < String.length doc.Document.text
                  && doc.Document.text.[open_idx] = '('
                then
                  let argc =
                    parse_call_arg_count ~text:doc.Document.text ~open_idx
                  in
                  (true, argc)
                else (false, None)
          in
          match
            select_define_decl doc ~key ~call_ctx ~arg_count ~cursor_off
          with
          | None -> None
          | Some d -> Some (d, word_loc)))

let location_json ~(uri : T.DocumentUri.t) (loc : Ast.Loc.t) : Yojson.Safe.t =
  T.Location.yojson_of_t (Lsp_conv.location_of_loc ~uri loc)

let range_json_of_loc (loc : Ast.Loc.t) : Yojson.Safe.t =
  T.Range.yojson_of_t (Lsp_conv.range_of_loc loc)

let symbol_kind_of_def_kind (k : int) : T.SymbolKind.t =
  if k = sym_kind_module then T.SymbolKind.Module
  else if k = sym_kind_type then T.SymbolKind.Class
  else if k = sym_kind_field then T.SymbolKind.Field
  else if k = sym_kind_func then T.SymbolKind.Function
  else if k = sym_kind_const then T.SymbolKind.Constant
  else T.SymbolKind.Variable

let completion_item_kind_of_def_kind (k : int) : T.CompletionItemKind.t =
  if k = sym_kind_module then T.CompletionItemKind.Module
  else if k = sym_kind_type then T.CompletionItemKind.Class
  else if k = sym_kind_field then T.CompletionItemKind.Property
  else if k = sym_kind_func then T.CompletionItemKind.Function
  else if k = sym_kind_const then T.CompletionItemKind.Constant
  else T.CompletionItemKind.Variable

let location_of_def (d : def) : T.Location.t =
  Lsp_conv.location_of_loc ~uri:d.uri d.loc

let symbol_info_of_def (d : def) : T.SymbolInformation.t =
  T.SymbolInformation.create ?containerName:d.container
    ~kind:(symbol_kind_of_def_kind d.kind)
    ~location:(location_of_def d) ~name:d.name ()

let symbol_json (d : def) : Yojson.Safe.t =
  let base =
    [
      ("name", `String d.name);
      ("kind", `Int d.kind);
      ("location", location_json ~uri:d.uri d.loc);
    ]
  in
  let base =
    match d.container with
    | None -> base
    | Some c -> ("containerName", `String c) :: base
  in
  `Assoc base

let doc_at_path (ws : t) (path : string) : Document.t option =
  doc_from_path_cached ws path

let doc_at_path_cached (ws : t) (path : string) : Document.t option =
  doc_from_path_cached_only ws path

let resolve_import_paths (ws : t) (doc : Document.t) : string list =
  match ws.index with
  | None -> []
  | Some idx ->
      let acc = Hashtbl.create 16 in
      List.iter
        (fun (imp : Preprocess.import) ->
          match imp.kind with
          | Preprocess.Compool -> (
              match Workspace_index.find_compool idx ~name:imp.name with
              | None -> ()
              | Some p -> Hashtbl.replace acc (normalize_path_key p) p))
        (Document.imports doc);
      Hashtbl.fold (fun _ p xs -> p :: xs) acc []

let docs_for_lookup (ws : t) (doc : Document.t) : Document.t list =
  let seen = Hashtbl.create 32 in
  let out = ref [] in
  let add_doc (d : Document.t) =
    let u = Uri_path.docuri_to_string d.Document.uri in
    if not (Hashtbl.mem seen u) then (
      Hashtbl.add seen u true;
      out := d :: !out)
  in
  add_doc doc;
  resolve_import_paths ws doc
  |> List.iter (fun p ->
      match doc_at_path_cached ws p with
      | None ->
          enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"lookup_import"
            ~high:true p
      | Some d -> add_doc d);
  List.rev !out

let has_unscoped_fallback_context (doc : Document.t) : bool =
  doc.Document.ast = None
  || doc.Document.parse_diags <> []
  || Document.imports doc = []

let docs_for_rename (ws : t) (doc : Document.t) : Document.t list =
  let seen = Hashtbl.create 64 in
  let out = ref [] in
  let add_doc (d : Document.t) =
    let u = Uri_path.docuri_to_string d.Document.uri in
    if not (Hashtbl.mem seen u) then (
      Hashtbl.add seen u true;
      out := d :: !out)
  in
  docs_for_lookup ws doc |> List.iter add_doc;
  if has_unscoped_fallback_context doc then
    Hashtbl.iter (fun _ d -> add_doc d) ws.files;
  Hashtbl.iter (fun _ d -> add_doc d) ws.docs;
  List.rev !out

let compare_pos (a : T.Position.t) (b : T.Position.t) : int =
  if a.line < b.line then -1
  else if a.line > b.line then 1
  else if a.character < b.character then -1
  else if a.character > b.character then 1
  else 0

let pos_in_range (p : T.Position.t) (r : T.Range.t) : bool =
  compare_pos r.start p <= 0 && compare_pos p r.end_ <= 0

let kind_name (k : int) : string =
  if k = sym_kind_module then "module"
  else if k = sym_kind_type then "type"
  else if k = sym_kind_field then "field"
  else if k = sym_kind_func then "procedure"
  else if k = sym_kind_var then "item"
  else if k = sym_kind_const then "constant"
  else "symbol"

type nav_binding = { sym_id : string; decl : def }

type nav_scope = {
  values : (string, nav_binding) Hashtbl.t;
  types : (string, nav_binding) Hashtbl.t;
  labels : (string, nav_binding) Hashtbl.t;
  fields : (string, nav_binding) Hashtbl.t;
}

type doc_nav = {
  defs_by_id : (string, def) Hashtbl.t;
  occs_by_id : (string, (T.DocumentUri.t * Ast.Loc.t) list) Hashtbl.t;
  seen_occ : (string, bool) Hashtbl.t;
}

let nav_scope_empty () : nav_scope =
  {
    values = Hashtbl.create 64;
    types = Hashtbl.create 32;
    labels = Hashtbl.create 32;
    fields = Hashtbl.create 64;
  }

let nav_scope_copy (s : nav_scope) : nav_scope =
  {
    values = copy_tbl s.values;
    types = copy_tbl s.types;
    labels = copy_tbl s.labels;
    fields = copy_tbl s.fields;
  }

let doc_nav_create () : doc_nav =
  {
    defs_by_id = Hashtbl.create 128;
    occs_by_id = Hashtbl.create 256;
    seen_occ = Hashtbl.create 512;
  }

let def_symbol_id (d : def) : string = def_key d

let snapshot_def_of_def ~(sym_id : string) (d : def) : Doc_snapshot.nav_def =
  {
    Doc_snapshot.sym_id;
    uri = d.uri;
    name = d.name;
    key = d.key;
    loc = d.loc;
    kind = d.kind;
    container = d.container;
  }

let def_of_snapshot_def (d : Doc_snapshot.nav_def) : def =
  {
    uri = d.uri;
    name = d.name;
    key = d.key;
    loc = d.loc;
    kind = d.kind;
    container = d.container;
  }

let doc_nav_of_snapshot (snap : Doc_snapshot.t) : doc_nav =
  let nav = doc_nav_create () in
  List.iter
    (fun (sym_id, d) ->
      Hashtbl.replace nav.defs_by_id sym_id (def_of_snapshot_def d))
    snap.nav_defs;
  List.iter
    (fun (sym_id, occs) ->
      Hashtbl.replace nav.occs_by_id sym_id occs;
      List.iter
        (fun (u, loc) ->
          let k = Printf.sprintf "%s|%s" sym_id (loc_key ~uri:u loc) in
          Hashtbl.replace nav.seen_occ k true)
        occs)
    snap.nav_occs;
  nav

let snapshot_for_doc (_ws : t) (doc : Document.t) (nav : doc_nav) :
    Doc_snapshot.t =
  let path_key =
    match doc.Document.file with
    | None -> None
    | Some p -> Some (normalize_path_key p)
  in
  let nav_defs =
    Hashtbl.fold
      (fun sym_id d acc -> (sym_id, snapshot_def_of_def ~sym_id d) :: acc)
      nav.defs_by_id []
  in
  let nav_occs =
    Hashtbl.fold
      (fun sym_id occs acc -> (sym_id, occs) :: acc)
      nav.occs_by_id []
  in
  let symbol_keys_touched =
    nav_defs
    |> List.filter_map (fun (_, d) ->
        let k = normalize_name d.Doc_snapshot.key in
        if k = "" then None else Some k)
    |> List.sort_uniq String.compare
  in
  Doc_snapshot.build ~uri:doc.Document.uri ~path_key
    ~doc_rev:doc.Document.parse_rev ~text:doc.Document.text
    ~imports:doc.Document.imports ~defines:doc.Document.defines
    ~compool_def:doc.Document.compool_def ~nav_defs ~nav_occs ~proc_param_map:[]
    ~symbol_keys_touched

let upsert_semantic_snapshot_for_doc_with_nav (ws : t) (doc : Document.t)
    (nav : doc_nav) : unit =
  if ws.sem_store_enabled then
    Perf_stats.time "snapshot.build" (fun () ->
        let snap = snapshot_for_doc ws doc nav in
        Semantic_store.upsert_snapshot ws.semantic_store snap)

let nav_add_occurrence (nav : doc_nav) ~(sym_id : string)
    ~(uri : T.DocumentUri.t) ~(loc : Ast.Loc.t) : unit =
  let k = Printf.sprintf "%s|%s" sym_id (loc_key ~uri loc) in
  if not (Hashtbl.mem nav.seen_occ k) then (
    Hashtbl.replace nav.seen_occ k true;
    let prev =
      match Hashtbl.find_opt nav.occs_by_id sym_id with
      | None -> []
      | Some xs -> xs
    in
    Hashtbl.replace nav.occs_by_id sym_id ((uri, loc) :: prev))

let nav_add_decl (nav : doc_nav) (d : def) : nav_binding option =
  if d.key = "" then None
  else
    let sym_id = def_symbol_id d in
    Hashtbl.replace nav.defs_by_id sym_id d;
    nav_add_occurrence nav ~sym_id ~uri:d.uri ~loc:d.loc;
    Some { sym_id; decl = d }

let nav_bind_value (scope : nav_scope) (b : nav_binding) : unit =
  Hashtbl.replace scope.values b.decl.key b

let nav_bind_type (scope : nav_scope) (b : nav_binding) : unit =
  Hashtbl.replace scope.types b.decl.key b

let nav_bind_label (scope : nav_scope) (b : nav_binding) : unit =
  Hashtbl.replace scope.labels b.decl.key b

let nav_bind_field (scope : nav_scope) (b : nav_binding) : unit =
  Hashtbl.replace scope.fields b.decl.key b

let nav_bind_decl_default (scope : nav_scope) (b : nav_binding) : unit =
  if b.decl.kind = sym_kind_type then nav_bind_type scope b
  else if b.decl.kind = sym_kind_field then nav_bind_field scope b
  else nav_bind_value scope b

let nav_find_value (scope : nav_scope) (name : string) : nav_binding option =
  Hashtbl.find_opt scope.values (normalize_name name)

let nav_find_type (scope : nav_scope) (name : string) : nav_binding option =
  let k = normalize_name name in
  match Hashtbl.find_opt scope.types k with
  | Some _ as x -> x
  | None -> Hashtbl.find_opt scope.values k

let nav_find_label (scope : nav_scope) (name : string) : nav_binding option =
  let k = normalize_name name in
  match Hashtbl.find_opt scope.labels k with
  | Some _ as x -> x
  | None -> Hashtbl.find_opt scope.values k

let nav_find_field (scope : nav_scope) (name : string) : nav_binding option =
  let k = normalize_name name in
  match Hashtbl.find_opt scope.fields k with
  | Some _ as x -> x
  | None -> Hashtbl.find_opt scope.values k

let def_of_ident ~(uri : T.DocumentUri.t) ~(id : Ast.ident) ~(kind : int)
    ~(container : string option) : def option =
  let key = normalize_name id.v in
  if key = "" then None
  else Some { uri; name = id.v; key; loc = id.loc; kind; container }

let uniq_defs (xs : def list) : def list =
  let seen = Hashtbl.create 64 in
  let acc = ref [] in
  List.iter
    (fun d ->
      let k = def_key d in
      if not (Hashtbl.mem seen k) then (
        Hashtbl.add seen k true;
        acc := d :: !acc))
    xs;
  List.rev !acc

let exported_defs_for_import_scope (doc : Document.t) : def list =
  let block_containers =
    match doc.Document.ast with
    | None -> Hashtbl.create 1
    | Some prog -> block_proc_names_of_program prog
  in
  let is_exported (d : def) : bool =
    if d.kind = sym_kind_field then false
    else
      match d.container with
      | None -> true
      | Some c -> Hashtbl.mem block_containers (normalize_name c)
  in
  collect_doc_defs doc |> uniq_defs |> List.filter is_exported

let same_uri (a : T.DocumentUri.t) (b : T.DocumentUri.t) : bool =
  Uri_path.docuri_to_string a = Uri_path.docuri_to_string b

let loc_span_weight (loc : Ast.Loc.t) : int =
  let sp = loc.Ast.Loc.start_pos in
  let ep = loc.Ast.Loc.end_pos in
  let line_span = max 0 (ep.line - sp.line) in
  if line_span = 0 then max 0 (ep.col - sp.col)
  else (line_span * 10000) + max 0 ep.col

let symbol_at_position_in_nav (nav : doc_nav) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : (string * Ast.Loc.t) option =
  let best : (string * Ast.Loc.t * int) option ref = ref None in
  let consider (sym_id : string) (loc : Ast.Loc.t) =
    let span = loc_span_weight loc in
    match !best with
    | None -> best := Some (sym_id, loc, span)
    | Some (_, _, cur) when span < cur -> best := Some (sym_id, loc, span)
    | _ -> ()
  in
  Hashtbl.iter
    (fun sym_id occs ->
      List.iter
        (fun (u, loc) ->
          if same_uri u uri && position_in_loc pos loc then consider sym_id loc)
        occs)
    nav.occs_by_id;
  match !best with None -> None | Some (sym_id, loc, _) -> Some (sym_id, loc)

let build_doc_nav (ws : t) (doc : Document.t) : doc_nav =
  let nav = doc_nav_create () in
  let uri = doc.Document.uri in
  let root_scope = nav_scope_empty () in

  let add_decl_binding (scope : nav_scope) ~(id : Ast.ident) ~(kind : int)
      ~(container : string option) (binder : nav_scope -> nav_binding -> unit) :
      unit =
    match def_of_ident ~uri ~id ~kind ~container with
    | None -> ()
    | Some d -> (
        match nav_add_decl nav d with None -> () | Some b -> binder scope b)
  in

  let add_decl_default scope ~id ~kind ~container =
    add_decl_binding scope ~id ~kind ~container nav_bind_decl_default
  in

  let add_decl_value scope ~id ~kind ~container =
    add_decl_binding scope ~id ~kind ~container nav_bind_value
  in

  let add_decl_type scope ~id ~kind ~container =
    add_decl_binding scope ~id ~kind ~container nav_bind_type
  in

  let add_decl_label scope ~id ~kind ~container =
    add_decl_binding scope ~id ~kind ~container nav_bind_label
  in

  let add_decl_field scope ~id ~kind ~container =
    add_decl_binding scope ~id ~kind ~container nav_bind_field
  in

  let bind_external_def (scope : nav_scope) (d : def) : unit =
    if d.key <> "" then (
      let sym_id = def_symbol_id d in
      Hashtbl.replace nav.defs_by_id sym_id d;
      nav_bind_decl_default scope { sym_id; decl = d })
  in

  let add_usage (b : nav_binding option) (id : Ast.ident) : unit =
    match b with
    | None -> ()
    | Some hit -> nav_add_occurrence nav ~sym_id:hit.sym_id ~uri ~loc:id.loc
  in

  let use_value (scope : nav_scope) (id : Ast.ident) : unit =
    add_usage (nav_find_value scope id.v) id
  in
  let use_type (scope : nav_scope) (id : Ast.ident) : unit =
    add_usage (nav_find_type scope id.v) id
  in
  let use_label (scope : nav_scope) (id : Ast.ident) : unit =
    add_usage (nav_find_label scope id.v) id
  in
  let use_field (scope : nav_scope) (id : Ast.ident) : unit =
    add_usage (nav_find_field scope id.v) id
  in

  let rec prebind_decl (scope : nav_scope) ~(container : string option)
      (d : Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DVar { name; _ } ->
        add_decl_default scope ~id:name ~kind:sym_kind_var ~container
    | Ast.DConst { name; _ } ->
        add_decl_default scope ~id:name ~kind:sym_kind_const ~container
    | Ast.DType { name; _ } ->
        add_decl_type scope ~id:name ~kind:sym_kind_type ~container
    | Ast.DProc p ->
        add_decl_value scope ~id:p.v.name ~kind:sym_kind_func ~container
    | Ast.DDirective _ -> ()
  and prebind_stmt (scope : nav_scope) ~(container : string option)
      (s : Ast.stmt Ast.node) : unit =
    match s.v with
    | Ast.SDecl d -> prebind_decl scope ~container d
    | Ast.SLabel { label; _ } ->
        add_decl_label scope ~id:label ~kind:sym_kind_var ~container
    | _ -> ()
  and walk_type (scope : nav_scope) ~(container : string option)
      (t : Ast.type_expr Ast.node) : unit =
    match t.v with
    | Ast.TName id -> use_type scope id
    | Ast.TPointer inner -> walk_type scope ~container inner
    | Ast.TArray { elem; dims } ->
        walk_type scope ~container elem;
        List.iter (walk_expr scope ~container) dims
    | Ast.TRecord fields ->
        List.iter
          (fun f ->
            let fv = f.v in
            add_decl_field scope ~id:fv.fname ~kind:sym_kind_field ~container;
            walk_type scope ~container fv.ftype)
          fields
    | Ast.TFunc { params; returns } -> (
        let fn_scope = nav_scope_copy scope in
        List.iter
          (fun prm ->
            add_decl_value fn_scope ~id:prm.v.pname ~kind:sym_kind_var
              ~container;
            walk_type fn_scope ~container prm.v.ptype)
          params;
        match returns with
        | None -> ()
        | Some r -> walk_type fn_scope ~container r)
  and walk_expr (scope : nav_scope) ~(container : string option)
      (e : Ast.expr Ast.node) : unit =
    match e.v with
    | Ast.EName id -> use_value scope id
    | Ast.ELit _ -> ()
    | Ast.EUnop { rhs; _ } -> walk_expr scope ~container rhs
    | Ast.EBinop { lhs; rhs; _ } ->
        walk_expr scope ~container lhs;
        walk_expr scope ~container rhs
    | Ast.ECall { callee; args } ->
        use_value scope callee;
        List.iter (walk_expr scope ~container) args
    | Ast.EIndex { base; index } ->
        walk_expr scope ~container base;
        List.iter (walk_expr scope ~container) index
    | Ast.EField { base; field } ->
        walk_expr scope ~container base;
        use_field scope field
    | Ast.EAt { field; ptr } ->
        (match field.v with
        | Ast.EName id -> use_field scope id
        | Ast.EIndex { base; index } ->
            (match base.v with
            | Ast.EName id -> use_field scope id
            | _ -> walk_expr scope ~container base);
            List.iter (walk_expr scope ~container) index
        | _ -> walk_expr scope ~container field);
        walk_expr scope ~container ptr
    | Ast.EDeref { ptr } -> walk_expr scope ~container ptr
    | Ast.EParen x -> walk_expr scope ~container x
  and walk_decl (scope : nav_scope) ~(container : string option)
      (d : Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DVar { dtype; init; _ } -> (
        walk_type scope ~container dtype;
        match init with None -> () | Some e -> walk_expr scope ~container e)
    | Ast.DConst { dtype; value; _ } ->
        (match dtype with None -> () | Some t -> walk_type scope ~container t);
        walk_expr scope ~container value
    | Ast.DType { name; defn } -> walk_type scope ~container:(Some name.v) defn
    | Ast.DProc p ->
        let proc_container = Some p.v.name.v in
        let proc_scope = nav_scope_copy scope in
        if nav_find_value proc_scope p.v.name.v = None then
          add_decl_value proc_scope ~id:p.v.name ~kind:sym_kind_func ~container;
        List.iter
          (fun prm ->
            add_decl_value proc_scope ~id:prm.v.pname ~kind:sym_kind_var
              ~container:proc_container)
          p.v.params;
        List.iter (prebind_decl proc_scope ~container:proc_container) p.v.locals;
        List.iter
          (fun prm ->
            walk_type proc_scope ~container:proc_container prm.v.ptype)
          p.v.params;
        (match p.v.returns with
        | None -> ()
        | Some r -> walk_type proc_scope ~container:proc_container r);
        List.iter (walk_decl proc_scope ~container:proc_container) p.v.locals;
        walk_stmt proc_scope ~container:proc_container p.v.body
    | Ast.DDirective _ -> ()
  and walk_stmt (scope : nav_scope) ~(container : string option)
      (s : Ast.stmt Ast.node) : unit =
    match s.v with
    | Ast.SEmpty -> ()
    | Ast.SDecl d ->
        prebind_decl scope ~container d;
        walk_decl scope ~container d
    | Ast.SBlock xs ->
        let block_scope = nav_scope_copy scope in
        walk_stmt_list block_scope ~container xs
    | Ast.SAssign { lhs; rhs } ->
        walk_expr scope ~container lhs;
        walk_expr scope ~container rhs
    | Ast.SCallStmt { callee; args; _ } ->
        use_value scope callee;
        List.iter (walk_expr scope ~container) args
    | Ast.SIf { cond; then_; else_ } -> (
        walk_expr scope ~container cond;
        let then_scope = nav_scope_copy scope in
        walk_stmt then_scope ~container then_;
        match else_ with
        | None -> ()
        | Some e ->
            let else_scope = nav_scope_copy scope in
            walk_stmt else_scope ~container e)
    | Ast.SWhile { cond; body } ->
        walk_expr scope ~container cond;
        let body_scope = nav_scope_copy scope in
        walk_stmt body_scope ~container body
    | Ast.SFor { init; cond; step; body } ->
        let loop_scope = nav_scope_copy scope in
        (match init with
        | None -> ()
        | Some i ->
            prebind_stmt loop_scope ~container i;
            walk_stmt loop_scope ~container i);
        (match cond with
        | None -> ()
        | Some e -> walk_expr loop_scope ~container e);
        (match step with
        | None -> ()
        | Some st ->
            prebind_stmt loop_scope ~container st;
            walk_stmt loop_scope ~container st);
        let body_scope = nav_scope_copy loop_scope in
        walk_stmt body_scope ~container body
    | Ast.SReturn eo -> (
        match eo with None -> () | Some e -> walk_expr scope ~container e)
    | Ast.SLabel { body; _ } -> walk_stmt scope ~container body
    | Ast.SGoto id -> use_label scope id
  and walk_stmt_list (scope : nav_scope) ~(container : string option)
      (xs : Ast.stmt Ast.node list) : unit =
    List.iter (prebind_stmt scope ~container) xs;
    List.iter (walk_stmt scope ~container) xs
  in

  let bind_imports () =
    let is_importable_def (d : def) : bool =
      d.kind <> sym_kind_module && d.kind <> sym_kind_func
    in
    sem_import_dirs doc
    |> List.iter (fun (imp : compool_import_dir) ->
        match resolve_compool_doc_uncached ws ~name:imp.compool with
        | None -> ()
        | Some target ->
            let defs = exported_defs_for_import_scope target in
            if imp.selected = [] then
              List.iter
                (fun d ->
                  if is_importable_def d then bind_external_def root_scope d)
                defs
            else
              let selected = Hashtbl.create 32 in
              List.iter
                (fun (nm, _loc) ->
                  Hashtbl.replace selected (normalize_name nm) true)
                imp.selected;
              List.iter
                (fun d ->
                  if is_importable_def d && Hashtbl.mem selected d.key then
                    bind_external_def root_scope d)
                defs)
  in

  let bind_defines () =
    List.iter
      (fun (dm : Preprocess.define) ->
        let d = def_of_preprocess_define doc dm in
        match nav_add_decl nav d with
        | None -> ()
        | Some b -> nav_bind_value root_scope b)
      doc.Document.defines
  in

  bind_imports ();
  bind_defines ();
  (match doc.Document.ast with
  | None -> ()
  | Some prog ->
      List.iter
        (function
          | Ast.TopDecl d -> prebind_decl root_scope ~container:None d
          | Ast.TopStmt s -> prebind_stmt root_scope ~container:None s)
        prog;
      List.iter
        (function
          | Ast.TopDecl d -> walk_decl root_scope ~container:None d
          | Ast.TopStmt s -> walk_stmt root_scope ~container:None s)
        prog);
  nav
