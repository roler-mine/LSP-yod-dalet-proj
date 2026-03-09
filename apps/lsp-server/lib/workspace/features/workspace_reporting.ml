module T = Lsp.Types
open Ast
open Workspace_state
open Workspace_runtime
open Workspace_index_graph
open Workspace_imports
open Workspace_background
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_tuning

type semantic_token = {
  st_line : int;
  st_start : int;
  st_len : int;
  st_typ : int;
  st_mods : int;
}

(* Keep these indices aligned with lsp_server.ml semanticTokens legend. *)
let sem_tt_namespace = 0
let sem_tt_type = 1
let sem_tt_function = 2
let sem_tt_variable = 3
let sem_tt_property = 4
let sem_tt_keyword = 5
let sem_tt_string = 6
let sem_tt_number = 7
let sem_tt_operator = 8
let sem_tt_macro = 9
let sem_mod_declaration = 1 lsl 0
let sem_mod_readonly = 1 lsl 1

let semantic_token_compare (a : semantic_token) (b : semantic_token) : int =
  if a.st_line <> b.st_line then compare a.st_line b.st_line
  else if a.st_start <> b.st_start then compare a.st_start b.st_start
  else if a.st_len <> b.st_len then compare a.st_len b.st_len
  else if a.st_typ <> b.st_typ then compare a.st_typ b.st_typ
  else compare a.st_mods b.st_mods

let semantic_token_key (t : semantic_token) : string =
  Printf.sprintf "%d|%d|%d" t.st_line t.st_start t.st_len

let snapshot_token_of_semantic (t : semantic_token) : Doc_snapshot.token =
  {
    line = t.st_line;
    start = t.st_start;
    len = t.st_len;
    typ = t.st_typ;
    mods = t.st_mods;
  }

let semantic_token_of_snapshot (t : Doc_snapshot.token) : semantic_token =
  {
    st_line = t.line;
    st_start = t.start;
    st_len = t.len;
    st_typ = t.typ;
    st_mods = t.mods;
  }

let normalize_range_order (r : T.Range.t) : T.Range.t =
  if compare_pos r.start r.end_ <= 0 then r
  else { T.Range.start = r.end_; end_ = r.start }

let semantic_token_intersects_range (t : semantic_token) (r : T.Range.t) : bool
    =
  let line = t.st_line in
  let s = t.st_start in
  let e = t.st_start + t.st_len in
  if line < r.start.line || line > r.end_.line then false
  else
    let after_start =
      if line = r.start.line then e > r.start.character else true
    in
    let before_end =
      if line = r.end_.line then s < r.end_.character else true
    in
    after_start && before_end

let semantic_token_of_span ~(line0 : int) ~(col0 : int) ~(col1 : int)
    ~(typ : int) ~(mods : int) : semantic_token option =
  let len = col1 - col0 in
  if line0 < 0 || col0 < 0 || len <= 0 then None
  else
    Some
      {
        st_line = line0;
        st_start = col0;
        st_len = len;
        st_typ = typ;
        st_mods = mods;
      }

let semantic_token_of_loc (loc : Ast.Loc.t) ~(typ : int) ~(mods : int) :
    semantic_token option =
  let sp = loc.Ast.Loc.start_pos in
  let ep = loc.Ast.Loc.end_pos in
  if sp.line <> ep.line then None
  else
    semantic_token_of_span
      ~line0:(max 0 (sp.line - 1))
      ~col0:(max 0 sp.col) ~col1:(max 0 ep.col) ~typ ~mods

let loc_same_span (a : Ast.Loc.t) (b : Ast.Loc.t) : bool =
  a.start_pos.line = b.start_pos.line
  && a.start_pos.col = b.start_pos.col
  && a.end_pos.line = b.end_pos.line
  && a.end_pos.col = b.end_pos.col

let semantic_type_of_def_kind (k : int) : int option =
  if k = sym_kind_module then Some sem_tt_namespace
  else if k = sym_kind_type then Some sem_tt_type
  else if k = sym_kind_func then Some sem_tt_function
  else if k = sym_kind_field then Some sem_tt_property
  else if k = sym_kind_var || k = sym_kind_const then Some sem_tt_variable
  else None

let semantic_mods_for_occurrence ~(decl : def) ~(occ_uri : T.DocumentUri.t)
    ~(occ_loc : Ast.Loc.t) : int =
  let mods = if decl.kind = sym_kind_const then sem_mod_readonly else 0 in
  if same_uri occ_uri decl.uri && loc_same_span occ_loc decl.loc then
    mods lor sem_mod_declaration
  else mods

let collect_define_macro_keys (doc : Document.t) : (string, bool) Hashtbl.t =
  let doc = Document.ensure_parsed doc in
  let out = Hashtbl.create 32 in
  let add_define_args (args : Ast.ident list) : unit =
    match args with
    | [] -> ()
    | first :: _ ->
        let k = normalize_name first.v in
        if k <> "" then Hashtbl.replace out k true
  in
  let rec walk_decl (d : Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DDirective { name; args } ->
        if normalize_name name.v = "DEFINE" then add_define_args args
    | Ast.DProc p ->
        List.iter walk_decl p.v.locals;
        walk_stmt p.v.body
    | Ast.DVar _ | Ast.DConst _ | Ast.DType _ -> ()
  and walk_stmt (s : Ast.stmt Ast.node) : unit =
    match s.v with
    | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _ | Ast.SGoto _
      ->
        ()
    | Ast.SDecl d -> walk_decl d
    | Ast.SBlock xs -> List.iter walk_stmt xs
    | Ast.SIf { then_; else_; _ } -> (
        walk_stmt then_;
        match else_ with None -> () | Some e -> walk_stmt e)
    | Ast.SWhile { body; _ } -> walk_stmt body
    | Ast.SFor { init; step; body; _ } ->
        (match init with None -> () | Some i -> walk_stmt i);
        (match step with None -> () | Some st -> walk_stmt st);
        walk_stmt body
    | Ast.SLabel { body; _ } -> walk_stmt body
  in
  (match doc.Document.ast with
  | None -> ()
  | Some prog ->
      List.iter
        (function Ast.TopDecl d -> walk_decl d | Ast.TopStmt s -> walk_stmt s)
        prog);
  out

let semantic_symbol_tokens_for_doc (ws : t) (doc : Document.t) :
    semantic_token list =
  let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 1 in
  let nav = nav_for_doc_cached ws cache doc in
  let out = ref [] in
  Hashtbl.iter
    (fun sym_id occs ->
      match Hashtbl.find_opt nav.defs_by_id sym_id with
      | None -> ()
      | Some decl -> (
          match semantic_type_of_def_kind decl.kind with
          | None -> ()
          | Some typ ->
              List.iter
                (fun (occ_uri, occ_loc) ->
                  if same_uri occ_uri doc.Document.uri then
                    let mods =
                      semantic_mods_for_occurrence ~decl ~occ_uri ~occ_loc
                    in
                    match semantic_token_of_loc occ_loc ~typ ~mods with
                    | None -> ()
                    | Some tok -> out := tok :: !out)
                occs))
    nav.occs_by_id;
  List.rev !out

let semantic_class_of_lex_token ~(macro_keys : (string, bool) Hashtbl.t)
    (tok : Parser.token) : (int * int) option =
  match tok with
  | Parser.ID s ->
      let k = normalize_name s in
      if k = "" then None
      else if Hashtbl.mem macro_keys k then Some (sem_tt_macro, 0)
      else if is_builtin_function_name k then Some (sem_tt_function, 0)
      else if is_builtin_type k then Some (sem_tt_type, 0)
      else if is_reserved_keyword k then Some (sem_tt_keyword, 0)
      else Some (sem_tt_variable, 0)
  | Parser.INTLIT _ | Parser.FLOATLIT _ -> Some (sem_tt_number, 0)
  | Parser.STRINGLIT _ -> Some (sem_tt_string, 0)
  | Parser.START | Parser.TERM | Parser.BEGIN | Parser.END | Parser.DEF
  | Parser.REF | Parser.PROC | Parser.ITEM | Parser.TABLE | Parser.STATIC
  | Parser.CONSTANT | Parser.IF | Parser.ELSE | Parser.WHILE | Parser.FOR
  | Parser.BY | Parser.THEN | Parser.CASE | Parser.DEFAULT | Parser.FALLTHRU
  | Parser.EXIT | Parser.GOTO | Parser.RETURN | Parser.ABORT | Parser.STOP
  | Parser.NOT | Parser.AND | Parser.OR | Parser.XOR | Parser.EQV | Parser.MOD
  | Parser.TRUE | Parser.FALSE | Parser.PROGRAM | Parser.COMPOOL
  | Parser.ICOMPOOL | Parser.DEFINE | Parser.TYPE | Parser.BLOCK ->
      Some (sem_tt_keyword, 0)
  | Parser.EQ | Parser.NE | Parser.LT | Parser.LE | Parser.GT | Parser.GE
  | Parser.PLUS | Parser.MINUS | Parser.STAR | Parser.SLASH | Parser.POW
  | Parser.AT | Parser.BANG | Parser.CONV_L | Parser.CONV_R ->
      Some (sem_tt_operator, 0)
  | Parser.LPAREN | Parser.RPAREN | Parser.COMMA | Parser.SEMI | Parser.COLON
  | Parser.DOT | Parser.EOF ->
      None

let semantic_lex_tokens_for_doc (doc : Document.t)
    ~(macro_keys : (string, bool) Hashtbl.t) : semantic_token list =
  Lexer.with_session_state (fun () ->
      let lexbuf = Lexing.from_string doc.Document.text in
      (match doc.Document.file with
      | None -> ()
      | Some f ->
          lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = f });
      let out = ref [] in
      let rec loop () =
        let tok = Lexer.token lexbuf in
        let sp = Lexing.lexeme_start_p lexbuf in
        let ep = Lexing.lexeme_end_p lexbuf in
        let line0 = max 0 (sp.Lexing.pos_lnum - 1) in
        let col0 = max 0 (sp.Lexing.pos_cnum - sp.Lexing.pos_bol) in
        let col1 = max 0 (ep.Lexing.pos_cnum - ep.Lexing.pos_bol) in
        (match semantic_class_of_lex_token ~macro_keys tok with
        | None -> ()
        | Some (typ, mods) -> (
            match semantic_token_of_span ~line0 ~col0 ~col1 ~typ ~mods with
            | None -> ()
            | Some st -> out := st :: !out));
        match tok with Parser.EOF -> () | _ -> loop ()
      in
      (try loop () with _ -> ());
      List.rev !out)

let semantic_tokens_json_of_tokens (tokens : semantic_token list) :
    Yojson.Safe.t =
  let data_rev = ref [] in
  let prev_line = ref 0 in
  let prev_start = ref 0 in
  let first = ref true in
  List.iter
    (fun t ->
      let delta_line, delta_start =
        if !first then (t.st_line, t.st_start)
        else if t.st_line = !prev_line then (0, t.st_start - !prev_start)
        else (t.st_line - !prev_line, t.st_start)
      in
      data_rev :=
        t.st_mods :: t.st_typ :: t.st_len :: delta_start :: delta_line
        :: !data_rev;
      first := false;
      prev_line := t.st_line;
      prev_start := t.st_start)
    tokens;
  `Assoc [ ("data", `List (List.rev_map (fun i -> `Int i) !data_rev)) ]

let semantic_tokens_with_result_id (doc : Document.t) (base : Yojson.Safe.t) :
    Yojson.Safe.t =
  let result_id = Digest.(to_hex (string doc.Document.text)) in
  match base with
  | `Assoc xs -> `Assoc (("resultId", `String result_id) :: xs)
  | _ -> base

let semantic_tokens_for_doc (ws : t) (doc : Document.t)
    ~(range : T.Range.t option) : semantic_token list =
  let norm_range = Option.map normalize_range_order range in
  let in_range tok =
    match norm_range with
    | None -> true
    | Some r -> semantic_token_intersects_range tok r
  in
  let cached =
    if ws.sem_store_enabled then
      match
        Semantic_store.snapshot_for_uri ws.semantic_store ~uri:doc.Document.uri
      with
      | Some snap when snap.Doc_snapshot.doc_rev = doc.Document.rev -> (
          match snap.Doc_snapshot.semantic_tokens_full with
          | None -> None
          | Some toks ->
              Perf_stats.tick "semantic_tokens.cache_hit";
              Some
                (List.map semantic_token_of_snapshot toks
                |> List.filter in_range))
      | _ -> None
    else None
  in
  match cached with
  | Some toks -> toks
  | None ->
      Perf_stats.tick "semantic_tokens.cache_miss";
      let seen = Hashtbl.create 2048 in
      let out = ref [] in
      let add tok =
        if in_range tok then
          let k = semantic_token_key tok in
          if not (Hashtbl.mem seen k) then (
            Hashtbl.add seen k true;
            out := tok :: !out)
      in
      semantic_symbol_tokens_for_doc ws doc |> List.iter add;
      let macro_keys = collect_define_macro_keys doc in
      semantic_lex_tokens_for_doc doc ~macro_keys |> List.iter add;
      let toks = List.sort semantic_token_compare (List.rev !out) in
      (if ws.sem_store_enabled && range = None then
         match
           Semantic_store.snapshot_for_uri ws.semantic_store
             ~uri:doc.Document.uri
         with
         | Some snap when snap.Doc_snapshot.doc_rev = doc.Document.rev ->
             Doc_snapshot.set_semantic_tokens_full snap
               (List.map snapshot_token_of_semantic toks)
         | _ -> ());
      toks

let semantic_tokens_full_json_for (ws : t) ~(uri : T.DocumentUri.t) :
    Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Assoc [ ("data", `List []) ]
  | Some doc ->
      semantic_tokens_for_doc ws doc ~range:None
      |> semantic_tokens_json_of_tokens
      |> semantic_tokens_with_result_id doc

let semantic_tokens_range_json_for (ws : t) ~(uri : T.DocumentUri.t)
    ~(range : T.Range.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Assoc [ ("data", `List []) ]
  | Some doc ->
      semantic_tokens_for_doc ws doc ~range:(Some range)
      |> semantic_tokens_json_of_tokens
      |> semantic_tokens_with_result_id doc

type doc_symbol = {
  ds_name : string;
  ds_kind : int;
  ds_range : Ast.Loc.t;
  ds_selection : Ast.Loc.t;
  ds_detail : string option;
  ds_children : doc_symbol list;
}

let rec doc_symbol_json (s : doc_symbol) : Yojson.Safe.t =
  let fields =
    [
      ("name", `String s.ds_name);
      ("kind", `Int s.ds_kind);
      ("range", range_json_of_loc s.ds_range);
      ("selectionRange", range_json_of_loc s.ds_selection);
      ("children", `List (List.map doc_symbol_json s.ds_children));
    ]
  in
  let fields =
    match s.ds_detail with
    | Some d when String.trim d <> "" -> ("detail", `String d) :: fields
    | _ -> fields
  in
  `Assoc fields

let document_symbols_from_ast (doc : Document.t) : doc_symbol list =
  let doc = Document.ensure_parsed doc in
  let current_compool_key =
    match doc.Document.compool_def with
    | None -> None
    | Some n ->
        let k = normalize_name n in
        if k = "" then None else Some k
  in
  let mk_symbol ~(name : string) ~(kind : int) ~(range : Ast.Loc.t)
      ~(selection : Ast.Loc.t) ~(detail : string option)
      ~(children : doc_symbol list) : doc_symbol =
    {
      ds_name = name;
      ds_kind = kind;
      ds_range = range;
      ds_selection = selection;
      ds_detail = detail;
      ds_children = children;
    }
  in
  let of_type_fields (t : Ast.type_expr Ast.node) : doc_symbol list =
    match t.v with
    | Ast.TRecord fields ->
        fields
        |> List.map (fun f ->
            let fv = f.v in
            mk_symbol ~name:fv.fname.v ~kind:sym_kind_field ~range:f.loc
              ~selection:fv.fname.loc
              ~detail:(Some (type_expr_to_compact_string fv.ftype))
              ~children:[])
    | _ -> []
  in
  let proc_detail (p : Ast.proc Ast.node) : string =
    let params =
      p.v.params
      |> List.map (fun prm ->
          let pname = prm.v.pname.v in
          match prm.v.ptype.v with
          | Ast.TName id when normalize_name id.v = "__IMPLICIT__" -> pname
          | _ -> pname ^ ": " ^ type_expr_to_compact_string prm.v.ptype)
      |> String.concat ", "
    in
    let ret =
      match p.v.returns with
      | None -> ""
      | Some r -> " -> " ^ type_expr_to_compact_string r
    in
    let attr =
      match p.v.use_attr with
      | Ast.UseNormal -> ""
      | Ast.UseRec -> " REC"
      | Ast.UseRent -> " RENT"
    in
    "(" ^ params ^ ")" ^ ret ^ attr
  in
  let rec of_decl (d : Ast.decl Ast.node) : doc_symbol list =
    match d.v with
    | Ast.DVar { name; dtype; _ } ->
        [
          mk_symbol ~name:name.v ~kind:sym_kind_var ~range:d.loc
            ~selection:name.loc
            ~detail:(Some (type_expr_to_compact_string dtype))
            ~children:[];
        ]
    | Ast.DConst { name; dtype; _ } ->
        let detail =
          match dtype with
          | None -> Some "const"
          | Some t -> Some ("const " ^ type_expr_to_compact_string t)
        in
        [
          mk_symbol ~name:name.v ~kind:sym_kind_const ~range:d.loc
            ~selection:name.loc ~detail ~children:[];
        ]
    | Ast.DType { name; defn } ->
        [
          mk_symbol ~name:name.v ~kind:sym_kind_type ~range:d.loc
            ~selection:name.loc
            ~detail:(Some (type_expr_to_compact_string defn))
            ~children:(of_type_fields defn);
        ]
    | Ast.DProc p ->
        let param_children =
          p.v.params
          |> List.map (fun prm ->
              mk_symbol ~name:prm.v.pname.v ~kind:sym_kind_var ~range:prm.loc
                ~selection:prm.v.pname.loc
                ~detail:(Some (type_expr_to_compact_string prm.v.ptype))
                ~children:[])
        in
        let local_children = p.v.locals |> List.concat_map of_decl in
        [
          mk_symbol ~name:p.v.name.v ~kind:sym_kind_func ~range:p.loc
            ~selection:p.v.name.loc
            ~detail:(Some (proc_detail p))
            ~children:(param_children @ local_children);
        ]
    | Ast.DDirective { name; args } -> (
        let key = normalize_name name.v in
        match (key, args) with
        | "PROGRAM", first :: _ ->
            [
              mk_symbol ~name:first.v ~kind:sym_kind_module ~range:d.loc
                ~selection:first.loc ~detail:(Some "PROGRAM") ~children:[];
            ]
        | "PROGRAM", [] ->
            [
              mk_symbol ~name:name.v ~kind:sym_kind_module ~range:d.loc
                ~selection:name.loc ~detail:None ~children:[];
            ]
        | ("COMPOOL" | "ICOMPOOL"), first :: _ ->
            let fk = normalize_name first.v in
            let is_file_compool =
              match current_compool_key with
              | Some ck -> ck = fk
              | None -> key = "COMPOOL"
            in
            if not is_file_compool then []
            else
              [
                mk_symbol ~name:first.v ~kind:sym_kind_module ~range:d.loc
                  ~selection:first.loc ~detail:(Some "COMPOOL") ~children:[];
              ]
        | ("COMPOOL" | "ICOMPOOL"), [] -> []
        | "BLOCK", _ -> []
        | _ -> [])
  in
  match doc.Document.ast with
  | None -> []
  | Some prog ->
      prog
      |> List.concat_map (function
        | Ast.TopDecl d -> of_decl d
        | Ast.TopStmt _ -> [])

let document_symbols_json_for (ws : t) ~(uri : T.DocumentUri.t) : Yojson.Safe.t
    =
  match doc_of_uri ws uri with
  | None -> `List []
  | Some doc -> (
      if ws.sem_store_enabled then
        ignore (nav_for_doc_cached ws (Hashtbl.create 1) doc);
      let from_cache =
        if ws.sem_store_enabled then
          match
            Semantic_store.snapshot_for_uri ws.semantic_store
              ~uri:doc.Document.uri
          with
          | Some snap when snap.Doc_snapshot.doc_rev = doc.Document.rev -> (
              match snap.Doc_snapshot.doc_symbols with
              | Some xs ->
                  Perf_stats.tick "document_symbols.cache_hit";
                  Some (`List xs)
              | None -> None)
          | _ -> None
        else None
      in
      match from_cache with
      | Some x -> x
      | None ->
          Perf_stats.tick "document_symbols.cache_miss";
          let payload =
            match document_symbols_from_ast doc with
            | [] -> collect_doc_defs doc |> uniq_defs |> List.map symbol_json
            | xs -> List.map doc_symbol_json xs
          in
          (if ws.sem_store_enabled then
             match
               Semantic_store.snapshot_for_uri ws.semantic_store
                 ~uri:doc.Document.uri
             with
             | Some snap when snap.Doc_snapshot.doc_rev = doc.Document.rev ->
                 Doc_snapshot.set_doc_symbols snap payload
             | _ -> ());
          `List payload)

let debug_report_for (ws : t) ~(uri : T.DocumentUri.t) ~(max_tokens : int) :
    Yojson.Safe.t =
  match Hashtbl.find_opt ws.docs uri with
  | None -> `Assoc [ ("error", `String "document not open") ]
  | Some doc ->
      let root =
        match ws.root_path with None -> `Null | Some p -> `String p
      in

      let compools =
        match ws.index with
        | None ->
            `Assoc
              [
                ("count", `Int 0);
                ("sources", `Int 0);
                ("complete", `Bool false);
                ("checkpointLoaded", `Bool false);
                ("reconcilePending", `Bool false);
                ("reconcileEpoch", `Int 0);
                ("sourcesBeforeReconcile", `Int 0);
                ("sourcesAfterReconcile", `Int 0);
                ("stalePrunedCount", `Int 0);
                ("sample", `List []);
              ]
        | Some idx ->
            let sources = Workspace_index.all_source_paths idx in
            let sample =
              Workspace_index.sample idx 10
              |> List.map (fun (k, v) ->
                  `Assoc [ ("name", `String k); ("path", `String v) ])
            in
            `Assoc
              [
                ("count", `Int (Workspace_index.compool_count idx));
                ("sources", `Int (List.length sources));
                ("complete", `Bool (Workspace_index.is_complete idx));
                ( "checkpointLoaded",
                  `Bool (index_checkpoint_loaded_for_report ws) );
                ( "reconcilePending",
                  `Bool (index_reconcile_pending_for_report ws) );
                ("reconcileEpoch", `Int (index_reconcile_epoch_for_report ws));
                ( "sourcesBeforeReconcile",
                  `Int (index_reconcile_sources_before_for_report ws) );
                ( "sourcesAfterReconcile",
                  `Int (index_reconcile_sources_after_for_report ws) );
                ( "stalePrunedCount",
                  `Int (index_reconcile_stale_pruned_for_report ws) );
                ("sample", `List sample);
              ]
      in

      let imports =
        Document.imports doc
        |> List.map (fun (imp : Preprocess.import) ->
            let resolved =
              match ws.index with
              | None -> None
              | Some idx -> Workspace_index.find_compool idx ~name:imp.name
            in
            `Assoc
              [
                ("name", `String imp.name);
                ( "resolved",
                  match resolved with None -> `Null | Some p -> `String p );
                ( "loc",
                  `Assoc
                    [
                      ("line", `Int imp.loc.start_pos.line);
                      ("col", `Int imp.loc.start_pos.col);
                    ] );
              ])
      in

      let tokens =
        let text = Document.text doc in
        Lexer.with_session_state (fun () ->
            let lexbuf = Lexing.from_string text in
            let rec loop i acc =
              if i >= max_tokens then List.rev acc
              else
                let t = Lexer.token lexbuf in
                let sp = Lexing.lexeme_start_p lexbuf in
                let ep = Lexing.lexeme_end_p lexbuf in
                let lex = Lexing.lexeme lexbuf in
                let item =
                  `Assoc
                    [
                      ("token", `String (Parse.Debug.string_of_token t));
                      ("lexeme", `String lex);
                      ("range", lsp_range_of_lex sp ep);
                    ]
                in
                match t with
                | Parser.EOF -> List.rev (item :: acc)
                | _ -> loop (i + 1) (item :: acc)
            in
            loop 0 [])
      in

      let workspace_diag_mode_s =
        match ws.workspace_diag_mode with
        | WorkspaceDiagsOff -> "off"
        | WorkspaceDiagsErrors -> "errors"
        | WorkspaceDiagsAll -> "all"
      in

      let oldest_open_unconverged_ms =
        let now = Perf_stats.now_ms () in
        Hashtbl.fold
          (fun _ started acc ->
            let age = max 0.0 (now -. started) in
            match acc with None -> Some age | Some prev -> Some (max prev age))
          ws.open_provisional_since_ms None
      in

      let open_pending_docs =
        Hashtbl.fold
          (fun uri doc acc ->
            if doc.Document.parse_rev = doc.Document.rev then acc
            else
              `Assoc
                [
                  ("uri", `String (Uri_path.docuri_to_string uri));
                  ( "file",
                    match doc.Document.file with
                    | None -> `Null
                    | Some path -> `String path );
                  ("rev", `Int doc.Document.rev);
                  ("parseRev", `Int doc.Document.parse_rev);
                ]
              :: acc)
          ws.docs []
        |> List.sort (fun a b ->
            let file_of = function
              | `Assoc fields -> (
                  match List.assoc_opt "file" fields with
                  | Some (`String path) -> path
                  | _ -> "")
              | _ -> ""
            in
            compare (file_of a) (file_of b))
      in

      let background =
        let pressure_mode =
          workspace_pressure_mode ws |> pressure_mode_to_string
        in
        let pressure_live_mb = workspace_pressure_live_mb ws in
        let xmodule_prereqs_ready = xmodule_diag_prereqs_ready ws in
        let xmodule_suppression_active =
          warmup_suppress_crossmodule_unresolved && not xmodule_prereqs_ready
        in
        `Assoc
          [
            ("diagMode", `String workspace_diag_mode_s);
            ("pressureMode", `String pressure_mode);
            ("pressureLiveMB", `Int pressure_live_mb);
            ("xmoduleDiagPrereqsReady", `Bool xmodule_prereqs_ready);
            ("xmoduleSuppressionActive", `Bool xmodule_suppression_active);
            ("queueHighSmall", `Int (Queue.length ws.bg_high_small_queue));
            ("queueRootSmall", `Int (Queue.length ws.bg_root_small_queue));
            ("queueNormalSmall", `Int (Queue.length ws.bg_norm_small_queue));
            ("queueHighLarge", `Int (Queue.length ws.bg_high_large_queue));
            ("queueRootLarge", `Int (Queue.length ws.bg_root_large_queue));
            ("queueNormalLarge", `Int (Queue.length ws.bg_norm_large_queue));
            ("enqueuedSet", `Int (Hashtbl.length ws.bg_enqueued));
            ( "parseWorkerInFlight",
              `Int (Hashtbl.length ws.parse_worker_inflight) );
            ("parseWorkerJobs", `Int (Queue.length ws.parse_worker_jobs));
            ("parseWorkerResults", `Int (Queue.length ws.parse_worker_results));
            ("parsedDocs", `Int (Hashtbl.length ws.bg_parsed));
            ("closedDocLruMax", `Int ws.closed_doc_lru_max);
            ("closedDocTouched", `Int (Hashtbl.length ws.closed_doc_last_touch));
            ("seedCursor", `Int ws.bg_seed_cursor);
            ("seedTotal", `Int (Array.length ws.bg_seed_paths));
            ("seedNeedsRefresh", `Bool ws.bg_seed_needs_refresh);
            ("graphNeedsRefresh", `Bool ws.graph_needs_refresh);
            ("graphEpoch", `Int ws.graph_epoch);
            ("graphSccCount", `Int ws.graph_scc_count);
            ("graphRoots", `Int (Hashtbl.length ws.graph_root_reason));
            ("graphNodes", `Int (Hashtbl.length ws.graph_nodes));
            ("graphRootClosureCursor", `Int ws.graph_root_closure_cursor);
            ( "graphRootClosureTotal",
              `Int (Array.length ws.graph_root_closure_paths) );
            ("closedDiagUris", `Int (Hashtbl.length ws.bg_closed_diags));
            ( "pendingDiagUpdates",
              `Int (Queue.length ws.bg_pending_diag_updates) );
            ( "pendingDiagPayloads",
              `Int (Hashtbl.length ws.bg_pending_diag_payloads) );
            ("pendingDiagSet", `Int (Hashtbl.length ws.bg_pending_diag_set));
            ( "openDiagRevalidateUpdates",
              `Int (Queue.length ws.open_diag_revalidate_updates) );
            ( "openDiagRevalidatePayloads",
              `Int (Hashtbl.length ws.open_diag_revalidate_payloads) );
            ( "openDiagRevalidateSet",
              `Int (Hashtbl.length ws.open_diag_revalidate_set) );
            ( "openPendingGenerations",
              `Int (Hashtbl.length ws.open_parse_generation) );
            ( "oldestOpenUnconvergedMs",
              match oldest_open_unconverged_ms with
              | None -> `Null
              | Some ms -> `Int (int_of_float ms) );
            ("openPendingDocs", `List open_pending_docs);
          ]
      in

      `Assoc
        [
          ("uri", `String (Uri_path.docuri_to_string uri));
          ("root", root);
          ("startup", startup_readiness_json_for_report ws);
          ("compools", compools);
          ("imports", `List imports);
          ("background", background);
          ("tokens", `List tokens);
        ]

type lsif_ref = {
  r_uri : T.DocumentUri.t;
  r_loc : Ast.Loc.t;
  r_declaration : bool;
}

type lsif_symbol_bucket = {
  sb_id : string;
  mutable sb_key : string;
  mutable sb_kind : int;
  sb_defs : (string, T.DocumentUri.t * Ast.Loc.t) Hashtbl.t;
  sb_impls : (string, T.DocumentUri.t * Ast.Loc.t) Hashtbl.t;
  sb_refs : (string, lsif_ref) Hashtbl.t;
}

let lsif_symbol_bucket_create ~(id : string) ~(key : string) (kind : int) :
    lsif_symbol_bucket =
  {
    sb_id = id;
    sb_key = key;
    sb_kind = kind;
    sb_defs = Hashtbl.create 8;
    sb_impls = Hashtbl.create 8;
    sb_refs = Hashtbl.create 16;
  }

let lsif_bucket_for (tbl : (string, lsif_symbol_bucket) Hashtbl.t)
    ~(sym_id : string) ~(key : string) ~(kind : int) : lsif_symbol_bucket =
  match Hashtbl.find_opt tbl sym_id with
  | Some b ->
      if b.sb_key = "" then b.sb_key <- key;
      if b.sb_kind <= 0 then b.sb_kind <- kind;
      b
  | None ->
      let b = lsif_symbol_bucket_create ~id:sym_id ~key kind in
      Hashtbl.add tbl sym_id b;
      b

let lsif_add_key_index (tbl : (string, (string, bool) Hashtbl.t) Hashtbl.t)
    ~(key : string) ~(sym_id : string) : unit =
  let set =
    match Hashtbl.find_opt tbl key with
    | Some s -> s
    | None ->
        let s = Hashtbl.create 4 in
        Hashtbl.add tbl key s;
        s
  in
  Hashtbl.replace set sym_id true

let sorted_set_keys (set : (string, bool) Hashtbl.t) : string list =
  Hashtbl.fold (fun k _ acc -> k :: acc) set [] |> List.sort String.compare

let lsif_add_loc (tbl : (string, T.DocumentUri.t * Ast.Loc.t) Hashtbl.t)
    ~(uri : T.DocumentUri.t) ~(loc : Ast.Loc.t) : unit =
  let k = loc_key ~uri loc in
  if not (Hashtbl.mem tbl k) then Hashtbl.add tbl k (uri, loc)

let lsif_add_ref (tbl : (string, lsif_ref) Hashtbl.t) ~(uri : T.DocumentUri.t)
    ~(loc : Ast.Loc.t) ~(declaration : bool) : unit =
  let k = loc_key ~uri loc in
  if not (Hashtbl.mem tbl k) then
    Hashtbl.add tbl k { r_uri = uri; r_loc = loc; r_declaration = declaration }

let docs_for_lsif (ws : t) : Document.t list =
  (* Keep LSIF export non-blocking for interactive navigation requests.
     Index growth continues via background pumps triggered by workspace events. *)
  pump_index_background ws;
  let pressure = workspace_pressure_mode ws in
  let load_budget = lsif_doc_load_budget_for_pressure ws in
  let seen = Hashtbl.create 512 in
  let out = ref [] in
  let add_doc (doc : Document.t) =
    let uri_s = Uri_path.docuri_to_string doc.Document.uri in
    if not (Hashtbl.mem seen uri_s) then (
      Hashtbl.add seen uri_s true;
      out := doc :: !out)
  in
  let open_docs = Hashtbl.fold (fun _ doc acc -> doc :: acc) ws.docs [] in
  List.iter
    (fun doc ->
      Perf_stats.tick "lsif.docs_from_open";
      let ready_doc =
        if doc.Document.parse_rev = doc.Document.rev then doc
        else
          Perf_stats.time "lsif.doc_load" (fun () ->
              try
                let parsed =
                  parse_guarded_document_make ws ~uri:doc.Document.uri
                    ~file:doc.Document.file ~text:doc.Document.text
                  |> background_doc_with_diags ws
                in
                store_doc ~import_lookup_pump:false ws doc.Document.uri parsed;
                parsed
              with _ -> doc)
      in
      add_doc ready_doc)
    open_docs;
  let loaded = ref 0 in
  (match ws.index with
  | None -> ()
  | Some idx ->
      if (not (is_network_root ws)) && load_budget > 0 then
        Workspace_index.all_source_paths idx
        |> List.iter (fun p ->
            if !loaded < load_budget then (
              Perf_stats.tick "lsif.docs_path_scan";
              let doc_opt =
                match pressure with
                | PressureNormal ->
                    Perf_stats.time "lsif.doc_load" (fun () -> doc_at_path ws p)
                | PressureSoft | PressureCritical ->
                    Perf_stats.time "lsif.doc_load" (fun () ->
                        doc_at_path_cached ws p)
              in
              match doc_opt with
              | None -> Perf_stats.tick "lsif.doc_load_miss"
              | Some d ->
                  Perf_stats.tick "lsif.doc_load_hit";
                  incr loaded;
                  add_doc d)));
  List.rev !out

let sorted_assoc_values (tbl : (string, 'a) Hashtbl.t) : (string * 'a) list =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let lsif_index_payload (ws : t) :
    Yojson.Safe.t * (string, Yojson.Safe.t) Hashtbl.t * int =
  let docs = docs_for_lsif ws in
  let nav_cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 128 in
  let symbols : (string, lsif_symbol_bucket) Hashtbl.t = Hashtbl.create 2048 in
  let key_index : (string, (string, bool) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 2048
  in

  let add_definition (sym_id : string) (d : def) : unit =
    if d.key <> "" then (
      let bucket = lsif_bucket_for symbols ~sym_id ~key:d.key ~kind:d.kind in
      lsif_add_key_index key_index ~key:d.key ~sym_id;
      lsif_add_loc bucket.sb_defs ~uri:d.uri ~loc:d.loc;
      if d.kind = sym_kind_func && is_likely_proc_implementation ws d then
        lsif_add_loc bucket.sb_impls ~uri:d.uri ~loc:d.loc)
  in

  List.iter
    (fun doc ->
      let nav = nav_for_doc_cached ws nav_cache doc in
      Hashtbl.iter (fun sym_id d -> add_definition sym_id d) nav.defs_by_id;
      if Hashtbl.length nav.defs_by_id = 0 then
        collect_doc_defs doc
        |> List.iter (fun d ->
            if d.key <> "" then (
              let sym_id = def_symbol_id d in
              add_definition sym_id d;
              let bucket =
                lsif_bucket_for symbols ~sym_id ~key:d.key ~kind:d.kind
              in
              lsif_add_key_index key_index ~key:d.key ~sym_id;
              lsif_add_ref bucket.sb_refs ~uri:d.uri ~loc:d.loc
                ~declaration:true));
      Hashtbl.iter
        (fun sym_id occs ->
          match Hashtbl.find_opt nav.defs_by_id sym_id with
          | None -> ()
          | Some d ->
              if d.key <> "" then (
                let bucket =
                  lsif_bucket_for symbols ~sym_id ~key:d.key ~kind:d.kind
                in
                lsif_add_key_index key_index ~key:d.key ~sym_id;
                let decl_key = loc_key ~uri:d.uri d.loc in
                List.iter
                  (fun (occ_uri, occ_loc) ->
                    let occ_key = loc_key ~uri:occ_uri occ_loc in
                    lsif_add_ref bucket.sb_refs ~uri:occ_uri ~loc:occ_loc
                      ~declaration:(occ_key = decl_key))
                  occs))
        nav.occs_by_id)
    docs;

  let symbols_table : (string, Yojson.Safe.t) Hashtbl.t = Hashtbl.create 2048 in
  let symbols_json =
    sorted_assoc_values symbols
    |> List.map (fun (_sym_id, bucket) ->
        let defs_json =
          sorted_assoc_values bucket.sb_defs
          |> List.map (fun (_k, (uri, loc)) -> location_json ~uri loc)
        in
        let impls_json =
          sorted_assoc_values bucket.sb_impls
          |> List.map (fun (_k, (uri, loc)) -> location_json ~uri loc)
        in
        let refs_json =
          sorted_assoc_values bucket.sb_refs
          |> List.map (fun (_k, r) ->
              `Assoc
                [
                  ("location", location_json ~uri:r.r_uri r.r_loc);
                  ("declaration", `Bool r.r_declaration);
                ])
        in
        let item =
          `Assoc
            [
              ("id", `String bucket.sb_id);
              ("key", `String bucket.sb_key);
              ("kind", `Int bucket.sb_kind);
              ("definitions", `List defs_json);
              ("implementations", `List impls_json);
              ("references", `List refs_json);
            ]
        in
        Hashtbl.replace symbols_table bucket.sb_id item;
        item)
  in
  let key_index_json =
    sorted_assoc_values key_index
    |> List.map (fun (key, sym_set) ->
        `Assoc
          [
            ("key", `String key);
            ( "symbolIds",
              `List
                (List.map
                   (fun sym_id -> `String sym_id)
                   (sorted_set_keys sym_set)) );
          ])
  in

  let revision =
    if ws.sem_store_enabled then Semantic_store.global_rev ws.semantic_store
    else 0
  in
  ( `Assoc
      [
        ("format", `String "jovial-lsif-lite");
        ("revision", `Int revision);
        ("docCount", `Int (List.length docs));
        ("symbolCount", `Int (List.length symbols_json));
        ("keyIndex", `List key_index_json);
        ("symbols", `List symbols_json);
      ],
    symbols_table,
    revision )

let current_lsif_revision (ws : t) : int = ws.lsif_snapshot_revision

let revision_of_lsif_payload (payload : Yojson.Safe.t) : int option =
  match payload with
  | `Assoc fields -> (
      match List.assoc_opt "revision" fields with
      | Some (`Int n) -> Some n
      | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
      | _ -> None)
  | _ -> None

let ensure_lsif_snapshot ?(allow_stale_startup : bool = true) (ws : t) :
    Yojson.Safe.t * (string, Yojson.Safe.t) Hashtbl.t * int =
  let pressure = workspace_pressure_mode ws in
  let current_revision = current_lsif_revision ws in
  match (pressure, ws.lsif_snapshot_payload, ws.lsif_snapshot_symbols) with
  | PressureCritical, Some payload, Some symbols ->
      Perf_stats.tick "lsif.defer_critical";
      (payload, symbols, ws.lsif_snapshot_revision)
  | _ -> (
      match (ws.lsif_snapshot_payload, ws.lsif_snapshot_symbols) with
      | Some payload, Some symbols
        when ws.lsif_snapshot_revision = current_revision
             &&
             match revision_of_lsif_payload payload with
             | Some payload_rev -> payload_rev = current_revision
             | None -> true ->
          Perf_stats.tick "lsif.snapshot_hit";
          (payload, symbols, current_revision)
      | Some payload, Some symbols
        when allow_stale_startup && ws.startup_fully_nav_ready_ms = None ->
          Perf_stats.tick "lsif.snapshot_hit";
          Perf_stats.tick "lsif.snapshot_hit_stale_startup";
          let payload_revision =
            match revision_of_lsif_payload payload with
            | Some n -> n
            | None -> ws.lsif_snapshot_revision
          in
          (payload, symbols, payload_revision)
      | _ ->
          Perf_stats.tick "lsif.snapshot_miss";
          let payload, symbols, payload_revision =
            Perf_stats.time "lsif.snapshot_rebuild_ms" (fun () ->
                lsif_index_payload ws)
          in
          let revision =
            if ws.sem_store_enabled then payload_revision else current_revision
          in
          ws.lsif_snapshot_revision <- revision;
          ws.lsif_snapshot_payload <- Some payload;
          ws.lsif_snapshot_symbols <- Some symbols;
          (payload, symbols, revision))

let lsif_index_json (ws : t) : Yojson.Safe.t =
  let payload, symbols_table, revision = ensure_lsif_snapshot ws in
  if ws.lsif_delta_enabled then
    Lsif_delta.update_full ws.lsif_delta_state ~revision ~symbols:symbols_table;
  payload

let lsif_delta_json (ws : t) ~(base_revision : int) : Yojson.Safe.t =
  let current_revision = current_lsif_revision ws in
  if not ws.lsif_delta_enabled then
    Lsif_delta.delta_json
      {
        base_revision;
        revision = current_revision;
        reset = true;
        deletes = [];
        upserts = [];
      }
  else if
    base_revision = current_revision
    && Lsif_delta.revision ws.lsif_delta_state = current_revision
  then
    Lsif_delta.delta_json
      {
        base_revision;
        revision = current_revision;
        reset = false;
        deletes = [];
        upserts = [];
      }
  else
    let _payload, symbols_table, revision =
      ensure_lsif_snapshot ws ~allow_stale_startup:false
    in
    let delta =
      Lsif_delta.diff ws.lsif_delta_state ~base_revision
        ~current_revision:revision ~current_symbols:symbols_table
    in
    Lsif_delta.update_full ws.lsif_delta_state ~revision ~symbols:symbols_table;
    Lsif_delta.delta_json delta

let semantic_tokens_data_of_tokens (tokens : semantic_token list) : int array =
  let data_rev = ref [] in
  let prev_line = ref 0 in
  let prev_start = ref 0 in
  let first = ref true in
  List.iter
    (fun t ->
      let delta_line, delta_start =
        if !first then (t.st_line, t.st_start)
        else if t.st_line = !prev_line then (0, t.st_start - !prev_start)
        else (t.st_line - !prev_line, t.st_start)
      in
      data_rev :=
        t.st_mods :: t.st_typ :: t.st_len :: delta_start :: delta_line
        :: !data_rev;
      first := false;
      prev_line := t.st_line;
      prev_start := t.st_start)
    tokens;
  Array.of_list (List.rev !data_rev)

let semantic_tokens_for (ws : t) ~(uri : T.DocumentUri.t)
    ~(range : T.Range.t option) : T.SemanticTokens.t option =
  match doc_of_uri ws uri with
  | None -> None
  | Some doc ->
      let data =
        semantic_tokens_for_doc ws doc ~range |> semantic_tokens_data_of_tokens
      in
      let resultId = Some Digest.(to_hex (string doc.Document.text)) in
      Some (T.SemanticTokens.create ~data ?resultId ())

let semantic_tokens_full_for (ws : t) ~(uri : T.DocumentUri.t) :
    T.SemanticTokens.t option =
  semantic_tokens_for ws ~uri ~range:None

let semantic_tokens_range_for (ws : t) ~(uri : T.DocumentUri.t)
    ~(range : T.Range.t) : T.SemanticTokens.t option =
  semantic_tokens_for ws ~uri ~range:(Some range)

let rec document_symbol_t_of_doc_symbol (s : doc_symbol) : T.DocumentSymbol.t =
  T.DocumentSymbol.create ~name:s.ds_name
    ~kind:(symbol_kind_of_def_kind s.ds_kind)
    ~range:(Lsp_conv.range_of_loc s.ds_range)
    ~selectionRange:(Lsp_conv.range_of_loc s.ds_selection)
    ?detail:s.ds_detail
    ~children:(List.map document_symbol_t_of_doc_symbol s.ds_children)
    ()

let document_symbols_for (ws : t) ~(uri : T.DocumentUri.t) :
    [ `DocumentSymbol of T.DocumentSymbol.t
    | `SymbolInformation of T.SymbolInformation.t ]
    list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let from_ast = document_symbols_from_ast doc in
      if from_ast <> [] then
        List.map
          (fun symbol ->
            `DocumentSymbol (document_symbol_t_of_doc_symbol symbol))
          from_ast
      else
        collect_doc_defs doc |> uniq_defs
        |> List.map (fun d -> `SymbolInformation (symbol_info_of_def d))
