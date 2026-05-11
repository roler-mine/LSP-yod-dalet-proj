module T = Lsp.Types
open Ast
open Workspace_foundation
open Workspace_state
open Workspace_runtime
open Workspace_index_graph
open Workspace_imports
open Workspace_nav_model
open Workspace_symbol_kinds
open Workspace_tuning
module Metadata = Workspace_symbol_metadata

let display_path_of_def (d : def) : string =
  match d.loc.Ast.Loc.file with
  | Some p -> p
  | None -> (
      match Uri_path.file_path_of_uri d.uri with
      | Some p -> p
      | None -> Uri_path.docuri_to_string d.uri)

let file_line_of_def (d : def) : string =
  let f = Filename.basename (display_path_of_def d) in
  Printf.sprintf "%s:%d" f d.loc.Ast.Loc.start_pos.line

let source_path_fact_for_def (d : def) : string =
  Printf.sprintf "Source: `%s`" (display_path_of_def d)

let declaration_location_fact_for_def (d : def) : string =
  let f = Filename.basename (display_path_of_def d) in
  Printf.sprintf "Declared at: `%s:%d:%d`" f d.loc.Ast.Loc.start_pos.line
    (d.loc.Ast.Loc.start_pos.col + 1)

let is_name_start_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '$' | '_' -> true
  | _ -> false

let is_valid_rename_name (s : string) : bool =
  let n = String.length s in
  if n = 0 || not (is_name_start_char s.[0]) then false
  else
    let rec loop i =
      if i >= n then true
      else if is_ident_char s.[i] then loop (i + 1)
      else false
    in
    loop 1

let truncate_text (max_len : int) (s : string) : string =
  let n = String.length s in
  if n <= max_len || max_len < 4 then s
  else String.sub s 0 (max_len - 3) ^ "..."

let line_text_in_doc (doc : Document.t) ~(line1 : int) : string option =
  let line0 = line1 - 1 in
  match Text_index.line_start_offset doc.Document.index ~line:line0 with
  | None -> None
  | Some start ->
      let stop =
        match
          Text_index.line_start_offset doc.Document.index ~line:(line0 + 1)
        with
        | Some s -> s
        | None -> String.length doc.Document.text
      in
      let len = max 0 (stop - start) in
      if len = 0 then Some ""
      else
        let raw = String.sub doc.Document.text start len in
        let n = String.length raw in
        let n = if n > 0 && raw.[n - 1] = '\n' then n - 1 else n in
        let n = if n > 0 && raw.[n - 1] = '\r' then n - 1 else n in
        Some (String.trim (if n <= 0 then "" else String.sub raw 0 n))

let strip_boundary_quotes (s : string) : string =
  let n = String.length s in
  let rec left i =
    if i < n && (s.[i] = '\'' || s.[i] = '"') then left (i + 1) else i
  in
  let rec right i =
    if i > 0 && (s.[i - 1] = '\'' || s.[i - 1] = '"') then right (i - 1)
    else i
  in
  let a = left 0 in
  let b = right n in
  if b <= a then "" else String.sub s a (b - a)

let normalize_import_cursor_name (s : string) : string =
  normalize_name (strip_boundary_quotes s)

let import_named_on_line (doc : Document.t) ~(line1 : int) ~(key : string) :
    Preprocess.import option =
  Document.imports doc
  |> List.find_opt (fun (imp : Preprocess.import) ->
         normalize_name imp.name = key && imp.loc.start_pos.line = line1)

let import_from_line_text (doc : Document.t) (pos : T.Position.t) :
    Preprocess.import option =
  let line1 = pos.T.Position.line + 1 in
  match line_text_in_doc doc ~line1 with
  | None -> None
  | Some line ->
      let tokens =
        tokenize_ident_words line
        |> List.map (fun (w, _, _) -> normalize_import_cursor_name w)
      in
      Document.imports doc
      |> List.find_opt (fun (imp : Preprocess.import) ->
             imp.loc.start_pos.line = line1
             && List.exists
                  (fun tok -> tok <> "" && tok = normalize_name imp.name)
                  tokens)

let import_under_cursor (doc : Document.t) (pos : T.Position.t) :
    Preprocess.import option =
  match
    Document.imports doc
    |> List.find_opt (fun (imp : Preprocess.import) ->
           position_in_loc pos imp.loc)
  with
  | Some _ as hit -> hit
  | None ->
      let line_hit = import_from_line_text doc pos in
      let word_hit =
        match word_at_position doc pos with
        | None -> None
        | Some (name, loc) ->
            let key = normalize_import_cursor_name name in
            if key = "" then None
            else import_named_on_line doc ~line1:loc.start_pos.line ~key
      in
      (match word_hit with Some _ as hit -> hit | None -> line_hit)

let doc_of_uri (ws : t) (uri : T.DocumentUri.t) : Document.t option =
  Hashtbl.find_opt ws.docs uri

let line_text_in_file ~(path : string) ~(line1 : int) : string option =
  if line1 <= 0 then None
  else
    try
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop cur =
            if cur >= line1 then Some (String.trim (input_line ic))
            else (
              ignore (input_line ic);
              loop (cur + 1))
          in
          loop 1)
    with _ -> None

let source_line_file_cache : (string, string option) Hashtbl.t =
  Hashtbl.create 256

let line_text_in_file_cached ~(path : string) ~(line1 : int) : string option =
  if line1 <= 0 then None
  else
    let key =
      try
        let st = Unix.stat path in
        Printf.sprintf "%s|%d|%.0f|%d" path line1 st.Unix.st_mtime
          st.Unix.st_size
      with _ -> Printf.sprintf "%s|%d|missing" path line1
    in
    match Hashtbl.find_opt source_line_file_cache key with
    | Some cached -> cached
    | None ->
        if Hashtbl.length source_line_file_cache > 512 then
          Hashtbl.clear source_line_file_cache;
        let line = line_text_in_file ~path ~line1 in
        Hashtbl.replace source_line_file_cache key line;
        line

let source_line_for_def_cached_only (ws : t) (d : def) : string option =
  let line1 = d.loc.Ast.Loc.start_pos.line in
  match doc_of_uri ws d.uri with
  | Some d0 -> line_text_in_doc d0 ~line1
  | None -> (
      match d.loc.Ast.Loc.file with
      | None -> None
      | Some p -> (
          match doc_at_path_cached ws p with
          | Some d0 -> line_text_in_doc d0 ~line1
          | None ->
              enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"source_line"
                ~high:true p;
              None))

let source_line_for_def (ws : t) (d : def) : string option =
  let line1 = d.loc.Ast.Loc.start_pos.line in
  match doc_of_uri ws d.uri with
  | Some d0 -> line_text_in_doc d0 ~line1
  | None -> (
      match d.loc.Ast.Loc.file with
      | None -> None
      | Some p -> (
          match doc_at_path_cached ws p with
          | Some d0 -> line_text_in_doc d0 ~line1
          | None ->
              enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"source_line"
                ~high:true p;
              line_text_in_file_cached ~path:p ~line1))

let source_line_for_def_line (ws : t) (d : def) ~(line1 : int) : string option
    =
  match doc_of_uri ws d.uri with
  | Some d0 -> line_text_in_doc d0 ~line1
  | None -> (
      match d.loc.Ast.Loc.file with
      | None -> None
      | Some p -> (
          match doc_at_path_cached ws p with
          | Some d0 -> line_text_in_doc d0 ~line1
          | None -> line_text_in_file_cached ~path:p ~line1))

let source_proc_body_after_def (ws : t) (d : def) : bool =
  let rec scan line1 remaining =
    if remaining <= 0 then false
    else
      match source_line_for_def_line ws d ~line1 with
      | None -> false
      | Some line ->
          let trimmed = String.trim line in
          if trimmed = "" then scan (line1 + 1) (remaining - 1)
          else
            let toks =
              tokenize_ident_words trimmed
              |> List.map (fun (w, _, _) -> normalize_name w)
            in
            match toks with
            | "BEGIN" :: _ -> true
            | ("PROC" | "DEF" | "REF" | "TERM" | "ITEM" | "TABLE" | "TYPE")
              :: _ ->
                false
            | _ -> false
  in
  scan (d.loc.Ast.Loc.start_pos.line + 1) 8

let expr_to_compact_string = Metadata.expr_display
let type_expr_to_compact_string = Metadata.type_display

let param_to_signature_piece (p : Ast.param Ast.node) : string =
  let name = p.v.pname.v in
  let ty =
    match p.v.ptype.v with
    | Ast.TName id when normalize_name id.v = "__IMPLICIT__" -> None
    | _ -> Some (type_expr_to_compact_string p.v.ptype)
  in
  let mode =
    match p.v.pmode with
    | Ast.In -> ""
    | Ast.Out -> "OUT "
    | Ast.InOut -> "INOUT "
  in
  match ty with None -> mode ^ name | Some t -> mode ^ name ^ " " ^ t

let proc_use_suffix (u : Ast.proc_use) : string =
  match u with
  | Ast.UseNormal -> ""
  | Ast.UseRec -> " REC"
  | Ast.UseRent -> " RENT"

let proc_signature_of_proc (p : Ast.proc Ast.node) : string =
  let params =
    p.v.params |> List.map param_to_signature_piece |> String.concat ", "
  in
  let ret =
    match p.v.returns with
    | None -> ""
    | Some r -> " " ^ type_expr_to_compact_string r
  in
  Printf.sprintf "PROC %s(%s)%s%s;" p.v.name.v params ret
    (proc_use_suffix p.v.use_attr)

let proc_signature_for_def (ws : t) (d : def) : string option =
  if d.kind <> sym_kind_func then None
  else
    let doc_opt =
      match doc_of_uri ws d.uri with
      | Some d0 -> Some d0
      | None -> (
          match d.loc.Ast.Loc.file with
          | Some p -> (
              match doc_at_path_cached ws p with
              | Some d0 -> Some d0
              | None ->
                  enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"signature"
                    ~high:true p;
                  None)
          | None -> None)
    in
    let same_ident_loc (id : Ast.ident) =
      id.loc.start_pos.line = d.loc.start_pos.line
      && id.loc.start_pos.col = d.loc.start_pos.col
      && normalize_name id.v = d.key
    in
    let rec find_in_decl (decl : Ast.decl Ast.node) : Ast.proc Ast.node option =
      match decl.v with
      | Ast.DProc p when same_ident_loc p.v.name -> Some p
      | Ast.DProc p -> find_in_decls p.v.locals
      | _ -> None
    and find_in_decls (decls : Ast.decl Ast.node list) :
        Ast.proc Ast.node option =
      match decls with
      | [] -> None
      | d0 :: tl -> (
          match find_in_decl d0 with
          | Some _ as x -> x
          | None -> find_in_decls tl)
    in
    match doc_opt with
    | None -> None
    | Some d0 -> (
        let d0 = Document.ensure_parsed d0 in
        match Document.current_parse d0 with
        | Some { Document.parsed_ast = Some prog; _ } ->
            let rec find_top = function
              | [] -> None
              | Ast.TopDecl dcl :: tl -> (
                  match find_in_decl dcl with
                  | Some p -> Some (proc_signature_of_proc p)
                  | None -> find_top tl)
              | Ast.TopStmt _ :: tl -> find_top tl
            in
            find_top prog
        | _ -> None)

let source_line_keywords (line : string) : string list =
  tokenize_ident_words line |> List.map (fun (w, _, _) -> normalize_name w)

let source_line_has_keyword (line : string) (kw : string) : bool =
  let key = normalize_name kw in
  List.exists (fun tok -> tok = key) (source_line_keywords line)

let proc_signature_has_return (sig_line : string) : bool =
  match String.index_opt sig_line ')' with
  | None -> false
  | Some close ->
      let tail_start = close + 1 in
      let tail_len = String.length sig_line - tail_start in
      if tail_len <= 0 then false
      else
        let tail = String.sub sig_line tail_start tail_len |> String.trim in
        let tail =
          if String.ends_with ~suffix:";" tail then
            String.sub tail 0 (String.length tail - 1) |> String.trim
          else tail
        in
        let tail_upper = normalize_name tail in
        tail_upper <> "" && tail_upper <> "REC" && tail_upper <> "RENT"

let jovial_kind_for_def (ws : t) (d : def) : string =
  match d.metadata.Metadata.jovial_kind with
  | Metadata.JovialProcedure when d.kind = sym_kind_func -> (
      match proc_signature_for_def ws d with
      | Some sig_line when proc_signature_has_return sig_line -> "function"
      | _ -> (
          match source_line_for_def ws d with
          | Some sig_line when proc_signature_has_return sig_line -> "function"
          | _ -> "procedure"))
  | Metadata.JovialUnknownSymbol when d.kind = sym_kind_func -> (
      match proc_signature_for_def ws d with
      | Some sig_line when proc_signature_has_return sig_line -> "function"
      | _ -> (
          match source_line_for_def ws d with
          | Some sig_line when proc_signature_has_return sig_line -> "function"
          | _ -> "procedure"))
  | Metadata.JovialUnknownSymbol when d.kind = sym_kind_module -> "module"
  | Metadata.JovialUnknownSymbol when d.kind = sym_kind_type -> "type"
  | Metadata.JovialUnknownSymbol when d.kind = sym_kind_field -> "field"
  | Metadata.JovialUnknownSymbol when d.kind = sym_kind_const -> "status constant"
  | Metadata.JovialUnknownSymbol when d.kind = sym_kind_var -> "item"
  | Metadata.JovialUnknownSymbol -> "symbol"
  | kind -> Metadata.symbol_kind_label kind

let semantic_role_for_def (ws : t) (d : def) : string =
  match d.metadata.Metadata.decl_role with
  | Metadata.ExternalDefinition -> "exported external declaration"
  | Metadata.ExternalReferenceImport -> "imported external reference"
  | Metadata.CompoolImport -> "COMPOOL import"
  | Metadata.MacroDefinition -> "macro / define capability"
  | Metadata.MacroUse -> "macro use"
  | Metadata.TypeUse -> "type use"
  | Metadata.UsageReference -> "usage reference"
  | Metadata.RealDeclaration -> (
      match jovial_kind_for_def ws d with
  | "item" -> "scalar data object / variable or constant"
  | "table" -> "structured collection of entries"
  | "block" -> "contiguous data grouping"
  | "type" -> "user-defined type description"
  | "procedure" -> "executable subroutine"
  | "function" -> "value-returning subroutine"
  | "module" -> "separately compiled program unit"
  | "compool" -> "shared declaration module"
  | "define" -> "macro / define capability"
  | "external DEF" -> "exported external declaration"
  | "external REF" -> "imported external reference"
  | "label" -> "statement name / jump target"
  | "status constant" -> "status value / status constant"
      | _ -> role_of_def_kind d.kind)

let cached_doc_for_def (ws : t) (d : def) : Document.t option =
  match doc_of_uri ws d.uri with
  | Some d0 -> Some d0
  | None -> (
      match d.loc.Ast.Loc.file with
      | None -> None
      | Some p -> doc_at_path_cached ws p)

let trim_trailing_blank_lines (lines : string list) : string list =
  let rec drop = function
    | [] -> []
    | line :: tl when String.trim line = "" -> drop tl
    | xs -> xs
  in
  lines |> List.rev |> drop |> List.rev

let implementation_boundary_line (line : string) : bool =
  match source_line_keywords line with
  | "END" :: _ | "TERM" :: _ -> true
  | _ -> false

let implementation_preview_for_def (ws : t) (d : def) ~(max_lines : int) :
    string option =
  if d.kind <> sym_kind_func then None
  else if Metadata.is_external_ref d.metadata then None
  else if d.metadata.Metadata.has_body = Some false then None
  else
    let line1 = d.loc.Ast.Loc.start_pos.line in
    let from_doc doc =
      let max_lines = max 1 max_lines in
      let rec loop n line_no acc =
        if n >= max_lines then List.rev acc
        else
          match line_text_in_doc doc ~line1:line_no with
          | None -> List.rev acc
          | Some line ->
              let acc = line :: acc in
              if implementation_boundary_line line then List.rev acc
              else loop (n + 1) (line_no + 1) acc
      in
      let lines = loop 0 line1 [] |> trim_trailing_blank_lines in
      match lines with [] -> None | _ -> Some (String.concat "\n" lines)
    in
    match cached_doc_for_def ws d with
    | Some doc -> from_doc doc
    | None -> (
        match source_line_for_def ws d with
        | Some line when String.trim line <> "" -> Some line
        | _ -> None)

let find_compool_target (ws : t) ~(name : string) : Document.t option =
  pump_index_lookup ws;
  let key = normalize_name name in
  match find_open_compool_doc_by_key ws key with
  | Some d -> Some d
  | None -> (
      let path_opt =
        match ws.index with
        | Some idx -> (
            match Workspace_index.find_compool idx ~name:key with
            | Some p -> Some p
            | None ->
                if allow_fallback_scan ws then
                  find_compool_path_fallback ws ~key
                else None)
        | None ->
            if allow_fallback_scan ws then find_compool_path_fallback ws ~key
            else None
      in
      match path_opt with
      | None -> None
      | Some path -> (
          match doc_at_path_cached ws path with
          | Some d -> Some d
          | None ->
              enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"compool_target"
                ~high:true path;
              None))

let nav_for_doc_cached (ws : t) (cache : (string, doc_nav) Hashtbl.t)
    (doc : Document.t) : doc_nav =
  let k = Uri_path.docuri_to_string doc.Document.uri in
  match Hashtbl.find_opt cache k with
  | Some nav -> nav
  | None ->
      let stale = doc.Document.parse_rev <> doc.Document.rev in
      if stale then
        Perf_log.record_sync_workspace_nav_rebuild
          ~uri:(Uri_path.docuri_to_string doc.Document.uri)
          ~bytes:(String.length doc.Document.text) ~rev:doc.Document.rev ();
      let nav =
        if stale then (
          Perf_stats.tick "nav.stale_snapshot_empty";
          doc_nav_create ())
        else if ws.sem_store_enabled then (
          match
            Semantic_store.snapshot_for_uri ws.semantic_store
              ~uri:doc.Document.uri
          with
          | Some snap when snap.Semantic_store.Snapshot.doc_rev = doc.Document.parse_rev ->
              Perf_stats.tick "nav.cache_hit";
              doc_nav_of_snapshot snap
          | _ ->
              Perf_stats.tick "nav.cache_miss";
              let nav =
                Perf_stats.time "nav.build" (fun () -> build_doc_nav ws doc)
              in
              if doc.Document.parse_rev = doc.Document.rev then
                upsert_semantic_snapshot_for_doc_with_nav ws doc nav;
              nav)
        else Perf_stats.time "nav.build" (fun () -> build_doc_nav ws doc)
      in
      Hashtbl.replace cache k nav;
      nav

let find_def_for_sym_id (ws : t) (cache : (string, doc_nav) Hashtbl.t)
    ~(docs : Document.t list) ~(sym_id : string) : def option =
  let rec loop = function
    | [] -> None
    | d :: tl -> (
        let nav = nav_for_doc_cached ws cache d in
        match Hashtbl.find_opt nav.defs_by_id sym_id with
        | Some _ as hit -> hit
        | None -> loop tl)
  in
  loop docs

let defs_for_import_cursor (ws : t) (imp : Preprocess.import) : def list =
  match find_compool_target ws ~name:imp.name with
  | None -> []
  | Some d ->
      let defs = collect_doc_defs d in
      let key = normalize_name imp.name in
      let hits =
        List.filter (fun x -> x.key = key && x.kind = sym_kind_module) defs
      in
      if hits <> [] then hits
      else
        let loc =
          let z = { Ast.Loc.line = 1; col = 0; offset = 0 } in
          Ast.Loc.make ~file:d.Document.file ~start_pos:z ~end_pos:z
        in
        [
          {
            uri = d.Document.uri;
            name = imp.name;
            key;
            loc;
            kind = sym_kind_module;
            container = None;
            metadata = metadata_for_module ~compool:true ();
          };
        ]

let fallback_defs_by_name (ws : t) (doc : Document.t) (key : string) : def list
    =
  if key = "" || is_reserved_keyword key then []
  else
    let suppress_ambiguous_fields (defs : def list) : def list =
      let field_count =
        List.fold_left
          (fun n d -> if d.kind = sym_kind_field then n + 1 else n)
          0 defs
      in
      if field_count <= 1 then defs
      else List.filter (fun d -> d.kind <> sym_kind_field) defs
    in
    let collect (docs : Document.t list) : def list =
      docs
      |> List.concat_map (fun d ->
          collect_doc_defs d |> List.filter (fun x -> x.key = key))
      |> uniq_defs
      |> suppress_ambiguous_fields
    in
    let from_semantic_store () : def list =
      if not ws.sem_store_enabled then []
      else
        Semantic_store.sym_ids_for_key ws.semantic_store ~key
        |> List.concat_map (fun sym_id ->
            Semantic_store.defs_for_sym_id ws.semantic_store sym_id)
        |> List.map def_of_snapshot_def
        |> List.filter (fun d -> d.key = key)
        |> uniq_defs
        |> suppress_ambiguous_fields
    in
    let local_hits = collect (docs_for_lookup ws doc) in
    if local_hits <> [] then local_hits
    else
      let sem_hits = from_semantic_store () in
      if sem_hits <> [] then sem_hits else collect (docs_for_rename ws doc)

let allow_unscoped_fallback (doc : Document.t) : bool =
  has_unscoped_fallback_context doc

let is_ref_proc_decl_line (line : string) : bool =
  let toks =
    tokenize_ident_words (normalize_name line) |> List.map (fun (w, _, _) -> w)
  in
  let rec has_ref_proc = function
    | "REF" :: "PROC" :: _ -> true
    | _ :: tl -> has_ref_proc tl
    | [] -> false
  in
  has_ref_proc toks

let is_likely_proc_implementation (ws : t) (d : def) : bool =
  if d.kind <> sym_kind_func then true
  else if Metadata.has_real_implementation d.metadata then true
  else if Metadata.is_external_ref d.metadata then false
  else if d.metadata.Metadata.has_body = Some false then
    source_proc_body_after_def ws d
  else
    match source_line_for_def_cached_only ws d with
    | None -> true
    | Some line -> not (is_ref_proc_decl_line line)

let is_ref_import_def (d : def) : bool =
  Metadata.is_external_ref d.metadata

let is_real_definition_target (ws : t) (d : def) : bool =
  (not (is_ref_import_def d))
  &&
  if d.kind = sym_kind_func then
    Metadata.is_external_def d.metadata || is_likely_proc_implementation ws d
    || d.metadata.Metadata.external_kind = Metadata.ExternalLocal
  else true

let prefer_real_definition_targets (ws : t) (defs : def list) : def list =
  let real = List.filter (is_real_definition_target ws) defs in
  if real = [] then defs else real

let prefer_non_ref_targets (defs : def list) : def list =
  let non_ref = List.filter (fun d -> not (is_ref_import_def d)) defs in
  if non_ref = [] then defs else non_ref

let docuri_of_path_unsafe (path : string) : T.DocumentUri.t =
  match Uri_path.docuri_of_path path with
  | Some u -> u
  | None -> (
      match
        T.DocumentUri.t_of_yojson (`String (Uri_path.file_uri_of_path path))
      with
      | u -> u
      | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))

type quick_proc_scan_hit = {
  qps_name : string;
  qps_s : int;
  qps_e : int;
  qps_external_modifier : Ast.external_modifier;
  qps_returns : Ast.type_expr Ast.node option;
  qps_has_body : bool;
}

let ast_ident_node_of_loc ~(loc : Ast.Loc.t) (name : string) : Ast.ident =
  Ast.node ~loc name

let ast_type_name_node ~(loc : Ast.Loc.t) (name : string) :
    Ast.type_expr Ast.node =
  Ast.node ~loc (Ast.TName (ast_ident_node_of_loc ~loc name))

let ast_expr_name_node ~(loc : Ast.Loc.t) (name : string) : Ast.expr Ast.node =
  Ast.node ~loc (Ast.EName (ast_ident_node_of_loc ~loc name))

let quick_return_type_node ~(file : string option) ~(idx : Text_index.t)
    ~(line_base : int) (line : string) ~(start_col : int) :
    Ast.type_expr Ast.node option =
  let tail =
    if start_col >= String.length line then ""
    else String.sub line start_col (String.length line - start_col)
  in
  let tokens = tokenize_ident_words tail in
  let is_attr tok =
    match normalize_name tok with
    | "REC" | "RENT" | "BEGIN" | "END" | "TERM" -> true
    | _ -> false
  in
  match List.filter (fun (tok, _, _) -> not (is_attr tok)) tokens with
  | [] -> None
  | (name, c0, c1) :: rest ->
      let loc_of_cols c0 c1 =
        loc_of_offsets ~file ~idx ~s:(line_base + start_col + c0)
          ~e:(line_base + start_col + c1)
      in
      let name_loc = loc_of_cols c0 c1 in
      let base = ast_type_name_node ~loc:name_loc name in
      let key = normalize_name name in
      let dim_count =
        match key with
        | "A" -> 2
        | "U" | "S" | "F" | "B" | "C" -> 1
        | _ -> 0
      in
      if dim_count <= 0 then Some base
      else
        let dims =
          rest
          |> List.filter_map (fun (tok, dc0, dc1) ->
                 let k = normalize_name tok in
                 if k = "" || Metadata.is_builtin_type_name k then None
                 else
                   let loc = loc_of_cols dc0 dc1 in
                   Some (ast_expr_name_node ~loc tok))
        in
        let rec take n xs =
          if n <= 0 then []
          else match xs with [] -> [] | x :: tl -> x :: take (n - 1) tl
        in
        let dims = take dim_count dims in
        if dims = [] then Some base
        else
          let end_loc =
            match List.rev dims with
            | last :: _ -> last.Ast.loc
            | [] -> name_loc
          in
          let loc =
            Ast.Loc.make ~file ~start_pos:name_loc.Ast.Loc.start_pos
              ~end_pos:end_loc.Ast.Loc.end_pos
          in
          Some (Ast.node ~loc (Ast.TArray { elem = base; dims }))

let quick_proc_has_body ~(external_modifier : Ast.external_modifier)
    (lines : string array) ~(line_idx : int) ~(line_tail : string) : bool =
  match external_modifier with
  | Ast.RefDecl -> false
  | _ ->
      if String.contains (String.uppercase_ascii line_tail) 'B' then
        let tail_tokens =
          tokenize_ident_words line_tail
          |> List.map (fun (tok, _, _) -> normalize_name tok)
        in
        if List.exists (( = ) "BEGIN") tail_tokens then true
        else
          let rec next n =
            if n >= Array.length lines || n > line_idx + 8 then false
            else
              let trimmed = String.trim lines.(n) in
              if trimmed = "" then next (n + 1)
              else
                let toks =
                  tokenize_ident_words trimmed
                  |> List.map (fun (tok, _, _) -> normalize_name tok)
                in
                match toks with
                | "BEGIN" :: _ -> true
                | ("PROC" | "DEF" | "REF" | "TERM" | "ITEM" | "TABLE" | "TYPE")
                  :: _ ->
                    false
                | _ -> false
          in
          next (line_idx + 1)
      else
        let rec next n =
          if n >= Array.length lines || n > line_idx + 8 then false
          else
            let trimmed = String.trim lines.(n) in
            if trimmed = "" then next (n + 1)
            else
              let toks =
                tokenize_ident_words trimmed
                |> List.map (fun (tok, _, _) -> normalize_name tok)
              in
              match toks with
              | "BEGIN" :: _ -> true
              | ("PROC" | "DEF" | "REF" | "TERM" | "ITEM" | "TABLE" | "TYPE")
                :: _ ->
                  false
              | _ -> false
        in
        next (line_idx + 1)

let quick_proc_hits_in_text ~(path : string) ~(text : string) ~(key : string) :
    quick_proc_scan_hit list =
  let idx = Text_index.of_string text in
  let file = Some path in
  let lines = text |> String.split_on_char '\n' |> Array.of_list in
  let line_base line_idx =
    match Text_index.line_start_offset idx ~line:line_idx with
    | Some off -> off
    | None -> 0
  in
  let hits = ref [] in
  Array.iteri
    (fun line_idx raw_line ->
      let line =
        let n = String.length raw_line in
        if n > 0 && raw_line.[n - 1] = '\r' then String.sub raw_line 0 (n - 1)
        else raw_line
      in
      let tokens = tokenize_ident_words line in
      let rec loop = function
        | [] -> ()
        | ("PROC", _, proc_e) :: (name, name_s, name_e) :: _
        | ("proc", _, proc_e) :: (name, name_s, name_e) :: _
          when normalize_name name = key ->
            let external_modifier =
              match
                tokens
                |> List.filter (fun (_, _, e) -> e <= proc_e - 4)
                |> List.rev
              with
              | (tok, _, _) :: _ when normalize_name tok = "DEF" -> Ast.DefDecl
              | (tok, _, _) :: _ when normalize_name tok = "REF" -> Ast.RefDecl
              | _ -> Ast.LocalDecl
            in
            let base = line_base line_idx in
            let ret_start =
              match
                try Some (String.index_from line name_e ')')
                with Not_found -> None
              with
              | Some close -> close + 1
              | None -> name_e
            in
            let returns =
              quick_return_type_node ~file ~idx ~line_base:base line
                ~start_col:ret_start
            in
            let line_tail =
              if ret_start >= String.length line then ""
              else String.sub line ret_start (String.length line - ret_start)
            in
            let has_body =
              quick_proc_has_body ~external_modifier lines ~line_idx ~line_tail
            in
            hits :=
              {
                qps_name = name;
                qps_s = base + name_s;
                qps_e = base + name_e;
                qps_external_modifier = external_modifier;
                qps_returns = returns;
                qps_has_body = has_body;
              }
              :: !hits
        | _ :: tl -> loop tl
      in
      let upper_tokens =
        tokens |> List.map (fun (tok, s, e) -> (normalize_name tok, s, e))
      in
      loop upper_tokens)
    lines;
  List.rev !hits

let quick_proc_hit_rank (d : def) : int =
  let base =
    match d.metadata.Metadata.external_kind with
    | Metadata.ExternalRef -> 0
    | Metadata.ExternalLocal -> 2
    | Metadata.ExternalDef -> 3
    | Metadata.ExternalSystem -> 1
  in
  let body =
    match d.metadata.Metadata.has_body with Some true -> 10 | _ -> 0
  in
  base + body

let sort_quick_proc_hits (defs : def list) : def list =
  List.sort
    (fun a b -> compare (quick_proc_hit_rank b) (quick_proc_hit_rank a))
    defs

let quick_proc_defs_from_nav_index (ws : t) (doc : Document.t) ~(key : string) :
    def list =
  if key = "" then []
  else
    let current_path_key =
      match doc.Document.file with
      | None -> None
      | Some p -> Some (normalize_path_key p)
    in
    let entries =
      match Hashtbl.find_opt ws.quick_nav_index key with
      | None -> []
      | Some xs -> xs
    in
    entries
    |> List.filter_map (fun e ->
        let same_doc =
          match (Uri_path.file_path_of_uri e.qn_uri, current_path_key) with
          | Some p, Some cur -> normalize_path_key p = cur
          | _ -> false
        in
        if same_doc then None
        else (
          (match Uri_path.file_path_of_uri e.qn_uri with
          | Some p ->
              enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"quick_nav_hit"
                ~high:true p
          | None -> ());
          Some
            {
              uri = e.qn_uri;
              name = e.qn_name;
              key = e.qn_key;
              loc = e.qn_loc;
              kind = e.qn_kind;
              container = e.qn_container;
              metadata = e.qn_metadata;
            }))
    |> uniq_defs

let quick_proc_defs_from_index_sources (ws : t) (doc : Document.t)
    ~(key : string) : def list =
  if
    key = "" || nav_quick_scan_files <= 0
    || nav_quick_scan_total_bytes <= 0
    || nav_quick_scan_per_file_bytes <= 0
  then []
  else
    let indexed_hits = quick_proc_defs_from_nav_index ws doc ~key in
    match ws.index with
      | None -> sort_quick_proc_hits (uniq_defs indexed_hits)
      | Some idx ->
          let profile = workspace_profile_for_budget ws in
          let scan_files_budget =
            match profile with
            | ProfileLarge -> max nav_quick_scan_files 128
            | ProfileMedium -> max nav_quick_scan_files 72
            | ProfileSmall -> nav_quick_scan_files
          in
          let scan_total_budget =
            match profile with
            | ProfileLarge -> max nav_quick_scan_total_bytes 4_194_304
            | ProfileMedium -> max nav_quick_scan_total_bytes 2_359_296
            | ProfileSmall -> nav_quick_scan_total_bytes
          in
          let scan_per_file_budget =
            match profile with
            | ProfileLarge -> max nav_quick_scan_per_file_bytes 393_216
            | ProfileMedium -> nav_quick_scan_per_file_bytes
            | ProfileSmall -> nav_quick_scan_per_file_bytes
          in
          let current_path_key =
            match doc.Document.file with
            | None -> None
            | Some p -> Some (normalize_path_key p)
          in
          ensure_graph_fresh ws;
          let seen_paths = Hashtbl.create 512 in
          let candidate_paths_rev = ref [] in
          let push_path (path : string) =
            let key = normalize_path_key path in
            if key <> "" && not (Hashtbl.mem seen_paths key) then (
              Hashtbl.replace seen_paths key true;
              candidate_paths_rev := path :: !candidate_paths_rev)
          in
          Workspace_index.source_paths_for_proc_hint idx ~name:key
          |> List.iter push_path;
          Array.iter push_path ws.graph_root_closure_paths;
          Workspace_index.all_source_paths idx |> List.iter push_path;
          let candidate_paths = List.rev !candidate_paths_rev in
          let mk_quick_hit ~(path : string) ~(text : string)
              (hit : quick_proc_scan_hit) : def =
            let uri = docuri_of_path_unsafe path in
            let idx = Text_index.of_string text in
            let loc =
              loc_of_offsets ~file:(Some path) ~idx ~s:hit.qps_s
                ~e:hit.qps_e
            in
            {
              uri;
              name = hit.qps_name;
              key;
              loc;
              kind = sym_kind_func;
              container = None;
              metadata =
                metadata_for_proc
                  ~external_modifier:hit.qps_external_modifier
                  ~returns:hit.qps_returns ~has_body:hit.qps_has_body ();
            }
          in
          let rec scan scanned scanned_bytes (acc : def list) = function
            | [] -> sort_quick_proc_hits (uniq_defs acc)
            | _ when scanned >= scan_files_budget ->
                sort_quick_proc_hits (uniq_defs acc)
            | _ when scanned_bytes >= scan_total_budget ->
                sort_quick_proc_hits (uniq_defs acc)
            | path :: tl ->
                let path_key = normalize_path_key path in
                let is_current =
                  match current_path_key with
                  | Some k -> k = path_key
                  | None -> false
                in
                if is_current then scan scanned scanned_bytes acc tl
                else (
                  enqueue_bg_path ws ~lane:LaneRoot
                    ~reason_group:"quick_nav_scan" ~high:true path;
                  if Hashtbl.mem ws.files path_key then
                    scan scanned scanned_bytes acc tl
                  else
                    let offset =
                      match
                        Hashtbl.find_opt ws.nav_quick_scan_offset_by_path
                          path_key
                      with
                      | Some n when n >= 0 -> n
                      | _ -> 0
                    in
                    match
                      read_file_window_text path ~offset
                        ~max_bytes:scan_per_file_budget
                    with
                    | None -> scan (scanned + 1) scanned_bytes acc tl
                    | Some (text, next_offset) -> (
                        Hashtbl.replace ws.nav_quick_scan_offset_by_path
                          path_key next_offset;
                        let text_len = String.length text in
                        if text_len = 0 then
                          scan (scanned + 1) scanned_bytes acc tl
                        else
                          let next_bytes = scanned_bytes + text_len in
                          if next_bytes > scan_total_budget then
                            sort_quick_proc_hits (uniq_defs acc)
                          else
                            let hits =
                              quick_proc_hits_in_text ~path ~text ~key
                              |> List.map (mk_quick_hit ~path ~text)
                            in
                            scan (scanned + 1) next_bytes (hits @ acc) tl))
          in
          scan 0 0 indexed_hits candidate_paths

let proc_defs_by_key (ws : t) (doc : Document.t) ~(key : string) : def list =
  if key = "" then []
  else (
    pump_index_lookup ws;
    let from_local_docs () : def list =
      docs_for_rename ws doc
      |> List.concat_map collect_doc_defs
      |> List.filter (fun d -> d.kind = sym_kind_func && d.key = key)
      |> uniq_defs
    in
    let from_semantic_store () : def list =
      if not ws.sem_store_enabled then []
      else
        Semantic_store.sym_ids_for_key ws.semantic_store ~key
        |> List.concat_map (fun sym_id ->
            Semantic_store.defs_for_sym_id ws.semantic_store sym_id)
        |> List.map def_of_snapshot_def
        |> List.filter (fun d -> d.kind = sym_kind_func && d.key = key)
        |> uniq_defs
    in
    let sem_hits = from_semantic_store () in
    let sem_has_impl =
      List.exists (is_likely_proc_implementation ws) sem_hits
    in
    if sem_has_impl then sem_hits
    else
      let local_hits = from_local_docs () in
      let local_has_impl =
        List.exists (is_likely_proc_implementation ws) local_hits
      in
      if local_has_impl then local_hits
      else
        let quick_hits = quick_proc_defs_from_index_sources ws doc ~key in
        let combined = uniq_defs (sem_hits @ local_hits @ quick_hits) in
        combined)

let proc_impl_defs_by_key (ws : t) (doc : Document.t) ~(key : string) : def list
    =
  let defs = proc_defs_by_key ws doc ~key in
  let impls = List.filter (is_likely_proc_implementation ws) defs in
  impls

let proc_real_defs_by_key (ws : t) (doc : Document.t) ~(key : string) :
    def list =
  proc_defs_by_key ws doc ~key |> prefer_real_definition_targets ws
  |> prefer_non_ref_targets

let perf_stats_json (_ws : t) : Yojson.Safe.t =
  match Perf_stats.snapshot_json () with
  | `Assoc fields -> `Assoc (("syncCounters", Perf_log.counters_json ()) :: fields)
  | other -> other
