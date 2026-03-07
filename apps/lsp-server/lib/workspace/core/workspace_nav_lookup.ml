module T = Lsp.Types
open Ast
open Workspace_foundation
open Workspace_state
open Workspace_runtime
open Workspace_index_graph
open Workspace_imports
open Workspace_nav_model
open Workspace_tuning

let display_path_of_def (d:def) : string =
  match d.loc.Ast.Loc.file with
  | Some p -> p
  | None ->
      (match Uri_path.file_path_of_uri d.uri with
       | Some p -> p
       | None -> Uri_path.docuri_to_string d.uri)

let file_line_of_def (d:def) : string =
  let f = Filename.basename (display_path_of_def d) in
  Printf.sprintf "%s:%d" f d.loc.Ast.Loc.start_pos.line

let is_name_start_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '$' | '_' -> true
  | _ -> false

let is_valid_rename_name (s:string) : bool =
  let n = String.length s in
  if n = 0 || not (is_name_start_char s.[0]) then false
  else
    let rec loop i =
      if i >= n then true
      else if is_ident_char s.[i] then loop (i + 1)
      else false
    in
    loop 1

let truncate_text (max_len:int) (s:string) : string =
  let n = String.length s in
  if n <= max_len || max_len < 4 then s
  else String.sub s 0 (max_len - 3) ^ "..."

let import_under_cursor (doc:Document.t) (pos:T.Position.t) : Preprocess.import option =
  Document.imports doc
  |> List.find_opt (fun (imp:Preprocess.import) -> position_in_loc pos imp.loc)

let def_to_loc_json (d:def) : Yojson.Safe.t =
  location_json ~uri:d.uri d.loc

let doc_of_uri (ws:t) (uri:T.DocumentUri.t) : Document.t option =
  Hashtbl.find_opt ws.docs uri

let line_text_in_doc (doc:Document.t) ~(line1:int) : string option =
  let line0 = line1 - 1 in
  match Text_index.line_start_offset doc.Document.index ~line:line0 with
  | None -> None
  | Some start ->
      let stop =
        match Text_index.line_start_offset doc.Document.index ~line:(line0 + 1) with
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

let line_text_in_file ~(path:string) ~(line1:int) : string option =
  if line1 <= 0 then None
  else
    try
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop cur =
            if cur >= line1 then
              Some (String.trim (input_line ic))
            else (
              ignore (input_line ic);
              loop (cur + 1)
            )
          in
          loop 1)
    with _ -> None

let source_line_for_def (ws:t) (d:def) : string option =
  let line1 = d.loc.Ast.Loc.start_pos.line in
  match doc_of_uri ws d.uri with
  | Some d0 ->
      line_text_in_doc d0 ~line1
  | None ->
      (match d.loc.Ast.Loc.file with
       | None -> None
       | Some p ->
           (match doc_at_path_cached ws p with
            | Some d0 ->
                line_text_in_doc d0 ~line1
            | None ->
                enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"source_line" ~high:true p;
                line_text_in_file ~path:p ~line1))

let literal_to_string = function
  | Ast.LInt s -> s
  | Ast.LFloat s -> s
  | Ast.LString s -> Printf.sprintf "'%s'" s
  | Ast.LChar c -> Printf.sprintf "'%c'" c
  | Ast.LBool b -> if b then "TRUE" else "FALSE"

let rec expr_to_compact_string (e:Ast.expr Ast.node) : string =
  match e.v with
  | Ast.EName id -> id.v
  | Ast.ELit lit -> literal_to_string lit
  | Ast.EParen x -> "(" ^ expr_to_compact_string x ^ ")"
  | Ast.EUnop { rhs; _ } -> expr_to_compact_string rhs
  | Ast.EBinop { lhs; rhs; _ } ->
      expr_to_compact_string lhs ^ "..." ^ expr_to_compact_string rhs
  | Ast.ECall { callee; args } ->
      let xs = args |> List.map expr_to_compact_string |> String.concat ", " in
      callee.v ^ "(" ^ xs ^ ")"
  | Ast.EIndex { base; index } ->
      let xs = index |> List.map expr_to_compact_string |> String.concat ", " in
      expr_to_compact_string base ^ "(" ^ xs ^ ")"
  | Ast.EField { base; field } ->
      expr_to_compact_string base ^ "." ^ field.v
  | Ast.EAt { field; ptr } ->
      expr_to_compact_string field ^ " @ " ^ expr_to_compact_string ptr
  | Ast.EDeref { ptr } ->
      "@ " ^ expr_to_compact_string ptr

let rec type_expr_to_compact_string (t:Ast.type_expr Ast.node) : string =
  match t.v with
  | Ast.TName id -> id.v
  | Ast.TPointer inner -> "P " ^ type_expr_to_compact_string inner
  | Ast.TArray { elem; dims } ->
      let ds = dims |> List.map expr_to_compact_string |> String.concat "," in
      type_expr_to_compact_string elem ^ "(" ^ ds ^ ")"
  | Ast.TRecord _ -> "RECORD"
  | Ast.TFunc _ -> "FUNC"

let param_to_signature_piece (p:Ast.param Ast.node) : string =
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
  match ty with
  | None -> mode ^ name
  | Some t -> mode ^ name ^ " " ^ t

let proc_use_suffix (u:Ast.proc_use) : string =
  match u with
  | Ast.UseNormal -> ""
  | Ast.UseRec -> " REC"
  | Ast.UseRent -> " RENT"

let proc_signature_of_proc (p:Ast.proc Ast.node) : string =
  let params = p.v.params |> List.map param_to_signature_piece |> String.concat ", " in
  let ret =
    match p.v.returns with
    | None -> ""
    | Some r -> " " ^ type_expr_to_compact_string r
  in
  Printf.sprintf "PROC %s(%s)%s%s;" p.v.name.v params ret (proc_use_suffix p.v.use_attr)

let proc_signature_for_def (ws:t) (d:def) : string option =
  if d.kind <> sym_kind_func then None
  else
    let doc_opt =
      match doc_of_uri ws d.uri with
      | Some d0 -> Some d0
      | None ->
          (match d.loc.Ast.Loc.file with
           | Some p ->
               (match doc_at_path_cached ws p with
                | Some d0 -> Some d0
                | None ->
                    enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"signature" ~high:true p;
                    None)
           | None -> None)
    in
    let same_ident_loc (id:Ast.ident) =
      id.loc.start_pos.line = d.loc.start_pos.line
      && id.loc.start_pos.col = d.loc.start_pos.col
      && normalize_name id.v = d.key
    in
    let rec find_in_decl (decl:Ast.decl Ast.node) : Ast.proc Ast.node option =
      match decl.v with
      | Ast.DProc p when same_ident_loc p.v.name -> Some p
      | Ast.DProc p ->
          find_in_decls p.v.locals
      | _ -> None
    and find_in_decls (decls:Ast.decl Ast.node list) : Ast.proc Ast.node option =
      match decls with
      | [] -> None
      | d0 :: tl ->
          (match find_in_decl d0 with
           | Some _ as x -> x
           | None -> find_in_decls tl)
    in
    match doc_opt with
    | None -> None
    | Some d0 ->
        (match d0.Document.ast with
         | None -> None
         | Some prog ->
             let rec find_top = function
               | [] -> None
               | Ast.TopDecl dcl :: tl ->
                   (match find_in_decl dcl with
                    | Some p -> Some (proc_signature_of_proc p)
                    | None -> find_top tl)
               | Ast.TopStmt _ :: tl -> find_top tl
             in
             find_top prog)

let find_compool_target (ws:t) ~(name:string) : Document.t option =
  pump_index_lookup ws;
  let key = normalize_name name in
  match find_open_compool_doc_by_key ws key with
  | Some d -> Some d
  | None ->
      let path_opt =
        match ws.index with
        | Some idx ->
            (match Workspace_index.find_compool idx ~name:key with
             | Some p -> Some p
             | None ->
                 if allow_fallback_scan ws then
                   find_compool_path_fallback ws ~key
                 else
                   None)
        | None ->
            if allow_fallback_scan ws then find_compool_path_fallback ws ~key else None
      in
      (match path_opt with
       | None -> None
       | Some path ->
           (match doc_at_path_cached ws path with
            | Some d -> Some d
            | None ->
                enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"compool_target" ~high:true path;
                None))

let nav_for_doc_cached
    (ws:t)
    (cache:(string, doc_nav) Hashtbl.t)
    (doc:Document.t)
    : doc_nav =
  let k = Uri_path.docuri_to_string doc.Document.uri in
  match Hashtbl.find_opt cache k with
  | Some nav -> nav
  | None ->
      let nav =
        if ws.sem_store_enabled then
          match Semantic_store.snapshot_for_uri ws.semantic_store ~uri:doc.Document.uri with
          | Some snap when snap.Doc_snapshot.doc_rev = doc.Document.parse_rev ->
              Perf_stats.tick "nav.cache_hit";
              doc_nav_of_snapshot snap
          | _ ->
              Perf_stats.tick "nav.cache_miss";
              let nav = Perf_stats.time "nav.build" (fun () -> build_doc_nav ws doc) in
              if doc.Document.parse_rev = doc.Document.rev then
                upsert_semantic_snapshot_for_doc_with_nav ws doc nav;
              nav
        else
          Perf_stats.time "nav.build" (fun () -> build_doc_nav ws doc)
      in
      Hashtbl.replace cache k nav;
      nav

let find_def_for_sym_id
    (ws:t)
    (cache:(string, doc_nav) Hashtbl.t)
    ~(docs:Document.t list)
    ~(sym_id:string)
    : def option =
  let rec loop = function
    | [] -> None
    | d :: tl ->
        let nav = nav_for_doc_cached ws cache d in
        (match Hashtbl.find_opt nav.defs_by_id sym_id with
         | Some _ as hit -> hit
         | None -> loop tl)
  in
  loop docs

let defs_for_import_cursor (ws:t) (imp:Preprocess.import) : def list =
  match find_compool_target ws ~name:imp.name with
  | None -> []
  | Some d ->
      let defs = collect_doc_defs d in
      let key = normalize_name imp.name in
      let hits = List.filter (fun x -> x.key = key && x.kind = sym_kind_module) defs in
      if hits <> [] then hits
      else
        let loc =
          let z = { Ast.Loc.line = 1; col = 0; offset = 0 } in
          Ast.Loc.make ~file:d.Document.file ~start_pos:z ~end_pos:z
        in
        [ { uri = d.Document.uri; name = imp.name; key; loc; kind = sym_kind_module; container = None } ]

let fallback_defs_by_name (ws:t) (doc:Document.t) (key:string) : def list =
  if key = "" || is_reserved_keyword key then []
  else
    let collect (docs:Document.t list) : def list =
      docs
      |> List.concat_map (fun d ->
           collect_doc_defs d |> List.filter (fun x -> x.key = key))
      |> uniq_defs
    in
    let from_semantic_store () : def list =
      if not ws.sem_store_enabled then []
      else
        Semantic_store.sym_ids_for_key ws.semantic_store ~key
        |> List.concat_map (fun sym_id -> Semantic_store.defs_for_sym_id ws.semantic_store sym_id)
        |> List.map def_of_snapshot_def
        |> List.filter (fun d -> d.key = key)
        |> uniq_defs
    in
    let local_hits = collect (docs_for_lookup ws doc) in
    if local_hits <> [] then local_hits
    else
      let sem_hits = from_semantic_store () in
      if sem_hits <> [] then sem_hits
      else collect (docs_for_rename ws doc)

let allow_unscoped_fallback (doc:Document.t) : bool =
  has_unscoped_fallback_context doc

let is_ref_proc_decl_line (line:string) : bool =
  let toks =
    tokenize_ident_words (normalize_name line)
    |> List.map (fun (w, _, _) -> w)
  in
  let rec has_ref_proc = function
    | "REF" :: "PROC" :: _ -> true
    | _ :: tl -> has_ref_proc tl
    | [] -> false
  in
  has_ref_proc toks

let is_likely_proc_implementation (ws:t) (d:def) : bool =
  if d.kind <> sym_kind_func then true
  else
    match source_line_for_def ws d with
    | None -> true
    | Some line -> not (is_ref_proc_decl_line line)

let substring_matches_at (s:string) ~(sub:string) ~(at:int) : bool =
  let n = String.length s in
  let m = String.length sub in
  if at < 0 || m <= 0 || at + m > n then false
  else
    let rec loop i =
      if i >= m then true
      else if s.[at + i] <> sub.[i] then false
      else loop (i + 1)
    in
    loop 0

let find_substring_from (s:string) ~(sub:string) ~(start:int) : int option =
  let n = String.length s in
  let m = String.length sub in
  if m <= 0 then Some (max 0 start)
  else
    let rec loop i =
      if i + m > n then None
      else if substring_matches_at s ~sub ~at:i then Some i
      else loop (i + 1)
    in
    loop (max 0 start)

let find_proc_def_name_offsets ~(upper_text:string) ~(key:string) : (int * int) option =
  if key = "" then None
  else
    let pat = "DEF PROC " ^ key in
    let key_len = String.length key in
    let rec loop from =
      match find_substring_from upper_text ~sub:pat ~start:from with
      | None -> None
      | Some i ->
          let name_s = i + 9 in
          let name_e = name_s + key_len in
          let before_ok = i = 0 || not (is_ident_char upper_text.[i - 1]) in
          let after_ok =
            name_e >= String.length upper_text || not (is_ident_char upper_text.[name_e])
          in
          if before_ok && after_ok then Some (name_s, name_e)
          else loop (i + 1)
    in
    loop 0

let find_proc_decl_name_offsets ~(upper_text:string) ~(key:string) : (int * int) option =
  if key = "" then None
  else
    let pat = "PROC " ^ key in
    let key_len = String.length key in
    let rec loop from =
      match find_substring_from upper_text ~sub:pat ~start:from with
      | None -> None
      | Some i ->
          let name_s = i + 5 in
          let name_e = name_s + key_len in
          let before_ok = i = 0 || not (is_ident_char upper_text.[i - 1]) in
          let after_ok =
            name_e >= String.length upper_text || not (is_ident_char upper_text.[name_e])
          in
          if before_ok && after_ok then Some (name_s, name_e)
          else loop (i + 1)
    in
    loop 0

let docuri_of_path_unsafe (path:string) : T.DocumentUri.t =
  match Uri_path.docuri_of_path path with
  | Some u -> u
  | None ->
      (match T.DocumentUri.t_of_yojson (`String (Uri_path.file_uri_of_path path)) with
       | u -> u
       | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))

let quick_proc_defs_from_nav_index (ws:t) (doc:Document.t) ~(key:string) : def list =
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
           match Uri_path.file_path_of_uri e.qn_uri, current_path_key with
           | Some p, Some cur -> normalize_path_key p = cur
           | _ -> false
         in
         if same_doc then None
         else (
           (match Uri_path.file_path_of_uri e.qn_uri with
            | Some p -> enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"quick_nav_hit" ~high:true p
            | None -> ());
           Some
             {
               uri = e.qn_uri;
               name = e.qn_name;
               key = e.qn_key;
               loc = e.qn_loc;
               kind = e.qn_kind;
               container = e.qn_container;
             }))
    |> uniq_defs

let quick_proc_defs_from_index_sources (ws:t) (doc:Document.t) ~(key:string) : def list =
  if key = ""
     || nav_quick_scan_files <= 0
     || nav_quick_scan_total_bytes <= 0
     || nav_quick_scan_per_file_bytes <= 0
  then
    []
  else
    let indexed_hits = quick_proc_defs_from_nav_index ws doc ~key in
    if indexed_hits <> [] then indexed_hits
    else
      match ws.index with
      | None -> []
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
        let push_path (path:string) =
          let key = normalize_path_key path in
          if key <> "" && not (Hashtbl.mem seen_paths key) then (
            Hashtbl.replace seen_paths key true;
            candidate_paths_rev := path :: !candidate_paths_rev
          )
        in
        Array.iter push_path ws.graph_root_closure_paths;
        Workspace_index.all_source_paths idx |> List.iter push_path;
        let candidate_paths = List.rev !candidate_paths_rev in
        let mk_quick_hit ~(path:string) ~(text:string) ~(s:int) ~(e:int) : def =
          let uri = docuri_of_path_unsafe path in
          let idx = Text_index.of_string text in
          let loc = loc_of_offsets ~file:(Some path) ~idx ~s ~e in
          { uri; name = key; key; loc; kind = sym_kind_func; container = None }
        in
        let rec scan scanned scanned_bytes (fallback:def option) = function
          | [] ->
              (match fallback with
               | None -> []
               | Some d -> [ d ])
          | _ when scanned >= scan_files_budget -> []
          | _ when scanned_bytes >= scan_total_budget -> []
          | path :: tl ->
              let path_key = normalize_path_key path in
              let is_current =
                match current_path_key with
                | Some k -> k = path_key
                | None -> false
              in
              if is_current then scan scanned scanned_bytes fallback tl
              else (
                enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"quick_nav_scan" ~high:true path;
                if Hashtbl.mem ws.files path_key then scan scanned scanned_bytes fallback tl
                else
                  let offset =
                    match Hashtbl.find_opt ws.nav_quick_scan_offset_by_path path_key with
                    | Some n when n >= 0 -> n
                    | _ -> 0
                  in
                  match read_file_window_text path ~offset ~max_bytes:scan_per_file_budget with
                  | None ->
                      scan (scanned + 1) scanned_bytes fallback tl
                  | Some (text, next_offset) ->
                      Hashtbl.replace ws.nav_quick_scan_offset_by_path path_key next_offset;
                      let text_len = String.length text in
                      if text_len = 0 then
                        scan (scanned + 1) scanned_bytes fallback tl
                      else
                      let next_bytes = scanned_bytes + text_len in
                      if next_bytes > scan_total_budget then
                        (match fallback with
                         | None -> []
                         | Some d -> [ d ])
                      else
                        let upper_text = String.uppercase_ascii text in
                        (match find_proc_def_name_offsets ~upper_text ~key with
                         | Some (s, e) ->
                             [ mk_quick_hit ~path ~text ~s ~e ]
                         | None ->
                             let fallback =
                               match fallback with
                               | Some _ -> fallback
                               | None ->
                                   (match find_proc_decl_name_offsets ~upper_text ~key with
                                    | None -> None
                                    | Some (s, e) -> Some (mk_quick_hit ~path ~text ~s ~e))
                             in
                             scan (scanned + 1) next_bytes fallback tl)
              )
        in
        scan 0 0 None candidate_paths

let proc_defs_by_key (ws:t) (doc:Document.t) ~(key:string) : def list =
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
        |> List.concat_map (fun sym_id -> Semantic_store.defs_for_sym_id ws.semantic_store sym_id)
        |> List.map def_of_snapshot_def
        |> List.filter (fun d -> d.kind = sym_kind_func && d.key = key)
        |> uniq_defs
    in
    let sem_hits = from_semantic_store () in
    let sem_has_impl = List.exists (is_likely_proc_implementation ws) sem_hits in
    if sem_has_impl then sem_hits
    else
      let local_hits = from_local_docs () in
      let local_has_impl = List.exists (is_likely_proc_implementation ws) local_hits in
      if local_has_impl then local_hits
      else
      let quick_hits =
        quick_proc_defs_from_index_sources ws doc ~key
      in
      let combined = uniq_defs (sem_hits @ local_hits @ quick_hits) in
      combined
  )

let proc_impl_defs_by_key (ws:t) (doc:Document.t) ~(key:string) : def list =
  let defs = proc_defs_by_key ws doc ~key in
  let impls = List.filter (is_likely_proc_implementation ws) defs in
  if impls = [] then defs else impls

let perf_stats_json (_ws:t) : Yojson.Safe.t =
  Perf_stats.snapshot_json ()
