(* Module overview: Resolves Jovial COMPOOL, !COPY, and include relationships between files. *)

module T = Lsp.Types
open Ast
open Workspace_foundation
open Workspace_state
open Workspace_index_graph
open Workspace_tuning

let large_workspace_startup_quiet_active (ws : t) : bool =
  (ws.quick_nav_index_total >= 320
  || ws.source_bytes_estimate_count >= 320
  || List.length ws.source_file_paths >= 320)
  &&
  match ws.startup_fully_nav_ready_ms with
  | None -> true
  | Some ready_ms ->
      post_startup_large_parse_idle_quiet_ms > 0
      && Perf_stats.now_ms () -. ready_ms
         < float_of_int post_startup_large_parse_idle_quiet_ms

let diag_missing_compool (loc : Ast.Loc.t) (name : string) : T.Diagnostic.t =
  let key = normalize_name name in
  {
    (Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"import"
       ~message:("Missing COMPOOL: " ^ name)
       loc)
    with
    T.Diagnostic.data =
      Some
        (`Assoc
          [
            ("kind", `String "missingCompool");
            ("compool", `String key);
          ]);
  }

let diag_unresolved_icopy
    (target : Workspace_include_model.include_target) : T.Diagnostic.t =
  let display =
    if target.target <> "" then target.target else target.normalized_target
  in
  {
    (Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"include"
       ~message:("Unresolved !COPY target: " ^ display)
       target.target_loc)
    with
    T.Diagnostic.data =
      Some
        (`Assoc
          [
            ("kind", `String "unresolvedIcopy");
            ("target", `String display);
            ("normalizedTarget", `String target.normalized_target);
          ]);
  }

let diag_cyclic_icopy
    (target : Workspace_include_model.include_target) : T.Diagnostic.t =
  let display =
    if target.target <> "" then target.target else target.normalized_target
  in
  {
    (Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"include"
       ~message:("Cyclic !COPY include detected for " ^ display)
       target.directive_loc)
    with
    T.Diagnostic.data =
      Some
        (`Assoc
          [
            ("kind", `String "cyclicIcopy");
            ("target", `String display);
            ("normalizedTarget", `String target.normalized_target);
          ]);
  }

let has_known_source_ext_name ~(source_extensions : string list) (name : string)
    : bool =
  Source_file.has_extension ~extensions:source_extensions name

let source_stem_of_filename ~(source_extensions : string list) (name : string) :
    string option =
  if not (has_known_source_ext_name ~source_extensions name) then None
  else
    let n = String.length name in
    let rec find_dot i =
      if i < 0 then None else if name.[i] = '.' then Some i else find_dot (i - 1)
    in
    match find_dot (n - 1) with
    | None -> None
    | Some i when i <= 0 -> None
    | Some i -> Some (String.sub name 0 i)

let find_compool_path_fallback (ws : t) ~(key : string) : string option =
  if key = "" then None
  else (
    Perf_stats.tick "query.cross_module.fallback_scan";
    ws.source_file_paths
    |> List.find_opt (fun path ->
           match
             source_stem_of_filename ~source_extensions:ws.source_extensions
               (Filename.basename path)
           with
           | Some stem -> normalize_name stem = key
           | None -> false))

let allow_import_fallback_scan (ws : t) : bool =
  allow_fallback_scan ws
  &&
  match Workspace_runtime.workspace_profile_for_budget ws with
  | ProfileLarge -> ws.startup_fully_nav_ready_ms <> None
  | ProfileSmall | ProfileMedium -> true

let find_open_compool_doc_by_key (ws : t) (key : string) : Document.t option =
  let found = ref None in
  Hashtbl.iter
    (fun _ doc ->
      match (!found, doc.Document.compool_def) with
      | Some _, _ -> ()
      | None, Some nm when normalize_name nm = key -> found := Some doc
      | None, _ -> ())
    ws.docs;
  !found

let has_compool_target (ws : t) (name : string) : bool =
  let key = normalize_name name in
  match find_open_compool_doc_by_key ws key with
  | Some _ -> true
  | None -> (
      match ws.index with
      | Some idx -> (
          match Workspace_index.find_compool idx ~name:key with
          | Some _ -> true
          | None ->
              if allow_import_fallback_scan ws then
                match find_compool_path_fallback ws ~key with
                | Some _ -> true
                | None -> false
              else false)
      | None -> (
         match
            if allow_import_fallback_scan ws then
              find_compool_path_fallback ws ~key
            else None
          with
          | Some _ -> true
          | None -> false))

type compool_import_dir = {
  compool : string;
  selected : (string * Ast.Loc.t) list; (* imported element name + location *)
}

let diag_missing_import_hint ~(selected_imported : bool) ~(loc : Ast.Loc.t)
    ~(kind : string) ~(symbol : string) ~(compools : string list) : T.Diagnostic.t =
  let targets =
    match compools with [] -> "" | [ c ] -> c | xs -> String.concat ", " xs
  in
  let msg =
    if targets = "" then
      Printf.sprintf "%s %S may require a COMPOOL import." kind symbol
    else if selected_imported then
      Printf.sprintf
        "%s %S is available in COMPOOL %s but is not in the selective import \
         list. Add it to !COMPOOL '%s'."
        kind symbol targets (List.hd compools)
    else
      Printf.sprintf
        "%s %S is available in COMPOOL %s. Import it with !COMPOOL '%s' (or \
         selective import)."
        kind symbol targets (List.hd compools)
  in
  let compools = compools |> List.map normalize_name |> List.filter (( <> ) "") in
  {
    (Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Warning ~source:"import"
       ~message:msg loc)
    with
    T.Diagnostic.data =
      Some
        (`Assoc
          [
            ("kind", `String "missingImportHint");
            ("symbolKind", `String kind);
            ("symbol", `String symbol);
            ("compools", `List (List.map (fun c -> `String c) compools));
          ]);
  }

let is_builtin_type (k : string) : bool = Keyword.is_builtin_type_name k

let is_builtin_function_name (name : string) : bool =
  match normalize_name name with
  | "LOC" | "NEXT" | "BIT" | "BYTE" | "SHIFTL" | "SHIFTR" | "ABS" | "SGN"
  | "BITSIZE" | "BYTESIZE" | "WORDSIZE" | "LBOUND" | "UBOUND" | "NWDSEN"
  | "FIRST" | "LAST" | "REP" | "V" ->
      true
  | _ -> false

let is_control_stmt_keyword (name : string) : bool =
  match normalize_name name with
  | "EXIT" | "ABORT" | "STOP" -> true
  | _ -> false

let is_reserved_keyword (name : string) : bool =
  match normalize_name name with
  | "START" | "TERM" | "BEGIN" | "END" | "DEF" | "REF" | "STATIC" | "CONSTANT"
  | "READONLY" | "PROC" | "ITEM" | "TABLE" | "TYPE" | "IF" | "THEN" | "ELSE" | "WHILE"
  | "FOR" | "BY" | "CASE" | "DEFAULT" | "FALLTHRU" | "EXIT" | "GOTO" | "RETURN"
  | "ABORT" | "STOP" | "TRUE" | "FALSE" | "NOT" | "AND" | "OR" | "XOR" | "EQV"
  | "MOD" | "PROGRAM" | "COMPOOL" | "ICOMPOOL" | "DEFINE" | "BLOCK" | "ICOPY"
  | "ISKIP" | "IBEGIN" | "IEND" | "ILINKAGE" | "ITRACE" | "IINTERFERENCE"
  | "IREDUCIBLE" | "ILIST" | "INOLIST" | "IEJECT" | "IBASE" | "IISBASE"
  | "IDROP" | "ILEFTRIGHT" | "IREARRANGE" | "IINITIALIZE" | "IORDER" | "REC"
  | "COPY" | "SKIP" | "LIST" | "NOLIST" | "EJECT" | "BASE" | "ISBASE"
  | "DROP" | "LEFTRIGHT" | "REARRANGE" | "INITIALIZE" | "ORDER" | "LINKAGE"
  | "TRACE" | "INTERFERENCE" | "REDUCIBLE"
  | "RENT" | "LISTEXP" | "LISTINV" | "LISTBOTH" | "INLINE" | "INSTANCE"
  | "LABEL" | "LIKE" | "OVERLAY" | "PARALLEL" | "POS" | "NULL" ->
      true
  | _ -> false

let trim_import_token (s : string) : string =
  let s = String.trim s in
  let n = String.length s in
  if
    n >= 2
    && ((s.[0] = '\'' && s.[n - 1] = '\'')
       || (s.[0] = '"' && s.[n - 1] = '"'))
  then String.sub s 1 (n - 2)
  else s

let import_loc_of_cols (doc : Document.t) ~(line0 : int) ~(c0 : int) ~(c1 : int)
    : Ast.Loc.t =
  let base =
    match Text_index.line_start_offset doc.Document.index ~line:line0 with
    | Some n -> n
    | None -> 0
  in
  let start_pos = { Ast.Loc.line = line0 + 1; col = c0; offset = base + c0 } in
  let end_pos = { Ast.Loc.line = line0 + 1; col = c1; offset = base + c1 } in
  Ast.Loc.make ~file:doc.Document.file ~start_pos ~end_pos

let import_tokens_of_line ~(line0 : int) (line : string) :
    (string * int * int * int) list =
  let n = String.length line in
  let is_delim = function
    | ' ' | '\t' | '\r' | '\n' | '(' | ')' | ',' | ';' -> true
    | _ -> false
  in
  let rec loop i acc =
    if i >= n then List.rev acc
    else if is_delim line.[i] then loop (i + 1) acc
    else
      let j = ref i in
      while !j < n && not (is_delim line.[!j]) do
        incr j
      done;
      loop !j ((String.sub line i (!j - i), line0, i, !j) :: acc)
  in
  loop 0 []

let import_marker_key (token : string) : string =
  let token = trim_import_token token in
  Preprocess.canonical_directive_name token

let text_compool_import_dirs (doc : Document.t) : compool_import_dir list =
  let lines = String.split_on_char '\n' doc.Document.text in
  let line_count = List.length lines in
  let line_at i = try List.nth lines i with _ -> "" in
  let line_has_marker line =
    import_tokens_of_line ~line0:0 line
    |> List.exists (fun (tok, _, _, _) ->
           let k = import_marker_key tok in
           k = "COMPOOL")
  in
  let gather_directive start =
    let rec loop i acc =
      if i >= line_count || i >= start + 12 then acc
      else
        let line = line_at i in
        let acc = acc @ import_tokens_of_line ~line0:i line in
        if String.contains line ';' then acc else loop (i + 1) acc
    in
    loop start []
  in
  let dirs = ref [] in
  lines
  |> List.iteri (fun line0 line ->
         if line_has_marker line then (
           let tokens = gather_directive line0 in
           let rec find_marker idx = function
             | [] -> None
             | (tok, _, _, _) :: rest ->
                 let k = import_marker_key tok in
                 if k = "COMPOOL" then Some idx
                 else find_marker (idx + 1) rest
           in
           match find_marker 0 tokens with
           | None -> ()
           | Some idx -> (
               match List.nth_opt tokens (idx + 1) with
               | None -> ()
               | Some (raw_compool, _cline, _cc0, _cc1) ->
                   let compool = normalize_name (trim_import_token raw_compool) in
                   if compool <> "" then
                     let selected =
                       tokens
                       |> List.mapi (fun i tok -> (i, tok))
                       |> List.filter_map (fun (i, (raw, line0, c0, c1)) ->
                              if i <= idx + 1 then None
                              else
                                let key = normalize_name (trim_import_token raw) in
                                if key = "" || is_reserved_keyword key then None
                                else
                                  Some
                                    ( key,
                                      import_loc_of_cols doc ~line0 ~c0 ~c1 ))
                     in
                     dirs :=
                       {
                         compool;
                         selected;
                       }
                       :: !dirs)));
  List.rev !dirs

let extract_compool_import_dirs (doc : Document.t) : compool_import_dir list =
  let doc = Document.ensure_parsed doc in
  let ast_dirs =
    match Document.current_parse doc with
    | Some { Document.parsed_ast = Some prog; _ } ->
      let from_decl (d : Ast.decl Ast.node) : compool_import_dir option =
        match d.v with
        | Ast.DDirective { name; args = first :: rest } ->
            let dn = Preprocess.canonical_directive_name name.v in
            if dn = "COMPOOL" then
              let compool = normalize_name first.v in
              if compool = "" then None
              else
                let selected =
                  rest
                  |> List.filter_map (fun arg ->
                      let k = normalize_name arg.v in
                      if k = "" then None else Some (k, arg.loc))
                in
                Some { compool; selected }
            else None
        | _ -> None
      in
      let rec from_toplevel = function
        | Ast.TopDecl d -> (
            match from_decl d with None -> [] | Some dir -> [ dir ])
        | Ast.TopStmt _ | Ast.TopError _ -> []
        | Ast.TopModule m -> List.concat_map from_toplevel m.v.module_items
      in
      List.concat_map from_toplevel prog
    | _ -> []
  in
  let text_dirs = text_compool_import_dirs doc in
  match text_dirs with
  | [] -> ast_dirs
  | _ ->
      let text_seen = Hashtbl.create 16 in
      List.iter
        (fun (d : compool_import_dir) ->
          Hashtbl.replace text_seen (normalize_name d.compool) true)
        text_dirs;
      let ast_extras =
        ast_dirs
        |> List.filter (fun (d : compool_import_dir) ->
               not (Hashtbl.mem text_seen (normalize_name d.compool)))
      in
      text_dirs @ ast_extras

let read_file_text (path : string) : string option =
  try
    let t0 = Perf_log.now_ms () in
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let txt = really_input_string ic len in
    close_in_noerr ic;
    Perf_log.log_event "file_text_load" ~uri:path ~bytes:len
      ~ms:(max 0.0 (Perf_log.now_ms () -. t0));
    Some txt
  with _ -> None

let read_file_prefix_text (path : string) ~(max_bytes : int) : string option =
  if max_bytes <= 0 then None
  else
    try
      let t0 = Perf_log.now_ms () in
      let ic = open_in_bin path in
      let len = in_channel_length ic in
      let take = min len max_bytes in
      let txt = really_input_string ic take in
      close_in_noerr ic;
      Perf_log.log_event "file_text_load" ~uri:path ~bytes:take
        ~ms:(max 0.0 (Perf_log.now_ms () -. t0));
      Some txt
    with _ -> None

let read_file_window_text (path : string) ~(offset : int) ~(max_bytes : int) :
    (string * int) option =
  if max_bytes <= 0 then None
  else
    try
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let len = in_channel_length ic in
          let start = max 0 (min offset len) in
          seek_in ic start;
          let take = min max_bytes (len - start) in
          if take <= 0 then Some ("", 0)
          else
            let txt = really_input_string ic take in
            let next = if start + take >= len then 0 else start + take in
            Some (txt, next))
    with _ -> None

let doc_from_path_cached_only (ws : t) (path : string) : Document.t option =
  let key = normalize_path_key path in
  match Hashtbl.find_opt ws.files key with
  | Some d ->
      touch_closed_doc_path ws ~path_key:key;
      Some d
  | None -> (
      match find_open_doc_for_path ws ~path with
      | Some d ->
          Hashtbl.replace ws.files key d;
          touch_closed_doc_path ws ~path_key:key;
          evict_closed_docs_if_needed ws;
          Some d
      | None -> None)

let doc_from_path_cached (ws : t) (path : string) : Document.t option =
  match doc_from_path_cached_only ws path with
  | Some d -> Some d
  | None -> (
      let key = normalize_path_key path in
      let uri =
        match Uri_path.docuri_of_path path with
        | Some u -> u
        | None -> (
            match
              T.DocumentUri.t_of_yojson
                (`String (Uri_path.file_uri_of_path path))
            with
            | u -> u
            | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))
      in
      let d_opt =
        if large_workspace_startup_quiet_active ws then
          Some
            (Document.make_parse_skipped ~uri ~file:(Some path) ~text:""
               ~parse_diags:[])
        else
          match file_size_bytes path with
        | Some n when n >= ws.bg_large_file_bytes ->
            Some
              (Document.make_parse_skipped ~uri ~file:(Some path) ~text:""
                 ~parse_diags:[])
        | Some n
          when is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes
                 ~text_len:n ->
            Some
              (make_doc_with_parse_guard ws ~uri ~file:(Some path) ~text:""
                 ~actual_bytes:n)
        | _ -> (
            match read_file_text path with
            | None -> None
            | Some txt ->
                let d =
                  try
                    parse_guarded_document_make ~profile:Parser.Background ws
                      ~uri ~file:(Some path)
                      ~text:txt
                  with exn ->
                    ignore exn;
                    Document.make_with_profile ~profile:Parser.Background ~uri
                      ~file:(Some path) ~text:""
                in
                Some d)
      in
      match d_opt with
      | None -> None
      | Some d ->
          Hashtbl.replace ws.files key d;
          touch_closed_doc_path ws ~path_key:key;
          evict_closed_docs_if_needed ws;
          Some d)

let resolve_compool_doc_uncached (ws : t) ~(name : string) : Document.t option =
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
                if allow_import_fallback_scan ws then
                  find_compool_path_fallback ws ~key
                else None)
        | None ->
            if allow_import_fallback_scan ws then
              find_compool_path_fallback ws ~key
            else None
      in
      match path_opt with
      | None -> None
      | Some path -> doc_from_path_cached ws path)

let read_include_prefix_text (path : string) ~(max_bytes : int) :
    string option =
  if max_bytes <= 0 then None
  else
    try
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let len = in_channel_length ic in
          let take = min len max_bytes in
          Some (really_input_string ic take))
    with _ -> None

let include_cycle_detected (ws : t) ~(importer_path : string)
    (target : Workspace_include_model.include_target) : bool =
  let importer_key = normalize_path_key importer_path in
  match target.resolved_path with
  | None -> false
  | Some resolved_path ->
      let resolved_key = normalize_path_key resolved_path in
      if importer_key <> "" && resolved_key = importer_key then true
      else
        match read_include_prefix_text resolved_path ~max_bytes:65536 with
        | None -> false
        | Some text ->
            Workspace_include_model.include_targets_of_text
              ~file:(Some resolved_path) ~text
            |> List.exists
                 (fun (nested : Workspace_include_model.include_target) ->
                   match
                     resolve_icopy_include_path ws ~importer_path:resolved_path
                       nested.target
                   with
                   | Some path -> normalize_path_key path = importer_key
                   | None -> false)

let validate_imports ?(pump_lookup : bool = true) (ws : t) (doc : Document.t) :
    T.Diagnostic.t list =
  let pre_imports = Document.imports doc in
  let has_compool_import = pre_imports <> [] in
  let raw_icopy_includes = Workspace_include_model.include_targets_of_doc doc in
  let has_icopy_include = raw_icopy_includes <> [] in
  if (has_compool_import || has_icopy_include) && pump_lookup then
    pump_index_lookup ws;
  let missing_compools =
    pre_imports
    |> List.filter_map (fun (imp : Preprocess.import) ->
        match imp.kind with
        | Preprocess.Compool ->
            if has_compool_target ws imp.name then None
            else Some (diag_missing_compool imp.loc imp.name))
  in
  let include_diags =
    match doc.Document.file with
    | None -> []
    | Some importer_path ->
        let include_targets =
          if has_icopy_include then icopy_include_targets_for_doc ws doc else []
        in
        include_targets
        |> List.filter_map
             (fun (target : Workspace_include_model.include_target) ->
               match target.resolved_path with
               | None -> Some (diag_unresolved_icopy target)
               | Some _ ->
                   if include_cycle_detected ws ~importer_path target then
                     Some (diag_cyclic_icopy target)
                   else None)
  in
  missing_compools @ include_diags
