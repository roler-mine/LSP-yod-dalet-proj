module T = Lsp.Types
open Ast
open Workspace_foundation
open Workspace_state
open Workspace_index_graph

let diag_parse_guard ~(file : string option) ~(max_bytes : int)
    ~(actual_bytes : int) : T.Diagnostic.t =
  let z = { Ast.Loc.line = 1; col = 0; offset = 0 } in
  let loc = Ast.Loc.make ~file ~start_pos:z ~end_pos:z in
  Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"parse"
    ~message:
      (Printf.sprintf "File parse skipped (%d bytes exceeds guard %d bytes)."
         actual_bytes max_bytes)
    loc

let make_doc_with_parse_guard ?lsp_version (ws : t)
    ~(uri : T.DocumentUri.t) ~(file : string option) ~(text : string)
    ~(actual_bytes : int) : Document.t =
  Perf_stats.tick "parse.large_file_guard";
  Document.make_parse_skipped_versioned ~lsp_version ~uri ~file ~text
    ~parse_diags:
      [
        diag_parse_guard ~file ~max_bytes:ws.parse_file_max_bytes ~actual_bytes;
      ]

let parse_guarded_document_make ?lsp_version
    ?(profile = Parser.Interactive) (ws : t) ~(uri : T.DocumentUri.t)
    ~(file : string option) ~(text : string) : Document.t =
  if
    is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes
      ~text_len:(String.length text)
  then
    make_doc_with_parse_guard ?lsp_version ws ~uri ~file ~text
      ~actual_bytes:(String.length text)
  else Document.make_with_profile_versioned ~lsp_version ~profile ~uri ~file ~text

let diag_missing_compool (loc : Ast.Loc.t) (name : string) : T.Diagnostic.t =
  Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"import"
    ~message:("Missing COMPOOL: " ^ name)
    loc

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
  else
    ws.source_file_paths
    |> List.find_opt (fun path ->
           match
             source_stem_of_filename ~source_extensions:ws.source_extensions
               (Filename.basename path)
           with
           | Some stem -> normalize_name stem = key
           | None -> false)

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
              if allow_fallback_scan ws then
                match find_compool_path_fallback ws ~key with
                | Some _ -> true
                | None -> false
              else false)
      | None -> (
          match
            if allow_fallback_scan ws then find_compool_path_fallback ws ~key
            else None
          with
          | Some _ -> true
          | None -> false))

type compool_import_dir = {
  compool : string;
  selected : (string * Ast.Loc.t) list; (* imported element name + location *)
}

let diag_missing_import_hint ~(loc : Ast.Loc.t) ~(kind : string)
    ~(symbol : string) ~(compools : string list) : T.Diagnostic.t =
  let targets =
    match compools with [] -> "" | [ c ] -> c | xs -> String.concat ", " xs
  in
  let msg =
    if targets = "" then
      Printf.sprintf "%s %S may require a COMPOOL import." kind symbol
    else
      Printf.sprintf
        "%s %S is available in COMPOOL %s. Import it with !COMPOOL '%s' (or \
         selective import)."
        kind symbol targets (List.hd compools)
  in
  Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Warning ~source:"import"
    ~message:msg loc

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
  | "PROC" | "ITEM" | "TABLE" | "TYPE" | "IF" | "THEN" | "ELSE" | "WHILE"
  | "FOR" | "BY" | "CASE" | "DEFAULT" | "FALLTHRU" | "EXIT" | "GOTO" | "RETURN"
  | "ABORT" | "STOP" | "TRUE" | "FALSE" | "NOT" | "AND" | "OR" | "XOR" | "EQV"
  | "MOD" | "PROGRAM" | "COMPOOL" | "ICOMPOOL" | "DEFINE" | "BLOCK" | "ICOPY"
  | "ISKIP" | "IBEGIN" | "IEND" | "ILINKAGE" | "ITRACE" | "IINTERFERENCE"
  | "IREDUCIBLE" | "ILIST" | "INOLIST" | "IEJECT" | "IBASE" | "IISBASE"
  | "IDROP" | "ILEFTRIGHT" | "IREARRANGE" | "IINITIALIZE" | "IORDER" | "REC"
  | "RENT" | "LISTEXP" | "LISTINV" | "LISTBOTH" | "INLINE" | "INSTANCE"
  | "LABEL" | "LIKE" | "OVERLAY" | "PARALLEL" | "POS" | "NULL" ->
      true
  | _ -> false

let extract_compool_import_dirs (doc : Document.t) : compool_import_dir list =
  let doc = Document.ensure_parsed doc in
  match Document.current_parse doc with
  | Some { Document.parsed_ast = Some prog; _ } ->
      let from_decl (d : Ast.decl Ast.node) : compool_import_dir option =
        match d.v with
        | Ast.DDirective { name; args = first :: rest } ->
            let dn = normalize_name name.v in
            if dn = "COMPOOL" || dn = "ICOMPOOL" then
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
      prog
      |> List.filter_map (function
        | Ast.TopDecl d -> from_decl d
        | Ast.TopStmt _ -> None)
  | _ -> []

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
        match file_size_bytes path with
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
                if allow_fallback_scan ws then
                  find_compool_path_fallback ws ~key
                else None)
        | None ->
            if allow_fallback_scan ws then find_compool_path_fallback ws ~key
            else None
      in
      match path_opt with
      | None -> None
      | Some path -> doc_from_path_cached ws path)

let validate_imports ?(pump_lookup : bool = true) (ws : t) (doc : Document.t) :
    T.Diagnostic.t list =
  let pre_imports = Document.imports doc in
  let has_compool_import = pre_imports <> [] in
  if has_compool_import && pump_lookup then pump_index_lookup ws;
  let missing_compools =
    pre_imports
    |> List.filter_map (fun (imp : Preprocess.import) ->
        match imp.kind with
        | Preprocess.Compool ->
            if has_compool_target ws imp.name then None
            else Some (diag_missing_compool imp.loc imp.name))
  in
  missing_compools
