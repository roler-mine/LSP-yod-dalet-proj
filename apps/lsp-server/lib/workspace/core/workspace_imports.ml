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

let make_doc_with_parse_guard (ws : t) ~(uri : T.DocumentUri.t)
    ~(file : string option) ~(text : string) ~(actual_bytes : int) : Document.t
    =
  Perf_stats.tick "parse.large_file_guard";
  Document.make_unparsed ~uri ~file ~text
    ~parse_diags:
      [
        diag_parse_guard ~file ~max_bytes:ws.parse_file_max_bytes ~actual_bytes;
      ]

let parse_guarded_document_make (ws : t) ~(uri : T.DocumentUri.t)
    ~(file : string option) ~(text : string) : Document.t =
  if
    is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes
      ~text_len:(String.length text)
  then
    make_doc_with_parse_guard ws ~uri ~file ~text
      ~actual_bytes:(String.length text)
  else Document.make ~uri ~file ~text

let diag_missing_compool (loc : Ast.Loc.t) (name : string) : T.Diagnostic.t =
  Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"import"
    ~message:("Missing COMPOOL: " ^ name)
    loc

let has_known_source_ext_name (name : string) : bool =
  let lower = String.lowercase_ascii name in
  let ends_with ext =
    let n = String.length lower in
    let m = String.length ext in
    n >= m && String.sub lower (n - m) m = ext
  in
  ends_with ".jov" || ends_with ".j73" || ends_with ".jvl" || ends_with ".j"

let source_stem_of_filename (name : string) : string option =
  if not (has_known_source_ext_name name) then None
  else
    let n = String.length name in
    let rec find_dot i =
      if i < 0 then None else if name.[i] = '.' then Some i else find_dot (i - 1)
    in
    match find_dot (n - 1) with
    | None -> None
    | Some i when i <= 0 -> None
    | Some i -> Some (String.sub name 0 i)

let is_ignored_lookup_dir (name : string) : bool =
  name = ".git" || name = "_build" || name = "node_modules" || name = ".vscode"

let find_compool_in_dir_tree ~(key : string) ~(root : string) : string option =
  let rec walk (dir : string) : string option =
    let entries = try Sys.readdir dir |> Array.to_list with _ -> [] in
    let rec loop = function
      | [] -> None
      | name :: tl -> (
          let full = Filename.concat dir name in
          try
            if Sys.is_directory full then
              if is_ignored_lookup_dir name then loop tl
              else match walk full with Some _ as hit -> hit | None -> loop tl
            else
              match source_stem_of_filename name with
              | Some stem when normalize_name stem = key -> Some full
              | _ -> loop tl
          with _ -> loop tl)
    in
    loop entries
  in
  walk root

let find_compool_path_fallback (ws : t) ~(key : string) : string option =
  if key = "" then None
  else
    let dirs = Hashtbl.create 16 in
    let add_dir (d : string) =
      let k = normalize_path_key d in
      if k <> "" then Hashtbl.replace dirs k d
    in
    (match ws.root_path with None -> () | Some root -> add_dir root);
    Hashtbl.iter
      (fun _ doc ->
        match doc.Document.file with
        | None -> ()
        | Some p -> add_dir (Filename.dirname p))
      ws.docs;
    let roots = Hashtbl.fold (fun _ d acc -> d :: acc) dirs [] in
    let rec loop = function
      | [] -> None
      | root :: tl -> (
          match find_compool_in_dir_tree ~key ~root with
          | Some _ as hit -> hit
          | None -> loop tl)
    in
    loop roots

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

let diag_missing_imported_type ~(loc : Ast.Loc.t) ~(item : string)
    ~(typ : string) : T.Diagnostic.t =
  Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"import"
    ~message:
      (Printf.sprintf "Imported item %S requires explicit import of type %S."
         item typ)
    loc

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

let is_builtin_type (k : string) : bool =
  match k with
  | "A" | "B" | "U" | "S" | "F" | "C" | "P" | "W" | "V" | "STATUS" -> true
  | _ -> false

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
  | x when is_builtin_function_name x -> true
  | _ -> false

let extract_compool_import_dirs (doc : Document.t) : compool_import_dir list =
  let doc = Document.ensure_parsed doc in
  match doc.Document.ast with
  | None -> []
  | Some prog ->
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

type dep_info = {
  types : (string, bool) Hashtbl.t; (* explicit type declarations *)
  item_deps : (string, string list) Hashtbl.t;
      (* item/table name -> required type keys *)
}

let dep_info_create () : dep_info =
  { types = Hashtbl.create 64; item_deps = Hashtbl.create 128 }

let rec type_keys_of_type_expr (t : Ast.type_expr Ast.node)
    (acc : (string, bool) Hashtbl.t) : unit =
  match t.v with
  | Ast.TName id ->
      let k = normalize_name id.v in
      if k <> "" && not (is_builtin_type k) then Hashtbl.replace acc k true
  | Ast.TPointer inner -> type_keys_of_type_expr inner acc
  | Ast.TArray { elem; _ } -> type_keys_of_type_expr elem acc
  | Ast.TRecord fields ->
      List.iter (fun f -> type_keys_of_type_expr f.v.ftype acc) fields
  | Ast.TFunc { params; returns } -> (
      List.iter (fun p -> type_keys_of_type_expr p.v.Ast.ptype acc) params;
      match returns with None -> () | Some r -> type_keys_of_type_expr r acc)

let keys_of_type_expr (t : Ast.type_expr Ast.node) : string list =
  let h = Hashtbl.create 8 in
  type_keys_of_type_expr t h;
  Hashtbl.fold (fun k _ xs -> k :: xs) h []

let rec dep_info_add_stmt (info : dep_info) (s : Ast.stmt Ast.node) : unit =
  match s.v with
  | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _ | Ast.SGoto _
    ->
      ()
  | Ast.SDecl d -> dep_info_add_decl info d
  | Ast.SBlock xs -> List.iter (dep_info_add_stmt info) xs
  | Ast.SIf { then_; else_; _ } -> (
      dep_info_add_stmt info then_;
      match else_ with None -> () | Some e -> dep_info_add_stmt info e)
  | Ast.SWhile { body; _ } -> dep_info_add_stmt info body
  | Ast.SFor { init; step; body; _ } ->
      (match init with None -> () | Some i -> dep_info_add_stmt info i);
      (match step with None -> () | Some st -> dep_info_add_stmt info st);
      dep_info_add_stmt info body
  | Ast.SLabel { body; _ } -> dep_info_add_stmt info body

and dep_info_add_decl (info : dep_info) (d : Ast.decl Ast.node) : unit =
  match d.v with
  | Ast.DType { name; defn = _ } ->
      let k = normalize_name name.v in
      if k <> "" then Hashtbl.replace info.types k true
  | Ast.DVar { name; dtype; _ } ->
      let n = normalize_name name.v in
      if n <> "" then Hashtbl.replace info.item_deps n (keys_of_type_expr dtype)
  | Ast.DConst _ -> ()
  | Ast.DDirective _ -> ()
  | Ast.DProc p ->
      List.iter (dep_info_add_decl info) p.v.locals;
      dep_info_add_stmt info p.v.body

let dep_info_of_doc (doc : Document.t) : dep_info =
  let doc = Document.ensure_parsed doc in
  let info = dep_info_create () in
  (match doc.Document.ast with
  | None -> ()
  | Some prog ->
      List.iter
        (function
          | Ast.TopDecl d -> dep_info_add_decl info d
          | Ast.TopStmt s -> dep_info_add_stmt info s)
        prog);
  info

let read_file_text (path : string) : string option =
  try
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let txt = really_input_string ic len in
    close_in_noerr ic;
    Some txt
  with _ -> None

let read_file_prefix_text (path : string) ~(max_bytes : int) : string option =
  if max_bytes <= 0 then None
  else
    try
      let ic = open_in_bin path in
      let len = in_channel_length ic in
      let take = min len max_bytes in
      let txt = really_input_string ic take in
      close_in_noerr ic;
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
                    parse_guarded_document_make ws ~uri ~file:(Some path)
                      ~text:txt
                  with exn ->
                    ignore exn;
                    Document.make ~uri ~file:(Some path) ~text:""
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
  let imports = extract_compool_import_dirs doc in
  let missing_type_imports =
    if imports = [] then []
    else
      let doc_cache : (string, Document.t option) Hashtbl.t =
        Hashtbl.create 16
      in
      let info_cache : (string, dep_info option) Hashtbl.t =
        Hashtbl.create 16
      in

      let get_doc_for_compool (name : string) : Document.t option =
        let key = normalize_name name in
        match Hashtbl.find_opt doc_cache key with
        | Some x -> x
        | None ->
            let x = resolve_compool_doc_uncached ws ~name:key in
            Hashtbl.replace doc_cache key x;
            x
      in

      let get_info_for_compool (name : string) : dep_info option =
        let key = normalize_name name in
        match Hashtbl.find_opt info_cache key with
        | Some x -> x
        | None ->
            let x =
              match get_doc_for_compool key with
              | None -> None
              | Some d -> Some (dep_info_of_doc d)
            in
            Hashtbl.replace info_cache key x;
            x
      in

      let available_types : (string, bool) Hashtbl.t = Hashtbl.create 64 in
      let add_available_type k =
        if k <> "" then Hashtbl.replace available_types (normalize_name k) true
      in

      let self_info = dep_info_of_doc doc in
      Hashtbl.iter (fun tk _ -> add_available_type tk) self_info.types;

      (* Pass 1: collect explicitly imported type names. *)
      List.iter
        (fun imp ->
          match get_info_for_compool imp.compool with
          | None -> ()
          | Some info ->
              if imp.selected = [] then
                Hashtbl.iter (fun tk _ -> add_available_type tk) info.types
              else
                List.iter
                  (fun (nm, _loc) ->
                    if Hashtbl.mem info.types nm then add_available_type nm)
                  imp.selected)
        imports;

      (* Pass 2: for selectively imported items, require explicit import of their types. *)
      let seen : (string, bool) Hashtbl.t = Hashtbl.create 64 in
      let hint_seen : (string, bool) Hashtbl.t = Hashtbl.create 64 in
      let out = ref [] in
      let add_diag_once (loc : Ast.Loc.t) ~(item : string) ~(typ : string) =
        let k =
          Printf.sprintf "%s|%d|%d|%s|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line loc.start_pos.col item typ
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := diag_missing_imported_type ~loc ~item ~typ :: !out)
      in
      let add_type_hint_once (loc : Ast.Loc.t) ~(typ : string) =
        let k =
          Printf.sprintf "%s|%d|%d|type-hint|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line loc.start_pos.col typ
        in
        if not (Hashtbl.mem hint_seen k) then (
          Hashtbl.replace hint_seen k true;
          out :=
            diag_missing_import_hint ~loc ~kind:"Type" ~symbol:typ ~compools:[]
            :: !out)
      in
      List.iter
        (fun imp ->
          if imp.selected <> [] then
            match get_info_for_compool imp.compool with
            | None -> ()
            | Some info ->
                List.iter
                  (fun (sel_name, sel_loc) ->
                    match Hashtbl.find_opt info.item_deps sel_name with
                    | None -> ()
                    | Some deps ->
                        List.iter
                          (fun dep ->
                            let dep = normalize_name dep in
                            if
                              dep <> "" && not (Hashtbl.mem available_types dep)
                            then (
                              add_diag_once sel_loc ~item:sel_name ~typ:dep;
                              add_type_hint_once sel_loc ~typ:dep))
                          deps)
                  imp.selected)
        imports;
      List.rev !out
  in
  missing_compools @ missing_type_imports
