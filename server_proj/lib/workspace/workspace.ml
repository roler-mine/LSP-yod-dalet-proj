module T = Lsp.Types
open Ast

type t = {
  docs : (T.DocumentUri.t, Document.t) Hashtbl.t;
  files : (string, Document.t) Hashtbl.t;  (* normalized file path -> parsed document *)
  mutable root_path : string option;
  mutable index : Workspace_index.t option;
  mutable symbol_hints :
    ((string, string list) Hashtbl.t * (string, string list) Hashtbl.t) option;
  (* values map + types map: symbol key -> candidate compool names *)
}

let create () : t =
  {
    docs = Hashtbl.create 32;
    files = Hashtbl.create 64;
    root_path = None;
    index = None;
    symbol_hints = None;
  }

let normalize_name (s:string) : string =
  String.uppercase_ascii (String.trim s)

let normalize_path_key (p:string) : string =
  let p = String.map (fun c -> if c = '\\' then '/' else c) p in
  if Sys.win32 then String.lowercase_ascii p else p

let same_path a b =
  normalize_path_key a = normalize_path_key b

let set_root_path (ws:t) (root:string option) : unit =
  ws.root_path <- root

let set_root_uri (ws:t) (root_uri:T.DocumentUri.t option) : unit =
  ws.root_path <- (match root_uri with None -> None | Some u -> Uri_path.file_path_of_uri u)

let index_bootstrap_dirs = 64
let index_bootstrap_files = 6000
let index_background_dirs = 4
let index_background_files = 200
let index_lookup_dirs = 12
let index_lookup_files = 600

let ensure_index_started (ws:t) : unit =
  match ws.root_path, ws.index with
  | Some root, None ->
      ws.index <- Some (Workspace_index.start ~root)
  | _ ->
      ()

let pump_index (ws:t) ~(max_dirs:int) ~(max_files:int) : unit =
  ensure_index_started ws;
  match ws.index with
  | None -> ()
  | Some idx ->
      (try
         ignore (Workspace_index.scan_step idx ~max_dirs ~max_files)
       with _ -> ())

let pump_index_background (ws:t) : unit =
  pump_index ws ~max_dirs:index_background_dirs ~max_files:index_background_files

let rescan (ws:t) : unit =
  Hashtbl.clear ws.files;
  ws.symbol_hints <- None;
  match ws.root_path with
  | None -> ws.index <- None
  | Some root ->
      let idx = Workspace_index.start ~root in
      ws.index <- Some idx;
      (try
         ignore
           (Workspace_index.scan_step idx
              ~max_dirs:index_bootstrap_dirs
              ~max_files:index_bootstrap_files)
       with _ -> ())

let compool_count (ws:t) : int =
  pump_index_background ws;
  match ws.index with None -> 0 | Some idx -> Workspace_index.compool_count idx

let diag_missing_compool (loc:Ast.Loc.t) (name:string) : T.Diagnostic.t =
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Error
    ~source:"import"
    ~message:("Missing COMPOOL: " ^ name)
    loc

let has_known_source_ext_name (name:string) : bool =
  let lower = String.lowercase_ascii name in
  let ends_with ext =
    let n = String.length lower in
    let m = String.length ext in
    n >= m && String.sub lower (n - m) m = ext
  in
  ends_with ".jov" || ends_with ".j73" || ends_with ".jvl" || ends_with ".j"

let source_stem_of_filename (name:string) : string option =
  if not (has_known_source_ext_name name) then None
  else
    let n = String.length name in
    let rec find_dot i =
      if i < 0 then None
      else if name.[i] = '.' then Some i
      else find_dot (i - 1)
    in
    match find_dot (n - 1) with
    | None -> None
    | Some i when i <= 0 -> None
    | Some i -> Some (String.sub name 0 i)

let is_ignored_lookup_dir (name:string) : bool =
  name = ".git" || name = "_build" || name = "node_modules" || name = ".vscode"

let find_compool_in_dir_tree ~(key:string) ~(root:string) : string option =
  let rec walk (dir:string) : string option =
    let entries =
      try Sys.readdir dir |> Array.to_list
      with _ -> []
    in
    let rec loop = function
      | [] -> None
      | name :: tl ->
          let full = Filename.concat dir name in
          (try
             if Sys.is_directory full then
               if is_ignored_lookup_dir name then loop tl
               else
                 (match walk full with
                  | Some _ as hit -> hit
                  | None -> loop tl)
             else
               match source_stem_of_filename name with
               | Some stem when normalize_name stem = key -> Some full
               | _ -> loop tl
           with _ -> loop tl)
    in
    loop entries
  in
  walk root

let find_compool_path_fallback (ws:t) ~(key:string) : string option =
  if key = "" then None
  else
    let dirs = Hashtbl.create 16 in
    let add_dir (d:string) =
      let k = normalize_path_key d in
      if k <> "" then Hashtbl.replace dirs k d
    in
    (match ws.root_path with
     | None -> ()
     | Some root -> add_dir root);
    Hashtbl.iter (fun _ doc ->
      match doc.Document.file with
      | None -> ()
      | Some p -> add_dir (Filename.dirname p)
    ) ws.docs;
    let roots = Hashtbl.fold (fun _ d acc -> d :: acc) dirs [] in
    let rec loop = function
      | [] -> None
      | root :: tl ->
          (match find_compool_in_dir_tree ~key ~root with
           | Some _ as hit -> hit
           | None -> loop tl)
    in
    loop roots

let find_open_compool_doc_by_key (ws:t) (key:string) : Document.t option =
  let found = ref None in
  Hashtbl.iter (fun _ doc ->
    match !found, doc.Document.compool_def with
    | Some _, _ -> ()
    | None, Some nm when normalize_name nm = key -> found := Some doc
    | None, _ -> ()
  ) ws.docs;
  !found

let has_compool_target (ws:t) (name:string) : bool =
  pump_index ws ~max_dirs:index_lookup_dirs ~max_files:index_lookup_files;
  let key = normalize_name name in
  match find_open_compool_doc_by_key ws key with
  | Some _ -> true
  | None ->
      (match ws.index with
       | Some idx ->
           (match Workspace_index.find_compool idx ~name:key with
            | Some _ -> true
            | None ->
                if Workspace_index.is_complete idx then
                  (match find_compool_path_fallback ws ~key with
                   | Some _ -> true
                   | None -> false)
                else
                  false)
       | None ->
           (match find_compool_path_fallback ws ~key with
            | Some _ -> true
            | None -> false))

type compool_import_dir = {
  compool : string;
  selected : (string * Ast.Loc.t) list;  (* imported element name + location *)
}

let diag_missing_imported_type ~(loc:Ast.Loc.t) ~(item:string) ~(typ:string) : T.Diagnostic.t =
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Error
    ~source:"import"
    ~message:(Printf.sprintf "Imported item %S requires explicit import of type %S." item typ)
    loc

let diag_missing_import_hint ~(loc:Ast.Loc.t) ~(kind:string) ~(symbol:string) ~(compools:string list) : T.Diagnostic.t =
  let targets =
    match compools with
    | [] -> ""
    | [c] -> c
    | xs -> String.concat ", " xs
  in
  let msg =
    if targets = "" then
      Printf.sprintf "%s %S may require a COMPOOL import." kind symbol
    else
      Printf.sprintf
        "%s %S is available in COMPOOL %s. Import it with !COMPOOL '%s' (or selective import)."
        kind
        symbol
        targets
        (List.hd compools)
  in
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Warning
    ~source:"import"
    ~message:msg
    loc

let is_builtin_type (k:string) : bool =
  match k with
  | "A" | "B" | "U" | "S" | "F" | "C" | "P" | "W" | "V" | "STATUS" -> true
  | _ -> false

let is_builtin_function_name (name:string) : bool =
  match normalize_name name with
  | "LOC" | "NEXT" | "BIT" | "BYTE"
  | "SHIFTL" | "SHIFTR" | "ABS" | "SGN"
  | "BITSIZE" | "BYTESIZE" | "WORDSIZE"
  | "LBOUND" | "UBOUND" | "NWDSEN"
  | "FIRST" | "LAST" | "REP" | "V" ->
      true
  | _ ->
      false

let is_reserved_keyword (name:string) : bool =
  match normalize_name name with
  | "START" | "TERM" | "BEGIN" | "END"
  | "DEF" | "REF" | "STATIC" | "CONSTANT" | "PROC" | "ITEM" | "TABLE" | "TYPE"
  | "IF" | "THEN" | "ELSE" | "WHILE" | "FOR" | "BY"
  | "CASE" | "DEFAULT" | "FALLTHRU"
  | "EXIT" | "GOTO" | "RETURN" | "ABORT" | "STOP"
  | "TRUE" | "FALSE"
  | "NOT" | "AND" | "OR" | "XOR" | "EQV" | "MOD"
  | "PROGRAM" | "COMPOOL" | "ICOMPOOL" | "DEFINE" | "BLOCK"
  | "ICOPY" | "ISKIP" | "IBEGIN" | "IEND" | "ILINKAGE" | "ITRACE"
  | "IINTERFERENCE" | "IREDUCIBLE" | "ILIST" | "INOLIST" | "IEJECT"
  | "IBASE" | "IISBASE" | "IDROP" | "ILEFTRIGHT" | "IREARRANGE"
  | "IINITIALIZE" | "IORDER"
  | "REC" | "RENT"
  | "LISTEXP" | "LISTINV" | "LISTBOTH"
  | "INLINE" | "INSTANCE" | "LABEL" | "LIKE"
  | "OVERLAY" | "PARALLEL" | "POS" | "NULL" ->
      true
  | x when is_builtin_function_name x ->
      true
  | _ ->
      false

let extract_compool_import_dirs (doc:Document.t) : compool_import_dir list =
  match doc.Document.ast with
  | None -> []
  | Some prog ->
      let from_decl (d:Ast.decl Ast.node) : compool_import_dir option =
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
            else
              None
        | _ -> None
      in
      prog
      |> List.filter_map (function
           | Ast.TopDecl d -> from_decl d
           | Ast.TopStmt _ -> None)

type dep_info = {
  types : (string, bool) Hashtbl.t;            (* explicit type declarations *)
  item_deps : (string, string list) Hashtbl.t; (* item/table name -> required type keys *)
}

let dep_info_create () : dep_info =
  { types = Hashtbl.create 64; item_deps = Hashtbl.create 128 }

let rec type_keys_of_type_expr (t:Ast.type_expr Ast.node) (acc:(string, bool) Hashtbl.t) : unit =
  match t.v with
  | Ast.TName id ->
      let k = normalize_name id.v in
      if k <> "" && not (is_builtin_type k) then Hashtbl.replace acc k true
  | Ast.TPointer inner ->
      type_keys_of_type_expr inner acc
  | Ast.TArray { elem; _ } ->
      type_keys_of_type_expr elem acc
  | Ast.TRecord fields ->
      List.iter (fun f -> type_keys_of_type_expr f.v.ftype acc) fields
  | Ast.TFunc { params; returns } ->
      List.iter (fun p -> type_keys_of_type_expr p.v.Ast.ptype acc) params;
      (match returns with None -> () | Some r -> type_keys_of_type_expr r acc)

let keys_of_type_expr (t:Ast.type_expr Ast.node) : string list =
  let h = Hashtbl.create 8 in
  type_keys_of_type_expr t h;
  Hashtbl.fold (fun k _ xs -> k :: xs) h []

let rec dep_info_add_stmt (info:dep_info) (s:Ast.stmt Ast.node) : unit =
  match s.v with
  | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _ | Ast.SGoto _ -> ()
  | Ast.SDecl d -> dep_info_add_decl info d
  | Ast.SBlock xs -> List.iter (dep_info_add_stmt info) xs
  | Ast.SIf { then_; else_; _ } ->
      dep_info_add_stmt info then_;
      (match else_ with None -> () | Some e -> dep_info_add_stmt info e)
  | Ast.SWhile { body; _ } -> dep_info_add_stmt info body
  | Ast.SFor { init; step; body; _ } ->
      (match init with None -> () | Some i -> dep_info_add_stmt info i);
      (match step with None -> () | Some st -> dep_info_add_stmt info st);
      dep_info_add_stmt info body
  | Ast.SLabel { body; _ } -> dep_info_add_stmt info body

and dep_info_add_decl (info:dep_info) (d:Ast.decl Ast.node) : unit =
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

let dep_info_of_doc (doc:Document.t) : dep_info =
  let info = dep_info_create () in
  (match doc.Document.ast with
   | None -> ()
   | Some prog ->
       List.iter (function
         | Ast.TopDecl d -> dep_info_add_decl info d
         | Ast.TopStmt s -> dep_info_add_stmt info s
       ) prog);
  info

let read_file_text (path:string) : string option =
  try
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let txt = really_input_string ic len in
    close_in_noerr ic;
    Some txt
  with _ -> None

let resolve_compool_doc_uncached (ws:t) ~(name:string) : Document.t option =
  pump_index ws ~max_dirs:index_lookup_dirs ~max_files:index_lookup_files;
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
                 if Workspace_index.is_complete idx then
                   find_compool_path_fallback ws ~key
                 else
                   None)
        | None ->
            find_compool_path_fallback ws ~key
      in
      (match path_opt with
       | None -> None
       | Some path ->
           match read_file_text path with
           | None -> None
           | Some txt ->
               let uri =
                 match Uri_path.docuri_of_path path with
                 | Some u -> u
                 | None ->
                     (match Uri_path.docuri_of_string (Uri_path.file_uri_of_path path) with
                      | Some u -> u
                      | None -> T.DocumentUri.t_of_yojson (`String "file:///"))
               in
               Some (Document.make ~uri ~file:(Some path) ~text:txt))

let validate_imports (ws:t) (doc:Document.t) : T.Diagnostic.t list =
  let missing_compools =
    doc
    |> Document.imports
    |> List.filter_map (fun (imp:Preprocess.import) ->
         match imp.kind with
         | Preprocess.Compool ->
             if has_compool_target ws imp.name then None
             else Some (diag_missing_compool imp.loc imp.name))
  in
  let imports = extract_compool_import_dirs doc in
  let missing_type_imports =
    if imports = [] then []
    else
      let doc_cache : (string, Document.t option) Hashtbl.t = Hashtbl.create 16 in
      let info_cache : (string, dep_info option) Hashtbl.t = Hashtbl.create 16 in

      let get_doc_for_compool (name:string) : Document.t option =
        let key = normalize_name name in
        match Hashtbl.find_opt doc_cache key with
        | Some x -> x
        | None ->
            let x = resolve_compool_doc_uncached ws ~name:key in
            Hashtbl.replace doc_cache key x;
            x
      in

      let get_info_for_compool (name:string) : dep_info option =
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
      List.iter (fun imp ->
        match get_info_for_compool imp.compool with
        | None -> ()
        | Some info ->
            if imp.selected = [] then
              Hashtbl.iter (fun tk _ -> add_available_type tk) info.types
            else
              List.iter (fun (nm, _loc) ->
                if Hashtbl.mem info.types nm then add_available_type nm
              ) imp.selected
      ) imports;

      (* Pass 2: for selectively imported items, require explicit import of their types. *)
      let seen : (string, bool) Hashtbl.t = Hashtbl.create 64 in
      let hint_seen : (string, bool) Hashtbl.t = Hashtbl.create 64 in
      let out = ref [] in
      let add_diag_once (loc:Ast.Loc.t) ~(item:string) ~(typ:string) =
        let k =
          Printf.sprintf "%s|%d|%d|%s|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line
            loc.start_pos.col
            item
            typ
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := diag_missing_imported_type ~loc ~item ~typ :: !out
        )
      in
      let add_type_hint_once (loc:Ast.Loc.t) ~(typ:string) =
        let k =
          Printf.sprintf "%s|%d|%d|type-hint|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line
            loc.start_pos.col
            typ
        in
        if not (Hashtbl.mem hint_seen k) then (
          Hashtbl.replace hint_seen k true;
          out :=
            diag_missing_import_hint
              ~loc
              ~kind:"Type"
              ~symbol:typ
              ~compools:[]
            :: !out
        )
      in
      List.iter (fun imp ->
        if imp.selected <> [] then
          match get_info_for_compool imp.compool with
          | None -> ()
          | Some info ->
              List.iter (fun (sel_name, sel_loc) ->
                match Hashtbl.find_opt info.item_deps sel_name with
                | None -> ()
                | Some deps ->
                    List.iter (fun dep ->
                      let dep = normalize_name dep in
                      if dep <> "" && not (Hashtbl.mem available_types dep) then (
                        add_diag_once sel_loc ~item:sel_name ~typ:dep;
                        add_type_hint_once sel_loc ~typ:dep
                      )
                    ) deps
              ) imp.selected
      ) imports;
      List.rev !out
  in
  missing_compools @ missing_type_imports

type sem_ty =
  | TyUnknown
  | TyInt
  | TyFloat
  | TyBit
  | TyChar
  | TyString
  | TyStatus
  | TyPointer of sem_ty option
  | TyArray of sem_ty
  | TyRecord of (string * sem_ty) list

type sem_proc_sig = {
  param_tys : sem_ty list option;
  ret_ty : sem_ty option;
  use_attr : Ast.proc_use;
}

type sem_value =
  | SVVar of sem_ty
  | SVConst of sem_ty
  | SVProc of sem_proc_sig

type sem_exports = {
  values : (string, sem_value) Hashtbl.t;
  types : (string, Ast.type_expr Ast.node) Hashtbl.t;
}

type sem_scope = sem_exports

type sem_proc_ctx = {
  proc_key : string;
  proc_name : string;
  proc_ret_ty : sem_ty option;
}

let diag_semantic (loc:Ast.Loc.t) (msg:string) : T.Diagnostic.t =
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Error
    ~source:"semantic"
    ~message:msg
    loc

let copy_tbl (tbl:('k, 'v) Hashtbl.t) : ('k, 'v) Hashtbl.t =
  let out = Hashtbl.create (max 16 (Hashtbl.length tbl * 2)) in
  Hashtbl.iter (fun k v -> Hashtbl.replace out k v) tbl;
  out

let sem_scope_copy (s:sem_scope) : sem_scope =
  { values = copy_tbl s.values; types = copy_tbl s.types }

let sem_scope_empty () : sem_scope =
  { values = Hashtbl.create 64; types = Hashtbl.create 64 }

let sem_add_value ?(overwrite=true) (s:sem_scope) (name:string) (v:sem_value) : unit =
  let k = normalize_name name in
  if k <> "" && (overwrite || not (Hashtbl.mem s.values k)) then
    Hashtbl.replace s.values k v

let sem_add_type ?(overwrite=true) (s:sem_scope) (name:string) (t:Ast.type_expr Ast.node) : unit =
  let k = normalize_name name in
  if k <> "" && (overwrite || not (Hashtbl.mem s.types k)) then
    Hashtbl.replace s.types k t

let sem_find_record_field (fields:(string * sem_ty) list) (name:string) : sem_ty option =
  let key = normalize_name name in
  fields
  |> List.find_opt (fun (nm, _) -> normalize_name nm = key)
  |> Option.map snd

let sem_is_builtin_type (k:string) : bool =
  match k with
  | "A" | "B" | "U" | "S" | "F" | "C" | "P" | "W" | "V" | "STATUS" -> true
  | _ -> false

let is_single_letter_loop_control (name:string) : bool =
  String.length name = 1
  && match name.[0] with
     | 'A' .. 'Z' | 'a' .. 'z' -> true
     | _ -> false

let rec sem_ty_of_type_expr ?(seen:string list=[]) (types:(string, Ast.type_expr Ast.node) Hashtbl.t) (t:Ast.type_expr Ast.node) : sem_ty =
  match t.v with
  | Ast.TName id ->
      let k = normalize_name id.v in
      (match k with
       | "B" -> TyBit
       | "U" | "S" | "W" -> TyInt
       | "F" | "A" -> TyFloat
       | "C" -> TyChar
       | "P" -> TyPointer None
       | "STATUS" | "V" -> TyStatus
       | "" -> TyUnknown
       | _ ->
           if List.mem k seen || sem_is_builtin_type k then TyUnknown
           else
             match Hashtbl.find_opt types k with
             | None -> TyUnknown
             | Some defn -> sem_ty_of_type_expr ~seen:(k :: seen) types defn)
  | Ast.TArray { elem; _ } ->
      TyArray (sem_ty_of_type_expr ~seen types elem)
  | Ast.TPointer inner ->
      TyPointer (Some (sem_ty_of_type_expr ~seen types inner))
  | Ast.TRecord fields ->
      TyRecord
        (fields
         |> List.map (fun f ->
              let fv = f.v in
              (fv.fname.v, sem_ty_of_type_expr ~seen types fv.ftype)))
  | Ast.TFunc _ ->
      TyUnknown

let sem_proc_sig_of_proc (types:(string, Ast.type_expr Ast.node) Hashtbl.t) (p:Ast.proc Ast.node) : sem_proc_sig =
  let local_var_tys : (string, sem_ty) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun d ->
    match d.v with
    | Ast.DVar { name; dtype; _ } ->
        Hashtbl.replace local_var_tys (normalize_name name.v) (sem_ty_of_type_expr types dtype)
    | _ -> ()
  ) p.v.locals;
  let param_tys =
    if p.v.params = [] then Some []
    else
      Some
        (p.v.params
         |> List.map (fun prm ->
              let pn = normalize_name prm.v.pname.v in
              let direct = sem_ty_of_type_expr types prm.v.ptype in
              match direct with
              | TyUnknown ->
                  (match Hashtbl.find_opt local_var_tys pn with
                   | Some ty -> ty
                   | None -> TyUnknown)
               | ty -> ty))
  in
  let ret_ty =
    match p.v.returns with
    | None -> None
    | Some r -> Some (sem_ty_of_type_expr types r)
  in
  { param_tys; ret_ty; use_attr = p.v.use_attr }

let block_proc_names_of_program (prog:Ast.program) : (string, bool) Hashtbl.t =
  let out = Hashtbl.create 32 in
  List.iter (function
    | Ast.TopDecl d ->
        (match d.v with
         | Ast.DDirective { name; args = nm :: _ } when normalize_name name.v = "BLOCK" ->
             let k = normalize_name nm.v in
             if k <> "" then Hashtbl.replace out k true
         | _ -> ())
    | Ast.TopStmt _ -> ()
  ) prog;
  out

let sem_exports_of_program (prog:Ast.program) : sem_exports =
  let out = sem_scope_empty () in
  let block_names = block_proc_names_of_program prog in
  let is_block_proc (p:Ast.proc Ast.node) : bool =
    Hashtbl.mem block_names (normalize_name p.v.name.v)
  in
  let rec collect_types_decl ~(in_block:bool) (d:Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DType { name; defn } ->
        sem_add_type out name.v defn
    | Ast.DProc p ->
        if in_block || is_block_proc p then
          List.iter (collect_types_decl ~in_block:true) p.v.locals
    | Ast.DVar _ | Ast.DConst _ | Ast.DDirective _ -> ()
  in
  let rec collect_values_decl ~(in_block:bool) (d:Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DVar { name; dtype; _ } ->
        sem_add_value out name.v (SVVar (sem_ty_of_type_expr out.types dtype))
    | Ast.DConst { name; dtype; value = _ } ->
        let ty =
          match dtype with
          | Some t -> sem_ty_of_type_expr out.types t
          | None -> TyUnknown
        in
        sem_add_value out name.v (SVConst ty)
    | Ast.DType _ ->
        ()
    | Ast.DProc p ->
        if not in_block then
          sem_add_value out p.v.name.v (SVProc (sem_proc_sig_of_proc out.types p));
        if in_block || is_block_proc p then
          List.iter (collect_values_decl ~in_block:true) p.v.locals
    | Ast.DDirective _ ->
        ()
  in
  List.iter (function
    | Ast.TopDecl d -> collect_types_decl ~in_block:false d
    | Ast.TopStmt _ -> ()
  ) prog;
  List.iter (function
    | Ast.TopDecl d -> collect_values_decl ~in_block:false d
    | Ast.TopStmt _ -> ()
  ) prog;
  out

let sem_exports_of_doc (doc:Document.t) : sem_exports =
  match doc.Document.ast with
  | None -> sem_scope_empty ()
  | Some prog -> sem_exports_of_program prog

let add_compool_hint
    (tbl:(string, string list) Hashtbl.t)
    ~(symbol_key:string)
    ~(compool_key:string)
    : unit =
  let key = normalize_name symbol_key in
  let compool = normalize_name compool_key in
  if key <> "" && compool <> "" then
    let prev =
      match Hashtbl.find_opt tbl key with
      | None -> []
      | Some xs -> xs
    in
    if not (List.mem compool prev) then
      Hashtbl.replace tbl key (compool :: prev)

let symbol_hint_max_file_count = 1500
let symbol_hint_max_chars = 20_000_000

let build_symbol_hint_index (ws:t) : (string, string list) Hashtbl.t * (string, string list) Hashtbl.t =
  let values = Hashtbl.create 1024 in
  let types = Hashtbl.create 1024 in
  let seen_paths = Hashtbl.create 512 in
  let parsed_files = ref 0 in
  let parsed_chars = ref 0 in

  let hint_compool_key_of_doc (doc:Document.t) : string option =
    match doc.Document.compool_def with
    | Some compool ->
        let k = normalize_name compool in
        if k = "" then None else Some k
    | None ->
        (match doc.Document.file with
         | None -> None
         | Some path ->
             (match source_stem_of_filename (Filename.basename path) with
              | None -> None
              | Some stem ->
                  let k = normalize_name stem in
                  if k = "" then None else Some k))
  in

  let add_doc_hints (doc:Document.t) : unit =
    match hint_compool_key_of_doc doc with
    | None -> ()
    | Some compool ->
        let exp = sem_exports_of_doc doc in
        Hashtbl.iter (fun sym v ->
          match v with
          | SVProc _ -> ()
          | _ -> add_compool_hint values ~symbol_key:sym ~compool_key:compool
        ) exp.values;
        Hashtbl.iter (fun sym _ -> add_compool_hint types ~symbol_key:sym ~compool_key:compool) exp.types
  in

  let add_path_hints (p:string) : unit =
    let pk = normalize_path_key p in
    if not (Hashtbl.mem seen_paths pk)
       && !parsed_files < symbol_hint_max_file_count
       && !parsed_chars < symbol_hint_max_chars
    then (
      Hashtbl.replace seen_paths pk true;
      match read_file_text p with
      | None -> ()
      | Some txt ->
          let txt_len = String.length txt in
          if !parsed_chars + txt_len <= symbol_hint_max_chars then (
            incr parsed_files;
            parsed_chars := !parsed_chars + txt_len;
            let uri =
              match Uri_path.docuri_of_path p with
              | Some u -> u
              | None ->
                  (match Uri_path.docuri_of_string (Uri_path.file_uri_of_path p) with
                   | Some u -> u
                   | None -> T.DocumentUri.t_of_yojson (`String "file:///"))
            in
            let d = Document.make ~uri ~file:(Some p) ~text:txt in
            add_doc_hints d
          )
    )
  in

  Hashtbl.iter (fun _ doc ->
    (match doc.Document.file with
     | None -> ()
     | Some p -> Hashtbl.replace seen_paths (normalize_path_key p) true);
    add_doc_hints doc
  ) ws.docs;

  (match ws.index with
   | None -> ()
   | Some idx ->
       Workspace_index.all_paths idx |> List.iter add_path_hints);

  (values, types)

let symbol_hint_index (ws:t) : (string, string list) Hashtbl.t * (string, string list) Hashtbl.t =
  pump_index ws ~max_dirs:index_lookup_dirs ~max_files:index_lookup_files;
  match ws.symbol_hints with
  | Some idx -> idx
  | None ->
      let idx = build_symbol_hint_index ws in
      ws.symbol_hints <- Some idx;
      idx

let sem_import_dirs (doc:Document.t) : compool_import_dir list =
  let ast_dirs = extract_compool_import_dirs doc in
  let ast_compools : (string, bool) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun d -> Hashtbl.replace ast_compools (normalize_name d.compool) true) ast_dirs;
  let pre_dirs =
    Document.imports doc
    |> List.filter_map (fun (imp:Preprocess.import) ->
         match imp.kind with
         | Preprocess.Compool ->
             let k = normalize_name imp.name in
             if k = "" || Hashtbl.mem ast_compools k then None
             else Some { compool = k; selected = [] })
  in
  ast_dirs @ pre_dirs

let sem_ty_of_literal (lit:Ast.literal) : sem_ty =
  match lit with
  | Ast.LInt s ->
      let u = normalize_name s in
      if String.length u >= 3 && String.contains u '\'' && String.contains u 'B' then TyBit else TyInt
  | Ast.LFloat _ -> TyFloat
  | Ast.LString s -> if String.length s = 1 then TyChar else TyString
  | Ast.LChar _ -> TyChar
  | Ast.LBool _ -> TyBit

let sem_ty_to_string (t:sem_ty) : string =
  let rec is_bit_like = function
    | TyBit -> true
    | TyArray inner -> is_bit_like inner
    | _ -> false
  in
  if is_bit_like t then "bit"
  else
  match t with
  | TyUnknown -> "unknown"
  | TyInt -> "integer"
  | TyFloat -> "float"
  | TyBit -> "bit"
  | TyChar -> "character"
  | TyString -> "string"
  | TyStatus -> "status"
  | TyPointer _ -> "pointer"
  | TyArray _ -> "table/array"
  | TyRecord _ -> "record"

let rec sem_is_primitive = function
  | TyUnknown | TyInt | TyFloat | TyBit | TyChar | TyString | TyStatus | TyPointer _ -> true
  | TyArray inner -> sem_is_primitive inner
  | TyRecord _ -> false

let rec sem_scalarize = function
  | TyArray inner when sem_is_primitive inner -> sem_scalarize inner
  | t -> t

let rec sem_compatible (lhs:sem_ty) (rhs:sem_ty) : bool =
  let lhs = sem_scalarize lhs in
  let rhs = sem_scalarize rhs in
  match lhs, rhs with
  | TyUnknown, _ | _, TyUnknown -> true
  | TyInt, TyInt
  | TyFloat, TyFloat
  | TyBit, TyBit
  | TyChar, TyChar
  | TyString, TyString
  | TyStatus, TyStatus
  | TyPointer _, TyPointer _ -> true
  | TyInt, TyFloat
  | TyFloat, TyInt -> true
  | TyRecord _, TyRecord _ -> true
  | TyArray a, TyArray b -> sem_compatible a b
  | _ -> false

let validate_semantics (ws:t) (doc:Document.t) : T.Diagnostic.t list =
  match doc.Document.ast with
  | None -> []
  | Some prog ->
      let seen = Hashtbl.create 128 in
      let out = ref [] in
      let emit (loc:Ast.Loc.t) (msg:string) =
        let k =
          Printf.sprintf "%s|%d|%d|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line
            loc.start_pos.col
            msg
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := diag_semantic loc msg :: !out
        )
      in
      let emit_import_hint (loc:Ast.Loc.t) ~(kind:string) ~(symbol:string) ~(compools:string list) =
        let k =
          Printf.sprintf "%s|%d|%d|import|%s|%s|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line
            loc.start_pos.col
            kind
            symbol
            (String.concat "," compools)
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := diag_missing_import_hint ~loc ~kind ~symbol ~compools :: !out
        )
      in

      let import_dirs = sem_import_dirs doc in
      let doc_cache : (string, Document.t option) Hashtbl.t = Hashtbl.create 16 in
      let exports_cache : (string, sem_exports option) Hashtbl.t = Hashtbl.create 16 in
      let imported_compools : (string, [ `All | `Selected ]) Hashtbl.t = Hashtbl.create 32 in
      let mark_import_mode (compool:string) (mode:[ `All | `Selected ]) =
        let k = normalize_name compool in
        if k <> "" then
          match Hashtbl.find_opt imported_compools k, mode with
          | Some `All, _ -> ()
          | _, `All -> Hashtbl.replace imported_compools k `All
          | Some `Selected, `Selected -> ()
          | None, `Selected -> Hashtbl.replace imported_compools k `Selected
      in
      List.iter (fun imp ->
        let mode = if imp.selected = [] then `All else `Selected in
        mark_import_mode imp.compool mode
      ) import_dirs;
      (match doc.Document.compool_def with
       | None -> ()
       | Some c -> mark_import_mode c `All);

      let hint_tables
        : ((string, string list) Hashtbl.t * (string, string list) Hashtbl.t) option ref
        = ref None
      in
      let get_hint_tables ()
        : (string, string list) Hashtbl.t * (string, string list) Hashtbl.t =
        match !hint_tables with
        | Some t -> t
        | None ->
            let t = symbol_hint_index ws in
            hint_tables := Some t;
            t
      in
      let hint_compools_for ~(is_type:bool) (name:string) : string list =
        let key = normalize_name name in
        if key = "" then []
        else
          let hint_values, hint_types = get_hint_tables () in
          let tbl = if is_type then hint_types else hint_values in
          match Hashtbl.find_opt tbl key with
          | None -> []
          | Some xs ->
              xs
              |> List.filter (fun c ->
                   match Hashtbl.find_opt imported_compools (normalize_name c) with
                   | Some `All -> false
                   | Some `Selected | None -> true)
              |> List.sort_uniq String.compare
              |> (fun ys ->
                    let rec take n acc = function
                      | [] -> List.rev acc
                      | _ when n <= 0 -> List.rev acc
                      | x :: tl -> take (n - 1) (x :: acc) tl
                    in
                    take 3 [] ys)
      in
      let suggest_missing_import ~(loc:Ast.Loc.t) ~(kind:string) ~(is_type:bool) ~(symbol:string) : unit =
        let compools = hint_compools_for ~is_type symbol in
        if compools <> [] then
          emit_import_hint loc ~kind ~symbol ~compools
      in

      let has_import_hint ~(is_type:bool) (symbol:string) : bool =
        hint_compools_for ~is_type symbol <> []
      in

      let get_doc_for_compool (name:string) : Document.t option =
        let key = normalize_name name in
        match Hashtbl.find_opt doc_cache key with
        | Some x -> x
        | None ->
            let x = resolve_compool_doc_uncached ws ~name:key in
            Hashtbl.replace doc_cache key x;
            x
      in
      let get_exports_for_compool (name:string) : sem_exports option =
        let key = normalize_name name in
        match Hashtbl.find_opt exports_cache key with
        | Some x -> x
        | None ->
            let x =
              match get_doc_for_compool key with
              | None -> None
              | Some d -> Some (sem_exports_of_doc d)
            in
            Hashtbl.replace exports_cache key x;
            x
      in

      let scope = sem_scope_copy (sem_exports_of_program prog) in
      List.iter (fun imp ->
        match get_exports_for_compool imp.compool with
        | None -> ()
        | Some exp ->
            if imp.selected = [] then (
              Hashtbl.iter (fun k v -> sem_add_value ~overwrite:false scope k v) exp.values;
              Hashtbl.iter (fun k v -> sem_add_type ~overwrite:false scope k v) exp.types
            ) else (
              List.iter (fun (nm, _loc) ->
                match Hashtbl.find_opt exp.values nm with
                | Some v -> sem_add_value ~overwrite:false scope nm v
                | None -> ()
              ) imp.selected;
              List.iter (fun (nm, _loc) ->
                match Hashtbl.find_opt exp.types nm with
                | Some t -> sem_add_type ~overwrite:false scope nm t
                | None -> ()
              ) imp.selected
            )
      ) import_dirs;

      let sem_lookup_value (scp:sem_scope) (name:string) : sem_value option =
        Hashtbl.find_opt scp.values (normalize_name name)
      in

      let sem_is_builtin_call (name:string) : bool =
        let k = normalize_name name in
        k = "__CONV__" || k = "__PRESET__" || k = "__POW__" || k = "__RANGE__"
        || is_builtin_function_name k
      in

      let rec check_type_import_hints (scp:sem_scope) (t:Ast.type_expr Ast.node) : unit =
        match t.v with
        | Ast.TName id ->
            let k = normalize_name id.v in
            if k <> "" && not (sem_is_builtin_type k) && not (Hashtbl.mem scp.types k) then
              suggest_missing_import ~loc:id.loc ~kind:"Type" ~is_type:true ~symbol:id.v
        | Ast.TPointer inner ->
            check_type_import_hints scp inner
        | Ast.TArray { elem; _ } ->
            check_type_import_hints scp elem
        | Ast.TRecord fields ->
            List.iter (fun f -> check_type_import_hints scp f.v.ftype) fields
        | Ast.TFunc { params; returns } ->
            List.iter (fun p -> check_type_import_hints scp p.v.ptype) params;
            (match returns with None -> () | Some r -> check_type_import_hints scp r)
      in

      let rec sem_subscript_array (ty:sem_ty) (count:int) : sem_ty =
        if count <= 0 then ty
        else
          match ty with
          | TyArray elem -> sem_subscript_array elem (count - 1)
          | _ -> TyUnknown
      in

      let sem_subscript_pointer (ty:sem_ty) (count:int) : sem_ty =
        match ty with
        | TyPointer None ->
            TyPointer None
        | TyPointer (Some target) ->
            let inner = sem_subscript_array target count in
            if inner = TyUnknown then TyUnknown else TyPointer (Some inner)
        | _ ->
            TyUnknown
      in

      let sem_subscript_value (ty:sem_ty) (count:int) : sem_ty =
        match ty with
        | TyArray _ -> sem_subscript_array ty count
        | TyPointer _ -> sem_subscript_pointer ty count
        | _ -> TyUnknown
      in

      let sem_deref_target ~(ptr_loc:Ast.Loc.t) (ty:sem_ty) : sem_ty =
        match ty with
        | TyPointer (Some inner) ->
            inner
        | TyPointer None ->
            emit ptr_loc "Dereference requires a typed pointer.";
            TyUnknown
        | other ->
            (* Keep existing table-qualified behavior for compatibility. *)
            other
      in

      let sem_field_ty_in (field_name:string) (ty:sem_ty) : sem_ty option =
        match ty with
        | TyRecord fields -> sem_find_record_field fields field_name
        | TyArray (TyRecord fields) -> sem_find_record_field fields field_name
        | TyArray inner -> (
            match inner with
            | TyRecord fields -> sem_find_record_field fields field_name
            | _ -> None)
        | _ -> None
      in

      let rec ty_of_expr (scp:sem_scope) (current_proc:sem_proc_ctx option) ?(status_atom=false) (e:Ast.expr Ast.node) : sem_ty =
        match e.v with
        | Ast.ELit lit ->
            sem_ty_of_literal lit
        | Ast.EName id ->
            if status_atom then TyStatus
            else
              (match sem_lookup_value scp id.v with
               | Some (SVVar ty) | Some (SVConst ty) -> ty
               | Some (SVProc _) ->
                   (match current_proc with
                    | Some cp when normalize_name id.v = cp.proc_key ->
                        (match cp.proc_ret_ty with
                         | Some ty -> ty
                         | None ->
                             emit id.loc (Printf.sprintf "%S is a procedure and cannot be used as a value." id.v);
                             TyUnknown)
                    | _ ->
                        emit id.loc (Printf.sprintf "%S is a procedure and cannot be used as a value." id.v);
                        TyUnknown)
               | None ->
                    if has_import_hint ~is_type:false id.v then
                      suggest_missing_import ~loc:id.loc ~kind:"Identifier" ~is_type:false ~symbol:id.v
                    else
                      emit id.loc (Printf.sprintf "Undefined identifier %S." id.v);
                    TyUnknown)
        | Ast.EUnop { rhs; _ } ->
            ty_of_expr scp current_proc rhs
        | Ast.EBinop { lhs; rhs; _ } ->
            let l = ty_of_expr scp current_proc lhs in
            let r = ty_of_expr scp current_proc rhs in
            if l = TyFloat || r = TyFloat then TyFloat else l
        | Ast.ECall { callee; args } ->
            let ck = normalize_name callee.v in
            if ck = "V" then (
              List.iter (fun a -> ignore (ty_of_expr scp current_proc ~status_atom:true a)) args;
              TyStatus
            ) else if sem_is_builtin_call callee.v then (
              match ck, args with
              | "LOC", a0 :: rest ->
                  let target_ty = ty_of_expr scp current_proc a0 in
                  List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) rest;
                  TyPointer (Some target_ty)
              | "LOC", [] ->
                  TyPointer None
              | "NEXT", p0 :: rest ->
                  let pty = ty_of_expr scp current_proc p0 in
                  List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) rest;
                  (match pty with
                   | TyPointer _ -> pty
                   | _ -> TyUnknown)
              | _ ->
                  List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) args;
                  TyUnknown
            ) else
              (match sem_lookup_value scp callee.v with
               | Some (SVProc sig_) ->
                   (match current_proc with
                    | Some cp when normalize_name callee.v = cp.proc_key ->
                        (match sig_.use_attr with
                         | Ast.UseRec -> ()
                         | Ast.UseRent ->
                             emit callee.loc
                               (Printf.sprintf
                                  "Recursive call to %S requires REC (RENT alone is not enough)."
                                  cp.proc_name)
                         | Ast.UseNormal ->
                             emit callee.loc
                               (Printf.sprintf "Recursive call to %S requires REC." cp.proc_name))
                    | _ -> ());
                   (match sig_.param_tys with
                    | None -> ()
                    | Some pts ->
                        let rec check_pairs ps xs =
                          match ps, xs with
                          | pty :: pst, arg :: xst ->
                              let aty = ty_of_expr scp current_proc arg in
                              if not (sem_compatible pty aty) then
                                emit arg.loc
                                  (Printf.sprintf
                                     "Argument type mismatch in call to %S: expected %s, got %s."
                                     callee.v
                                     (sem_ty_to_string pty)
                                     (sem_ty_to_string aty));
                              check_pairs pst xst
                          | _, [] -> ()
                          | [], _ :: xst ->
                              (* Extra args: parse them, but avoid noisy count errors for now. *)
                              List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) xst
                        in
                        check_pairs pts args);
                   (match sig_.ret_ty with Some rt -> rt | None -> TyUnknown)
               | Some (SVVar ty) | Some (SVConst ty) ->
                   List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) args;
                   if args = [] then (
                     emit callee.loc (Printf.sprintf "%S is not callable." callee.v);
                     TyUnknown
                   ) else
                     let out_ty = sem_subscript_value ty (List.length args) in
                     if out_ty = TyUnknown then (
                       emit callee.loc (Printf.sprintf "Cannot subscript %S." callee.v);
                       TyUnknown
                     ) else out_ty
               | None ->
                   emit callee.loc
                     (Printf.sprintf
                        "Undefined procedure %S. Declare it with REF PROC %S in scope."
                        callee.v
                        callee.v);
                   List.iter (fun a -> ignore (ty_of_expr scp current_proc a)) args;
                   TyUnknown)
        | Ast.EIndex { base; index } ->
            let bt = ty_of_expr scp current_proc base in
            List.iter (fun i -> ignore (ty_of_expr scp current_proc i)) index;
            sem_subscript_value bt (List.length index)
        | Ast.EField { base; field } ->
            let bt = ty_of_expr scp current_proc base in
            (match bt with
             | TyRecord fields ->
                 (match sem_find_record_field fields field.v with
                  | Some t -> t
                  | None ->
                      emit field.loc (Printf.sprintf "Unknown field %S." field.v);
                      TyUnknown)
             | TyArray (TyRecord fields) ->
                 (match sem_find_record_field fields field.v with
                  | Some t -> t
                  | None ->
                      emit field.loc (Printf.sprintf "Unknown field %S." field.v);
                      TyUnknown)
             | _ -> TyUnknown)
        | Ast.EAt { field; ptr } ->
            let pt = ty_of_expr scp current_proc ptr in
            let target_ty = sem_deref_target ~ptr_loc:ptr.loc pt in
            let field_ref =
              match field.v with
              | Ast.EName id ->
                  Some (id, [])
              | Ast.EIndex { base; index } -> (
                  match base.v with
                  | Ast.EName id -> Some (id, index)
                  | _ -> None)
              | _ ->
                  ignore (ty_of_expr scp current_proc field);
                  None
            in
            (match field_ref with
             | None -> TyUnknown
             | Some (id, indexes) ->
                 List.iter (fun i -> ignore (ty_of_expr scp current_proc i)) indexes;
                 let qualified_target = sem_subscript_array target_ty (List.length indexes) in
                 (match sem_field_ty_in id.v qualified_target with
                  | Some ty -> ty
                  | None ->
                       emit id.loc (Printf.sprintf "Unknown field %S for @ access." id.v);
                       TyUnknown))
        | Ast.EDeref { ptr } ->
            let pt = ty_of_expr scp current_proc ptr in
            sem_deref_target ~ptr_loc:ptr.loc pt
        | Ast.EParen inner ->
            ty_of_expr scp current_proc inner
      in

      let ty_of_lvalue (scp:sem_scope) (current_proc:sem_proc_ctx option) (e:Ast.expr Ast.node) : sem_ty option =
        match e.v with
        | Ast.EName id ->
            (match sem_lookup_value scp id.v with
             | Some (SVVar ty) | Some (SVConst ty) -> Some ty
             | Some (SVProc _) ->
                 (match current_proc with
                  | Some cp when normalize_name id.v = cp.proc_key ->
                      (match cp.proc_ret_ty with
                       | Some ty -> Some ty
                       | None ->
                           emit id.loc (Printf.sprintf "Cannot assign to procedure %S." id.v);
                           None)
                  | _ ->
                      emit id.loc (Printf.sprintf "Cannot assign to procedure %S." id.v);
                      None)
             | None ->
                  if has_import_hint ~is_type:false id.v then
                    suggest_missing_import ~loc:id.loc ~kind:"Item" ~is_type:false ~symbol:id.v
                  else
                    emit id.loc (Printf.sprintf "Undefined item %S." id.v);
                  None)
        | Ast.EField _ | Ast.EAt _ | Ast.EDeref _ | Ast.EIndex _ ->
            Some (ty_of_expr scp current_proc e)
        | _ ->
            ignore (ty_of_expr scp current_proc e);
            None
      in

      let add_decl_symbol (scp:sem_scope) (d:Ast.decl Ast.node) : unit =
        match d.v with
        | Ast.DType { name; defn } ->
            sem_add_type scp name.v defn
        | Ast.DVar { name; dtype; _ } ->
            sem_add_value scp name.v (SVVar (sem_ty_of_type_expr scp.types dtype))
        | Ast.DConst { name; dtype; _ } ->
            let ty =
              match dtype with
              | None -> TyUnknown
              | Some t -> sem_ty_of_type_expr scp.types t
            in
            sem_add_value scp name.v (SVConst ty)
        | Ast.DProc p ->
            sem_add_value scp p.v.name.v (SVProc (sem_proc_sig_of_proc scp.types p))
        | Ast.DDirective _ -> ()
      in

      let rec check_stmt (scp:sem_scope) (current_proc:sem_proc_ctx option) (s:Ast.stmt Ast.node) : unit =
        match s.v with
        | Ast.SEmpty -> ()
        | Ast.SDecl d ->
            add_decl_symbol scp d;
            check_decl scp current_proc d
        | Ast.SBlock xs ->
            List.iter (fun st -> check_stmt scp current_proc st) xs
        | Ast.SAssign { lhs; rhs } ->
            let lhs_ty = ty_of_lvalue scp current_proc lhs in
            let rhs_ty = ty_of_expr scp current_proc rhs in
            (match lhs_ty with
             | None -> ()
             | Some lt ->
                 if not (sem_compatible lt rhs_ty) then
                   emit rhs.loc
                     (Printf.sprintf
                        "Type mismatch in assignment: left is %s, right is %s."
                        (sem_ty_to_string lt)
                         (sem_ty_to_string rhs_ty)))
        | Ast.SCallStmt { callee; args } ->
            ignore (ty_of_expr scp current_proc (Ast.node ~loc:s.loc (Ast.ECall { callee; args })))
        | Ast.SIf { cond; then_; else_ } ->
            ignore (ty_of_expr scp current_proc cond);
            check_stmt scp current_proc then_;
            (match else_ with None -> () | Some e -> check_stmt scp current_proc e)
        | Ast.SWhile { cond; body } ->
            ignore (ty_of_expr scp current_proc cond);
            check_stmt scp current_proc body
        | Ast.SFor { init; cond; step; body } ->
            let for_scope =
              match init with
              | Some ({ v = Ast.SAssign { lhs = { v = Ast.EName lc; _ }; rhs }; _ })
                when is_single_letter_loop_control lc.v ->
                  let scp2 = sem_scope_copy scp in
                  let lty = ty_of_expr scp current_proc rhs in
                  sem_add_value scp2 lc.v (SVVar lty);
                  scp2
              | _ ->
                  scp
            in
            (match init with None -> () | Some i -> check_stmt for_scope current_proc i);
            (match cond with None -> () | Some c -> ignore (ty_of_expr for_scope current_proc c));
            (match step with None -> () | Some st -> check_stmt for_scope current_proc st);
            check_stmt for_scope current_proc body
        | Ast.SReturn eo ->
            (match eo with None -> () | Some e -> ignore (ty_of_expr scp current_proc e))
        | Ast.SLabel { body; _ } ->
            check_stmt scp current_proc body
        | Ast.SGoto _ -> ()

      and check_decl (scp:sem_scope) (current_proc:sem_proc_ctx option) (d:Ast.decl Ast.node) : unit =
        match d.v with
        | Ast.DVar { dtype; init; _ } ->
            check_type_import_hints scp dtype;
            (match init with
             | None -> ()
             | Some rhs ->
                 let lty = sem_ty_of_type_expr scp.types dtype in
                 let rty = ty_of_expr scp current_proc rhs in
                 if not (sem_compatible lty rty) then
                   emit rhs.loc
                     (Printf.sprintf
                        "Type mismatch in initializer: expected %s, got %s."
                        (sem_ty_to_string lty)
                        (sem_ty_to_string rty)))
        | Ast.DConst { dtype; value; _ } ->
            (match dtype with
             | None -> ()
             | Some t -> check_type_import_hints scp t);
            ignore (ty_of_expr scp current_proc value)
        | Ast.DType { defn; _ } ->
            check_type_import_hints scp defn
        | Ast.DDirective _ ->
            ()
        | Ast.DProc p ->
            let proc_scope = sem_scope_copy scp in
            List.iter (add_decl_symbol proc_scope) p.v.locals;
            List.iter (fun prm -> check_type_import_hints proc_scope prm.v.ptype) p.v.params;
            (match p.v.returns with
             | None -> ()
             | Some r -> check_type_import_hints proc_scope r);
            let proc_ret_ty =
              match p.v.returns with
              | None -> None
              | Some r -> Some (sem_ty_of_type_expr proc_scope.types r)
            in
            let proc_ctx =
              Some
                {
                  proc_key = normalize_name p.v.name.v;
                  proc_name = p.v.name.v;
                  proc_ret_ty;
                }
            in
            List.iter (fun prm ->
              let pname = prm.v.pname.v in
              let direct_ty = sem_ty_of_type_expr proc_scope.types prm.v.ptype in
              let inferred_ty =
                match direct_ty with
                | TyUnknown ->
                    (match Hashtbl.find_opt proc_scope.values (normalize_name pname) with
                     | Some (SVVar ty) -> ty
                     | _ -> TyUnknown)
                | ty -> ty
              in
              sem_add_value proc_scope pname (SVVar inferred_ty)
            ) p.v.params;
            List.iter (fun pd -> check_decl proc_scope proc_ctx pd) p.v.locals;
            check_stmt proc_scope proc_ctx p.v.body
      in

      List.iter (function
        | Ast.TopDecl d -> check_decl scope None d
        | Ast.TopStmt s -> check_stmt scope None s
      ) prog;
      List.rev !out

let store_doc (ws:t) (uri:T.DocumentUri.t) (doc:Document.t) : unit =
  let old_doc = Hashtbl.find_opt ws.docs uri in
  let import_diags = validate_imports ws doc in
  let semantic_diags =
    if doc.Document.ast = None || doc.Document.parse_diags <> [] then
      []
    else
      validate_semantics ws doc
  in
  let doc = Document.with_import_diags (import_diags @ semantic_diags) doc in
  let has_compool (d:Document.t) =
    match d.Document.compool_def with
    | None -> false
    | Some name -> normalize_name name <> ""
  in
  if has_compool doc || Option.fold ~none:false ~some:has_compool old_doc then
    ws.symbol_hints <- None;
  Hashtbl.replace ws.docs uri doc;
  match doc.Document.file with
  | None -> ()
  | Some f ->
      Hashtbl.replace ws.files (normalize_path_key f) doc

let open_doc (ws:t) ~(uri:T.DocumentUri.t) ~(file:string option) ~(text:string) : unit =
  (* If no workspace root was set, fall back to the first opened file's directory. *)
  (match ws.root_path, file with
   | None, Some f ->
       ws.root_path <- Some (Filename.dirname f);
       rescan ws
   | _ -> ());
  let doc = Document.make ~uri ~file ~text in
  store_doc ws uri doc;
  pump_index_background ws

let change_doc (ws:t) ~(uri:T.DocumentUri.t) ~(changes:T.TextDocumentContentChangeEvent.t list) : unit =
  match Hashtbl.find_opt ws.docs uri with
  | None ->
      let file = Uri_path.file_path_of_uri uri in
      let base = Document.make ~uri ~file ~text:"" in
      let doc = Document.apply_changes_and_reparse ~changes base in
      store_doc ws uri doc;
      pump_index_background ws
  | Some doc ->
      let doc' = Document.apply_changes_and_reparse ~changes doc in
      store_doc ws uri doc';
      pump_index_background ws

let close_doc (ws:t) ~(uri:T.DocumentUri.t) : unit =
  (match Hashtbl.find_opt ws.docs uri with
   | Some d ->
       (match d.Document.compool_def with
        | Some name when normalize_name name <> "" -> ws.symbol_hints <- None
        | _ -> ())
   | None -> ());
  Hashtbl.remove ws.docs uri;
  pump_index_background ws

let apply_watched_file_changes
    (ws:t)
    ~(changes:(string * [ `Created | `Changed | `Deleted ]) list)
    : unit =
  ensure_index_started ws;
  let hints_dirty = ref false in
  (match ws.index with
   | None -> ()
   | Some idx ->
       List.iter (fun (path, kind) ->
         try
           let mapped_kind =
             match kind with
             | `Created -> Workspace_index.Created
             | `Changed -> Workspace_index.Changed
             | `Deleted -> Workspace_index.Deleted
           in
           if Workspace_index.apply_file_change idx ~path ~kind:mapped_kind then
             hints_dirty := true;
           Hashtbl.remove ws.files (normalize_path_key path)
         with _ -> ()
       ) changes;
       (try
          ignore
            (Workspace_index.scan_step idx
               ~max_dirs:index_background_dirs
               ~max_files:index_bootstrap_files)
        with _ -> ()));
  if !hints_dirty then ws.symbol_hints <- None

let revalidate_all (ws:t) : T.DocumentUri.t list =
  let uris = Hashtbl.fold (fun uri _ acc -> uri :: acc) ws.docs [] in
  List.iter (fun uri ->
    match Hashtbl.find_opt ws.docs uri with
    | None -> ()
    | Some doc -> store_doc ws uri doc
  ) uris;
  uris

let diagnostics_for (ws:t) ~(uri:T.DocumentUri.t) : T.Diagnostic.t list =
  match Hashtbl.find_opt ws.docs uri with
  | None -> []
  | Some doc -> Document.diagnostics doc

let ast_dump_for (ws:t) ~(uri:T.DocumentUri.t) : string option =
  match Hashtbl.find_opt ws.docs uri with
  | None -> None
  | Some doc -> Document.ast_dump doc

let cst_dump_for (ws:t) ~(uri:T.DocumentUri.t) : string option =
  match Hashtbl.find_opt ws.docs uri with
  | None -> None
  | Some doc ->
      let lexbuf = Lexing.from_string doc.Document.text in
      (match doc.Document.file with
       | None -> ()
       | Some f ->
           lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = f });
      let b = Buffer.create 4096 in
      Buffer.add_string b "CST (token stream)\n";
      let add_token_row idx tok sp ep lexeme =
        let line0 = max 1 sp.Lexing.pos_lnum in
        let line1 = max 1 ep.Lexing.pos_lnum in
        let col0 = max 0 (sp.Lexing.pos_cnum - sp.Lexing.pos_bol) in
        let col1 = max 0 (ep.Lexing.pos_cnum - ep.Lexing.pos_bol) in
        let tok_s = Parse.Debug.string_of_token tok in
        let lex = String.escaped lexeme in
        Buffer.add_string b
          (Printf.sprintf
             "%5d  %-14s %-28s @ %d:%d-%d:%d\n"
             idx
             tok_s
             ("\"" ^ lex ^ "\"")
             line0
             col0
             line1
             col1)
      in
      let rec loop idx =
        let tok = Lexer.token lexbuf in
        let sp = Lexing.lexeme_start_p lexbuf in
        let ep = Lexing.lexeme_end_p lexbuf in
        let lexeme = Lexing.lexeme lexbuf in
        add_token_row idx tok sp ep lexeme;
        match tok with
        | Parser.EOF -> ()
        | _ -> loop (idx + 1)
      in
      (try loop 1 with exn ->
         Buffer.add_string b
           (Printf.sprintf
              "\n<tokenization stopped: %s>\n"
              (Printexc.to_string exn)));
      Some (Buffer.contents b)

let lsp_pos_of_lex (p:Lexing.position) : Yojson.Safe.t =
  let line0 = max 0 (p.pos_lnum - 1) in
  let col0 = max 0 (p.pos_cnum - p.pos_bol) in
  `Assoc [ "line", `Int line0; "character", `Int col0 ]

let lsp_range_of_lex (sp:Lexing.position) (ep:Lexing.position) : Yojson.Safe.t =
  `Assoc [ "start", lsp_pos_of_lex sp; "end", lsp_pos_of_lex ep ]

type def = {
  uri : T.DocumentUri.t;
  name : string;
  key : string;
  loc : Ast.Loc.t;
  kind : int;
  container : string option;
}

let def_key (d:def) : string =
  Printf.sprintf "%s|%d|%d|%d|%d|%s"
    (Uri_path.docuri_to_string d.uri)
    d.loc.Ast.Loc.start_pos.line
    d.loc.Ast.Loc.start_pos.col
    d.loc.Ast.Loc.end_pos.line
    d.loc.Ast.Loc.end_pos.col
    d.key

let loc_key ~(uri:T.DocumentUri.t) (loc:Ast.Loc.t) : string =
  Printf.sprintf "%s|%d|%d|%d|%d"
    (Uri_path.docuri_to_string uri)
    loc.Ast.Loc.start_pos.line
    loc.Ast.Loc.start_pos.col
    loc.Ast.Loc.end_pos.line
    loc.Ast.Loc.end_pos.col

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' | '\'' -> true
  | _ -> false

let ast_pos_of_offset (idx:Text_index.t) (off:int) : Ast.Loc.pos =
  let line0, col0 = Text_index.line_col_of_offset idx off in
  { Ast.Loc.line = line0 + 1; col = col0; offset = off }

let loc_of_offsets ~(file:string option) ~(idx:Text_index.t) ~(s:int) ~(e:int) : Ast.Loc.t =
  Ast.Loc.make ~file ~start_pos:(ast_pos_of_offset idx s) ~end_pos:(ast_pos_of_offset idx e)

let add_def_raw (acc:def list) ~(uri:T.DocumentUri.t) ~(name:string) ~(loc:Ast.Loc.t) ~(kind:int) ~(container:string option) : def list =
  let key = normalize_name name in
  if key = "" then acc else { uri; name; key; loc; kind; container } :: acc

let add_ident_def (acc:def list) ~(uri:T.DocumentUri.t) ~(id:Ast.ident) ~(kind:int) ~(container:string option) : def list =
  add_def_raw acc ~uri ~name:id.v ~loc:id.loc ~kind ~container

let sym_kind_module = 2
let sym_kind_type = 5
let sym_kind_field = 8
let sym_kind_func = 12
let sym_kind_var = 13
let sym_kind_const = 14

let def_of_preprocess_define (doc:Document.t) (d:Preprocess.define) : def =
  {
    uri = doc.Document.uri;
    name = d.name;
    key = d.key;
    loc = d.loc;
    kind = sym_kind_const;
    container = None;
  }

let nth_opt (xs:'a list) (n:int) : 'a option =
  let rec go i = function
    | [] -> None
    | x :: tl ->
        if i = 0 then Some x else go (i - 1) tl
  in
  if n < 0 then None else go n xs

let tokenize_ident_words (line:string) : (string * int * int) list =
  let n = String.length line in
  let rec scan i acc =
    if i >= n then List.rev acc
    else if is_ident_char line.[i] then
      let j = ref (i + 1) in
      while !j < n && is_ident_char line.[!j] do incr j done;
      let tok = String.sub line i (!j - i) in
      scan !j ((tok, i, !j) :: acc)
    else
      scan (i + 1) acc
  in
  scan 0 []

let preceded_by_bang (line:string) (col:int) : bool =
  col > 0 && line.[col - 1] = '!'

let classify_fallback_decl ~(line:string) (tokens:(string * int * int) list) : (int * int) option =
  let classify kw_idx kw kw_col =
    match kw with
    | "ITEM" | "TABLE" -> Some (kw_idx, sym_kind_var)
    | "TYPE" -> Some (kw_idx, sym_kind_type)
    | "PROC" -> Some (kw_idx, sym_kind_func)
    | "DEFINE" -> Some (kw_idx, sym_kind_const)
    | "BLOCK" -> Some (kw_idx, sym_kind_module)
    | "COMPOOL" ->
        if preceded_by_bang line kw_col then None else Some (kw_idx, sym_kind_module)
    | _ -> None
  in
  let token_upper i =
    match nth_opt tokens i with
    | None -> None
    | Some (w, col, _) -> Some (normalize_name w, col)
  in
  match token_upper 0 with
  | None -> None
  | Some (kw0, col0) ->
      (match classify 0 kw0 col0 with
       | Some _ as hit -> hit
       | None ->
           if kw0 = "DEF" || kw0 = "REF" || kw0 = "STATIC" || kw0 = "CONSTANT" then
             (match token_upper 1 with
              | None -> None
              | Some (kw1, col1) -> classify 1 kw1 col1)
           else
             None)

let fallback_line_defs (doc:Document.t) : def list =
  let uri = doc.Document.uri in
  let idx = doc.Document.index in
  let file = doc.Document.file in
  let lines = String.split_on_char '\n' doc.Document.text in
  let defs_rev =
    lines
    |> List.mapi (fun line0 line -> (line0, line))
    |> List.fold_left (fun acc (line0, line) ->
         let tokens = tokenize_ident_words line in
         match classify_fallback_decl ~line tokens with
         | None -> acc
         | Some (kw_idx, kind) ->
             (match nth_opt tokens (kw_idx + 1), Text_index.line_start_offset idx ~line:line0 with
              | Some (name, c0, c1), Some base ->
                  let s = base + c0 in
                  let e = base + c1 in
                  let loc = loc_of_offsets ~file ~idx ~s ~e in
                  add_def_raw acc ~uri ~name ~loc ~kind ~container:None
              | _ -> acc)
       ) []
  in
  List.rev defs_rev

let max_fallback_scan_lines = 75_000

let rec collect_type_defs ~(uri:T.DocumentUri.t) ~(container:string option) (acc:def list) (t:Ast.type_expr Ast.node) : def list =
  match t.v with
  | Ast.TName _ -> acc
  | Ast.TPointer inner -> collect_type_defs ~uri ~container acc inner
  | Ast.TArray { elem; _ } -> collect_type_defs ~uri ~container acc elem
  | Ast.TRecord fields ->
      List.fold_left (fun a f ->
        let fv = f.v in
        let a = add_ident_def a ~uri ~id:fv.fname ~kind:sym_kind_field ~container in
        collect_type_defs ~uri ~container a fv.ftype
      ) acc fields
  | Ast.TFunc { params; returns } ->
      let acc =
        List.fold_left (fun a p ->
          let pv = p.v in
          let a = add_ident_def a ~uri ~id:pv.pname ~kind:sym_kind_var ~container in
          collect_type_defs ~uri ~container a pv.ptype
        ) acc params
      in
      (match returns with None -> acc | Some r -> collect_type_defs ~uri ~container acc r)

let rec collect_stmt_defs ~(uri:T.DocumentUri.t) ~(container:string option) (acc:def list) (s:Ast.stmt Ast.node) : def list =
  match s.v with
  | Ast.SEmpty -> acc
  | Ast.SDecl d -> collect_decl_defs ~uri ~container acc d
  | Ast.SBlock xs ->
      List.fold_left (collect_stmt_defs ~uri ~container) acc xs
  | Ast.SAssign _ -> acc
  | Ast.SCallStmt _ -> acc
  | Ast.SIf { then_; else_; _ } ->
      let acc = collect_stmt_defs ~uri ~container acc then_ in
      (match else_ with None -> acc | Some e -> collect_stmt_defs ~uri ~container acc e)
  | Ast.SWhile { body; _ } ->
      collect_stmt_defs ~uri ~container acc body
  | Ast.SFor { init; step; body; _ } ->
      let acc = (match init with None -> acc | Some i -> collect_stmt_defs ~uri ~container acc i) in
      let acc = (match step with None -> acc | Some st -> collect_stmt_defs ~uri ~container acc st) in
      collect_stmt_defs ~uri ~container acc body
  | Ast.SReturn _ -> acc
  | Ast.SLabel { label; body } ->
      let acc = add_ident_def acc ~uri ~id:label ~kind:sym_kind_var ~container in
      collect_stmt_defs ~uri ~container acc body
  | Ast.SGoto _ -> acc

and collect_decl_defs ~(uri:T.DocumentUri.t) ~(container:string option) (acc:def list) (d:Ast.decl Ast.node) : def list =
  match d.v with
  | Ast.DVar { name; dtype; _ } ->
      let acc = add_ident_def acc ~uri ~id:name ~kind:sym_kind_var ~container in
      collect_type_defs ~uri ~container acc dtype
  | Ast.DConst { name; dtype; _ } ->
      let acc = add_ident_def acc ~uri ~id:name ~kind:sym_kind_const ~container in
      (match dtype with None -> acc | Some ty -> collect_type_defs ~uri ~container acc ty)
  | Ast.DType { name; defn } ->
      let acc = add_ident_def acc ~uri ~id:name ~kind:sym_kind_type ~container in
      collect_type_defs ~uri ~container:(Some name.v) acc defn
  | Ast.DProc p ->
      let pv = p.v in
      let proc_name = pv.name.v in
      let acc = add_ident_def acc ~uri ~id:pv.name ~kind:sym_kind_func ~container in
      let in_proc = Some proc_name in
      let acc =
        List.fold_left (fun a prm ->
          let prm_v = prm.v in
          let a = add_ident_def a ~uri ~id:prm_v.pname ~kind:sym_kind_var ~container:in_proc in
          collect_type_defs ~uri ~container:in_proc a prm_v.ptype
        ) acc pv.params
      in
      let acc = List.fold_left (collect_decl_defs ~uri ~container:in_proc) acc pv.locals in
      collect_stmt_defs ~uri ~container:in_proc acc pv.body
  | Ast.DDirective _ -> acc

let find_compool_loc_in_doc (doc:Document.t) (key:string) : Ast.Loc.t option =
  let match_directive (d:Ast.decl Ast.node) =
    match d.v with
    | Ast.DDirective { name; args = first :: _ } ->
        let dir = normalize_name name.v in
        if (dir = "COMPOOL" || dir = "ICOMPOOL") && normalize_name first.v = key then
          Some first.loc
        else
          None
    | _ -> None
  in
  match doc.Document.ast with
  | None -> None
  | Some prog ->
      let rec go = function
        | [] -> None
        | Ast.TopDecl d :: tl ->
            (match match_directive d with
             | Some _ as hit -> hit
             | None -> go tl)
        | _ :: tl -> go tl
      in
      go prog

let collect_doc_defs (doc:Document.t) : def list =
  let uri = doc.Document.uri in
  let defs0 =
    match doc.Document.ast with
    | None -> []
    | Some prog ->
        List.fold_left (fun acc top ->
          match top with
            | Ast.TopDecl d -> collect_decl_defs ~uri ~container:None acc d
            | Ast.TopStmt s -> collect_stmt_defs ~uri ~container:None acc s
        ) [] prog
  in
  let defs =
    List.fold_left (fun acc (dm:Preprocess.define) ->
      add_def_raw
        acc
        ~uri
        ~name:dm.name
        ~loc:dm.loc
        ~kind:sym_kind_const
        ~container:None
    ) defs0 doc.Document.defines
  in
  let use_fallback_scan =
    let broken_or_partial = doc.Document.ast = None || doc.Document.parse_diags <> [] in
    broken_or_partial
    && Text_index.line_count doc.Document.index <= max_fallback_scan_lines
  in
  let defs =
    if use_fallback_scan then
      fallback_line_defs doc
      |> List.fold_left (fun acc d -> d :: acc) defs
    else
      defs
  in
  match doc.Document.compool_def with
  | None -> List.rev defs
  | Some name ->
      let k = normalize_name name in
      let defs =
        match find_compool_loc_in_doc doc k with
        | None -> defs
        | Some loc ->
            add_def_raw defs ~uri ~name:k ~loc ~kind:sym_kind_module ~container:None
      in
      List.rev defs

let position_in_loc (pos:T.Position.t) (loc:Ast.Loc.t) : bool =
  let line = pos.T.Position.line + 1 in
  let col = pos.T.Position.character in
  let sp = loc.Ast.Loc.start_pos in
  let ep = loc.Ast.Loc.end_pos in
  (line > sp.line || (line = sp.line && col >= sp.col))
  && (line < ep.line || (line = ep.line && col <= ep.col))

let word_at_position (doc:Document.t) (pos:T.Position.t) : (string * Ast.Loc.t) option =
  match Text_index.offset_of_line_col doc.Document.index ~line:pos.T.Position.line ~col:pos.T.Position.character with
  | None -> None
  | Some off ->
      let text = doc.Document.text in
      let n = String.length text in
      if n = 0 then None
      else
        let pivot =
          if off < n && is_ident_char text.[off] then Some off
          else if off > 0 && off - 1 < n && is_ident_char text.[off - 1] then Some (off - 1)
          else None
        in
        match pivot with
        | None -> None
        | Some i ->
            let a = ref i in
            while !a > 0 && is_ident_char text.[!a - 1] do decr a done;
            let b = ref (i + 1) in
            while !b < n && is_ident_char text.[!b] do incr b done;
            if !b <= !a then None
            else
              let name = String.sub text !a (!b - !a) in
              let loc = loc_of_offsets ~file:doc.Document.file ~idx:doc.Document.index ~s:!a ~e:!b in
              Some (name, loc)

let nav_word_at_position (doc:Document.t) (pos:T.Position.t) : (string * Ast.Loc.t) option =
  match word_at_position doc pos with
  | None -> None
  | Some (name, _) when is_reserved_keyword name -> None
  | Some x -> Some x

let has_define_key (doc:Document.t) (key:string) : bool =
  key <> "" && List.exists (fun (d:Preprocess.define) -> d.key = key) doc.Document.defines

let find_define_key_in_word
    (doc:Document.t)
    ~(word:string)
    ~(word_loc:Ast.Loc.t)
    ~(cursor_col:int)
  : string option =
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
      List.iter (fun (d:Preprocess.define) -> Hashtbl.replace uniq_keys_tbl d.key true) doc.Document.defines;
      let keys = Hashtbl.fold (fun k _ acc -> k :: acc) uniq_keys_tbl [] in
      let best : (string * int * int) option ref = ref None in
      let consider key start_idx len =
        match !best with
        | None -> best := Some (key, len, start_idx)
        | Some (_, best_len, best_start) ->
            if len > best_len || (len = best_len && start_idx >= best_start) then
              best := Some (key, len, start_idx)
      in
      List.iter (fun key ->
        let m = String.length key in
        if m > 0 && m <= n then (
          let rec scan i =
            if i + m > n then ()
            else (
              let rec eq j =
                j = m || (upper_word.[i + j] = key.[j] && eq (j + 1))
              in
              if eq 0 && rel >= i && rel < i + m then consider key i m;
              scan (i + 1)
            )
          in
          scan 0
        )
      ) keys;
      match !best with
      | None -> None
      | Some (key, _, _) -> Some key

let is_ws_char = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let skip_ws_forward (s:string) (i:int) : int =
  let n = String.length s in
  let rec go j =
    if j < n && is_ws_char s.[j] then go (j + 1) else j
  in
  go i

let parse_call_arg_count ~(text:string) ~(open_idx:int) : int option =
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
    while not !done_ && !i < n do
      let c = text.[!i] in
      if !in_single then (
        if c = '\'' then
          if !i + 1 < n && text.[!i + 1] = '\'' then
            i := !i + 1
          else
            in_single := false
      ) else if !in_double then (
        if c = '"' then
          if !i + 1 < n && text.[!i + 1] = '"' then
            i := !i + 1
          else
            in_double := false
      ) else (
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
        | ',' when !depth = 1 ->
            incr comma_count
        | _ ->
            if !depth = 1 && not (is_ws_char c) then seen_non_ws := true
      );
      incr i
    done;
    if !depth <> 0 then None
    else if not !seen_non_ws then Some 0
    else Some (!comma_count + 1)

let select_define_decl
    (doc:Document.t)
    ~(key:string)
    ~(call_ctx:bool)
    ~(arg_count:int option)
    ~(cursor_off:int option)
  : Preprocess.define option =
  let defs0 =
    doc.Document.defines
    |> List.filter (fun (d:Preprocess.define) -> d.key = key)
  in
  let defs1 =
    match cursor_off with
    | None -> defs0
    | Some off ->
        let before =
          defs0
          |> List.filter (fun (d:Preprocess.define) -> d.decl_start_off <= off)
        in
        if before = [] then defs0 else before
  in
  let defs2 =
    let same_call =
      defs1
      |> List.filter (fun (d:Preprocess.define) -> d.requires_call = call_ctx)
    in
    if same_call = [] then defs1 else same_call
  in
  let defs3 =
    match arg_count with
    | None -> defs2
    | Some n ->
        let same_arity =
          defs2
          |> List.filter (fun (d:Preprocess.define) -> List.length d.formals = n)
        in
        if same_arity = [] then defs2 else same_arity
  in
  defs3
  |> List.fold_left (fun best (d:Preprocess.define) ->
       match best with
       | None -> Some d
       | Some (cur:Preprocess.define) ->
           if d.decl_start_off >= cur.decl_start_off then Some d else Some cur
     ) None

let define_under_cursor (doc:Document.t) (pos:T.Position.t)
  : (Preprocess.define * Ast.Loc.t) option =
  match word_at_position doc pos with
  | None -> None
  | Some (word, word_loc) ->
      let key_opt =
        find_define_key_in_word doc ~word ~word_loc ~cursor_col:pos.character
      in
      (match key_opt with
       | None -> None
       | Some key ->
           let cursor_off =
             Text_index.offset_of_line_col doc.Document.index ~line:pos.line ~col:pos.character
           in
           let end_line0 = max 0 (word_loc.end_pos.line - 1) in
           let after_word_off =
             Text_index.offset_of_line_col
               doc.Document.index
               ~line:end_line0
               ~col:word_loc.end_pos.col
           in
           let call_ctx, arg_count =
             match after_word_off with
             | None -> (false, None)
             | Some off ->
                 let open_idx = skip_ws_forward doc.Document.text off in
                 if open_idx < String.length doc.Document.text && doc.Document.text.[open_idx] = '('
                 then
                   let argc = parse_call_arg_count ~text:doc.Document.text ~open_idx in
                   (true, argc)
                 else
                   (false, None)
           in
           (match select_define_decl doc ~key ~call_ctx ~arg_count ~cursor_off with
            | None -> None
            | Some d -> Some (d, word_loc)))

let location_json ~(uri:T.DocumentUri.t) (loc:Ast.Loc.t) : Yojson.Safe.t =
  T.Location.yojson_of_t (Lsp_conv.location_of_loc ~uri loc)

let range_json_of_loc (loc:Ast.Loc.t) : Yojson.Safe.t =
  T.Range.yojson_of_t (Lsp_conv.range_of_loc loc)

let symbol_json (d:def) : Yojson.Safe.t =
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

let doc_at_path (ws:t) (path:string) : Document.t option =
  let key = normalize_path_key path in
  let found_open = ref None in
  if Hashtbl.mem ws.files key then Hashtbl.find_opt ws.files key
  else (
    Hashtbl.iter (fun _uri doc ->
      match doc.Document.file with
      | Some p when same_path p path -> found_open := Some doc
      | _ -> ()
    ) ws.docs;
    match !found_open with
    | Some d ->
        Hashtbl.replace ws.files key d;
        Some d
    | None ->
        try
          let ic = open_in_bin path in
          let len = in_channel_length ic in
          let txt = really_input_string ic len in
          close_in_noerr ic;
          let uri =
            match Uri_path.docuri_of_path path with
            | Some u -> u
            | None ->
                (match T.DocumentUri.t_of_yojson (`String (Uri_path.file_uri_of_path path)) with
                 | u -> u
                 | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))
          in
          let d = Document.make ~uri ~file:(Some path) ~text:txt in
          Hashtbl.replace ws.files key d;
          Some d
        with _ -> None
  )

let resolve_import_paths (ws:t) (doc:Document.t) : string list =
  pump_index ws ~max_dirs:index_lookup_dirs ~max_files:index_lookup_files;
  match ws.index with
  | None -> []
  | Some idx ->
      let acc = Hashtbl.create 16 in
      List.iter (fun (imp:Preprocess.import) ->
        match imp.kind with
        | Preprocess.Compool ->
            (match Workspace_index.find_compool idx ~name:imp.name with
             | None -> ()
             | Some p -> Hashtbl.replace acc (normalize_path_key p) p)
      ) (Document.imports doc);
      Hashtbl.fold (fun _ p xs -> p :: xs) acc []

let docs_for_lookup (ws:t) (doc:Document.t) : Document.t list =
  let seen = Hashtbl.create 32 in
  let out = ref [] in
  let add_doc (d:Document.t) =
    let u = Uri_path.docuri_to_string d.Document.uri in
    if not (Hashtbl.mem seen u) then (
      Hashtbl.add seen u true;
      out := d :: !out
    )
  in
  add_doc doc;
  Hashtbl.iter (fun _ d -> add_doc d) ws.docs;
  resolve_import_paths ws doc
  |> List.iter (fun p -> match doc_at_path ws p with None -> () | Some d -> add_doc d);
  List.rev !out

let docs_for_rename (ws:t) (doc:Document.t) : Document.t list =
  pump_index ws ~max_dirs:index_lookup_dirs ~max_files:index_lookup_files;
  let seen = Hashtbl.create 64 in
  let out = ref [] in
  let add_doc (d:Document.t) =
    let u = Uri_path.docuri_to_string d.Document.uri in
    if not (Hashtbl.mem seen u) then (
      Hashtbl.add seen u true;
      out := d :: !out
    )
  in
  docs_for_lookup ws doc |> List.iter add_doc;
  Hashtbl.iter (fun _ d -> add_doc d) ws.docs;
  (match ws.index with
   | None -> ()
   | Some idx ->
       Workspace_index.all_source_paths idx
       |> List.iter (fun p ->
            match doc_at_path ws p with
            | None -> ()
            | Some d -> add_doc d));
  List.rev !out

let compare_pos (a:T.Position.t) (b:T.Position.t) : int =
  if a.line < b.line then -1
  else if a.line > b.line then 1
  else if a.character < b.character then -1
  else if a.character > b.character then 1
  else 0

let pos_in_range (p:T.Position.t) (r:T.Range.t) : bool =
  compare_pos r.start p <= 0 && compare_pos p r.end_ <= 0

let kind_name (k:int) : string =
  if k = sym_kind_module then "module"
  else if k = sym_kind_type then "type"
  else if k = sym_kind_field then "field"
  else if k = sym_kind_func then "procedure"
  else if k = sym_kind_var then "item"
  else if k = sym_kind_const then "constant"
  else "symbol"

type nav_binding = {
  sym_id : string;
  decl : def;
}

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

let nav_scope_copy (s:nav_scope) : nav_scope =
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

let def_symbol_id (d:def) : string =
  def_key d

let nav_add_occurrence (nav:doc_nav) ~(sym_id:string) ~(uri:T.DocumentUri.t) ~(loc:Ast.Loc.t) : unit =
  let k = Printf.sprintf "%s|%s" sym_id (loc_key ~uri loc) in
  if not (Hashtbl.mem nav.seen_occ k) then (
    Hashtbl.replace nav.seen_occ k true;
    let prev =
      match Hashtbl.find_opt nav.occs_by_id sym_id with
      | None -> []
      | Some xs -> xs
    in
    Hashtbl.replace nav.occs_by_id sym_id ((uri, loc) :: prev)
  )

let nav_add_decl (nav:doc_nav) (d:def) : nav_binding option =
  if d.key = "" then None
  else
    let sym_id = def_symbol_id d in
    Hashtbl.replace nav.defs_by_id sym_id d;
    nav_add_occurrence nav ~sym_id ~uri:d.uri ~loc:d.loc;
    Some { sym_id; decl = d }

let nav_bind_value (scope:nav_scope) (b:nav_binding) : unit =
  Hashtbl.replace scope.values b.decl.key b

let nav_bind_type (scope:nav_scope) (b:nav_binding) : unit =
  Hashtbl.replace scope.types b.decl.key b

let nav_bind_label (scope:nav_scope) (b:nav_binding) : unit =
  Hashtbl.replace scope.labels b.decl.key b

let nav_bind_field (scope:nav_scope) (b:nav_binding) : unit =
  Hashtbl.replace scope.fields b.decl.key b

let nav_bind_decl_default (scope:nav_scope) (b:nav_binding) : unit =
  if b.decl.kind = sym_kind_type then nav_bind_type scope b
  else if b.decl.kind = sym_kind_field then nav_bind_field scope b
  else nav_bind_value scope b

let nav_find_value (scope:nav_scope) (name:string) : nav_binding option =
  Hashtbl.find_opt scope.values (normalize_name name)

let nav_find_type (scope:nav_scope) (name:string) : nav_binding option =
  let k = normalize_name name in
  match Hashtbl.find_opt scope.types k with
  | Some _ as x -> x
  | None -> Hashtbl.find_opt scope.values k

let nav_find_label (scope:nav_scope) (name:string) : nav_binding option =
  let k = normalize_name name in
  match Hashtbl.find_opt scope.labels k with
  | Some _ as x -> x
  | None -> Hashtbl.find_opt scope.values k

let nav_find_field (scope:nav_scope) (name:string) : nav_binding option =
  let k = normalize_name name in
  match Hashtbl.find_opt scope.fields k with
  | Some _ as x -> x
  | None -> Hashtbl.find_opt scope.values k

let def_of_ident ~(uri:T.DocumentUri.t) ~(id:Ast.ident) ~(kind:int) ~(container:string option) : def option =
  let key = normalize_name id.v in
  if key = "" then None
  else Some { uri; name = id.v; key; loc = id.loc; kind; container }

let uniq_defs (xs:def list) : def list =
  let seen = Hashtbl.create 64 in
  let acc = ref [] in
  List.iter (fun d ->
    let k = def_key d in
    if not (Hashtbl.mem seen k) then (
      Hashtbl.add seen k true;
      acc := d :: !acc
    )
  ) xs;
  List.rev !acc

let exported_defs_for_import_scope (doc:Document.t) : def list =
  let block_containers =
    match doc.Document.ast with
    | None -> Hashtbl.create 1
    | Some prog -> block_proc_names_of_program prog
  in
  let is_exported (d:def) : bool =
    if d.kind = sym_kind_field then false
    else
      match d.container with
      | None -> true
      | Some c -> Hashtbl.mem block_containers (normalize_name c)
  in
  collect_doc_defs doc
  |> uniq_defs
  |> List.filter is_exported

let same_uri (a:T.DocumentUri.t) (b:T.DocumentUri.t) : bool =
  Uri_path.docuri_to_string a = Uri_path.docuri_to_string b

let loc_span_weight (loc:Ast.Loc.t) : int =
  let sp = loc.Ast.Loc.start_pos in
  let ep = loc.Ast.Loc.end_pos in
  let line_span = max 0 (ep.line - sp.line) in
  if line_span = 0 then max 0 (ep.col - sp.col)
  else (line_span * 10000) + max 0 ep.col

let symbol_at_position_in_nav (nav:doc_nav) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t)
  : (string * Ast.Loc.t) option =
  let best : (string * Ast.Loc.t * int) option ref = ref None in
  let consider (sym_id:string) (loc:Ast.Loc.t) =
    let span = loc_span_weight loc in
    match !best with
    | None -> best := Some (sym_id, loc, span)
    | Some (_, _, cur) when span < cur -> best := Some (sym_id, loc, span)
    | _ -> ()
  in
  Hashtbl.iter (fun sym_id occs ->
    List.iter (fun (u, loc) ->
      if same_uri u uri && position_in_loc pos loc then consider sym_id loc
    ) occs
  ) nav.occs_by_id;
  match !best with
  | None -> None
  | Some (sym_id, loc, _) -> Some (sym_id, loc)

let build_doc_nav (ws:t) (doc:Document.t) : doc_nav =
  let nav = doc_nav_create () in
  let uri = doc.Document.uri in
  let root_scope = nav_scope_empty () in

  let add_decl_binding
      (scope:nav_scope)
      ~(id:Ast.ident)
      ~(kind:int)
      ~(container:string option)
      (binder:nav_scope -> nav_binding -> unit)
      : unit =
    match def_of_ident ~uri ~id ~kind ~container with
    | None -> ()
    | Some d ->
        (match nav_add_decl nav d with
         | None -> ()
         | Some b -> binder scope b)
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

  let bind_external_def (scope:nav_scope) (d:def) : unit =
    if d.key <> "" then (
      let sym_id = def_symbol_id d in
      Hashtbl.replace nav.defs_by_id sym_id d;
      nav_bind_decl_default scope { sym_id; decl = d }
    )
  in

  let add_usage (b:nav_binding option) (id:Ast.ident) : unit =
    match b with
    | None -> ()
    | Some hit ->
        nav_add_occurrence nav ~sym_id:hit.sym_id ~uri ~loc:id.loc
  in

  let use_value (scope:nav_scope) (id:Ast.ident) : unit =
    add_usage (nav_find_value scope id.v) id
  in
  let use_type (scope:nav_scope) (id:Ast.ident) : unit =
    add_usage (nav_find_type scope id.v) id
  in
  let use_label (scope:nav_scope) (id:Ast.ident) : unit =
    add_usage (nav_find_label scope id.v) id
  in
  let use_field (scope:nav_scope) (id:Ast.ident) : unit =
    add_usage (nav_find_field scope id.v) id
  in

  let rec prebind_decl (scope:nav_scope) ~(container:string option) (d:Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DVar { name; _ } ->
        add_decl_default scope ~id:name ~kind:sym_kind_var ~container
    | Ast.DConst { name; _ } ->
        add_decl_default scope ~id:name ~kind:sym_kind_const ~container
    | Ast.DType { name; _ } ->
        add_decl_type scope ~id:name ~kind:sym_kind_type ~container
    | Ast.DProc p ->
        add_decl_value scope ~id:p.v.name ~kind:sym_kind_func ~container
    | Ast.DDirective _ ->
        ()

  and prebind_stmt (scope:nav_scope) ~(container:string option) (s:Ast.stmt Ast.node) : unit =
    match s.v with
    | Ast.SDecl d ->
        prebind_decl scope ~container d
    | Ast.SLabel { label; _ } ->
        add_decl_label scope ~id:label ~kind:sym_kind_var ~container
    | _ ->
        ()

  and walk_type (scope:nav_scope) ~(container:string option) (t:Ast.type_expr Ast.node) : unit =
    match t.v with
    | Ast.TName id ->
        use_type scope id
    | Ast.TPointer inner ->
        walk_type scope ~container inner
    | Ast.TArray { elem; dims } ->
        walk_type scope ~container elem;
        List.iter (walk_expr scope ~container) dims
    | Ast.TRecord fields ->
        List.iter (fun f ->
          let fv = f.v in
          add_decl_field scope ~id:fv.fname ~kind:sym_kind_field ~container;
          walk_type scope ~container fv.ftype
        ) fields
    | Ast.TFunc { params; returns } ->
        let fn_scope = nav_scope_copy scope in
        List.iter (fun prm ->
          add_decl_value fn_scope ~id:prm.v.pname ~kind:sym_kind_var ~container;
          walk_type fn_scope ~container prm.v.ptype
        ) params;
        (match returns with
         | None -> ()
         | Some r -> walk_type fn_scope ~container r)

  and walk_expr (scope:nav_scope) ~(container:string option) (e:Ast.expr Ast.node) : unit =
    match e.v with
    | Ast.EName id ->
        use_value scope id
    | Ast.ELit _ ->
        ()
    | Ast.EUnop { rhs; _ } ->
        walk_expr scope ~container rhs
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
         | Ast.EName id ->
             use_field scope id
         | Ast.EIndex { base; index } ->
             (match base.v with
              | Ast.EName id ->
                  use_field scope id
              | _ ->
                  walk_expr scope ~container base);
             List.iter (walk_expr scope ~container) index
         | _ ->
             walk_expr scope ~container field);
        walk_expr scope ~container ptr
    | Ast.EDeref { ptr } ->
        walk_expr scope ~container ptr
    | Ast.EParen x ->
        walk_expr scope ~container x

  and walk_decl (scope:nav_scope) ~(container:string option) (d:Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DVar { dtype; init; _ } ->
        walk_type scope ~container dtype;
        (match init with None -> () | Some e -> walk_expr scope ~container e)
    | Ast.DConst { dtype; value; _ } ->
        (match dtype with None -> () | Some t -> walk_type scope ~container t);
        walk_expr scope ~container value
    | Ast.DType { name; defn } ->
        walk_type scope ~container:(Some name.v) defn
    | Ast.DProc p ->
        let proc_container = Some p.v.name.v in
        let proc_scope = nav_scope_copy scope in
        (if nav_find_value proc_scope p.v.name.v = None then
           add_decl_value proc_scope ~id:p.v.name ~kind:sym_kind_func ~container);
        List.iter (fun prm ->
          add_decl_value proc_scope ~id:prm.v.pname ~kind:sym_kind_var ~container:proc_container
        ) p.v.params;
        List.iter (prebind_decl proc_scope ~container:proc_container) p.v.locals;
        List.iter (fun prm ->
          walk_type proc_scope ~container:proc_container prm.v.ptype
        ) p.v.params;
        (match p.v.returns with
         | None -> ()
         | Some r -> walk_type proc_scope ~container:proc_container r);
        List.iter (walk_decl proc_scope ~container:proc_container) p.v.locals;
        walk_stmt proc_scope ~container:proc_container p.v.body
    | Ast.DDirective _ ->
        ()

  and walk_stmt (scope:nav_scope) ~(container:string option) (s:Ast.stmt Ast.node) : unit =
    match s.v with
    | Ast.SEmpty ->
        ()
    | Ast.SDecl d ->
        prebind_decl scope ~container d;
        walk_decl scope ~container d
    | Ast.SBlock xs ->
        let block_scope = nav_scope_copy scope in
        walk_stmt_list block_scope ~container xs
    | Ast.SAssign { lhs; rhs } ->
        walk_expr scope ~container lhs;
        walk_expr scope ~container rhs
    | Ast.SCallStmt { callee; args } ->
        use_value scope callee;
        List.iter (walk_expr scope ~container) args
    | Ast.SIf { cond; then_; else_ } ->
        walk_expr scope ~container cond;
        let then_scope = nav_scope_copy scope in
        walk_stmt then_scope ~container then_;
        (match else_ with
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
        (match cond with None -> () | Some e -> walk_expr loop_scope ~container e);
        (match step with
         | None -> ()
         | Some st ->
             prebind_stmt loop_scope ~container st;
             walk_stmt loop_scope ~container st);
        let body_scope = nav_scope_copy loop_scope in
        walk_stmt body_scope ~container body
    | Ast.SReturn eo ->
        (match eo with None -> () | Some e -> walk_expr scope ~container e)
    | Ast.SLabel { body; _ } ->
        walk_stmt scope ~container body
    | Ast.SGoto id ->
        use_label scope id

  and walk_stmt_list (scope:nav_scope) ~(container:string option) (xs:Ast.stmt Ast.node list) : unit =
    List.iter (prebind_stmt scope ~container) xs;
    List.iter (walk_stmt scope ~container) xs
  in

  let bind_imports () =
    let is_importable_def (d:def) : bool =
      d.kind <> sym_kind_module && d.kind <> sym_kind_func
    in
    sem_import_dirs doc
    |> List.iter (fun (imp:compool_import_dir) ->
         match resolve_compool_doc_uncached ws ~name:imp.compool with
         | None -> ()
         | Some target ->
             let defs = exported_defs_for_import_scope target in
             if imp.selected = [] then
               List.iter (fun d ->
                 if is_importable_def d then bind_external_def root_scope d
               ) defs
             else
               let selected = Hashtbl.create 32 in
               List.iter (fun (nm, _loc) ->
                 Hashtbl.replace selected (normalize_name nm) true
               ) imp.selected;
               List.iter (fun d ->
                 if is_importable_def d && Hashtbl.mem selected d.key then
                   bind_external_def root_scope d
                ) defs)
  in

  let bind_defines () =
    List.iter (fun (dm:Preprocess.define) ->
      let d = def_of_preprocess_define doc dm in
      match nav_add_decl nav d with
      | None -> ()
      | Some b -> nav_bind_value root_scope b
    ) doc.Document.defines
  in

  bind_imports ();
  bind_defines ();
  (match doc.Document.ast with
   | None -> ()
   | Some prog ->
       List.iter (function
         | Ast.TopDecl d -> prebind_decl root_scope ~container:None d
         | Ast.TopStmt s -> prebind_stmt root_scope ~container:None s
       ) prog;
       List.iter (function
         | Ast.TopDecl d -> walk_decl root_scope ~container:None d
         | Ast.TopStmt s -> walk_stmt root_scope ~container:None s
       ) prog);
  nav

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

let source_line_for_def (ws:t) (d:def) : string option =
  let doc_opt =
    match doc_of_uri ws d.uri with
    | Some d0 -> Some d0
    | None ->
        (match d.loc.Ast.Loc.file with
         | Some p -> doc_at_path ws p
         | None -> None)
  in
  match doc_opt with
  | None -> None
  | Some d0 ->
      line_text_in_doc d0 ~line1:d.loc.Ast.Loc.start_pos.line

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
           | Some p -> doc_at_path ws p
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
  pump_index ws ~max_dirs:index_lookup_dirs ~max_files:index_lookup_files;
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
                 if Workspace_index.is_complete idx then
                   find_compool_path_fallback ws ~key
                 else
                   None)
        | None ->
            find_compool_path_fallback ws ~key
      in
      (match path_opt with
       | None -> None
       | Some path -> doc_at_path ws path)

let nav_for_doc_cached
    (ws:t)
    (cache:(string, doc_nav) Hashtbl.t)
    (doc:Document.t)
    : doc_nav =
  let k = Uri_path.docuri_to_string doc.Document.uri in
  match Hashtbl.find_opt cache k with
  | Some nav -> nav
  | None ->
      let nav = build_doc_nav ws doc in
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
    let local_hits = collect (docs_for_lookup ws doc) in
    if local_hits <> [] then local_hits
    else collect (docs_for_rename ws doc)

let allow_unscoped_fallback (doc:Document.t) : bool =
  doc.Document.ast = None || doc.Document.parse_diags <> []

let is_ref_proc_decl_line (line:string) : bool =
  let toks =
    tokenize_ident_words (normalize_name line)
    |> List.map (fun (w, _, _) -> w)
  in
  match toks with
  | "REF" :: "PROC" :: _ -> true
  | _ -> false

let is_likely_proc_implementation (ws:t) (d:def) : bool =
  if d.kind <> sym_kind_func then true
  else
    match source_line_for_def ws d with
    | None -> true
    | Some line -> not (is_ref_proc_decl_line line)

let proc_defs_by_key (ws:t) (doc:Document.t) ~(key:string) : def list =
  if key = "" then []
  else
    docs_for_rename ws doc
    |> List.concat_map collect_doc_defs
    |> List.filter (fun d -> d.kind = sym_kind_func && d.key = key)
    |> uniq_defs

let proc_impl_defs_by_key (ws:t) (doc:Document.t) ~(key:string) : def list =
  let defs = proc_defs_by_key ws doc ~key in
  let impls = List.filter (is_likely_proc_implementation ws) defs in
  if impls = [] then defs else impls

let definition_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Null
  | Some doc ->
      match import_under_cursor doc pos with
      | Some imp ->
          let hits = defs_for_import_cursor ws imp in
          if hits = [] then `Null else `List (List.map def_to_loc_json hits)
      | None ->
          (match define_under_cursor doc pos with
           | Some (dm, _) ->
               let d = def_of_preprocess_define doc dm in
               `List [ def_to_loc_json d ]
           | None ->
          let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 32 in
          let nav = nav_for_doc_cached ws cache doc in
          let docs = docs_for_lookup ws doc in
          let hit =
            match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
            | None -> None
            | Some (sym_id, _) ->
                (match Hashtbl.find_opt nav.defs_by_id sym_id with
                 | Some _ as d0 -> d0
                 | None -> find_def_for_sym_id ws cache ~docs ~sym_id)
          in
          (match hit with
           | Some d ->
               let defs =
                 if d.kind = sym_kind_func && not (is_likely_proc_implementation ws d)
                 then
                   let impls = proc_impl_defs_by_key ws doc ~key:d.key in
                   if impls = [] then [ d ] else impls
                 else
                   [ d ]
               in
               `List (List.map def_to_loc_json defs)
           | None ->
               let proc_by_name =
                 match nav_word_at_position doc pos with
                 | None -> []
                 | Some (nm, _) ->
                     let key = normalize_name nm in
                     if key = "" then [] else proc_impl_defs_by_key ws doc ~key
               in
               if proc_by_name <> [] then
                 `List (List.map def_to_loc_json proc_by_name)
               else if not (allow_unscoped_fallback doc) then `Null
               else
                 let by_name =
                   match nav_word_at_position doc pos with
                   | None -> []
                   | Some (nm, _) -> fallback_defs_by_name ws doc (normalize_name nm)
                  in
                  if by_name = [] then `Null
                  else `List (List.map def_to_loc_json by_name)))

let implementation_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Null
  | Some doc ->
      (match import_under_cursor doc pos with
       | Some _ ->
           definition_json_for ws ~uri ~pos
       | None ->
           let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 32 in
           let nav = nav_for_doc_cached ws cache doc in
           let docs = docs_for_lookup ws doc in
           let defs =
             match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
             | None -> []
             | Some (sym_id, _) ->
                 docs
                 |> List.filter_map (fun d ->
                      let dnav = nav_for_doc_cached ws cache d in
                      Hashtbl.find_opt dnav.defs_by_id sym_id)
                 |> uniq_defs
           in
           let defs =
             if defs = [] then defs
             else
               let impls = List.filter (is_likely_proc_implementation ws) defs in
               if impls = [] then [] else impls
           in
           let key_opt =
             match defs with
             | d :: _ when d.key <> "" -> Some d.key
             | _ ->
                 (match nav_word_at_position doc pos with
                  | Some (nm, _) ->
                      let key = normalize_name nm in
                      if key = "" then None else Some key
                  | None -> None)
           in
           let defs =
             if defs <> [] then defs
             else
               match key_opt with
               | None -> []
               | Some key -> proc_impl_defs_by_key ws doc ~key
           in
           if defs = [] then definition_json_for ws ~uri ~pos
           else `List (List.map def_to_loc_json defs))

let occurrences_in_doc_fallback (doc:Document.t) ~(key:string) : (T.DocumentUri.t * Ast.Loc.t) list =
  let text = doc.Document.text in
  let n = String.length text in
  let rec loop i acc =
    if i >= n then List.rev acc
    else if is_ident_char text.[i] && (i = 0 || not (is_ident_char text.[i - 1])) then
      let j = ref (i + 1) in
      while !j < n && is_ident_char text.[!j] do incr j done;
      let tok = String.sub text i (!j - i) in
      let acc =
        if normalize_name tok = key then
          let loc = loc_of_offsets ~file:doc.Document.file ~idx:doc.Document.index ~s:i ~e:!j in
          (doc.Document.uri, loc) :: acc
        else
          acc
      in
      loop !j acc
    else
      loop (i + 1) acc
  in
  loop 0 []

let occurrences_in_doc (doc:Document.t) ~(key:string) : (T.DocumentUri.t * Ast.Loc.t) list =
  try
    let lexbuf = Lexing.from_string doc.Document.text in
    (match doc.Document.file with
     | None -> ()
     | Some f ->
         lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = f });
    let rec loop acc =
      let tok = Lexer.token lexbuf in
      let sp = Lexing.lexeme_start_p lexbuf in
      let ep = Lexing.lexeme_end_p lexbuf in
      match tok with
      | Parser.EOF ->
          List.rev acc
      | Parser.ID s when normalize_name s = key ->
          let loc = Ast.Loc.of_lexing_positions sp ep ~file:doc.Document.file in
          loop ((doc.Document.uri, loc) :: acc)
      | _ ->
          loop acc
    in
    loop []
  with _ ->
    occurrences_in_doc_fallback doc ~key

let references_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) ~(include_decl:bool) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `List []
  | Some doc ->
      match import_under_cursor doc pos with
      | Some imp ->
          let key = normalize_name imp.name in
          if key = "" then `List []
          else
            let docs = docs_for_lookup ws doc in
            let defs =
              docs
              |> List.concat_map collect_doc_defs
              |> List.filter (fun d -> d.key = key)
            in
            let def_keys =
              let h = Hashtbl.create 32 in
              List.iter (fun d -> Hashtbl.replace h (loc_key ~uri:d.uri d.loc) true) defs;
              h
            in
            let occs = docs |> List.concat_map (fun d -> occurrences_in_doc d ~key) in
            let occs =
              if include_decl then occs
              else
                List.filter (fun (u, loc) -> not (Hashtbl.mem def_keys (loc_key ~uri:u loc))) occs
            in
            let seen = Hashtbl.create 128 in
            let json =
              occs
              |> List.filter_map (fun (u, loc) ->
                   let k = loc_key ~uri:u loc in
                   if Hashtbl.mem seen k then None
                   else (
                     Hashtbl.add seen k true;
                     Some (location_json ~uri:u loc)
                   ))
            in
            `List json
      | None ->
          let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 64 in
          let nav = nav_for_doc_cached ws cache doc in
          let resolved =
            symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos
          in
          (match resolved with
           | Some (sym_id, _) ->
               let docs = docs_for_rename ws doc in
               let sym_defs =
                 docs
                 |> List.filter_map (fun d ->
                      let dnav = nav_for_doc_cached ws cache d in
                      Hashtbl.find_opt dnav.defs_by_id sym_id)
                 |> uniq_defs
               in
               let decl_keys = Hashtbl.create 8 in
               List.iter (fun defn ->
                 Hashtbl.replace decl_keys (loc_key ~uri:defn.uri defn.loc) true
               ) sym_defs;
               let base_occs =
                 docs
                 |> List.concat_map (fun d ->
                      let dnav = nav_for_doc_cached ws cache d in
                      match Hashtbl.find_opt dnav.occs_by_id sym_id with
                      | None -> []
                      | Some xs -> xs)
               in
               let occs =
                 let needs_proc_link =
                   sym_defs <> []
                   && List.for_all
                        (fun d -> d.kind = sym_kind_func && not (is_likely_proc_implementation ws d))
                        sym_defs
                 in
                 if not needs_proc_link then base_occs
                 else
                   let key =
                     match sym_defs with
                     | d :: _ -> d.key
                     | [] -> ""
                   in
                   if key = "" then base_occs
                   else
                     let known =
                       let h = Hashtbl.create 8 in
                       List.iter (fun d -> Hashtbl.replace h (def_key d) true) sym_defs;
                       h
                     in
                     let extra_defs =
                       proc_defs_by_key ws doc ~key
                       |> List.filter (fun d -> not (Hashtbl.mem known (def_key d)))
                     in
                     List.iter (fun d ->
                       Hashtbl.replace decl_keys (loc_key ~uri:d.uri d.loc) true
                     ) extra_defs;
                     let extra_ids =
                       extra_defs
                       |> List.map def_symbol_id
                       |> List.sort_uniq String.compare
                     in
                     let extra_occs =
                       extra_ids
                       |> List.concat_map (fun sid ->
                            docs
                            |> List.concat_map (fun d ->
                                 let dnav = nav_for_doc_cached ws cache d in
                                 match Hashtbl.find_opt dnav.occs_by_id sid with
                                 | None -> []
                                 | Some xs -> xs))
                     in
                     base_occs @ extra_occs
               in
               let occs =
                 if include_decl then occs
                 else
                   List.filter (fun (u, loc) ->
                     not (Hashtbl.mem decl_keys (loc_key ~uri:u loc))
                   ) occs
               in
               let seen = Hashtbl.create 256 in
               let json =
                 occs
                 |> List.filter_map (fun (u, loc) ->
                      let k = loc_key ~uri:u loc in
                      if Hashtbl.mem seen k then None
                      else (
                        Hashtbl.add seen k true;
                        Some (location_json ~uri:u loc)
                      ))
               in
               `List json
           | None ->
               (match nav_word_at_position doc pos with
                | None -> `List []
                | Some (nm, _) ->
                    let key = normalize_name nm in
                    if key = "" then `List []
                    else
                      let docs = docs_for_rename ws doc in
                      let proc_defs = proc_defs_by_key ws doc ~key in
                      let defs =
                        if proc_defs <> [] then proc_defs
                        else if allow_unscoped_fallback doc then
                          docs
                          |> List.concat_map collect_doc_defs
                          |> List.filter (fun d -> d.key = key)
                        else
                          []
                      in
                      if defs = [] then `List []
                      else
                        let def_keys =
                          let h = Hashtbl.create 32 in
                          List.iter (fun d -> Hashtbl.replace h (loc_key ~uri:d.uri d.loc) true) defs;
                          h
                        in
                        let occs = docs |> List.concat_map (fun d -> occurrences_in_doc d ~key) in
                        let occs =
                          if include_decl then occs
                          else
                            List.filter (fun (u, loc) -> not (Hashtbl.mem def_keys (loc_key ~uri:u loc))) occs
                        in
                        let seen = Hashtbl.create 128 in
                        let json =
                          occs
                          |> List.filter_map (fun (u, loc) ->
                               let k = loc_key ~uri:u loc in
                               if Hashtbl.mem seen k then None
                               else (
                                 Hashtbl.add seen k true;
                                 Some (location_json ~uri:u loc)
                               ))
                        in
                        `List json))

let hover_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Null
  | Some doc ->
      let import_text =
        match import_under_cursor doc pos with
        | None -> None
        | Some imp ->
            let resolved =
              match ws.index with
              | None -> None
              | Some idx -> Workspace_index.find_compool idx ~name:imp.name
            in
            Some
              (match resolved with
               | None -> Printf.sprintf "COMPOOL `%s` (unresolved in workspace index)." imp.name
               | Some p -> Printf.sprintf "COMPOOL `%s` -> `%s`" imp.name (Filename.basename p))
      in
      (match define_under_cursor doc pos with
       | Some (dm, word_loc) ->
           let d = def_of_preprocess_define doc dm in
           let primary_decl =
             match source_line_for_def ws d with
             | Some line when String.trim line <> "" -> line
             | _ ->
                 if dm.requires_call then
                   Printf.sprintf "DEFINE %s(%s) \"%s\";"
                     dm.name
                     (String.concat "," dm.formals)
                     dm.body
                 else
                   Printf.sprintf "DEFINE %s \"%s\";" dm.name dm.body
           in
           let head =
             Printf.sprintf "### define `%s`\nDefined at `%s`"
               d.name
               (file_line_of_def d)
           in
           let decl_block =
             let line = truncate_text 280 primary_decl in
             if String.trim line = "" then ""
             else Printf.sprintf "\n```jovial\n%s\n```" line
           in
           let expansion =
             if dm.formals = [] then
               Printf.sprintf "\nExpands to: `%s`" (truncate_text 180 dm.body)
             else
               Printf.sprintf
                 "\nFormals: `%s`\nExpansion template: `%s`"
                 (String.concat ", " dm.formals)
                 (truncate_text 180 dm.body)
           in
           let body = head ^ decl_block ^ expansion in
           `Assoc [
             "contents",
             `Assoc [ "kind", `String "markdown"; "value", `String body ];
             "range", range_json_of_loc word_loc;
           ]
       | None ->
      let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 32 in
      let nav = nav_for_doc_cached ws cache doc in
      let word = nav_word_at_position doc pos in
      let resolved = symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos in
      let defs, hover_loc =
        match resolved with
        | Some (sym_id, loc) ->
            let docs = docs_for_lookup ws doc in
            let defn =
              match Hashtbl.find_opt nav.defs_by_id sym_id with
              | Some _ as d0 -> d0
              | None -> find_def_for_sym_id ws cache ~docs ~sym_id
            in
            (match defn with
             | Some d -> ([d], Some loc)
             | None ->
                 (match word with
                  | None -> ([], Some loc)
                 | Some (nm, wloc) ->
                      let defs0 =
                        if allow_unscoped_fallback doc
                        then fallback_defs_by_name ws doc (normalize_name nm)
                        else []
                      in
                      (defs0, Some wloc)))
        | None ->
            (match word with
             | None -> ([], None)
             | Some (nm, wloc) ->
                 let defs0 =
                   if allow_unscoped_fallback doc
                   then fallback_defs_by_name ws doc (normalize_name nm)
                   else []
                 in
                 (defs0, Some wloc))
      in
      let defs =
        match defs with
        | d :: _ when d.kind = sym_kind_func && not (is_likely_proc_implementation ws d) ->
            let impls = proc_impl_defs_by_key ws doc ~key:d.key in
            if impls = [] then defs else impls
        | [] ->
            (match word with
             | None -> []
             | Some (nm, _) ->
                 let key = normalize_name nm in
                 if key = "" then [] else proc_impl_defs_by_key ws doc ~key)
        | _ ->
            defs
      in
      if defs = [] then
        (match import_text with
         | None ->
             (match word with
              | None -> `Null
              | Some (nm, word_loc) ->
                  `Assoc [
                    "contents",
                    `Assoc [
                      "kind", `String "markdown";
                      "value",
                      `String (Printf.sprintf "No definition found for `%s` in current workspace scope." nm);
                    ];
                    "range", range_json_of_loc word_loc;
                  ])
         | Some txt ->
             `Assoc [
               "contents", `Assoc [ "kind", `String "markdown"; "value", `String txt ];
             ])
      else
        let top_lines =
          defs
          |> List.map (fun d ->
               let head =
                 Printf.sprintf "### %s `%s`\nDefined at `%s`"
                   (kind_name d.kind) d.name (file_line_of_def d)
               in
               let primary_decl =
                 match proc_signature_for_def ws d with
                 | Some sig_line -> Some sig_line
                 | None -> source_line_for_def ws d
               in
               let decl_block =
                 match primary_decl with
                 | None -> ""
                 | Some line ->
                     let line = truncate_text 280 line in
                     if String.trim line = "" then ""
                     else Printf.sprintf "\n```jovial\n%s\n```" line
               in
               let extra =
                 match source_line_for_def ws d, proc_signature_for_def ws d with
                 | Some src, Some sig_line when String.trim src <> String.trim sig_line ->
                     let src = truncate_text 280 src in
                     if String.trim src = "" then ""
                     else Printf.sprintf "\nDeclaration:\n```jovial\n%s\n```" src
                 | _ -> ""
               in
               head ^ decl_block ^ extra)
        in
        let lines =
          match import_text with
          | None -> top_lines
          | Some imp -> imp :: "" :: top_lines
        in
        let body = String.concat "\n" lines in
        let base =
          [ "contents", `Assoc [ "kind", `String "markdown"; "value", `String body ] ]
        in
        let with_range =
          match hover_loc with
          | None -> base
          | Some loc -> ("range", range_json_of_loc loc) :: base
        in
        `Assoc with_range)

let prepare_rename_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Null
  | Some doc ->
      let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 64 in
      let nav = nav_for_doc_cached ws cache doc in
      (match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
       | Some (sym_id, sym_loc) ->
           let docs = docs_for_rename ws doc in
           let has_any =
             docs
             |> List.exists (fun d ->
                  let dnav = nav_for_doc_cached ws cache d in
                  match Hashtbl.find_opt dnav.occs_by_id sym_id with
                  | None -> false
                  | Some xs -> xs <> [])
           in
           if not has_any then `Null
           else
             let placeholder =
               match word_at_position doc pos with
               | Some (nm, _) -> nm
               | None ->
                   (match Hashtbl.find_opt nav.defs_by_id sym_id with
                    | Some d -> d.name
                    | None -> "name")
             in
             `Assoc [
               "range", range_json_of_loc sym_loc;
               "placeholder", `String placeholder;
             ]
       | None ->
           (match nav_word_at_position doc pos with
            | None -> `Null
            | Some (nm, word_loc) ->
                let key = normalize_name nm in
                if key = "" || not (allow_unscoped_fallback doc) then `Null
                else
                  let has_any =
                    docs_for_rename ws doc
                    |> List.exists (fun d -> occurrences_in_doc d ~key <> [])
                  in
                  if not has_any then `Null
                  else
                    `Assoc [
                      "range", range_json_of_loc word_loc;
                      "placeholder", `String nm;
                    ]))

let rename_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) ~(new_name:string) : Yojson.Safe.t =
  if not (is_valid_rename_name new_name) then `Null
  else
    match doc_of_uri ws uri with
    | None -> `Null
    | Some doc ->
        let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 64 in
        let nav = nav_for_doc_cached ws cache doc in
        let docs = docs_for_rename ws doc in
        let seen = Hashtbl.create 1024 in
        let edits_by_uri : (string, Yojson.Safe.t list) Hashtbl.t = Hashtbl.create 128 in
        let add_edit (u:T.DocumentUri.t) (loc:Ast.Loc.t) =
          let lk = loc_key ~uri:u loc in
          if not (Hashtbl.mem seen lk) then (
            Hashtbl.add seen lk true;
            let uri_s = Uri_path.docuri_to_string u in
            let edit =
              `Assoc [
                "range", range_json_of_loc loc;
                "newText", `String new_name;
              ]
            in
            let prev =
              match Hashtbl.find_opt edits_by_uri uri_s with
              | None -> []
              | Some xs -> xs
            in
            Hashtbl.replace edits_by_uri uri_s (edit :: prev)
          )
        in
        let apply_changes () =
          let changes =
            Hashtbl.fold (fun uri_s edits acc ->
              (uri_s, `List (List.rev edits)) :: acc
            ) edits_by_uri []
          in
          if changes = [] then `Null
          else `Assoc [ "changes", `Assoc changes ]
        in
        (match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
         | Some (sym_id, _) ->
             List.iter (fun d ->
               let dnav = nav_for_doc_cached ws cache d in
               match Hashtbl.find_opt dnav.occs_by_id sym_id with
               | None -> ()
               | Some xs ->
                   List.iter (fun (u, loc) -> add_edit u loc) xs
             ) docs;
             apply_changes ()
         | None ->
             (match nav_word_at_position doc pos with
              | None -> `Null
              | Some (nm, _) ->
                  let key = normalize_name nm in
                  if key = "" || not (allow_unscoped_fallback doc) then `Null
                  else (
                    List.iter (fun d ->
                      occurrences_in_doc d ~key
                      |> List.iter (fun (u, loc) -> add_edit u loc)
                    ) docs;
                    apply_changes ()
                  )))

let starts_with_ci ~(prefix:string) (s:string) : bool =
  let p = normalize_name prefix in
  if p = "" then true
  else
    let u = normalize_name s in
    let lp = String.length p in
    String.length u >= lp && String.sub u 0 lp = p

let completion_item_kind_of_def_kind (k:int) : int =
  if k = sym_kind_module then 9
  else if k = sym_kind_type then 7
  else if k = sym_kind_field then 10
  else if k = sym_kind_func then 3
  else if k = sym_kind_const then 21
  else 6

let completion_item_json
    ~(label:string)
    ~(kind:int)
    ?detail
    ?insert_text
    ?sort_text
    ()
  : Yojson.Safe.t =
  let fields =
    [
      ("label", `String label);
      ("kind", `Int kind);
    ]
  in
  let fields =
    match detail with
    | Some d when String.trim d <> "" -> ("detail", `String d) :: fields
    | _ -> fields
  in
  let fields =
    match insert_text with
    | Some txt when txt <> "" -> ("insertText", `String txt) :: fields
    | _ -> fields
  in
  let fields =
    match sort_text with
    | Some st when st <> "" -> ("sortText", `String st) :: fields
    | _ -> fields
  in
  `Assoc fields

let completion_keywords : (string * int * string option) list =
  [
    ("START", 14, None);
    ("TERM", 14, None);
    ("BEGIN", 14, None);
    ("END", 14, None);
    ("DEF", 14, None);
    ("REF", 14, None);
    ("STATIC", 14, None);
    ("CONSTANT", 14, None);
    ("PROC", 14, Some "procedure declaration");
    ("ITEM", 14, Some "item declaration");
    ("TABLE", 14, Some "table declaration");
    ("TYPE", 14, Some "type declaration");
    ("IF", 14, None);
    ("THEN", 14, None);
    ("ELSE", 14, None);
    ("WHILE", 14, None);
    ("FOR", 14, None);
    ("BY", 14, None);
    ("FALLTHRU", 14, None);
    ("RETURN", 14, None);
    ("GOTO", 14, None);
    ("EXIT", 14, None);
    ("ABORT", 14, None);
    ("STOP", 14, None);
    ("CASE", 14, None);
    ("DEFAULT", 14, None);
    ("COMPOOL", 14, Some "compool directive");
    ("ICOMPOOL", 14, Some "import compool directive");
    ("ICOPY", 14, Some "copy directive");
    ("ISKIP", 14, Some "skip directive");
    ("IBEGIN", 14, Some "directive scope begin");
    ("IEND", 14, Some "directive scope end");
    ("ILINKAGE", 14, Some "linkage directive");
    ("ITRACE", 14, Some "trace directive");
    ("IINTERFERENCE", 14, Some "interference directive");
    ("IREDUCIBLE", 14, Some "reducible directive");
    ("ILIST", 14, Some "listing directive");
    ("INOLIST", 14, Some "listing directive");
    ("IEJECT", 14, Some "listing directive");
    ("IBASE", 14, Some "base directive");
    ("IISBASE", 14, Some "base directive");
    ("IDROP", 14, Some "drop directive");
    ("ILEFTRIGHT", 14, Some "layout directive");
    ("IREARRANGE", 14, Some "rearrange directive");
    ("IINITIALIZE", 14, Some "initialize directive");
    ("IORDER", 14, Some "order directive");
    ("DEFINE", 14, Some "macro directive");
    ("PROGRAM", 14, Some "program directive");
    ("BLOCK", 14, Some "block directive");
    ("REC", 14, Some "recursive subroutine");
    ("RENT", 14, Some "reentrant subroutine");
    ("LISTEXP", 14, Some "define list option");
    ("LISTINV", 14, Some "define list option");
    ("LISTBOTH", 14, Some "define list option");
    ("INLINE", 14, Some "inline declaration");
    ("LABEL", 14, Some "statement-name declaration");
    ("LIKE", 14, Some "table type option");
    ("OVERLAY", 14, Some "overlay declaration");
    ("PARALLEL", 14, Some "table structure option");
    ("POS", 14, Some "preset positioner");
    ("INSTANCE", 14, Some "def block instance");
    ("NULL", 14, Some "pointer literal");
    ("TRUE", 14, None);
    ("FALSE", 14, None);
    ("MOD", 14, None);
    ("AND", 14, None);
    ("OR", 14, None);
    ("NOT", 14, None);
    ("XOR", 14, None);
    ("EQV", 14, None);
  ]

let completion_types_builtin : (string * int * string option) list =
  [
    ("A", 7, Some "fixed type indicator");
    ("B", 7, Some "built-in type");
    ("U", 7, Some "built-in type");
    ("S", 7, Some "built-in type");
    ("F", 7, Some "built-in type");
    ("C", 7, Some "built-in type");
    ("P", 7, Some "built-in type");
    ("W", 7, Some "compatibility type marker");
    ("V", 7, Some "compatibility/status marker");
    ("STATUS", 7, Some "built-in type");
  ]

let completion_functions_builtin : (string * int * string option) list =
  [
    ("ABS", 3, Some "built-in function");
    ("BIT", 3, Some "built-in function");
    ("BITSIZE", 3, Some "built-in function");
    ("BYTE", 3, Some "built-in function");
    ("BYTESIZE", 3, Some "built-in function");
    ("FIRST", 3, Some "built-in function");
    ("LAST", 3, Some "built-in function");
    ("LBOUND", 3, Some "built-in function");
    ("LOC", 3, Some "built-in function");
    ("NEXT", 3, Some "built-in function");
    ("NWDSEN", 3, Some "built-in function");
    ("REP", 3, Some "built-in function");
    ("SGN", 3, Some "built-in function");
    ("SHIFTL", 3, Some "built-in function");
    ("SHIFTR", 3, Some "built-in function");
    ("UBOUND", 3, Some "built-in function");
    ("V", 3, Some "built-in status constructor");
    ("WORDSIZE", 3, Some "built-in function");
  ]

let completion_snippets : (string * string * int * string option) list =
  [
    ("!COMPOOL", "!COMPOOL(\"COMP\");", 15, Some "import compool");
    ("!ICOMPOOL", "!ICOMPOOL(\"COMP\");", 15, Some "import compool");
  ]

let completion_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `List []
  | Some doc ->
      let prefix =
        match word_at_position doc pos with
        | None -> ""
        | Some (nm, _) -> nm
      in
      let seen = Hashtbl.create 512 in
      let out = ref [] in
      let count = ref 0 in
      let max_items = 500 in
      let add_item ~(uniq_key:string) (item:Yojson.Safe.t) : unit =
        if !count < max_items && not (Hashtbl.mem seen uniq_key) then (
          Hashtbl.replace seen uniq_key true;
          out := item :: !out;
          incr count
        )
      in

      let add_symbol_item (d:def) : unit =
        if starts_with_ci ~prefix d.name then
          let detail =
            match d.container with
            | None -> Some (kind_name d.kind)
            | Some c -> Some (Printf.sprintf "%s in %s" (kind_name d.kind) c)
          in
          let kind = completion_item_kind_of_def_kind d.kind in
          let uniq_key = Printf.sprintf "sym|%s|%d" (normalize_name d.name) kind in
          let sort_text =
            if same_uri d.uri doc.Document.uri then Some ("0_" ^ normalize_name d.name)
            else Some ("1_" ^ normalize_name d.name)
          in
          add_item
            ~uniq_key
            (completion_item_json
               ~label:d.name
               ~kind
               ?detail
               ?sort_text
               ())
      in

      let add_keyword (label:string) (kind:int) (detail:string option) : unit =
        if starts_with_ci ~prefix label then
          let uniq_key = "kw|" ^ normalize_name label in
          add_item
            ~uniq_key
            (completion_item_json
               ~label
               ~kind
               ?detail
               ~sort_text:("2_" ^ normalize_name label)
               ())
      in

      let add_builtin_function (label:string) (kind:int) (detail:string option) : unit =
        if starts_with_ci ~prefix label then
          let uniq_key = "fn|" ^ normalize_name label in
          add_item
            ~uniq_key
            (completion_item_json
               ~label
               ~kind
               ?detail
               ~sort_text:("3_" ^ normalize_name label)
               ())
      in

      let add_snippet (label:string) (insert_text:string) (kind:int) (detail:string option) : unit =
        if starts_with_ci ~prefix label || starts_with_ci ~prefix insert_text then
          let uniq_key = "snip|" ^ normalize_name label in
          add_item
            ~uniq_key
            (completion_item_json
               ~label
               ~kind
               ?detail
               ~insert_text
               ~sort_text:("4_" ^ normalize_name label)
               ())
      in

      docs_for_lookup ws doc
      |> List.concat_map collect_doc_defs
      |> uniq_defs
      |> List.iter add_symbol_item;

      List.iter (fun (label, kind, detail) -> add_keyword label kind detail) completion_keywords;
      List.iter (fun (label, kind, detail) -> add_keyword label kind detail) completion_types_builtin;
      List.iter (fun (label, kind, detail) -> add_builtin_function label kind detail) completion_functions_builtin;
      List.iter (fun (label, insert_text, kind, detail) -> add_snippet label insert_text kind detail) completion_snippets;

      `List (List.rev !out)

let range_intersects (a:T.Range.t) (b:T.Range.t) : bool =
  compare_pos a.start b.end_ <= 0 && compare_pos b.start a.end_ <= 0

let range_pos_json (p:T.Position.t) : Yojson.Safe.t =
  `Assoc [ "line", `Int p.line; "character", `Int p.character ]

let range_zero_json (p:T.Position.t) : Yojson.Safe.t =
  `Assoc [ "start", range_pos_json p; "end", range_pos_json p ]

let workspace_single_edit_json ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) ~(new_text:string) : Yojson.Safe.t =
  let uri_s = Uri_path.docuri_to_string uri in
  `Assoc [
    "changes",
    `Assoc [
      (uri_s,
       `List [
         `Assoc [
           "range", range_zero_json pos;
           "newText", `String new_text;
         ]
       ])
    ];
  ]

let parse_missing_compool_name (msg:string) : string option =
  let prefix = "Missing COMPOOL:" in
  let lp = String.length prefix in
  if String.length msg < lp || String.sub msg 0 lp <> prefix then None
  else
    let name = String.trim (String.sub msg lp (String.length msg - lp)) in
    let k = normalize_name name in
    if k = "" then None else Some k

let find_substring_index ~(haystack:string) ~(needle:string) : int option =
  let hn = String.length haystack in
  let nn = String.length needle in
  if nn = 0 || hn < nn then None
  else
    let rec loop i =
      if i + nn > hn then None
      else if String.sub haystack i nn = needle then Some i
      else loop (i + 1)
    in
    loop 0

let parse_compool_name_from_hint (msg:string) : string option =
  let upper = String.uppercase_ascii msg in
  match find_substring_index ~haystack:upper ~needle:"COMPOOL " with
  | None -> None
  | Some i ->
      let j0 = i + String.length "COMPOOL " in
      let n = String.length upper in
      let rec skip j =
        if j >= n then n
        else
          match upper.[j] with
          | ' ' | '\t' | '\'' | '"' | '`' -> skip (j + 1)
          | _ -> j
      in
      let rec take j =
        if j >= n then j
        else
          match upper.[j] with
          | 'A' .. 'Z' | '0' .. '9' | '_' | '$' -> take (j + 1)
          | _ -> j
      in
      let s = skip j0 in
      let e = take s in
      if e <= s then None
      else
        let name = String.sub upper s (e - s) in
        let k = normalize_name name in
        if k = "" then None else Some k

let import_insert_position (doc:Document.t) : T.Position.t * bool =
  let idx = doc.Document.index in
  let line_count = Text_index.line_count idx in
  let line_max =
    Document.imports doc
    |> List.fold_left (fun acc (imp:Preprocess.import) ->
         let line0 = max 0 (imp.loc.start_pos.line - 1) in
         if line0 > acc then line0 else acc)
         (-1)
  in
  if line_max >= 0 && line_count > 0 then
    let line = min line_max (line_count - 1) in
    let ch =
      match Text_index.line_length idx ~line with
      | Some n -> max 0 n
      | None -> 0
    in
    ((({ line; character = ch } : T.Position.t)), true)
  else
    ((({ line = 0; character = 0 } : T.Position.t)), false)

let has_import_for_compool (doc:Document.t) (name:string) : bool =
  let key = normalize_name name in
  Document.imports doc
  |> List.exists (fun (imp:Preprocess.import) -> normalize_name imp.name = key)

let quickfix_add_import_action
    ~(doc:Document.t)
    ~(diag:T.Diagnostic.t)
    ~(compool:string)
  : Yojson.Safe.t option =
  let key = normalize_name compool in
  if key = "" || has_import_for_compool doc key then None
  else
    let pos, append_after_line = import_insert_position doc in
    let text =
      if append_after_line then Printf.sprintf "\n!COMPOOL(\"%s\");" key
      else Printf.sprintf "!COMPOOL(\"%s\");\n" key
    in
    Some
      (`Assoc [
         "title", `String (Printf.sprintf "Import COMPOOL %s" key);
         "kind", `String "quickfix";
         "isPreferred", `Bool true;
         "diagnostics", `List [ T.Diagnostic.yojson_of_t diag ];
         "edit", workspace_single_edit_json ~uri:doc.uri ~pos ~new_text:text;
       ])

let code_actions_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(range:T.Range.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `List []
  | Some doc ->
      let diag_in_range (d:T.Diagnostic.t) : bool =
        range_intersects d.range range
      in
      let seen = Hashtbl.create 32 in
      let actions = ref [] in
      let add_action (key:string) (action:Yojson.Safe.t option) =
        if not (Hashtbl.mem seen key) then
          match action with
          | None -> ()
          | Some a ->
              Hashtbl.replace seen key true;
              actions := a :: !actions
      in
      Document.diagnostics doc
      |> List.filter diag_in_range
      |> List.iter (fun (d:T.Diagnostic.t) ->
           let msg =
             match d.message with
             | `String s -> s
             | `MarkupContent mc -> mc.value
           in
           let compool_opt =
             match parse_missing_compool_name msg with
             | Some c -> Some c
             | None -> parse_compool_name_from_hint msg
           in
           match compool_opt with
           | None -> ()
           | Some c ->
               add_action
                 ("import|" ^ normalize_name c)
                 (quickfix_add_import_action ~doc ~diag:d ~compool:c));
      `List (List.rev !actions)

let collect_proc_param_map (doc:Document.t) (out:(string, string list) Hashtbl.t) : unit =
  let rec add_expr (e:Ast.expr Ast.node) : unit =
    match e.v with
    | Ast.EName _ | Ast.ELit _ -> ()
    | Ast.EUnop { rhs; _ } -> add_expr rhs
    | Ast.EBinop { lhs; rhs; _ } -> add_expr lhs; add_expr rhs
    | Ast.ECall { args; _ } -> List.iter add_expr args
    | Ast.EIndex { base; index } -> add_expr base; List.iter add_expr index
    | Ast.EField { base; _ } -> add_expr base
    | Ast.EAt { field; ptr } -> add_expr field; add_expr ptr
    | Ast.EDeref { ptr } -> add_expr ptr
    | Ast.EParen x -> add_expr x
  in
  let rec add_stmt (s:Ast.stmt Ast.node) : unit =
    match s.v with
    | Ast.SEmpty | Ast.SGoto _ -> ()
    | Ast.SDecl d -> add_decl d
    | Ast.SBlock xs -> List.iter add_stmt xs
    | Ast.SAssign { lhs; rhs } -> add_expr lhs; add_expr rhs
    | Ast.SCallStmt { args; _ } -> List.iter add_expr args
    | Ast.SIf { cond; then_; else_ } ->
        add_expr cond;
        add_stmt then_;
        (match else_ with None -> () | Some e -> add_stmt e)
    | Ast.SWhile { cond; body } ->
        add_expr cond;
        add_stmt body
    | Ast.SFor { init; cond; step; body } ->
        (match init with None -> () | Some i -> add_stmt i);
        (match cond with None -> () | Some c -> add_expr c);
        (match step with None -> () | Some st -> add_stmt st);
        add_stmt body
    | Ast.SReturn eo ->
        (match eo with None -> () | Some e -> add_expr e)
    | Ast.SLabel { body; _ } -> add_stmt body
  and add_decl (d:Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DVar { init; _ } ->
        (match init with None -> () | Some e -> add_expr e)
    | Ast.DConst { value; _ } ->
        add_expr value
    | Ast.DType _ | Ast.DDirective _ ->
        ()
    | Ast.DProc p ->
        let key = normalize_name p.v.name.v in
        if key <> "" && not (Hashtbl.mem out key) then
          Hashtbl.add out key (List.map (fun prm -> prm.v.pname.v) p.v.params);
        List.iter add_decl p.v.locals;
        add_stmt p.v.body
  in
  match doc.Document.ast with
  | None -> ()
  | Some prog ->
      List.iter (function
        | Ast.TopDecl d -> add_decl d
        | Ast.TopStmt s -> add_stmt s
      ) prog

let inlay_hints_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(range:T.Range.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `List []
  | Some doc ->
      let proc_params : (string, string list) Hashtbl.t = Hashtbl.create 128 in
      docs_for_lookup ws doc |> List.iter (fun d -> collect_proc_param_map d proc_params);
      let seen = Hashtbl.create 256 in
      let hints = ref [] in
      let add_call_hints (callee:Ast.ident) (args:Ast.expr Ast.node list) =
        let key = normalize_name callee.v in
        match Hashtbl.find_opt proc_params key with
        | None -> ()
        | Some params ->
            List.iteri (fun i (arg:Ast.expr Ast.node) ->
              match nth_opt params i with
              | None -> ()
              | Some pname ->
                  let pos = Lsp_conv.position_of_ast_pos arg.loc.start_pos in
                  if pos_in_range pos range then
                    let hk = Printf.sprintf "%d|%d|%s" pos.line pos.character pname in
                    if not (Hashtbl.mem seen hk) then (
                      Hashtbl.add seen hk true;
                      hints :=
                        `Assoc [
                          "position", T.Position.yojson_of_t pos;
                          "label", `String (pname ^ ":");
                          "kind", `Int 2;
                          "paddingRight", `Bool true;
                        ]
                        :: !hints
                    )
            ) args
      in
      let rec walk_expr (e:Ast.expr Ast.node) : unit =
        match e.v with
        | Ast.EName _ | Ast.ELit _ -> ()
        | Ast.EUnop { rhs; _ } -> walk_expr rhs
        | Ast.EBinop { lhs; rhs; _ } -> walk_expr lhs; walk_expr rhs
        | Ast.ECall { callee; args } ->
            add_call_hints callee args;
            List.iter walk_expr args
        | Ast.EIndex { base; index } ->
            walk_expr base;
            List.iter walk_expr index
        | Ast.EField { base; _ } ->
            walk_expr base
        | Ast.EAt { field; ptr } ->
            walk_expr field;
            walk_expr ptr
        | Ast.EDeref { ptr } ->
            walk_expr ptr
        | Ast.EParen x ->
            walk_expr x
      in
      let rec walk_stmt (s:Ast.stmt Ast.node) : unit =
        match s.v with
        | Ast.SEmpty | Ast.SGoto _ -> ()
        | Ast.SDecl d -> walk_decl d
        | Ast.SBlock xs -> List.iter walk_stmt xs
        | Ast.SAssign { lhs; rhs } -> walk_expr lhs; walk_expr rhs
        | Ast.SCallStmt { callee; args } ->
            add_call_hints callee args;
            List.iter walk_expr args
        | Ast.SIf { cond; then_; else_ } ->
            walk_expr cond;
            walk_stmt then_;
            (match else_ with None -> () | Some e -> walk_stmt e)
        | Ast.SWhile { cond; body } ->
            walk_expr cond;
            walk_stmt body
        | Ast.SFor { init; cond; step; body } ->
            (match init with None -> () | Some i -> walk_stmt i);
            (match cond with None -> () | Some c -> walk_expr c);
            (match step with None -> () | Some st -> walk_stmt st);
            walk_stmt body
        | Ast.SReturn eo ->
            (match eo with None -> () | Some e -> walk_expr e)
        | Ast.SLabel { body; _ } ->
            walk_stmt body
      and walk_decl (d:Ast.decl Ast.node) : unit =
        match d.v with
        | Ast.DVar { init; _ } ->
            (match init with None -> () | Some e -> walk_expr e)
        | Ast.DConst { value; _ } ->
            walk_expr value
        | Ast.DType _ | Ast.DDirective _ ->
            ()
        | Ast.DProc p ->
            List.iter walk_decl p.v.locals;
            walk_stmt p.v.body
      in
      (match doc.Document.ast with
       | None -> ()
       | Some prog ->
           List.iter (function
             | Ast.TopDecl d -> walk_decl d
             | Ast.TopStmt s -> walk_stmt s
            ) prog);
      `List (List.rev !hints)

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

let semantic_token_compare (a:semantic_token) (b:semantic_token) : int =
  if a.st_line <> b.st_line then compare a.st_line b.st_line
  else if a.st_start <> b.st_start then compare a.st_start b.st_start
  else if a.st_len <> b.st_len then compare a.st_len b.st_len
  else if a.st_typ <> b.st_typ then compare a.st_typ b.st_typ
  else compare a.st_mods b.st_mods

let semantic_token_key (t:semantic_token) : string =
  Printf.sprintf "%d|%d|%d" t.st_line t.st_start t.st_len

let normalize_range_order (r:T.Range.t) : T.Range.t =
  if compare_pos r.start r.end_ <= 0 then r
  else { T.Range.start = r.end_; end_ = r.start }

let semantic_token_intersects_range (t:semantic_token) (r:T.Range.t) : bool =
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

let semantic_token_of_span ~(line0:int) ~(col0:int) ~(col1:int) ~(typ:int) ~(mods:int)
  : semantic_token option =
  let len = col1 - col0 in
  if line0 < 0 || col0 < 0 || len <= 0 then None
  else Some { st_line = line0; st_start = col0; st_len = len; st_typ = typ; st_mods = mods }

let semantic_token_of_loc (loc:Ast.Loc.t) ~(typ:int) ~(mods:int) : semantic_token option =
  let sp = loc.Ast.Loc.start_pos in
  let ep = loc.Ast.Loc.end_pos in
  if sp.line <> ep.line then None
  else
    semantic_token_of_span
      ~line0:(max 0 (sp.line - 1))
      ~col0:(max 0 sp.col)
      ~col1:(max 0 ep.col)
      ~typ
      ~mods

let loc_same_span (a:Ast.Loc.t) (b:Ast.Loc.t) : bool =
  a.start_pos.line = b.start_pos.line
  && a.start_pos.col = b.start_pos.col
  && a.end_pos.line = b.end_pos.line
  && a.end_pos.col = b.end_pos.col

let semantic_type_of_def_kind (k:int) : int option =
  if k = sym_kind_module then Some sem_tt_namespace
  else if k = sym_kind_type then Some sem_tt_type
  else if k = sym_kind_func then Some sem_tt_function
  else if k = sym_kind_field then Some sem_tt_property
  else if k = sym_kind_var || k = sym_kind_const then Some sem_tt_variable
  else None

let semantic_mods_for_occurrence ~(decl:def) ~(occ_uri:T.DocumentUri.t) ~(occ_loc:Ast.Loc.t) : int =
  let mods =
    if decl.kind = sym_kind_const then sem_mod_readonly else 0
  in
  if same_uri occ_uri decl.uri && loc_same_span occ_loc decl.loc then
    mods lor sem_mod_declaration
  else
    mods

let collect_define_macro_keys (doc:Document.t) : (string, bool) Hashtbl.t =
  let out = Hashtbl.create 32 in
  let add_define_args (args:Ast.ident list) : unit =
    match args with
    | [] -> ()
    | first :: _ ->
        let k = normalize_name first.v in
        if k <> "" then Hashtbl.replace out k true
  in
  let rec walk_decl (d:Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DDirective { name; args } ->
        if normalize_name name.v = "DEFINE" then add_define_args args
    | Ast.DProc p ->
        List.iter walk_decl p.v.locals;
        walk_stmt p.v.body
    | Ast.DVar _ | Ast.DConst _ | Ast.DType _ ->
        ()
  and walk_stmt (s:Ast.stmt Ast.node) : unit =
    match s.v with
    | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _ | Ast.SGoto _ ->
        ()
    | Ast.SDecl d ->
        walk_decl d
    | Ast.SBlock xs ->
        List.iter walk_stmt xs
    | Ast.SIf { then_; else_; _ } ->
        walk_stmt then_;
        (match else_ with None -> () | Some e -> walk_stmt e)
    | Ast.SWhile { body; _ } ->
        walk_stmt body
    | Ast.SFor { init; step; body; _ } ->
        (match init with None -> () | Some i -> walk_stmt i);
        (match step with None -> () | Some st -> walk_stmt st);
        walk_stmt body
    | Ast.SLabel { body; _ } ->
        walk_stmt body
  in
  (match doc.Document.ast with
   | None -> ()
   | Some prog ->
       List.iter (function
         | Ast.TopDecl d -> walk_decl d
         | Ast.TopStmt s -> walk_stmt s
       ) prog);
  out

let semantic_symbol_tokens_for_doc (ws:t) (doc:Document.t) : semantic_token list =
  let nav = build_doc_nav ws doc in
  let out = ref [] in
  Hashtbl.iter (fun sym_id occs ->
    match Hashtbl.find_opt nav.defs_by_id sym_id with
    | None -> ()
    | Some decl ->
        (match semantic_type_of_def_kind decl.kind with
         | None -> ()
         | Some typ ->
             List.iter (fun (occ_uri, occ_loc) ->
               if same_uri occ_uri doc.Document.uri then
                 let mods = semantic_mods_for_occurrence ~decl ~occ_uri ~occ_loc in
                 match semantic_token_of_loc occ_loc ~typ ~mods with
                 | None -> ()
                 | Some tok -> out := tok :: !out
             ) occs)
  ) nav.occs_by_id;
  List.rev !out

let semantic_class_of_lex_token
    ~(macro_keys:(string, bool) Hashtbl.t)
    (tok:Parser.token)
  : (int * int) option =
  match tok with
  | Parser.ID s ->
      let k = normalize_name s in
      if k = "" then None
      else if Hashtbl.mem macro_keys k then Some (sem_tt_macro, 0)
      else if is_builtin_function_name k then Some (sem_tt_function, 0)
      else if is_builtin_type k then Some (sem_tt_type, 0)
      else if is_reserved_keyword k then Some (sem_tt_keyword, 0)
      else Some (sem_tt_variable, 0)

  | Parser.INTLIT _ | Parser.FLOATLIT _ ->
      Some (sem_tt_number, 0)
  | Parser.STRINGLIT _ ->
      Some (sem_tt_string, 0)

  | Parser.START | Parser.TERM | Parser.BEGIN | Parser.END
  | Parser.DEF | Parser.REF | Parser.PROC | Parser.ITEM | Parser.TABLE
  | Parser.STATIC | Parser.CONSTANT
  | Parser.IF | Parser.ELSE | Parser.WHILE | Parser.FOR | Parser.BY | Parser.THEN
  | Parser.CASE | Parser.DEFAULT | Parser.FALLTHRU
  | Parser.EXIT | Parser.GOTO | Parser.RETURN | Parser.ABORT | Parser.STOP
  | Parser.NOT | Parser.AND | Parser.OR | Parser.XOR | Parser.EQV | Parser.MOD
  | Parser.TRUE | Parser.FALSE
  | Parser.PROGRAM | Parser.COMPOOL | Parser.ICOMPOOL | Parser.DEFINE | Parser.TYPE | Parser.BLOCK ->
      Some (sem_tt_keyword, 0)

  | Parser.EQ | Parser.NE | Parser.LT | Parser.LE | Parser.GT | Parser.GE
  | Parser.PLUS | Parser.MINUS | Parser.STAR | Parser.SLASH | Parser.POW
  | Parser.AT | Parser.BANG | Parser.CONV_L | Parser.CONV_R ->
      Some (sem_tt_operator, 0)

  | Parser.LPAREN | Parser.RPAREN | Parser.COMMA | Parser.SEMI | Parser.COLON | Parser.DOT
  | Parser.EOF ->
      None

let semantic_lex_tokens_for_doc
    (doc:Document.t)
    ~(macro_keys:(string, bool) Hashtbl.t)
  : semantic_token list =
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
     | Some (typ, mods) ->
         (match semantic_token_of_span ~line0 ~col0 ~col1 ~typ ~mods with
          | None -> ()
          | Some st -> out := st :: !out));
    match tok with
    | Parser.EOF -> ()
    | _ -> loop ()
  in
  (try loop () with _ -> ());
  List.rev !out

let semantic_tokens_json_of_tokens (tokens:semantic_token list) : Yojson.Safe.t =
  let data_rev = ref [] in
  let prev_line = ref 0 in
  let prev_start = ref 0 in
  let first = ref true in
  List.iter (fun t ->
    let delta_line, delta_start =
      if !first then
        (t.st_line, t.st_start)
      else if t.st_line = !prev_line then
        (0, t.st_start - !prev_start)
      else
        (t.st_line - !prev_line, t.st_start)
    in
    data_rev :=
      t.st_mods :: t.st_typ :: t.st_len :: delta_start :: delta_line :: !data_rev;
    first := false;
    prev_line := t.st_line;
    prev_start := t.st_start
  ) tokens;
  `Assoc [ "data", `List (List.rev_map (fun i -> `Int i) !data_rev) ]

let semantic_tokens_with_result_id (doc:Document.t) (base:Yojson.Safe.t) : Yojson.Safe.t =
  let result_id = Digest.(to_hex (string doc.Document.text)) in
  match base with
  | `Assoc xs -> `Assoc (("resultId", `String result_id) :: xs)
  | _ -> base

let semantic_tokens_for_doc (ws:t) (doc:Document.t) ~(range:T.Range.t option) : semantic_token list =
  let norm_range = Option.map normalize_range_order range in
  let in_range tok =
    match norm_range with
    | None -> true
    | Some r -> semantic_token_intersects_range tok r
  in
  let seen = Hashtbl.create 2048 in
  let out = ref [] in
  let add tok =
    if in_range tok then
      let k = semantic_token_key tok in
      if not (Hashtbl.mem seen k) then (
        Hashtbl.add seen k true;
        out := tok :: !out
      )
  in
  semantic_symbol_tokens_for_doc ws doc |> List.iter add;
  let macro_keys = collect_define_macro_keys doc in
  semantic_lex_tokens_for_doc doc ~macro_keys |> List.iter add;
  List.sort semantic_token_compare (List.rev !out)

let semantic_tokens_full_json_for (ws:t) ~(uri:T.DocumentUri.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Assoc [ "data", `List [] ]
  | Some doc ->
      semantic_tokens_for_doc ws doc ~range:None
      |> semantic_tokens_json_of_tokens
      |> semantic_tokens_with_result_id doc

let semantic_tokens_range_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(range:T.Range.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Assoc [ "data", `List [] ]
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

let rec doc_symbol_json (s:doc_symbol) : Yojson.Safe.t =
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

let document_symbols_from_ast (doc:Document.t) : doc_symbol list =
  let current_compool_key =
    match doc.Document.compool_def with
    | None -> None
    | Some n ->
        let k = normalize_name n in
        if k = "" then None else Some k
  in
  let mk_symbol
      ~(name:string)
      ~(kind:int)
      ~(range:Ast.Loc.t)
      ~(selection:Ast.Loc.t)
      ~(detail:string option)
      ~(children:doc_symbol list)
      : doc_symbol =
    {
      ds_name = name;
      ds_kind = kind;
      ds_range = range;
      ds_selection = selection;
      ds_detail = detail;
      ds_children = children;
    }
  in
  let of_type_fields (t:Ast.type_expr Ast.node) : doc_symbol list =
    match t.v with
    | Ast.TRecord fields ->
        fields
        |> List.map (fun f ->
             let fv = f.v in
             mk_symbol
               ~name:fv.fname.v
               ~kind:sym_kind_field
               ~range:f.loc
               ~selection:fv.fname.loc
               ~detail:(Some (type_expr_to_compact_string fv.ftype))
               ~children:[])
    | _ -> []
  in
  let proc_detail (p:Ast.proc Ast.node) : string =
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
  let rec of_decl (d:Ast.decl Ast.node) : doc_symbol list =
    match d.v with
    | Ast.DVar { name; dtype; _ } ->
        [
          mk_symbol
            ~name:name.v
            ~kind:sym_kind_var
            ~range:d.loc
            ~selection:name.loc
            ~detail:(Some (type_expr_to_compact_string dtype))
            ~children:[]
        ]
    | Ast.DConst { name; dtype; _ } ->
        let detail =
          match dtype with
          | None -> Some "const"
          | Some t -> Some ("const " ^ type_expr_to_compact_string t)
        in
        [
          mk_symbol
            ~name:name.v
            ~kind:sym_kind_const
            ~range:d.loc
            ~selection:name.loc
            ~detail
            ~children:[]
        ]
    | Ast.DType { name; defn } ->
        [
          mk_symbol
            ~name:name.v
            ~kind:sym_kind_type
            ~range:d.loc
            ~selection:name.loc
            ~detail:(Some (type_expr_to_compact_string defn))
            ~children:(of_type_fields defn)
        ]
    | Ast.DProc p ->
        let param_children =
          p.v.params
          |> List.map (fun prm ->
               mk_symbol
                 ~name:prm.v.pname.v
                 ~kind:sym_kind_var
                 ~range:prm.loc
                 ~selection:prm.v.pname.loc
                 ~detail:(Some (type_expr_to_compact_string prm.v.ptype))
                 ~children:[])
        in
        let local_children = p.v.locals |> List.concat_map of_decl in
        [
          mk_symbol
            ~name:p.v.name.v
            ~kind:sym_kind_func
            ~range:p.loc
            ~selection:p.v.name.loc
            ~detail:(Some (proc_detail p))
            ~children:(param_children @ local_children)
        ]
    | Ast.DDirective { name; args } ->
        let key = normalize_name name.v in
        (match key, args with
         | "PROGRAM", first :: _ ->
             [
               mk_symbol
                 ~name:first.v
                 ~kind:sym_kind_module
                 ~range:d.loc
                 ~selection:first.loc
                 ~detail:(Some "PROGRAM")
                 ~children:[]
             ]
         | "PROGRAM", [] ->
             [
               mk_symbol
                 ~name:name.v
                 ~kind:sym_kind_module
                 ~range:d.loc
                 ~selection:name.loc
                 ~detail:None
                 ~children:[]
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
                 mk_symbol
                   ~name:first.v
                   ~kind:sym_kind_module
                   ~range:d.loc
                   ~selection:first.loc
                   ~detail:(Some "COMPOOL")
                   ~children:[]
               ]
         | ("COMPOOL" | "ICOMPOOL"), [] ->
             []
         | "BLOCK", _ ->
             []
         | _ ->
             [])
  in
  match doc.Document.ast with
  | None -> []
  | Some prog ->
      prog
      |> List.concat_map (function
           | Ast.TopDecl d -> of_decl d
           | Ast.TopStmt _ -> [])

let document_symbols_json_for (ws:t) ~(uri:T.DocumentUri.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `List []
  | Some doc ->
      (match document_symbols_from_ast doc with
       | [] ->
           collect_doc_defs doc
           |> uniq_defs
           |> List.map symbol_json
           |> fun xs -> `List xs
       | xs ->
           `List (List.map doc_symbol_json xs))

let debug_report_for (ws:t) ~(uri:T.DocumentUri.t) ~(max_tokens:int) : Yojson.Safe.t =
  match Hashtbl.find_opt ws.docs uri with
  | None -> `Assoc [ "error", `String "document not open" ]
  | Some doc ->
      let root =
        match ws.root_path with
        | None -> `Null
        | Some p -> `String p
      in

      let compools =
        match ws.index with
        | None ->
            `Assoc [ "count", `Int 0; "sources", `Int 0; "complete", `Bool false; "sample", `List [] ]
        | Some idx ->
            let sources = Workspace_index.all_source_paths idx in
            let sample =
              Workspace_index.sample idx 10
              |> List.map (fun (k,v) -> `Assoc [ "name", `String k; "path", `String v ])
            in
            `Assoc [
              "count", `Int (Workspace_index.compool_count idx);
              "sources", `Int (List.length sources);
              "complete", `Bool (Workspace_index.is_complete idx);
              "sample", `List sample;
            ]
      in

      let imports =
        Document.imports doc
        |> List.map (fun (imp:Preprocess.import) ->
             let resolved =
               match ws.index with
               | None -> None
               | Some idx -> Workspace_index.find_compool idx ~name:imp.name
             in
             `Assoc [
               "name", `String imp.name;
               "resolved", (match resolved with None -> `Null | Some p -> `String p);
               "loc",
               `Assoc [
                 "line", `Int imp.loc.start_pos.line;
                 "col", `Int imp.loc.start_pos.col;
               ];
             ])
      in

      let tokens =
        let text = Document.text doc in
        let lexbuf = Lexing.from_string text in
        let rec loop i acc =
          if i >= max_tokens then List.rev acc
          else
            let t = Lexer.token lexbuf in
            let sp = Lexing.lexeme_start_p lexbuf in
            let ep = Lexing.lexeme_end_p lexbuf in
            let lex = Lexing.lexeme lexbuf in
            let item =
              `Assoc [
                "token", `String (Parse.Debug.string_of_token t);
                "lexeme", `String lex;
                "range", lsp_range_of_lex sp ep;
              ]
            in
            match t with
            | Parser.EOF -> List.rev (item :: acc)
            | _ -> loop (i+1) (item :: acc)
        in
        loop 0 []
      in

      `Assoc [
        "uri", `String (Uri_path.docuri_to_string uri);
        "root", root;
        "compools", compools;
        "imports", `List imports;
        "tokens", `List tokens;
      ]
