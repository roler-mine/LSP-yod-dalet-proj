module T = Lsp.Types
open Ast
open Workspace_foundation
open Workspace_state
open Workspace_index_graph
open Workspace_imports
open Workspace_tuning

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

type sem_value = SVVar of sem_ty | SVConst of sem_ty | SVProc of sem_proc_sig

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

let diag_semantic (loc : Ast.Loc.t) (msg : string) : T.Diagnostic.t =
  Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"semantic"
    ~message:msg loc

let doc_zero_loc (doc : Document.t) : Ast.Loc.t =
  let z = { Ast.Loc.line = 1; col = 0; offset = 0 } in
  Ast.Loc.make ~file:doc.Document.file ~start_pos:z ~end_pos:z

let diag_internal_phase_failure ~(phase : string) (doc : Document.t) (exn : exn)
    : T.Diagnostic.t =
  let msg =
    Printf.sprintf
      "Internal %s failure: %s. Showing partial diagnostics from completed \
       phases."
      phase (Printexc.to_string exn)
  in
  Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"internal"
    ~message:msg (doc_zero_loc doc)

let with_internal_phase_diag (doc : Document.t) ~(phase : string) ~(exn : exn) :
    Document.t =
  let d = diag_internal_phase_failure ~phase doc exn in
  Document.with_import_diags (doc.Document.import_diags @ [ d ]) doc

let copy_tbl (tbl : ('k, 'v) Hashtbl.t) : ('k, 'v) Hashtbl.t =
  let out = Hashtbl.create (max 16 (Hashtbl.length tbl * 2)) in
  Hashtbl.iter (fun k v -> Hashtbl.replace out k v) tbl;
  out

let sem_scope_copy (s : sem_scope) : sem_scope =
  { values = copy_tbl s.values; types = copy_tbl s.types }

let sem_scope_empty () : sem_scope =
  { values = Hashtbl.create 64; types = Hashtbl.create 64 }

let sem_add_value ?(overwrite = true) (s : sem_scope) (name : string)
    (v : sem_value) : unit =
  let k = normalize_name name in
  if k <> "" && (overwrite || not (Hashtbl.mem s.values k)) then
    Hashtbl.replace s.values k v

let sem_add_type ?(overwrite = true) (s : sem_scope) (name : string)
    (t : Ast.type_expr Ast.node) : unit =
  let k = normalize_name name in
  if k <> "" && (overwrite || not (Hashtbl.mem s.types k)) then
    Hashtbl.replace s.types k t

let sem_find_record_field (fields : (string * sem_ty) list) (name : string) :
    sem_ty option =
  let key = normalize_name name in
  fields
  |> List.find_opt (fun (nm, _) -> normalize_name nm = key)
  |> Option.map snd

let sem_is_builtin_type (k : string) : bool =
  match k with
  | "A" | "B" | "U" | "S" | "F" | "C" | "P" | "W" | "V" | "STATUS" -> true
  | _ -> false

let is_single_letter_loop_control (name : string) : bool =
  String.length name = 1
  && match name.[0] with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

let rec sem_ty_of_type_expr ?(seen : string list = [])
    (types : (string, Ast.type_expr Ast.node) Hashtbl.t)
    (t : Ast.type_expr Ast.node) : sem_ty =
  match t.v with
  | Ast.TName id -> (
      let k = normalize_name id.v in
      match k with
      | "B" -> TyBit
      | "U" | "S" | "W" -> TyInt
      | "F" | "A" -> TyFloat
      | "C" -> TyChar
      | "P" -> TyPointer None
      | "STATUS" | "V" -> TyStatus
      | "" -> TyUnknown
      | _ -> (
          if List.mem k seen || sem_is_builtin_type k then TyUnknown
          else
            match Hashtbl.find_opt types k with
            | None -> TyUnknown
            | Some defn -> sem_ty_of_type_expr ~seen:(k :: seen) types defn))
  | Ast.TArray { elem; _ } -> TyArray (sem_ty_of_type_expr ~seen types elem)
  | Ast.TPointer inner ->
      TyPointer (Some (sem_ty_of_type_expr ~seen types inner))
  | Ast.TRecord fields ->
      TyRecord
        (fields
        |> List.map (fun f ->
            let fv = f.v in
            (fv.fname.v, sem_ty_of_type_expr ~seen types fv.ftype)))
  | Ast.TFunc _ -> TyUnknown

let sem_proc_sig_of_proc (types : (string, Ast.type_expr Ast.node) Hashtbl.t)
    (p : Ast.proc Ast.node) : sem_proc_sig =
  let local_var_tys : (string, sem_ty) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (fun d ->
      match d.v with
      | Ast.DVar { name; dtype; _ } ->
          Hashtbl.replace local_var_tys (normalize_name name.v)
            (sem_ty_of_type_expr types dtype)
      | _ -> ())
    p.v.locals;
  let param_tys =
    if p.v.params = [] then Some []
    else
      Some
        (p.v.params
        |> List.map (fun prm ->
            let pn = normalize_name prm.v.pname.v in
            let direct = sem_ty_of_type_expr types prm.v.ptype in
            match direct with
            | TyUnknown -> (
                match Hashtbl.find_opt local_var_tys pn with
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

let block_proc_names_of_program (prog : Ast.program) : (string, bool) Hashtbl.t
    =
  let out = Hashtbl.create 32 in
  List.iter
    (function
      | Ast.TopDecl d -> (
          match d.v with
          | Ast.DDirective { name; args = nm :: _ }
            when normalize_name name.v = "BLOCK" ->
              let k = normalize_name nm.v in
              if k <> "" then Hashtbl.replace out k true
          | _ -> ())
      | Ast.TopStmt _ -> ())
    prog;
  out

let sem_exports_of_program (prog : Ast.program) : sem_exports =
  let out = sem_scope_empty () in
  let block_names = block_proc_names_of_program prog in
  let is_block_proc (p : Ast.proc Ast.node) : bool =
    Hashtbl.mem block_names (normalize_name p.v.name.v)
  in
  let rec collect_types_decl ~(in_block : bool) (d : Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DType { name; defn } -> sem_add_type out name.v defn
    | Ast.DProc p ->
        if in_block || is_block_proc p then
          List.iter (collect_types_decl ~in_block:true) p.v.locals
    | Ast.DVar _ | Ast.DConst _ | Ast.DDirective _ -> ()
  in
  let rec collect_values_decl ~(in_block : bool) (d : Ast.decl Ast.node) : unit
      =
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
    | Ast.DType _ -> ()
    | Ast.DProc p ->
        if not in_block then
          sem_add_value out p.v.name.v
            (SVProc (sem_proc_sig_of_proc out.types p));
        if in_block || is_block_proc p then
          List.iter (collect_values_decl ~in_block:true) p.v.locals
    | Ast.DDirective _ -> ()
  in
  List.iter
    (function
      | Ast.TopDecl d -> collect_types_decl ~in_block:false d
      | Ast.TopStmt _ -> ())
    prog;
  List.iter
    (function
      | Ast.TopDecl d -> collect_values_decl ~in_block:false d
      | Ast.TopStmt _ -> ())
    prog;
  out

let sem_exports_of_doc (doc : Document.t) : sem_exports =
  match doc.Document.ast with
  | None -> sem_scope_empty ()
  | Some prog -> sem_exports_of_program prog

let add_compool_hint (tbl : (string, string list) Hashtbl.t)
    ~(symbol_key : string) ~(compool_key : string) : unit =
  let key = normalize_name symbol_key in
  let compool = normalize_name compool_key in
  if key <> "" && compool <> "" then
    let prev =
      match Hashtbl.find_opt tbl key with None -> [] | Some xs -> xs
    in
    if not (List.mem compool prev) then Hashtbl.replace tbl key (compool :: prev)

let symbol_hint_max_file_count = 1500
let symbol_hint_max_chars = 20_000_000

let build_symbol_hint_index (ws : t) :
    (string, string list) Hashtbl.t * (string, string list) Hashtbl.t =
  let values = Hashtbl.create 1024 in
  let types = Hashtbl.create 1024 in
  let seen_paths = Hashtbl.create 512 in
  let parsed_files = ref 0 in
  let parsed_chars = ref 0 in
  let network_root = is_network_root ws in

  let hint_compool_key_of_doc (doc : Document.t) : string option =
    match doc.Document.compool_def with
    | Some compool ->
        let k = normalize_name compool in
        if k = "" then None else Some k
    | None -> (
        match doc.Document.file with
        | None -> None
        | Some path -> (
            match source_stem_of_filename (Filename.basename path) with
            | None -> None
            | Some stem ->
                let k = normalize_name stem in
                if k = "" then None else Some k))
  in

  let add_doc_hints (doc : Document.t) : unit =
    match hint_compool_key_of_doc doc with
    | None -> ()
    | Some compool ->
        let exp = sem_exports_of_doc doc in
        Hashtbl.iter
          (fun sym v ->
            match v with
            | SVProc _ -> ()
            | _ -> add_compool_hint values ~symbol_key:sym ~compool_key:compool)
          exp.values;
        Hashtbl.iter
          (fun sym _ ->
            add_compool_hint types ~symbol_key:sym ~compool_key:compool)
          exp.types
  in

  let add_path_hints (p : string) : unit =
    let pk = normalize_path_key p in
    if
      (not (Hashtbl.mem seen_paths pk))
      && !parsed_files < symbol_hint_max_file_count
      && !parsed_chars < symbol_hint_max_chars
    then (
      Hashtbl.replace seen_paths pk true;
      match doc_from_path_cached_only ws p with
      | None -> enqueue_bg_path ws ~high:false p
      | Some d ->
          let txt_len = String.length d.Document.text in
          if !parsed_chars + txt_len <= symbol_hint_max_chars then (
            incr parsed_files;
            parsed_chars := !parsed_chars + txt_len;
            add_doc_hints d))
  in

  Hashtbl.iter
    (fun _ doc ->
      (match doc.Document.file with
      | None -> ()
      | Some p -> Hashtbl.replace seen_paths (normalize_path_key p) true);
      add_doc_hints doc)
    ws.docs;

  Hashtbl.iter
    (fun _ doc ->
      match doc.Document.file with
      | None -> ()
      | Some p ->
          let pk = normalize_path_key p in
          if not (Hashtbl.mem seen_paths pk) then (
            Hashtbl.replace seen_paths pk true;
            add_doc_hints doc))
    ws.files;

  (match ws.index with
  | None -> ()
  | Some idx ->
      if not network_root then
        Workspace_index.all_paths idx |> List.iter add_path_hints);

  (values, types)

let symbol_hint_index (ws : t) :
    (string, string list) Hashtbl.t * (string, string list) Hashtbl.t =
  pump_index_lookup ws;
  match ws.symbol_hints with
  | Some idx -> idx
  | None ->
      let idx = build_symbol_hint_index ws in
      ws.symbol_hints <- Some idx;
      enqueue_all_open_diag_revalidate ws ~reason:"hint_ready";
      idx

let sem_import_dirs (doc : Document.t) : compool_import_dir list =
  let ast_dirs = extract_compool_import_dirs doc in
  let ast_compools : (string, bool) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun d -> Hashtbl.replace ast_compools (normalize_name d.compool) true)
    ast_dirs;
  let pre_dirs =
    Document.imports doc
    |> List.filter_map (fun (imp : Preprocess.import) ->
        match imp.kind with
        | Preprocess.Compool ->
            let k = normalize_name imp.name in
            if k = "" || Hashtbl.mem ast_compools k then None
            else Some { compool = k; selected = [] })
  in
  ast_dirs @ pre_dirs

let sem_ty_of_literal (lit : Ast.literal) : sem_ty =
  match lit with
  | Ast.LInt s ->
      let u = normalize_name s in
      if String.length u >= 3 && String.contains u '\'' && String.contains u 'B'
      then TyBit
      else TyInt
  | Ast.LFloat _ -> TyFloat
  | Ast.LString s -> if String.length s = 1 then TyChar else TyString
  | Ast.LChar _ -> TyChar
  | Ast.LBool _ -> TyBit

let sem_ty_to_string (t : sem_ty) : string =
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
  | TyUnknown | TyInt | TyFloat | TyBit | TyChar | TyString | TyStatus
  | TyPointer _ ->
      true
  | TyArray inner -> sem_is_primitive inner
  | TyRecord _ -> false

let rec sem_scalarize = function
  | TyArray inner when sem_is_primitive inner -> sem_scalarize inner
  | t -> t

let rec sem_compatible (lhs : sem_ty) (rhs : sem_ty) : bool =
  let lhs = sem_scalarize lhs in
  let rhs = sem_scalarize rhs in
  match (lhs, rhs) with
  | TyUnknown, _ | _, TyUnknown -> true
  | TyInt, TyInt
  | TyFloat, TyFloat
  | TyBit, TyBit
  | TyChar, TyChar
  | TyString, TyString
  | TyStatus, TyStatus
  | TyPointer _, TyPointer _ ->
      true
  | TyInt, TyFloat | TyFloat, TyInt -> true
  | TyRecord _, TyRecord _ -> true
  | TyArray a, TyArray b -> sem_compatible a b
  | _ -> false

let validate_semantics (ws : t) (doc : Document.t) : T.Diagnostic.t list =
  match doc.Document.ast with
  | None -> []
  | Some prog ->
      let seen = Hashtbl.create 128 in
      let out = ref [] in
      let emit (loc : Ast.Loc.t) (msg : string) =
        let k =
          Printf.sprintf "%s|%d|%d|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line loc.start_pos.col msg
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := diag_semantic loc msg :: !out)
      in
      let emit_import_hint (loc : Ast.Loc.t) ~(kind : string) ~(symbol : string)
          ~(compools : string list) =
        let k =
          Printf.sprintf "%s|%d|%d|import|%s|%s|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line loc.start_pos.col kind symbol
            (String.concat "," compools)
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := diag_missing_import_hint ~loc ~kind ~symbol ~compools :: !out)
      in

      let import_dirs = sem_import_dirs doc in
      let doc_cache : (string, Document.t option) Hashtbl.t =
        Hashtbl.create 16
      in
      let exports_cache : (string, sem_exports option) Hashtbl.t =
        Hashtbl.create 16
      in
      let imported_compools : (string, [ `All | `Selected ]) Hashtbl.t =
        Hashtbl.create 32
      in
      let mark_import_mode (compool : string) (mode : [ `All | `Selected ]) =
        let k = normalize_name compool in
        if k <> "" then
          match (Hashtbl.find_opt imported_compools k, mode) with
          | Some `All, _ -> ()
          | _, `All -> Hashtbl.replace imported_compools k `All
          | Some `Selected, `Selected -> ()
          | None, `Selected -> Hashtbl.replace imported_compools k `Selected
      in
      List.iter
        (fun imp ->
          let mode = if imp.selected = [] then `All else `Selected in
          mark_import_mode imp.compool mode)
        import_dirs;
      (match doc.Document.compool_def with
      | None -> ()
      | Some c -> mark_import_mode c `All);

      let hint_tables :
          ((string, string list) Hashtbl.t * (string, string list) Hashtbl.t)
          option
          ref =
        ref ws.symbol_hints
      in
      let hint_compools_for ~(is_type : bool) (name : string) : string list =
        let key = normalize_name name in
        if key = "" then []
        else
          match !hint_tables with
          | None -> []
          | Some (hint_values, hint_types) -> (
              let tbl = if is_type then hint_types else hint_values in
              match Hashtbl.find_opt tbl key with
              | None -> []
              | Some xs ->
                  xs
                  |> List.filter (fun c ->
                      match
                        Hashtbl.find_opt imported_compools (normalize_name c)
                      with
                      | Some `All -> false
                      | Some `Selected | None -> true)
                  |> List.sort_uniq String.compare
                  |> fun ys ->
                  let rec take n acc = function
                    | [] -> List.rev acc
                    | _ when n <= 0 -> List.rev acc
                    | x :: tl -> take (n - 1) (x :: acc) tl
                  in
                  take 3 [] ys)
      in
      let suggest_missing_import ~(loc : Ast.Loc.t) ~(kind : string)
          ~(is_type : bool) ~(symbol : string) : unit =
        let compools = hint_compools_for ~is_type symbol in
        if compools <> [] then emit_import_hint loc ~kind ~symbol ~compools
      in

      let has_import_hint ~(is_type : bool) (symbol : string) : bool =
        hint_compools_for ~is_type symbol <> []
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
      let get_exports_for_compool (name : string) : sem_exports option =
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

      let should_suppress_cross_module_unresolved ~(is_type : bool)
          ~(name : string) : bool =
        if not warmup_suppress_crossmodule_unresolved then false
        else if ws.startup_diag_hover_ready_ms <> None then false
        else if import_dirs = [] then false
        else
          let key = normalize_name name in
          if key = "" then false
          else
            let qualified_import_match =
              match String.index_opt key '\'' with
              | None -> false
              | Some i ->
                  if i <= 0 || i + 1 >= String.length key then false
                  else
                    let compool =
                      String.sub key (i + 1) (String.length key - i - 1)
                    in
                    compool <> ""
                    && List.exists
                         (fun imp -> imp.compool = compool)
                         import_dirs
            in
            if qualified_import_match then (
              Perf_stats.tick "diag.xmodule_suppressed";
              Perf_stats.tick "diag.warmup_suppressed";
              true)
            else
              let maybe_external =
                List.exists
                  (fun imp ->
                    let selected_match =
                      List.exists (fun (nm, _loc) -> nm = key) imp.selected
                    in
                    match get_exports_for_compool imp.compool with
                    | None -> true
                    | Some exp ->
                        let exported =
                          if is_type then Hashtbl.mem exp.types key
                          else Hashtbl.mem exp.values key
                        in
                        if imp.selected = [] then exported else selected_match)
                  import_dirs
              in
              if maybe_external then (
                Perf_stats.tick "diag.xmodule_suppressed";
                Perf_stats.tick "diag.warmup_suppressed";
                true)
              else false
      in

      let scope = sem_scope_copy (sem_exports_of_program prog) in
      List.iter
        (fun imp ->
          match get_exports_for_compool imp.compool with
          | None -> ()
          | Some exp ->
              if imp.selected = [] then (
                Hashtbl.iter
                  (fun k v -> sem_add_value ~overwrite:false scope k v)
                  exp.values;
                Hashtbl.iter
                  (fun k v -> sem_add_type ~overwrite:false scope k v)
                  exp.types)
              else (
                List.iter
                  (fun (nm, _loc) ->
                    match Hashtbl.find_opt exp.values nm with
                    | Some v -> sem_add_value ~overwrite:false scope nm v
                    | None -> ())
                  imp.selected;
                List.iter
                  (fun (nm, _loc) ->
                    match Hashtbl.find_opt exp.types nm with
                    | Some t -> sem_add_type ~overwrite:false scope nm t
                    | None -> ())
                  imp.selected))
        import_dirs;

      let sem_lookup_value (scp : sem_scope) (name : string) : sem_value option
          =
        Hashtbl.find_opt scp.values (normalize_name name)
      in

      let sem_is_builtin_call (name : string) : bool =
        let k = normalize_name name in
        k = "__CONV__" || k = "__PRESET__" || k = "__POW__" || k = "__RANGE__"
        || is_builtin_function_name k
      in

      let rec check_type_import_hints (scp : sem_scope)
          (t : Ast.type_expr Ast.node) : unit =
        match t.v with
        | Ast.TName id ->
            let k = normalize_name id.v in
            if
              k <> ""
              && (not (sem_is_builtin_type k))
              && not (Hashtbl.mem scp.types k)
            then
              suggest_missing_import ~loc:id.loc ~kind:"Type" ~is_type:true
                ~symbol:id.v
        | Ast.TPointer inner -> check_type_import_hints scp inner
        | Ast.TArray { elem; _ } -> check_type_import_hints scp elem
        | Ast.TRecord fields ->
            List.iter (fun f -> check_type_import_hints scp f.v.ftype) fields
        | Ast.TFunc { params; returns } -> (
            List.iter (fun p -> check_type_import_hints scp p.v.ptype) params;
            match returns with
            | None -> ()
            | Some r -> check_type_import_hints scp r)
      in

      let rec sem_subscript_array (ty : sem_ty) (count : int) : sem_ty =
        if count <= 0 then ty
        else
          match ty with
          | TyArray elem -> sem_subscript_array elem (count - 1)
          | _ -> TyUnknown
      in

      let sem_subscript_pointer (ty : sem_ty) (count : int) : sem_ty =
        match ty with
        | TyPointer None -> TyPointer None
        | TyPointer (Some target) ->
            let inner = sem_subscript_array target count in
            if inner = TyUnknown then TyUnknown else TyPointer (Some inner)
        | _ -> TyUnknown
      in

      let sem_subscript_value (ty : sem_ty) (count : int) : sem_ty =
        match ty with
        | TyArray _ -> sem_subscript_array ty count
        | TyPointer _ -> sem_subscript_pointer ty count
        | _ -> TyUnknown
      in

      let sem_deref_target ~(ptr_loc : Ast.Loc.t) (ty : sem_ty) : sem_ty =
        match ty with
        | TyPointer (Some inner) -> inner
        | TyPointer None ->
            emit ptr_loc "Dereference requires a typed pointer.";
            TyUnknown
        | other ->
            (* Keep existing table-qualified behavior for compatibility. *)
            other
      in

      let sem_field_ty_in (field_name : string) (ty : sem_ty) : sem_ty option =
        match ty with
        | TyRecord fields -> sem_find_record_field fields field_name
        | TyArray (TyRecord fields) -> sem_find_record_field fields field_name
        | TyArray inner -> (
            match inner with
            | TyRecord fields -> sem_find_record_field fields field_name
            | _ -> None)
        | _ -> None
      in

      let rec ty_of_expr (scp : sem_scope) (current_proc : sem_proc_ctx option)
          ?(status_atom = false) (e : Ast.expr Ast.node) : sem_ty =
        match e.v with
        | Ast.ELit lit -> sem_ty_of_literal lit
        | Ast.EName id -> (
            if status_atom then TyStatus
            else
              match sem_lookup_value scp id.v with
              | Some (SVVar ty) | Some (SVConst ty) -> ty
              | Some (SVProc _) -> (
                  match current_proc with
                  | Some cp when normalize_name id.v = cp.proc_key -> (
                      match cp.proc_ret_ty with
                      | Some ty -> ty
                      | None ->
                          emit id.loc
                            (Printf.sprintf
                               "%S is a procedure and cannot be used as a \
                                value."
                               id.v);
                          TyUnknown)
                  | _ ->
                      emit id.loc
                        (Printf.sprintf
                           "%S is a procedure and cannot be used as a value."
                           id.v);
                      TyUnknown)
              | None ->
                  if
                    should_suppress_cross_module_unresolved ~is_type:false
                      ~name:id.v
                  then ()
                  else if has_import_hint ~is_type:false id.v then
                    suggest_missing_import ~loc:id.loc ~kind:"Identifier"
                      ~is_type:false ~symbol:id.v
                  else
                    emit id.loc (Printf.sprintf "Undefined identifier %S." id.v);
                  TyUnknown)
        | Ast.EUnop { rhs; _ } -> ty_of_expr scp current_proc rhs
        | Ast.EBinop { lhs; rhs; _ } ->
            let l = ty_of_expr scp current_proc lhs in
            let r = ty_of_expr scp current_proc rhs in
            if l = TyFloat || r = TyFloat then TyFloat else l
        | Ast.ECall { callee; args } -> (
            let ck = normalize_name callee.v in
            if ck = "V" then (
              List.iter
                (fun a ->
                  ignore (ty_of_expr scp current_proc ~status_atom:true a))
                args;
              TyStatus)
            else if sem_is_builtin_call callee.v then (
              match (ck, args) with
              | "LOC", a0 :: rest ->
                  let target_ty = ty_of_expr scp current_proc a0 in
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    rest;
                  TyPointer (Some target_ty)
              | "LOC", [] -> TyPointer None
              | "NEXT", p0 :: rest -> (
                  let pty = ty_of_expr scp current_proc p0 in
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    rest;
                  match pty with TyPointer _ -> pty | _ -> TyUnknown)
              | _ ->
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyUnknown)
            else
              match sem_lookup_value scp callee.v with
              | Some (SVProc sig_) -> (
                  (match current_proc with
                  | Some cp when normalize_name callee.v = cp.proc_key -> (
                      match sig_.use_attr with
                      | Ast.UseRec -> ()
                      | Ast.UseRent ->
                          emit callee.loc
                            (Printf.sprintf
                               "Recursive call to %S requires REC (RENT alone \
                                is not enough)."
                               cp.proc_name)
                      | Ast.UseNormal ->
                          emit callee.loc
                            (Printf.sprintf "Recursive call to %S requires REC."
                               cp.proc_name))
                  | _ -> ());
                  (match sig_.param_tys with
                  | None -> ()
                  | Some pts ->
                      let rec check_pairs ps xs =
                        match (ps, xs) with
                        | pty :: pst, arg :: xst ->
                            let aty = ty_of_expr scp current_proc arg in
                            if not (sem_compatible pty aty) then
                              emit arg.loc
                                (Printf.sprintf
                                   "Argument type mismatch in call to %S: \
                                    expected %s, got %s."
                                   callee.v (sem_ty_to_string pty)
                                   (sem_ty_to_string aty));
                            check_pairs pst xst
                        | _, [] -> ()
                        | [], _ :: xst ->
                            (* Extra args: parse them, but avoid noisy count errors for now. *)
                            List.iter
                              (fun a -> ignore (ty_of_expr scp current_proc a))
                              xst
                      in
                      check_pairs pts args);
                  match sig_.ret_ty with Some rt -> rt | None -> TyUnknown)
              | Some (SVVar ty) | Some (SVConst ty) ->
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  if args = [] then (
                    emit callee.loc
                      (Printf.sprintf "%S is not callable." callee.v);
                    TyUnknown)
                  else
                    let out_ty = sem_subscript_value ty (List.length args) in
                    if out_ty = TyUnknown then (
                      emit callee.loc
                        (Printf.sprintf "Cannot subscript %S." callee.v);
                      TyUnknown)
                    else out_ty
              | None ->
                  if
                    not
                      (should_suppress_cross_module_unresolved ~is_type:false
                         ~name:callee.v)
                  then
                    emit callee.loc
                      (Printf.sprintf
                         "Undefined procedure %S. Declare it with REF PROC %S \
                          in scope."
                         callee.v callee.v);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyUnknown)
        | Ast.EIndex { base; index } ->
            let bt = ty_of_expr scp current_proc base in
            List.iter (fun i -> ignore (ty_of_expr scp current_proc i)) index;
            sem_subscript_value bt (List.length index)
        | Ast.EField { base; field } -> (
            let bt = ty_of_expr scp current_proc base in
            match bt with
            | TyRecord fields -> (
                match sem_find_record_field fields field.v with
                | Some t -> t
                | None ->
                    emit field.loc (Printf.sprintf "Unknown field %S." field.v);
                    TyUnknown)
            | TyArray (TyRecord fields) -> (
                match sem_find_record_field fields field.v with
                | Some t -> t
                | None ->
                    emit field.loc (Printf.sprintf "Unknown field %S." field.v);
                    TyUnknown)
            | _ -> TyUnknown)
        | Ast.EAt { field; ptr } -> (
            let pt = ty_of_expr scp current_proc ptr in
            let target_ty = sem_deref_target ~ptr_loc:ptr.loc pt in
            let field_ref =
              match field.v with
              | Ast.EName id -> Some (id, [])
              | Ast.EIndex { base; index } -> (
                  match base.v with
                  | Ast.EName id -> Some (id, index)
                  | _ -> None)
              | _ ->
                  ignore (ty_of_expr scp current_proc field);
                  None
            in
            match field_ref with
            | None -> TyUnknown
            | Some (id, indexes) -> (
                List.iter
                  (fun i -> ignore (ty_of_expr scp current_proc i))
                  indexes;
                let qualified_target =
                  sem_subscript_array target_ty (List.length indexes)
                in
                match sem_field_ty_in id.v qualified_target with
                | Some ty -> ty
                | None ->
                    emit id.loc
                      (Printf.sprintf "Unknown field %S for @ access." id.v);
                    TyUnknown))
        | Ast.EDeref { ptr } ->
            let pt = ty_of_expr scp current_proc ptr in
            sem_deref_target ~ptr_loc:ptr.loc pt
        | Ast.EParen inner -> ty_of_expr scp current_proc inner
      in

      let ty_of_lvalue (scp : sem_scope) (current_proc : sem_proc_ctx option)
          (e : Ast.expr Ast.node) : sem_ty option =
        match e.v with
        | Ast.EName id -> (
            match sem_lookup_value scp id.v with
            | Some (SVVar ty) | Some (SVConst ty) -> Some ty
            | Some (SVProc _) -> (
                match current_proc with
                | Some cp when normalize_name id.v = cp.proc_key -> (
                    match cp.proc_ret_ty with
                    | Some ty -> Some ty
                    | None ->
                        emit id.loc
                          (Printf.sprintf "Cannot assign to procedure %S." id.v);
                        None)
                | _ ->
                    emit id.loc
                      (Printf.sprintf "Cannot assign to procedure %S." id.v);
                    None)
            | None ->
                if
                  should_suppress_cross_module_unresolved ~is_type:false
                    ~name:id.v
                then ()
                else if has_import_hint ~is_type:false id.v then
                  suggest_missing_import ~loc:id.loc ~kind:"Item" ~is_type:false
                    ~symbol:id.v
                else emit id.loc (Printf.sprintf "Undefined item %S." id.v);
                None)
        | Ast.EField _ | Ast.EAt _ | Ast.EDeref _ | Ast.EIndex _ ->
            Some (ty_of_expr scp current_proc e)
        | _ ->
            ignore (ty_of_expr scp current_proc e);
            None
      in

      let add_decl_symbol (scp : sem_scope) (d : Ast.decl Ast.node) : unit =
        match d.v with
        | Ast.DType { name; defn } -> sem_add_type scp name.v defn
        | Ast.DVar { name; dtype; _ } ->
            sem_add_value scp name.v
              (SVVar (sem_ty_of_type_expr scp.types dtype))
        | Ast.DConst { name; dtype; _ } ->
            let ty =
              match dtype with
              | None -> TyUnknown
              | Some t -> sem_ty_of_type_expr scp.types t
            in
            sem_add_value scp name.v (SVConst ty)
        | Ast.DProc p ->
            sem_add_value scp p.v.name.v
              (SVProc (sem_proc_sig_of_proc scp.types p))
        | Ast.DDirective _ -> ()
      in

      let rec collect_label_depths_for_stmt (out : (string, int) Hashtbl.t)
          ~(loop_depth : int) (s : Ast.stmt Ast.node) : unit =
        match s.v with
        | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _
        | Ast.SGoto _ ->
            ()
        | Ast.SDecl _ -> ()
        | Ast.SBlock xs ->
            List.iter (collect_label_depths_for_stmt out ~loop_depth) xs
        | Ast.SIf { then_; else_; _ } -> (
            collect_label_depths_for_stmt out ~loop_depth then_;
            match else_ with
            | None -> ()
            | Some e -> collect_label_depths_for_stmt out ~loop_depth e)
        | Ast.SWhile { body; _ } ->
            collect_label_depths_for_stmt out ~loop_depth:(loop_depth + 1) body
        | Ast.SFor { init; step; body; _ } ->
            (match init with
            | None -> ()
            | Some i -> collect_label_depths_for_stmt out ~loop_depth i);
            (match step with
            | None -> ()
            | Some st -> collect_label_depths_for_stmt out ~loop_depth st);
            collect_label_depths_for_stmt out ~loop_depth:(loop_depth + 1) body
        | Ast.SLabel { label; body } ->
            let key = normalize_name label.v in
            if key <> "" && not (Hashtbl.mem out key) then
              Hashtbl.add out key loop_depth;
            collect_label_depths_for_stmt out ~loop_depth body
      in

      let collect_label_depths (s : Ast.stmt Ast.node) : (string, int) Hashtbl.t
          =
        let out = Hashtbl.create 64 in
        collect_label_depths_for_stmt out ~loop_depth:0 s;
        out
      in

      let top_level_label_depths : (string, int) Hashtbl.t =
        let out = Hashtbl.create 64 in
        List.iter
          (function
            | Ast.TopStmt s -> collect_label_depths_for_stmt out ~loop_depth:0 s
            | Ast.TopDecl _ -> ())
          prog;
        out
      in

      let rec check_stmt (scp : sem_scope) (current_proc : sem_proc_ctx option)
          ~(loop_depth : int) ~(label_depths : (string, int) Hashtbl.t)
          (s : Ast.stmt Ast.node) : unit =
        match s.v with
        | Ast.SEmpty -> ()
        | Ast.SDecl d ->
            add_decl_symbol scp d;
            check_decl scp current_proc ~loop_depth ~label_depths d
        | Ast.SBlock xs ->
            List.iter
              (fun st ->
                check_stmt scp current_proc ~loop_depth ~label_depths st)
              xs
        | Ast.SAssign { lhs; rhs } -> (
            let lhs_ty = ty_of_lvalue scp current_proc lhs in
            let rhs_ty = ty_of_expr scp current_proc rhs in
            match lhs_ty with
            | None -> ()
            | Some lt ->
                if not (sem_compatible lt rhs_ty) then
                  emit rhs.loc
                    (Printf.sprintf
                       "Type mismatch in assignment: left is %s, right is %s."
                       (sem_ty_to_string lt) (sem_ty_to_string rhs_ty)))
        | Ast.SCallStmt { callee; args; abort_label } -> (
            let ck = normalize_name callee.v in
            (if is_control_stmt_keyword ck then (
               if ck = "EXIT" && loop_depth <= 0 then
                 emit callee.loc "EXIT is only valid inside a loop.";
               if ck = "ABORT" && current_proc = None then
                 emit callee.loc "ABORT is only valid inside a procedure.";
               if ck = "STOP" then
                 List.iter
                   (fun arg -> ignore (ty_of_expr scp current_proc arg))
                   args;
               if ck = "STOP" && List.length args > 1 then
                 emit callee.loc
                   "STOP accepts at most one optional stop code expression.")
             else
               let diag_count_before = List.length !out in
               ignore
                 (ty_of_expr scp current_proc
                    (Ast.node ~loc:s.loc (Ast.ECall { callee; args })));
               let diag_count_after = List.length !out in
               if diag_count_after = diag_count_before then
                 match sem_lookup_value scp callee.v with
                 | Some _ -> ()
                 | None ->
                     if
                       (not (sem_is_builtin_call callee.v))
                       && not
                            (should_suppress_cross_module_unresolved
                               ~is_type:false ~name:callee.v)
                     then
                       emit callee.loc
                         (Printf.sprintf
                            "Undefined procedure %S. Declare it with REF PROC \
                             %S in scope."
                            callee.v callee.v));
            match abort_label with
            | None -> ()
            | Some lab ->
                if current_proc = None then
                  emit lab.loc
                    "ABORT label phrase is only valid inside a procedure call \
                     statement."
                else
                  let lk = normalize_name lab.v in
                  if lk = "" || not (Hashtbl.mem label_depths lk) then
                    emit lab.loc
                      (Printf.sprintf "Undefined ABORT target label %S." lab.v))
        | Ast.SIf { cond; then_; else_ } -> (
            ignore (ty_of_expr scp current_proc cond);
            check_stmt scp current_proc ~loop_depth ~label_depths then_;
            match else_ with
            | None -> ()
            | Some e -> check_stmt scp current_proc ~loop_depth ~label_depths e)
        | Ast.SWhile { cond; body } ->
            ignore (ty_of_expr scp current_proc cond);
            check_stmt scp current_proc ~loop_depth:(loop_depth + 1)
              ~label_depths body
        | Ast.SFor { init; cond; step; body } ->
            let for_scope =
              match init with
              | Some
                  { v = Ast.SAssign { lhs = { v = Ast.EName lc; _ }; rhs }; _ }
                when is_single_letter_loop_control lc.v ->
                  let scp2 = sem_scope_copy scp in
                  let lty = ty_of_expr scp current_proc rhs in
                  sem_add_value scp2 lc.v (SVVar lty);
                  scp2
              | _ -> scp
            in
            (match init with
            | None -> ()
            | Some i ->
                check_stmt for_scope current_proc ~loop_depth ~label_depths i);
            (match cond with
            | None -> ()
            | Some c -> ignore (ty_of_expr for_scope current_proc c));
            (match step with
            | None -> ()
            | Some st ->
                check_stmt for_scope current_proc ~loop_depth ~label_depths st);
            check_stmt for_scope current_proc ~loop_depth:(loop_depth + 1)
              ~label_depths body
        | Ast.SReturn eo -> (
            if current_proc = None then
              emit s.loc "RETURN is only valid inside a procedure.";
            match eo with
            | None -> ()
            | Some e -> ignore (ty_of_expr scp current_proc e))
        | Ast.SLabel { body; _ } ->
            check_stmt scp current_proc ~loop_depth ~label_depths body
        | Ast.SGoto id -> (
            let key = normalize_name id.v in
            match Hashtbl.find_opt label_depths key with
            | None ->
                emit id.loc (Printf.sprintf "Undefined target label %S." id.v)
            | Some target_depth ->
                if target_depth > loop_depth then
                  emit id.loc
                    (Printf.sprintf
                       "GOTO to %S enters a deeper loop body, which is not \
                        allowed."
                       id.v))
      and check_decl (scp : sem_scope) (current_proc : sem_proc_ctx option)
          ~(loop_depth : int)
          ~label_depths:(_label_depths : (string, int) Hashtbl.t)
          (d : Ast.decl Ast.node) : unit =
        let _ = loop_depth in
        match d.v with
        | Ast.DVar { dtype; init; _ } -> (
            check_type_import_hints scp dtype;
            match init with
            | None -> ()
            | Some rhs ->
                let lty = sem_ty_of_type_expr scp.types dtype in
                let rty = ty_of_expr scp current_proc rhs in
                if not (sem_compatible lty rty) then
                  emit rhs.loc
                    (Printf.sprintf
                       "Type mismatch in initializer: expected %s, got %s."
                       (sem_ty_to_string lty) (sem_ty_to_string rty)))
        | Ast.DConst { dtype; value; _ } ->
            (match dtype with
            | None -> ()
            | Some t -> check_type_import_hints scp t);
            ignore (ty_of_expr scp current_proc value)
        | Ast.DType { defn; _ } -> check_type_import_hints scp defn
        | Ast.DDirective _ -> ()
        | Ast.DProc p ->
            let proc_scope = sem_scope_copy scp in
            let proc_label_depths = collect_label_depths p.v.body in
            List.iter (add_decl_symbol proc_scope) p.v.locals;
            let local_var_tys : (string, sem_ty) Hashtbl.t =
              Hashtbl.create 32
            in
            List.iter
              (fun dlocal ->
                match dlocal.v with
                | Ast.DVar { name; dtype; _ } ->
                    Hashtbl.replace local_var_tys (normalize_name name.v)
                      (sem_ty_of_type_expr proc_scope.types dtype)
                | _ -> ())
              p.v.locals;
            List.iter
              (fun prm -> check_type_import_hints proc_scope prm.v.ptype)
              p.v.params;
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
            List.iter
              (fun prm ->
                let pname = prm.v.pname.v in
                let direct_ty =
                  sem_ty_of_type_expr proc_scope.types prm.v.ptype
                in
                let inferred_ty =
                  match direct_ty with
                  | TyUnknown -> (
                      match
                        Hashtbl.find_opt local_var_tys (normalize_name pname)
                      with
                      | Some ty -> ty
                      | _ -> TyUnknown)
                  | ty -> ty
                in
                sem_add_value proc_scope pname (SVVar inferred_ty))
              p.v.params;
            List.iter
              (fun pd ->
                check_decl proc_scope proc_ctx ~loop_depth:0
                  ~label_depths:proc_label_depths pd)
              p.v.locals;
            check_stmt proc_scope proc_ctx ~loop_depth:0
              ~label_depths:proc_label_depths p.v.body
      in

      List.iter
        (function
          | Ast.TopDecl d ->
              check_decl scope None ~loop_depth:0
                ~label_depths:top_level_label_depths d
          | Ast.TopStmt s ->
              check_stmt scope None ~loop_depth:0
                ~label_depths:top_level_label_depths s)
        prog;
      List.rev !out
