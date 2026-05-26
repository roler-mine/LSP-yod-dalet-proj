(* Module overview: Semantic validation and type-aware symbol metadata extraction. *)

module T = Lsp.Types
open Ast
open Workspace_foundation
open Workspace_state
open Workspace_index_graph
open Workspace_imports
open Workspace_tuning
module Metadata = Workspace_symbol_metadata
module DiagAuth = Workspace_diagnostic_authority

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
  external_modifier : Ast.external_modifier;
}

type sem_value = SVVar of sem_ty | SVConst of sem_ty | SVProc of sem_proc_sig

let rec sem_ty_of_jovial_type (ty : Jovial_type.t) : sem_ty =
  match ty with
  | Jovial_type.Unknown | Jovial_type.Named _ | Jovial_type.Procedure _ ->
      TyUnknown
  | Jovial_type.Integer _ -> TyInt
  | Jovial_type.Float _ | Jovial_type.Fixed _ -> TyFloat
  | Jovial_type.BitString _ -> TyBit
  | Jovial_type.CharString _ -> TyChar
  | Jovial_type.Status _ -> TyStatus
  | Jovial_type.Pointer { target; _ } ->
      TyPointer (Option.map sem_ty_of_jovial_type target)
  | Jovial_type.Table { entry; _ } -> TyArray (sem_ty_of_jovial_type entry)
  | Jovial_type.Block fields ->
      TyRecord
        (List.map
           (fun (field : Jovial_type.field) ->
             (field.name, sem_ty_of_jovial_type field.ty))
           fields)

let rec jovial_type_of_sem_ty (ty : sem_ty) : Jovial_type.t =
  match ty with
  | TyUnknown -> Jovial_type.Unknown
  | TyInt -> Jovial_type.Integer { kind = Jovial_type.Signed; bits = None }
  | TyFloat -> Jovial_type.Float { precision = None }
  | TyBit -> Jovial_type.BitString { bits = None }
  | TyChar | TyString -> Jovial_type.CharString { chars = None }
  | TyStatus -> Jovial_type.Status { values = [] }
  | TyPointer target ->
      Jovial_type.Pointer
        { target = Option.map jovial_type_of_sem_ty target; typed = Option.is_some target }
  | TyArray entry ->
      Jovial_type.Table { dims = []; entry = jovial_type_of_sem_ty entry }
  | TyRecord fields ->
      Jovial_type.Block
        (List.map
           (fun (name, ty) ->
             {
               Jovial_type.name = name;
               key = normalize_name name;
               ty = jovial_type_of_sem_ty ty;
               loc = Ast.Loc.none;
             })
           fields)

type sem_exports = {
  values : (string, sem_value) Hashtbl.t;
  types : (string, Ast.type_expr Ast.node) Hashtbl.t;
}

type sem_scope = sem_exports

type rich_proc_sig = {
  rich_param_tys : Jovial_type.t list option;
  rich_ret_ty : Jovial_type.t option;
  rich_use_attr : Ast.proc_use;
}

type rich_value =
  | RichVar of Jovial_type.t
  | RichConst of Jovial_type.t
  | RichProc of rich_proc_sig

type rich_scope = {
  rich_values : (string, rich_value) Hashtbl.t;
  rich_types : (string, Ast.type_expr Ast.node) Hashtbl.t;
}

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

let rich_scope_empty () : rich_scope =
  { rich_values = Hashtbl.create 64; rich_types = Hashtbl.create 64 }

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

let rich_scope_copy (s : rich_scope) : rich_scope =
  {
    rich_values = copy_tbl s.rich_values;
    rich_types = copy_tbl s.rich_types;
  }

let rich_add_value ?(overwrite = true) (s : rich_scope) (name : string)
    (v : rich_value) : unit =
  let k = normalize_name name in
  if k <> "" && (overwrite || not (Hashtbl.mem s.rich_values k)) then
    Hashtbl.replace s.rich_values k v

let rich_add_type ?(overwrite = true) (s : rich_scope) (name : string)
    (t : Ast.type_expr Ast.node) : unit =
  let k = normalize_name name in
  if k <> "" && (overwrite || not (Hashtbl.mem s.rich_types k)) then
    Hashtbl.replace s.rich_types k t

let rich_type_env (s : rich_scope) : Jovial_type.type_env =
  let env = Jovial_type.empty_type_env () in
  Hashtbl.iter (fun k v -> Jovial_type.add_type env k v) s.rich_types;
  env

let rich_ty_of_type_expr (s : rich_scope) (t : Ast.type_expr Ast.node) :
    Jovial_type.t =
  Jovial_type.of_ast_type_expr (rich_type_env s) t

let rich_lookup_value (s : rich_scope) (name : string) : rich_value option =
  Hashtbl.find_opt s.rich_values (normalize_name name)

let sem_find_record_field (fields : (string * sem_ty) list) (name : string) :
    sem_ty option =
  let key = normalize_name name in
  fields
  |> List.find_opt (fun (nm, _) -> normalize_name nm = key)
  |> Option.map snd

let sem_is_builtin_type (k : string) : bool = Keyword.is_builtin_type_name k

let is_single_letter_loop_control (name : string) : bool =
  String.length name = 1
  && match name.[0] with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

let sem_ty_of_scalar_base (base : Ast.scalar_base) : sem_ty =
  match base with
  | Ast.ScalarUnsigned | Ast.ScalarSigned -> TyInt
  | Ast.ScalarFloat | Ast.ScalarFixed -> TyFloat
  | Ast.ScalarBit -> TyBit
  | Ast.ScalarChar -> TyChar

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
  | Ast.TScalar { base; _ } -> sem_ty_of_scalar_base base
  | Ast.TArray { elem; _ } | Ast.TSpecifiedTable { elem; _ } ->
      TyArray (sem_ty_of_type_expr ~seen types elem)
  | Ast.TPointer { v = Ast.TName id; _ }
    when normalize_name id.v = "__UNTYPED_POINTER__" ->
      TyPointer None
  | Ast.TPointer inner ->
      TyPointer (Some (sem_ty_of_type_expr ~seen types inner))
  | Ast.TStatus _ -> TyStatus
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
  {
    param_tys;
    ret_ty;
    use_attr = p.v.use_attr;
    external_modifier = p.v.external_modifier;
  }

let block_proc_names_of_program (prog : Ast.program) : (string, bool) Hashtbl.t
    =
  let out = Hashtbl.create 32 in
  let rec collect_top = function
    | Ast.TopDecl d -> (
        match d.v with
        | Ast.DDirective { name; args = nm :: _ }
          when normalize_name name.v = "BLOCK" ->
            let k = normalize_name nm.v in
            if k <> "" then Hashtbl.replace out k true
        | _ -> ())
    | Ast.TopStmt _ | Ast.TopError _ -> ()
    | Ast.TopModule m -> List.iter collect_top m.v.module_items
  in
  List.iter collect_top prog;
  out

let sem_exports_of_program_with_base ?(base : sem_scope option)
    (prog : Ast.program) : sem_exports =
  let out =
    match base with None -> sem_scope_empty () | Some s -> sem_scope_copy s
  in
  let block_names = block_proc_names_of_program prog in
  let is_block_proc (p : Ast.proc Ast.node) : bool =
    Hashtbl.mem block_names (normalize_name p.v.name.v)
  in
  let rec collect_types_decl ~(in_block : bool) (d : Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DError _ -> ()
    | Ast.DType { name; defn; _ } -> sem_add_type out name.v defn
    | Ast.DProc p ->
        if in_block || is_block_proc p then
          List.iter (collect_types_decl ~in_block:true) p.v.locals
    | Ast.DVar _ | Ast.DConst _ | Ast.DOverlay _ | Ast.DDirective _ -> ()
  in
  let rec collect_values_decl ~(in_block : bool) (d : Ast.decl Ast.node) : unit
      =
    match d.v with
    | Ast.DError _ -> ()
    | Ast.DVar { name; dtype; data_decl_kind; _ } ->
        sem_add_value out name.v (SVVar (sem_ty_of_type_expr out.types dtype));
        (match (data_decl_kind, dtype.v) with
        | Ast.DataBlock, Ast.TRecord fields ->
            List.iter
              (fun field ->
                let fv = field.v in
                sem_add_value ~overwrite:false out fv.fname.v
                  (SVVar (sem_ty_of_type_expr out.types fv.ftype)))
              fields
        | _ -> ())
    | Ast.DConst { name; dtype; value = _; _ } ->
        let ty =
          match dtype with
          | Some t -> sem_ty_of_type_expr out.types t
          | None -> TyUnknown
        in
        sem_add_value out name.v (SVConst ty)
    | Ast.DType _ | Ast.DOverlay _ -> ()
    | Ast.DProc p ->
        if not in_block then
          sem_add_value out p.v.name.v
            (SVProc (sem_proc_sig_of_proc out.types p));
        if in_block || is_block_proc p then
          List.iter (collect_values_decl ~in_block:true) p.v.locals
    | Ast.DDirective _ -> ()
  in
  let rec collect_types_top = function
    | Ast.TopDecl d -> collect_types_decl ~in_block:false d
    | Ast.TopStmt _ | Ast.TopError _ -> ()
    | Ast.TopModule m -> List.iter collect_types_top m.v.module_items
  in
  let rec collect_values_top = function
    | Ast.TopDecl d -> collect_values_decl ~in_block:false d
    | Ast.TopStmt _ | Ast.TopError _ -> ()
    | Ast.TopModule m -> List.iter collect_values_top m.v.module_items
  in
  List.iter collect_types_top prog;
  List.iter collect_values_top prog;
  out

let sem_exports_of_program (prog : Ast.program) : sem_exports =
  sem_exports_of_program_with_base prog

let rich_proc_sig_of_proc (scope : rich_scope) (p : Ast.proc Ast.node) :
    rich_proc_sig =
  let local_var_tys : (string, Jovial_type.t) Hashtbl.t = Hashtbl.create 32 in
  let proc_scope = rich_scope_copy scope in
  List.iter
    (fun d ->
      match d.v with
      | Ast.DType { name; defn; _ } -> rich_add_type proc_scope name.v defn
      | _ -> ())
    p.v.locals;
  List.iter
    (fun d ->
      match d.v with
      | Ast.DVar { name; dtype; _ } ->
          let ty = rich_ty_of_type_expr proc_scope dtype in
          Hashtbl.replace local_var_tys (normalize_name name.v) ty
      | Ast.DConst { name; dtype = Some dtype; _ } ->
          let ty = rich_ty_of_type_expr proc_scope dtype in
          Hashtbl.replace local_var_tys (normalize_name name.v) ty
      | _ -> ())
    p.v.locals;
  let param_tys =
    if p.v.params = [] then Some []
    else
      Some
        (List.map
           (fun prm ->
             let direct = rich_ty_of_type_expr proc_scope prm.v.ptype in
             match direct with
             | Jovial_type.Named _ | Jovial_type.Unknown -> (
                 match
                   Hashtbl.find_opt local_var_tys
                     (normalize_name prm.v.pname.v)
                 with
                 | Some ty -> ty
                 | None -> direct)
             | ty -> ty)
           p.v.params)
  in
  let ret_ty = Option.map (rich_ty_of_type_expr proc_scope) p.v.returns in
  { rich_param_tys = param_tys; rich_ret_ty = ret_ty; rich_use_attr = p.v.use_attr }

let rich_exports_of_program_with_base ?(base : rich_scope option)
    (prog : Ast.program) : rich_scope =
  let out =
    match base with None -> rich_scope_empty () | Some s -> rich_scope_copy s
  in
  let block_names = block_proc_names_of_program prog in
  let is_block_proc (p : Ast.proc Ast.node) : bool =
    Hashtbl.mem block_names (normalize_name p.v.name.v)
  in
  let rec collect_types_decl ~(in_block : bool) (d : Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DError _ -> ()
    | Ast.DType { name; defn; _ } -> rich_add_type out name.v defn
    | Ast.DProc p ->
        if in_block || is_block_proc p then
          List.iter (collect_types_decl ~in_block:true) p.v.locals
    | Ast.DVar _ | Ast.DConst _ | Ast.DOverlay _ | Ast.DDirective _ -> ()
  in
  let rec collect_values_decl ~(in_block : bool) (d : Ast.decl Ast.node) : unit
      =
    match d.v with
    | Ast.DError _ -> ()
    | Ast.DVar { name; dtype; data_decl_kind; _ } ->
        rich_add_value out name.v (RichVar (rich_ty_of_type_expr out dtype));
        (match (data_decl_kind, dtype.v) with
        | Ast.DataBlock, Ast.TRecord fields ->
            List.iter
              (fun field ->
                let fv = field.v in
                rich_add_value ~overwrite:false out fv.fname.v
                  (RichVar (rich_ty_of_type_expr out fv.ftype)))
              fields
        | _ -> ())
    | Ast.DConst { name; dtype; _ } ->
        let ty =
          match dtype with
          | Some t -> rich_ty_of_type_expr out t
          | None -> Jovial_type.Unknown
        in
        rich_add_value out name.v (RichConst ty)
    | Ast.DType _ | Ast.DOverlay _ -> ()
    | Ast.DProc p ->
        if not in_block then
          rich_add_value out p.v.name.v
            (RichProc (rich_proc_sig_of_proc out p));
        if in_block || is_block_proc p then
          List.iter (collect_values_decl ~in_block:true) p.v.locals
    | Ast.DDirective _ -> ()
  in
  let rec collect_types_top = function
    | Ast.TopDecl d -> collect_types_decl ~in_block:false d
    | Ast.TopStmt _ | Ast.TopError _ -> ()
    | Ast.TopModule m -> List.iter collect_types_top m.v.module_items
  in
  let rec collect_values_top = function
    | Ast.TopDecl d -> collect_values_decl ~in_block:false d
    | Ast.TopStmt _ | Ast.TopError _ -> ()
    | Ast.TopModule m -> List.iter collect_values_top m.v.module_items
  in
  List.iter collect_types_top prog;
  List.iter collect_values_top prog;
  out

let rich_exports_of_program (prog : Ast.program) : rich_scope =
  rich_exports_of_program_with_base prog

let sem_exports_of_doc (doc : Document.t) : sem_exports =
  let doc = Document.ensure_parsed doc in
  match Document.current_parse doc with
  | Some { Document.parsed_ast = Some prog; _ } -> sem_exports_of_program prog
  | _ -> sem_scope_empty ()

let rich_exports_of_doc (doc : Document.t) : rich_scope =
  let doc = Document.ensure_parsed doc in
  match Document.current_parse doc with
  | Some { Document.parsed_ast = Some prog; _ } -> rich_exports_of_program prog
  | _ -> rich_scope_empty ()

let semantic_export_cache_limit = 512
let sem_exports_with_import_cache : (string, sem_exports) Hashtbl.t =
  Hashtbl.create 128

let rich_exports_with_import_cache : (string, rich_scope) Hashtbl.t =
  Hashtbl.create 128

let doc_text_hash_for_export_cache (ws : t) (doc : Document.t) : string =
  match
    Semantic_store.snapshot_for_uri ws.semantic_store ~uri:doc.Document.uri
  with
  | Some snap
    when snap.Semantic_store.Snapshot.doc_rev = doc.Document.parse_rev ->
      snap.Semantic_store.Snapshot.text_hash
  | _ -> Digest.to_hex (Digest.string doc.Document.text)

let export_cache_key (ws : t) (doc : Document.t) ~(kind : string) : string =
  Printf.sprintf "%s|wid:%d|store:%d|rev:%d|parse:%d|uri:%s|hash:%s" kind
    ws.workspace_id
    (Semantic_store.global_rev ws.semantic_store)
    doc.Document.rev doc.Document.parse_rev
    (Uri_path.docuri_to_string doc.Document.uri)
    (doc_text_hash_for_export_cache ws doc)

let clear_export_cache_if_large tbl =
  if Hashtbl.length tbl > semantic_export_cache_limit then Hashtbl.clear tbl

let cached_sem_exports_with_imports (ws : t) (doc : Document.t)
    (compute : unit -> sem_exports) : sem_exports =
  let key = export_cache_key ws doc ~kind:"sem" in
  match Hashtbl.find_opt sem_exports_with_import_cache key with
  | Some exp ->
      Perf_stats.tick "diag.semantic_exports_cache_hit";
      exp
  | None ->
      Perf_stats.tick "diag.semantic_exports_cache_miss";
      let exp = compute () in
      clear_export_cache_if_large sem_exports_with_import_cache;
      Hashtbl.replace sem_exports_with_import_cache key exp;
      exp

let cached_rich_exports_with_imports (ws : t) (doc : Document.t)
    (compute : unit -> rich_scope) : rich_scope =
  let key = export_cache_key ws doc ~kind:"rich" in
  match Hashtbl.find_opt rich_exports_with_import_cache key with
  | Some exp ->
      Perf_stats.tick "diag.rich_exports_cache_hit";
      exp
  | None ->
      Perf_stats.tick "diag.rich_exports_cache_miss";
      let exp = compute () in
      clear_export_cache_if_large rich_exports_with_import_cache;
      Hashtbl.replace rich_exports_with_import_cache key exp;
      exp

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
            match
              source_stem_of_filename ~source_extensions:ws.source_extensions
                (Filename.basename path)
            with
            | None -> None
            | Some stem ->
                let k = normalize_name stem in
                if k = "" then None else Some k))
  in

  let add_skeleton_hints compool (doc : Document.t) : unit =
    match Skeleton_index.of_document doc with
    | None -> ()
    | Some sk ->
        Skeleton_index.symbols sk
        |> List.iter (fun (sym : Skeleton_index.symbol_decl) ->
               match sym.metadata.Metadata.jovial_kind with
               | Metadata.JovialType | Metadata.JovialBuiltinType ->
                   add_compool_hint types ~symbol_key:sym.normalized_name
                     ~compool_key:compool
               | Metadata.JovialItem | Metadata.JovialTable
               | Metadata.JovialBlock | Metadata.JovialProcedure
               | Metadata.JovialFunction | Metadata.JovialConstantItem
               | Metadata.JovialConstantTable
               | Metadata.JovialStatusConstant ->
                   add_compool_hint values ~symbol_key:sym.normalized_name
                     ~compool_key:compool
               | _ -> ())
  in

  let add_doc_hints (doc : Document.t) : unit =
    match hint_compool_key_of_doc doc with
    | None -> ()
    | Some compool ->
        if String.length doc.Document.text <= ws.large_file_threshold_bytes then (
          let exp = sem_exports_of_doc doc in
          Hashtbl.iter
            (fun sym _v ->
              add_compool_hint values ~symbol_key:sym ~compool_key:compool)
            exp.values;
          Hashtbl.iter
            (fun sym _ ->
              add_compool_hint types ~symbol_key:sym ~compool_key:compool)
            exp.types)
        else Perf_stats.tick "bg.hint_large_doc_skeleton_only";
        add_skeleton_hints compool doc
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
  | Ast.LBit _ -> TyBit
  | Ast.LString s -> if String.length s = 1 then TyChar else TyString
  | Ast.LChar _ -> TyChar
  | Ast.LBool _ -> TyBit
  | Ast.LNull -> TyPointer None

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

let sem_ty_to_mismatch_string (t : sem_ty) : string =
  sem_ty_to_string (sem_scalarize t)

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

let sem_is_numeric = function TyInt | TyFloat -> true | _ -> false
let sem_is_integer = function TyInt -> true | _ -> false
let sem_is_bit = function TyBit -> true | _ -> false
let sem_is_character = function TyChar | TyString -> true | _ -> false
let sem_is_status = function TyStatus -> true | _ -> false
let sem_is_table = function TyArray _ -> true | _ -> false

let validate_semantics_with_authority (ws : t) (doc : Document.t) :
    DiagAuth.diagnostic list =
  let doc = Document.ensure_parsed doc in
  match Document.current_parse doc with
  | Some { Document.parsed_ast = Some prog; _ } ->
      let seen = Hashtbl.create 128 in
      let out = ref [] in
      let diagnostic_message (diag : T.Diagnostic.t) =
        match diag.message with
        | `String s -> s
        | `MarkupContent mc -> mc.value
      in
      let emit_diag (diag : DiagAuth.diagnostic) =
        let loc = diag.lsp.range in
        let k =
          Printf.sprintf "%d|%d|%d|%s|%s"
            loc.start.line loc.start.character loc.end_.line
            (DiagAuth.label diag.authority)
            (diagnostic_message diag.lsp)
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := diag :: !out)
      in
      let emit (loc : Ast.Loc.t) (msg : string) =
        let k =
          Printf.sprintf "%s|%d|%d|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line loc.start_pos.col msg
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := DiagAuth.local_semantic (diag_semantic loc msg) :: !out)
      in
      let emit_cross_module_provisional (loc : Ast.Loc.t) (msg : string) =
        let diag =
          diag_semantic loc msg |> DiagAuth.soften_for_warmup
          |> DiagAuth.cross_module_provisional
        in
        emit_diag diag
      in
      let emit_import_hint (loc : Ast.Loc.t) ~(kind : string) ~(symbol : string)
          ~(compools : string list) ~(selected_imported : bool) =
        let k =
          Printf.sprintf "%s|%d|%d|import|%s|%s|%b|%s"
            (match loc.file with Some f -> f | None -> "")
            loc.start_pos.line loc.start_pos.col kind symbol selected_imported
            (String.concat "," compools)
        in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out :=
            DiagAuth.cross_module_authoritative
              (diag_missing_import_hint ~selected_imported ~loc ~kind ~symbol
                 ~compools)
            :: !out)
      in

      let import_dirs = sem_import_dirs doc in
      let doc_cache : (string, Document.t option) Hashtbl.t =
        Hashtbl.create 16
      in
      let exports_cache : (string, sem_exports option) Hashtbl.t =
        Hashtbl.create 16
      in
      let rich_exports_cache : (string, rich_scope option) Hashtbl.t =
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

      let emit_warning (loc : Ast.Loc.t) (msg : string) =
        Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Warning
          ~source:"semantic" ~message:msg loc
        |> DiagAuth.local_semantic |> emit_diag
      in

      let status_owners = Jovial_status.owners_of_program prog in
      let status_owners_by_value : (string, Jovial_status.owner list) Hashtbl.t =
        Hashtbl.create 64
      in
      let add_status_owner_value (owner : Jovial_status.owner)
          (value : Jovial_status.value) =
        if value.key <> "" then
          let prev =
            Option.value
              (Hashtbl.find_opt status_owners_by_value value.key)
              ~default:[]
          in
          if
            not
              (List.exists
                 (fun (old : Jovial_status.owner) ->
                   old.owner_key = owner.owner_key
                   && old.owner_name = owner.owner_name)
                 prev)
          then Hashtbl.replace status_owners_by_value value.key (owner :: prev)
      in
      List.iter
        (fun (owner : Jovial_status.owner) ->
          List.iter (add_status_owner_value owner) owner.values)
        status_owners;

      let hint_tables :
          ((string, string list) Hashtbl.t * (string, string list) Hashtbl.t)
          option
          ref =
        ref ws.symbol_hints
      in
      let raw_hint_compools_for ~(is_type : bool) (name : string) :
          string list =
        let key = normalize_name name in
        if key = "" then []
        else
          let hint_values, hint_types =
            match !hint_tables with
            | Some idx -> idx
            | None ->
                let idx = symbol_hint_index ws in
                hint_tables := Some idx;
                idx
          in
          let tbl = if is_type then hint_types else hint_values in
          match Hashtbl.find_opt tbl key with
          | None -> []
          | Some xs -> List.map normalize_name xs
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
        let selected_compools =
          compools
          |> List.filter (fun c ->
                 match Hashtbl.find_opt imported_compools (normalize_name c) with
                 | Some `Selected -> true
                 | Some `All | None -> false)
        in
        if selected_compools <> [] then
          emit_import_hint loc ~kind ~symbol ~compools:selected_compools
            ~selected_imported:true
        else if compools <> [] then
          emit_import_hint loc ~kind ~symbol ~compools
            ~selected_imported:false
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
      let merge_sem_import (base : sem_scope) (imp : compool_import_dir)
          (exp : sem_exports) : unit =
        if imp.selected = [] then (
          Hashtbl.iter
            (fun k v -> sem_add_value ~overwrite:false base k v)
            exp.values;
          Hashtbl.iter
            (fun k v -> sem_add_type ~overwrite:false base k v)
            exp.types)
        else
          List.iter
            (fun (nm, _loc) ->
              let key = normalize_name nm in
              (match Hashtbl.find_opt exp.values key with
              | Some v -> sem_add_value ~overwrite:false base key v
              | None -> ());
              match Hashtbl.find_opt exp.types key with
              | Some t -> sem_add_type ~overwrite:false base key t
              | None -> ())
            imp.selected
      in
      let merge_rich_import (base : rich_scope) (imp : compool_import_dir)
          (exp : rich_scope) : unit =
        if imp.selected = [] then (
          Hashtbl.iter
            (fun k v -> rich_add_value ~overwrite:false base k v)
            exp.rich_values;
          Hashtbl.iter
            (fun k v -> rich_add_type ~overwrite:false base k v)
            exp.rich_types)
        else
          List.iter
            (fun (nm, _loc) ->
              let key = normalize_name nm in
              (match Hashtbl.find_opt exp.rich_values key with
              | Some v -> rich_add_value ~overwrite:false base key v
              | None -> ());
              match Hashtbl.find_opt exp.rich_types key with
              | Some t -> rich_add_type ~overwrite:false base key t
              | None -> ())
            imp.selected
      in
      let rec get_exports_for_compool (name : string) : sem_exports option =
        let key = normalize_name name in
        match Hashtbl.find_opt exports_cache key with
        | Some x -> x
        | None ->
            Hashtbl.replace exports_cache key None;
            let x =
              match get_doc_for_compool key with
              | None -> None
              | Some d -> Some (sem_exports_of_doc_with_imports d)
            in
            Hashtbl.replace exports_cache key x;
            x
      and sem_exports_of_doc_with_imports (d : Document.t) : sem_exports =
        cached_sem_exports_with_imports ws d (fun () ->
            let d = Document.ensure_parsed d in
            match Document.current_parse d with
            | Some { Document.parsed_ast = Some prog; _ } ->
                let base = sem_scope_empty () in
                sem_import_dirs d
                |> List.iter (fun imp ->
                       match get_exports_for_compool imp.compool with
                       | None -> ()
                       | Some exp -> merge_sem_import base imp exp);
                sem_exports_of_program_with_base ~base prog
            | _ -> sem_scope_empty ())
      in
      let rec get_rich_exports_for_compool (name : string) : rich_scope option =
        let key = normalize_name name in
        match Hashtbl.find_opt rich_exports_cache key with
        | Some x -> x
        | None ->
            Hashtbl.replace rich_exports_cache key None;
            let x =
              match get_doc_for_compool key with
              | None -> None
              | Some d -> Some (rich_exports_of_doc_with_imports d)
            in
            Hashtbl.replace rich_exports_cache key x;
            x
      and rich_exports_of_doc_with_imports (d : Document.t) : rich_scope =
        cached_rich_exports_with_imports ws d (fun () ->
            let d = Document.ensure_parsed d in
            match Document.current_parse d with
            | Some { Document.parsed_ast = Some prog; _ } ->
                let base = rich_scope_empty () in
                sem_import_dirs d
                |> List.iter (fun imp ->
                       match get_rich_exports_for_compool imp.compool with
                       | None -> ()
                       | Some exp -> merge_rich_import base imp exp);
                rich_exports_of_program_with_base ~base prog
            | _ -> rich_scope_empty ())
      in

      let selected_import_contains (imp : compool_import_dir) (key : string) :
          bool =
        List.exists (fun (nm, _loc) -> normalize_name nm = key) imp.selected
      in

      let maybe_visible_through_import ~(is_type : bool) ~(name : string) :
          bool =
        let key = normalize_name name in
        key <> ""
        &&
        let hinted_compools = lazy (raw_hint_compools_for ~is_type name) in
        List.exists
          (fun imp ->
               let selected = selected_import_contains imp key in
               let hinted =
                 List.mem (normalize_name imp.compool) (Lazy.force hinted_compools)
               in
               match get_exports_for_compool imp.compool with
                | None ->
                    if imp.selected = [] then hinted else selected
                | Some exp ->
                    let exported =
                      if is_type then Hashtbl.mem exp.types key
                      else Hashtbl.mem exp.values key
                   in
                   let visible = exported || hinted in
                   if imp.selected = [] then visible else selected && visible)
          import_dirs
      in

      let likely_cross_module_unresolved_candidate ~(is_type : bool)
          ~(name : string) : bool =
        if import_dirs = [] then false
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
            qualified_import_match
            ||
            let hinted_compools = lazy (raw_hint_compools_for ~is_type name) in
            let import_matches_hint imp =
              List.mem (normalize_name imp.compool) (Lazy.force hinted_compools)
              && (imp.selected = [] || selected_import_contains imp key)
            in
            List.exists import_matches_hint import_dirs
            ||
            List.exists
              (fun imp ->
                let selected_match =
                  List.exists (fun (nm, _loc) -> nm = key) imp.selected
                in
                match get_exports_for_compool imp.compool with
                | None -> selected_match
                | Some exp ->
                    let exported =
                      if is_type then Hashtbl.mem exp.types key
                      else Hashtbl.mem exp.values key
                    in
                    if imp.selected = [] then exported else selected_match)
              import_dirs
      in
      let should_suppress_cross_module_unresolved ~(is_type : bool)
          ~(name : string) : bool =
        if not warmup_suppress_crossmodule_unresolved then false
        else if ws.startup_diag_hover_ready_ms <> None then false
        else if likely_cross_module_unresolved_candidate ~is_type ~name then (
          Perf_stats.tick "diag.xmodule_suppressed";
          Perf_stats.tick "diag.warmup_suppressed";
          true)
        else false
      in
      let emit_provisional_cross_module_unresolved ~(is_type : bool)
          ~(name : string) ~(loc : Ast.Loc.t) ~(message : string) : bool =
        if should_suppress_cross_module_unresolved ~is_type ~name then (
          emit_cross_module_provisional loc message;
          true)
        else false
      in
      let emit_authoritative_unresolved ~(is_type : bool) ~(name : string)
          ~(loc : Ast.Loc.t) ~(message : string) : unit =
        if likely_cross_module_unresolved_candidate ~is_type ~name then
          diag_semantic loc message
          |> DiagAuth.cross_module_authoritative |> emit_diag
        else emit loc message
      in

      let scope = sem_scope_copy (sem_exports_of_doc_with_imports doc) in
      let rich_scope =
        rich_scope_copy (rich_exports_of_doc_with_imports doc)
      in

      let sem_lookup_value (scp : sem_scope) (name : string) : sem_value option
          =
        Hashtbl.find_opt scp.values (normalize_name name)
      in

      let sem_is_builtin_call (name : string) : bool =
        let k = normalize_name name in
        k = "__CONV__" || k = "__PRESET__" || k = "__POW__" || k = "__RANGE__"
        || is_builtin_function_name k
        || Implementation_config.is_system_subroutine ws.implementation_config k
      in

      let asm_visible_compools =
        let imported =
          extract_compool_import_dirs doc
          |> List.map (fun (d : compool_import_dir) -> d.compool)
        in
        match doc.Document.compool_def with
        | None -> imported
        | Some c -> c :: imported
      in

      let sem_is_asm_proc (name : string) : bool =
        let key = normalize_name name in
        key <> ""
        && Workspace_asm.label_exists_for_key
             ~visible_compools:asm_visible_compools ws ~key
      in

      let register_ctf_decl_symbol (ctf_env : Jovial_compile_time.env)
          (d : Ast.decl Ast.node) : unit =
        match d.v with
        | Ast.DError _ -> ()
        | Ast.DConst { name; data_decl_kind = Ast.DataTable; _ } ->
            Jovial_compile_time.add_non_constant ctf_env name.v
        | Ast.DConst { name; value; _ } ->
            Jovial_compile_time.add_constant ctf_env name.v value
        | Ast.DVar { name; _ } | Ast.DType { name; _ } ->
            Jovial_compile_time.add_non_constant ctf_env name.v
        | Ast.DOverlay overlay ->
            Jovial_compile_time.add_non_constant ctf_env
              overlay.overlay_name.v
        | Ast.DProc p ->
            Jovial_compile_time.add_non_constant ctf_env p.v.name.v
        | Ast.DDirective _ -> ()
      in

      let top_ctf_env = Jovial_compile_time.empty_env () in
      Jovial_compile_time.add_implementation_config top_ctf_env
        ws.implementation_config;
      let rec register_ctf_top = function
        | Ast.TopDecl d -> register_ctf_decl_symbol top_ctf_env d
        | Ast.TopStmt _ | Ast.TopError _ -> ()
        | Ast.TopModule m -> List.iter register_ctf_top m.v.module_items
      in
      List.iter register_ctf_top prog;
      let layout_config =
        Jovial_layout.config_of_implementation_config ws.implementation_config
      in

      let emit_compile_time_required ?message
          (ctf_env : Jovial_compile_time.env) (e : Ast.expr Ast.node) : unit =
        let result = Jovial_compile_time.eval_expr ~env:ctf_env e in
        let diag =
          match message with
          | None -> Jovial_compile_time.diagnostic_for_required result e.loc
          | Some message ->
              Jovial_compile_time.diagnostic_for_required ~message result e.loc
        in
        match diag with
        | None -> ()
        | Some d -> emit_diag (DiagAuth.local_semantic d)
      in

      let is_open_table_dim (e : Ast.expr Ast.node) : bool =
        match e.v with
        | Ast.EName id -> normalize_name id.v = "*"
        | _ -> false
      in

      let ctf_int_value (ctf_env : Jovial_compile_time.env)
          (e : Ast.expr Ast.node) : int64 option =
        match Jovial_compile_time.eval_expr ~env:ctf_env e with
        | Jovial_compile_time.Known (Jovial_compile_time.CtfInt n) -> Some n
        | _ -> None
      in

      let ctf_int_required_value ~(message : string)
          (ctf_env : Jovial_compile_time.env) (e : Ast.expr Ast.node) :
          int64 option =
        match Jovial_compile_time.eval_expr ~env:ctf_env e with
        | Jovial_compile_time.Known (Jovial_compile_time.CtfInt n) -> Some n
        | Jovial_compile_time.Known _ ->
            emit e.loc (message ^ ": expected an integer compile-time value.");
            None
        | (Jovial_compile_time.Unknown _ as result) ->
            let diag =
              Jovial_compile_time.diagnostic_for_required
                ~diagnose_unknown_identifiers:true
                ~diagnose_unsupported_constructs:true ~message result e.loc
            in
            (match diag with
            | Some d -> emit_diag (DiagAuth.local_semantic d)
            | None -> emit e.loc (message ^ ": compile-time integer required."));
            None
      in

      let check_specified_table_layout
          (ctf_env : Jovial_compile_time.env)
          ~(kind : Ast.specified_table_kind) (elem : Ast.type_expr Ast.node) :
          unit =
        let entry_size =
          match kind with
          | Ast.SpecTableW entry_size | Ast.SpecTableV (Some entry_size) ->
              ctf_int_required_value
                ~message:"Invalid specified table entry size" ctf_env
                entry_size
          | Ast.SpecTableV None -> None
        in
        let fields =
          match elem.v with Ast.TRecord fields -> fields | _ -> []
        in
        let seen_positions : (string, Ast.ident) Hashtbl.t =
          Hashtbl.create 16
        in
        List.iter
          (fun (field : Ast.field_decl Ast.node) ->
            match field.v.fpos with
            | None ->
                emit field.v.fname.loc
                  (Printf.sprintf
                     "Specified table field %S requires a POS(startbit,startword) clause."
                     field.v.fname.v)
            | Some pos ->
                let start_bit =
                  ctf_int_required_value ~message:"Invalid POS expression"
                    ctf_env pos.pos_start_bit
                in
                let start_word =
                  ctf_int_required_value ~message:"Invalid POS expression"
                    ctf_env pos.pos_start_word
                in
                (match (start_bit, entry_size) with
                | Some bit, Some size when bit < 0L || bit >= size ->
                    emit pos.pos_start_bit.loc
                      (Printf.sprintf
                         "Specified table field %S POS start bit %Ld is outside entry size %Ld."
                         field.v.fname.v bit size)
                | _ -> ());
                (match (start_bit, start_word) with
                | Some bit, Some word ->
                    let key = Int64.to_string bit ^ ":" ^ Int64.to_string word in
                    (match Hashtbl.find_opt seen_positions key with
                    | Some first ->
                        emit field.v.fname.loc
                          (Printf.sprintf
                             "Duplicate specified table field POS(%Ld,%Ld) for %S; first used by %S."
                             bit word field.v.fname.v first.v)
                    | None -> Hashtbl.add seen_positions key field.v.fname)
                | _ -> ()))
          fields
      in

      let emit_layout_issues (ctf_env : Jovial_compile_time.env)
          (t : Ast.type_expr Ast.node) : unit =
        match Jovial_layout.table_layout_of_type ~config:layout_config ctf_env t with
        | None -> ()
        | Some layout ->
            Jovial_layout.issues_for_layout layout
            |> List.iter (fun (issue : Jovial_layout.issue) ->
                   emit issue.loc issue.message)
      in

      let emit_invalid_dimension (loc : Ast.Loc.t) (msg : string) =
        emit loc ("Invalid table dimension: " ^ msg)
      in

      let check_simple_dim_bounds (ctf_env : Jovial_compile_time.env)
          (e : Ast.expr Ast.node) : unit =
        if is_open_table_dim e then ()
        else
          match e.v with
          | Ast.ERange { lo; hi } -> (
              match (ctf_int_value ctf_env lo, ctf_int_value ctf_env hi) with
              | Some lo_n, Some hi_n when hi_n < lo_n ->
                  emit_invalid_dimension e.loc
                    (Printf.sprintf
                       "lower bound %Ld is greater than upper bound %Ld."
                       lo_n hi_n)
              | _ -> ())
          | _ -> (
              match ctf_int_value ctf_env e with
              | Some n when n <= 0L ->
                  emit_invalid_dimension e.loc
                    (Printf.sprintf
                       "entry count must be positive, got %Ld." n)
              | _ -> ())
      in

      let dim_entry_count (ctf_env : Jovial_compile_time.env)
          (e : Ast.expr Ast.node) : int64 option =
        if is_open_table_dim e then None
        else
          match e.v with
          | Ast.ERange { lo; hi } -> (
              match (ctf_int_value ctf_env lo, ctf_int_value ctf_env hi) with
              | Some lo_n, Some hi_n when hi_n >= lo_n ->
                  Some (Int64.succ (Int64.sub hi_n lo_n))
              | _ -> None)
          | _ -> (
              match ctf_int_value ctf_env e with
              | Some n when n > 0L -> Some n
              | _ -> None)
      in

      let table_entry_capacity (ctf_env : Jovial_compile_time.env)
          (dims : Ast.expr Ast.node list) : int64 option =
        match dims with
        | [] -> None
        | _ ->
            let rec loop acc = function
              | [] -> Some acc
              | dim :: rest -> (
                  match dim_entry_count ctf_env dim with
                  | None -> None
                  | Some n ->
                      let next = Int64.mul acc n in
                      if acc <> 0L && Int64.div next acc <> n then None
                      else loop next rest)
            in
            loop 1L dims
      in

      let rec check_compile_time_type_expr
          (ctf_env : Jovial_compile_time.env) (t : Ast.type_expr Ast.node) :
          unit =
        match t.v with
        | Ast.TName _ -> ()
        | Ast.TScalar { sizes; _ } ->
            List.iter (check_compile_time_dim ctf_env) sizes;
            emit_layout_issues ctf_env t
        | Ast.TArray { elem; dims } ->
            List.iter (check_compile_time_dim ctf_env) dims;
            emit_layout_issues ctf_env t;
            check_compile_time_type_expr ctf_env elem
        | Ast.TSpecifiedTable { elem; dims; kind } ->
            List.iter (check_compile_time_dim ctf_env) dims;
            check_specified_table_layout ctf_env ~kind elem;
            emit_layout_issues ctf_env t;
            check_compile_time_type_expr ctf_env elem
        | Ast.TPointer inner -> check_compile_time_type_expr ctf_env inner
        | Ast.TStatus values ->
            List.iter
              (fun (value : Ast.status_value Ast.node) ->
                Option.iter (emit_compile_time_required ctf_env)
                  value.v.sv_representation)
              values
        | Ast.TRecord fields ->
            List.iter
              (fun field -> check_compile_time_type_expr ctf_env field.v.ftype)
              fields
        | Ast.TFunc { params; returns } ->
            List.iter
              (fun param -> check_compile_time_type_expr ctf_env param.v.ptype)
              params;
            (match returns with
            | None -> ()
            | Some r -> check_compile_time_type_expr ctf_env r)
      and check_compile_time_dim (ctf_env : Jovial_compile_time.env)
          (e : Ast.expr Ast.node) : unit =
        if is_open_table_dim e then ()
        else
          match e.v with
          | Ast.ERange { lo; hi } ->
              emit_compile_time_required ctf_env lo;
              emit_compile_time_required ctf_env hi;
              check_simple_dim_bounds ctf_env e
          | _ ->
              emit_compile_time_required ctf_env e;
              check_simple_dim_bounds ctf_env e
      in

      let rec check_type_import_hints (scp : sem_scope)
          (t : Ast.type_expr Ast.node) : unit =
        match t.v with
        | Ast.TName id ->
            let k = normalize_name id.v in
            if
              k <> ""
              && k <> "__IMPLICIT__"
              && k <> "__UNTYPED_POINTER__"
              && (not (sem_is_builtin_type k))
              && not (Hashtbl.mem scp.types k)
            then
              if maybe_visible_through_import ~is_type:true ~name:id.v then ()
              else if has_import_hint ~is_type:true id.v then
                suggest_missing_import ~loc:id.loc ~kind:"Type" ~is_type:true
                  ~symbol:id.v
              else
                emit id.loc (Printf.sprintf "Undefined type %S." id.v)
        | Ast.TScalar _ -> ()
        | Ast.TPointer inner -> check_type_import_hints scp inner
        | Ast.TArray { elem; _ } | Ast.TSpecifiedTable { elem; _ } ->
            check_type_import_hints scp elem
        | Ast.TStatus _ -> ()
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

      let rec sem_field_ty_in (field_name : string) (ty : sem_ty) :
          sem_ty option =
        match ty with
        | TyRecord fields -> sem_find_record_field fields field_name
        | TyArray (TyRecord fields) -> sem_find_record_field fields field_name
        | TyArray inner -> sem_field_ty_in field_name inner
        | _ -> None
      in

      let emit_function_result_mismatch ~(loc : Ast.Loc.t)
          ~(proc_name : string) ~(expected : sem_ty) ~(provided : sem_ty) : unit
          =
        if not (sem_compatible expected provided) then
          emit loc
            (Printf.sprintf
               "Function %S result type mismatch: expected %s, provided %s."
               proc_name
               (sem_ty_to_mismatch_string expected)
               (sem_ty_to_mismatch_string provided))
      in

      let rec ty_of_expr (scp : sem_scope) (current_proc : sem_proc_ctx option)
          ?(status_atom = false) ?(value_context = true)
          (e : Ast.expr Ast.node) : sem_ty =
        match e.v with
        | Ast.EError _ | Ast.EMissing _ -> TyUnknown
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
                  if maybe_visible_through_import ~is_type:false ~name:id.v
                  then ()
                  else if
                    emit_provisional_cross_module_unresolved ~is_type:false
                      ~name:id.v ~loc:id.loc
                      ~message:
                        (Printf.sprintf "Undefined identifier %S." id.v)
                  then ()
                  else if has_import_hint ~is_type:false id.v then
                    suggest_missing_import ~loc:id.loc ~kind:"Identifier"
                      ~is_type:false ~symbol:id.v
                  else
                    emit_authoritative_unresolved ~is_type:false ~name:id.v
                      ~loc:id.loc
                      ~message:
                        (Printf.sprintf "Undefined identifier %S." id.v);
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
              let actual = List.length args in
              let emit_arity expected =
                emit callee.loc
                  (Printf.sprintf
                     "Built-in function %S expects %d argument%s, got %d."
                     callee.v expected
                     (if expected = 1 then "" else "s")
                     actual)
              in
              let expect_arity expected =
                if actual <> expected then (
                  emit_arity expected;
                  false)
                else true
              in
              let emit_arg_type arg index expected got =
                if got <> TyUnknown then
                  emit arg.loc
                    (Printf.sprintf
                       "Built-in function %S argument %d expects %s, got %s."
                       callee.v index expected (sem_ty_to_mismatch_string got))
              in
              let check_arg arg index expected pred ty =
                if not (pred ty) then
                  emit_arg_type arg index expected ty
              in
              let ty_of_type_name_arg arg =
                match arg.v with
                | Ast.EName id -> (
                    match Hashtbl.find_opt scp.types (normalize_name id.v) with
                    | Some ty -> sem_ty_of_type_expr scp.types ty
                    | None -> ty_of_expr scp current_proc arg)
                | _ -> ty_of_expr scp current_proc arg
              in
              let ty_of_loc_arg arg =
                match arg.v with
                | Ast.EName id -> (
                    match sem_lookup_value scp id.v with
                    | Some (SVVar ty) | Some (SVConst ty) -> ty
                    | Some (SVProc _) -> TyUnknown
                    | None -> ty_of_expr scp current_proc arg)
                | _ -> ty_of_expr scp current_proc arg
              in
              let check_int_arg arg index =
                let ty = ty_of_expr scp current_proc arg in
                check_arg arg index "integer" sem_is_integer ty
              in
              let rec is_rep_reference_arg (arg : Ast.expr Ast.node) : bool =
                match arg.v with
                | Ast.EName _ | Ast.EIndex _ | Ast.EField _ | Ast.EAt _
                | Ast.EDeref _ ->
                    true
                | Ast.EParen inner -> is_rep_reference_arg inner
                | Ast.ELit _ | Ast.EUnop _ | Ast.EBinop _ | Ast.ECall _
                | Ast.EConvert _ | Ast.EPreset _ | Ast.EOmitted
                | Ast.ERepeat _ | Ast.EPositioned _ | Ast.ERange _
                | Ast.EError _ | Ast.EMissing _ ->
                    false
              in
              match (ck, args) with
              | "LOC", [ a0 ] ->
                  let target_ty = ty_of_loc_arg a0 in
                  (match target_ty with
                  | TyUnknown -> TyPointer None
                  | _ -> TyPointer (Some target_ty))
              | "LOC", _ ->
                  ignore (expect_arity 1);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyPointer None
              | "NEXT", [ value; increment ] ->
                  let value_ty = ty_of_expr scp current_proc value in
                  check_int_arg increment 2;
                  let scalar = sem_scalarize value_ty in
                  if sem_is_status scalar then TyStatus
                  else
                    (match scalar with
                    | TyPointer _ -> scalar
                    | TyUnknown -> TyUnknown
                    | _ ->
                        emit_arg_type value 1 "pointer or status" value_ty;
                        TyUnknown)
              | "NEXT", _ ->
                  ignore (expect_arity 2);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyUnknown
              | ("BIT" | "BYTE"), [ source; first; length ] ->
                  let source_ty = ty_of_expr scp current_proc source in
                  check_int_arg first 2;
                  check_int_arg length 3;
                  let scalar = sem_scalarize source_ty in
                  if ck = "BIT" then (
                    check_arg source 1 "bit" sem_is_bit scalar;
                    if sem_is_bit scalar then source_ty else TyUnknown)
                  else (
                    check_arg source 1 "character" sem_is_character scalar;
                    if sem_is_character scalar then source_ty else TyUnknown)
              | ("BIT" | "BYTE"), _ ->
                  ignore (expect_arity 3);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyUnknown
              | ("SHIFTL" | "SHIFTR"), [ value; count ] ->
                  let value_ty = ty_of_expr scp current_proc value in
                  check_int_arg count 2;
                  let scalar = sem_scalarize value_ty in
                  check_arg value 1 "bit" sem_is_bit scalar;
                  if sem_is_bit scalar then value_ty else TyUnknown
              | ("SHIFTL" | "SHIFTR"), _ ->
                  ignore (expect_arity 2);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyUnknown
              | "REP", [ source ] ->
                  ignore (ty_of_expr scp current_proc source);
                  if not (is_rep_reference_arg source) then
                    emit source.loc
                      "Built-in conversion REP expects a named variable or data reference.";
                  TyBit
              | "REP", _ ->
                  ignore (expect_arity 1);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyBit
              | "ABS", [ value ] ->
                  let value_ty = ty_of_expr scp current_proc value in
                  check_arg value 1 "numeric" sem_is_numeric
                    (sem_scalarize value_ty);
                  if sem_is_numeric (sem_scalarize value_ty) then value_ty
                  else TyUnknown
              | "ABS", _ ->
                  ignore (expect_arity 1);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyUnknown
              | "SGN", [ value ] ->
                  let value_ty = ty_of_expr scp current_proc value in
                  check_arg value 1 "numeric" sem_is_numeric
                    (sem_scalarize value_ty);
                  TyInt
              | "SGN", _ ->
                  ignore (expect_arity 1);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyInt
              | ("BITSIZE" | "BYTESIZE" | "WORDSIZE"), [ value ] ->
                  ignore (ty_of_type_name_arg value);
                  TyInt
              | ("BITSIZE" | "BYTESIZE" | "WORDSIZE"), _ ->
                  ignore (expect_arity 1);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyInt
              | ("LBOUND" | "UBOUND"), [ table; dim ] ->
                  let table_ty = ty_of_expr scp current_proc table in
                  check_arg table 1 "table" sem_is_table table_ty;
                  check_int_arg dim 2;
                  TyInt
              | ("LBOUND" | "UBOUND"), _ ->
                  ignore (expect_arity 2);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyInt
              | "NWDSEN", [ table ] ->
                  let table_ty = ty_of_type_name_arg table in
                  check_arg table 1 "table" sem_is_table table_ty;
                  TyInt
              | "NWDSEN", _ ->
                  ignore (expect_arity 1);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyInt
              | ("FIRST" | "LAST"), [ value ] ->
                  let value_ty = ty_of_type_name_arg value in
                  check_arg value 1 "status" sem_is_status value_ty;
                  if sem_is_status (sem_scalarize value_ty) then TyStatus
                  else TyUnknown
              | ("FIRST" | "LAST"), _ ->
                  ignore (expect_arity 1);
                  List.iter
                    (fun a -> ignore (ty_of_expr scp current_proc a))
                    args;
                  TyStatus
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
                      let expected_count = List.length pts in
                      let actual_count = List.length args in
                      if
                        sig_.external_modifier <> Ast.RefDecl
                        && actual_count <> expected_count
                      then
                        emit callee.loc
                          (Printf.sprintf
                             "Argument count mismatch in call to %S: expected \
                              %d, provided %d."
                             callee.v expected_count actual_count);
                      let rec check_pairs ps xs =
                        match (ps, xs) with
                        | pty :: pst, arg :: xst ->
                            let aty = ty_of_expr scp current_proc arg in
                            if not (sem_compatible pty aty) then
                              emit arg.loc
                                (Printf.sprintf
                                   "Argument type mismatch in call to %S: \
                                    expected %s, provided %s."
                                   callee.v
                                   (sem_ty_to_mismatch_string pty)
                                   (sem_ty_to_mismatch_string aty));
                            check_pairs pst xst
                        | _, [] -> ()
                        | [], xst ->
                            List.iter
                              (fun a -> ignore (ty_of_expr scp current_proc a))
                              xst
                      in
                      check_pairs pts args);
                  (match sig_.ret_ty with
                  | Some rt -> rt
                  | None ->
                      if value_context then
                        emit callee.loc
                          (Printf.sprintf
                             "%S is a procedure and cannot be used as a value."
                             callee.v);
                      TyUnknown))
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
                  if sem_is_asm_proc callee.v then ()
                  else if maybe_visible_through_import ~is_type:false ~name:callee.v
                  then ()
                  else if
                    emit_provisional_cross_module_unresolved ~is_type:false
                      ~name:callee.v ~loc:callee.loc
                      ~message:
                        (Printf.sprintf
                           "Undefined procedure %S. Declare it with REF PROC \
                            %S in scope."
                           callee.v callee.v)
                  then ()
                  else if has_import_hint ~is_type:false callee.v then
                    suggest_missing_import ~loc:callee.loc ~kind:"Procedure"
                      ~is_type:false ~symbol:callee.v
                  else
                    emit_authoritative_unresolved ~is_type:false ~name:callee.v
                      ~loc:callee.loc
                      ~message:
                        (Printf.sprintf
                           "Undefined procedure %S. Declare it with REF PROC \
                            %S in scope."
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
        | Ast.EConvert { ty; expr } ->
            ignore (ty_of_expr scp current_proc expr);
            sem_ty_of_type_expr scp.types ty
        | Ast.EPreset { base; items } ->
            ignore (ty_of_expr scp current_proc base);
            List.iter (fun i -> ignore (ty_of_expr scp current_proc i)) items;
            TyUnknown
        | Ast.EOmitted -> TyUnknown
        | Ast.ERepeat { count; items } ->
            ignore (ty_of_expr scp current_proc count);
            List.iter (fun i -> ignore (ty_of_expr scp current_proc i)) items;
            TyUnknown
        | Ast.EPositioned { indexes; values } ->
            List.iter (fun i -> ignore (ty_of_expr scp current_proc i)) indexes;
            List.iter (fun v -> ignore (ty_of_expr scp current_proc v)) values;
            TyUnknown
        | Ast.ERange { lo; hi } ->
            ignore (ty_of_expr scp current_proc lo);
            ignore (ty_of_expr scp current_proc hi);
            TyUnknown
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
                    if has_import_hint ~is_type:false id.v then ()
                    else
                      emit id.loc
                        (Printf.sprintf "Unknown field %S for @ access." id.v);
                    TyUnknown))
        | Ast.EDeref { ptr } ->
            let pt = ty_of_expr scp current_proc ptr in
            sem_deref_target ~ptr_loc:ptr.loc pt
        | Ast.EParen inner -> ty_of_expr scp current_proc inner
      in

      let emit_typecheck_issues (issues : Jovial_typecheck.issue list) : unit =
        List.iter
          (fun issue ->
            Jovial_typecheck.diagnostic issue
            |> DiagAuth.local_semantic |> emit_diag)
          issues
      in

      let status_owners_for_value (name : string) : Jovial_status.owner list =
        match Hashtbl.find_opt status_owners_by_value (normalize_name name) with
        | None -> []
        | Some owners -> owners
      in

      let rec status_owner_of_jovial_type (ty : Jovial_type.t) :
          Jovial_status.owner option =
        match ty with
        | Jovial_type.Status { values = [] } -> None
        | Jovial_type.Status { values } ->
            let ast_values =
              values
              |> List.map (fun (value : Jovial_type.status_value) ->
                     let loc = Option.value value.loc ~default:Ast.Loc.none in
                     Ast.node ~loc
                       {
                         Ast.sv_name = Ast.node ~loc value.name;
                         sv_representation = None;
                       })
            in
            Some
              {
                Jovial_status.owner_name = None;
                owner_key = None;
                owner_loc = None;
                values =
                  List.mapi
                    (fun ordinal (sv : Ast.status_value Ast.node) ->
                      {
                        Jovial_status.name = sv.v.sv_name.v;
                        key = normalize_name sv.v.sv_name.v;
                        loc = sv.v.sv_name.loc;
                        ordinal;
                        representation = None;
                      })
                    ast_values;
              }
        | Jovial_type.Table { entry; _ } -> status_owner_of_jovial_type entry
        | _ -> None
      in

      let status_expected_membership expected id =
        match status_owner_of_jovial_type expected with
        | None -> `Unknown
        | Some owner ->
            if Jovial_status.value_is_member owner id.Ast.v then `Member owner
            else `NotMember owner
      in

      let emit_ambiguous_status_if_needed (id : Ast.ident) =
        match status_owners_for_value id.v with
        | [] | [ _ ] -> ()
        | owners ->
            let names =
              owners
              |> List.map Jovial_status.owner_display
              |> List.sort_uniq String.compare
              |> String.concat ", "
            in
            emit_warning id.loc
              (Printf.sprintf
                 "Ambiguous status constant V(%s): the value appears in \
                  multiple STATUS lists (%s), and this expression has no \
                  known status context."
                 id.v names)
      in

      let check_status_value_expr ?expected (e : Ast.expr Ast.node) : unit =
        match Jovial_status.status_constructor_arg e with
        | None -> ()
        | Some id -> (
            match expected with
            | Some expected -> (
                match status_expected_membership expected id with
                | `Member _ -> ()
                | `NotMember owner ->
                    emit id.loc
                      (Printf.sprintf
                         "Status value V(%s) is not a member of expected \
                          status type %s."
                         id.v (Jovial_status.owner_display owner))
                | `Unknown -> emit_ambiguous_status_if_needed id)
            | None -> emit_ambiguous_status_if_needed id)
      in

      List.iter
        (fun (owner : Jovial_status.owner) ->
          List.iter
            (fun ((first : Jovial_status.value), (dup : Jovial_status.value)) ->
              emit dup.Jovial_status.loc
                (Printf.sprintf
                   "Duplicate status value V(%s) in STATUS list for %s; first \
                    declared at line %d."
                   dup.name (Jovial_status.owner_display owner)
                   first.loc.Ast.Loc.start_pos.line))
            (Jovial_status.duplicate_values owner))
        status_owners;

      let constant_table_keys : (string, Ast.ident) Hashtbl.t =
        Hashtbl.create 32
      in
      let readonly_data_keys : (string, Ast.ident) Hashtbl.t =
        Hashtbl.create 32
      in
      let rec collect_constant_table_decl (d : Ast.decl Ast.node) : unit =
        match d.v with
        | Ast.DError _ -> ()
        | Ast.DConst { name; data_decl_kind = Ast.DataTable; _ } ->
            let key = normalize_name name.v in
            if key <> "" then Hashtbl.replace constant_table_keys key name
        | Ast.DVar { name; is_readonly = true; _ } ->
            let key = normalize_name name.v in
            if key <> "" then Hashtbl.replace readonly_data_keys key name
        | Ast.DProc p ->
            List.iter collect_constant_table_decl p.v.locals;
            collect_constant_table_stmt p.v.body
        | Ast.DVar _ | Ast.DConst _ | Ast.DType _ | Ast.DOverlay _
        | Ast.DDirective _ ->
            ()
      and collect_constant_table_stmt (s : Ast.stmt Ast.node) : unit =
        match s.v with
        | Ast.SDecl d -> collect_constant_table_decl d
        | Ast.SBlock xs -> List.iter collect_constant_table_stmt xs
        | Ast.SIf { then_; else_; _ } ->
            collect_constant_table_stmt then_;
            Option.iter collect_constant_table_stmt else_
        | Ast.SWhile { body; _ } -> collect_constant_table_stmt body
        | Ast.SFor { init; step; body; _ } ->
            Option.iter collect_constant_table_stmt init;
            Option.iter collect_constant_table_stmt step;
            collect_constant_table_stmt body
        | Ast.SCase { options; _ } ->
            List.iter
              (fun (opt : Ast.case_option Ast.node) ->
                collect_constant_table_stmt opt.v.case_body)
              options
        | Ast.SLabel { body; _ } -> collect_constant_table_stmt body
        | Ast.SEmpty | Ast.SError _ | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _
        | Ast.SGoto _ ->
            ()
      in
      let rec collect_constant_table_top = function
        | Ast.TopDecl d -> collect_constant_table_decl d
        | Ast.TopStmt s -> collect_constant_table_stmt s
        | Ast.TopModule m ->
            List.iter collect_constant_table_top m.v.module_items
        | Ast.TopError _ -> ()
      in
      List.iter collect_constant_table_top prog;

      let rec constant_table_lvalue (e : Ast.expr Ast.node) : Ast.ident option =
        match e.v with
        | Ast.EName id | Ast.ECall { callee = id; _ } ->
            if Hashtbl.mem constant_table_keys (normalize_name id.v) then
              Some id
            else None
        | Ast.EIndex { base; _ } | Ast.EField { base; _ } ->
            constant_table_lvalue base
        | Ast.EAt { field; _ } -> constant_table_lvalue field
        | Ast.EParen inner -> constant_table_lvalue inner
        | Ast.ELit _ | Ast.EUnop _ | Ast.EBinop _ | Ast.EConvert _
        | Ast.EPreset _ | Ast.EOmitted | Ast.ERepeat _ | Ast.EPositioned _
        | Ast.ERange _ | Ast.EDeref _ | Ast.EError _ | Ast.EMissing _ ->
            None
      in

      let rec readonly_data_lvalue (e : Ast.expr Ast.node) :
          Ast.ident option =
        match e.v with
        | Ast.EName id | Ast.ECall { callee = id; _ } ->
            if Hashtbl.mem readonly_data_keys (normalize_name id.v) then
              Some id
            else None
        | Ast.EIndex { base; _ } | Ast.EField { base; _ } ->
            readonly_data_lvalue base
        | Ast.EAt { field; _ } -> readonly_data_lvalue field
        | Ast.EParen inner -> readonly_data_lvalue inner
        | Ast.ELit _ | Ast.EUnop _ | Ast.EBinop _ | Ast.EConvert _
        | Ast.EPreset _ | Ast.EOmitted | Ast.ERepeat _ | Ast.EPositioned _
        | Ast.ERange _ | Ast.EDeref _ | Ast.EError _ | Ast.EMissing _ ->
            None
      in

      let rec rich_subscript_value (ty : Jovial_type.t) (count : int) :
          Jovial_type.t =
        if count <= 0 then ty
        else
          match ty with
          | Jovial_type.Table { entry; _ } -> rich_subscript_value entry (count - 1)
          | Jovial_type.Pointer { target = Some target; typed } ->
              let target = rich_subscript_value target count in
              Jovial_type.Pointer { target = Some target; typed }
          | _ -> Jovial_type.Unknown
      in

      let rec rich_ty_of_expr (rscp : rich_scope)
          (current_proc : sem_proc_ctx option) (e : Ast.expr Ast.node) :
          Jovial_type.t =
        match e.v with
        | Ast.EError _ | Ast.EMissing _ -> Jovial_type.Unknown
        | Ast.ELit lit -> Jovial_typecheck.literal_type lit
        | Ast.EName id -> (
            match rich_lookup_value rscp id.v with
            | Some (RichVar ty) | Some (RichConst ty) -> ty
            | Some (RichProc sig_) -> (
                match current_proc with
                | Some cp when normalize_name id.v = cp.proc_key -> (
                    match sig_.rich_ret_ty with
                    | Some ty -> ty
                    | None -> Jovial_type.Unknown)
                | _ -> Jovial_type.Unknown)
            | None -> Jovial_type.Unknown)
        | Ast.EUnop { op; rhs } ->
            let rhs_ty = rich_ty_of_expr rscp current_proc rhs in
            let result = Jovial_typecheck.unary_result ~op ~rhs:rhs_ty ~loc:e.loc in
            emit_typecheck_issues result.issues;
            result.ty
        | Ast.EBinop { op; lhs; rhs } ->
            let lhs_ty = rich_ty_of_expr rscp current_proc lhs in
            let rhs_ty = rich_ty_of_expr rscp current_proc rhs in
            let result =
              Jovial_typecheck.binary_result ~op ~lhs:lhs_ty ~rhs:rhs_ty
                ~loc:e.loc
            in
            emit_typecheck_issues result.issues;
            result.ty
        | Ast.ECall { callee; args } -> (
            let ck = normalize_name callee.v in
            let signed_int =
              Jovial_type.Integer { kind = Jovial_type.Signed; bits = None }
            in
            let signed_one_bit =
              Jovial_type.Integer { kind = Jovial_type.Signed; bits = Some 1 }
            in
            let rec rich_scalarize = function
              | Jovial_type.Table { entry; _ } -> rich_scalarize entry
              | ty -> ty
            in
            let rich_is_unknown = function
              | Jovial_type.Unknown | Jovial_type.Named _ -> true
              | _ -> false
            in
            let rich_is_numeric = function
              | Jovial_type.Integer _ | Jovial_type.Float _ | Jovial_type.Fixed _ ->
                  true
              | _ -> false
            in
            let rich_is_bit = function
              | Jovial_type.BitString _ -> true
              | _ -> false
            in
            let rich_is_character = function
              | Jovial_type.CharString _ -> true
              | _ -> false
            in
            let rich_is_status = function
              | Jovial_type.Status _ -> true
              | _ -> false
            in
            let rich_ty_of_type_name_arg arg =
              match arg.v with
              | Ast.EName id -> (
                  match Hashtbl.find_opt rscp.rich_types (normalize_name id.v) with
                  | Some ty -> rich_ty_of_type_expr rscp ty
                  | None -> rich_ty_of_expr rscp current_proc arg)
              | _ -> rich_ty_of_expr rscp current_proc arg
            in
            let int_arg_value arg =
              match arg.v with
              | Ast.ELit (Ast.LInt raw) -> int_of_string_opt raw
              | _ -> None
            in
            let dim_has_status_bound (dim : Jovial_type.dim) =
              match (dim.lower, dim.upper) with
              | Some (Jovial_type.BoundStatus _), _
              | _, Some (Jovial_type.BoundStatus _) ->
                  true
              | _ -> false
            in
            let bounds_result table_ty dim_arg =
              match (table_ty, int_arg_value dim_arg) with
              | Jovial_type.Table { dims; _ }, Some index -> (
                  match List.nth_opt dims index with
                  | Some dim when dim_has_status_bound dim ->
                      Jovial_type.Status { values = [] }
                  | _ -> signed_int)
              | _ -> signed_int
            in
            if ck = "V" then (
              match args with
              | [ { v = Ast.EName id; _ } ] ->
                  Jovial_type.Status
                    {
                      values =
                        [
                          {
                            Jovial_type.name = id.v;
                            loc = Some id.loc;
                            representation = None;
                          };
                        ];
                    }
              | _ ->
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    args;
                  Jovial_type.Status { values = [] })
            else if ck = "LOC" then
              match args with
              | arg :: rest ->
                  let target = rich_ty_of_expr rscp current_proc arg in
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    rest;
                  let target =
                    if rich_is_unknown target then None else Some target
                  in
                  Jovial_type.Pointer { target; typed = Option.is_some target }
              | [] -> Jovial_type.Pointer { target = None; typed = false }
            else if ck = "NEXT" then
              match args with
              | value :: rest ->
                  let value_ty = rich_ty_of_expr rscp current_proc value in
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    rest;
                  (match rich_scalarize value_ty with
                  | Jovial_type.Pointer _ -> value_ty
                  | Jovial_type.Status _ -> value_ty
                  | _ -> Jovial_type.Unknown)
              | [] -> Jovial_type.Unknown
            else if ck = "BIT" then
              match args with
              | source :: rest ->
                  let source_ty = rich_ty_of_expr rscp current_proc source in
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    rest;
                  if rich_is_bit (rich_scalarize source_ty) then source_ty
                  else Jovial_type.Unknown
              | [] -> Jovial_type.Unknown
            else if ck = "BYTE" then
              match args with
              | source :: rest ->
                  let source_ty = rich_ty_of_expr rscp current_proc source in
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    rest;
                  if rich_is_character (rich_scalarize source_ty) then source_ty
                  else Jovial_type.Unknown
              | [] -> Jovial_type.Unknown
            else if ck = "SHIFTL" || ck = "SHIFTR" then
              match args with
              | source :: rest ->
                  let source_ty = rich_ty_of_expr rscp current_proc source in
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    rest;
                  if rich_is_bit (rich_scalarize source_ty) then source_ty
                  else Jovial_type.Unknown
              | [] -> Jovial_type.Unknown
            else if ck = "REP" then (
              List.iter
                (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                args;
              Jovial_type.BitString { bits = None })
            else if ck = "ABS" then
              match args with
              | source :: rest ->
                  let source_ty = rich_ty_of_expr rscp current_proc source in
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    rest;
                  if rich_is_numeric (rich_scalarize source_ty) then source_ty
                  else Jovial_type.Unknown
              | [] -> Jovial_type.Unknown
            else if ck = "SGN" then (
              List.iter
                (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                args;
              signed_one_bit)
            else if ck = "BITSIZE" || ck = "BYTESIZE" || ck = "WORDSIZE" then (
              List.iter
                (fun a -> ignore (rich_ty_of_type_name_arg a))
                args;
              signed_int)
            else if ck = "LBOUND" || ck = "UBOUND" then
              match args with
              | table :: dim :: rest ->
                  let table_ty = rich_ty_of_expr rscp current_proc table in
                  ignore (rich_ty_of_expr rscp current_proc dim);
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    rest;
                  bounds_result table_ty dim
              | _ ->
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    args;
                  signed_int
            else if ck = "NWDSEN" then (
              List.iter
                (fun a -> ignore (rich_ty_of_type_name_arg a))
                args;
              signed_int)
            else if ck = "FIRST" || ck = "LAST" then
              match args with
              | value :: rest ->
                  let value_ty = rich_ty_of_type_name_arg value in
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    rest;
                  if rich_is_status (rich_scalarize value_ty) then value_ty
                  else Jovial_type.Unknown
              | [] -> Jovial_type.Status { values = [] }
            else
              match rich_lookup_value rscp callee.v with
              | Some (RichProc sig_) ->
                  (match sig_.rich_param_tys with
                  | None -> ()
                  | Some param_tys ->
                      let rec check_pairs ps xs =
                        match (ps, xs) with
                        | pty :: pst, arg :: xst ->
                            let aty = rich_ty_of_expr rscp current_proc arg in
                            check_status_value_expr ~expected:pty arg;
                            emit_typecheck_issues
                              (Jovial_typecheck.assignment_issues ~lhs:pty
                                 ~rhs:aty ~loc:arg.loc);
                            check_pairs pst xst
                        | _, [] -> ()
                        | [], extra ->
                            List.iter
                              (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                              extra
                      in
                      check_pairs param_tys args);
                  Option.value sig_.rich_ret_ty ~default:Jovial_type.Unknown
              | Some (RichVar ty) | Some (RichConst ty) ->
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    args;
                  if args = [] then Jovial_type.Unknown
                  else rich_subscript_value ty (List.length args)
              | None ->
                  List.iter
                    (fun a -> ignore (rich_ty_of_expr rscp current_proc a))
                    args;
                  Jovial_type.Unknown)
        | Ast.EIndex { base; index } ->
            let base_ty = rich_ty_of_expr rscp current_proc base in
            List.iter
              (fun i -> ignore (rich_ty_of_expr rscp current_proc i))
              index;
            rich_subscript_value base_ty (List.length index)
        | Ast.EField { base; field } -> (
            let base_ty = rich_ty_of_expr rscp current_proc base in
            match Jovial_type.field_type base_ty field.v with
            | Some ty -> ty
            | None -> Jovial_type.Unknown)
        | Ast.EConvert { ty; expr } ->
            let target = rich_ty_of_type_expr rscp ty in
            let source = rich_ty_of_expr rscp current_proc expr in
            emit_typecheck_issues
              (Jovial_typecheck.conversion_issues ~target ~source ~loc:e.loc);
            target
        | Ast.EPreset { base; items } ->
            ignore (rich_ty_of_expr rscp current_proc base);
            List.iter
              (fun item -> ignore (rich_ty_of_expr rscp current_proc item))
              items;
            Jovial_type.Unknown
        | Ast.EOmitted -> Jovial_type.Unknown
        | Ast.ERepeat { count; items } ->
            ignore (rich_ty_of_expr rscp current_proc count);
            List.iter
              (fun item -> ignore (rich_ty_of_expr rscp current_proc item))
              items;
            Jovial_type.Unknown
        | Ast.EPositioned { indexes; values } ->
            List.iter
              (fun index -> ignore (rich_ty_of_expr rscp current_proc index))
              indexes;
            List.iter
              (fun value -> ignore (rich_ty_of_expr rscp current_proc value))
              values;
            Jovial_type.Unknown
        | Ast.ERange { lo; hi } ->
            ignore (rich_ty_of_expr rscp current_proc lo);
            ignore (rich_ty_of_expr rscp current_proc hi);
            Jovial_type.Unknown
        | Ast.EAt { field; ptr } ->
            let ptr_ty = rich_ty_of_expr rscp current_proc ptr in
            (match field.v with
            | Ast.EName id -> (
                let container_ty =
                  match Jovial_type.field_type ptr_ty id.v with
                  | Some _ -> ptr_ty
                  | None ->
                      let result =
                        Jovial_typecheck.dereference_result ~ptr:ptr_ty
                          ~loc:ptr.loc
                      in
                      emit_typecheck_issues result.issues;
                      result.ty
                in
                match Jovial_type.field_type container_ty id.v with
                | Some ty -> ty
                | None -> Jovial_type.Unknown)
            | Ast.EIndex { base = { v = Ast.EName id; _ }; index } ->
                List.iter
                  (fun i -> ignore (rich_ty_of_expr rscp current_proc i))
                  index;
                let container_ty =
                  match Jovial_type.field_type ptr_ty id.v with
                  | Some _ -> ptr_ty
                  | None ->
                      let result =
                        Jovial_typecheck.dereference_result ~ptr:ptr_ty
                          ~loc:ptr.loc
                      in
                      emit_typecheck_issues result.issues;
                      result.ty
                in
                let field_ty =
                  match Jovial_type.field_type container_ty id.v with
                  | Some ty -> ty
                  | None -> Jovial_type.Unknown
                in
                rich_subscript_value field_ty (List.length index)
            | _ ->
                ignore (rich_ty_of_expr rscp current_proc field);
                Jovial_type.Unknown)
        | Ast.EDeref { ptr } ->
            let ptr_ty = rich_ty_of_expr rscp current_proc ptr in
            let result =
              Jovial_typecheck.dereference_result ~ptr:ptr_ty ~loc:ptr.loc
            in
            emit_typecheck_issues result.issues;
            result.ty
        | Ast.EParen inner -> rich_ty_of_expr rscp current_proc inner
      in

      let rich_ty_of_lvalue (rscp : rich_scope)
          (current_proc : sem_proc_ctx option) (e : Ast.expr Ast.node) :
          Jovial_type.t =
        let rec scalarize = function
          | Jovial_type.Table { entry; _ } -> scalarize entry
          | ty -> ty
        in
        let length_arg_size = function
          | { Ast.v = Ast.ELit (Ast.LInt raw); _ } -> int_of_string_opt raw
          | _ -> None
        in
        match e.v with
        | Ast.ECall { callee; args = [ source ] }
          when normalize_name callee.v = "REP" ->
            ignore (rich_ty_of_expr rscp current_proc source);
            Jovial_type.BitString { bits = None }
        | Ast.ECall { callee; args = [ source; _first; length ] } -> (
            match normalize_name callee.v with
            | "BIT" ->
                let source_ty = rich_ty_of_expr rscp current_proc source in
                if
                  match scalarize source_ty with
                  | Jovial_type.BitString _ -> true
                  | Jovial_type.Unknown | Jovial_type.Named _ -> false
                  | _ -> false
                then Jovial_type.BitString { bits = length_arg_size length }
                else Jovial_type.Unknown
            | "BYTE" ->
                let source_ty = rich_ty_of_expr rscp current_proc source in
                if
                  match scalarize source_ty with
                  | Jovial_type.CharString _ -> true
                  | Jovial_type.Unknown | Jovial_type.Named _ -> false
                  | _ -> false
                then Jovial_type.CharString { chars = length_arg_size length }
                else Jovial_type.Unknown
            | _ -> rich_ty_of_expr rscp current_proc e)
        | _ -> rich_ty_of_expr rscp current_proc e
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
                if maybe_visible_through_import ~is_type:false ~name:id.v then
                  ()
                else if
                  emit_provisional_cross_module_unresolved ~is_type:false
                    ~name:id.v ~loc:id.loc
                    ~message:(Printf.sprintf "Undefined item %S." id.v)
                then ()
                else if has_import_hint ~is_type:false id.v then
                  suggest_missing_import ~loc:id.loc ~kind:"Item" ~is_type:false
                    ~symbol:id.v
                else
                  emit_authoritative_unresolved ~is_type:false ~name:id.v
                    ~loc:id.loc
                    ~message:(Printf.sprintf "Undefined item %S." id.v);
                None)
        | Ast.EField _ | Ast.EAt _ | Ast.EDeref _ | Ast.EIndex _ ->
            Some (ty_of_expr scp current_proc e)
        | Ast.ECall { callee; _ }
          when (
            match normalize_name callee.v with
            | "BIT" | "BYTE" | "REP" -> true
            | _ -> false) ->
            Some (ty_of_expr scp current_proc e)
        | _ ->
            ignore (ty_of_expr scp current_proc e);
            None
      in

      let rec table_dims_of_type_expr (rscp : rich_scope)
          (t : Ast.type_expr Ast.node) : Ast.expr Ast.node list option =
        match t.v with
        | Ast.TArray { dims; _ } | Ast.TSpecifiedTable { dims; _ } -> Some dims
        | Ast.TName id -> (
            match Hashtbl.find_opt rscp.rich_types (normalize_name id.v) with
            | Some defn -> table_dims_of_type_expr rscp defn
            | None -> None)
        | _ -> None
      in

      let is_omitted_preset_value (item : Ast.expr Ast.node) : bool =
        match item.v with
        | Ast.EOmitted -> true
        | Ast.ELit (Ast.LString "") -> true
        | _ -> false
      in

      let validate_table_preset (rscp : rich_scope)
          (ctf_env : Jovial_compile_time.env)
          (current_proc : sem_proc_ctx option)
          ~(table_type : Ast.type_expr Ast.node) ~(preset : Ast.expr Ast.node) :
          unit =
        match preset.v with
        | Ast.EPreset { items; _ } ->
            let items =
              match items with
              | [ { v = Ast.ERepeat { count; items = nested }; _ } ] -> (
                  match
                    Jovial_compile_time.eval_expr ~env:ctf_env count
                    |> Jovial_compile_time.int_value
                  with
                  | Some 0L -> nested
                  | _ -> items)
              | _ -> items
            in
            (match table_dims_of_type_expr rscp table_type with
            | Some dims -> (
                match table_entry_capacity ctf_env dims with
                | Some capacity
                  when Int64.of_int (List.length items) > capacity ->
                    emit preset.loc
                      (Printf.sprintf
                         "Table preset has %d positions but table capacity is \
                          %Ld."
                         (List.length items) capacity)
                | _ -> ())
            | None -> ());
            let table_ty = rich_ty_of_type_expr rscp table_type in
            let entry_ty =
              match Jovial_type.table_entry_type table_ty with
              | Some (Jovial_type.Block _) -> None
              | Some entry -> Some entry
              | None -> Some table_ty
            in
            (match entry_ty with
            | None -> ()
            | Some lhs ->
                items
                |> List.filter (fun item -> not (is_omitted_preset_value item))
                |> List.iter (fun item ->
                       let rhs = rich_ty_of_expr rscp current_proc item in
                       emit_typecheck_issues
                         (Jovial_typecheck.assignment_issues ~lhs ~rhs
                            ~loc:item.loc);
                       check_status_value_expr ~expected:lhs item))
        | _ -> ()
      in

      let validate_overlay_decl (scp : sem_scope)
          (ctf_env : Jovial_compile_time.env)
          (overlay : Ast.overlay_decl) : unit =
        let seen_targets : (string, Ast.ident) Hashtbl.t = Hashtbl.create 16 in
        let check_target (id : Ast.ident) =
          let key = normalize_name id.v in
          if key <> "" then (
            (match Hashtbl.find_opt seen_targets key with
            | Some first ->
                emit id.loc
                  (Printf.sprintf
                     "Duplicate OVERLAY target %S; first listed at line %d."
                     id.v first.loc.Ast.Loc.start_pos.line)
            | None -> Hashtbl.add seen_targets key id);
            match sem_lookup_value scp id.v with
            | Some _ -> ()
            | None ->
                emit id.loc
                  (Printf.sprintf
                     "Unknown OVERLAY target %S. Declare the item, table, or block before using it in an OVERLAY."
                     id.v))
        in
        let check_spacer (expr : Ast.expr Ast.node) =
          match
            ctf_int_required_value
              ~message:"Invalid OVERLAY spacer expression" ctf_env expr
          with
          | Some n when n < 0L ->
              emit expr.loc
                (Printf.sprintf
                   "Invalid OVERLAY spacer expression: spacer size must be non-negative, got %Ld."
                   n)
          | _ -> ()
        in
        let rec check_item (item : Ast.overlay_item Ast.node) =
          match item.v with
          | Ast.OverlayTarget id -> check_target id
          | Ast.OverlaySpacer expr -> check_spacer expr
          | Ast.OverlayGroup items -> List.iter check_item items
        in
        (match overlay.overlay_pos with
        | None -> ()
        | Some pos ->
            ignore
              (ctf_int_required_value
                 ~message:"Invalid OVERLAY POS expression" ctf_env pos));
        List.iter check_item overlay.overlay_items
      in

      let add_decl_symbol (scp : sem_scope) (d : Ast.decl Ast.node) : unit =
        match d.v with
        | Ast.DError _ -> ()
        | Ast.DType { name; defn; _ } -> sem_add_type scp name.v defn
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
        | Ast.DOverlay _ | Ast.DDirective _ -> ()
      in

      let rich_add_decl_symbol (rscp : rich_scope) (d : Ast.decl Ast.node) :
          unit =
        match d.v with
        | Ast.DError _ -> ()
        | Ast.DType { name; defn; _ } -> rich_add_type rscp name.v defn
        | Ast.DVar { name; dtype; _ } ->
            rich_add_value rscp name.v
              (RichVar (rich_ty_of_type_expr rscp dtype))
        | Ast.DConst { name; dtype; _ } ->
            let ty =
              match dtype with
              | None -> Jovial_type.Unknown
              | Some t -> rich_ty_of_type_expr rscp t
            in
            rich_add_value rscp name.v (RichConst ty)
        | Ast.DProc p ->
            rich_add_value rscp p.v.name.v
              (RichProc (rich_proc_sig_of_proc rscp p))
        | Ast.DOverlay _ | Ast.DDirective _ -> ()
      in

      let rec collect_label_depths_for_stmt (out : (string, int) Hashtbl.t)
          ~(loop_depth : int) (s : Ast.stmt Ast.node) : unit =
        match s.v with
        | Ast.SEmpty | Ast.SError _ | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _
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
        | Ast.SCase { options; _ } ->
            List.iter
              (fun (opt : Ast.case_option Ast.node) ->
                collect_label_depths_for_stmt out ~loop_depth
                  opt.v.case_body)
              options
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
        let rec collect_top = function
          | Ast.TopStmt s -> collect_label_depths_for_stmt out ~loop_depth:0 s
          | Ast.TopDecl _ | Ast.TopError _ -> ()
          | Ast.TopModule m -> List.iter collect_top m.v.module_items
        in
        List.iter collect_top prog;
        out
      in

      let rec check_stmt (scp : sem_scope) (current_proc : sem_proc_ctx option)
          (rscp : rich_scope) (ctf_env : Jovial_compile_time.env)
          ~(loop_depth : int)
          ~(label_depths : (string, int) Hashtbl.t)
          (s : Ast.stmt Ast.node) : unit =
        match s.v with
        | Ast.SEmpty | Ast.SError _ -> ()
        | Ast.SDecl d ->
            add_decl_symbol scp d;
            rich_add_decl_symbol rscp d;
            register_ctf_decl_symbol ctf_env d;
            check_decl scp current_proc rscp ctf_env ~loop_depth ~label_depths d
        | Ast.SBlock xs ->
            List.iter
              (fun st ->
                check_stmt scp current_proc rscp ctf_env ~loop_depth
                  ~label_depths st)
              xs
        | Ast.SAssign { lhs; rhs } -> (
            (match constant_table_lvalue lhs with
            | None -> ()
            | Some id ->
                emit lhs.loc
                  (Printf.sprintf
                     "Cannot assign to constant table %S; CONSTANT TABLE \
                      declarations are readonly."
                     id.v));
            (match readonly_data_lvalue lhs with
            | None -> ()
            | Some id ->
                emit lhs.loc
                  (Printf.sprintf
                     "Cannot assign to readonly data %S; READONLY \
                      declarations are readonly."
                     id.v));
            let lhs_ty = ty_of_lvalue scp current_proc lhs in
            let rhs_ty = ty_of_expr scp current_proc rhs in
            let lhs_rich_ty = rich_ty_of_lvalue rscp current_proc lhs in
            let rhs_rich_ty = rich_ty_of_expr rscp current_proc rhs in
            emit_typecheck_issues
              (Jovial_typecheck.assignment_issues ~lhs:lhs_rich_ty
                 ~rhs:rhs_rich_ty ~loc:rhs.loc);
            check_status_value_expr ~expected:lhs_rich_ty rhs;
            match lhs_ty with
            | None -> ()
            | Some lt ->
                let is_current_function_result =
                  match (lhs.v, current_proc) with
                  | Ast.EName id, Some cp ->
                      normalize_name id.v = cp.proc_key
                      && Option.is_some cp.proc_ret_ty
                  | _ -> false
                in
                if is_current_function_result then
                  match current_proc with
                  | Some cp ->
                      emit_function_result_mismatch ~loc:rhs.loc
                        ~proc_name:cp.proc_name ~expected:lt ~provided:rhs_ty
                  | None -> ()
                else if not (sem_compatible lt rhs_ty) then
                  emit rhs.loc
                    (Printf.sprintf
                       "Type mismatch in assignment: left is %s, right is %s."
                       (sem_ty_to_mismatch_string lt)
                       (sem_ty_to_mismatch_string rhs_ty)))
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
                    ~value_context:false
                    (Ast.node ~loc:s.loc (Ast.ECall { callee; args })));
               let diag_count_after = List.length !out in
               if diag_count_after = diag_count_before then
                 match sem_lookup_value scp callee.v with
                 | Some _ -> ()
                 | None ->
                     if
                       (not (sem_is_builtin_call callee.v))
                       && not (sem_is_asm_proc callee.v)
                       && not
                            (maybe_visible_through_import ~is_type:false
                               ~name:callee.v)
                       && not
                            (emit_provisional_cross_module_unresolved
                               ~is_type:false ~name:callee.v ~loc:callee.loc
                               ~message:
                                 (Printf.sprintf
                                    "Undefined procedure %S. Declare it with \
                                     REF PROC %S in scope."
                                    callee.v callee.v))
                     then
                       if has_import_hint ~is_type:false callee.v then
                         suggest_missing_import ~loc:callee.loc
                           ~kind:"Procedure" ~is_type:false ~symbol:callee.v
                       else
                         emit_authoritative_unresolved ~is_type:false
                           ~name:callee.v ~loc:callee.loc
                           ~message:
                             (Printf.sprintf
                                "Undefined procedure %S. Declare it with REF \
                                 PROC %S in scope."
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
            ignore (rich_ty_of_expr rscp current_proc cond);
            check_status_value_expr cond;
            check_stmt scp current_proc rscp ctf_env ~loop_depth ~label_depths
              then_;
            match else_ with
            | None -> ()
            | Some e ->
                check_stmt scp current_proc rscp ctf_env ~loop_depth
                  ~label_depths e)
        | Ast.SWhile { cond; body } ->
            ignore (ty_of_expr scp current_proc cond);
            ignore (rich_ty_of_expr rscp current_proc cond);
            check_status_value_expr cond;
            check_stmt scp current_proc rscp ctf_env
              ~loop_depth:(loop_depth + 1) ~label_depths body
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
                check_stmt for_scope current_proc rscp ctf_env ~loop_depth
                  ~label_depths i);
            (match cond with
            | None -> ()
            | Some c ->
                ignore (ty_of_expr for_scope current_proc c);
                ignore (rich_ty_of_expr rscp current_proc c);
                check_status_value_expr c);
            (match step with
            | None -> ()
            | Some st ->
                check_stmt for_scope current_proc rscp ctf_env ~loop_depth
                  ~label_depths st);
            check_stmt for_scope current_proc rscp ctf_env
              ~loop_depth:(loop_depth + 1) ~label_depths body
        | Ast.SCase { selector; options } ->
            ignore (ty_of_expr scp current_proc selector);
            ignore (rich_ty_of_expr rscp current_proc selector);
            check_status_value_expr selector;
            let check_case_index (idx : Ast.case_index Ast.node) : unit =
              match idx.v with
              | Ast.CaseDefault -> ()
              | Ast.CaseValue value ->
                  ignore (ty_of_expr scp current_proc value);
                  ignore (rich_ty_of_expr rscp current_proc value);
                  check_status_value_expr value
              | Ast.CaseRange (lo, hi) ->
                  ignore (ty_of_expr scp current_proc lo);
                  ignore (ty_of_expr scp current_proc hi);
                  ignore (rich_ty_of_expr rscp current_proc lo);
                  ignore (rich_ty_of_expr rscp current_proc hi);
                  check_status_value_expr lo;
                  check_status_value_expr hi
            in
            List.iter
              (fun (opt : Ast.case_option Ast.node) ->
                List.iter check_case_index opt.v.case_indexes;
                check_stmt scp current_proc rscp ctf_env ~loop_depth
                  ~label_depths opt.v.case_body)
              options
        | Ast.SReturn eo -> (
            if current_proc = None then
              emit s.loc "RETURN is only valid inside a procedure.";
            match (current_proc, eo) with
            | _, None -> ()
            | None, Some e ->
                ignore (ty_of_expr scp current_proc e);
                ignore (rich_ty_of_expr rscp current_proc e)
            | Some cp, Some e -> (
                let provided = ty_of_expr scp current_proc e in
                let provided_rich = rich_ty_of_expr rscp current_proc e in
                let expected_rich =
                  match rich_lookup_value rscp cp.proc_name with
                  | Some (RichProc sig_) -> sig_.rich_ret_ty
                  | _ -> None
                in
                (match expected_rich with
                | None -> ()
                | Some expected ->
                    emit_typecheck_issues
                      (Jovial_typecheck.assignment_issues ~lhs:expected
                         ~rhs:provided_rich ~loc:e.loc);
                    check_status_value_expr ~expected e);
                match cp.proc_ret_ty with
                | Some expected ->
                    emit_function_result_mismatch ~loc:e.loc
                      ~proc_name:cp.proc_name ~expected ~provided
                | None -> ()))
        | Ast.SLabel { body; _ } ->
            check_stmt scp current_proc rscp ctf_env ~loop_depth ~label_depths
              body
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
          (rscp : rich_scope) (ctf_env : Jovial_compile_time.env)
          ~(loop_depth : int)
          ~label_depths:(_label_depths : (string, int) Hashtbl.t)
          (d : Ast.decl Ast.node) : unit =
        let _ = loop_depth in
        match d.v with
        | Ast.DError _ -> ()
        | Ast.DVar { dtype; init; data_decl_kind; _ } -> (
            check_type_import_hints scp dtype;
            check_compile_time_type_expr ctf_env dtype;
            match init with
            | None -> ()
            | Some rhs ->
                let lty = sem_ty_of_type_expr scp.types dtype in
                let rty = ty_of_expr scp current_proc rhs in
                let lhs_rich_ty = rich_ty_of_type_expr rscp dtype in
                let rhs_rich_ty = rich_ty_of_expr rscp current_proc rhs in
                emit_typecheck_issues
                  (Jovial_typecheck.assignment_issues ~lhs:lhs_rich_ty
                     ~rhs:rhs_rich_ty ~loc:rhs.loc);
                check_status_value_expr ~expected:lhs_rich_ty rhs;
                if not (sem_compatible lty rty) then
                  emit rhs.loc
                    (Printf.sprintf
                       "Type mismatch in initializer: expected %s, got %s."
                       (sem_ty_to_mismatch_string lty)
                       (sem_ty_to_mismatch_string rty));
                if data_decl_kind = Ast.DataTable then
                  validate_table_preset rscp ctf_env current_proc ~table_type:dtype
                    ~preset:rhs)
        | Ast.DConst { dtype; value; data_decl_kind; _ } ->
            (match dtype with
            | None -> ()
            | Some t ->
                check_type_import_hints scp t;
                check_compile_time_type_expr ctf_env t);
            (match data_decl_kind with
            | Ast.DataTable -> ()
            | _ ->
                emit_compile_time_required
                  ~message:
                    "Compile-time constant expression required for constant item"
                  ctf_env value);
            (match dtype with
            | None -> ()
            | Some t ->
                let lhs_rich_ty = rich_ty_of_type_expr rscp t in
                let rhs_rich_ty = rich_ty_of_expr rscp current_proc value in
                emit_typecheck_issues
                  (Jovial_typecheck.assignment_issues ~lhs:lhs_rich_ty
                     ~rhs:rhs_rich_ty ~loc:value.loc);
                check_status_value_expr ~expected:lhs_rich_ty value;
                if data_decl_kind = Ast.DataTable then
                  validate_table_preset rscp ctf_env current_proc ~table_type:t
                    ~preset:value);
            ignore (ty_of_expr scp current_proc value)
        | Ast.DType { defn; _ } ->
            check_type_import_hints scp defn;
            check_compile_time_type_expr ctf_env defn
        | Ast.DOverlay overlay -> validate_overlay_decl scp ctf_env overlay
        | Ast.DDirective _ -> ()
        | Ast.DProc p ->
            let proc_scope = sem_scope_copy scp in
            let proc_rich_scope = rich_scope_copy rscp in
            let proc_ctf_env = Jovial_compile_time.copy_env ctf_env in
            List.iter
              (fun prm ->
                Jovial_compile_time.add_non_constant proc_ctf_env prm.v.pname.v)
              p.v.params;
            List.iter (register_ctf_decl_symbol proc_ctf_env) p.v.locals;
            let proc_label_depths = collect_label_depths p.v.body in
            List.iter (add_decl_symbol proc_scope) p.v.locals;
            List.iter (rich_add_decl_symbol proc_rich_scope) p.v.locals;
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
              (fun prm ->
                check_type_import_hints proc_scope prm.v.ptype;
                check_compile_time_type_expr proc_ctf_env prm.v.ptype)
              p.v.params;
            (match p.v.returns with
            | None -> ()
            | Some r ->
                check_type_import_hints proc_scope r;
                check_compile_time_type_expr proc_ctf_env r);
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
              (fun prm ->
                rich_add_value proc_rich_scope prm.v.pname.v
                  (RichVar (rich_ty_of_type_expr proc_rich_scope prm.v.ptype)))
              p.v.params;
            List.iter
              (fun pd ->
                check_decl proc_scope proc_ctx proc_rich_scope proc_ctf_env
                  ~loop_depth:0 ~label_depths:proc_label_depths pd)
              p.v.locals;
            check_stmt proc_scope proc_ctx proc_rich_scope proc_ctf_env ~loop_depth:0
              ~label_depths:proc_label_depths p.v.body
      in

      let rec check_top = function
        | Ast.TopDecl d ->
            check_decl scope None rich_scope top_ctf_env ~loop_depth:0
              ~label_depths:top_level_label_depths d
        | Ast.TopStmt s ->
            check_stmt scope None rich_scope top_ctf_env ~loop_depth:0
              ~label_depths:top_level_label_depths s
        | Ast.TopModule m -> List.iter check_top m.v.module_items
        | Ast.TopError _ -> ()
      in
      List.iter check_top prog;
      List.rev !out
  | _ -> []

let validate_semantics (ws : t) (doc : Document.t) : T.Diagnostic.t list =
  validate_semantics_with_authority ws doc |> DiagAuth.to_lsp_list
