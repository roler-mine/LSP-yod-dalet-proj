module T = Lsp.Types
module JT = Jovial_type

open Workspace_state
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_navigation_support

type proc_sig = {
  params : Ast.param Ast.node list;
}

type context = {
  ws : t;
  range : T.Range.t;
  budget : Workspace_budget.t;
  type_env : JT.type_env;
  proc_sigs : (string, proc_sig list) Hashtbl.t;
  mutable hints : T.InlayHint.t list;
}

let inlay_hint_budget_ms = 20

let loc_range (loc : Ast.Loc.t) : T.Range.t = Lsp_conv.range_of_loc loc
let loc_start loc = (loc_range loc).start
let loc_end loc = (loc_range loc).end_

let loc_intersects range loc = range_intersects (loc_range loc) range
let pos_in_requested_range ctx pos = pos_in_range pos ctx.range

let current_program (doc : Document.t) : Ast.program option =
  match Document.current_parse doc with
  | Some { Document.parsed_ast = Some prog; _ } -> Some prog
  | _ -> None

let inlay_label = function
  | `String s -> s
  | `List parts ->
      parts |> List.map (fun (p : T.InlayHintLabelPart.t) -> p.value)
      |> String.concat ""

let hint_data ~source ctx =
  let stopped =
    match Workspace_budget.reason_if_stopped ctx.budget with
    | None -> []
    | Some reason ->
        [
          ("stoppedReason", `String (Workspace_readiness.reason_label reason));
        ]
  in
  `Assoc
    ([
       ("kind", `String "jovial.inlayHint");
       ("source", `String source);
       ("provisional", `Bool (ctx.ws.startup_fully_nav_ready_ms = None));
     ]
    @ stopped)

let add_hint ctx ~(position : T.Position.t) ~(kind : T.InlayHintKind.t)
    ~(label : string) ~(source : string) ?(padding_left = false)
    ?(padding_right = false) () : unit =
  if label <> "" && pos_in_requested_range ctx position then
    let hint =
      T.InlayHint.create ~position ~kind ~label:(`String label)
        ~paddingLeft:padding_left ~paddingRight:padding_right
        ~data:(hint_data ~source ctx)
        ()
    in
    ctx.hints <- hint :: ctx.hints

let suffix_opt ~singular ~plural = function
  | None -> ""
  | Some n -> Printf.sprintf ", %d %s" n (if n = 1 then singular else plural)

let rec type_detail_text (ty : JT.t) : string option =
  match ty with
  | JT.Unknown | JT.Named _ -> None
  | JT.Integer { kind = JT.Unsigned; bits } ->
      Some ("unsigned integer" ^ suffix_opt ~singular:"bit" ~plural:"bits" bits)
  | JT.Integer { kind = JT.Signed; bits } ->
      Some ("signed integer" ^ suffix_opt ~singular:"bit" ~plural:"bits" bits)
  | JT.Float { precision } ->
      Some
        ("floating point"
        ^ suffix_opt ~singular:"bit precision" ~plural:"bits precision"
            precision)
  | JT.Fixed { scale; fraction } ->
      let pieces =
        [
          Option.map (Printf.sprintf "scale %d") scale;
          Option.map (Printf.sprintf "fraction %d") fraction;
        ]
        |> List.filter_map Fun.id
      in
      Some
        (match pieces with
        | [] -> "fixed point"
        | _ -> "fixed point, " ^ String.concat ", " pieces)
  | JT.BitString { bits } ->
      Some ("bit string" ^ suffix_opt ~singular:"bit" ~plural:"bits" bits)
  | JT.CharString { chars } ->
      Some
        ("character string"
        ^ suffix_opt ~singular:"character" ~plural:"characters" chars)
  | JT.Status { values = [] } -> Some "status value"
  | JT.Status { values } ->
      Some
        (Printf.sprintf "status value, %d %s" (List.length values)
           (if List.length values = 1 then "case" else "cases"))
  | JT.Pointer { target = None; _ } -> Some "pointer"
  | JT.Pointer { target = Some target; _ } ->
      Some
        ("pointer to "
        ^
        match type_detail_text target with
        | Some detail -> detail
        | None -> JT.display target)
  | JT.Table { entry; _ } ->
      Some
        ("table of "
        ^
        match type_detail_text entry with
        | Some detail -> detail
        | None -> JT.display entry)
  | JT.Block [] -> Some "block"
  | JT.Block fields ->
      Some
        (Printf.sprintf "block, %d %s" (List.length fields)
           (if List.length fields = 1 then "field" else "fields"))
  | JT.Procedure _ -> Some "procedure signature"

let dim_extent (dim : JT.dim) : int option =
  match (dim.lower, dim.upper) with
  | None, Some (JT.BoundInt n) when n >= 0 -> Some n
  | Some (JT.BoundInt lo), Some (JT.BoundInt hi) when hi >= lo ->
      Some (hi - lo + 1)
  | _ -> None

let table_entry_count (ty : JT.t) : int option =
  match ty with
  | JT.Table { dims; _ } ->
      let rec loop acc = function
        | [] -> if acc > 0 then Some acc else None
        | dim :: rest -> (
            match dim_extent dim with
            | Some n when n > 0 -> loop (acc * n) rest
            | _ -> None)
      in
      loop 1 dims
  | _ -> None

let add_type_hint ctx ~(loc : Ast.Loc.t) (ty : Ast.type_expr Ast.node) : unit =
  if loc_intersects ctx.range loc then
    let rich = JT.of_ast_type_expr ctx.type_env ty in
    match type_detail_text rich with
    | None -> ()
    | Some label ->
        add_hint ctx ~position:(loc_end ty.loc) ~kind:T.InlayHintKind.Type
          ~label ~source:"type-detail" ~padding_left:true ()

let add_table_count_hint ctx ~(name_loc : Ast.Loc.t)
    (ty : Ast.type_expr Ast.node) : unit =
  if loc_intersects ctx.range name_loc || loc_intersects ctx.range ty.loc then
    let rich = JT.of_ast_type_expr ctx.type_env ty in
    match table_entry_count rich with
    | None -> ()
    | Some count ->
        add_hint ctx ~position:(loc_end name_loc) ~kind:T.InlayHintKind.Type
          ~label:
            (Printf.sprintf "%d %s" count
               (if count = 1 then "entry" else "entries"))
          ~source:"table-dimensions" ~padding_left:true ()

let add_proc_sig table (proc : Ast.proc Ast.node) : unit =
  let key = normalize_name proc.v.name.v in
  if key <> "" then
    let sig_ = { params = proc.v.params } in
    let current =
      match Hashtbl.find_opt table key with None -> [] | Some xs -> xs
    in
    Hashtbl.replace table key (current @ [ sig_ ])

let rec collect_type_env_from_program env (prog : Ast.program) : unit =
  List.iter
    (function
      | Ast.TopDecl d -> collect_type_env_from_decl env d
      | Ast.TopStmt s -> collect_type_env_from_stmt env s)
    prog

and collect_type_env_from_decl env (decl : Ast.decl Ast.node) : unit =
  match decl.v with
  | Ast.DType { name; defn; _ } -> JT.add_type env name.v defn
  | Ast.DProc proc ->
      List.iter (collect_type_env_from_decl env) proc.v.locals;
      collect_type_env_from_stmt env proc.v.body
  | Ast.DVar _ | Ast.DConst _ | Ast.DDirective _ -> ()

and collect_type_env_from_stmt env (stmt : Ast.stmt Ast.node) : unit =
  match stmt.v with
  | Ast.SBlock stmts -> List.iter (collect_type_env_from_stmt env) stmts
  | Ast.SDecl decl -> collect_type_env_from_decl env decl
  | Ast.SIf { then_; else_; _ } ->
      collect_type_env_from_stmt env then_;
      Option.iter (collect_type_env_from_stmt env) else_
  | Ast.SWhile { body; _ } -> collect_type_env_from_stmt env body
  | Ast.SFor { init; step; body; _ } ->
      Option.iter (collect_type_env_from_stmt env) init;
      Option.iter (collect_type_env_from_stmt env) step;
      collect_type_env_from_stmt env body
  | Ast.SLabel { body; _ } -> collect_type_env_from_stmt env body
  | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _ | Ast.SGoto _ ->
      ()

let rec collect_proc_sigs_from_program table (prog : Ast.program) : unit =
  List.iter
    (function
      | Ast.TopDecl d -> collect_proc_sigs_from_decl table d
      | Ast.TopStmt s -> collect_proc_sigs_from_stmt table s)
    prog

and collect_proc_sigs_from_decl table (decl : Ast.decl Ast.node) : unit =
  match decl.v with
  | Ast.DProc proc ->
      add_proc_sig table proc;
      List.iter (collect_proc_sigs_from_decl table) proc.v.locals;
      collect_proc_sigs_from_stmt table proc.v.body
  | Ast.DVar _ | Ast.DConst _ | Ast.DType _ | Ast.DDirective _ -> ()

and collect_proc_sigs_from_stmt table (stmt : Ast.stmt Ast.node) : unit =
  match stmt.v with
  | Ast.SBlock stmts -> List.iter (collect_proc_sigs_from_stmt table) stmts
  | Ast.SDecl decl -> collect_proc_sigs_from_decl table decl
  | Ast.SIf { then_; else_; _ } ->
      collect_proc_sigs_from_stmt table then_;
      Option.iter (collect_proc_sigs_from_stmt table) else_
  | Ast.SWhile { body; _ } -> collect_proc_sigs_from_stmt table body
  | Ast.SFor { init; step; body; _ } ->
      Option.iter (collect_proc_sigs_from_stmt table) init;
      Option.iter (collect_proc_sigs_from_stmt table) step;
      collect_proc_sigs_from_stmt table body
  | Ast.SLabel { body; _ } -> collect_proc_sigs_from_stmt table body
  | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _ | Ast.SGoto _ ->
      ()

let lookup_docs_for_hints ws doc budget =
  if Workspace_budget.should_stop ~phase:"inlay.lookup_docs" budget then [ doc ]
  else docs_for_lookup ws doc

let build_type_env ws doc budget =
  let env = JT.empty_type_env () in
  lookup_docs_for_hints ws doc budget
  |> List.iter (fun doc ->
         if not (Workspace_budget.should_stop ~phase:"inlay.type_env" budget)
         then
           match current_program doc with
           | Some prog -> collect_type_env_from_program env prog
           | None -> ());
  env

let build_proc_sigs ws doc budget =
  let table = Hashtbl.create 32 in
  lookup_docs_for_hints ws doc budget
  |> List.iter (fun doc ->
         if not (Workspace_budget.should_stop ~phase:"inlay.proc_sigs" budget)
         then
           match current_program doc with
           | Some prog -> collect_proc_sigs_from_program table prog
           | None -> ());
  table

let first_proc_sig (ctx : context) (callee : Ast.ident) : proc_sig option =
  let key = normalize_name callee.v in
  match Hashtbl.find_opt ctx.proc_sigs key with
  | Some (sig_ :: _) -> Some sig_
  | _ -> None

let add_call_parameter_hints (ctx : context) (callee : Ast.ident)
    (args : Ast.expr Ast.node list) : unit =
  match first_proc_sig ctx callee with
  | None -> ()
  | Some (sig_ : proc_sig) ->
      args
      |> List.iteri (fun i (arg : Ast.expr Ast.node) ->
             if
               not
                 (Workspace_budget.should_stop ~phase:"inlay.call_param"
                    ctx.budget)
             then
               match List.nth_opt sig_.params i with
               | None -> ()
               | Some param ->
                   let name = String.trim param.v.pname.v in
                   let key = normalize_name name in
                   if key <> "" && key <> "__IMPLICIT__" then
                     add_hint ctx ~position:(loc_start arg.loc)
                       ~kind:T.InlayHintKind.Parameter ~label:(name ^ ":")
                       ~source:"call-parameter" ~padding_right:true ())

let rec visit_program ctx (prog : Ast.program) : unit =
  List.iter
    (fun top ->
      if not (Workspace_budget.should_stop ~phase:"inlay.program" ctx.budget)
      then
        match top with
        | Ast.TopDecl d -> visit_decl ctx d
        | Ast.TopStmt s -> visit_stmt ctx s)
    prog

and visit_decl ctx (decl : Ast.decl Ast.node) : unit =
  if not (loc_intersects ctx.range decl.loc) then ()
  else
    match decl.v with
    | Ast.DVar { name; dtype; init; data_decl_kind = Ast.DataTable; _ } ->
        add_table_count_hint ctx ~name_loc:name.loc dtype;
        Option.iter (visit_expr ctx) init
    | Ast.DVar { dtype; init; _ } ->
        add_type_hint ctx ~loc:decl.loc dtype;
        Option.iter (visit_expr ctx) init
    | Ast.DConst
        { name; dtype = Some dtype; value; data_decl_kind = Ast.DataTable; _ } ->
        add_table_count_hint ctx ~name_loc:name.loc dtype;
        add_type_hint ctx ~loc:decl.loc dtype;
        visit_expr ctx value
    | Ast.DConst { dtype = Some dtype; value; _ } ->
        add_type_hint ctx ~loc:decl.loc dtype;
        visit_expr ctx value
    | Ast.DConst { dtype = None; value; _ } -> visit_expr ctx value
    | Ast.DType { defn; _ } -> add_type_hint ctx ~loc:decl.loc defn
    | Ast.DProc proc ->
        List.iter (visit_decl ctx) proc.v.locals;
        visit_stmt ctx proc.v.body
    | Ast.DDirective _ -> ()

and visit_stmt ctx (stmt : Ast.stmt Ast.node) : unit =
  if not (loc_intersects ctx.range stmt.loc) then ()
  else
    match stmt.v with
    | Ast.SEmpty -> ()
    | Ast.SBlock stmts -> List.iter (visit_stmt ctx) stmts
    | Ast.SDecl decl -> visit_decl ctx decl
    | Ast.SAssign { lhs; rhs } ->
        visit_expr ctx lhs;
        visit_expr ctx rhs
    | Ast.SCallStmt { callee; args; _ } ->
        add_call_parameter_hints ctx callee args;
        List.iter (visit_expr ctx) args
    | Ast.SIf { cond; then_; else_ } ->
        visit_expr ctx cond;
        visit_stmt ctx then_;
        Option.iter (visit_stmt ctx) else_
    | Ast.SWhile { cond; body } ->
        visit_expr ctx cond;
        visit_stmt ctx body
    | Ast.SFor { init; cond; step; body } ->
        Option.iter (visit_stmt ctx) init;
        Option.iter (visit_expr ctx) cond;
        Option.iter (visit_stmt ctx) step;
        visit_stmt ctx body
    | Ast.SReturn expr -> Option.iter (visit_expr ctx) expr
    | Ast.SLabel { body; _ } -> visit_stmt ctx body
    | Ast.SGoto _ -> ()

and visit_expr ctx (expr : Ast.expr Ast.node) : unit =
  if not (loc_intersects ctx.range expr.loc) then ()
  else
    match expr.v with
    | Ast.EName _ | Ast.ELit _ -> ()
    | Ast.EUnop { rhs; _ } -> visit_expr ctx rhs
    | Ast.EBinop { lhs; rhs; _ } ->
        visit_expr ctx lhs;
        visit_expr ctx rhs
    | Ast.ECall { callee; args } ->
        add_call_parameter_hints ctx callee args;
        List.iter (visit_expr ctx) args
    | Ast.EIndex { base; index } ->
        visit_expr ctx base;
        List.iter (visit_expr ctx) index
    | Ast.EField { base; _ } -> visit_expr ctx base
    | Ast.EConvert { ty; expr } ->
        add_type_hint ctx ~loc:ty.loc ty;
        visit_expr ctx expr
    | Ast.EPreset { base; items } ->
        visit_expr ctx base;
        List.iter (visit_expr ctx) items
    | Ast.ERange { lo; hi } ->
        visit_expr ctx lo;
        visit_expr ctx hi
    | Ast.EAt { field; ptr } ->
        visit_expr ctx field;
        visit_expr ctx ptr
    | Ast.EDeref { ptr } -> visit_expr ctx ptr
    | Ast.EParen expr -> visit_expr ctx expr

let sort_and_dedupe_hints (hints : T.InlayHint.t list) : T.InlayHint.t list =
  let compare_hint a b =
    let c = compare_pos a.T.InlayHint.position b.T.InlayHint.position in
    if c <> 0 then c
    else String.compare (inlay_label a.label) (inlay_label b.label)
  in
  let sorted = List.sort compare_hint hints in
  let seen = Hashtbl.create 64 in
  let out = ref [] in
  List.iter
    (fun hint ->
      let pos = hint.T.InlayHint.position in
      let key =
        Printf.sprintf "%d:%d:%s" pos.line pos.character (inlay_label hint.label)
      in
      if not (Hashtbl.mem seen key) then (
        Hashtbl.replace seen key true;
        out := hint :: !out))
    sorted;
  List.rev !out

let inlay_hints_for (ws : t) ~(uri : T.DocumentUri.t) ~(range : T.Range.t) :
    T.InlayHint.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc -> (
      let budget = Workspace_budget.start ~ws ~soft_budget_ms:inlay_hint_budget_ms in
      match current_program doc with
      | None -> []
      | Some prog ->
          let ctx =
            {
              ws;
              range;
              budget;
              type_env = build_type_env ws doc budget;
              proc_sigs = build_proc_sigs ws doc budget;
              hints = [];
            }
          in
          visit_program ctx prog;
          ignore (Workspace_budget.check ~phase:"inlay.finish" budget);
          sort_and_dedupe_hints ctx.hints)
