(* Module overview: Inlay hint provider for types, procedure calls, and declaration details. *)

module T = Lsp.Types
module JT = Jovial_type
open Workspace_state
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_navigation_support

type proc_sig = { params : string list }

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
let loc_before_range (range : T.Range.t) loc =
  compare_pos (loc_end loc) range.start < 0

let loc_after_range (range : T.Range.t) loc =
  compare_pos range.end_ (loc_start loc) < 0

let should_visit_loc range loc =
  not (loc_before_range range loc || loc_after_range range loc)

let budget_allows budget ~phase =
  not (Workspace_budget.should_stop ~phase budget)

let rec iter_ordered_locs ?range ~phase budget loc_of f = function
  | [] -> ()
  | item :: rest -> (
      if budget_allows budget ~phase then
        let continue () =
          iter_ordered_locs ?range ~phase budget loc_of f rest
        in
        match range with
        | Some requested when loc_after_range requested (loc_of item) -> ()
        | Some requested when loc_before_range requested (loc_of item) ->
            continue ()
        | _ ->
            f item;
            continue ())

let stmt_loc (stmt : Ast.stmt Ast.node) = stmt.loc
let decl_loc (decl : Ast.decl Ast.node) = decl.loc

let top_loc = function
  | Ast.TopDecl decl -> decl.loc
  | Ast.TopStmt stmt -> stmt.loc

let range_contains_loc_opt range loc =
  match range with None -> true | Some range -> should_visit_loc range loc

let current_program (doc : Document.t) : Ast.program option =
  match Document.current_parse doc with
  | Some { Document.parsed_ast = Some prog; _ } -> Some prog
  | _ -> None

let inlay_label = function
  | `String s -> s
  | `List parts ->
      parts
      |> List.map (fun (p : T.InlayHintLabelPart.t) -> p.value)
      |> String.concat ""

let hint_data ~source ctx =
  let stopped =
    match Workspace_budget.reason_if_stopped ctx.budget with
    | None -> []
    | Some reason ->
        [ ("stoppedReason", `String (Workspace_readiness.reason_label reason)) ]
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
        ~data:(hint_data ~source ctx) ()
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
    let sig_ =
      {
        params =
          List.map (fun (p : Ast.param Ast.node) -> p.v.pname.v) proc.v.params;
      }
    in
    let current =
      match Hashtbl.find_opt table key with None -> [] | Some xs -> xs
    in
    Hashtbl.replace table key (current @ [ sig_ ])

let add_proc_sig_names table ~(name : string) ~(params : string list) : unit =
  let key = normalize_name name in
  if key <> "" then
    let current =
      match Hashtbl.find_opt table key with None -> [] | Some xs -> xs
    in
    Hashtbl.replace table key (current @ [ { params } ])

(* Keep top-level facts broad for forward references, but make body walks
   viewport-scoped so huge files do not traverse unrelated procedures. *)
let rec collect_type_env_from_program ?range budget env (prog : Ast.program) :
    unit =
  List.iter
    (function
      | Ast.TopDecl d ->
          collect_type_env_from_decl ?range budget env ~top:true d
      | Ast.TopStmt s -> collect_type_env_from_stmt ?range budget env s)
    prog

and collect_type_env_from_decl ?range budget env ~top (decl : Ast.decl Ast.node)
    : unit =
  if budget_allows budget ~phase:"inlay.type_env.decl" then
    match decl.v with
    | Ast.DType { name; defn; _ } ->
        if top || range_contains_loc_opt range decl.loc then
          JT.add_type env name.v defn
    | Ast.DProc proc ->
        if range_contains_loc_opt range decl.loc then (
          iter_ordered_locs ?range ~phase:"inlay.type_env.locals" budget
            decl_loc
            (collect_type_env_from_decl ?range budget env ~top:false)
            proc.v.locals;
          collect_type_env_from_stmt ?range budget env proc.v.body)
    | Ast.DVar _ | Ast.DConst _ | Ast.DOverlay _ | Ast.DDirective _ -> ()

and collect_type_env_from_stmt ?range budget env (stmt : Ast.stmt Ast.node) :
    unit =
  if
    budget_allows budget ~phase:"inlay.type_env.stmt"
    && range_contains_loc_opt range stmt.loc
  then
    match stmt.v with
    | Ast.SBlock stmts ->
        iter_ordered_locs ?range ~phase:"inlay.type_env.block" budget stmt_loc
          (collect_type_env_from_stmt ?range budget env)
          stmts
    | Ast.SDecl decl ->
        collect_type_env_from_decl ?range budget env ~top:false decl
    | Ast.SIf { then_; else_; _ } ->
        collect_type_env_from_stmt ?range budget env then_;
        Option.iter (collect_type_env_from_stmt ?range budget env) else_
    | Ast.SWhile { body; _ } ->
        collect_type_env_from_stmt ?range budget env body
    | Ast.SFor { init; step; body; _ } ->
        Option.iter (collect_type_env_from_stmt ?range budget env) init;
        Option.iter (collect_type_env_from_stmt ?range budget env) step;
        collect_type_env_from_stmt ?range budget env body
    | Ast.SLabel { body; _ } ->
        collect_type_env_from_stmt ?range budget env body
    | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _ | Ast.SGoto _
      ->
        ()

let rec collect_proc_sigs_from_program ?range budget table (prog : Ast.program)
    : unit =
  List.iter
    (function
      | Ast.TopDecl d ->
          collect_proc_sigs_from_decl ?range budget table ~top:true d
      | Ast.TopStmt s -> collect_proc_sigs_from_stmt ?range budget table s)
    prog

and collect_proc_sigs_from_decl ?range budget table ~top
    (decl : Ast.decl Ast.node) : unit =
  if budget_allows budget ~phase:"inlay.proc_sigs.decl" then
    match decl.v with
    | Ast.DProc proc ->
        if top || range_contains_loc_opt range decl.loc then
          add_proc_sig table proc;
        if range_contains_loc_opt range decl.loc then (
          iter_ordered_locs ?range ~phase:"inlay.proc_sigs.locals" budget
            decl_loc
            (collect_proc_sigs_from_decl ?range budget table ~top:false)
            proc.v.locals;
          collect_proc_sigs_from_stmt ?range budget table proc.v.body)
    | Ast.DVar _ | Ast.DConst _ | Ast.DType _ | Ast.DOverlay _
    | Ast.DDirective _ ->
        ()

and collect_proc_sigs_from_stmt ?range budget table (stmt : Ast.stmt Ast.node) :
    unit =
  if
    budget_allows budget ~phase:"inlay.proc_sigs.stmt"
    && range_contains_loc_opt range stmt.loc
  then
    match stmt.v with
    | Ast.SBlock stmts ->
        iter_ordered_locs ?range ~phase:"inlay.proc_sigs.block" budget stmt_loc
          (collect_proc_sigs_from_stmt ?range budget table)
          stmts
    | Ast.SDecl decl ->
        collect_proc_sigs_from_decl ?range budget table ~top:false decl
    | Ast.SIf { then_; else_; _ } ->
        collect_proc_sigs_from_stmt ?range budget table then_;
        Option.iter (collect_proc_sigs_from_stmt ?range budget table) else_
    | Ast.SWhile { body; _ } ->
        collect_proc_sigs_from_stmt ?range budget table body
    | Ast.SFor { init; step; body; _ } ->
        Option.iter (collect_proc_sigs_from_stmt ?range budget table) init;
        Option.iter (collect_proc_sigs_from_stmt ?range budget table) step;
        collect_proc_sigs_from_stmt ?range budget table body
    | Ast.SLabel { body; _ } ->
        collect_proc_sigs_from_stmt ?range budget table body
    | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _ | Ast.SGoto _
      ->
        ()

let lookup_docs_for_hints ws doc budget =
  if Workspace_budget.should_stop ~phase:"inlay.lookup_docs" budget then [ doc ]
  else docs_for_lookup ws doc

let collection_range_for_doc ws request_doc range candidate =
  if candidate.Document.uri = request_doc.Document.uri then Some range
  else if Workspace_tuning.is_large_doc candidate ws then Some range
  else None

let build_type_env ws doc range budget =
  let env = JT.empty_type_env () in
  lookup_docs_for_hints ws doc budget
  |> List.iter (fun candidate ->
      if not (Workspace_budget.should_stop ~phase:"inlay.type_env" budget) then
        match current_program candidate with
        | Some prog ->
            let range = collection_range_for_doc ws doc range candidate in
            collect_type_env_from_program ?range budget env prog
        | None -> ());
  env

let build_proc_sigs ws doc range budget =
  let table = Hashtbl.create 32 in
  lookup_docs_for_hints ws doc budget
  |> List.iter (fun candidate ->
      if not (Workspace_budget.should_stop ~phase:"inlay.proc_sigs" budget) then
        match current_program candidate with
        | Some prog ->
            let range = collection_range_for_doc ws doc range candidate in
            collect_proc_sigs_from_program ?range budget table prog
           | None -> ());
  table

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' -> true
  | _ -> false

let skip_spaces s i =
  let n = String.length s in
  let rec loop i =
    if i < n then
      match s.[i] with ' ' | '\t' -> loop (i + 1) | _ -> i
    else i
  in
  loop i

let read_ident s i =
  let n = String.length s in
  let i = skip_spaces s i in
  if i >= n || not (is_ident_char s.[i]) then None
  else
    let rec stop j = if j < n && is_ident_char s.[j] then stop (j + 1) else j in
    let j = stop i in
    Some (String.sub s i (j - i), j)

let find_char_from s start ch =
  try Some (String.index_from s start ch) with Not_found -> None

let find_substring_from s start needle =
  let n = String.length s in
  let m = String.length needle in
  let rec loop i =
    if m = 0 then Some i
    else if i + m > n then None
    else if String.sub s i m = needle then Some i
    else loop (i + 1)
  in
  loop start

let first_param_name raw =
  match read_ident (String.trim raw) 0 with Some (name, _) -> name | None -> ""

let split_param_names text =
  text |> String.split_on_char ',' |> List.map first_param_name
  |> List.filter (fun name -> normalize_name name <> "")

let scan_proc_sig_line table line =
  let upper = String.uppercase_ascii line in
  match find_substring_from upper 0 "PROC" with
  | None -> ()
  | Some proc_idx -> (
      match read_ident line (proc_idx + 4) with
      | None -> ()
      | Some (name, after_name) ->
          let after_name = skip_spaces line after_name in
          if after_name < String.length line && line.[after_name] = '(' then
            match find_char_from line (after_name + 1) ')' with
            | None -> ()
            | Some close_idx ->
                let params =
                  String.sub line (after_name + 1) (close_idx - after_name - 1)
                  |> split_param_names
                in
                add_proc_sig_names table ~name ~params)

let line_text (doc : Document.t) line =
  match
    ( Text_index.line_start_offset doc.Document.index ~line,
      Text_index.line_length doc.Document.index ~line )
  with
  | Some start, Some len ->
      let max_len = max 0 (String.length doc.Document.text - start) in
      Some (String.sub doc.Document.text start (min len max_len))
  | _ -> None

let iter_doc_lines (doc : Document.t) ~(start_line : int) ~(end_line : int) f =
  let last = min end_line (Text_index.line_count doc.Document.index - 1) in
  let rec loop line =
    if line <= last then (
      match line_text doc line with
      | Some text ->
          f line text;
          loop (line + 1)
      | None -> ())
  in
  if start_line <= last then loop (max 0 start_line)

let text_lookup_docs_for_hints ws doc budget =
  let docs = lookup_docs_for_hints ws doc budget in
  if Document.imports doc <> [] then docs
  else
    let seen = Hashtbl.create 16 in
    let out = ref [] in
    let add_doc d =
      let key = Uri_path.docuri_to_string d.Document.uri in
      if not (Hashtbl.mem seen key) then (
        Hashtbl.replace seen key true;
        out := d :: !out)
    in
    List.iter add_doc docs;
    Hashtbl.iter (fun _ d -> add_doc d) ws.files;
    Hashtbl.iter (fun _ d -> add_doc d) ws.docs;
    List.rev !out

let build_text_proc_sigs ws doc (range : T.Range.t) budget =
  let table = Hashtbl.create 32 in
  text_lookup_docs_for_hints ws doc budget
  |> List.iter (fun candidate ->
         if budget_allows budget ~phase:"inlay.text.proc_sigs" then
           let start_line, end_line =
             if Workspace_tuning.is_large_doc candidate ws then
               (range.start.line, range.end_.line)
             else (0, Text_index.line_count candidate.Document.index - 1)
           in
           iter_doc_lines candidate ~start_line ~end_line (fun _ line ->
               if budget_allows budget ~phase:"inlay.text.proc_sig.line" then
                 scan_proc_sig_line table line));
  table

let type_detail_of_decl_text text =
  let upper = String.uppercase_ascii (String.trim text) in
  match read_ident upper 0 with
  | Some ("U", after) -> (
      match read_ident upper after with
      | Some (bits, _) -> Some ("unsigned integer, " ^ bits ^ " bits")
      | None -> Some "unsigned integer")
  | Some ("S", after) -> (
      match read_ident upper after with
      | Some (bits, _) -> Some ("signed integer, " ^ bits ^ " bits")
      | None -> Some "signed integer")
  | Some ("F", after) -> (
      match read_ident upper after with
      | Some (bits, _) -> Some ("floating point, " ^ bits ^ " bits precision")
      | None -> Some "floating point")
  | Some ("C", after) -> (
      match read_ident upper after with
      | Some (chars, _) -> Some ("character string, " ^ chars ^ " characters")
      | None -> Some "character string")
  | _ -> None

let scan_text_type_hint ctx ~line_no line =
  let trimmed = String.trim line in
  let upper = String.uppercase_ascii trimmed in
  if
    String.length upper >= 5
    && String.sub upper 0 5 = "ITEM "
    && budget_allows ctx.budget ~phase:"inlay.text.type"
  then
    match read_ident line 4 with
    | None -> ()
    | Some (_, after_name) ->
        let ty_start = skip_spaces line after_name in
        let ty_end =
          match find_char_from line ty_start ';' with
          | Some i -> i
          | None -> String.length line
        in
        if ty_end > ty_start then
          let ty_text = String.sub line ty_start (ty_end - ty_start) in
          match type_detail_of_decl_text ty_text with
          | None -> ()
          | Some label ->
              add_hint ctx
                ~position:{ T.Position.line = line_no; character = ty_end }
                ~kind:T.InlayHintKind.Type ~label ~source:"type-detail"
                ~padding_left:true ()

let call_arg_positions line open_idx close_idx =
  let rec loop i arg_start acc =
    if i >= close_idx then
      let start = skip_spaces line arg_start in
      if start < close_idx then List.rev (start :: acc) else List.rev acc
    else if line.[i] = ',' then
      let start = skip_spaces line arg_start in
      let acc = if start < i then start :: acc else acc in
      loop (i + 1) (i + 1) acc
    else loop (i + 1) arg_start acc
  in
  loop (open_idx + 1) (open_idx + 1) []

let callee_before_open line open_idx =
  let rec left i =
    if i >= 0 && is_ident_char line.[i] then left (i - 1) else i + 1
  in
  let start = left (open_idx - 1) in
  if start < open_idx then Some (String.sub line start (open_idx - start), start)
  else None

let add_text_call_hints ctx ~line_no ~callee ~arg_positions =
  let key = normalize_name callee in
  match Hashtbl.find_opt ctx.proc_sigs key with
  | Some (sig_ :: _) ->
      List.iteri
        (fun i character ->
          match List.nth_opt sig_.params i with
          | None -> ()
          | Some name ->
              let name = String.trim name in
              let key = normalize_name name in
              if key <> "" && key <> "__IMPLICIT__" then
                add_hint ctx
                  ~position:{ T.Position.line = line_no; character }
                  ~kind:T.InlayHintKind.Parameter ~label:(name ^ ":")
                  ~source:"call-parameter" ~padding_right:true ())
        arg_positions
  | _ -> ()

let scan_text_call_hints ctx ~line_no line =
  let rec loop start =
    if budget_allows ctx.budget ~phase:"inlay.text.call" then
      match find_char_from line start '(' with
      | None -> ()
      | Some open_idx -> (
          match (callee_before_open line open_idx, find_char_from line open_idx ')') with
          | Some (callee, _), Some close_idx ->
              add_text_call_hints ctx ~line_no ~callee
                ~arg_positions:(call_arg_positions line open_idx close_idx);
              loop (close_idx + 1)
          | _ -> loop (open_idx + 1))
  in
  loop 0

let add_text_range_hints ctx doc =
  iter_doc_lines doc ~start_line:ctx.range.start.line
    ~end_line:ctx.range.end_.line (fun line_no line ->
      scan_text_type_hint ctx ~line_no line;
      scan_text_call_hints ctx ~line_no line)

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
              (Workspace_budget.should_stop ~phase:"inlay.call_param" ctx.budget)
          then
            match List.nth_opt sig_.params i with
            | None -> ()
            | Some param ->
                let name = String.trim param in
                let key = normalize_name name in
                if key <> "" && key <> "__IMPLICIT__" then
                  add_hint ctx ~position:(loc_start arg.loc)
                    ~kind:T.InlayHintKind.Parameter ~label:(name ^ ":")
                    ~source:"call-parameter" ~padding_right:true ())

let rec visit_program ctx (prog : Ast.program) : unit =
  iter_ordered_locs ~range:ctx.range ~phase:"inlay.program" ctx.budget top_loc
    (function
      | Ast.TopDecl d -> visit_decl ctx d | Ast.TopStmt s -> visit_stmt ctx s)
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
        { name; dtype = Some dtype; value; data_decl_kind = Ast.DataTable; _ }
      ->
        add_table_count_hint ctx ~name_loc:name.loc dtype;
        add_type_hint ctx ~loc:decl.loc dtype;
        visit_expr ctx value
    | Ast.DConst { dtype = Some dtype; value; _ } ->
        add_type_hint ctx ~loc:decl.loc dtype;
        visit_expr ctx value
    | Ast.DConst { dtype = None; value; _ } -> visit_expr ctx value
    | Ast.DType { defn; _ } -> add_type_hint ctx ~loc:decl.loc defn
    | Ast.DProc proc ->
        iter_ordered_locs ~range:ctx.range ~phase:"inlay.proc.locals" ctx.budget
          decl_loc (visit_decl ctx) proc.v.locals;
        visit_stmt ctx proc.v.body
    | Ast.DOverlay _ | Ast.DDirective _ -> ()

and visit_stmt ctx (stmt : Ast.stmt Ast.node) : unit =
  if not (loc_intersects ctx.range stmt.loc) then ()
  else
    match stmt.v with
    | Ast.SEmpty -> ()
    | Ast.SBlock stmts ->
        iter_ordered_locs ~range:ctx.range ~phase:"inlay.stmt.block" ctx.budget
          stmt_loc (visit_stmt ctx) stmts
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
        Printf.sprintf "%d:%d:%s" pos.line pos.character
          (inlay_label hint.label)
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
      let budget =
        Workspace_budget.start ~ws ~soft_budget_ms:inlay_hint_budget_ms
      in
      if Workspace_tuning.is_large_doc doc ws then (
        let ctx =
          {
            ws;
            range;
            budget;
            type_env = JT.empty_type_env ();
            proc_sigs = build_text_proc_sigs ws doc range budget;
            hints = [];
          }
        in
        add_text_range_hints ctx doc;
        ignore (Workspace_budget.check ~phase:"inlay.text.finish" budget);
        sort_and_dedupe_hints ctx.hints)
      else
        match current_program doc with
      | None -> []
      | Some prog ->
          let ctx =
            {
              ws;
              range;
              budget;
              type_env = build_type_env ws doc range budget;
              proc_sigs = build_proc_sigs ws doc range budget;
              hints = [];
            }
          in
          visit_program ctx prog;
          ignore (Workspace_budget.check ~phase:"inlay.finish" budget);
          sort_and_dedupe_hints ctx.hints)
