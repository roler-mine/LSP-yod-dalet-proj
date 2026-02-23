(* lib/ast.ml *)

module Loc = struct
  type pos = {
    line : int;   (* 1-based *)
    col : int;    (* 0-based *)
    offset : int; (* 0-based absolute char offset *)
  }

  type t = {
    file : string option;
    start_pos : pos;
    end_pos : pos;
  }

  let none =
    let z = { line = 1; col = 0; offset = 0 } in
    { file = None; start_pos = z; end_pos = z }

  let make ~start_pos ~end_pos ~file =
  { file; start_pos; end_pos }

let make_no_file ~start_pos ~end_pos =
  { file = None; start_pos; end_pos }

  let of_lexing_positions (sp : Lexing.position) (ep : Lexing.position) ~file =
    let mk (p : Lexing.position) =
      let col = p.pos_cnum - p.pos_bol in
      { line = p.pos_lnum; col; offset = p.pos_cnum }
    in
    let file =
      match file with
      | Some _ as f -> f
      | None -> if sp.pos_fname = "" then None else Some sp.pos_fname
    in
    { file; start_pos = mk sp; end_pos = mk ep }

  let of_lexing_positions_no_file sp ep =
    of_lexing_positions sp ep ~file:None

  let start_offset (t : t) = t.start_pos.offset
  let end_offset (t : t) = t.end_pos.offset

  let pp fmt (t : t) =
    let file = match t.file with Some f -> f | None -> "<nofile>" in
    Format.fprintf fmt "%s:%d:%d-%d:%d"
      file
      t.start_pos.line t.start_pos.col
      t.end_pos.line t.end_pos.col

  let to_string t = Format.asprintf "%a" pp t
end

type 'a node = { loc : Loc.t; v : 'a }

let node ?(loc = Loc.none) v = { loc; v }
let map f n = { n with v = f n.v }
let value n = n.v
let loc n = n.loc

type ident = string node

type literal =
  | LInt of string
  | LFloat of string
  | LString of string
  | LChar of char
  | LBool of bool

type unop = UPlus | UMinus | UNot | UBitNot

type binop =
  | BAdd | BSub | BMul | BDiv | BMod
  | BAnd | BOr
  | BBitAnd | BBitOr | BBitXor | BShl | BShr
  | BEq | BNe | BLt | BLe | BGt | BGe

type type_expr =
  | TName of ident
  | TArray of { elem : type_expr node; dims : expr node list }
  | TPointer of type_expr node
  | TRecord of field_decl node list
  | TFunc of { params : param node list; returns : type_expr node option }

and field_decl = { fname : ident; ftype : type_expr node }

and param_mode = In | Out | InOut

and param = { pname : ident; pmode : param_mode; ptype : type_expr node }

and expr =
  | EName of ident
  | ELit of literal
  | EUnop of { op : unop; rhs : expr node }
  | EBinop of { op : binop; lhs : expr node; rhs : expr node }
  | ECall of { callee : ident; args : expr node list }
  | EIndex of { base : expr node; index : expr node list }
  | EField of { base : expr node; field : ident }
  | EAt of { field : expr node; ptr : expr node }
  | EDeref of { ptr : expr node }
  | EParen of expr node

and stmt =
  | SEmpty
  | SBlock of stmt node list
  | SDecl of decl node
  | SAssign of { lhs : expr node; rhs : expr node }
  | SCallStmt of { callee : ident; args : expr node list }
  | SIf of { cond : expr node; then_ : stmt node; else_ : stmt node option }
  | SWhile of { cond : expr node; body : stmt node }
  | SFor of { init : stmt node option; cond : expr node option; step : stmt node option; body : stmt node }
  | SReturn of expr node option
  | SLabel of { label : ident; body : stmt node }
  | SGoto of ident

and storage = Automatic | Static | External

and proc_use = UseNormal | UseRec | UseRent

and decl =
  | DVar of { name : ident; dtype : type_expr node; init : expr node option; storage : storage }
  | DConst of { name : ident; dtype : type_expr node option; value : expr node }
  | DType of { name : ident; defn : type_expr node }
  | DProc of proc node
  | DDirective of { name : ident; args : string node list }

and proc = {
  name : ident;
  params : param node list;
  returns : type_expr node option;
  use_attr : proc_use;
  locals : decl node list;
  body : stmt node;
}

type toplevel = TopDecl of decl node | TopStmt of stmt node
type program = toplevel list

module Debug = struct
  type dump_opts = {
    show_locs : bool;
    max_depth : int option;
    max_nodes : int option;
  }

  let default_opts = { show_locs = false; max_depth = None; max_nodes = None }

  (* Keep helpers outside the mutually-recursive printer group (avoids monomorphism traps). *)
  let pp_opt (ppv : Format.formatter -> 'a -> unit) fmt (o : 'a option) =
    match o with
    | None -> Format.pp_print_string fmt "None"
    | Some x -> Format.fprintf fmt "Some(%a)" ppv x

  let rec pp_list ?(sep = ";") (pp : Format.formatter -> 'a -> unit) fmt (xs : 'a list) =
    match xs with
    | [] -> ()
    | [x] -> pp fmt x
    | x :: tl ->
        pp fmt x;
        Format.fprintf fmt "%s@ " sep;
        pp_list ~sep pp fmt tl

  type budget = { mutable nodes_left : int option }

  let take_node (b : budget) =
    match b.nodes_left with
    | None -> true
    | Some n ->
        if n <= 0 then false else (b.nodes_left <- Some (n - 1); true)

  let over_depth (opts : dump_opts) (depth : int) =
    match opts.max_depth with None -> false | Some md -> depth > md

  let pp_loc_if (opts : dump_opts) fmt (l : Loc.t) =
    if opts.show_locs then Format.fprintf fmt " @[%a@]" Loc.pp l else ()

  let pp_ident fmt (id : ident) = Format.fprintf fmt "%s" id.v

  let pp_literal fmt = function
    | LInt s -> Format.fprintf fmt "Int(%s)" s
    | LFloat s -> Format.fprintf fmt "Float(%s)" s
    | LString s -> Format.fprintf fmt "String(%S)" s
    | LChar c -> Format.fprintf fmt "Char(%C)" c
    | LBool b -> Format.fprintf fmt "Bool(%b)" b

  let pp_unop fmt = function
    | UPlus -> Format.pp_print_string fmt "UPlus"
    | UMinus -> Format.pp_print_string fmt "UMinus"
    | UNot -> Format.pp_print_string fmt "UNot"
    | UBitNot -> Format.pp_print_string fmt "UBitNot"

  let pp_binop fmt = function
    | BAdd -> Format.pp_print_string fmt "BAdd"
    | BSub -> Format.pp_print_string fmt "BSub"
    | BMul -> Format.pp_print_string fmt "BMul"
    | BDiv -> Format.pp_print_string fmt "BDiv"
    | BMod -> Format.pp_print_string fmt "BMod"
    | BAnd -> Format.pp_print_string fmt "BAnd"
    | BOr -> Format.pp_print_string fmt "BOr"
    | BBitAnd -> Format.pp_print_string fmt "BBitAnd"
    | BBitOr -> Format.pp_print_string fmt "BBitOr"
    | BBitXor -> Format.pp_print_string fmt "BBitXor"
    | BShl -> Format.pp_print_string fmt "BShl"
    | BShr -> Format.pp_print_string fmt "BShr"
    | BEq -> Format.pp_print_string fmt "BEq"
    | BNe -> Format.pp_print_string fmt "BNe"
    | BLt -> Format.pp_print_string fmt "BLt"
    | BLe -> Format.pp_print_string fmt "BLe"
    | BGt -> Format.pp_print_string fmt "BGt"
    | BGe -> Format.pp_print_string fmt "BGe"

  let rec pp_type_expr opts b depth fmt (t : type_expr node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else if over_depth opts depth then Format.pp_print_string fmt "<depth-limit>"
    else
      match t.v with
      | TName n ->
          Format.fprintf fmt "TName(%a)%a" pp_ident n (pp_loc_if opts) t.loc
      | TPointer inner ->
          Format.fprintf fmt "@[TPointer(%a)@]%a"
            (pp_type_expr opts b (depth + 1)) inner
            (pp_loc_if opts) t.loc
      | TArray { elem; dims } ->
          Format.fprintf fmt "@[TArray(elem=%a; dims=[%a])@]%a"
            (pp_type_expr opts b (depth + 1)) elem
            (pp_list (pp_expr opts b (depth + 1))) dims
            (pp_loc_if opts) t.loc
      | TRecord fields ->
          Format.fprintf fmt "@[TRecord([%a])@]%a"
            (pp_list (pp_field_decl opts b (depth + 1))) fields
            (pp_loc_if opts) t.loc
      | TFunc { params; returns } ->
          Format.fprintf fmt "@[TFunc(params=[%a]; returns=%a)@]%a"
            (pp_list (pp_param opts b (depth + 1))) params
            (pp_opt (pp_type_expr opts b (depth + 1))) returns
            (pp_loc_if opts) t.loc

  and pp_field_decl opts b depth fmt (f : field_decl node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      let x = f.v in
      Format.fprintf fmt "@[Field(%a : %a)@]%a"
        pp_ident x.fname
        (pp_type_expr opts b (depth + 1)) x.ftype
        (pp_loc_if opts) f.loc

  and pp_param_mode fmt = function
    | In -> Format.pp_print_string fmt "In"
    | Out -> Format.pp_print_string fmt "Out"
    | InOut -> Format.pp_print_string fmt "InOut"

  and pp_param opts b depth fmt (p : param node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      let x = p.v in
      Format.fprintf fmt "@[Param(%a %a : %a)@]%a"
        pp_param_mode x.pmode
        pp_ident x.pname
        (pp_type_expr opts b (depth + 1)) x.ptype
        (pp_loc_if opts) p.loc

  and pp_expr opts b depth fmt (e : expr node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else if over_depth opts depth then Format.pp_print_string fmt "<depth-limit>"
    else
      match e.v with
      | EName id ->
          Format.fprintf fmt "EName(%a)%a" pp_ident id (pp_loc_if opts) e.loc
      | ELit lit ->
          Format.fprintf fmt "ELit(%a)%a" pp_literal lit (pp_loc_if opts) e.loc
      | EUnop { op; rhs } ->
          Format.fprintf fmt "@[EUnop(%a, %a)@]%a"
            pp_unop op
            (pp_expr opts b (depth + 1)) rhs
            (pp_loc_if opts) e.loc
      | EBinop { op; lhs; rhs } ->
          Format.fprintf fmt "@[EBinop(%a, %a, %a)@]%a"
            pp_binop op
            (pp_expr opts b (depth + 1)) lhs
            (pp_expr opts b (depth + 1)) rhs
            (pp_loc_if opts) e.loc
      | ECall { callee; args } ->
          Format.fprintf fmt "@[ECall(%a, [%a])@]%a"
            pp_ident callee
            (pp_list (pp_expr opts b (depth + 1))) args
            (pp_loc_if opts) e.loc
      | EIndex { base; index } ->
          Format.fprintf fmt "@[EIndex(base=%a; index=[%a])@]%a"
            (pp_expr opts b (depth + 1)) base
            (pp_list (pp_expr opts b (depth + 1))) index
            (pp_loc_if opts) e.loc
      | EField { base; field } ->
          Format.fprintf fmt "@[EField(%a.%a)@]%a"
            (pp_expr opts b (depth + 1)) base
            pp_ident field
            (pp_loc_if opts) e.loc
      | EAt { field; ptr } ->
          Format.fprintf fmt "@[EAt(%a @ %a)@]%a"
            (pp_expr opts b (depth + 1)) field
            (pp_expr opts b (depth + 1)) ptr
            (pp_loc_if opts) e.loc

      | EDeref { ptr } ->
          Format.fprintf fmt "@[EDeref(@ %a)@]%a"
            (pp_expr opts b (depth + 1)) ptr
            (pp_loc_if opts) e.loc

      | EParen inner ->
          Format.fprintf fmt "@[EParen(%a)@]%a"
            (pp_expr opts b (depth + 1)) inner
            (pp_loc_if opts) e.loc

  and pp_stmt opts b depth fmt (s : stmt node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else if over_depth opts depth then Format.pp_print_string fmt "<depth-limit>"
    else
      match s.v with
      | SEmpty ->
          Format.fprintf fmt "SEmpty%a" (pp_loc_if opts) s.loc
      | SBlock xs ->
          Format.fprintf fmt "@[SBlock([%a])@]%a"
            (pp_list (pp_stmt opts b (depth + 1))) xs
            (pp_loc_if opts) s.loc
      | SDecl d ->
          Format.fprintf fmt "@[SDecl(%a)@]%a"
            (pp_decl opts b (depth + 1)) d
            (pp_loc_if opts) s.loc
      | SAssign { lhs; rhs } ->
          Format.fprintf fmt "@[SAssign(lhs=%a; rhs=%a)@]%a"
            (pp_expr opts b (depth + 1)) lhs
            (pp_expr opts b (depth + 1)) rhs
            (pp_loc_if opts) s.loc
      | SCallStmt { callee; args } ->
          Format.fprintf fmt "@[SCall(%a, [%a])@]%a"
            pp_ident callee
            (pp_list (pp_expr opts b (depth + 1))) args
            (pp_loc_if opts) s.loc
      | SIf { cond; then_; else_ } ->
          Format.fprintf fmt "@[SIf(cond=%a; then=%a; else=%a)@]%a"
            (pp_expr opts b (depth + 1)) cond
            (pp_stmt opts b (depth + 1)) then_
            (pp_opt (pp_stmt opts b (depth + 1))) else_
            (pp_loc_if opts) s.loc
      | SWhile { cond; body } ->
          Format.fprintf fmt "@[SWhile(cond=%a; body=%a)@]%a"
            (pp_expr opts b (depth + 1)) cond
            (pp_stmt opts b (depth + 1)) body
            (pp_loc_if opts) s.loc
      | SFor { init; cond; step; body } ->
          Format.fprintf fmt "@[SFor(init=%a; cond=%a; step=%a; body=%a)@]%a"
            (pp_opt (pp_stmt opts b (depth + 1))) init
            (pp_opt (pp_expr opts b (depth + 1))) cond
            (pp_opt (pp_stmt opts b (depth + 1))) step
            (pp_stmt opts b (depth + 1)) body
            (pp_loc_if opts) s.loc
      | SReturn eo ->
          Format.fprintf fmt "@[SReturn(%a)@]%a"
            (pp_opt (pp_expr opts b (depth + 1))) eo
            (pp_loc_if opts) s.loc
      | SLabel { label; body } ->
          Format.fprintf fmt "@[SLabel(%a: %a)@]%a"
            pp_ident label
            (pp_stmt opts b (depth + 1)) body
            (pp_loc_if opts) s.loc
      | SGoto id ->
          Format.fprintf fmt "@[SGoto(%a)@]%a"
            pp_ident id
            (pp_loc_if opts) s.loc

  and pp_storage fmt = function
    | Automatic -> Format.pp_print_string fmt "Automatic"
    | Static -> Format.pp_print_string fmt "Static"
    | External -> Format.pp_print_string fmt "External"

  and pp_proc_use fmt = function
    | UseNormal -> Format.pp_print_string fmt "Normal"
    | UseRec -> Format.pp_print_string fmt "REC"
    | UseRent -> Format.pp_print_string fmt "RENT"

  and pp_string_node fmt (s : string node) =
    Format.fprintf fmt "%S" s.v

  and pp_decl opts b depth fmt (d : decl node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      match d.v with
      | DVar { name; dtype; init; storage } ->
          Format.fprintf fmt "@[DVar(%a : %a; storage=%a; init=%a)@]%a"
            pp_ident name
            (pp_type_expr opts b (depth + 1)) dtype
            pp_storage storage
            (pp_opt (pp_expr opts b (depth + 1))) init
            (pp_loc_if opts) d.loc
      | DConst { name; dtype; value } ->
          Format.fprintf fmt "@[DConst(%a : %a; value=%a)@]%a"
            pp_ident name
            (pp_opt (pp_type_expr opts b (depth + 1))) dtype
            (pp_expr opts b (depth + 1)) value
            (pp_loc_if opts) d.loc
      | DType { name; defn } ->
          Format.fprintf fmt "@[DType(%a = %a)@]%a"
            pp_ident name
            (pp_type_expr opts b (depth + 1)) defn
            (pp_loc_if opts) d.loc
      | DProc p ->
          pp_proc opts b (depth + 1) fmt p
      | DDirective { name; args } ->
          Format.fprintf fmt "@[DDirective(%a, [%a])@]%a"
            pp_ident name
            (pp_list pp_string_node) args
            (pp_loc_if opts) d.loc

  and pp_proc opts b depth fmt (p : proc node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      let x = p.v in
      Format.fprintf fmt "@[DProc(name=%a; use=%a; params=[%a]; returns=%a; locals=[%a]; body=%a)@]%a"
        pp_ident x.name
        pp_proc_use x.use_attr
        (pp_list (pp_param opts b (depth + 1))) x.params
        (pp_opt (pp_type_expr opts b (depth + 1))) x.returns
        (pp_list (pp_decl opts b (depth + 1))) x.locals
        (pp_stmt opts b (depth + 1)) x.body
        (pp_loc_if opts) p.loc

  let pp_toplevel opts b fmt = function
    | TopDecl d -> Format.fprintf fmt "TopDecl(%a)" (pp_decl opts b 0) d
    | TopStmt s -> Format.fprintf fmt "TopStmt(%a)" (pp_stmt opts b 0) s

  let pp_program ?(opts = default_opts) fmt (p : program) =
    let b = { nodes_left = opts.max_nodes } in
    Format.fprintf fmt "@[<v>Program[@,%a@]@]"
      (pp_list ~sep:", "
         (fun fmt tl ->
           Format.fprintf fmt "@[<hov>%a@]" (pp_toplevel opts b) tl))
      p

  let to_string ?(opts = default_opts) (p : program) =
    Format.asprintf "%a" (pp_program ~opts) p
end
