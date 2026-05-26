(* Module overview: Jovial AST data model with locations, recovery metadata, and debug renderers. *)

(* lib/ast.ml *)

module T = Lsp.Types

module Loc = struct
  type pos = {
    line : int; (* 1-based *)
    col : int; (* 0-based *)
    offset : int; (* 0-based absolute char offset *)
  }

  type t = { file : string option; start_pos : pos; end_pos : pos }

  let none =
    let z = { line = 1; col = 0; offset = 0 } in
    { file = None; start_pos = z; end_pos = z }

  let make ~start_pos ~end_pos ~file = { file; start_pos; end_pos }
  let make_no_file ~start_pos ~end_pos = { file = None; start_pos; end_pos }

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

  let of_lexing_positions_no_file sp ep = of_lexing_positions sp ep ~file:None
  let start_offset (t : t) = t.start_pos.offset
  let end_offset (t : t) = t.end_pos.offset

  let pp fmt (t : t) =
    let file = match t.file with Some f -> f | None -> "<nofile>" in
    Format.fprintf fmt "%s:%d:%d-%d:%d" file t.start_pos.line t.start_pos.col
      t.end_pos.line t.end_pos.col

  let to_string t = Format.asprintf "%a" pp t
end

type node_id = int

type token_range = {
  first_token : int;
  last_token : int;
}

type recovery =
  | Complete
  | Missing of { expected : string list; message : string }
  | Invalid of { diagnostics : T.Diagnostic.t list }

type 'a node = {
  id : node_id;
  loc : Loc.t;
  tokens : token_range option;
  hash : int64 option;
  recovery : recovery;
  v : 'a;
}

let node_id_counter = ref 0
let node_id_mtx = Mutex.create ()

let fresh_node_id () =
  Mutex.lock node_id_mtx;
  incr node_id_counter;
  let id = !node_id_counter in
  Mutex.unlock node_id_mtx;
  id

let node ?(loc = Loc.none) ?tokens ?hash ?(recovery = Complete) v =
  { id = fresh_node_id (); loc; tokens; hash; recovery; v }

let missing_node ?(loc = Loc.none) ?tokens ~expected ~message v =
  node ~loc ?tokens ~recovery:(Missing { expected; message }) v

let invalid_node ?(loc = Loc.none) ?tokens ~diagnostics v =
  node ~loc ?tokens ~recovery:(Invalid { diagnostics }) v

let map f n = { n with v = f n.v }
let value n = n.v
let loc n = n.loc
let id n = n.id
let recovery n = n.recovery
let token_range n = n.tokens
let is_recovered n = match n.recovery with Complete -> false | _ -> true

type ident = string node

type parse_error = {
  message : string;
  expected : string list;
  actual : string option;
  recovery_kind : string;
}

let canonical_name (s : string) : string =
  let u = String.uppercase_ascii s in
  if String.length u <= 31 then u else String.sub u 0 31

let is_manual_name (s : string) : bool =
  let len = String.length s in
  if len < 2 then false
  else
    let is_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false in
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let is_start c = is_letter c || c = '$' in
    let is_rest c = is_letter c || is_digit c || c = '$' || c = '\'' in
    let rec loop i =
      if i >= len then true
      else if is_rest s.[i] then loop (i + 1)
      else false
    in
    is_start s.[0] && loop 1

let is_index_letter (s : string) : bool =
  String.length s = 1
  && (match s.[0] with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false)

type literal =
  | LInt of string
  | LFloat of string
  | LBit of { bead_size : int; beads : string; raw : string }
  | LString of string
  | LChar of char
  | LBool of bool
  | LNull

type round_mode = Round | Truncate

type scalar_base =
  | ScalarUnsigned
  | ScalarSigned
  | ScalarFloat
  | ScalarFixed
  | ScalarBit
  | ScalarChar

type unop = UPlus | UMinus | UNot | UBitNot

type binop =
  | BAdd
  | BSub
  | BMul
  | BDiv
  | BMod
  | BPow
  | BAnd
  | BOr
  | BBitAnd
  | BBitOr
  | BBitXor
  | BEqv
  | BShl
  | BShr
  | BEq
  | BNe
  | BLt
  | BLe
  | BGt
  | BGe

type type_expr =
  | TName of ident
  | TScalar of { base : scalar_base; round : round_mode option; sizes : expr node list }
  | TArray of { elem : type_expr node; dims : expr node list }
  | TSpecifiedTable of {
      elem : type_expr node;
      dims : expr node list;
      kind : specified_table_kind;
    }
  | TPointer of type_expr node
  | TStatus of status_value node list
  | TRecord of field_decl node list
  | TFunc of { params : param node list; returns : type_expr node option }

and specified_table_kind =
  | SpecTableW of expr node
  | SpecTableV of expr node option

and status_value = {
  sv_name : ident;
  sv_representation : expr node option;
}

and field_position = {
  pos_start_bit : expr node;
  pos_start_word : expr node;
}

and field_decl = {
  fname : ident;
  ftype : type_expr node;
  fpos : field_position option;
}
and param_mode = In | Out | InOut
and param = { pname : ident; pmode : param_mode; ptype : type_expr node }

and overlay_item =
  | OverlayTarget of ident
  | OverlaySpacer of expr node
  | OverlayGroup of overlay_item node list

and overlay_decl = {
  overlay_name : ident;
  overlay_items : overlay_item node list;
  overlay_pos : expr node option;
}

and expr =
  | EName of ident
  | ELit of literal
  | EError of parse_error
  | EMissing of parse_error
  | EUnop of { op : unop; rhs : expr node }
  | EBinop of { op : binop; lhs : expr node; rhs : expr node }
  | ECall of { callee : ident; args : expr node list }
  | EIndex of { base : expr node; index : expr node list }
  | EField of { base : expr node; field : ident }
  | EConvert of { ty : type_expr node; expr : expr node }
  | EPreset of { base : expr node; items : expr node list }
  | EOmitted
  | ERepeat of { count : expr node; items : expr node list }
  | EPositioned of { indexes : expr node list; values : expr node list }
  | ERange of { lo : expr node; hi : expr node }
  | EAt of { field : expr node; ptr : expr node }
  | EDeref of { ptr : expr node }
  | EParen of expr node

and case_index =
  | CaseDefault
  | CaseValue of expr node
  | CaseRange of expr node * expr node

and case_option = {
  case_indexes : case_index node list;
  case_body : stmt node;
  case_fallthru : bool;
}

and stmt =
  | SEmpty
  | SError of parse_error
  | SBlock of stmt node list
  | SDecl of decl node
  | SAssign of { lhs : expr node; rhs : expr node }
  | SCallStmt of {
      callee : ident;
      args : expr node list;
      abort_label : ident option;
    }
  | SIf of { cond : expr node; then_ : stmt node; else_ : stmt node option }
  | SWhile of { cond : expr node; body : stmt node }
  | SFor of {
      init : stmt node option;
      cond : expr node option;
      step : stmt node option;
      body : stmt node;
    }
  | SCase of { selector : expr node; options : case_option node list }
  | SReturn of expr node option
  | SLabel of { label : ident; body : stmt node }
  | SGoto of ident

and storage = Automatic | Static | External
and proc_use = UseNormal | UseRec | UseRent
and external_modifier = LocalDecl | DefDecl | RefDecl
and data_decl_kind = DataItem | DataTable | DataBlock | DataUnknown
and decl =
  | DError of parse_error
  | DVar of {
      name : ident;
      dtype : type_expr node;
      init : expr node option;
      storage : storage;
      external_modifier : external_modifier;
      data_decl_kind : data_decl_kind;
      is_readonly : bool;
    }
  | DConst of {
      name : ident;
      dtype : type_expr node option;
      value : expr node;
      external_modifier : external_modifier;
      data_decl_kind : data_decl_kind;
    }
  | DType of {
      name : ident;
      defn : type_expr node;
      external_modifier : external_modifier;
    }
  | DProc of proc node
  | DOverlay of overlay_decl
  | DDirective of { name : ident; args : string node list }

and proc = {
  name : ident;
  params : param node list;
  returns : type_expr node option;
  use_attr : proc_use;
  linkage : ident option;
  locals : decl node list;
  body : stmt node;
  external_modifier : external_modifier;
  has_body : bool;
  is_inline : bool;
}

type module_kind =
  | MainProgram of ident option
  | CompoolModule of ident option
  | ProcedureModule
  | UnknownModule

type toplevel =
  | TopDecl of decl node
  | TopStmt of stmt node
  | TopModule of jovial_module node
  | TopError of parse_error node

and jovial_module = {
  module_kind : module_kind;
  module_items : toplevel list;
}

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

  let rec pp_list ?(sep = ";") (pp : Format.formatter -> 'a -> unit) fmt
      (xs : 'a list) =
    match xs with
    | [] -> ()
    | [ x ] -> pp fmt x
    | x :: tl ->
        pp fmt x;
        Format.fprintf fmt "%s@ " sep;
        pp_list ~sep pp fmt tl

  type budget = { mutable nodes_left : int option }

  let take_node (b : budget) =
    match b.nodes_left with
    | None -> true
    | Some n ->
        if n <= 0 then false
        else (
          b.nodes_left <- Some (n - 1);
          true)

  let over_depth (opts : dump_opts) (depth : int) =
    match opts.max_depth with None -> false | Some md -> depth > md

  let pp_loc_if (opts : dump_opts) fmt (l : Loc.t) =
    if opts.show_locs then Format.fprintf fmt " @[%a@]" Loc.pp l else ()

  let pp_ident fmt (id : ident) = Format.fprintf fmt "%s" id.v

  let pp_parse_error fmt (err : parse_error) =
    let expected =
      match err.expected with [] -> "" | xs -> String.concat "," xs
    in
    let actual = Option.value err.actual ~default:"" in
    Format.fprintf fmt
      "{message=%S; expected=[%s]; actual=%S; recovery=%S}" err.message
      expected actual err.recovery_kind

  let pp_literal fmt = function
    | LInt s -> Format.fprintf fmt "Int(%s)" s
    | LFloat s -> Format.fprintf fmt "Float(%s)" s
    | LBit { bead_size; beads; raw } ->
        Format.fprintf fmt "Bit(size=%d; beads=%S; raw=%S)" bead_size beads raw
    | LString s -> Format.fprintf fmt "String(%S)" s
    | LChar c -> Format.fprintf fmt "Char(%C)" c
    | LBool b -> Format.fprintf fmt "Bool(%b)" b
    | LNull -> Format.pp_print_string fmt "NULL"

  let pp_round_mode fmt = function
    | Round -> Format.pp_print_string fmt "R"
    | Truncate -> Format.pp_print_string fmt "T"

  let pp_scalar_base fmt = function
    | ScalarUnsigned -> Format.pp_print_string fmt "U"
    | ScalarSigned -> Format.pp_print_string fmt "S"
    | ScalarFloat -> Format.pp_print_string fmt "F"
    | ScalarFixed -> Format.pp_print_string fmt "A"
    | ScalarBit -> Format.pp_print_string fmt "B"
    | ScalarChar -> Format.pp_print_string fmt "C"

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
    | BPow -> Format.pp_print_string fmt "BPow"
    | BAnd -> Format.pp_print_string fmt "BAnd"
    | BOr -> Format.pp_print_string fmt "BOr"
    | BBitAnd -> Format.pp_print_string fmt "BBitAnd"
    | BBitOr -> Format.pp_print_string fmt "BBitOr"
    | BBitXor -> Format.pp_print_string fmt "BBitXor"
    | BEqv -> Format.pp_print_string fmt "BEqv"
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
    else if over_depth opts depth then
      Format.pp_print_string fmt "<depth-limit>"
    else
      match t.v with
      | TName n ->
          Format.fprintf fmt "TName(%a)%a" pp_ident n (pp_loc_if opts) t.loc
      | TScalar { base; round; sizes } ->
          Format.fprintf fmt "@[TScalar(%a; round=%a; sizes=[%a])@]%a"
            pp_scalar_base base
            (pp_opt pp_round_mode) round
            (pp_list (pp_expr opts b (depth + 1)))
            sizes (pp_loc_if opts) t.loc
      | TPointer inner ->
          Format.fprintf fmt "@[TPointer(%a)@]%a"
            (pp_type_expr opts b (depth + 1))
            inner (pp_loc_if opts) t.loc
      | TArray { elem; dims } ->
          Format.fprintf fmt "@[TArray(elem=%a; dims=[%a])@]%a"
            (pp_type_expr opts b (depth + 1))
            elem
            (pp_list (pp_expr opts b (depth + 1)))
            dims (pp_loc_if opts) t.loc
      | TSpecifiedTable { elem; dims; kind } ->
          Format.fprintf fmt
            "@[TSpecifiedTable(elem=%a; dims=[%a]; kind=%a)@]%a"
            (pp_type_expr opts b (depth + 1))
            elem
            (pp_list (pp_expr opts b (depth + 1)))
            dims
            (pp_specified_table_kind opts b (depth + 1))
            kind (pp_loc_if opts) t.loc
      | TStatus values ->
          Format.fprintf fmt "@[TStatus([%a])@]%a"
            (pp_list (pp_status_value opts b (depth + 1)))
            values (pp_loc_if opts) t.loc
      | TRecord fields ->
          Format.fprintf fmt "@[TRecord([%a])@]%a"
            (pp_list (pp_field_decl opts b (depth + 1)))
            fields (pp_loc_if opts) t.loc
      | TFunc { params; returns } ->
          Format.fprintf fmt "@[TFunc(params=[%a]; returns=%a)@]%a"
            (pp_list (pp_param opts b (depth + 1)))
            params
            (pp_opt (pp_type_expr opts b (depth + 1)))
            returns (pp_loc_if opts) t.loc

  and pp_specified_table_kind opts b depth fmt = function
    | SpecTableW entry_size ->
        Format.fprintf fmt "W(%a)"
          (pp_expr opts b (depth + 1))
          entry_size
    | SpecTableV None -> Format.pp_print_string fmt "V"
    | SpecTableV (Some entry_size) ->
        Format.fprintf fmt "V(%a)"
          (pp_expr opts b (depth + 1))
          entry_size

  and pp_status_value opts b depth fmt (sv : status_value node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      let x = sv.v in
      Format.fprintf fmt "@[V(%a%a)@]%a" pp_ident x.sv_name
        (fun fmt -> function
          | None -> ()
          | Some rep ->
              Format.fprintf fmt " = %a"
                (pp_expr opts b (depth + 1))
                rep)
        x.sv_representation (pp_loc_if opts) sv.loc

  and pp_field_decl opts b depth fmt (f : field_decl node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      let x = f.v in
      Format.fprintf fmt "@[Field(%a : %a%a)@]%a" pp_ident x.fname
        (pp_type_expr opts b (depth + 1))
        x.ftype
        (fun fmt -> function
          | None -> ()
          | Some pos ->
              Format.fprintf fmt " POS(%a, %a)"
                (pp_expr opts b (depth + 1))
                pos.pos_start_bit
                (pp_expr opts b (depth + 1))
                pos.pos_start_word)
        x.fpos (pp_loc_if opts) f.loc

  and pp_param_mode fmt = function
    | In -> Format.pp_print_string fmt "In"
    | Out -> Format.pp_print_string fmt "Out"
    | InOut -> Format.pp_print_string fmt "InOut"

  and pp_param opts b depth fmt (p : param node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      let x = p.v in
      Format.fprintf fmt "@[Param(%a %a : %a)@]%a" pp_param_mode x.pmode
        pp_ident x.pname
        (pp_type_expr opts b (depth + 1))
        x.ptype (pp_loc_if opts) p.loc

  and pp_overlay_item opts b depth fmt (item : overlay_item node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      match item.v with
      | OverlayTarget id ->
          Format.fprintf fmt "OverlayTarget(%a)%a" pp_ident id
            (pp_loc_if opts) item.loc
      | OverlaySpacer expr ->
          Format.fprintf fmt "OverlaySpacer(%a)%a"
            (pp_expr opts b (depth + 1))
            expr (pp_loc_if opts) item.loc
      | OverlayGroup items ->
          Format.fprintf fmt "@[OverlayGroup([%a])@]%a"
            (pp_list (pp_overlay_item opts b (depth + 1)))
            items (pp_loc_if opts) item.loc

  and pp_expr opts b depth fmt (e : expr node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else if over_depth opts depth then
      Format.pp_print_string fmt "<depth-limit>"
    else
      match e.v with
      | EName id ->
          Format.fprintf fmt "EName(%a)%a" pp_ident id (pp_loc_if opts) e.loc
      | ELit lit ->
          Format.fprintf fmt "ELit(%a)%a" pp_literal lit (pp_loc_if opts) e.loc
      | EError err ->
          Format.fprintf fmt "EError(%a)%a" pp_parse_error err
            (pp_loc_if opts) e.loc
      | EMissing err ->
          Format.fprintf fmt "EMissing(%a)%a" pp_parse_error err
            (pp_loc_if opts) e.loc
      | EUnop { op; rhs } ->
          Format.fprintf fmt "@[EUnop(%a, %a)@]%a" pp_unop op
            (pp_expr opts b (depth + 1))
            rhs (pp_loc_if opts) e.loc
      | EBinop { op; lhs; rhs } ->
          Format.fprintf fmt "@[EBinop(%a, %a, %a)@]%a" pp_binop op
            (pp_expr opts b (depth + 1))
            lhs
            (pp_expr opts b (depth + 1))
            rhs (pp_loc_if opts) e.loc
      | ECall { callee; args } ->
          Format.fprintf fmt "@[ECall(%a, [%a])@]%a" pp_ident callee
            (pp_list (pp_expr opts b (depth + 1)))
            args (pp_loc_if opts) e.loc
      | EIndex { base; index } ->
          Format.fprintf fmt "@[EIndex(base=%a; index=[%a])@]%a"
            (pp_expr opts b (depth + 1))
            base
            (pp_list (pp_expr opts b (depth + 1)))
            index (pp_loc_if opts) e.loc
      | EField { base; field } ->
          Format.fprintf fmt "@[EField(%a.%a)@]%a"
            (pp_expr opts b (depth + 1))
            base pp_ident field (pp_loc_if opts) e.loc
      | EConvert { ty; expr } ->
          Format.fprintf fmt "@[EConvert(%a, %a)@]%a"
            (pp_type_expr opts b (depth + 1))
            ty
            (pp_expr opts b (depth + 1))
            expr (pp_loc_if opts) e.loc
      | EPreset { base; items } ->
          Format.fprintf fmt "@[EPreset(base=%a; items=[%a])@]%a"
            (pp_expr opts b (depth + 1))
            base
            (pp_list (pp_expr opts b (depth + 1)))
            items (pp_loc_if opts) e.loc
      | EOmitted ->
          Format.fprintf fmt "EOmitted%a" (pp_loc_if opts) e.loc
      | ERepeat { count; items } ->
          Format.fprintf fmt "@[ERepeat(count=%a; items=[%a])@]%a"
            (pp_expr opts b (depth + 1))
            count
            (pp_list (pp_expr opts b (depth + 1)))
            items (pp_loc_if opts) e.loc
      | EPositioned { indexes; values } ->
          Format.fprintf fmt "@[EPositioned(indexes=[%a]; values=[%a])@]%a"
            (pp_list (pp_expr opts b (depth + 1)))
            indexes
            (pp_list (pp_expr opts b (depth + 1)))
            values (pp_loc_if opts) e.loc
      | ERange { lo; hi } ->
          Format.fprintf fmt "@[ERange(%a, %a)@]%a"
            (pp_expr opts b (depth + 1))
            lo
            (pp_expr opts b (depth + 1))
            hi (pp_loc_if opts) e.loc
      | EAt { field; ptr } ->
          Format.fprintf fmt "@[EAt(%a @ %a)@]%a"
            (pp_expr opts b (depth + 1))
            field
            (pp_expr opts b (depth + 1))
            ptr (pp_loc_if opts) e.loc
      | EDeref { ptr } ->
          Format.fprintf fmt "@[EDeref(@ %a)@]%a"
            (pp_expr opts b (depth + 1))
            ptr (pp_loc_if opts) e.loc
      | EParen inner ->
          Format.fprintf fmt "@[EParen(%a)@]%a"
            (pp_expr opts b (depth + 1))
            inner (pp_loc_if opts) e.loc

  and pp_case_index opts b depth fmt (ci : case_index node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      match ci.v with
      | CaseDefault -> Format.fprintf fmt "DEFAULT%a" (pp_loc_if opts) ci.loc
      | CaseValue e ->
          Format.fprintf fmt "CaseValue(%a)%a"
            (pp_expr opts b (depth + 1)) e (pp_loc_if opts) ci.loc
      | CaseRange (lo, hi) ->
          Format.fprintf fmt "CaseRange(%a:%a)%a"
            (pp_expr opts b (depth + 1)) lo
            (pp_expr opts b (depth + 1)) hi
            (pp_loc_if opts) ci.loc

  and pp_case_option opts b depth fmt (co : case_option node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      let x = co.v in
      Format.fprintf fmt "@[CaseOption(indexes=[%a]; fallthru=%b; body=%a)@]%a"
        (pp_list (pp_case_index opts b (depth + 1))) x.case_indexes
        x.case_fallthru
        (pp_stmt opts b (depth + 1)) x.case_body
        (pp_loc_if opts) co.loc

  and pp_stmt opts b depth fmt (s : stmt node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else if over_depth opts depth then
      Format.pp_print_string fmt "<depth-limit>"
    else
      match s.v with
      | SEmpty -> Format.fprintf fmt "SEmpty%a" (pp_loc_if opts) s.loc
      | SError err ->
          Format.fprintf fmt "SError(%a)%a" pp_parse_error err
            (pp_loc_if opts) s.loc
      | SBlock xs ->
          Format.fprintf fmt "@[SBlock([%a])@]%a"
            (pp_list (pp_stmt opts b (depth + 1)))
            xs (pp_loc_if opts) s.loc
      | SDecl d ->
          Format.fprintf fmt "@[SDecl(%a)@]%a"
            (pp_decl opts b (depth + 1))
            d (pp_loc_if opts) s.loc
      | SAssign { lhs; rhs } ->
          Format.fprintf fmt "@[SAssign(lhs=%a; rhs=%a)@]%a"
            (pp_expr opts b (depth + 1))
            lhs
            (pp_expr opts b (depth + 1))
            rhs (pp_loc_if opts) s.loc
      | SCallStmt { callee; args; abort_label } ->
          Format.fprintf fmt "@[SCall(%a, [%a], abort=%a)@]%a" pp_ident callee
            (pp_list (pp_expr opts b (depth + 1)))
            args (pp_opt pp_ident) abort_label (pp_loc_if opts) s.loc
      | SIf { cond; then_; else_ } ->
          Format.fprintf fmt "@[SIf(cond=%a; then=%a; else=%a)@]%a"
            (pp_expr opts b (depth + 1))
            cond
            (pp_stmt opts b (depth + 1))
            then_
            (pp_opt (pp_stmt opts b (depth + 1)))
            else_ (pp_loc_if opts) s.loc
      | SWhile { cond; body } ->
          Format.fprintf fmt "@[SWhile(cond=%a; body=%a)@]%a"
            (pp_expr opts b (depth + 1))
            cond
            (pp_stmt opts b (depth + 1))
            body (pp_loc_if opts) s.loc
      | SFor { init; cond; step; body } ->
          Format.fprintf fmt "@[SFor(init=%a; cond=%a; step=%a; body=%a)@]%a"
            (pp_opt (pp_stmt opts b (depth + 1)))
            init
            (pp_opt (pp_expr opts b (depth + 1)))
            cond
            (pp_opt (pp_stmt opts b (depth + 1)))
            step
            (pp_stmt opts b (depth + 1))
            body (pp_loc_if opts) s.loc
      | SCase { selector; options } ->
          Format.fprintf fmt "@[SCase(selector=%a; options=[%a])@]%a"
            (pp_expr opts b (depth + 1)) selector
            (pp_list (pp_case_option opts b (depth + 1))) options
            (pp_loc_if opts) s.loc
      | SReturn eo ->
          Format.fprintf fmt "@[SReturn(%a)@]%a"
            (pp_opt (pp_expr opts b (depth + 1)))
            eo (pp_loc_if opts) s.loc
      | SLabel { label; body } ->
          Format.fprintf fmt "@[SLabel(%a: %a)@]%a" pp_ident label
            (pp_stmt opts b (depth + 1))
            body (pp_loc_if opts) s.loc
      | SGoto id ->
          Format.fprintf fmt "@[SGoto(%a)@]%a" pp_ident id (pp_loc_if opts)
            s.loc

  and pp_storage fmt = function
    | Automatic -> Format.pp_print_string fmt "Automatic"
    | Static -> Format.pp_print_string fmt "Static"
    | External -> Format.pp_print_string fmt "External"

  and pp_proc_use fmt = function
    | UseNormal -> Format.pp_print_string fmt "Normal"
    | UseRec -> Format.pp_print_string fmt "REC"
    | UseRent -> Format.pp_print_string fmt "RENT"

  and pp_external_modifier fmt = function
    | LocalDecl -> Format.pp_print_string fmt "Local"
    | DefDecl -> Format.pp_print_string fmt "DEF"
    | RefDecl -> Format.pp_print_string fmt "REF"

  and pp_data_decl_kind fmt = function
    | DataItem -> Format.pp_print_string fmt "ITEM"
    | DataTable -> Format.pp_print_string fmt "TABLE"
    | DataBlock -> Format.pp_print_string fmt "BLOCK"
    | DataUnknown -> Format.pp_print_string fmt "UNKNOWN"

  and pp_string_node fmt (s : string node) = Format.fprintf fmt "%S" s.v

  and pp_decl opts b depth fmt (d : decl node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      match d.v with
      | DError err ->
          Format.fprintf fmt "DError(%a)%a" pp_parse_error err
            (pp_loc_if opts) d.loc
      | DVar
          {
            name;
            dtype;
            init;
            storage;
            external_modifier;
            data_decl_kind;
            is_readonly;
          } ->
          Format.fprintf fmt
            "@[DVar(%a : %a; storage=%a; external=%a; data_kind=%a; readonly=%b; init=%a)@]%a"
            pp_ident name
            (pp_type_expr opts b (depth + 1))
            dtype pp_storage storage pp_external_modifier external_modifier
            pp_data_decl_kind data_decl_kind is_readonly
            (pp_opt (pp_expr opts b (depth + 1)))
            init (pp_loc_if opts) d.loc
      | DConst { name; dtype; value; external_modifier; data_decl_kind } ->
          Format.fprintf fmt
            "@[DConst(%a : %a; external=%a; data_kind=%a; value=%a)@]%a"
            pp_ident name
            (pp_opt (pp_type_expr opts b (depth + 1)))
            dtype pp_external_modifier external_modifier pp_data_decl_kind
            data_decl_kind
            (pp_expr opts b (depth + 1))
            value (pp_loc_if opts) d.loc
      | DType { name; defn; external_modifier } ->
          Format.fprintf fmt "@[DType(%a = %a; external=%a)@]%a" pp_ident name
            (pp_type_expr opts b (depth + 1))
            defn pp_external_modifier external_modifier (pp_loc_if opts) d.loc
      | DProc p -> pp_proc opts b (depth + 1) fmt p
      | DOverlay overlay ->
          Format.fprintf fmt "@[DOverlay(%a; pos=%a; items=[%a])@]%a"
            pp_ident overlay.overlay_name
            (pp_opt (pp_expr opts b (depth + 1)))
            overlay.overlay_pos
            (pp_list (pp_overlay_item opts b (depth + 1)))
            overlay.overlay_items (pp_loc_if opts) d.loc
      | DDirective { name; args } ->
          Format.fprintf fmt "@[DDirective(%a, [%a])@]%a" pp_ident name
            (pp_list pp_string_node) args (pp_loc_if opts) d.loc

  and pp_proc opts b depth fmt (p : proc node) =
    if not (take_node b) then Format.pp_print_string fmt "<...>"
    else
      let x = p.v in
      Format.fprintf fmt
        "@[DProc(name=%a; use=%a; linkage=%a; inline=%b; external=%a; has_body=%b; params=[%a]; \
         returns=%a; locals=[%a]; body=%a)@]%a"
        pp_ident x.name pp_proc_use x.use_attr (pp_opt pp_ident) x.linkage x.is_inline
        pp_external_modifier x.external_modifier x.has_body
        (pp_list (pp_param opts b (depth + 1)))
        x.params
        (pp_opt (pp_type_expr opts b (depth + 1)))
        x.returns
        (pp_list (pp_decl opts b (depth + 1)))
        x.locals
        (pp_stmt opts b (depth + 1))
        x.body (pp_loc_if opts) p.loc

  let pp_module_kind fmt = function
    | MainProgram None -> Format.pp_print_string fmt "MainProgram"
    | MainProgram (Some id) -> Format.fprintf fmt "MainProgram(%a)" pp_ident id
    | CompoolModule None -> Format.pp_print_string fmt "CompoolModule"
    | CompoolModule (Some id) -> Format.fprintf fmt "CompoolModule(%a)" pp_ident id
    | ProcedureModule -> Format.pp_print_string fmt "ProcedureModule"
    | UnknownModule -> Format.pp_print_string fmt "UnknownModule"

  let rec pp_toplevel opts b fmt = function
    | TopDecl d -> Format.fprintf fmt "TopDecl(%a)" (pp_decl opts b 0) d
    | TopStmt s -> Format.fprintf fmt "TopStmt(%a)" (pp_stmt opts b 0) s
    | TopError err ->
        Format.fprintf fmt "TopError(%a)%a" pp_parse_error err.v
          (pp_loc_if opts) err.loc
    | TopModule m ->
        Format.fprintf fmt "@[TopModule(kind=%a; items=[%a])@]"
          pp_module_kind m.v.module_kind
          (pp_list ~sep:", " (pp_toplevel opts b)) m.v.module_items

  let pp_program ?(opts = default_opts) fmt (p : program) =
    let b = { nodes_left = opts.max_nodes } in
    Format.fprintf fmt "@[<v>Program[@,%a@]@]"
      (pp_list ~sep:", " (fun fmt tl ->
           Format.fprintf fmt "@[<hov>%a@]" (pp_toplevel opts b) tl))
      p

  let to_string ?(opts = default_opts) (p : program) =
    Format.asprintf "%a" (pp_program ~opts) p
end
