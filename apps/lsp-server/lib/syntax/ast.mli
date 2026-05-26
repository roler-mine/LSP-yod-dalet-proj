(** Module overview: Jovial AST data model with locations, recovery metadata, and debug renderers. *)

(** lib/ast.mli Core AST + source locations + debug dump/pretty-printing. *)

module Loc : sig
  type pos = { line : int; col : int; offset : int }
  type t = { file : string option; start_pos : pos; end_pos : pos }

  val none : t
  val make : start_pos:pos -> end_pos:pos -> file:string option -> t
  val make_no_file : start_pos:pos -> end_pos:pos -> t

  val of_lexing_positions :
    Lexing.position -> Lexing.position -> file:string option -> t

  val of_lexing_positions_no_file : Lexing.position -> Lexing.position -> t
  val start_offset : t -> int
  val end_offset : t -> int
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

type node_id = int

type token_range = {
  first_token : int;
  last_token : int;
}

type recovery =
  | Complete
  | Missing of { expected : string list; message : string }
  | Invalid of { diagnostics : Lsp.Types.Diagnostic.t list }

type 'a node = {
  id : node_id;
  loc : Loc.t;
  tokens : token_range option;
  hash : int64 option;
  recovery : recovery;
  v : 'a;
}

val node :
  ?loc:Loc.t ->
  ?tokens:token_range ->
  ?hash:int64 ->
  ?recovery:recovery ->
  'a ->
  'a node

val missing_node :
  ?loc:Loc.t ->
  ?tokens:token_range ->
  expected:string list ->
  message:string ->
  'a ->
  'a node

val invalid_node :
  ?loc:Loc.t ->
  ?tokens:token_range ->
  diagnostics:Lsp.Types.Diagnostic.t list ->
  'a ->
  'a node

val map : ('a -> 'b) -> 'a node -> 'b node
val value : 'a node -> 'a
val loc : 'a node -> Loc.t
val id : 'a node -> node_id
val recovery : 'a node -> recovery
val token_range : 'a node -> token_range option
val is_recovered : 'a node -> bool

type ident = string node

type parse_error = {
  message : string;
  expected : string list;
  actual : string option;
  recovery_kind : string;
}

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
  | TScalar of {
      base : scalar_base;
      round : round_mode option;
      sizes : expr node list;
    }
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

module Debug : sig
  type dump_opts = {
    show_locs : bool;
    max_depth : int option;
    max_nodes : int option;
  }

  val default_opts : dump_opts
  val pp_program : ?opts:dump_opts -> Format.formatter -> program -> unit
  val to_string : ?opts:dump_opts -> program -> string
end
