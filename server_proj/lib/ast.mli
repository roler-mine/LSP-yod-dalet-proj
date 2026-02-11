(** lib/ast.mli
    Core AST + source locations + debug dump/pretty-printing.
*)

module Loc : sig
  type pos = { line:int; col:int; offset:int }
  type t = { file:string option; start_pos:pos; end_pos:pos }

  val none : t

  val make : start_pos:pos -> end_pos:pos -> file:string option -> t
  val make_no_file : start_pos:pos -> end_pos:pos -> t

  val of_lexing_positions : Lexing.position -> Lexing.position -> file:string option -> t
  val of_lexing_positions_no_file : Lexing.position -> Lexing.position -> t

  val start_offset : t -> int
  val end_offset : t -> int

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end



type 'a node = { loc : Loc.t; v : 'a }

val node : ?loc:Loc.t -> 'a -> 'a node
val map : ('a -> 'b) -> 'a node -> 'b node
val value : 'a node -> 'a
val loc : 'a node -> Loc.t

type ident = string node

type literal =
  | LInt of string
  | LFloat of string
  | LString of string
  | LChar of char
  | LBool of bool

type unop =
  | UPlus
  | UMinus
  | UNot
  | UBitNot

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

and field_decl = {
  fname : ident;
  ftype : type_expr node;
}

and param_mode = In | Out | InOut

and param = {
  pname : ident;
  pmode : param_mode;
  ptype : type_expr node;
}

and expr =
  | EName of ident
  | ELit of literal
  | EUnop of { op : unop; rhs : expr node }
  | EBinop of { op : binop; lhs : expr node; rhs : expr node }
  | ECall of { callee : ident; args : expr node list }
  | EIndex of { base : expr node; index : expr node list }
  | EField of { base : expr node; field : ident }
  | EAt of { field : expr node; ptr : expr node } 
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

and storage =
  | Automatic
  | Static
  | External

and decl =
  | DVar of {
      name : ident;
      dtype : type_expr node;
      init : expr node option;
      storage : storage;
    }
  | DConst of {
      name : ident;
      dtype : type_expr node option;
      value : expr node;
    }
  | DType of {
      name : ident;
      defn : type_expr node;
    }
  | DProc of proc node
  | DDirective of { name : ident; args : string node list }

and proc = {
  name : ident;
  params : param node list;
  returns : type_expr node option;
  locals : decl node list;
  body : stmt node;
}

type toplevel =
  | TopDecl of decl node
  | TopStmt of stmt node

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
