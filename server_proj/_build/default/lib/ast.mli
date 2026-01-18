(* server/lib/ast.mli *)

type span = private { startp : Lexing.position; endp : Lexing.position }

type 'a located = private
  { value : 'a
  ; span : span
  }

val located : startp:Lexing.position -> endp:Lexing.position -> 'a -> 'a located
val map_loc : ('a -> 'b) -> 'a located -> 'b located

val loc_value : 'a located -> 'a
val loc_span : 'a located -> span

type ident = string located
val ident : startp:Lexing.position -> endp:Lexing.position -> string -> ident

type scalar_type =
  | TyInt | TyReal | TyBool | TyChar | TyString | TyUnknown

type type_expr =
  | Scalar of scalar_type
  | Named of ident
  | Array of type_expr located * int option

type binop =
  | Add | Sub | Mul | Div
  | Eq | Ne | Lt | Le | Gt | Ge
  | And | Or

type unop = Neg | Not

type expr =
  | IntLit of int
  | RealLit of float
  | StringLit of string
  | CharLit of char
  | BoolLit of bool
  | Var of ident
  | Call of ident * expr located list
  | Unary of unop * expr located
  | Binary of binop * expr located * expr located

type stmt =
  | Assign of ident * expr located
  | If of expr located * stmt located list * stmt located list
  | While of expr located * stmt located list
  | CallStmt of ident * expr located list
  | Return of expr located option
  | Block of stmt located list

type item_decl =
  { name : ident
  ; ty : type_expr located option
  ; init : expr located option
  }

type table_decl =
  { name : ident
  ; elem_ty : type_expr located option
  ; size : int option
  }

type proc_decl =
  { name : ident
  ; params : (ident * type_expr located option) list
  ; returns : type_expr located option
  ; body : stmt located list
  }

type decl =
  | Item of item_decl
  | Table of table_decl
  | Proc of proc_decl

type compilation_unit =
  { decls : decl located list
  }
