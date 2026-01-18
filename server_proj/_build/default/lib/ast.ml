(* server/lib/ast.ml *)

type span =
  { startp : Lexing.position
  ; endp : Lexing.position
  }

let span ~startp ~endp : span = { startp; endp }

let dummy_span : span =
  { startp = Lexing.dummy_pos
  ; endp = Lexing.dummy_pos
  }

type 'a located =
  { value : 'a
  ; span : span
  }

let located ~startp ~endp (value : 'a) : 'a located =
  { value; span = { startp; endp } }

let map_loc (f : 'a -> 'b) (x : 'a located) : 'b located =
  { value = f x.value; span = x.span }

type ident = string located

let ident ~startp ~endp (s : string) : ident =
  located ~startp ~endp s

(* Convenience accessors (optional; exported only if in ast.mli) *)
let loc_value (x : 'a located) = x.value
let loc_span (x : 'a located) = x.span

(* ---- Types (minimal now; extend later as grammar grows) ---- *)

type scalar_type =
  | TyInt
  | TyReal
  | TyBool
  | TyChar
  | TyString
  | TyUnknown

type type_expr =
  | Scalar of scalar_type
  | Named of ident
  | Array of type_expr located * int option

(* ---- Expressions ---- *)

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

(* ---- Statements ---- *)

type stmt =
  | Assign of ident * expr located
  | If of expr located * stmt located list * stmt located list
  | While of expr located * stmt located list
  | CallStmt of ident * expr located list
  | Return of expr located option
  | Block of stmt located list

(* ---- Declarations ---- *)

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
