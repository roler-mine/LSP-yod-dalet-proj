(** Module overview: Jovial type representation, formatting, and compatibility helpers. *)

type int_kind = Signed | Unsigned

type status_value = {
  name : string;
  loc : Ast.Loc.t option;
  representation : int option;
}

type dim_bound =
  | BoundInt of int
  | BoundStatus of string
  | BoundExpr of Ast.expr Ast.node
  | BoundUnknown

type dim = {
  lower : dim_bound option;
  upper : dim_bound option;
}

type t =
  | Unknown
  | Named of string
  | Integer of { kind : int_kind; bits : int option }
  | Float of { precision : int option }
  | Fixed of {
      scale : int option;
      fraction : int option;
    }
  | BitString of { bits : int option }
  | CharString of { chars : int option }
  | Status of { values : status_value list }
  | Pointer of {
      target : t option;
      typed : bool;
    }
  | Table of {
      dims : dim list;
      entry : t;
    }
  | Block of field list
  | Procedure of proc_signature

and field = {
  name : string;
  key : string;
  ty : t;
  loc : Ast.Loc.t;
}

and proc_signature = {
  params : param list;
  returns : t option;
  use_attr : Ast.proc_use;
}

and param = {
  param_name : string;
  param_key : string;
  mode : Ast.param_mode;
  param_ty : t;
  param_loc : Ast.Loc.t;
}

type type_env

val empty_type_env : unit -> type_env
val type_env_of_list : (string * Ast.type_expr Ast.node) list -> type_env
val add_type : type_env -> string -> Ast.type_expr Ast.node -> unit

val of_ast_type_expr : type_env -> Ast.type_expr Ast.node -> t
val display : t -> string
val display_with_config : Implementation_config.t -> t -> string
val compatible : lhs:t -> rhs:t -> bool
val conversion_required : lhs:t -> rhs:t -> bool
val field_type : t -> string -> t option
val table_entry_type : t -> t option
val pointer_target : t -> t option
