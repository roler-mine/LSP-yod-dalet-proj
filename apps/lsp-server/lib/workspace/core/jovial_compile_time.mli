(** Module overview: Jovial compile-time expression evaluation helpers used by semantic checks. *)

type ctf_value =
  | CtfInt of int64
  | CtfFloat of string
  | CtfString of string
  | CtfChar of char
  | CtfBool of bool

type ctf_error_kind =
  | UnknownIdentifier of string
  | NonConstantReference of string
  | UnsupportedConstruct of string
  | UnsupportedBuiltin of string
  | TypeMismatch of string
  | DivideByZero
  | Overflow
  | UnsafeFloatComparison

type ctf_error = {
  loc : Ast.Loc.t;
  kind : ctf_error_kind;
  message : string;
}

type ctf_result = Known of ctf_value | Unknown of ctf_error list

type env

val empty_env : unit -> env
val copy_env : env -> env
val add_constant : env -> string -> Ast.expr Ast.node -> unit
val add_non_constant : env -> string -> unit
val add_impl_param : env -> string -> ctf_value -> unit
val add_implementation_config : env -> Implementation_config.t -> unit
val eval_expr : ?env:env -> Ast.expr Ast.node -> ctf_result
val int_value : ctf_result -> int64 option
val is_definitely_non_constant : ctf_result -> bool

val diagnostic_for_required :
  ?diagnose_unknown_identifiers:bool ->
  ?diagnose_unsupported_constructs:bool ->
  ?message:string ->
  ctf_result ->
  Ast.Loc.t ->
  Lsp.Types.Diagnostic.t option
