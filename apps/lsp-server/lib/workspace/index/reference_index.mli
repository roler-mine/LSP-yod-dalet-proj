(** Module overview: Collects symbol references for navigation, rename, codelens, and LSIF export. *)

type occurrence_kind =
  | Declaration
  | Definition
  | Reference
  | Read
  | Write
  | Call
  | TypeUse
  | ImportUse
  | MacroUse

type occurrence = {
  symbol_id : Symbol_index.symbol_id option;
  name : string;
  normalized_name : string;
  loc : Ast.Loc.t;
  scope_id : int;
  kind : occurrence_kind;
  confidence : [ `Exact | `Likely | `Unresolved ];
}

type t

val empty : unit -> t
val add : t -> occurrence -> unit
val of_skeleton : Symbol_index.t -> Skeleton_index.skeleton_file -> t
val by_symbol_id : t -> Symbol_index.symbol_id -> occurrence list
val by_name : t -> string -> occurrence list
val by_file : t -> string -> occurrence list
val by_scope : t -> int -> occurrence list
val by_kind : t -> occurrence_kind -> occurrence list
val all : t -> occurrence list
