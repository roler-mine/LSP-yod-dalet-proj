type type_info = Symbol_index.type_info

type binding_info = {
  symbol_id : Symbol_index.symbol_id;
  confidence : [ `Exact | `Likely | `Unresolved ];
}

type reference_info = {
  symbol_id : Symbol_index.symbol_id option;
  occurrence : Reference_index.occurrence_kind;
  confidence : [ `Exact | `Likely | `Unresolved ];
}

type t = {
  binding_by_node : (int, binding_info) Hashtbl.t;
  reference_by_node : (int, reference_info) Hashtbl.t;
  type_by_node : (int, type_info) Hashtbl.t;
  scope_by_node : (int, int) Hashtbl.t;
}

val empty : unit -> t
val of_skeleton : Symbol_index.t -> Skeleton_index.skeleton_file -> t
