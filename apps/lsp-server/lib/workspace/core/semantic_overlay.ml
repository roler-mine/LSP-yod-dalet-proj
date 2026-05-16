(* Module overview: Overlay layer for semantic information that can refresh independently of base indexes. *)

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

let empty () =
  {
    binding_by_node = Hashtbl.create 64;
    reference_by_node = Hashtbl.create 64;
    type_by_node = Hashtbl.create 64;
    scope_by_node = Hashtbl.create 64;
  }

let synthetic_node_key (loc : Ast.Loc.t) =
  Hashtbl.hash (loc.Ast.Loc.file, loc.start_pos.offset, loc.end_pos.offset)

let of_skeleton (symbols : Symbol_index.t) (sk : Skeleton_index.skeleton_file) :
    t =
  let t = empty () in
  Skeleton_index.symbols sk
  |> List.iter (fun sym ->
         match Symbol_index.by_name symbols sym.Skeleton_index.normalized_name with
         | record :: _ ->
             let node_key = synthetic_node_key sym.loc in
             Hashtbl.replace t.binding_by_node node_key
               { symbol_id = record.Symbol_index.id; confidence = `Likely };
             Hashtbl.replace t.scope_by_node node_key sym.scope_id
         | [] -> ());
  t
