(* Module overview: Maintains searchable symbol definitions and names across the workspace. *)

type symbol_id = Symbol_id.t

type type_info = { display : string }

type symbol_record = {
  id : symbol_id;
  name : string;
  normalized_name : string;
  kind : Skeleton_index.symbol_kind;
  declaration : Ast.Loc.t;
  definition : Ast.Loc.t option;
  container_scope : int;
  exported : bool;
  external_kind : [ `Def | `Ref | `Local | `System ];
  metadata : Workspace_symbol_metadata.jovial_symbol_metadata;
  type_info : type_info option;
  docs : string option;
}

type t = {
  by_id_tbl : (symbol_id, symbol_record) Hashtbl.t;
  by_name_tbl : (string, symbol_record list) Hashtbl.t;
  by_file_tbl : (string, symbol_record list) Hashtbl.t;
  by_scope_tbl : (int, symbol_record list) Hashtbl.t;
  exports_by_module_tbl : (string, symbol_record list) Hashtbl.t;
}

let empty () =
  {
    by_id_tbl = Hashtbl.create 512;
    by_name_tbl = Hashtbl.create 512;
    by_file_tbl = Hashtbl.create 128;
    by_scope_tbl = Hashtbl.create 64;
    exports_by_module_tbl = Hashtbl.create 128;
  }

let normalize_name (s : string) = String.uppercase_ascii (String.trim s)

let add_to tbl key value =
  let prev = Option.value (Hashtbl.find_opt tbl key) ~default:[] in
  Hashtbl.replace tbl key (value :: prev)

let add (t : t) (record : symbol_record) : unit =
  Hashtbl.replace t.by_id_tbl record.id record;
  add_to t.by_name_tbl record.normalized_name record;
  let file_key = Option.value record.declaration.Ast.Loc.file ~default:"" in
  add_to t.by_file_tbl file_key record;
  add_to t.by_scope_tbl record.container_scope record;
  if record.exported then add_to t.exports_by_module_tbl file_key record

let stable_symbol_key ~(uri : string) (sym : Skeleton_index.symbol_decl) =
  Printf.sprintf "snapshot-symbol|%s|%s|%d|%d" uri sym.normalized_name
    sym.loc.Ast.Loc.start_pos.offset sym.loc.Ast.Loc.end_pos.offset

let symbol_id_of_skeleton ~(uri : string) (sym : Skeleton_index.symbol_decl) =
  Symbol_id.of_stable_string (stable_symbol_key ~uri sym)

let external_kind (sym : Skeleton_index.symbol_decl) =
  if sym.exported then `Def else if sym.imported then `Ref else `Local

let add_skeleton_symbol t ~uri (sym : Skeleton_index.symbol_decl) =
  add t
    {
      id = symbol_id_of_skeleton ~uri sym;
      name = sym.name;
      normalized_name = sym.normalized_name;
      kind = sym.kind;
      declaration = sym.loc;
      definition = None;
      container_scope = sym.scope_id;
      exported = sym.exported;
      external_kind = external_kind sym;
      metadata = sym.metadata;
      type_info = None;
      docs = None;
    }

let of_skeleton (sk : Skeleton_index.skeleton_file) : t =
  let t = empty () in
  Skeleton_index.symbols sk |> List.iter (add_skeleton_symbol t ~uri:sk.uri);
  t

let sorted xs =
  List.sort
    (fun a b ->
      match String.compare a.normalized_name b.normalized_name with
      | 0 -> compare a.declaration.Ast.Loc.start_pos.offset b.declaration.Ast.Loc.start_pos.offset
      | n -> n)
    xs

let by_id (t : t) id = Hashtbl.find_opt t.by_id_tbl id

let by_name (t : t) name =
  Option.value (Hashtbl.find_opt t.by_name_tbl (normalize_name name)) ~default:[]
  |> sorted

let by_prefix (t : t) prefix =
  let prefix = normalize_name prefix in
  Hashtbl.fold
    (fun name records acc ->
      if String.starts_with ~prefix name then records @ acc else acc)
    t.by_name_tbl []
  |> sorted

let by_file (t : t) file =
  Option.value (Hashtbl.find_opt t.by_file_tbl file) ~default:[] |> sorted

let by_scope (t : t) scope_id =
  Option.value (Hashtbl.find_opt t.by_scope_tbl scope_id) ~default:[] |> sorted

let exports_by_module (t : t) module_key =
  Option.value (Hashtbl.find_opt t.exports_by_module_tbl module_key) ~default:[]
  |> sorted

let all (t : t) =
  Hashtbl.fold (fun _ record acc -> record :: acc) t.by_id_tbl [] |> sorted
