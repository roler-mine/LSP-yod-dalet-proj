(* Module overview: Collects symbol references for navigation, rename, codelens, and LSIF export. *)

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

type t = {
  by_symbol_id_tbl : (Symbol_index.symbol_id, occurrence list) Hashtbl.t;
  by_name_tbl : (string, occurrence list) Hashtbl.t;
  by_file_tbl : (string, occurrence list) Hashtbl.t;
  by_scope_tbl : (int, occurrence list) Hashtbl.t;
  by_kind_tbl : (occurrence_kind, occurrence list) Hashtbl.t;
}

let empty () =
  {
    by_symbol_id_tbl = Hashtbl.create 512;
    by_name_tbl = Hashtbl.create 512;
    by_file_tbl = Hashtbl.create 128;
    by_scope_tbl = Hashtbl.create 64;
    by_kind_tbl = Hashtbl.create 16;
  }

let add_to tbl key value =
  let prev = Option.value (Hashtbl.find_opt tbl key) ~default:[] in
  Hashtbl.replace tbl key (value :: prev)

let add (t : t) (occ : occurrence) : unit =
  (match occ.symbol_id with None -> () | Some id -> add_to t.by_symbol_id_tbl id occ);
  add_to t.by_name_tbl occ.normalized_name occ;
  add_to t.by_file_tbl (Option.value occ.loc.Ast.Loc.file ~default:"") occ;
  add_to t.by_scope_tbl occ.scope_id occ;
  add_to t.by_kind_tbl occ.kind occ

let normalize_name s = String.uppercase_ascii (String.trim s)

let sorted xs =
  List.sort
    (fun a b ->
      match String.compare a.normalized_name b.normalized_name with
      | 0 -> compare a.loc.Ast.Loc.start_pos.offset b.loc.Ast.Loc.start_pos.offset
      | n -> n)
    xs

let of_skeleton (symbols : Symbol_index.t) (sk : Skeleton_index.skeleton_file) :
    t =
  let t = empty () in
  Skeleton_index.symbols sk
  |> List.iter (fun (sym : Skeleton_index.symbol_decl) ->
         let candidates = Symbol_index.by_name symbols sym.normalized_name in
         let symbol_id =
           candidates
           |> List.find_opt (fun record ->
                  record.Symbol_index.declaration = sym.loc)
           |> Option.map (fun record -> record.Symbol_index.id)
         in
         let kind =
           match sym.Skeleton_index.kind with
           | Skeleton_index.Define -> MacroUse
           | Skeleton_index.ExternalDef -> Definition
           | Skeleton_index.ExternalRef -> ImportUse
           | _ -> Declaration
         in
         add t
           {
             symbol_id;
             name = sym.name;
             normalized_name = normalize_name sym.name;
             loc = sym.loc;
             scope_id = sym.scope_id;
             kind;
             confidence = (if symbol_id = None then `Likely else `Exact);
           });
  List.iter
    (fun (imp : Skeleton_index.import) ->
      add t
        {
          symbol_id = None;
          name = imp.name;
          normalized_name = normalize_name imp.name;
          loc = imp.loc;
          scope_id = 0;
          kind = ImportUse;
          confidence = `Likely;
        })
    sk.imports;
  t

let by_symbol_id t id =
  Option.value (Hashtbl.find_opt t.by_symbol_id_tbl id) ~default:[] |> sorted

let by_name t name =
  Option.value (Hashtbl.find_opt t.by_name_tbl (normalize_name name)) ~default:[]
  |> sorted

let by_file t file =
  Option.value (Hashtbl.find_opt t.by_file_tbl file) ~default:[] |> sorted

let by_scope t scope_id =
  Option.value (Hashtbl.find_opt t.by_scope_tbl scope_id) ~default:[] |> sorted

let by_kind t kind =
  Option.value (Hashtbl.find_opt t.by_kind_tbl kind) ~default:[] |> sorted

let all t =
  Hashtbl.fold (fun _ occs acc -> occs @ acc) t.by_name_tbl [] |> sorted
