type symbol_id = string
type import_id = int

type scope_kind =
  | SystemScope
  | CompoolScope
  | ModuleScope
  | ModuleBodyScope
  | ProcedureScope
  | FunctionScope
  | BlockScope
  | TableScope
  | TypeScope
  | LoopScope
  | MacroScope

type scope = {
  id : int;
  parent : int option;
  kind : scope_kind;
  name : string option;
  loc : Ast.Loc.t;
  symbols : symbol_id list;
  imports : import_id list;
}

type t = { by_id : (int, scope) Hashtbl.t }

let empty () = { by_id = Hashtbl.create 32 }

let add_scope (t : t) (scope : scope) : t =
  Hashtbl.replace t.by_id scope.id scope;
  t

let loc_contains_pos (loc : Ast.Loc.t) (pos : Ast.Loc.pos) : bool =
  let sp = loc.Ast.Loc.start_pos in
  let ep = loc.Ast.Loc.end_pos in
  (pos.line > sp.line || (pos.line = sp.line && pos.col >= sp.col))
  && (pos.line < ep.line || (pos.line = ep.line && pos.col <= ep.col))

let scope_span_width (scope : scope) =
  max 0 (scope.loc.Ast.Loc.end_pos.offset - scope.loc.Ast.Loc.start_pos.offset)

let scopes (t : t) : scope list =
  Hashtbl.fold (fun _ scope acc -> scope :: acc) t.by_id []
  |> List.sort (fun a b -> compare a.id b.id)

let by_id t id = Hashtbl.find_opt t.by_id id

let innermost_scope_at (t : t) (pos : Ast.Loc.pos) : scope option =
  scopes t
  |> List.filter (fun scope -> loc_contains_pos scope.loc pos)
  |> List.sort (fun a b -> compare (scope_span_width a) (scope_span_width b))
  |> List.find_opt (fun _ -> true)

let rec resolve_order (t : t) ~(scope_id : int) : int list =
  match Hashtbl.find_opt t.by_id scope_id with
  | None -> [ 0 ]
  | Some scope -> (
      match scope.parent with
      | None -> [ scope.id ]
      | Some parent -> scope.id :: resolve_order t ~scope_id:parent)

let kind_of_symbol (sym : Skeleton_index.symbol_decl) : scope_kind option =
  match sym.kind with
  | Skeleton_index.Compool -> Some CompoolScope
  | Skeleton_index.Module | Skeleton_index.Program -> Some ModuleScope
  | Skeleton_index.Procedure -> Some ProcedureScope
  | Skeleton_index.Function -> Some FunctionScope
  | Skeleton_index.Block -> Some BlockScope
  | Skeleton_index.Table -> Some TableScope
  | Skeleton_index.Type -> Some TypeScope
  | Skeleton_index.Define -> Some MacroScope
  | _ -> None

let symbol_id ~(uri : string) (sym : Skeleton_index.symbol_decl) =
  Printf.sprintf "%s:%s:%d:%d" uri sym.normalized_name
    sym.loc.Ast.Loc.start_pos.offset sym.loc.Ast.Loc.end_pos.offset

let string_contains_substring haystack needle =
  let h = String.length haystack in
  let n = String.length needle in
  if n = 0 then true
  else
    let rec loop i =
      i + n <= h
      && (String.sub haystack i n = needle || loop (i + 1))
    in
    loop 0

let symbol_id_matches_name ~(normalized_name : string) (id : symbol_id) : bool =
  string_contains_substring id (":" ^ normalized_name ^ ":")

let lookup_symbol_id t ~(scope_id : int) ~(normalized_name : string) :
    symbol_id option =
  resolve_order t ~scope_id
  |> List.find_map (fun id ->
         match by_id t id with
         | None -> None
         | Some scope ->
             scope.symbols
             |> List.find_opt (symbol_id_matches_name ~normalized_name))

let module_scope_kind = function
  | Skeleton_index.CompoolModule -> CompoolScope
  | Skeleton_index.MainProgram
  | Skeleton_index.ProcedureModule
  | Skeleton_index.UnknownModule ->
      ModuleScope

let module_loc (sk : Skeleton_index.skeleton_file) =
  Skeleton_index.symbols sk
  |> List.find_map (fun (sym : Skeleton_index.symbol_decl) ->
         match sym.Skeleton_index.kind with
         | Skeleton_index.Program
         | Skeleton_index.Module
         | Skeleton_index.Compool
         | Skeleton_index.ExternalDef
         | Skeleton_index.ExternalRef ->
             Some sym.loc
         | _ -> None)
  |> Option.value ~default:Ast.Loc.none

let of_skeleton (sk : Skeleton_index.skeleton_file) : t =
  let t = empty () in
  let root =
    {
      id = 0;
      parent = None;
      kind = SystemScope;
      name = None;
      loc = Ast.Loc.none;
      symbols = [];
      imports = [];
    }
  in
  ignore (add_scope t root);
  let module_id = 1 in
  let body_id = 2 in
  let module_scope =
    {
      id = module_id;
      parent = Some 0;
      kind = module_scope_kind sk.module_kind;
      name = sk.module_name;
      loc = module_loc sk;
      symbols = [];
      imports = [];
    }
  in
  ignore (add_scope t module_scope);
  let body_symbols =
    Skeleton_index.symbols sk |> List.map (symbol_id ~uri:sk.uri)
  in
  ignore
    (add_scope t
       {
         id = body_id;
         parent = Some module_id;
         kind = ModuleBodyScope;
         name = sk.module_name;
         loc = Ast.Loc.none;
         symbols = body_symbols;
         imports = List.mapi (fun i _ -> i) sk.imports;
       });
  let next = ref 3 in
  Skeleton_index.symbols sk
  |> List.iter (fun sym ->
         match kind_of_symbol sym with
         | None -> ()
         | Some kind ->
             let id = !next in
             incr next;
             ignore
               (add_scope t
                  {
                    id;
                    parent = Some body_id;
                    kind;
                    name = Some sym.name;
                    loc = sym.loc;
                    symbols = [ symbol_id ~uri:sk.uri sym ];
                    imports = [];
                  }));
  t
