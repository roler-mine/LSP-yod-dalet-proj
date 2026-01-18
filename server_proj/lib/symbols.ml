(* server/lib/symbols.ml *)

module T = Lsp.Types

type kind =
  | Item
  | Table
  | Proc
  | Param
  | Local

type symbol =
  { name : string
  ; kind : kind
  ; decl_span : Ast.span
  ; name_span : Ast.span
  ; ty : string option
  ; doc : string option
  ; container : string option
  }

let make ~name ~kind ~decl_span ~name_span ?ty ?doc ?container () : symbol =
  { name; kind; decl_span; name_span; ty; doc; container }

type add_error =
  { existing : symbol
  ; attempted : symbol
  }

type add_result = (unit, add_error) result

type scope =
  { table : (string, symbol) Hashtbl.t
  ; order : symbol list ref
  }

type t =
  { globals : (string, symbol) Hashtbl.t
  ; global_order : symbol list ref
  ; proc_scopes : (string, scope) Hashtbl.t
  }

let create () : t =
  { globals = Hashtbl.create 101
  ; global_order = ref []
  ; proc_scopes = Hashtbl.create 31
  }

let globals_in_order (idx : t) : symbol list =
  List.rev !(idx.global_order)

let scope_symbols_in_order (sc : scope) : symbol list =
  List.rev !(sc.order)

let proc_scope_opt (idx : t) (proc_name : string) : scope option =
  Hashtbl.find_opt idx.proc_scopes proc_name

let ensure_proc_scope (idx : t) (proc_name : string) : scope =
  match Hashtbl.find_opt idx.proc_scopes proc_name with
  | Some sc -> sc
  | None ->
      let sc = { table = Hashtbl.create 101; order = ref [] } in
      Hashtbl.add idx.proc_scopes proc_name sc;
      sc

let add_to_table ~(tbl : (string, symbol) Hashtbl.t) ~(order : symbol list ref) (sym : symbol)
  : add_result =
  match Hashtbl.find_opt tbl sym.name with
  | Some existing -> Error { existing; attempted = sym }
  | None ->
      Hashtbl.add tbl sym.name sym;
      order := sym :: !order;
      Ok ()

let add_global (idx : t) (sym : symbol) : add_result =
  add_to_table ~tbl:idx.globals ~order:idx.global_order sym

let add_proc (idx : t) (sym : symbol) : add_result =
  match add_global idx sym with
  | Error e -> Error e
  | Ok () ->
      ignore (ensure_proc_scope idx sym.name : scope);
      Ok ()

let add_in_proc (idx : t) ~(proc_name : string) (sym : symbol) : add_result =
  let sc = ensure_proc_scope idx proc_name in
  add_to_table ~tbl:sc.table ~order:sc.order sym

let find ?proc_name (idx : t) (name : string) : symbol option =
  match proc_name with
  | None -> Hashtbl.find_opt idx.globals name
  | Some p -> (
      match proc_scope_opt idx p with
      | Some sc -> (
          match Hashtbl.find_opt sc.table name with
          | Some s -> Some s
          | None -> Hashtbl.find_opt idx.globals name)
      | None -> Hashtbl.find_opt idx.globals name)

(* avoid List.concat_map (only exists from OCaml 4.10+) :contentReference[oaicite:0]{index=0} *)
let rec concat_map (f : 'a -> 'b list) (xs : 'a list) : 'b list =
  match xs with
  | [] -> []
  | x :: tl -> f x @ concat_map f tl

let all (idx : t) : symbol list =
  let globals = globals_in_order idx in
  let locals =
    globals
    |> List.filter (fun s -> match s.kind with Proc -> true | _ -> false)
    |> concat_map (fun p ->
           match proc_scope_opt idx p.name with
           | None -> []
           | Some sc -> scope_symbols_in_order sc)
  in
  globals @ locals

let symbol_decl_range (s : symbol) : T.Range.t =
  Lsp_convert.range_of_lex s.decl_span.startp s.decl_span.endp

let symbol_name_range (s : symbol) : T.Range.t =
  Lsp_convert.range_of_lex s.name_span.startp s.name_span.endp

(* accessors (used by your older handlers.ml) *)
let sym_name (s : symbol) = s.name
let sym_kind (s : symbol) = s.kind
let sym_decl_span (s : symbol) = s.decl_span
let sym_name_span (s : symbol) = s.name_span
let sym_ty (s : symbol) = s.ty
let sym_doc (s : symbol) = s.doc
let sym_container (s : symbol) = s.container
