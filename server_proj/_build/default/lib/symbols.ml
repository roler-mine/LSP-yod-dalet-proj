(* server/lib/symbols.ml
   Drop-in Symbols module.

   Goals:
   - Keep a stable public API (see symbols.mli)
   - Avoid record-label ambiguity (symbol.kind vs occurrence.kind, etc.)
   - Stay compatible with older OCaml stdlib (no List.hd_opt, Hashtbl.find_opt, etc.)
*)

module T = Lsp.Types

(* ---------- Public types (must match symbols.mli) ---------- *)

type kind =
  | Module
  | Compool
  | ICompool
  | Proc
  | Function
  | Type
  | Status
  | Label
  | Table
  | Item
  | Block
  | Param
  | Field
  | Constant
  | Local
  | Define

type occurrence_kind =
  | Ref
  | Read
  | Write
  | Call
  | TypeRef
  | LabelRef

type symbol =
  { sid : int
  ; name : string
  ; kind : kind
  ; uri : Lsp.Uri.t
  ; container : string list
  ; parent : int option
  ; decl_span : Ast.span
  ; name_span : Ast.span
  ; ty : string option
  ; doc : string option
  }

type occurrence =
  { name : string
  ; kind : occurrence_kind
  ; uri : Lsp.Uri.t
  ; container : string list
  ; span : Ast.span
  ; target : int option
  }

(* ---------- Small compat helpers (avoid newer stdlib) ---------- *)

let hd_opt = function
  | [] -> None
  | x :: _ -> Some x

let rec find_opt (f : 'a -> bool) (xs : 'a list) : 'a option =
  match xs with
  | [] -> None
  | x :: rest -> if f x then Some x else find_opt f rest

let hashtbl_find_opt tbl k =
  try Some (Hashtbl.find tbl k) with
  | Not_found -> None

let uritbl_find_opt (tbl : 'a UriTbl.t) (k : Lsp.Uri.t) : 'a option =
  try Some (UriTbl.find tbl k) with
  | Not_found -> None

let hashtbl_replace_list tbl k v =
  match hashtbl_find_opt tbl k with
  | None -> Hashtbl.replace tbl k [ v ]
  | Some xs -> Hashtbl.replace tbl k (v :: xs)

(* ---------- Uri hash table ---------- *)

module UriKey = struct
  type t = Lsp.Uri.t
  let equal = ( = )
  let hash = Hashtbl.hash
end

module UriTbl = Hashtbl.Make (UriKey)

(* ---------- Internal storage ---------- *)

type t =
  { mutable next_id : int
  ; by_id : (int, symbol) Hashtbl.t
  ; defs_by_name : (string, int list) Hashtbl.t
  ; syms_by_uri : (Lsp.Uri.t, int list) UriTbl.t
  ; occs_by_uri : (Lsp.Uri.t, occurrence list) UriTbl.t
  ; occs_by_target : (int, occurrence list) Hashtbl.t
  ; occs_by_name : (string, occurrence list) Hashtbl.t
  }

let create () : t =
  { next_id = 1
  ; by_id = Hashtbl.create 2048
  ; defs_by_name = Hashtbl.create 2048
  ; syms_by_uri = UriTbl.create 256
  ; occs_by_uri = UriTbl.create 256
  ; occs_by_target = Hashtbl.create 2048
  ; occs_by_name = Hashtbl.create 2048
  }

let clear (st : t) : unit =
  st.next_id <- 1;
  Hashtbl.clear st.by_id;
  Hashtbl.clear st.defs_by_name;
  UriTbl.clear st.syms_by_uri;
  UriTbl.clear st.occs_by_uri;
  Hashtbl.clear st.occs_by_target;
  Hashtbl.clear st.occs_by_name

(* ---------- Span -> LSP range helpers ---------- *)

let lsp_pos_of_lex (p : Lexing.position) : T.Position.t =
  (* Lexing.pos_lnum is 1-based; LSP is 0-based. *)
  let line = max 0 (p.pos_lnum - 1) in
  let character = max 0 (p.pos_cnum - p.pos_bol) in
  ({ line; character } : T.Position.t)

let range_of_span (sp : Ast.span) : T.Range.t =
  let start = lsp_pos_of_lex sp.sp_start in
  let end_ = lsp_pos_of_lex sp.sp_end in
  ({ start; end_ } : T.Range.t)

let pos_leq (a : T.Position.t) (b : T.Position.t) : bool =
  (a.line < b.line) || (a.line = b.line && a.character <= b.character)

let pos_geq (a : T.Position.t) (b : T.Position.t) : bool =
  (a.line > b.line) || (a.line = b.line && a.character >= b.character)

let range_contains_pos (r : T.Range.t) (p : T.Position.t) : bool =
  (* Use inclusive end because we often want cursor-on-token to match. *)
  pos_geq p r.start && pos_leq p r.end_

(* ---------- Public adders ---------- *)

let add_symbol
    (st : t)
    ~(uri : Lsp.Uri.t)
    ?parent
    ?(container = [])
    ~(name : string)
    ~(kind : kind)
    ~(decl_span : Ast.span)
    ~(name_span : Ast.span)
    ?ty
    ?doc
    ()
  : symbol
  =
  let sid = st.next_id in
  st.next_id <- st.next_id + 1;
  let sym : symbol =
    { sid; name; kind; uri; container; parent; decl_span; name_span; ty; doc }
  in
  Hashtbl.replace st.by_id sid sym;

  (* Index by name (definitions) *)
  let ids = match hashtbl_find_opt st.defs_by_name name with
    | None -> []
    | Some xs -> xs
  in
  Hashtbl.replace st.defs_by_name name (sid :: ids);

  (* Index by uri (symbols in a doc) *)
  let doc_ids = match uritbl_find_opt st.syms_by_uri uri with
    | exception Not_found -> []
    | xs -> xs
  in
  UriTbl.replace st.syms_by_uri uri (sid :: doc_ids);

  sym

let add_occurrence
    (st : t)
    ~(uri : Lsp.Uri.t)
    ?(container = [])
    ~(name : string)
    ~(kind : occurrence_kind)
    ~(span : Ast.span)
    ?target
    ()
  : occurrence
  =
  let occ : occurrence = { name; kind; uri; container; span; target } in

  (* Index by uri *)
  let existing =
    match uritbl_find_opt st.occs_by_uri uri with
    | exception Not_found -> []
    | xs -> xs
  in
  UriTbl.replace st.occs_by_uri uri (occ :: existing);

  (* Index by name *)
  hashtbl_replace_list st.occs_by_name name occ;

  (* Index by target (fast references) *)
  (match target with
   | None -> ()
   | Some sid -> hashtbl_replace_list st.occs_by_target sid occ);

  occ

(* ---------- Basic queries ---------- *)

let find_symbol_by_id (st : t) (sid : int) : symbol option =
  hashtbl_find_opt st.by_id sid

let find_defs (st : t) (name : string) : symbol list =
  match hashtbl_find_opt st.defs_by_name name with
  | None -> []
  | Some ids ->
    List.fold_left
      (fun acc sid ->
         match hashtbl_find_opt st.by_id sid with
         | None -> acc
         | Some s -> s :: acc)
      []
      ids
    |> List.rev

let symbols_in_doc (st : t) (uri : Lsp.Uri.t) : symbol list =
  let ids =
    match uritbl_find_opt st.syms_by_uri uri with
    | exception Not_found -> []
    | xs -> xs
  in
  List.fold_left
    (fun acc sid ->
       match hashtbl_find_opt st.by_id sid with
       | None -> acc
       | Some s -> s :: acc)
    []
    ids
  |> List.rev

let toplevel_symbols_in_doc (st : t) (uri : Lsp.Uri.t) : symbol list =
  symbols_in_doc st uri |> List.filter (fun (s : symbol) -> s.parent = None)

let occurrences_in_doc (st : t) (uri : Lsp.Uri.t) : occurrence list =
  match uritbl_find_opt st.occs_by_uri uri with
  | exception Not_found -> []
  | xs -> xs

(* ---------- Position queries ---------- *)

let symbol_name_range (s : symbol) : T.Range.t =
  range_of_span s.name_span

let symbol_decl_range (s : symbol) : T.Range.t =
  range_of_span s.decl_span

let symbols_at (st : t) ~(uri : Lsp.Uri.t) (pos : T.Position.t) : symbol list =
  symbols_in_doc st uri
  |> List.filter (fun (s : symbol) -> range_contains_pos (symbol_name_range s) pos)

let occurrences_at (st : t) ~(uri : Lsp.Uri.t) (pos : T.Position.t) : occurrence list =
  occurrences_in_doc st uri
  |> List.filter (fun (o : occurrence) -> range_contains_pos (range_of_span o.span) pos)

let definitions_at (st : t) ~(uri : Lsp.Uri.t) (pos : T.Position.t) : symbol list =
  match occurrences_at st ~uri pos with
  | occ :: _ ->
    (* Fast-path: resolved target id *)
    (match occ.target with
     | Some sid -> (match sym_by_id st sid with None -> [] | Some s -> [ s ])
     | None ->
       (* Fallback: name-based, prefer same file defs if present *)
       let defs = find_defs st occ.name in
       let same_doc = List.filter (fun (s : symbol) -> s.uri = uri) defs in
       if same_doc <> [] then same_doc else defs)
  | [] ->
    (* Cursor might be on a declared symbol name (not an occurrence) *)
    match symbols_at st ~uri pos with
    | s :: _ -> [ s ]
    | [] -> []

(* ---------- References ---------- *)

let references_of_symbol (st : t) ~(symbol_id : int) : occurrence list =
  match hashtbl_find_opt st.occs_by_target symbol_id with
  | None -> []
  | Some xs -> List.rev xs

let references_of_name (st : t) (name : string) : occurrence list =
  match hashtbl_find_opt st.occs_by_name name with
  | None -> []
  | Some xs -> List.rev xs



(* ---------- Public query API (matches symbols.mli) ---------- *)

let find_symbol_by_id (st : t) (sid : int) : symbol option =
  sym_by_id st sid

let find_symbols_by_name (st : t) (name : string) : symbol list =
  defs_by_name st name

let find_symbols_in_uri (st : t) (uri : Lsp.Uri.t) : symbol list =
  symbols_in_doc st uri

let find_toplevel_symbols_in_uri (st : t) (uri : Lsp.Uri.t) : symbol list =
  toplevel_symbols_in_doc st uri

let find_occurrences_in_uri (st : t) (uri : Lsp.Uri.t) : occurrence list =
  occurrences_in_doc st uri

let add_occurrences (st : t) (occs : occurrence list) : unit =
  List.iter (fun o -> ignore (add_occurrence st o)) occs

let locate_symbol_in_doc (st : t) (uri : Lsp.Uri.t) (pos : T.Position.t) : symbol list =
  symbols_at st ~uri pos

let locate_occurrence_in_doc (st : t) (uri : Lsp.Uri.t) (pos : T.Position.t) : occurrence option =
  match occurrences_at st ~uri pos with
  | [] -> None
  | o :: _ -> Some o

let locate_defs_in_doc (st : t) (uri : Lsp.Uri.t) (pos : T.Position.t) : symbol list =
  definitions_at st ~uri pos

let locate_def_in_doc (st : t) (uri : Lsp.Uri.t) (pos : T.Position.t) : symbol option =
  match locate_defs_in_doc st uri pos with
  | [] -> None
  | s :: _ -> Some s

let all_uris (st : t) : Lsp.Uri.t list =
  let add_uri uri acc = if List.exists ((=) uri) acc then acc else uri :: acc in
  let acc = UriTbl.fold (fun uri _ acc -> add_uri uri acc) st.sym_ids_by_uri [] in
  UriTbl.fold (fun uri _ acc -> add_uri uri acc) st.occs_by_uri acc

let locate_symbol (st : t) (pos : T.Position.t) : symbol list =
  let rec loop acc = function
    | [] -> List.rev acc
    | uri :: rest ->
      let here = locate_symbol_in_doc st uri pos in
      loop (List.rev_append here acc) rest
  in
  loop [] (all_uris st)

let locate_occurrence (st : t) (pos : T.Position.t) : occurrence option =
  let rec loop = function
    | [] -> None
    | uri :: rest ->
      (match locate_occurrence_in_doc st uri pos with
       | Some o -> Some o
       | None -> loop rest)
  in
  loop (all_uris st)

let locate_defs (st : t) (pos : T.Position.t) : symbol list =
  let rec loop acc = function
    | [] -> List.rev acc
    | uri :: rest ->
      let here = locate_defs_in_doc st uri pos in
      loop (List.rev_append here acc) rest
  in
  loop [] (all_uris st)

let locate_def (st : t) (pos : T.Position.t) : symbol option =
  match locate_defs st pos with
  | [] -> None
  | s :: _ -> Some s

let occurrences_of_symbol (st : t) (sid : int) : occurrence list =
  match hashtbl_find_opt st.occs_by_target sid with
  | None -> []
  | Some xs -> List.rev xs

let references_of_symbol (st : t) (sid : int) : occurrence list =
  occurrences_of_symbol st sid
  |> List.filter (fun (o : occurrence) -> o.kind = Ref)
(* ---------- Accessors (compat / clarity) ---------- *)

let symbol_decl_range (s : symbol) : T.Range.t =
  range_of_span s.decl_span

let symbol_name_range (s : symbol) : T.Range.t =
  range_of_span s.name_span

let symbol_uri (s : symbol) : Lsp.Uri.t =
  s.uri

let symbol_name (s : symbol) : string =
  s.name

let symbol_kind (s : symbol) : kind =
  s.kind

let symbol_container (s : symbol) : string list =
  s.container

let symbol_parent (s : symbol) : int option =
  s.parent

let occurrence_range (o : occurrence) : T.Range.t =
  range_of_span o.span

let occurrence_uri (o : occurrence) : Lsp.Uri.t =
  o.uri

let occurrence_name (o : occurrence) : string =
  o.name

let occurrence_kind (o : occurrence) : occurrence_kind =
  o.kind

let occurrence_container (o : occurrence) : string list =
  o.container

let occurrence_target (o : occurrence) : int option =
  o.target

let kind_to_lsp_symbol_kind : kind -> T.SymbolKind.t = function
  | Module -> T.SymbolKind.Module
  | Compool -> T.SymbolKind.Namespace
  | ICompool -> T.SymbolKind.Namespace
  | Proc -> T.SymbolKind.Function
  | Func -> T.SymbolKind.Function
  | Type -> T.SymbolKind.Struct
  | Table -> T.SymbolKind.Class
  | Constant -> T.SymbolKind.Constant
  | Variable -> T.SymbolKind.Variable
  | Label -> T.SymbolKind.Key
  | Param -> T.SymbolKind.Variable
  | Field -> T.SymbolKind.Field
  | Unknown -> T.SymbolKind.Object


(* Backward-compat API (older handlers/analyze code paths)                    *)
(* -------------------------------------------------------------------------- *)

type add_error =
  { existing : symbol
  ; attempted : symbol
  }

type add_result = (unit, add_error) result

let make
    ~(name : string)
    ~(kind : kind)
    ~(decl_span : Ast.span)
    ~(name_span : Ast.span)
    ?ty
    ?doc
    ?container
    ()
  : symbol
  =
  let uri = Lsp.Uri.of_string "file:///__dummy__" in
  let container = match container with None -> [] | Some c -> [ c ] in
  { sid = -1
  ; name
  ; kind
  ; uri
  ; container
  ; parent = None
  ; decl_span
  ; name_span
  ; ty
  ; doc
  }

let add_global (st : t) (sym : symbol) : add_result =
  (* very small dedupe: same name & kind at toplevel *)
  match find_defs st sym.name |> find_opt (fun (s : symbol) -> s.kind = sym.kind) with
  | Some existing -> Error { existing; attempted = sym }
  | None ->
    ignore
      (add_symbol st
         ~uri:sym.uri
         ?parent:sym.parent
         ~container:sym.container
         ~name:sym.name
         ~kind:sym.kind
         ~decl_span:sym.decl_span
         ~name_span:sym.name_span
         ?ty:sym.ty
         ?doc:sym.doc
         ());
    Ok ()

let add_proc (st : t) (sym : symbol) : add_result =
  add_global st sym

let add_in_proc (st : t) ~(proc_name : string) (sym : symbol) : add_result =
  ignore proc_name;
  add_global st sym

let find ?proc_name (st : t) (name : string) : symbol option =
  ignore proc_name;
  hd_opt (find_defs st name)

let globals_in_order (st : t) : symbol list =
  (* Preserves approximate insertion order by sid. *)
  Hashtbl.fold (fun _ s acc -> s :: acc) st.by_id []
  |> List.sort (fun (a : symbol) (b : symbol) -> compare a.sid b.sid)

let all (st : t) : symbol list =
  globals_in_order st
