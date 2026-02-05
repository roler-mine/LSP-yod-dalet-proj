(* server/lib/symbols.ml
   Drop-in replacement (v3): fixes UriTbl, Range.start field, option/list bugs,
   kind mismatches, and avoids Stdlib functions that may not exist in older OCaml.
*)

module T = Lsp.Types

(* -------------------------------------------------------------------------- *)
(* Small stdlib-compat helpers                                                *)
(* -------------------------------------------------------------------------- *)

let hd_opt = function [] -> None | x :: _ -> Some x

let rec find_opt f = function
  | [] -> None
  | x :: xs -> if f x then Some x else find_opt f xs

let hashtbl_find_opt (type a b) (tbl : (a, b) Hashtbl.t) (k : a) : b option =
  try Some (Hashtbl.find tbl k) with Not_found -> None

(* -------------------------------------------------------------------------- *)
(* Public types (match symbols.mli v1)                                        *)
(* -------------------------------------------------------------------------- *)

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

(* -------------------------------------------------------------------------- *)
(* Internal storage                                                            *)
(* -------------------------------------------------------------------------- *)

module UriTbl = Hashtbl.Make (struct
  type t = Lsp.Uri.t

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type t =
  { mutable next_id : int
  ; sym_by_id : (int, symbol) Hashtbl.t
  ; defs_by_name : (string, symbol list) Hashtbl.t
  ; syms_by_uri : symbol list UriTbl.t
  ; occs_by_uri : occurrence list UriTbl.t
  ; refs_by_target : (int, occurrence list) Hashtbl.t
  ; refs_by_name : (string, occurrence list) Hashtbl.t

  (* backward-compat helpers *)
  ; globals_order : symbol list ref
  ; procs_by_name : (string, symbol) Hashtbl.t
  ; locals_by_proc : (string, (string, symbol list) Hashtbl.t) Hashtbl.t
  }

let create () : t =
  { next_id = 1
  ; sym_by_id = Hashtbl.create 1024
  ; defs_by_name = Hashtbl.create 1024
  ; syms_by_uri = UriTbl.create 256
  ; occs_by_uri = UriTbl.create 256
  ; refs_by_target = Hashtbl.create 1024
  ; refs_by_name = Hashtbl.create 1024
  ; globals_order = ref []
  ; procs_by_name = Hashtbl.create 128
  ; locals_by_proc = Hashtbl.create 128
  }

let clear (st : t) : unit =
  st.next_id <- 1;
  Hashtbl.reset st.sym_by_id;
  Hashtbl.reset st.defs_by_name;
  UriTbl.reset st.syms_by_uri;
  UriTbl.reset st.occs_by_uri;
  Hashtbl.reset st.refs_by_target;
  Hashtbl.reset st.refs_by_name;
  st.globals_order := [];
  Hashtbl.reset st.procs_by_name;
  Hashtbl.reset st.locals_by_proc

let alloc_id (st : t) : int =
  let id = st.next_id in
  st.next_id <- st.next_id + 1;
  id

let add_to_name_index (tbl : (string, 'a list) Hashtbl.t) (name : string) (v : 'a) : unit =
  match hashtbl_find_opt tbl name with
  | None -> Hashtbl.add tbl name [ v ]
  | Some xs -> Hashtbl.replace tbl name (v :: xs)

let add_to_uri_index (tbl : 'a list UriTbl.t) (uri : Lsp.Uri.t) (v : 'a) : unit =
  match (try Some (UriTbl.find tbl uri) with Not_found -> None) with
  | None -> UriTbl.add tbl uri [ v ]
  | Some xs -> UriTbl.replace tbl uri (v :: xs)

let add_ref_target (st : t) (target_id : int) (occ : occurrence) : unit =
  match hashtbl_find_opt st.refs_by_target target_id with
  | None -> Hashtbl.add st.refs_by_target target_id [ occ ]
  | Some xs -> Hashtbl.replace st.refs_by_target target_id (occ :: xs)

(* -------------------------------------------------------------------------- *)
(* Span/range helpers                                                         *)
(* -------------------------------------------------------------------------- *)

let compare_pos (a : T.Position.t) (b : T.Position.t) : int =
  let c = Int.compare a.line b.line in
  if c <> 0 then c else Int.compare a.character b.character

let range_contains (r : T.Range.t) (p : T.Position.t) : bool =
  compare_pos r.start p <= 0 && compare_pos p r.end_ <= 0

let range_of_span (sp : Ast.span) : T.Range.t =
  (* delegate to the project-wide conversion to stay consistent *)
  Lsp_convert.range_of_span sp

let span_contains_pos (sp : Ast.span) (p : T.Position.t) : bool =
  range_contains (range_of_span sp) p

let range_len (r : T.Range.t) : int =
  (* not exact chars, but stable ordering *)
  ((r.end_.line - r.start.line) * 1_000_000) + (r.end_.character - r.start.character)

(* Choose the tightest (smallest) span that contains the position. *)
let choose_best_by_span (type a) (get_span : a -> Ast.span) (xs : a list) (p : T.Position.t) : a list =
  let ys = List.filter (fun x -> span_contains_pos (get_span x) p) xs in
  let ys =
    List.sort
      (fun a b ->
        let ra = range_of_span (get_span a) in
        let rb = range_of_span (get_span b) in
        Int.compare (range_len ra) (range_len rb))
      ys
  in
  ys

(* -------------------------------------------------------------------------- *)
(* Public adders                                                              *)
(* -------------------------------------------------------------------------- *)

let add_symbol
    (st : t)
    ~uri
    ?parent
    ?(container = [])
    ~name
    ~kind
    ~decl_span
    ~name_span
    ?ty
    ?doc
    () : symbol =
  let sid = alloc_id st in
  let sym =
    { sid
    ; name
    ; kind
    ; uri
    ; container
    ; parent
    ; decl_span
    ; name_span
    ; ty
    ; doc
    }
  in
  Hashtbl.add st.sym_by_id sid sym;
  add_to_name_index st.defs_by_name name sym;
  add_to_uri_index st.syms_by_uri uri sym;
  (* back-compat indices *)
  (match kind with
  | Proc | Function -> Hashtbl.replace st.procs_by_name name sym
  | _ -> ());
  (match container with
  | proc :: _ when kind = Local || kind = Param || kind = Item || kind = Label || kind = Field ->
      let tbl =
        match hashtbl_find_opt st.locals_by_proc proc with
        | Some t -> t
        | None ->
            let t = Hashtbl.create 64 in
            Hashtbl.add st.locals_by_proc proc t;
            t
      in
      add_to_name_index tbl name sym
  | _ -> ());
  sym

let add_occurrence
    (st : t)
    ~uri
    ?(container = [])
    ~name
    ~kind
    ~span
    ?target
    () : occurrence =
  let occ = { name; kind; uri; container; span; target } in
  add_to_uri_index st.occs_by_uri uri occ;
  add_to_name_index st.refs_by_name name occ;
  (match target with None -> () | Some id -> add_ref_target st id occ);
  occ

(* -------------------------------------------------------------------------- *)
(* Basic queries                                                              *)
(* -------------------------------------------------------------------------- *)

let find_symbol_by_id (st : t) (id : int) : symbol option =
  hashtbl_find_opt st.sym_by_id id

let find_defs (st : t) (name : string) : symbol list =
  match hashtbl_find_opt st.defs_by_name name with None -> [] | Some xs -> xs

let symbols_in_doc (st : t) (uri : Lsp.Uri.t) : symbol list =
  match (try Some (UriTbl.find st.syms_by_uri uri) with Not_found -> None) with
  | None -> []
  | Some xs -> xs

let toplevel_symbols_in_doc (st : t) (uri : Lsp.Uri.t) : symbol list =
  symbols_in_doc st uri
  |> List.filter (fun s -> s.parent = None && s.container = [])

let occurrences_in_doc (st : t) (uri : Lsp.Uri.t) : occurrence list =
  match (try Some (UriTbl.find st.occs_by_uri uri) with Not_found -> None) with
  | None -> []
  | Some xs -> xs

(* -------------------------------------------------------------------------- *)
(* Position queries                                                           *)
(* -------------------------------------------------------------------------- *)

let symbols_at (st : t) ~uri (pos : T.Position.t) : symbol list =
  pos |> choose_best_by_span (fun s -> s.name_span) (symbols_in_doc st uri)

let occurrences_at (st : t) ~uri (pos : T.Position.t) : occurrence list =
  pos |> choose_best_by_span (fun o -> o.span) (occurrences_in_doc st uri)

let definitions_at (st : t) ~uri (pos : T.Position.t) : symbol list =
  let occs = occurrences_at st ~uri pos in
  match hd_opt occs with
  | Some { target = Some id; _ } -> (
      match find_symbol_by_id st id with None -> [] | Some s -> [ s ])
  | Some occ ->
      (* fallback: name-based, with a light scope filter *)
      let defs = find_defs st occ.name in
      (match occ.container with
      | proc :: _ ->
          let local_defs =
            List.filter
              (fun s ->
                match s.container with
                | proc' :: _ -> String.equal proc proc'
                | [] -> false)
              defs
          in
          if local_defs <> [] then local_defs else defs
      | [] -> defs)
  | None ->
      (* maybe cursor is on a definition name span (no occurrence recorded) *)
      let syms = symbols_at st ~uri pos in
      match hd_opt syms with None -> [] | Some s -> [ s ]

let references_of_symbol (st : t) ~symbol_id : occurrence list =
  match hashtbl_find_opt st.refs_by_target symbol_id with None -> [] | Some xs -> xs

let references_of_name (st : t) (name : string) : occurrence list =
  match hashtbl_find_opt st.refs_by_name name with None -> [] | Some xs -> xs

(* -------------------------------------------------------------------------- *)
(* Helpers for handlers                                                       *)
(* -------------------------------------------------------------------------- *)

let symbol_decl_range (s : symbol) : T.Range.t = range_of_span s.decl_span
let symbol_name_range (s : symbol) : T.Range.t = range_of_span s.name_span

let sym_id (s : symbol) = s.sid
let sym_name (s : symbol) = s.name
let sym_kind (s : symbol) = s.kind
let sym_uri (s : symbol) = s.uri
let sym_container (s : symbol) = s.container
let sym_parent (s : symbol) = s.parent
let sym_decl_span (s : symbol) = s.decl_span
let sym_name_span (s : symbol) = s.name_span
let sym_ty (s : symbol) = s.ty
let sym_doc (s : symbol) = s.doc

(* -------------------------------------------------------------------------- *)
(* Backward-compat API                                                        *)
(* -------------------------------------------------------------------------- *)

type add_error =
  { existing : symbol
  ; attempted : symbol
  }

type add_result = (unit, add_error) result

let dummy_uri : Lsp.Uri.t =
  (* Avoid depending on a particular Uri representation; polymorphic Obj.magic
     is gross, but safer than guessing constructor availability.
     Prefer passing real [uri] via [add_symbol]. *)
  Obj.magic "file:///__jovial__/dummy"

let make
    ~name
    ~kind
    ~decl_span
    ~name_span
    ?ty
    ?doc
    ?container
    () : symbol =
  let container = match container with None -> [] | Some s -> [ s ] in
  { sid = -1
  ; name
  ; kind
  ; uri = dummy_uri
  ; container
  ; parent = None
  ; decl_span
  ; name_span
  ; ty
  ; doc
  }

let add_with_dedupe (st : t) ~(scope : [ `Global | `Proc of string ]) (sym : symbol) : add_result =
  let name = sym.name in

  let existing_opt =
    match scope with
    | `Global ->
        find_opt (fun s -> s.name = name && s.container = [] && s.parent = None) (find_defs st name)
    | `Proc proc -> (
        match hashtbl_find_opt st.locals_by_proc proc with
        | None -> None
        | Some tbl -> (
            match hashtbl_find_opt tbl name with
            | None -> None
            | Some (s :: _) -> Some s
            | Some [] -> None))
  in

  match existing_opt with
  | Some existing -> Error { existing; attempted = sym }
  | None ->
      let container =
        match scope with
        | `Global -> []
        | `Proc proc -> proc :: sym.container
      in
      ignore
        (add_symbol st ~uri:sym.uri ~name:sym.name ~kind:sym.kind ~decl_span:sym.decl_span
           ~name_span:sym.name_span ?ty:sym.ty ?doc:sym.doc ~container ());
      Ok ()

let add_global (st : t) (sym : symbol) : add_result =
  let res = add_with_dedupe st ~scope:`Global sym in
  (match res with Ok () -> st.globals_order := !(st.globals_order) @ [ sym ] | Error _ -> ());
  res

let add_proc (st : t) (sym : symbol) : add_result =
  (* Treat as global definition; also register proc name in procs_by_name in add_symbol. *)
  add_with_dedupe st ~scope:`Global sym

let add_in_proc (st : t) ~proc_name (sym : symbol) : add_result =
  add_with_dedupe st ~scope:(`Proc proc_name) sym

let find ?proc_name (st : t) (name : string) : symbol option =
  match proc_name with
  | Some proc -> (
      match hashtbl_find_opt st.locals_by_proc proc with
      | Some tbl -> (
          match hashtbl_find_opt tbl name with Some (s :: _) -> Some s | _ -> None)
      | None -> None)
  | None ->
      (* global: prefer container=[] *)
      find_opt (fun s -> s.container = [] && s.parent = None) (find_defs st name)

let globals_in_order (st : t) : symbol list = !(st.globals_order)

let all (st : t) : symbol list =
  Hashtbl.fold (fun _ s acc -> s :: acc) st.sym_by_id []
