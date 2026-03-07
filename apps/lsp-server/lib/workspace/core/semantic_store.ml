module T = Lsp.Types

type str_set = (string, bool) Hashtbl.t

type t = {
  snapshot_by_uri : (string, Doc_snapshot.t) Hashtbl.t;
  defs_by_sym_id : (string, Doc_snapshot.nav_def list) Hashtbl.t;
  occs_by_sym_id : (string, Doc_snapshot.nav_occ list) Hashtbl.t;
  sym_ids_by_key : (string, str_set) Hashtbl.t;
  reverse_imports_by_compool : (string, str_set) Hashtbl.t;
  doc_set_by_path_key : (string, str_set) Hashtbl.t;
  mutable global_rev : int;
}

let create () : t =
  {
    snapshot_by_uri = Hashtbl.create 256;
    defs_by_sym_id = Hashtbl.create 1024;
    occs_by_sym_id = Hashtbl.create 2048;
    sym_ids_by_key = Hashtbl.create 1024;
    reverse_imports_by_compool = Hashtbl.create 512;
    doc_set_by_path_key = Hashtbl.create 512;
    global_rev = 0;
  }

let reset (s : t) : unit =
  Hashtbl.clear s.snapshot_by_uri;
  Hashtbl.clear s.defs_by_sym_id;
  Hashtbl.clear s.occs_by_sym_id;
  Hashtbl.clear s.sym_ids_by_key;
  Hashtbl.clear s.reverse_imports_by_compool;
  Hashtbl.clear s.doc_set_by_path_key;
  s.global_rev <- 0

let global_rev (s : t) = s.global_rev
let uri_key (uri : T.DocumentUri.t) : string = Uri_path.docuri_to_string uri
let normalize_key (s : string) : string = String.uppercase_ascii (String.trim s)
let set_add (set : str_set) (k : string) : unit = Hashtbl.replace set k true
let set_remove (set : str_set) (k : string) : unit = Hashtbl.remove set k
let set_is_empty (set : str_set) : bool = Hashtbl.length set = 0

let set_for_key (tbl : (string, str_set) Hashtbl.t) (k : string) : str_set =
  match Hashtbl.find_opt tbl k with
  | Some s -> s
  | None ->
      let s = Hashtbl.create 8 in
      Hashtbl.add tbl k s;
      s

let nav_def_key (d : Doc_snapshot.nav_def) : string =
  Printf.sprintf "%s|%d|%d|%d|%d|%s" (uri_key d.uri)
    d.loc.Ast.Loc.start_pos.line d.loc.Ast.Loc.start_pos.col
    d.loc.Ast.Loc.end_pos.line d.loc.Ast.Loc.end_pos.col d.key

let nav_occ_key ((uri, loc) : Doc_snapshot.nav_occ) : string =
  Printf.sprintf "%s|%d|%d|%d|%d" (uri_key uri) loc.Ast.Loc.start_pos.line
    loc.Ast.Loc.start_pos.col loc.Ast.Loc.end_pos.line loc.Ast.Loc.end_pos.col

let dedupe_defs (xs : Doc_snapshot.nav_def list) : Doc_snapshot.nav_def list =
  let seen = Hashtbl.create (List.length xs + 1) in
  let acc = ref [] in
  List.iter
    (fun x ->
      let k = nav_def_key x in
      if not (Hashtbl.mem seen k) then (
        Hashtbl.add seen k true;
        acc := x :: !acc))
    xs;
  List.rev !acc

let dedupe_occs (xs : Doc_snapshot.nav_occ list) : Doc_snapshot.nav_occ list =
  let seen = Hashtbl.create (List.length xs + 1) in
  let acc = ref [] in
  List.iter
    (fun x ->
      let k = nav_occ_key x in
      if not (Hashtbl.mem seen k) then (
        Hashtbl.add seen k true;
        acc := x :: !acc))
    xs;
  List.rev !acc

let merge_defs (a : Doc_snapshot.nav_def list) (b : Doc_snapshot.nav_def list) :
    Doc_snapshot.nav_def list =
  dedupe_defs (a @ b)

let merge_occs (a : Doc_snapshot.nav_occ list) (b : Doc_snapshot.nav_occ list) :
    Doc_snapshot.nav_occ list =
  dedupe_occs (a @ b)

let rebuild_key_set (s : t) (key : string) : unit =
  let key = normalize_key key in
  if key = "" then ()
  else
    let set = Hashtbl.create 8 in
    Hashtbl.iter
      (fun sym_id defs ->
        if
          List.exists
            (fun (d : Doc_snapshot.nav_def) -> normalize_key d.key = key)
            defs
        then Hashtbl.replace set sym_id true)
      s.defs_by_sym_id;
    if set_is_empty set then Hashtbl.remove s.sym_ids_by_key key
    else Hashtbl.replace s.sym_ids_by_key key set

let uri_keys_of_set (set : str_set) : string list =
  Hashtbl.fold (fun k _ acc -> k :: acc) set []

let remove_uri_key (s : t) (uk : string) : unit =
  match Hashtbl.find_opt s.snapshot_by_uri uk with
  | None -> ()
  | Some old ->
      Hashtbl.remove s.snapshot_by_uri uk;

      (match old.path_key with
      | None -> ()
      | Some path_key -> (
          match Hashtbl.find_opt s.doc_set_by_path_key path_key with
          | None -> ()
          | Some set ->
              set_remove set uk;
              if set_is_empty set then
                Hashtbl.remove s.doc_set_by_path_key path_key));

      List.iter
        (fun (imp : Preprocess.import) ->
          match imp.kind with
          | Preprocess.Compool -> (
              let ck = normalize_key imp.name in
              if ck <> "" then
                match Hashtbl.find_opt s.reverse_imports_by_compool ck with
                | None -> ()
                | Some set ->
                    set_remove set uk;
                    if set_is_empty set then
                      Hashtbl.remove s.reverse_imports_by_compool ck))
        old.imports;

      List.iter
        (fun (sym_id, _) ->
          match Hashtbl.find_opt s.defs_by_sym_id sym_id with
          | None -> ()
          | Some defs ->
              let defs' =
                List.filter
                  (fun (d : Doc_snapshot.nav_def) -> uri_key d.uri <> uk)
                  defs
              in
              if defs' = [] then Hashtbl.remove s.defs_by_sym_id sym_id
              else Hashtbl.replace s.defs_by_sym_id sym_id defs')
        old.nav_defs;

      List.iter
        (fun (sym_id, _) ->
          match Hashtbl.find_opt s.occs_by_sym_id sym_id with
          | None -> ()
          | Some occs ->
              let occs' =
                List.filter
                  (fun ((u, _) : Doc_snapshot.nav_occ) -> uri_key u <> uk)
                  occs
              in
              if occs' = [] then Hashtbl.remove s.occs_by_sym_id sym_id
              else Hashtbl.replace s.occs_by_sym_id sym_id occs')
        old.nav_occs;

      List.iter (rebuild_key_set s) old.symbol_keys_touched;
      s.global_rev <- s.global_rev + 1

let remove_uri (s : t) ~(uri : T.DocumentUri.t) : unit =
  remove_uri_key s (uri_key uri)

let upsert_snapshot (s : t) (snap : Doc_snapshot.t) : unit =
  let uk = uri_key snap.uri in
  remove_uri_key s uk;
  Hashtbl.replace s.snapshot_by_uri uk snap;

  (match snap.path_key with
  | None -> ()
  | Some path_key ->
      let set = set_for_key s.doc_set_by_path_key path_key in
      set_add set uk);

  List.iter
    (fun (imp : Preprocess.import) ->
      match imp.kind with
      | Preprocess.Compool ->
          let ck = normalize_key imp.name in
          if ck <> "" then
            let set = set_for_key s.reverse_imports_by_compool ck in
            set_add set uk)
    snap.imports;

  List.iter
    (fun (sym_id, defn) ->
      let prev =
        match Hashtbl.find_opt s.defs_by_sym_id sym_id with
        | None -> []
        | Some xs -> xs
      in
      Hashtbl.replace s.defs_by_sym_id sym_id (merge_defs prev [ defn ]);
      let key = normalize_key defn.key in
      if key <> "" then
        let set = set_for_key s.sym_ids_by_key key in
        set_add set sym_id)
    snap.nav_defs;

  List.iter
    (fun (sym_id, occs) ->
      let prev =
        match Hashtbl.find_opt s.occs_by_sym_id sym_id with
        | None -> []
        | Some xs -> xs
      in
      Hashtbl.replace s.occs_by_sym_id sym_id (merge_occs prev occs))
    snap.nav_occs;

  List.iter (rebuild_key_set s) snap.symbol_keys_touched;
  s.global_rev <- s.global_rev + 1

let snapshot_for_uri (s : t) ~(uri : T.DocumentUri.t) : Doc_snapshot.t option =
  Hashtbl.find_opt s.snapshot_by_uri (uri_key uri)

let iter_snapshots (s : t) (f : Doc_snapshot.t -> unit) : unit =
  Hashtbl.iter (fun _ snap -> f snap) s.snapshot_by_uri

let compare_pos (a : T.Position.t) (b : T.Position.t) : int =
  if a.line < b.line then -1
  else if a.line > b.line then 1
  else if a.character < b.character then -1
  else if a.character > b.character then 1
  else 0

let pos_of_ast (p : Ast.Loc.pos) : T.Position.t =
  { T.Position.line = max 0 (p.line - 1); character = max 0 p.col }

let position_in_loc (pos : T.Position.t) (loc : Ast.Loc.t) : bool =
  let sp = pos_of_ast loc.start_pos in
  let ep = pos_of_ast loc.end_pos in
  compare_pos sp pos <= 0 && compare_pos pos ep <= 0

let loc_span_weight (loc : Ast.Loc.t) : int =
  let sp = loc.Ast.Loc.start_pos in
  let ep = loc.Ast.Loc.end_pos in
  let line_span = max 0 (ep.line - sp.line) in
  if line_span = 0 then max 0 (ep.col - sp.col)
  else (line_span * 10000) + max 0 ep.col

let resolve_symbol_at (s : t) ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    string option =
  match snapshot_for_uri s ~uri with
  | None -> None
  | Some snap -> (
      let best : (string * int) option ref = ref None in
      List.iter
        (fun (sym_id, occs) ->
          List.iter
            (fun ((u, loc) : Doc_snapshot.nav_occ) ->
              if uri_key u = uri_key uri && position_in_loc pos loc then
                let w = loc_span_weight loc in
                match !best with
                | None -> best := Some (sym_id, w)
                | Some (_, prev) when w < prev -> best := Some (sym_id, w)
                | _ -> ())
            occs)
        snap.nav_occs;
      match !best with None -> None | Some (sym_id, _) -> Some sym_id)

let defs_for_sym_id (s : t) (sym_id : string) : Doc_snapshot.nav_def list =
  match Hashtbl.find_opt s.defs_by_sym_id sym_id with
  | None -> []
  | Some xs -> xs

let refs_for_sym_id (s : t) (sym_id : string) : Doc_snapshot.nav_occ list =
  match Hashtbl.find_opt s.occs_by_sym_id sym_id with
  | None -> []
  | Some xs -> xs

let sym_ids_for_key (s : t) ~(key : string) : string list =
  let key = normalize_key key in
  if key = "" then []
  else
    match Hashtbl.find_opt s.sym_ids_by_key key with
    | None -> []
    | Some set -> uri_keys_of_set set |> List.sort_uniq String.compare

let starts_with ~(prefix : string) (s : string) : bool =
  let n = String.length s in
  let m = String.length prefix in
  n >= m && String.sub s 0 m = prefix

let symbols_for_prefix (s : t) ~(prefix : string) : string list =
  let prefix = normalize_key prefix in
  Hashtbl.fold
    (fun key _ acc ->
      if prefix = "" || starts_with ~prefix key then key :: acc else acc)
    s.sym_ids_by_key []
  |> List.sort_uniq String.compare

let uris_for_path_key (s : t) ~(path_key : string) : T.DocumentUri.t list =
  match Hashtbl.find_opt s.doc_set_by_path_key path_key with
  | None -> []
  | Some set ->
      uri_keys_of_set set
      |> List.filter_map (fun uk ->
          match Hashtbl.find_opt s.snapshot_by_uri uk with
          | None -> None
          | Some snap -> Some snap.uri)

let uris_importing_compool (s : t) ~(compool_key : string) :
    T.DocumentUri.t list =
  let compool_key = normalize_key compool_key in
  match Hashtbl.find_opt s.reverse_imports_by_compool compool_key with
  | None -> []
  | Some set ->
      uri_keys_of_set set
      |> List.filter_map (fun uk ->
          match Hashtbl.find_opt s.snapshot_by_uri uk with
          | None -> None
          | Some snap -> Some snap.uri)

let invalidate_path_and_dependents (s : t) ~(path_key : string) :
    T.DocumentUri.t list =
  let direct_exact =
    match Hashtbl.find_opt s.doc_set_by_path_key path_key with
    | None -> []
    | Some set -> uri_keys_of_set set
  in
  let direct =
    if direct_exact <> [] then direct_exact
    else
      let prefix =
        let n = String.length path_key in
        if n > 0 && path_key.[n - 1] = '/' then path_key else path_key ^ "/"
      in
      Hashtbl.fold
        (fun pk set acc ->
          if starts_with ~prefix pk then uri_keys_of_set set @ acc else acc)
        s.doc_set_by_path_key []
  in
  let to_remove = Hashtbl.create 32 in
  List.iter (fun uk -> Hashtbl.replace to_remove uk true) direct;

  let compools = Hashtbl.create 16 in
  List.iter
    (fun uk ->
      match Hashtbl.find_opt s.snapshot_by_uri uk with
      | None -> ()
      | Some snap -> (
          match snap.compool_def with
          | None -> ()
          | Some nm ->
              let key = normalize_key nm in
              if key <> "" then Hashtbl.replace compools key true))
    direct;

  Hashtbl.iter
    (fun ck _ ->
      match Hashtbl.find_opt s.reverse_imports_by_compool ck with
      | None -> ()
      | Some set ->
          Hashtbl.iter (fun uk _ -> Hashtbl.replace to_remove uk true) set)
    compools;

  let removed = ref [] in
  Hashtbl.iter
    (fun uk _ ->
      match Hashtbl.find_opt s.snapshot_by_uri uk with
      | None -> ()
      | Some snap -> removed := snap.uri :: !removed)
    to_remove;

  Hashtbl.iter (fun uk _ -> remove_uri_key s uk) to_remove;
  List.rev !removed
