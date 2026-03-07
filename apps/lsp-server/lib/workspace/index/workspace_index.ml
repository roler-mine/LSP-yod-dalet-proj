type t = {
  root : string;
  compools : (string, string) Hashtbl.t; (* NAME -> filepath *)
  sources : (string, string) Hashtbl.t; (* normalized path -> source filepath *)
  sources_by_dir : (string, (string, bool) Hashtbl.t) Hashtbl.t;
      (* normalized dir -> normalized source path keys *)
  file_compool_keys : (string, string) Hashtbl.t;
      (* normalized path -> COMPOOL key *)
  source_import_hints : (string, string list) Hashtbl.t;
      (* normalized path -> imported compool keys *)
  source_entry_hints : (string, bool) Hashtbl.t;
      (* normalized path -> entrypoint hint *)
  pending_dirs : string Queue.t;
  seen_dirs : (string, bool) Hashtbl.t;
  mutable complete : bool;
  mutable checkpoint_loaded : bool;
  mutable reconcile_pending : bool;
  mutable reconcile_epoch : int;
  reconcile_seen_epoch : (string, int) Hashtbl.t;
  mutable reconcile_sources_before : int;
  mutable reconcile_sources_after : int;
  mutable reconcile_stale_pruned : int;
  checkpoint_file : string;
  resume_spot_file : string;
  mutable checkpoint_dirty : bool;
  mutable checkpoint_dirty_ops : int;
}

type file_change_kind = Created | Changed | Deleted

let checkpoint_version = 4
let checkpoint_write_every_ops = 200

let normalize_path_key (p : string) : string =
  let p = String.map (fun c -> if c = '\\' then '/' else c) p in
  if Sys.win32 then String.lowercase_ascii p else p

let normalize_key (s : string) : string = String.uppercase_ascii (String.trim s)

let workspace_cache_key (root : string) : string =
  Digest.to_hex (Digest.string (normalize_path_key root))

let checkpoint_file_path ~(root : string) : string =
  let tmp = Filename.get_temp_dir_name () in
  let key = workspace_cache_key root in
  Filename.concat tmp ("jovial-lsp-index-checkpoint-" ^ key ^ ".json")

let resume_spot_file_path ~(root : string) : string =
  let tmp = Filename.get_temp_dir_name () in
  let key = workspace_cache_key root in
  Filename.concat tmp ("jovial-lsp-index-resume-" ^ key ^ ".txt")

let root t = t.root
let compool_count t = Hashtbl.length t.compools
let source_count t = Hashtbl.length t.sources
let checkpoint_loaded t = t.checkpoint_loaded
let reconcile_pending t = t.reconcile_pending
let reconcile_epoch t = t.reconcile_epoch
let reconcile_sources_before t = t.reconcile_sources_before
let reconcile_sources_after t = t.reconcile_sources_after
let reconcile_stale_pruned t = t.reconcile_stale_pruned

let source_import_hints (t : t) ~(path : string) : string list =
  let path_key = normalize_path_key path in
  match Hashtbl.find_opt t.source_import_hints path_key with
  | Some xs -> xs
  | None -> []

let source_entry_hint (t : t) ~(path : string) : bool =
  let path_key = normalize_path_key path in
  match Hashtbl.find_opt t.source_entry_hints path_key with
  | Some b -> b
  | None -> false

let source_entry_paths (t : t) : string list =
  Hashtbl.fold
    (fun path_key is_entry acc ->
      if is_entry then
        match Hashtbl.find_opt t.sources path_key with
        | Some path -> path :: acc
        | None -> acc
      else acc)
    t.source_entry_hints []

let mark_checkpoint_dirty (t : t) : unit =
  t.checkpoint_dirty <- true;
  t.checkpoint_dirty_ops <- t.checkpoint_dirty_ops + 1

let queue_to_list (q : string Queue.t) : string list =
  let acc = ref [] in
  Queue.iter (fun x -> acc := x :: !acc) q;
  List.rev !acc

let clear_queue (q : 'a Queue.t) : unit =
  while not (Queue.is_empty q) do
    ignore (Queue.pop q)
  done

let write_text_atomic (path : string) (text : string) : unit =
  let tmp = path ^ ".tmp" in
  let oc = open_out_bin tmp in
  try
    output_string oc text;
    close_out oc;
    (try Sys.remove path with _ -> ());
    Sys.rename tmp path
  with exn ->
    close_out_noerr oc;
    (try Sys.remove tmp with _ -> ());
    raise exn

let read_all_text (path : string) : string option =
  try
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let txt = really_input_string ic len in
    close_in_noerr ic;
    Some txt
  with _ -> None

let write_resume_spot (t : t) (dir : string) : unit =
  try write_text_atomic t.resume_spot_file dir with _ -> ()

let clear_resume_spot (t : t) : unit =
  try Sys.remove t.resume_spot_file with _ -> ()

let read_resume_spot (t : t) : string option =
  match read_all_text t.resume_spot_file with
  | None -> None
  | Some s ->
      let s = String.trim s in
      if s = "" then None else Some s

let int_of_json (j : Yojson.Safe.t) : int option =
  match j with
  | `Int n -> Some n
  | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
  | _ -> None

let bool_of_json_default (j : Yojson.Safe.t option) ~(default : bool) : bool =
  match j with Some (`Bool b) -> b | _ -> default

let string_of_json (j : Yojson.Safe.t) : string option =
  match j with `String s -> Some s | _ -> None

let string_list_of_json (j : Yojson.Safe.t) : string list =
  match j with `List xs -> xs |> List.filter_map string_of_json | _ -> []

let string_pairs_of_json (j : Yojson.Safe.t) : (string * string) list =
  let parse_pair = function
    | `List [ `String k; `String v ] -> Some (k, v)
    | `Assoc [ ("k", `String k); ("v", `String v) ] -> Some (k, v)
    | _ -> None
  in
  match j with `List xs -> List.filter_map parse_pair xs | _ -> []

let json_of_string_pairs (tbl : (string, string) Hashtbl.t) : Yojson.Safe.t =
  let xs =
    Hashtbl.fold (fun k v acc -> `List [ `String k; `String v ] :: acc) tbl []
  in
  `List xs

let checkpoint_json (t : t) : Yojson.Safe.t =
  let pending_dirs = queue_to_list t.pending_dirs in
  let seen_dirs =
    Hashtbl.fold (fun k _ acc -> `String k :: acc) t.seen_dirs []
  in
  let sources_by_dir_json =
    let dirs =
      Hashtbl.fold
        (fun dir set acc ->
          let paths =
            Hashtbl.fold (fun path_key _ xs -> `String path_key :: xs) set []
            |> List.rev
          in
          `Assoc [ ("dir", `String dir); ("paths", `List paths) ] :: acc)
        t.sources_by_dir []
    in
    `List dirs
  in
  let source_import_hints_json =
    let xs =
      Hashtbl.fold
        (fun path_key compools acc ->
          let compools_json = compools |> List.map (fun c -> `String c) in
          `Assoc
            [ ("path", `String path_key); ("compools", `List compools_json) ]
          :: acc)
        t.source_import_hints []
    in
    `List xs
  in
  let source_entry_hints_json =
    let xs =
      Hashtbl.fold
        (fun path_key is_entry acc ->
          if is_entry then `String path_key :: acc else acc)
        t.source_entry_hints []
    in
    `List xs
  in
  `Assoc
    [
      ("version", `Int checkpoint_version);
      ("root", `String t.root);
      ("complete", `Bool t.complete);
      ("checkpointLoaded", `Bool t.checkpoint_loaded);
      ("reconcilePending", `Bool t.reconcile_pending);
      ("reconcileEpoch", `Int t.reconcile_epoch);
      ("reconcileSourcesBefore", `Int t.reconcile_sources_before);
      ("reconcileSourcesAfter", `Int t.reconcile_sources_after);
      ("reconcileStalePruned", `Int t.reconcile_stale_pruned);
      ("pendingDirs", `List (List.map (fun d -> `String d) pending_dirs));
      ("seenDirs", `List seen_dirs);
      ("compools", json_of_string_pairs t.compools);
      ("sources", json_of_string_pairs t.sources);
      ("sourcesByDir", sources_by_dir_json);
      ("fileCompoolKeys", json_of_string_pairs t.file_compool_keys);
      ("sourceImportHints", source_import_hints_json);
      ("sourceEntryHints", source_entry_hints_json);
    ]

let save_checkpoint (t : t) : unit =
  try
    checkpoint_json t |> Yojson.Safe.to_string
    |> write_text_atomic t.checkpoint_file;
    t.checkpoint_dirty <- false;
    t.checkpoint_dirty_ops <- 0
  with _ -> ()

let maybe_save_checkpoint (t : t) ~(force : bool) : unit =
  if
    t.checkpoint_dirty
    && (force || t.checkpoint_dirty_ops >= checkpoint_write_every_ops)
  then save_checkpoint t

let is_dir (path : string) : bool = try Sys.is_directory path with _ -> false

let add_pending_dir_raw (t : t) (dir : string) : unit =
  let key = normalize_path_key dir in
  if key <> "" && is_dir dir && not (Hashtbl.mem t.seen_dirs key) then (
    Hashtbl.replace t.seen_dirs key true;
    Queue.add dir t.pending_dirs)

let prepend_pending_dir_raw (t : t) (dir : string) : unit =
  let key = normalize_path_key dir in
  if key = "" || not (is_dir dir) then ()
  else
    let old = queue_to_list t.pending_dirs in
    clear_queue t.pending_dirs;
    Queue.add dir t.pending_dirs;
    List.iter
      (fun d -> if normalize_path_key d <> key then Queue.add d t.pending_dirs)
      old;
    Hashtbl.replace t.seen_dirs key true

let start_reconcile (t : t) : bool =
  if not (is_dir t.root) then false
  else
    let was_pending = t.reconcile_pending in
    t.reconcile_epoch <- t.reconcile_epoch + 1;
    t.reconcile_pending <- true;
    t.reconcile_sources_before <- Hashtbl.length t.sources;
    t.reconcile_sources_after <- t.reconcile_sources_before;
    t.reconcile_stale_pruned <- 0;
    t.complete <- false;
    let root_key = normalize_path_key t.root in
    if Hashtbl.mem t.seen_dirs root_key then Hashtbl.remove t.seen_dirs root_key;
    prepend_pending_dir_raw t t.root;
    mark_checkpoint_dirty t;
    not was_pending

let force_reconcile (t : t) : bool = start_reconcile t

let load_checkpoint (t : t) : bool =
  match read_all_text t.checkpoint_file with
  | None -> false
  | Some txt -> (
      try
        match Yojson.Safe.from_string txt with
        | `Assoc fields ->
            let field k = List.assoc_opt k fields in
            let version_ok =
              match field "version" with
              | Some j -> (
                  match int_of_json j with
                  | Some v -> v >= 2 && v <= checkpoint_version
                  | None -> false)
              | None -> false
            in
            let root_ok =
              match field "root" with
              | Some (`String r) ->
                  normalize_path_key r = normalize_path_key t.root
              | _ -> false
            in
            if (not version_ok) || not root_ok then false
            else (
              Hashtbl.clear t.compools;
              Hashtbl.clear t.sources;
              Hashtbl.clear t.sources_by_dir;
              Hashtbl.clear t.file_compool_keys;
              Hashtbl.clear t.source_import_hints;
              Hashtbl.clear t.source_entry_hints;
              Hashtbl.clear t.seen_dirs;
              Hashtbl.clear t.reconcile_seen_epoch;
              clear_queue t.pending_dirs;

              (match field "compools" with
              | Some j ->
                  string_pairs_of_json j
                  |> List.iter (fun (k, v) -> Hashtbl.replace t.compools k v)
              | None -> ());
              (match field "sources" with
              | Some j ->
                  string_pairs_of_json j
                  |> List.iter (fun (k, v) -> Hashtbl.replace t.sources k v)
              | None -> ());
              (match field "sourcesByDir" with
              | Some (`List dirs) ->
                  List.iter
                    (function
                      | `Assoc fields -> (
                          match
                            ( List.assoc_opt "dir" fields,
                              List.assoc_opt "paths" fields )
                          with
                          | Some (`String dir), Some (`List paths) ->
                              let set = Hashtbl.create 16 in
                              List.iter
                                (function
                                  | `String path_key when path_key <> "" ->
                                      Hashtbl.replace set path_key true
                                  | _ -> ())
                                paths;
                              if Hashtbl.length set > 0 then
                                Hashtbl.replace t.sources_by_dir dir set
                          | _ -> ())
                      | _ -> ())
                    dirs
              | _ -> ());
              (match field "fileCompoolKeys" with
              | Some j ->
                  string_pairs_of_json j
                  |> List.iter (fun (k, v) ->
                      Hashtbl.replace t.file_compool_keys k v)
              | None -> ());
              (match field "sourceImportHints" with
              | Some (`List hints) ->
                  List.iter
                    (function
                      | `Assoc fields -> (
                          match
                            ( List.assoc_opt "path" fields,
                              List.assoc_opt "compools" fields )
                          with
                          | Some (`String path_key), Some (`List compools_json)
                            ->
                              let compools =
                                compools_json
                                |> List.filter_map (function
                                  | `String s ->
                                      let key = normalize_key s in
                                      if key = "" then None else Some key
                                  | _ -> None)
                              in
                              if path_key <> "" then
                                Hashtbl.replace t.source_import_hints path_key
                                  compools
                          | _ -> ())
                      | _ -> ())
                    hints
              | _ -> ());
              (match field "sourceEntryHints" with
              | Some (`List paths) ->
                  List.iter
                    (function
                      | `String path_key when path_key <> "" ->
                          Hashtbl.replace t.source_entry_hints path_key true
                      | _ -> ())
                    paths
              | _ -> ());
              (match field "seenDirs" with
              | Some j ->
                  string_list_of_json j
                  |> List.iter (fun k ->
                      if k <> "" then Hashtbl.replace t.seen_dirs k true)
              | None -> ());
              (match field "pendingDirs" with
              | Some j ->
                  string_list_of_json j
                  |> List.iter (fun d -> add_pending_dir_raw t d)
              | None -> ());

              t.complete <-
                bool_of_json_default (field "complete") ~default:false;
              t.checkpoint_loaded <-
                bool_of_json_default (field "checkpointLoaded") ~default:true;
              t.reconcile_pending <-
                bool_of_json_default (field "reconcilePending") ~default:false;
              t.reconcile_epoch <-
                (match field "reconcileEpoch" with
                | Some j -> (
                    match int_of_json j with Some n when n >= 0 -> n | _ -> 0)
                | None -> 0);
              t.reconcile_sources_before <-
                (match field "reconcileSourcesBefore" with
                | Some j -> (
                    match int_of_json j with
                    | Some n when n >= 0 -> n
                    | _ -> Hashtbl.length t.sources)
                | None -> Hashtbl.length t.sources);
              t.reconcile_sources_after <-
                (match field "reconcileSourcesAfter" with
                | Some j -> (
                    match int_of_json j with
                    | Some n when n >= 0 -> n
                    | _ -> Hashtbl.length t.sources)
                | None -> Hashtbl.length t.sources);
              t.reconcile_stale_pruned <-
                (match field "reconcileStalePruned" with
                | Some j -> (
                    match int_of_json j with Some n when n >= 0 -> n | _ -> 0)
                | None -> 0);
              if Hashtbl.length t.sources_by_dir = 0 then
                Hashtbl.iter
                  (fun path_key path ->
                    let dir = normalize_path_key (Filename.dirname path) in
                    if dir <> "" then
                      let set =
                        match Hashtbl.find_opt t.sources_by_dir dir with
                        | Some s -> s
                        | None ->
                            let s = Hashtbl.create 16 in
                            Hashtbl.replace t.sources_by_dir dir s;
                            s
                      in
                      Hashtbl.replace set path_key true)
                  t.sources;
              if t.reconcile_pending then
                Hashtbl.iter
                  (fun path_key _ ->
                    Hashtbl.replace t.reconcile_seen_epoch path_key
                      t.reconcile_epoch)
                  t.sources;
              t.checkpoint_dirty <- false;
              t.checkpoint_dirty_ops <- 0;
              true)
        | _ -> false
      with _ -> false)

let apply_resume_spot_if_any (t : t) : unit =
  match read_resume_spot t with
  | None -> ()
  | Some dir ->
      if is_dir dir then (
        prepend_pending_dir_raw t dir;
        t.complete <- false;
        mark_checkpoint_dirty t)
      else clear_resume_spot t

let sample t n =
  let acc = ref [] in
  Hashtbl.iter
    (fun k v -> if List.length !acc < n then acc := (k, v) :: !acc)
    t.compools;
  List.rev !acc

let all_paths t = Hashtbl.fold (fun _ path acc -> path :: acc) t.compools []

let all_source_paths t =
  Hashtbl.fold (fun _ path acc -> path :: acc) t.sources []

let is_ignored_dir name =
  name = ".git" || name = "_build" || name = "node_modules" || name = ".vscode"

let has_ext file =
  let lower = String.lowercase_ascii file in
  let ends_with ext =
    let n = String.length lower in
    let m = String.length ext in
    n >= m && String.sub lower (n - m) m = ext
  in
  ends_with ".jov" || ends_with ".j73" || ends_with ".jvl" || ends_with ".j"

let read_prefix path bytes =
  try
    let ic = open_in_bin path in
    let len = try min bytes (in_channel_length ic) with _ -> bytes in
    let s = really_input_string ic len in
    close_in_noerr ic;
    Some s
  with _ -> None

let is_hint_word_char = function
  | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let tokenize_upper_hint_words (text : string) : string list =
  let upper = String.uppercase_ascii text in
  let n = String.length upper in
  let out = ref [] in
  let rec scan i =
    if i >= n then ()
    else if is_hint_word_char upper.[i] then (
      let j = ref (i + 1) in
      while !j < n && is_hint_word_char upper.[!j] do
        incr j
      done;
      out := String.sub upper i (!j - i) :: !out;
      scan !j)
    else scan (i + 1)
  in
  scan 0;
  List.rev !out

let import_hints_from_prefix (text : string) : string list =
  let seen = Hashtbl.create 16 in
  let out = ref [] in
  let push_name (raw : string) =
    let key = normalize_key raw in
    if key <> "" && not (Hashtbl.mem seen key) then (
      Hashtbl.replace seen key true;
      out := key :: !out)
  in
  let rec collect = function
    | ("COMPOOL" | "ICOMPOOL") :: name :: tl ->
        push_name name;
        collect tl
    | _ :: tl -> collect tl
    | [] -> ()
  in
  collect (tokenize_upper_hint_words text);
  List.rev !out

let entry_hint_from_prefix (text : string) : bool =
  let rec has_program = function
    | "PROGRAM" :: _ -> true
    | _ :: tl -> has_program tl
    | [] -> false
  in
  has_program (tokenize_upper_hint_words text)

let starts_with ~(prefix : string) (s : string) : bool =
  let n = String.length s in
  let m = String.length prefix in
  n >= m && String.sub s 0 m = prefix

let file_stem_key (path : string) : string =
  let base = Filename.basename path in
  let stem =
    try Filename.chop_extension base with Invalid_argument _ -> base
  in
  normalize_key stem

let dir_prefix_key (path : string) : string =
  let p = normalize_path_key path in
  if p = "" then p else if p.[String.length p - 1] = '/' then p else p ^ "/"

let add_source_dir_entry (t : t) ~(path_key : string) ~(path : string) : unit =
  let dir_key = normalize_path_key (Filename.dirname path) in
  if dir_key <> "" then
    let set =
      match Hashtbl.find_opt t.sources_by_dir dir_key with
      | Some s -> s
      | None ->
          let s = Hashtbl.create 16 in
          Hashtbl.replace t.sources_by_dir dir_key s;
          s
    in
    Hashtbl.replace set path_key true

let remove_source_dir_entry (t : t) ~(path_key : string) ~(path : string) : unit
    =
  let dir_key = normalize_path_key (Filename.dirname path) in
  if dir_key <> "" then
    match Hashtbl.find_opt t.sources_by_dir dir_key with
    | None -> ()
    | Some set ->
        Hashtbl.remove set path_key;
        if Hashtbl.length set = 0 then Hashtbl.remove t.sources_by_dir dir_key

let remove_compool_for_path (t : t) ~(path_key : string) : bool =
  match Hashtbl.find_opt t.file_compool_keys path_key with
  | None -> false
  | Some compool_key -> (
      Hashtbl.remove t.file_compool_keys path_key;
      match Hashtbl.find_opt t.compools compool_key with
      | Some p when normalize_path_key p = path_key ->
          Hashtbl.remove t.compools compool_key;
          true
      | _ -> false)

let set_compool_for_path (t : t) ~(path : string) ~(path_key : string)
    ~(compool_key : string) : bool =
  let changed = ref false in
  if remove_compool_for_path t ~path_key then changed := true;
  if compool_key <> "" then (
    Hashtbl.replace t.file_compool_keys path_key compool_key;
    (match Hashtbl.find_opt t.compools compool_key with
    | Some prev when normalize_path_key prev = path_key -> ()
    | _ -> changed := true);
    Hashtbl.replace t.compools compool_key path);
  !changed

let index_source_file (t : t) (path : string) : bool =
  let path_key = normalize_path_key path in
  if t.reconcile_pending then
    Hashtbl.replace t.reconcile_seen_epoch path_key t.reconcile_epoch;
  let prev_source = Hashtbl.find_opt t.sources path_key in
  (match prev_source with
  | Some prev when prev <> path ->
      remove_source_dir_entry t ~path_key ~path:prev
  | _ -> ());
  Hashtbl.replace t.sources path_key path;
  add_source_dir_entry t ~path_key ~path;
  let source_changed =
    match prev_source with Some prev when prev = path -> false | _ -> true
  in
  let compool_changed, hints_changed =
    match read_prefix path 65536 with
    | None ->
        let compool_removed = remove_compool_for_path t ~path_key in
        let import_changed = Hashtbl.mem t.source_import_hints path_key in
        let entry_changed =
          match Hashtbl.find_opt t.source_entry_hints path_key with
          | Some true -> true
          | _ -> false
        in
        Hashtbl.remove t.source_import_hints path_key;
        Hashtbl.remove t.source_entry_hints path_key;
        (compool_removed, import_changed || entry_changed)
    | Some txt ->
        let compool_changed =
          match Preprocess.scan_compool_def ~text:txt with
          | None -> remove_compool_for_path t ~path_key
          | Some name ->
              let def_key = normalize_key name in
              let stem_key = file_stem_key path in
              if def_key = stem_key then
                set_compool_for_path t ~path ~path_key ~compool_key:def_key
              else remove_compool_for_path t ~path_key
        in
        let import_hints = import_hints_from_prefix txt in
        let import_changed =
          match Hashtbl.find_opt t.source_import_hints path_key with
          | Some prev -> prev <> import_hints
          | None -> import_hints <> []
        in
        if import_hints = [] then Hashtbl.remove t.source_import_hints path_key
        else Hashtbl.replace t.source_import_hints path_key import_hints;
        let entry_hint = entry_hint_from_prefix txt in
        let entry_changed =
          match Hashtbl.find_opt t.source_entry_hints path_key with
          | Some prev -> prev <> entry_hint
          | None -> entry_hint
        in
        if entry_hint then Hashtbl.replace t.source_entry_hints path_key true
        else Hashtbl.remove t.source_entry_hints path_key;
        (compool_changed, import_changed || entry_changed)
  in
  source_changed || compool_changed || hints_changed

let remove_source_file (t : t) (path : string) : bool =
  let path_key = normalize_path_key path in
  let prev_source = Hashtbl.find_opt t.sources path_key in
  let had_source = Hashtbl.mem t.sources path_key in
  (match prev_source with
  | Some prev -> remove_source_dir_entry t ~path_key ~path:prev
  | None -> remove_source_dir_entry t ~path_key ~path);
  Hashtbl.remove t.sources path_key;
  let had_import_hints = Hashtbl.mem t.source_import_hints path_key in
  Hashtbl.remove t.source_import_hints path_key;
  let had_entry_hint = Hashtbl.mem t.source_entry_hints path_key in
  Hashtbl.remove t.source_entry_hints path_key;
  let compool_removed = remove_compool_for_path t ~path_key in
  had_source || compool_removed || had_import_hints || had_entry_hint

let finalize_reconcile_if_complete (t : t) : unit =
  if t.reconcile_pending && Queue.is_empty t.pending_dirs then (
    let stale_paths =
      Hashtbl.fold
        (fun path_key path acc ->
          match Hashtbl.find_opt t.reconcile_seen_epoch path_key with
          | Some epoch when epoch = t.reconcile_epoch -> acc
          | _ -> path :: acc)
        t.sources []
    in
    List.iter (fun path -> ignore (remove_source_file t path)) stale_paths;
    t.reconcile_stale_pruned <- List.length stale_paths;
    t.reconcile_sources_after <- Hashtbl.length t.sources;
    t.reconcile_pending <- false;
    mark_checkpoint_dirty t)

let enqueue_dir (t : t) (dir : string) : unit =
  let key = normalize_path_key dir in
  if key <> "" && not (Hashtbl.mem t.seen_dirs key) then (
    Hashtbl.replace t.seen_dirs key true;
    Queue.add dir t.pending_dirs;
    t.complete <- false;
    mark_checkpoint_dirty t)

let requeue_dir (t : t) (dir : string) : unit =
  let key = normalize_path_key dir in
  if Hashtbl.mem t.seen_dirs key then (
    Hashtbl.remove t.seen_dirs key;
    mark_checkpoint_dirty t);
  enqueue_dir t dir

let remove_dir_tracking (t : t) (dir : string) : bool =
  let prefix = dir_prefix_key dir in
  let to_drop = ref [] in
  Hashtbl.iter
    (fun k _ -> if starts_with ~prefix k then to_drop := k :: !to_drop)
    t.seen_dirs;
  List.iter (fun k -> Hashtbl.remove t.seen_dirs k) !to_drop;
  !to_drop <> []

let remove_directory_tree (t : t) (dir : string) : bool =
  let prefix = dir_prefix_key dir in
  let prefix_dir =
    let n = String.length prefix in
    if n > 0 && prefix.[n - 1] = '/' then String.sub prefix 0 (n - 1)
    else prefix
  in
  let files = ref [] in
  Hashtbl.iter
    (fun dir_key set ->
      if starts_with ~prefix dir_key || dir_key = prefix_dir then
        Hashtbl.iter
          (fun path_key _ ->
            match Hashtbl.find_opt t.sources path_key with
            | None -> ()
            | Some path -> files := path :: !files)
          set)
    t.sources_by_dir;
  let tracking_changed = remove_dir_tracking t dir in
  let files_changed =
    List.fold_left
      (fun changed path -> remove_source_file t path || changed)
      false !files
  in
  tracking_changed || files_changed

let start ~(root : string) : t =
  let t =
    {
      root;
      compools = Hashtbl.create 97;
      sources = Hashtbl.create 512;
      sources_by_dir = Hashtbl.create 256;
      file_compool_keys = Hashtbl.create 97;
      source_import_hints = Hashtbl.create 256;
      source_entry_hints = Hashtbl.create 256;
      pending_dirs = Queue.create ();
      seen_dirs = Hashtbl.create 256;
      complete = false;
      checkpoint_loaded = false;
      reconcile_pending = false;
      reconcile_epoch = 0;
      reconcile_seen_epoch = Hashtbl.create 512;
      reconcile_sources_before = 0;
      reconcile_sources_after = 0;
      reconcile_stale_pruned = 0;
      checkpoint_file = checkpoint_file_path ~root;
      resume_spot_file = resume_spot_file_path ~root;
      checkpoint_dirty = false;
      checkpoint_dirty_ops = 0;
    }
  in
  let restored = load_checkpoint t in
  t.checkpoint_loaded <- restored;
  if not restored then enqueue_dir t root;
  apply_resume_spot_if_any t;
  if restored then ignore (start_reconcile t)
  else if Queue.is_empty t.pending_dirs && not t.complete then
    enqueue_dir t root;
  maybe_save_checkpoint t ~force:false;
  t

let is_complete (t : t) : bool = t.complete && not t.reconcile_pending

let scan_step (t : t) ~(max_dirs : int) ~(max_files : int) : int * int =
  let dirs_budget = max 0 max_dirs in
  let files_budget = max 0 max_files in
  let dirs_done = ref 0 in
  let files_done = ref 0 in
  while
    !dirs_done < dirs_budget && !files_done < files_budget
    && not (Queue.is_empty t.pending_dirs)
  do
    let dir = Queue.pop t.pending_dirs in
    write_resume_spot t dir;
    incr dirs_done;
    let entries = try Sys.readdir dir |> Array.to_list with _ -> [] in
    List.iter
      (fun name ->
        let full = Filename.concat dir name in
        try
          if Sys.is_directory full then (
            if not (is_ignored_dir name) then enqueue_dir t full)
          else if has_ext name then (
            if index_source_file t full then mark_checkpoint_dirty t;
            incr files_done)
        with _ -> ())
      entries;
    if Queue.is_empty t.pending_dirs then clear_resume_spot t
    else try write_resume_spot t (Queue.peek t.pending_dirs) with _ -> ()
  done;
  if Queue.is_empty t.pending_dirs then (
    finalize_reconcile_if_complete t;
    if not t.complete then mark_checkpoint_dirty t;
    t.complete <- true;
    clear_resume_spot t);
  maybe_save_checkpoint t ~force:(t.complete && not t.reconcile_pending);
  (!dirs_done, !files_done)

let build ~(root : string) : t =
  let t = start ~root in
  while not (is_complete t) do
    ignore (scan_step t ~max_dirs:4096 ~max_files:200_000)
  done;
  t

let apply_file_change (t : t) ~(path : string) ~(kind : file_change_kind) : bool
    =
  let changed =
    match kind with
    | Deleted ->
        if has_ext (Filename.basename path) then remove_source_file t path
        else remove_directory_tree t path
    | Created | Changed -> (
        if has_ext (Filename.basename path) then index_source_file t path
        else
          try
            if Sys.is_directory path then (
              requeue_dir t path;
              true)
            else false
          with _ -> false)
  in
  if changed then mark_checkpoint_dirty t;
  maybe_save_checkpoint t ~force:false;
  changed

let find_compool (t : t) ~(name : string) =
  Hashtbl.find_opt t.compools (normalize_key name)
