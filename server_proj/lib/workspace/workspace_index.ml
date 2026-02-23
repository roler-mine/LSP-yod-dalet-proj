type t = {
  root : string;
  compools : (string, string) Hashtbl.t;  (* NAME -> filepath *)
  sources : (string, string) Hashtbl.t;   (* normalized path -> source filepath *)
  file_compool_keys : (string, string) Hashtbl.t;  (* normalized path -> COMPOOL key *)
  pending_dirs : string Queue.t;
  seen_dirs : (string, bool) Hashtbl.t;
  mutable complete : bool;
}

type file_change_kind =
  | Created
  | Changed
  | Deleted

let normalize_path_key (p:string) : string =
  let p = String.map (fun c -> if c = '\\' then '/' else c) p in
  if Sys.win32 then String.lowercase_ascii p else p

let root t = t.root
let compool_count t = Hashtbl.length t.compools

let sample t n =
  let acc = ref [] in
  Hashtbl.iter (fun k v ->
    if List.length !acc < n then acc := (k, v) :: !acc
  ) t.compools;
  List.rev !acc

let all_paths t =
  Hashtbl.fold (fun _ path acc -> path :: acc) t.compools []

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
    let len =
      try min bytes (in_channel_length ic)
      with _ -> bytes
    in
    let s = really_input_string ic len in
    close_in_noerr ic;
    Some s
  with _ -> None

let normalize_key (s:string) : string =
  String.uppercase_ascii (String.trim s)

let starts_with ~(prefix:string) (s:string) : bool =
  let n = String.length s in
  let m = String.length prefix in
  n >= m && String.sub s 0 m = prefix

let file_stem_key (path:string) : string =
  let base = Filename.basename path in
  let stem =
    try Filename.chop_extension base
    with Invalid_argument _ -> base
  in
  normalize_key stem

let dir_prefix_key (path:string) : string =
  let p = normalize_path_key path in
  if p = "" then p
  else if p.[String.length p - 1] = '/' then p
  else p ^ "/"

let remove_compool_for_path (t:t) ~(path_key:string) : bool =
  match Hashtbl.find_opt t.file_compool_keys path_key with
  | None -> false
  | Some compool_key ->
      Hashtbl.remove t.file_compool_keys path_key;
      (match Hashtbl.find_opt t.compools compool_key with
       | Some p when normalize_path_key p = path_key ->
           Hashtbl.remove t.compools compool_key;
           true
       | _ ->
           false)

let set_compool_for_path (t:t) ~(path:string) ~(path_key:string) ~(compool_key:string) : bool =
  let changed = ref false in
  if remove_compool_for_path t ~path_key then changed := true;
  if compool_key <> "" then (
    Hashtbl.replace t.file_compool_keys path_key compool_key;
    (match Hashtbl.find_opt t.compools compool_key with
     | Some prev when normalize_path_key prev = path_key -> ()
     | _ -> changed := true);
    Hashtbl.replace t.compools compool_key path
  );
  !changed

let index_source_file (t:t) (path:string) : bool =
  let path_key = normalize_path_key path in
  Hashtbl.replace t.sources path_key path;
  match read_prefix path 65536 with
  | None ->
      remove_compool_for_path t ~path_key
  | Some txt ->
      (match Preprocess.scan_compool_def ~text:txt with
       | None ->
           remove_compool_for_path t ~path_key
       | Some name ->
           let def_key = normalize_key name in
           let stem_key = file_stem_key path in
           if def_key = stem_key then
             set_compool_for_path t ~path ~path_key ~compool_key:def_key
           else
             remove_compool_for_path t ~path_key)

let remove_source_file (t:t) (path:string) : bool =
  let path_key = normalize_path_key path in
  Hashtbl.remove t.sources path_key;
  remove_compool_for_path t ~path_key

let enqueue_dir (t:t) (dir:string) : unit =
  let key = normalize_path_key dir in
  if key <> "" && not (Hashtbl.mem t.seen_dirs key) then (
    Hashtbl.replace t.seen_dirs key true;
    Queue.add dir t.pending_dirs;
    t.complete <- false
  )

let requeue_dir (t:t) (dir:string) : unit =
  let key = normalize_path_key dir in
  Hashtbl.remove t.seen_dirs key;
  enqueue_dir t dir

let remove_dir_tracking (t:t) (dir:string) : unit =
  let prefix = dir_prefix_key dir in
  let to_drop = ref [] in
  Hashtbl.iter (fun k _ ->
    if starts_with ~prefix k then to_drop := k :: !to_drop
  ) t.seen_dirs;
  List.iter (fun k -> Hashtbl.remove t.seen_dirs k) !to_drop

let remove_directory_tree (t:t) (dir:string) : bool =
  let prefix = dir_prefix_key dir in
  let files = ref [] in
  Hashtbl.iter (fun key path ->
    if starts_with ~prefix key then files := path :: !files
  ) t.sources;
  remove_dir_tracking t dir;
  List.fold_left (fun changed path ->
    remove_source_file t path || changed
  ) false !files

let start ~(root:string) : t =
  let t =
    {
      root;
      compools = Hashtbl.create 97;
      sources = Hashtbl.create 512;
      file_compool_keys = Hashtbl.create 97;
      pending_dirs = Queue.create ();
      seen_dirs = Hashtbl.create 256;
      complete = false;
    }
  in
  enqueue_dir t root;
  t

let is_complete (t:t) : bool =
  t.complete

let scan_step (t:t) ~(max_dirs:int) ~(max_files:int) : int * int =
  let dirs_budget = max 0 max_dirs in
  let files_budget = max 0 max_files in
  let dirs_done = ref 0 in
  let files_done = ref 0 in
  while !dirs_done < dirs_budget
        && !files_done < files_budget
        && not (Queue.is_empty t.pending_dirs) do
    let dir = Queue.pop t.pending_dirs in
    incr dirs_done;
    let entries =
      try Sys.readdir dir |> Array.to_list
      with _ -> []
    in
    List.iter (fun name ->
      let full = Filename.concat dir name in
      try
        if Sys.is_directory full then (
          if not (is_ignored_dir name) then enqueue_dir t full
        ) else if has_ext name then (
          ignore (index_source_file t full);
          incr files_done
        )
      with _ -> ()
    ) entries
  done;
  if Queue.is_empty t.pending_dirs then t.complete <- true;
  (!dirs_done, !files_done)

let build ~(root:string) : t =
  let t = start ~root in
  while not (is_complete t) do
    ignore (scan_step t ~max_dirs:4096 ~max_files:200_000)
  done;
  t

let apply_file_change (t:t) ~(path:string) ~(kind:file_change_kind) : bool =
  match kind with
  | Deleted ->
      if has_ext (Filename.basename path) then
        remove_source_file t path
      else
        remove_directory_tree t path
  | Created | Changed ->
      if has_ext (Filename.basename path) then
        index_source_file t path
      else
        (try
           if Sys.is_directory path then (
             requeue_dir t path;
             false
           ) else
             false
         with _ ->
           false)

let find_compool (t:t) ~(name:string) =
  Hashtbl.find_opt t.compools (normalize_key name)
