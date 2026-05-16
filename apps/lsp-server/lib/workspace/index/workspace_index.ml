(* Module overview: Filesystem-backed workspace index for source discovery and dependency lookup. *)

type file_change_kind = Created | Changed | Deleted

type t = {
  source_extensions : string list;
  compools : (string, string) Hashtbl.t;
  sources : (string, string) Hashtbl.t;
  file_compool_keys : (string, string) Hashtbl.t;
  source_import_hints : (string, string list) Hashtbl.t;
  source_entry_hints : (string, bool) Hashtbl.t;
  source_proc_hints : (string, string list) Hashtbl.t;
  proc_hint_paths : (string, (string, bool) Hashtbl.t) Hashtbl.t;
  mutable all_source_paths_cache : string list option;
  mutable source_total_bytes_cache : int option;
}

let normalize_path_key = Uri_path.normalize_path_key
let normalize_key (s : string) : string = String.uppercase_ascii (String.trim s)

let index_hint_prefix_bytes =
  max 1024
    (Env_utils.nonneg_int "JOVIAL_INDEX_HINT_PREFIX_BYTES"
       ~default:(8 * 1024))

let source_count (t : t) : int = Hashtbl.length t.sources
let compool_count (t : t) : int = Hashtbl.length t.compools
let is_complete (_t : t) : bool = true

let invalidate_source_caches (t : t) : unit =
  t.all_source_paths_cache <- None;
  t.source_total_bytes_cache <- None

let has_ext (t : t) (file : string) : bool =
  Source_file.has_extension ~extensions:t.source_extensions file

let is_regular_source (t : t) (path : string) : bool =
  has_ext t (Filename.basename path)
  && Sys.file_exists path
  && try not (Sys.is_directory path) with _ -> false

let read_prefix (path : string) (bytes : int) : string option =
  try
    let ic = open_in_bin path in
    let len = try min bytes (in_channel_length ic) with _ -> bytes in
    let text = really_input_string ic len in
    close_in_noerr ic;
    Some text
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

let unique_keys (items : string list) : string list =
  let seen = Hashtbl.create 16 in
  let out = ref [] in
  List.iter
    (fun raw ->
      let key = normalize_key raw in
      if key <> "" && not (Hashtbl.mem seen key) then (
        Hashtbl.replace seen key true;
        out := key :: !out))
    items;
  List.rev !out

let import_hints_from_prefix (text : string) : string list =
  let rec collect acc = function
    | ("COMPOOL" | "ICOMPOOL") :: name :: tl -> collect (name :: acc) tl
    | _ :: tl -> collect acc tl
    | [] -> List.rev acc
  in
  collect [] (tokenize_upper_hint_words text) |> unique_keys

let entry_hint_from_prefix (text : string) : bool =
  tokenize_upper_hint_words text |> List.exists (( = ) "PROGRAM")

let proc_hints_from_prefix (text : string) : string list =
  let rec collect acc = function
    | "DEF" :: "PROC" :: name :: tl -> collect (name :: acc) tl
    | "PROC" :: name :: tl -> collect (name :: acc) tl
    | _ :: tl -> collect acc tl
    | [] -> List.rev acc
  in
  collect [] (tokenize_upper_hint_words text) |> unique_keys

let file_stem_key (path : string) : string =
  let base = Filename.basename path in
  let stem = try Filename.chop_extension base with Invalid_argument _ -> base in
  normalize_key stem

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

let add_proc_hint_path (t : t) ~(name_key : string) ~(path_key : string) : unit
    =
  let set =
    match Hashtbl.find_opt t.proc_hint_paths name_key with
    | Some set -> set
    | None ->
        let set = Hashtbl.create 8 in
        Hashtbl.replace t.proc_hint_paths name_key set;
        set
  in
  Hashtbl.replace set path_key true

let remove_proc_hints_for_path (t : t) ~(path_key : string) : bool =
  match Hashtbl.find_opt t.source_proc_hints path_key with
  | None -> false
  | Some prev ->
      Hashtbl.remove t.source_proc_hints path_key;
      List.iter
        (fun name_key ->
          match Hashtbl.find_opt t.proc_hint_paths name_key with
          | None -> ()
          | Some set ->
              Hashtbl.remove set path_key;
              if Hashtbl.length set = 0 then
                Hashtbl.remove t.proc_hint_paths name_key)
        prev;
      prev <> []

let set_proc_hints_for_path (t : t) ~(path_key : string)
    ~(proc_hints : string list) : bool =
  let prev = Option.value (Hashtbl.find_opt t.source_proc_hints path_key) ~default:[] in
  if prev = proc_hints then false
  else (
    ignore (remove_proc_hints_for_path t ~path_key);
    if proc_hints <> [] then (
      Hashtbl.replace t.source_proc_hints path_key proc_hints;
      List.iter
        (fun name_key -> add_proc_hint_path t ~name_key ~path_key)
        proc_hints);
    true)

let update_list_hint (tbl : (string, string list) Hashtbl.t) ~(path_key : string)
    ~(value : string list) : bool =
  let changed =
    match Hashtbl.find_opt tbl path_key with
    | Some prev -> prev <> value
    | None -> value <> []
  in
  if value = [] then Hashtbl.remove tbl path_key
  else Hashtbl.replace tbl path_key value;
  changed

let update_bool_hint (tbl : (string, bool) Hashtbl.t) ~(path_key : string)
    ~(value : bool) : bool =
  let changed =
    match Hashtbl.find_opt tbl path_key with
    | Some prev -> prev <> value
    | None -> value
  in
  if value then Hashtbl.replace tbl path_key true else Hashtbl.remove tbl path_key;
  changed

let index_source_file (t : t) (path : string) : bool =
  let path_key = normalize_path_key path in
  invalidate_source_caches t;
  let prev_source = Hashtbl.find_opt t.sources path_key in
  Hashtbl.replace t.sources path_key path;
  let source_changed =
    match prev_source with Some prev when prev = path -> false | _ -> true
  in
  let compool_changed, hints_changed =
    match read_prefix path index_hint_prefix_bytes with
    | None ->
        let compool_removed = remove_compool_for_path t ~path_key in
        let import_changed = Hashtbl.mem t.source_import_hints path_key in
        let entry_changed = Hashtbl.mem t.source_entry_hints path_key in
        let proc_changed = remove_proc_hints_for_path t ~path_key in
        Hashtbl.remove t.source_import_hints path_key;
        Hashtbl.remove t.source_entry_hints path_key;
        (compool_removed, import_changed || entry_changed || proc_changed)
    | Some text ->
        let compool_changed =
          match Preprocess.scan_compool_def ~text with
          | Some name when normalize_key name = file_stem_key path ->
              set_compool_for_path t ~path ~path_key
                ~compool_key:(normalize_key name)
          | _ -> remove_compool_for_path t ~path_key
        in
        let import_changed =
          update_list_hint t.source_import_hints ~path_key
            ~value:(import_hints_from_prefix text)
        in
        let entry_changed =
          update_bool_hint t.source_entry_hints ~path_key
            ~value:(entry_hint_from_prefix text)
        in
        let proc_changed =
          set_proc_hints_for_path t ~path_key
            ~proc_hints:(proc_hints_from_prefix text)
        in
        (compool_changed, import_changed || entry_changed || proc_changed)
  in
  source_changed || compool_changed || hints_changed

let remove_source_file (t : t) (path : string) : bool =
  let path_key = normalize_path_key path in
  invalidate_source_caches t;
  let had_source = Hashtbl.mem t.sources path_key in
  let had_import_hints = Hashtbl.mem t.source_import_hints path_key in
  let had_entry_hint = Hashtbl.mem t.source_entry_hints path_key in
  Hashtbl.remove t.sources path_key;
  Hashtbl.remove t.source_import_hints path_key;
  Hashtbl.remove t.source_entry_hints path_key;
  let had_proc_hints = remove_proc_hints_for_path t ~path_key in
  let compool_removed = remove_compool_for_path t ~path_key in
  had_source || compool_removed || had_import_hints || had_entry_hint
  || had_proc_hints

let create ~(source_extensions : string list) ~(root : string) : t =
  ignore root;
  {
    source_extensions = Source_file.normalize_extensions source_extensions;
    compools = Hashtbl.create 97;
    sources = Hashtbl.create 512;
    file_compool_keys = Hashtbl.create 97;
    source_import_hints = Hashtbl.create 256;
    source_entry_hints = Hashtbl.create 256;
    source_proc_hints = Hashtbl.create 256;
    proc_hint_paths = Hashtbl.create 256;
    all_source_paths_cache = None;
    source_total_bytes_cache = None;
  }

let of_source_files ~(source_extensions : string list) ~(root : string)
    ~(paths : string list) : t =
  let t = create ~source_extensions ~root in
  let seen = Hashtbl.create (max 16 (List.length paths)) in
  List.iter
    (fun path ->
      let path_key = normalize_path_key path in
      if
        path_key <> ""
        && not (Hashtbl.mem seen path_key)
        && is_regular_source t path
      then (
        Hashtbl.replace seen path_key true;
        ignore (index_source_file t path)))
    paths;
  t

let replace_source_files (t : t) ~(paths : string list) : bool =
  let wanted = Hashtbl.create (max 16 (List.length paths)) in
  List.iter
    (fun path ->
      let key = normalize_path_key path in
      if key <> "" then Hashtbl.replace wanted key path)
    paths;
  let changed = ref false in
  let current =
    Hashtbl.fold (fun key path acc -> (key, path) :: acc) t.sources []
  in
  List.iter
    (fun (key, path) ->
      if not (Hashtbl.mem wanted key) then
        if remove_source_file t path then changed := true)
    current;
  Hashtbl.iter
    (fun _ path -> if index_source_file t path then changed := true)
    wanted;
  !changed

let string_table_json (tbl : (string, string) Hashtbl.t) : Yojson.Safe.t =
  Hashtbl.fold
    (fun k v acc -> `List [ `String k; `String v ] :: acc)
    tbl []
  |> List.rev |> fun xs -> `List xs

let string_list_table_json (tbl : (string, string list) Hashtbl.t) :
    Yojson.Safe.t =
  Hashtbl.fold
    (fun k values acc ->
      `List [ `String k; `List (List.map (fun v -> `String v) values) ] :: acc)
    tbl []
  |> List.rev |> fun xs -> `List xs

let bool_key_table_json (tbl : (string, bool) Hashtbl.t) : Yojson.Safe.t =
  Hashtbl.fold (fun k v acc -> if v then `String k :: acc else acc) tbl []
  |> List.rev |> fun xs -> `List xs

let to_yojson (t : t) : Yojson.Safe.t =
  `Assoc
    [
      ("version", `Int 1);
      ( "sourceExtensions",
        `List (List.map (fun ext -> `String ext) t.source_extensions) );
      ("sources", string_table_json t.sources);
      ("compools", string_table_json t.compools);
      ("fileCompoolKeys", string_table_json t.file_compool_keys);
      ("sourceImportHints", string_list_table_json t.source_import_hints);
      ("sourceEntryHints", bool_key_table_json t.source_entry_hints);
      ("sourceProcHints", string_list_table_json t.source_proc_hints);
    ]

let field name fields = List.assoc_opt name fields

let string_list = function
  | `List xs ->
      xs
      |> List.filter_map (function `String s -> Some s | _ -> None)
  | _ -> []

let load_string_table json : (string, string) Hashtbl.t =
  let tbl = Hashtbl.create 128 in
  (match json with
  | `List rows ->
      List.iter
        (function
          | `List [ `String k; `String v ] -> Hashtbl.replace tbl k v
          | _ -> ())
        rows
  | _ -> ());
  tbl

let load_string_list_table json : (string, string list) Hashtbl.t =
  let tbl = Hashtbl.create 128 in
  (match json with
  | `List rows ->
      List.iter
        (function
          | `List [ `String k; `List values ] ->
              Hashtbl.replace tbl k
                (values
                |> List.filter_map (function `String s -> Some s | _ -> None))
          | _ -> ())
        rows
  | _ -> ());
  tbl

let load_bool_key_table json : (string, bool) Hashtbl.t =
  let tbl = Hashtbl.create 128 in
  (match json with
  | `List keys ->
      List.iter
        (function `String k -> Hashtbl.replace tbl k true | _ -> ())
        keys
  | _ -> ());
  tbl

let of_yojson = function
  | `Assoc fields -> (
      match field "version" fields with
      | Some (`Int 1) | Some (`Intlit "1") ->
          let source_extensions =
            field "sourceExtensions" fields |> Option.map string_list
            |> Option.value ~default:(Source_file.with_defaults [])
          in
          let t = create ~source_extensions ~root:"" in
          Hashtbl.iter
            (fun k v -> Hashtbl.replace t.sources k v)
            (load_string_table
               (Option.value (field "sources" fields) ~default:(`List [])));
          Hashtbl.iter
            (fun k v -> Hashtbl.replace t.compools k v)
            (load_string_table
               (Option.value (field "compools" fields) ~default:(`List [])));
          Hashtbl.iter
            (fun k v -> Hashtbl.replace t.file_compool_keys k v)
            (load_string_table
               (Option.value (field "fileCompoolKeys" fields)
                  ~default:(`List [])));
          Hashtbl.iter
            (fun k v -> Hashtbl.replace t.source_import_hints k v)
            (load_string_list_table
               (Option.value (field "sourceImportHints" fields)
                  ~default:(`List [])));
          Hashtbl.iter
            (fun k v -> Hashtbl.replace t.source_entry_hints k v)
            (load_bool_key_table
               (Option.value (field "sourceEntryHints" fields)
                  ~default:(`List [])));
          Hashtbl.iter
            (fun path_key proc_hints ->
              ignore (set_proc_hints_for_path t ~path_key ~proc_hints))
            (load_string_list_table
               (Option.value (field "sourceProcHints" fields)
                  ~default:(`List [])));
          Some t
      | _ -> None)
  | _ -> None

let apply_file_change (t : t) ~(path : string) ~(kind : file_change_kind) : bool
    =
  match kind with
  | Deleted ->
      if has_ext t (Filename.basename path) then remove_source_file t path
      else false
  | Created | Changed ->
      if is_regular_source t path then index_source_file t path else false

let find_compool (t : t) ~(name : string) : string option =
  Hashtbl.find_opt t.compools (normalize_key name)

let source_import_hints (t : t) ~(path : string) : string list =
  Option.value
    (Hashtbl.find_opt t.source_import_hints (normalize_path_key path))
    ~default:[]

let source_entry_hint (t : t) ~(path : string) : bool =
  Option.value
    (Hashtbl.find_opt t.source_entry_hints (normalize_path_key path))
    ~default:false

let source_paths_for_proc_hint (t : t) ~(name : string) : string list =
  let key = normalize_key name in
  match Hashtbl.find_opt t.proc_hint_paths key with
  | None -> []
  | Some set ->
      Hashtbl.fold
        (fun path_key _ acc ->
          match Hashtbl.find_opt t.sources path_key with
          | Some path -> path :: acc
          | None -> acc)
        set []

let sample (t : t) (n : int) : (string * string) list =
  let acc = ref [] in
  Hashtbl.iter
    (fun k v -> if List.length !acc < n then acc := (k, v) :: !acc)
    t.compools;
  List.rev !acc

let all_paths (t : t) : string list =
  Hashtbl.fold (fun _ path acc -> path :: acc) t.compools []

let all_source_paths (t : t) : string list =
  match t.all_source_paths_cache with
  | Some paths -> paths
  | None ->
      let paths = Hashtbl.fold (fun _ path acc -> path :: acc) t.sources [] in
      t.all_source_paths_cache <- Some paths;
      paths

let source_total_bytes (t : t) : int =
  match t.source_total_bytes_cache with
  | Some bytes -> bytes
  | None ->
      let bytes =
        all_source_paths t
        |> List.fold_left
             (fun acc path ->
               try
                 let n = Unix.(stat path).st_size in
                 if n > 0 then acc + n else acc
               with _ -> acc)
             0
      in
      t.source_total_bytes_cache <- Some bytes;
      bytes
