type t = {
  root : string;
  compools : (string, string) Hashtbl.t;  (* NAME -> filepath *)
}

let root t = t.root
let compool_count t = Hashtbl.length t.compools

let sample t n =
  let acc = ref [] in
  Hashtbl.iter (fun k v ->
    if List.length !acc < n then acc := (k, v) :: !acc
  ) t.compools;
  List.rev !acc

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

let rec walk (dir:string) (files:string list ref) =
  let entries =
    try Sys.readdir dir |> Array.to_list with _ -> []
  in
  List.iter (fun name ->
    let full = Filename.concat dir name in
    try
      if Sys.is_directory full then (
        if not (is_ignored_dir name) then walk full files
      ) else if has_ext name then (
        files := full :: !files
      )
    with _ -> ()
  ) entries

let build ~(root:string) : t =
  let files = ref [] in
  walk root files;

  let compools = Hashtbl.create 97 in
  List.iter (fun path ->
    match read_prefix path 65536 with
    | None -> ()
    | Some txt ->
        match Preprocess.scan_compool_def ~text:txt with
        | None -> ()
        | Some name ->
            let name = String.uppercase_ascii (String.trim name) in
            if name <> "" then Hashtbl.replace compools name path
  ) !files;

  { root; compools }

let find_compool (t:t) ~(name:string) =
  Hashtbl.find_opt t.compools (String.uppercase_ascii (String.trim name))
