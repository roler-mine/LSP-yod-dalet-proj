module Lib = Jovial_lsp_lib

let failf fmt = Printf.ksprintf failwith fmt

let mk_temp_dir (prefix : string) : string =
  let root = Filename.get_temp_dir_name () in
  let rec pick attempts =
    if attempts <= 0 then failf "failed to create temp dir for %s" prefix;
    let name =
      Printf.sprintf "%s-%d-%06x" prefix (Unix.getpid ())
        (Random.bits () land 0xFFFFFF)
    in
    let path = Filename.concat root name in
    try
      Unix.mkdir path 0o755;
      path
    with _ -> pick (attempts - 1)
  in
  pick 32

let write_text (path : string) (text : string) : unit =
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc

let normalize_path (path : string) : string =
  let path = String.map (fun c -> if c = '\\' then '/' else c) path in
  if Sys.win32 then String.lowercase_ascii path else path

let contains_path (paths : string list) (path : string) : bool =
  let want = normalize_path path in
  List.exists (fun got -> normalize_path got = want) paths

let expect_contains ~(name : string) ~(paths : string list) ~(path : string) :
    unit =
  if not (contains_path paths path) then
    failf "%s: expected %s in [%s]" name path (String.concat "; " paths)

let expect_empty ~(name : string) (paths : string list) : unit =
  if paths <> [] then
    failf "%s: expected no paths, got [%s]" name (String.concat "; " paths)

let expect_eq_int ~(name : string) ~(got : int) ~(want : int) : unit =
  if got <> want then failf "%s: expected %d, got %d" name want got

let alpha_text_before =
  String.concat "\n"
    [
      "START";
      "DEF PROC ALPHA RENT;";
      "REF PROC BETA;";
      "BEGIN";
      "END";
      "TERM";
      "";
    ]

let alpha_text_after =
  String.concat "\n"
    [ "START"; "DEF PROC GAMMA RENT;"; "BEGIN"; "END"; "TERM"; "" ]

let beta_text =
  String.concat "\n" [ "START"; "DEF PROC BETA;"; "BEGIN"; "END"; "TERM"; "" ]

let () =
  Random.self_init ();
  let root = mk_temp_dir "jovial-lsp-index-proc-hint" in
  let alpha_path = Filename.concat root "ALPHA.j73" in
  let beta_path = Filename.concat root "BETA.j73" in
  write_text alpha_path alpha_text_before;
  write_text beta_path beta_text;

  let idx = Lib.Workspace_index.build ~root in
  expect_contains ~name:"ALPHA proc hint"
    ~paths:(Lib.Workspace_index.source_paths_for_proc_hint idx ~name:"ALPHA")
    ~path:alpha_path;
  expect_contains ~name:"BETA proc hint from REF PROC"
    ~paths:(Lib.Workspace_index.source_paths_for_proc_hint idx ~name:"BETA")
    ~path:alpha_path;
  expect_contains ~name:"BETA proc hint from DEF PROC"
    ~paths:(Lib.Workspace_index.source_paths_for_proc_hint idx ~name:"BETA")
    ~path:beta_path;
  expect_eq_int ~name:"source bytes before change"
    ~got:(Lib.Workspace_index.source_total_bytes idx)
    ~want:(String.length alpha_text_before + String.length beta_text);

  write_text alpha_path alpha_text_after;
  ignore
    (Lib.Workspace_index.apply_file_change idx ~path:alpha_path
       ~kind:Lib.Workspace_index.Changed);

  expect_empty ~name:"ALPHA proc hint removed"
    (Lib.Workspace_index.source_paths_for_proc_hint idx ~name:"ALPHA");
  expect_contains ~name:"GAMMA proc hint added"
    ~paths:(Lib.Workspace_index.source_paths_for_proc_hint idx ~name:"GAMMA")
    ~path:alpha_path;
  expect_eq_int ~name:"source bytes after change"
    ~got:(Lib.Workspace_index.source_total_bytes idx)
    ~want:(String.length alpha_text_after + String.length beta_text);

  print_endline "workspace_index_proc_hint_test: ok"
