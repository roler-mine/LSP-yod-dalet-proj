module Lib = Jovial_lsp_lib
module WB = Jovial_lsp_lib.Workspace_base

let failf fmt = Printf.ksprintf failwith fmt

let getenv_int (name : string) ~(default : int) : int =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw -> ( try int_of_string (String.trim raw) with _ -> default)

let mk_temp_dir (prefix : string) : string =
  let root = Filename.get_temp_dir_name () in
  let rec pick attempts =
    if attempts <= 0 then
      failf "failed to create temporary directory for %s" prefix;
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

let sample_text (i : int) : string =
  String.concat "\n"
    [
      "START";
      Printf.sprintf "DEF PROC P_%d RENT;" i;
      "BEGIN";
      "  ITEM X U 1;";
      "  X = X + 1;";
      "END";
      "TERM";
      "";
    ]

let () =
  Random.self_init ();
  let hard_timeout_s =
    float_of_int
      (max 1 (getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300))
  in
  let started = Unix.gettimeofday () in
  let ensure_budget (phase : string) : unit =
    if hard_timeout_s -. (Unix.gettimeofday () -. started) <= 0.0 then
      failf "closed-doc LRU test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root = mk_temp_dir "jovial-lru-closed-docs" in
  let file_count = 96 in
  let files =
    List.init file_count (fun i ->
        let path = Filename.concat root (Printf.sprintf "F%03d.j73" i) in
        write_text path (sample_text i);
        path)
  in
  let ws = WB.create () in
  WB.set_root_path ws (Some root);
  WB.rescan ws;
  ensure_budget "after setup";

  List.iter (fun p -> ignore (WB.doc_at_path ws p)) files;
  ensure_budget "after loading closed docs";

  let closed_cache_count = Hashtbl.length ws.files in
  let lru_cap = ws.closed_doc_lru_max in
  if closed_cache_count > lru_cap then
    failf "closed-doc cache exceeded LRU cap: files=%d cap=%d"
      closed_cache_count lru_cap;
  if Hashtbl.length ws.docs <> 0 then
    failf "expected no open docs in LRU test, got %d" (Hashtbl.length ws.docs);

  print_endline "lsp_closed_doc_lru_eviction_test: ok"
