let failf = Lsp_test_helpers.failf

let create_workspace ~(root:string) ~(file_count:int) : string * string =
  let bucket_count = 24 in
  for i = 0 to bucket_count - 1 do
    Lsp_test_helpers.ensure_dir (Filename.concat root (Printf.sprintf "pkg_%02d" i))
  done;
  for i = 0 to file_count - 1 do
    let dir = Filename.concat root (Printf.sprintf "pkg_%02d" (i mod bucket_count)) in
    let path = Filename.concat dir (Printf.sprintf "LIB_%04d.j73" i) in
    let text =
      String.concat "\n"
        [
          "START";
          (Printf.sprintf "DEF PROC LIB_%04d RENT;" i);
          "BEGIN";
          "  ITEM V U 1;";
          "  V = 1;";
          "END";
          "TERM";
          "";
        ]
    in
    Lsp_test_helpers.write_text path text
  done;
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM X U 1;";
        "  X = X + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main_path main_text;
  (main_path, main_text)

let expect_definition_result (resp:Yojson.Safe.t) : unit =
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`List (_ :: _)) -> ()
       | Some `Null -> failf "definition returned null result"
       | _ -> failf "definition returned empty/invalid result")
  | _ ->
      failf "definition response is not a JSON object"

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300))
  in
  let index_files =
    max 0 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_RESP_INDEX_FILES" ~default:2000)
  in
  let timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_RESP_TIMEOUT_S" ~default:12))
  in
  let max_init_ms =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_RESP_MAX_INIT_MS" ~default:2500))
  in
  let max_definition_ms =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_RESP_MAX_DEFINITION_MS" ~default:1000))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    let left = remaining_budget () in
    if left <= 0.0 then
      failf "indexing responsiveness test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in
  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-responsiveness" in
  let main_path, main_text = create_workspace ~root ~file_count:index_files in
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let line, col = Lsp_test_helpers.line_col_of_first main_text ~needle:"X = X + 1;" in
  Lsp_test_helpers.with_server ~server_path (fun srv ->
    ensure_budget "before initialize";
    let init_timeout_s = min timeout_s (remaining_budget ()) in
    let init_ms =
      Lsp_test_helpers.initialize_and_open
        srv
        ~root_uri
        ~doc_uri
        ~doc_text:main_text
        ~timeout_s:init_timeout_s
    in
    ensure_budget "after initialize";
    let def_params =
      Lsp_test_helpers.definition_request_params
        ~uri:doc_uri
        ~line
        ~character:(col + 1)
    in
    let def_timeout_s = min timeout_s (remaining_budget ()) in
    let def_resp, def_ms =
      Lsp_test_helpers.request_timed srv
        ~id:2
        ~method_:"textDocument/definition"
        ~params:def_params
        ~timeout_s:def_timeout_s
    in
    expect_definition_result def_resp;
    if init_ms > max_init_ms then
      failf
        "initialize too slow: %.1fms (limit %.1fms, workspace files=%d)"
        init_ms
        max_init_ms
        index_files;
    if def_ms > max_definition_ms then
      failf
        "definition too slow: %.1fms (limit %.1fms)"
        def_ms
        max_definition_ms;
    ensure_budget "after definition";
    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s;
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );
  print_endline "lsp_responsiveness_test: ok"
