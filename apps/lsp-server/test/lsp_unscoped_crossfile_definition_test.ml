let failf = Lsp_test_helpers.failf

let create_workspace ~(root:string) ~(file_count:int) : string * string * string =
  for i = 0 to file_count - 1 do
    let path = Filename.concat root (Printf.sprintf "BIG_%04d.jov" i) in
    let prev_call =
      if i <= 0 then ""
      else Printf.sprintf "  PROC_%04d(OUTP:OUTP);\n" (i - 1)
    in
    let text =
      String.concat "\n"
        [
          "START PROGRAM BIG;";
          "BEGIN";
          (Printf.sprintf "PROC PROC_%04d(INP:OUTP);" i);
          "BEGIN";
          "  ITEM INP U 15;";
          "  ITEM OUTP U 15;";
          "  OUTP = INP + 1;";
          prev_call;
          "END";
          "STOP 0;";
          "END";
          "TERM";
          "";
        ]
    in
    Lsp_test_helpers.write_text path text
  done;
  let open_idx = max 1 (file_count - 1) in
  let open_path = Filename.concat root (Printf.sprintf "BIG_%04d.jov" open_idx) in
  let target_path = Filename.concat root (Printf.sprintf "BIG_%04d.jov" (open_idx - 1)) in
  let open_text =
    try
      let ic = open_in_bin open_path in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in_noerr ic;
      s
    with _ ->
      failf "failed reading generated open test file: %s" open_path
  in
  (open_path, open_text, target_path)

let response_has_result_uri (resp:Yojson.Safe.t) ~(uri:string) : bool =
  let normalize_uri_for_compare (u:string) : string =
    match Jovial_lsp_lib.Uri_path.file_path_of_uri_string u with
    | Some p ->
        let p = String.map (fun c -> if c = '\\' then '/' else c) p in
        if Sys.win32 then String.lowercase_ascii p else p
    | None ->
        String.lowercase_ascii (String.trim u)
  in
  let want = normalize_uri_for_compare uri in
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`List xs) ->
           List.exists (function
             | `Assoc lf ->
                 (match List.assoc_opt "uri" lf with
                  | Some (`String u) -> normalize_uri_for_compare u = want
                  | _ -> false)
             | _ -> false
           ) xs
       | _ -> false)
  | _ ->
      false

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300))
  in
  let file_count =
    max 3 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_UNSCOPED_XFILE_COUNT" ~default:120)
  in
  let timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_UNSCOPED_XFILE_TIMEOUT_S" ~default:10))
  in
  let first_max_ms =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_UNSCOPED_XFILE_FIRST_MAX_MS" ~default:2000))
  in
  let settle_timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_UNSCOPED_XFILE_SETTLE_TIMEOUT_S" ~default:6))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    let left = remaining_budget () in
    if left <= 0.0 then
      failf "unscoped cross-file definition test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-unscoped-crossfile" in
  let main_path, main_text, target_path = create_workspace ~root ~file_count in
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let target_uri = Lsp_test_helpers.lsp_doc_uri_of_path target_path in
  let needle =
    Printf.sprintf "PROC_%04d(OUTP:OUTP);" (file_count - 2)
  in
  let line, col = Lsp_test_helpers.line_col_of_first main_text ~needle in

  Lsp_test_helpers.with_server ~server_path (fun srv ->
    ensure_budget "before initialize";
    ignore (
      Lsp_test_helpers.initialize_and_open
        srv
        ~root_uri
        ~doc_uri
        ~doc_text:main_text
        ~timeout_s:(min timeout_s (remaining_budget ()))
    );
    ensure_budget "after initialize";

    let resp, first_ms =
      Lsp_test_helpers.request_timed srv
        ~id:2
        ~method_:"textDocument/definition"
        ~params:(Lsp_test_helpers.definition_request_params
          ~uri:doc_uri
          ~line
          ~character:(col + 6))
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    if first_ms > first_max_ms then
      failf
        "first unscoped cross-file definition too slow: %.1fms (limit %.1fms)"
        first_ms
        first_max_ms;
    if not (response_has_result_uri resp ~uri:target_uri) then (
      let deadline = Unix.gettimeofday () +. min settle_timeout_s (max 1.0 (remaining_budget ())) in
      let rec wait_for_target next_id last_payload =
        if Unix.gettimeofday () >= deadline then
          failf
            "first unscoped cross-file definition did not converge to expected target %s within %.1fs; last payload=%s"
            target_uri
            settle_timeout_s
            last_payload;
        let retry_resp, _ =
          Lsp_test_helpers.request_timed srv
            ~id:next_id
            ~method_:"textDocument/definition"
            ~params:(Lsp_test_helpers.definition_request_params
              ~uri:doc_uri
              ~line
              ~character:(col + 6))
            ~timeout_s:(min timeout_s (remaining_budget ()))
        in
        if response_has_result_uri retry_resp ~uri:target_uri then ()
        else (
          Thread.delay 0.1;
          wait_for_target (next_id + 1) (Yojson.Safe.to_string retry_resp)
        )
      in
      wait_for_target 3 (Yojson.Safe.to_string resp)
    );

    ensure_budget "before shutdown";
    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );

  print_endline "lsp_unscoped_crossfile_definition_test: ok"
