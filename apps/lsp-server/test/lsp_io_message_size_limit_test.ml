let failf = Lsp_test_helpers.failf

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int
      (max 1
         (Lsp_test_helpers.getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S"
            ~default:300))
  in
  let timeout_s =
    float_of_int
      (max 2
         (Lsp_test_helpers.getenv_int "JOVIAL_TEST_IO_SIZE_TIMEOUT_S"
            ~default:12))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase : string) : unit =
    if remaining_budget () <= 0.0 then
      failf "io size-limit test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-io-cap" in
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in

  Lsp_test_helpers.with_server ~server_path (fun srv ->
      ensure_budget "before oversize send";
      let huge_len = (16 * 1024 * 1024) + 2048 in
      let huge_payload = String.make huge_len 'A' in
      Lsp_test_helpers.send_notification srv ~method_:"$/setTrace"
        ~params:(`Assoc [ ("value", `String huge_payload) ]);

      ensure_budget "before initialize";
      let init_params =
        `Assoc
          [
            ("processId", `Null);
            ("rootUri", `String root_uri);
            ("capabilities", `Assoc []);
          ]
      in
      let init_resp, _ =
        Lsp_test_helpers.request_timed srv ~id:1 ~method_:"initialize"
          ~params:init_params
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      if not (Lsp_test_helpers.has_non_null_result init_resp) then
        failf "initialize returned null/empty result after oversize-frame drop";

      Lsp_test_helpers.send_notification srv ~method_:"initialized"
        ~params:(`Assoc []);
      Lsp_test_helpers.shutdown_and_exit srv
        ~timeout_s:(min timeout_s (remaining_budget ()));
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_io_message_size_limit_test: ok"
