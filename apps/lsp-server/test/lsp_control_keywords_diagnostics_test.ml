let failf = Lsp_test_helpers.failf

let contains_substring ~(haystack : string) ~(needle : string) : bool =
  let n = String.length haystack in
  let m = String.length needle in
  if m = 0 then true
  else
    let rec loop i =
      if i + m > n then false
      else if String.sub haystack i m = needle then true
      else loop (i + 1)
    in
    loop 0

let is_false_control_keyword_undefined (msg : string) : bool =
  contains_substring ~haystack:msg ~needle:"Undefined procedure \"EXIT\""
  || contains_substring ~haystack:msg ~needle:"Undefined procedure \"ABORT\""
  || contains_substring ~haystack:msg ~needle:"Undefined procedure \"STOP\""

let assert_no_false_control_keyword_diag ~(phase : string)
    (diags : Yojson.Safe.t list) : unit =
  let offending =
    diags
    |> List.filter_map Lsp_test_helpers.diag_message
    |> List.filter is_false_control_keyword_undefined
  in
  if offending <> [] then
    failf "control-keyword diagnostics regression at %s: %s" phase
      (String.concat " | " offending)

let wait_uri_diags_or_fail ~(srv : Lsp_test_helpers.server_proc) ~(uri : string)
    ~(timeout_s : float) ~(phase : string) : Yojson.Safe.t list =
  let t0 = Unix.gettimeofday () in
  match
    Lsp_test_helpers.wait_for_publish_diagnostics_for_uri ~srv ~target_uri:uri
      ~timeout_s
  with
  | None ->
      failf "did not receive publishDiagnostics for %s within %.2fs at %s" uri
        timeout_s phase
  | Some diags ->
      let elapsed = Unix.gettimeofday () -. t0 in
      if elapsed > timeout_s +. 0.001 then
        failf "publishDiagnostics for %s exceeded %.2fs at %s (got %.3fs)" uri
          timeout_s phase elapsed;
      diags

let did_open_params ~(uri : string) ~(version : int) ~(text : string) :
    Yojson.Safe.t =
  `Assoc
    [
      ( "textDocument",
        `Assoc
          [
            ("uri", `String uri);
            ("languageId", `String "jovial");
            ("version", `Int version);
            ("text", `String text);
          ] );
    ]

let expect_empty_diags_for_open ~(srv : Lsp_test_helpers.server_proc)
    ~(uri : string) ~(timeout_s : float) ~(phase : string) : unit =
  let diags = wait_uri_diags_or_fail ~srv ~uri ~timeout_s ~phase in
  assert_no_false_control_keyword_diag ~phase diags;
  if diags <> [] then
    failf "expected empty diagnostics for %s at %s, got %d entries" uri phase
      (List.length diags)

let diags_contain_message ~(needle : string) (diags : Yojson.Safe.t list) : bool
    =
  diags
  |> List.filter_map Lsp_test_helpers.diag_message
  |> List.exists (fun msg -> contains_substring ~haystack:msg ~needle)

let wait_uri_diag_contains_or_fail ~(srv : Lsp_test_helpers.server_proc)
    ~(uri : string) ~(timeout_s : float) ~(phase : string) ~(needle : string) :
    unit =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then
      failf "expected diagnostics for %s at %s to include %S within %.2fs" uri
        phase needle timeout_s;
    let chunk = min 0.4 remaining in
    if chunk <= 0.0 then
      failf "expected diagnostics for %s at %s to include %S within %.2fs" uri
        phase needle timeout_s;
    match
      Lsp_test_helpers.wait_for_publish_diagnostics_for_uri ~srv ~target_uri:uri
        ~timeout_s:chunk
    with
    | None -> loop ()
    | Some diags ->
        assert_no_false_control_keyword_diag ~phase diags;
        let found =
          diags
          |> List.filter_map Lsp_test_helpers.diag_message
          |> List.exists (fun msg -> contains_substring ~haystack:msg ~needle)
        in
        if found then () else loop ()
  in
  loop ()

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let open_timeout_s =
    max 0.2
      (match Sys.getenv_opt "JOVIAL_TEST_CONTROL_KEYWORDS_TIMEOUT_S" with
      | None -> 2.0
      | Some raw -> ( try float_of_string (String.trim raw) with _ -> 2.0))
  in
  let authoritative_timeout_s =
    max open_timeout_s
      (match
         Sys.getenv_opt "JOVIAL_TEST_CONTROL_KEYWORDS_AUTHORITATIVE_TIMEOUT_S"
       with
      | None -> 10.0
      | Some raw -> ( try float_of_string (String.trim raw) with _ -> 10.0))
  in
  let hard_timeout_s =
    float_of_int
      (max 1
         (Lsp_test_helpers.getenv_int
            "JOVIAL_TEST_CONTROL_KEYWORDS_HARD_TIMEOUT_S" ~default:120))
  in
  let started = Unix.gettimeofday () in
  let ensure_budget (phase : string) : unit =
    if Unix.gettimeofday () -. started > hard_timeout_s then
      failf
        "control-keywords diagnostics test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-control-keywords" in
  let exit_path = Filename.concat root "CONTROL_EXIT.j73" in
  let abort_path = Filename.concat root "CONTROL_ABORT.j73" in
  let stop_path = Filename.concat root "CONTROL_STOP.j73" in
  let exit_invalid_path = Filename.concat root "CONTROL_EXIT_INVALID.j73" in
  let abort_invalid_path = Filename.concat root "CONTROL_ABORT_INVALID.j73" in
  let return_invalid_path = Filename.concat root "CONTROL_RETURN_INVALID.j73" in
  let broken_path = Filename.concat root "CONTROL_BROKEN.j73" in

  let exit_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM I U 1;";
        "  I = 0;";
        "  WHILE I < 10;";
        "  BEGIN";
        "    EXIT;";
        "  END";
        "END";
        "TERM";
        "";
      ]
  in
  let abort_text =
    String.concat "\n"
      [ "START"; "DEF PROC MAIN RENT;"; "BEGIN"; "  ABORT;"; "END"; "TERM"; "" ]
  in
  let stop_text =
    String.concat "\n"
      [
        "START"; "DEF PROC MAIN RENT;"; "BEGIN"; "  STOP 0;"; "END"; "TERM"; "";
      ]
  in
  let exit_invalid_text =
    String.concat "\n"
      [ "START"; "DEF PROC MAIN RENT;"; "BEGIN"; "  EXIT;"; "END"; "TERM"; "" ]
  in
  let abort_invalid_text =
    String.concat "\n" [ "START"; "ABORT;"; "TERM"; "" ]
  in
  let return_invalid_text =
    String.concat "\n" [ "START"; "RETURN;"; "TERM"; "" ]
  in
  let broken_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  UNKNOWN_PROC;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text exit_path exit_text;
  Lsp_test_helpers.write_text abort_path abort_text;
  Lsp_test_helpers.write_text stop_path stop_text;
  Lsp_test_helpers.write_text exit_invalid_path exit_invalid_text;
  Lsp_test_helpers.write_text abort_invalid_path abort_invalid_text;
  Lsp_test_helpers.write_text return_invalid_path return_invalid_text;
  Lsp_test_helpers.write_text broken_path broken_text;

  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let exit_uri = Lsp_test_helpers.lsp_doc_uri_of_path exit_path in
  let abort_uri = Lsp_test_helpers.lsp_doc_uri_of_path abort_path in
  let stop_uri = Lsp_test_helpers.lsp_doc_uri_of_path stop_path in
  let exit_invalid_uri =
    Lsp_test_helpers.lsp_doc_uri_of_path exit_invalid_path
  in
  let abort_invalid_uri =
    Lsp_test_helpers.lsp_doc_uri_of_path abort_invalid_path
  in
  let return_invalid_uri =
    Lsp_test_helpers.lsp_doc_uri_of_path return_invalid_path
  in
  let broken_uri = Lsp_test_helpers.lsp_doc_uri_of_path broken_path in

  Lsp_test_helpers.with_server
    ~env:[ ("JOVIAL_WORKSPACE_DIAGS_MODE", "off") ]
    ~server_path
    (fun srv ->
      ensure_budget "initialize+open exit";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri:exit_uri
           ~doc_text:exit_text ~timeout_s:open_timeout_s);
      expect_empty_diags_for_open ~srv ~uri:exit_uri ~timeout_s:open_timeout_s
        ~phase:"didOpen EXIT";

      ensure_budget "open abort";
      Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen"
        ~params:(did_open_params ~uri:abort_uri ~version:1 ~text:abort_text);
      expect_empty_diags_for_open ~srv ~uri:abort_uri ~timeout_s:open_timeout_s
        ~phase:"didOpen ABORT";

      ensure_budget "open stop";
      Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen"
        ~params:(did_open_params ~uri:stop_uri ~version:1 ~text:stop_text);
      expect_empty_diags_for_open ~srv ~uri:stop_uri ~timeout_s:open_timeout_s
        ~phase:"didOpen STOP";

      ensure_budget "open exit invalid";
      Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen"
        ~params:
          (did_open_params ~uri:exit_invalid_uri ~version:1
             ~text:exit_invalid_text);
      wait_uri_diag_contains_or_fail ~srv ~uri:exit_invalid_uri
        ~timeout_s:authoritative_timeout_s ~phase:"didOpen EXIT invalid"
        ~needle:"EXIT is only valid inside a loop.";

      ensure_budget "open abort invalid";
      Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen"
        ~params:
          (did_open_params ~uri:abort_invalid_uri ~version:1
             ~text:abort_invalid_text);
      wait_uri_diag_contains_or_fail ~srv ~uri:abort_invalid_uri
        ~timeout_s:authoritative_timeout_s ~phase:"didOpen ABORT invalid"
        ~needle:"ABORT is only valid inside a procedure.";

      ensure_budget "open return invalid";
      Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen"
        ~params:
          (did_open_params ~uri:return_invalid_uri ~version:1
             ~text:return_invalid_text);
      wait_uri_diag_contains_or_fail ~srv ~uri:return_invalid_uri
        ~timeout_s:authoritative_timeout_s ~phase:"didOpen RETURN invalid"
        ~needle:"RETURN is only valid inside a procedure.";

      ensure_budget "open broken";
      Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen"
        ~params:(did_open_params ~uri:broken_uri ~version:1 ~text:broken_text);
      let first_broken =
        wait_uri_diags_or_fail ~srv ~uri:broken_uri ~timeout_s:open_timeout_s
          ~phase:"didOpen broken"
      in
      assert_no_false_control_keyword_diag ~phase:"didOpen broken" first_broken;
      if not (diags_contain_message ~needle:"Undefined procedure" first_broken)
      then
        wait_uri_diag_contains_or_fail ~srv ~uri:broken_uri
          ~timeout_s:authoritative_timeout_s ~phase:"didOpen broken"
          ~needle:"Undefined procedure";

      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:open_timeout_s;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));
  print_endline "lsp_control_keywords_diagnostics_test: ok"
