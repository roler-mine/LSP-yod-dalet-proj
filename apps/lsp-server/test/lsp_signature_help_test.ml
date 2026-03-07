let failf = Lsp_test_helpers.failf

let int_of_json = function
  | `Int n -> Some n
  | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
  | _ -> None

let signature_result (resp : Yojson.Safe.t) : (int * int) option =
  match resp with
  | `Assoc fields -> (
      match (List.assoc_opt "error" fields, List.assoc_opt "result" fields) with
      | Some _, _ -> None
      | _, Some (`Assoc rf) ->
          let active_param =
            match List.assoc_opt "activeParameter" rf with
            | Some j -> ( match int_of_json j with Some n -> n | None -> 0)
            | None -> 0
          in
          let sig_count =
            match List.assoc_opt "signatures" rf with
            | Some (`List xs) -> List.length xs
            | _ -> 0
          in
          Some (active_param, sig_count)
      | _ -> None)
  | _ -> None

let sighelp_params ~(uri : string) ~(line : int) ~(character : int) :
    Yojson.Safe.t =
  `Assoc
    [
      ("textDocument", `Assoc [ ("uri", `String uri) ]);
      ("position", `Assoc [ ("line", `Int line); ("character", `Int character) ]);
    ]

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
      (max 1
         (Lsp_test_helpers.getenv_int "JOVIAL_TEST_SIGNATURE_HELP_TIMEOUT_S"
            ~default:10))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase : string) : unit =
    if remaining_budget () <= 0.0 then
      failf "signatureHelp test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-signature-help" in
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC ADD(A, B) RENT;";
        "BEGIN";
        "END";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ADD(1, 2);";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main_path main_text;

  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let line, col =
    Lsp_test_helpers.line_col_of_first main_text ~needle:"ADD(1, 2);"
  in
  let pos_char = col + 7 in

  Lsp_test_helpers.with_server ~server_path (fun srv ->
      ensure_budget "before initialize";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri
           ~doc_text:main_text
           ~timeout_s:(min timeout_s (remaining_budget ())));

      let resp, _ =
        Lsp_test_helpers.request_timed srv ~id:2
          ~method_:"textDocument/signatureHelp"
          ~params:(sighelp_params ~uri:doc_uri ~line ~character:pos_char)
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      (match signature_result resp with
      | Some (active_param, sig_count) ->
          if sig_count <= 0 then
            failf "signatureHelp returned no signatures: %s"
              (Yojson.Safe.to_string resp);
          if active_param <> 1 then
            failf "signatureHelp expected activeParameter=1, got %d"
              active_param
      | None ->
          failf "signatureHelp returned invalid/errored response: %s"
            (Yojson.Safe.to_string resp));

      Lsp_test_helpers.shutdown_and_exit srv
        ~timeout_s:(min timeout_s (remaining_budget ()));
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_signature_help_test: ok"
