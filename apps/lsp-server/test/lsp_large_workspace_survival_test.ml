let failf = Lsp_test_helpers.failf

let getenv_int (name : string) ~(default : int) : int =
  Lsp_test_helpers.getenv_int name ~default

let write_text = Lsp_test_helpers.write_text

let has_error (resp : Yojson.Safe.t) : bool =
  match resp with
  | `Assoc fields -> (
      match List.assoc_opt "error" fields with Some _ -> true | None -> false)
  | _ -> true

let watched_change_obj ~(uri : string) : Yojson.Safe.t =
  `Assoc [ ("uri", `String uri); ("type", `Int 2) ]

let watched_files_params (uris : string list) : Yojson.Safe.t =
  `Assoc
    [ ("changes", `List (List.map (fun u -> watched_change_obj ~uri:u) uris)) ]

let references_params ~(uri : string) ~(line : int) ~(character : int) :
    Yojson.Safe.t =
  `Assoc
    [
      ("textDocument", `Assoc [ ("uri", `String uri) ]);
      ("position", `Assoc [ ("line", `Int line); ("character", `Int character) ]);
      ("context", `Assoc [ ("includeDeclaration", `Bool true) ]);
    ]

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int
      (max 1 (getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300))
  in
  let timeout_s =
    float_of_int
      (max 2 (getenv_int "JOVIAL_TEST_LARGE_WS_SURV_TIMEOUT_S" ~default:18))
  in
  let file_count =
    max 64 (getenv_int "JOVIAL_TEST_LARGE_WS_SURV_FILES" ~default:240)
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase : string) : unit =
    if remaining_budget () <= 0.0 then
      failf "large workspace survival test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-large-survival" in
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
  write_text main_path main_text;
  let extra_paths =
    List.init file_count (fun i ->
        let p = Filename.concat root (Printf.sprintf "W%04d.j73" i) in
        let txt =
          String.concat "\n"
            [
              "START";
              Printf.sprintf "DEF PROC P_%04d RENT;" i;
              "BEGIN";
              "  ITEM Y U 1;";
              "  Y = Y + 1;";
              "END";
              "TERM";
              "";
            ]
        in
        write_text p txt;
        p)
  in
  let changed_uris =
    extra_paths |> List.map Lsp_test_helpers.lsp_doc_uri_of_path
  in
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let line, col =
    Lsp_test_helpers.line_col_of_first main_text ~needle:"X = X + 1;"
  in
  let x_char = col + 1 in

  Lsp_test_helpers.with_server ~server_path (fun srv ->
      ensure_budget "before initialize";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri
           ~doc_text:main_text
           ~timeout_s:(min timeout_s (remaining_budget ())));

      for round = 1 to 3 do
        ensure_budget (Printf.sprintf "before watched changes round %d" round);
        Lsp_test_helpers.send_notification srv
          ~method_:"workspace/didChangeWatchedFiles"
          ~params:(watched_files_params changed_uris);

        let def_resp, _ =
          Lsp_test_helpers.request_timed srv ~id:(100 + round)
            ~method_:"textDocument/definition"
            ~params:
              (Lsp_test_helpers.definition_request_params ~uri:doc_uri ~line
                 ~character:x_char)
            ~timeout_s:(min timeout_s (remaining_budget ()))
        in
        if has_error def_resp then
          failf "definition returned error during survival round %d" round;

        let refs_resp, _ =
          Lsp_test_helpers.request_timed srv ~id:(200 + round)
            ~method_:"textDocument/references"
            ~params:(references_params ~uri:doc_uri ~line ~character:x_char)
            ~timeout_s:(min timeout_s (remaining_budget ()))
        in
        if has_error refs_resp then
          failf "references returned error during survival round %d" round
      done;

      Lsp_test_helpers.shutdown_and_exit srv
        ~timeout_s:(min timeout_s (remaining_budget ()));
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_large_workspace_survival_test: ok"
