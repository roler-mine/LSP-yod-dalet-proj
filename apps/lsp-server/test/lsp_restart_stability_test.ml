let failf = Lsp_test_helpers.failf

let expect_hover_result (resp : Yojson.Safe.t) : unit =
  match resp with
  | `Assoc fields -> (
      match List.assoc_opt "result" fields with
      | Some `Null -> failf "hover returned null result"
      | Some (`Assoc _) -> ()
      | _ -> failf "hover returned unexpected result shape")
  | _ -> failf "hover response is not a JSON object"

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

let hover_params ~(uri : string) ~(line : int) ~(character : int) :
    Yojson.Safe.t =
  `Assoc
    [
      ("textDocument", `Assoc [ ("uri", `String uri) ]);
      ("position", `Assoc [ ("line", `Int line); ("character", `Int character) ]);
    ]

let run_cycle ~(server_path : string) ~(root_uri : string) ~(doc_uri : string)
    ~(timeout_s : float) ~(graceful : bool) ~(cycle : int) : unit =
  Lsp_test_helpers.with_server ~server_path (fun srv ->
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri
           ~doc_text:main_text ~timeout_s);
      let line, col =
        Lsp_test_helpers.line_col_of_first main_text ~needle:"X = X + 1;"
      in
      let params = hover_params ~uri:doc_uri ~line ~character:(col + 1) in
      let hover_resp, _ =
        Lsp_test_helpers.request_timed srv ~id:(2000 + cycle)
          ~method_:"textDocument/hover" ~params ~timeout_s
      in
      expect_hover_result hover_resp;
      if graceful then (
        Lsp_test_helpers.shutdown_and_exit srv ~timeout_s;
        Lsp_test_helpers.close_stdin srv;
        match Lsp_test_helpers.wait_for_exit srv ~timeout_s with
        | Unix.WEXITED 0 -> ()
        | st ->
            failf "cycle %d graceful restart exited with %s" cycle
              (match st with
              | Unix.WEXITED n -> Printf.sprintf "code=%d" n
              | Unix.WSIGNALED n -> Printf.sprintf "signaled=%d" n
              | Unix.WSTOPPED n -> Printf.sprintf "stopped=%d" n))
      else (
        Lsp_test_helpers.send_notification srv ~method_:"exit" ~params:`Null;
        Lsp_test_helpers.close_stdin srv;
        ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s)))

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int
      (max 1
         (Lsp_test_helpers.getenv_int "JOVIAL_TEST_RESTART_HARD_TIMEOUT_S"
            ~default:300))
  in
  let cycles =
    max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_RESTART_CYCLES" ~default:14)
  in
  let timeout_s =
    float_of_int
      (max 1
         (Lsp_test_helpers.getenv_int "JOVIAL_TEST_RESTART_TIMEOUT_S" ~default:8))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-restart" in
  let main_path = Filename.concat root "MAIN.j73" in
  Lsp_test_helpers.write_text main_path main_text;
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  for i = 1 to cycles do
    let left = remaining_budget () in
    if left <= 0.0 then
      failf "restart stability test exceeded hard timeout (%.1fs) at cycle %d"
        hard_timeout_s i;
    let cycle_timeout = min timeout_s left in
    if cycle_timeout <= 0.0 then
      failf "restart stability test has no remaining timeout budget at cycle %d"
        i;
    let graceful = i mod 2 = 1 in
    run_cycle ~server_path ~root_uri ~doc_uri ~timeout_s:cycle_timeout ~graceful
      ~cycle:i
  done;
  print_endline "lsp_restart_stability_test: ok"
