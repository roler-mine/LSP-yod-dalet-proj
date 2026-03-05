let failf = Lsp_test_helpers.failf

let wait_for_workspace_ready_stage
    ~(srv:Lsp_test_helpers.server_proc)
    ~(stage:string)
    ~(timeout_s:float)
  : unit =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then
      failf "timed out waiting for jovial/workspaceReady stage=%s" stage;
    let chunk = min 0.25 remaining in
    try
      let msg = Lsp_test_helpers.wait_for_message srv ~timeout_s:chunk in
      match msg with
      | `Assoc fields ->
          (match List.assoc_opt "method" fields, List.assoc_opt "params" fields with
           | Some (`String "jovial/workspaceReady"), Some (`Assoc params) ->
               (match List.assoc_opt "stage" params with
                | Some (`String got) when got = stage -> ()
                | _ -> loop ())
           | _ -> loop ())
      | _ -> loop ()
    with
    | Failure m when Lsp_test_helpers.is_timeout_failure_message m -> loop ()
    | exn -> raise exn
  in
  loop ()

let mk_large_main_text () : string * (int * int) =
  let body = Buffer.create 300000 in
  for i = 0 to 22000 do
    Buffer.add_string body (Printf.sprintf "  ITEM X_%05d U 1;\n" i)
  done;
  Buffer.add_string body "  TARGET'NAME = 1;\n";
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAINPROC RENT;";
        "BEGIN";
        Buffer.contents body;
        "  STOP 0;";
        "END";
        "TERM";
        "";
      ]
  in
  let line, col = Lsp_test_helpers.line_col_of_first text ~needle:"MAINPROC" in
  (text, (line, col))

let definition_params ~(uri:string) ~(line:int) ~(character:int) : Yojson.Safe.t =
  `Assoc
    [
      "textDocument", `Assoc [ "uri", `String uri ];
      "position", `Assoc [ "line", `Int line; "character", `Int character ];
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
         (Lsp_test_helpers.getenv_int
            "JOVIAL_TEST_OPEN_DIAG_PRIORITY_TRAFFIC_HARD_TIMEOUT_S"
            ~default:180))
  in
  let started = Unix.gettimeofday () in
  let ensure_budget phase =
    if Unix.gettimeofday () -. started > hard_timeout_s then
      failf
        "open-diag priority under nav traffic test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s
        phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-open-diag-priority-traffic" in
  let main_path = Filename.concat root "MAINBIG.j73" in
  let main_text, (needle_line, needle_col) = mk_large_main_text () in
  Lsp_test_helpers.write_text main_path main_text;
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let main_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in

  Lsp_test_helpers.with_server
    ~env:
      [
        "JOVIAL_DIDOPEN_DEFER_PARSE", "true";
        "JOVIAL_DIDOPEN_DEFER_MIN_DOC_CHARS", "1";
        "JOVIAL_DIDOPEN_DISABLE_FOREGROUND_TICK", "true";
        "JOVIAL_BG_TICK_BUDGET_MS", "16";
        "JOVIAL_STARTUP_DIAG_HOVER_TARGET_MS", "15000";
      ]
    ~server_path
    (fun srv ->
      ensure_budget "initialize/open";
      ignore
        (Lsp_test_helpers.initialize_and_open
           srv
           ~root_uri
           ~doc_uri:main_uri
           ~doc_text:main_text
           ~timeout_s:6.0);

      let stop_traffic = ref false in
      let traffic_error : string option ref = ref None in
      let traffic_thread =
        Thread.create
          (fun () ->
            let req_id = ref 5000 in
            let until = Unix.gettimeofday () +. 14.0 in
            try
              while (not !stop_traffic) && Unix.gettimeofday () < until do
                let params =
                  definition_params
                    ~uri:main_uri
                    ~line:needle_line
                    ~character:needle_col
                in
                Lsp_test_helpers.send_request
                  srv
                  ~id:!req_id
                  ~method_:"textDocument/definition"
                  ~params;
                incr req_id;
                Lsp_test_helpers.sleep_seconds 0.01
              done
            with exn ->
              traffic_error := Some (Printexc.to_string exn))
          ()
      in

      ensure_budget "wait diagHoverReady";
      wait_for_workspace_ready_stage
        ~srv
        ~stage:"diagHoverReady"
        ~timeout_s:25.0;
      stop_traffic := true;
      Thread.join traffic_thread;

      (match !traffic_error with
       | None -> ()
       | Some msg -> failf "nav traffic thread failed: %s" msg);

      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:4.0;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_open_diag_priority_under_nav_traffic_test: ok"
