let failf = Lsp_test_helpers.failf

let exec_params ~(command : string) ~(arguments : Yojson.Safe.t list) :
    Yojson.Safe.t =
  `Assoc [ ("command", `String command); ("arguments", `List arguments) ]

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

let did_change_full_sync_params ~(uri : string) ~(version : int)
    ~(text : string) : Yojson.Safe.t =
  `Assoc
    [
      ( "textDocument",
        `Assoc [ ("uri", `String uri); ("version", `Int version) ] );
      ("contentChanges", `List [ `Assoc [ ("text", `String text) ] ]);
    ]

let perf_metric_calls ~(srv : Lsp_test_helpers.server_proc) ~(id : int)
    ~(name : string) ~(timeout_s : float) : int =
  let resp, _ =
    Lsp_test_helpers.request_timed srv ~id ~method_:"workspace/executeCommand"
      ~params:(exec_params ~command:"jovial.debugPerfStats" ~arguments:[])
      ~timeout_s
  in
  match resp with
  | `Assoc fields -> (
      match List.assoc_opt "result" fields with
      | Some (`Assoc result_fields) -> (
          match List.assoc_opt "metrics" result_fields with
          | Some (`List metrics) ->
              let rec find = function
                | [] -> 0
                | `Assoc mfields :: tl -> (
                    match
                      ( List.assoc_opt "name" mfields,
                        List.assoc_opt "calls" mfields )
                    with
                    | Some (`String metric_name), Some (`Int calls)
                      when metric_name = name ->
                        calls
                    | Some (`String metric_name), Some (`Intlit s)
                      when metric_name = name -> (
                        try int_of_string s with _ -> 0)
                    | _ -> find tl)
                | _ :: tl -> find tl
              in
              find metrics
          | _ -> 0)
      | _ -> 0)
  | _ -> 0

let wait_for_metric_calls ~(srv : Lsp_test_helpers.server_proc) ~(name : string)
    ~(min_calls : int) ~(timeout_s : float) : unit =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let req_id = ref 4000 in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then
      failf "timed out waiting for perf metric %s >= %d" name min_calls;
    let calls =
      perf_metric_calls ~srv ~id:!req_id ~name ~timeout_s:(min 0.8 remaining)
    in
    incr req_id;
    if calls >= min_calls then ()
    else (
      Lsp_test_helpers.sleep_seconds 0.1;
      loop ())
  in
  loop ()

let debug_report_assoc ~(srv : Lsp_test_helpers.server_proc) ~(id : int)
    ~(uri : string) ~(timeout_s : float) : (string * Yojson.Safe.t) list =
  let resp, _ =
    Lsp_test_helpers.request_timed srv ~id ~method_:"workspace/executeCommand"
      ~params:
        (exec_params ~command:"jovial.debugReport"
           ~arguments:[ `String uri; `Int 0 ])
      ~timeout_s
  in
  match resp with
  | `Assoc fields -> (
      match List.assoc_opt "result" fields with
      | Some (`Assoc result_fields) -> result_fields
      | Some other ->
          failf "debugReport returned non-object result: %s"
            (Yojson.Safe.to_string other)
      | None -> failf "debugReport response missing result")
  | _ -> failf "debugReport response was not an object"

let open_diag_revalidate_pending_of_report
    (report_fields : (string * Yojson.Safe.t) list) : int =
  match List.assoc_opt "startup" report_fields with
  | Some (`Assoc startup_fields) -> (
      match List.assoc_opt "components" startup_fields with
      | Some (`Assoc components) -> (
          match List.assoc_opt "openDiagRevalidatePending" components with
          | Some (`Int n) -> n
          | Some (`Intlit s) -> ( try int_of_string s with _ -> 0)
          | _ -> 0)
      | _ -> 0)
  | _ -> 0

let open_diag_revalidate_queue_empty_of_report
    (report_fields : (string * Yojson.Safe.t) list) : bool =
  match List.assoc_opt "startup" report_fields with
  | Some (`Assoc startup_fields) -> (
      match List.assoc_opt "components" startup_fields with
      | Some (`Assoc components) -> (
          match List.assoc_opt "openDiagRevalidateQueueEmpty" components with
          | Some (`Bool b) -> b
          | _ -> false)
      | _ -> false)
  | _ -> false

let wait_for_open_revalidate_zero ~(srv : Lsp_test_helpers.server_proc)
    ~(uri : string) ~(timeout_s : float) : unit =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let req_id = ref 7000 in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then
      failf "timed out waiting for openDiagRevalidatePending=0";
    let report =
      debug_report_assoc ~srv ~id:!req_id ~uri ~timeout_s:(min 1.5 remaining)
    in
    incr req_id;
    if
      open_diag_revalidate_pending_of_report report = 0
      && open_diag_revalidate_queue_empty_of_report report
    then ()
    else (
      Lsp_test_helpers.sleep_seconds 0.1;
      loop ())
  in
  loop ()

let wait_uri_diags_or_fail ~(srv : Lsp_test_helpers.server_proc) ~(uri : string)
    ~(timeout_s : float) ~(phase : string) : unit =
  match
    Lsp_test_helpers.wait_for_publish_diagnostics_for_uri ~srv ~target_uri:uri
      ~timeout_s
  with
  | Some _ -> ()
  | None ->
      failf "did not receive publishDiagnostics for %s within %.2fs at %s" uri
        timeout_s phase

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
            "JOVIAL_TEST_OPEN_DIAG_REVALIDATE_STEADY_HARD_TIMEOUT_S"
            ~default:180))
  in
  let started = Unix.gettimeofday () in
  let ensure_budget phase =
    if Unix.gettimeofday () -. started > hard_timeout_s then
      failf "open-diag steady-state test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-open-diag-steady-state" in
  let pool_path = Filename.concat root "POOLA.j73" in
  let body = Buffer.create 200000 in
  for i = 0 to 8000 do
    Buffer.add_string body (Printf.sprintf "  ITEM EXIST_%05d U 1;\n" i)
  done;
  let pool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL POOLA;";
        "DEF BEGIN";
        Buffer.contents body;
        "END";
        "TERM";
        "";
      ]
  in
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('POOLA');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM X U 1;";
        "  X = MISSING'POOLA;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text pool_path pool_text;
  Lsp_test_helpers.write_text main_path main_text;
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let main_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let pool_uri = Lsp_test_helpers.lsp_doc_uri_of_path pool_path in

  Lsp_test_helpers.with_server
    ~env:
      [
        ("JOVIAL_WORKSPACE_DIAGS_MODE", "all");
        ("JOVIAL_DIAG_WARMUP_SUPPRESS_XMODULE", "true");
        ("JOVIAL_BG_TICK_BUDGET_MS", "24");
      ]
    ~server_path
    (fun srv ->
      ensure_budget "initialize/open";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri:main_uri
           ~doc_text:main_text ~timeout_s:4.0);
      wait_uri_diags_or_fail ~srv ~uri:main_uri ~timeout_s:6.0
        ~phase:"didOpen MAIN";

      ensure_budget "didOpen pool";
      Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen"
        ~params:(did_open_params ~uri:pool_uri ~version:2 ~text:pool_text);
      wait_uri_diags_or_fail ~srv ~uri:pool_uri ~timeout_s:8.0
        ~phase:"didOpen POOLA";

      ensure_budget "didChange pool full-sync";
      let pool_text_changed = pool_text ^ "\n!changed\n" in
      Lsp_test_helpers.send_notification srv ~method_:"textDocument/didChange"
        ~params:
          (did_change_full_sync_params ~uri:pool_uri ~version:3
             ~text:pool_text_changed);

      ensure_budget "open revalidate metrics";
      wait_for_metric_calls ~srv ~name:"diag.open.revalidate_enqueued"
        ~min_calls:1 ~timeout_s:20.0;
      wait_for_metric_calls ~srv ~name:"diag.open.revalidate_drained"
        ~min_calls:1 ~timeout_s:20.0;
      wait_for_metric_calls ~srv ~name:"diag.open.revalidate_fast_path"
        ~min_calls:1 ~timeout_s:20.0;
      wait_for_metric_calls ~srv ~name:"store_doc.side_effects_skipped"
        ~min_calls:1 ~timeout_s:20.0;

      ensure_budget "wait open revalidate queue empty";
      wait_for_open_revalidate_zero ~srv ~uri:main_uri ~timeout_s:20.0;

      ensure_budget "steady-state metric plateau";
      let enqueued_before =
        perf_metric_calls ~srv ~id:9101 ~name:"diag.open.revalidate_enqueued"
          ~timeout_s:2.0
      in
      Lsp_test_helpers.sleep_seconds 0.6;
      wait_for_open_revalidate_zero ~srv ~uri:main_uri ~timeout_s:6.0;
      let enqueued_after =
        perf_metric_calls ~srv ~id:9102 ~name:"diag.open.revalidate_enqueued"
          ~timeout_s:2.0
      in
      if enqueued_after <> enqueued_before then
        failf
          "expected diag.open.revalidate_enqueued to plateau after queue drain \
           (before=%d after=%d)"
          enqueued_before enqueued_after;

      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:4.0;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_open_diag_revalidate_steady_state_test: ok"
