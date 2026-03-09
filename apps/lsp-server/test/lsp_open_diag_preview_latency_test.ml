let failf = Lsp_test_helpers.failf

let exec_params ~(command : string) ~(arguments : Yojson.Safe.t list) :
    Yojson.Safe.t =
  `Assoc [ ("command", `String command); ("arguments", `List arguments) ]

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
  let req_id = ref 6000 in
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

let wait_uri_diags_or_fail ~(srv : Lsp_test_helpers.server_proc) ~(uri : string)
    ~(timeout_s : float) ~(phase : string) : unit =
  let t0 = Unix.gettimeofday () in
  match
    Lsp_test_helpers.wait_for_publish_diagnostics_for_uri ~srv ~target_uri:uri
      ~timeout_s
  with
  | None ->
      failf "did not receive publishDiagnostics for %s within %.2fs at %s" uri
        timeout_s phase
  | Some _ ->
      let elapsed = Unix.gettimeofday () -. t0 in
      if elapsed > timeout_s +. 0.001 then
        failf "publishDiagnostics for %s exceeded %.2fs at %s (got %.3fs)" uri
          timeout_s phase elapsed

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

let startup_open_docs_pending_parse_of_report
    (report_fields : (string * Yojson.Safe.t) list) : int =
  match List.assoc_opt "startup" report_fields with
  | Some (`Assoc startup_fields) -> (
      match List.assoc_opt "components" startup_fields with
      | Some (`Assoc components) -> (
          match List.assoc_opt "openDocsPendingParse" components with
          | Some (`Int n) -> n
          | Some (`Intlit s) -> ( try int_of_string s with _ -> 0)
          | _ -> 0)
      | _ -> 0)
  | _ -> 0

let startup_xmodule_diag_ready_of_report
    (report_fields : (string * Yojson.Safe.t) list) : bool =
  match List.assoc_opt "startup" report_fields with
  | Some (`Assoc startup_fields) -> (
      match List.assoc_opt "components" startup_fields with
      | Some (`Assoc components) -> (
          match List.assoc_opt "xmoduleDiagReady" components with
          | Some (`Bool b) -> b
          | _ -> false)
      | _ -> false)
  | _ -> false

let startup_diag_hover_ready_of_report
    (report_fields : (string * Yojson.Safe.t) list) : bool =
  match List.assoc_opt "startup" report_fields with
  | Some (`Assoc startup_fields) -> (
      match List.assoc_opt "stages" startup_fields with
      | Some (`Assoc stages) -> (
          match List.assoc_opt "diagHoverReady" stages with
          | Some (`Assoc stage_fields) -> (
              match List.assoc_opt "isReady" stage_fields with
              | Some (`Bool b) -> b
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | _ -> false

let wait_for_ready_convergence ~(srv : Lsp_test_helpers.server_proc)
    ~(uri : string) ~(timeout_s : float) : unit =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let req_id = ref 9000 in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then
      failf "timed out waiting for diagnostics-stage readiness convergence";
    let report =
      debug_report_assoc ~srv ~id:!req_id ~uri ~timeout_s:(min 4.0 remaining)
    in
    incr req_id;
    let pending = startup_open_docs_pending_parse_of_report report in
    let xmodule_ready = startup_xmodule_diag_ready_of_report report in
    let diag_ready = startup_diag_hover_ready_of_report report in
    if pending = 0 && xmodule_ready && diag_ready then ()
    else (
      Lsp_test_helpers.sleep_seconds 0.2;
      loop ())
  in
  loop ()

let basecomp_text () : string =
  let body = Buffer.create 32768 in
  for i = 0 to 2199 do
    Buffer.add_string body
      (Printf.sprintf
         "DEF PROC BASEF%03d RENT U 4 (X);\n\
          BEGIN\n\
         \ BASEF%03d = HOTFUNC(X) + %d;\n\
          END\n\n"
         i i i)
  done;
  String.concat ""
    [
      "START\n";
      "COMPOOL BASECOMP;\n\n";
      "DEF BEGIN\n";
      " ITEM HOTVAR U 4 = 1;\n";
      " ITEM HOTACC U 4 = 0;\n";
      " TABLE HOTTAB (1:64) U 4;\n";
      "END\n\n";
      "DEF PROC HOTPROC RENT (X);\n";
      "BEGIN\n";
      " ITEM TMP U 4;\n";
      " TMP = X + HOTVAR;\n";
      " HOTACC = HOTACC + TMP;\n";
      "END\n\n";
      "DEF PROC HOTFUNC RENT U 4 (X);\n";
      "BEGIN\n";
      " HOTFUNC = X + HOTVAR + HOTACC;\n";
      "END\n\n";
      Buffer.contents body;
      "TERM\n";
    ]

let main_text ~(i : int) : string =
  Printf.sprintf
    "START\n\
     !COMPOOL ('BASECOMP');\n\
     DEF PROC MAIN%03d RENT;\n\
     BEGIN\n\
    \ ITEM X U 4;\n\
    \ X = HOTFUNC(%d);\n\
     END\n\
     TERM\n"
    i i

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let root =
    Lsp_test_helpers.mk_temp_dir "jovial-lsp-open-diag-preview-latency"
  in
  let base_path = Filename.concat root "BASECOMP.j73" in
  let base_text = basecomp_text () in
  Lsp_test_helpers.write_text base_path base_text;
  for i = 0 to 95 do
    let path = Filename.concat root (Printf.sprintf "MAIN%03d.j73" i) in
    Lsp_test_helpers.write_text path (main_text ~i)
  done;
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let base_uri = Lsp_test_helpers.lsp_doc_uri_of_path base_path in
  Lsp_test_helpers.with_server
    ~env:
      [
        ("JOVIAL_WORKSPACE_DIAGS_MODE", "off");
        ("JOVIAL_BG_TICK_BUDGET_MS", "24");
      ]
    ~server_path
    (fun srv ->
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri:base_uri
           ~doc_text:base_text ~timeout_s:4.0);
      wait_uri_diags_or_fail ~srv ~uri:base_uri ~timeout_s:2.0
        ~phase:"didOpen BASECOMP";
      wait_for_metric_calls ~srv ~name:"diag.open.preview_publish" ~min_calls:1
        ~timeout_s:6.0;
      wait_for_metric_calls ~srv ~name:"diag.open.inline_work_deferred"
        ~min_calls:1 ~timeout_s:6.0;
      wait_for_ready_convergence ~srv ~uri:base_uri ~timeout_s:60.0;
      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:6.0;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:3.0));
  print_endline "lsp_open_diag_preview_latency_test: ok"
