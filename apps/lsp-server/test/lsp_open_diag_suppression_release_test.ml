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

let diag_messages (diags : Yojson.Safe.t list) : string list =
  diags |> List.filter_map Lsp_test_helpers.diag_message

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
  let req_id = ref 3000 in
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

let wait_for_workspace_ready_stage ~(srv : Lsp_test_helpers.server_proc)
    ~(stage : string) ~(timeout_s : float) : unit =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then
      failf "timed out waiting for jovial/workspaceReady stage=%s" stage;
    let chunk = min 0.25 remaining in
    try
      let msg = Lsp_test_helpers.wait_for_message srv ~timeout_s:chunk in
      match msg with
      | `Assoc fields -> (
          match
            (List.assoc_opt "method" fields, List.assoc_opt "params" fields)
          with
          | Some (`String "jovial/workspaceReady"), Some (`Assoc params) -> (
              match List.assoc_opt "stage" params with
              | Some (`String got) when got = stage -> ()
              | _ -> loop ())
          | _ -> loop ())
      | _ -> loop ()
    with
    | Failure m when Lsp_test_helpers.is_timeout_failure_message m -> loop ()
    | exn -> raise exn
  in
  loop ()

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
            "JOVIAL_TEST_OPEN_DIAG_SUPPRESS_HARD_TIMEOUT_S" ~default:180))
  in
  let started = Unix.gettimeofday () in
  let ensure_budget phase =
    if Unix.gettimeofday () -. started > hard_timeout_s then
      failf
        "open-diag suppression release test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root =
    Lsp_test_helpers.mk_temp_dir "jovial-lsp-open-diag-suppress-release"
  in
  let pool_path = Filename.concat root "POOLA.j73" in
  let pool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL POOLA;";
        "DEF BEGIN";
        "  ITEM EXISTS U 1;";
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

  Lsp_test_helpers.with_server
    ~env:
      [
        ("JOVIAL_WORKSPACE_DIAGS_MODE", "all");
        ("JOVIAL_DIAG_WARMUP_SUPPRESS_XMODULE", "true");
        ("JOVIAL_BG_TICK_BUDGET_MS", "20");
      ]
    ~server_path
    (fun srv ->
      ensure_budget "initialize/open";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri:main_uri
           ~doc_text:main_text ~timeout_s:4.0);

      ensure_budget "first diagnostics";
      let first_diags =
        match
          Lsp_test_helpers.wait_for_publish_diagnostics_for_uri ~srv
            ~target_uri:main_uri ~timeout_s:4.0
        with
        | Some ds -> ds
        | None -> failf "missing first publishDiagnostics for %s" main_uri
      in
      let first_msgs = diag_messages first_diags in
      if
        List.exists
          (fun m -> contains_substring ~haystack:m ~needle:"MISSING")
          first_msgs
      then
        failf
          "unexpected unresolved cross-module diagnostic before readiness \
           release";

      ensure_budget "wait diagHoverReady";
      wait_for_workspace_ready_stage ~srv ~stage:"diagHoverReady"
        ~timeout_s:25.0;

      ensure_budget "metric suppression/ready transition";
      wait_for_metric_calls ~srv ~name:"diag.xmodule_ready_transition"
        ~min_calls:1 ~timeout_s:8.0;
      wait_for_metric_calls ~srv ~name:"diag.open.revalidate_drained"
        ~min_calls:1 ~timeout_s:8.0;

      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:4.0;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_open_diag_suppression_release_test: ok"
