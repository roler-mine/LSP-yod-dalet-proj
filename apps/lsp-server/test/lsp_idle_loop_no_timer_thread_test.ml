let failf = Lsp_test_helpers.failf

let int_of_json = function
  | `Int n -> Some n
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let metric_calls (resp:Yojson.Safe.t) ~(name:string) : int =
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`Assoc rf) ->
           (match List.assoc_opt "metrics" rf with
            | Some (`List entries) ->
                entries
                |> List.find_map (function
                     | `Assoc mf ->
                         (match List.assoc_opt "name" mf with
                          | Some (`String metric_name) when metric_name = name ->
                              (match List.assoc_opt "calls" mf with
                               | Some calls_json -> int_of_json calls_json
                               | None -> Some 0)
                          | _ -> None)
                     | _ -> None)
                |> (function Some n -> n | None -> 0)
            | _ -> 0)
       | _ -> 0)
  | _ -> 0

let debug_perf_params : Yojson.Safe.t =
  `Assoc [
    "command", `String "jovial.debugPerfStats";
    "arguments", `List [];
  ]

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300))
  in
  let timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_IDLE_LOOP_TIMEOUT_S" ~default:10))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    if remaining_budget () <= 0.0 then
      failf "idle-loop test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-idle-loop" in
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [ "START"; "DEF PROC MAIN RENT;"; "BEGIN"; "END"; "TERM"; "" ]
  in
  Lsp_test_helpers.write_text main_path main_text;

  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in

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

    Lsp_test_helpers.sleep_seconds 0.25;

    let perf_resp, _ =
      Lsp_test_helpers.request_timed srv
        ~id:2
        ~method_:"workspace/executeCommand"
        ~params:debug_perf_params
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    let select_wait_calls = metric_calls perf_resp ~name:"idle.select_wait" in
    let bg_tick_calls = metric_calls perf_resp ~name:"idle.bg_tick" in
    if select_wait_calls <= 0 then
      failf "expected idle.select_wait calls > 0, got %d" select_wait_calls;
    if bg_tick_calls <= 0 then
      failf "expected idle.bg_tick calls > 0, got %d" bg_tick_calls;

    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );

  print_endline "lsp_idle_loop_no_timer_thread_test: ok"
