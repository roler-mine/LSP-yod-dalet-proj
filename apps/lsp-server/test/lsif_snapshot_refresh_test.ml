let failf = Lsp_test_helpers.failf

let int_of_json = function
  | `Int n -> Some n
  | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
  | _ -> None

let metric_calls (resp : Yojson.Safe.t) ~(name : string) : int =
  match resp with
  | `Assoc fields -> (
      match List.assoc_opt "result" fields with
      | Some (`Assoc rf) -> (
          match List.assoc_opt "metrics" rf with
          | Some (`List entries) -> (
              entries
              |> List.find_map (function
                | `Assoc mf -> (
                    match List.assoc_opt "name" mf with
                    | Some (`String metric_name) when metric_name = name -> (
                        match List.assoc_opt "calls" mf with
                        | Some calls_json -> int_of_json calls_json
                        | None -> Some 0)
                    | _ -> None)
                | _ -> None)
              |> function
              | Some n -> n
              | None -> 0)
          | _ -> 0)
      | _ -> 0)
  | _ -> 0

let exec_params ~(command : string) ~(arguments : Yojson.Safe.t list) :
    Yojson.Safe.t =
  `Assoc [ ("command", `String command); ("arguments", `List arguments) ]

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
         (Lsp_test_helpers.getenv_int "JOVIAL_TEST_LSIF_SNAPSHOT_TIMEOUT_S"
            ~default:10))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase : string) : unit =
    if remaining_budget () <= 0.0 then
      failf "lsif snapshot test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-lsif-snapshot" in
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
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri
           ~doc_text:main_text
           ~timeout_s:(min timeout_s (remaining_budget ())));

      ignore
        (Lsp_test_helpers.request_timed srv ~id:2
           ~method_:"workspace/executeCommand"
           ~params:
             (exec_params ~command:"jovial.dumpLsifIndex"
                ~arguments:[ `String doc_uri ])
           ~timeout_s:(min timeout_s (remaining_budget ())));

      ignore
        (Lsp_test_helpers.request_timed srv ~id:3
           ~method_:"workspace/executeCommand"
           ~params:
             (exec_params ~command:"jovial.dumpLsifIndex"
                ~arguments:[ `String doc_uri ])
           ~timeout_s:(min timeout_s (remaining_budget ())));

      let perf_resp, _ =
        Lsp_test_helpers.request_timed srv ~id:4
          ~method_:"workspace/executeCommand"
          ~params:(exec_params ~command:"jovial.debugPerfStats" ~arguments:[])
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      let miss_calls = metric_calls perf_resp ~name:"lsif.snapshot_miss" in
      let hit_calls = metric_calls perf_resp ~name:"lsif.snapshot_hit" in
      if miss_calls <= 0 then
        failf "expected lsif.snapshot_miss calls > 0, got %d" miss_calls;
      if hit_calls <= 0 then
        failf "expected lsif.snapshot_hit calls > 0, got %d" hit_calls;

      Lsp_test_helpers.shutdown_and_exit srv
        ~timeout_s:(min timeout_s (remaining_budget ()));
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsif_snapshot_refresh_test: ok"
