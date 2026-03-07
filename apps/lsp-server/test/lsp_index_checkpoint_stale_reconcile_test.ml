let failf = Lsp_test_helpers.failf

let assoc_field (name : string) (fields : (string * Yojson.Safe.t) list) :
    Yojson.Safe.t option =
  List.assoc_opt name fields

let int_of_json = function
  | `Int n -> Some n
  | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
  | _ -> None

let bool_of_json = function `Bool b -> Some b | _ -> None

let exec_params ~(command : string) ~(arguments : Yojson.Safe.t list) :
    Yojson.Safe.t =
  `Assoc [ ("command", `String command); ("arguments", `List arguments) ]

let command_result_assoc ~(srv : Lsp_test_helpers.server_proc) ~(id : int)
    ~(command : string) ~(arguments : Yojson.Safe.t list) ~(timeout_s : float) :
    (string * Yojson.Safe.t) list =
  let resp, _ =
    Lsp_test_helpers.request_timed srv ~id ~method_:"workspace/executeCommand"
      ~params:(exec_params ~command ~arguments)
      ~timeout_s
  in
  match resp with
  | `Assoc fields -> (
      match assoc_field "result" fields with
      | Some (`Assoc rf) -> rf
      | Some other ->
          failf "command %s returned non-object result: %s" command
            (Yojson.Safe.to_string other)
      | None -> failf "command %s response missing result" command)
  | _ -> failf "command %s response was not an object" command

type compools_state = {
  count : int;
  sources : int;
  complete : bool;
  checkpoint_loaded : bool;
  reconcile_pending : bool;
  stale_pruned_count : int;
  sample_names : string list;
}

let compools_state_of_report (report_fields : (string * Yojson.Safe.t) list) :
    compools_state =
  let compools =
    match assoc_field "compools" report_fields with
    | Some (`Assoc cf) -> cf
    | _ -> failf "debugReport missing compools object"
  in
  let read_int name =
    match assoc_field name compools with
    | Some v -> ( match int_of_json v with Some n -> n | None -> 0)
    | None -> 0
  in
  let read_bool name =
    match assoc_field name compools with
    | Some v -> ( match bool_of_json v with Some b -> b | None -> false)
    | None -> false
  in
  let sample_names =
    match assoc_field "sample" compools with
    | Some (`List xs) ->
        xs
        |> List.filter_map (function
          | `Assoc sf -> (
              match assoc_field "name" sf with
              | Some (`String nm) -> Some nm
              | _ -> None)
          | _ -> None)
    | _ -> []
  in
  {
    count = read_int "count";
    sources = read_int "sources";
    complete = read_bool "complete";
    checkpoint_loaded = read_bool "checkpointLoaded";
    reconcile_pending = read_bool "reconcilePending";
    stale_pruned_count = read_int "stalePrunedCount";
    sample_names;
  }

let request_debug_report ~(srv : Lsp_test_helpers.server_proc) ~(id : int)
    ~(uri : string) ~(timeout_s : float) : compools_state =
  command_result_assoc ~srv ~id ~command:"jovial.debugReport"
    ~arguments:[ `String uri ]
    ~timeout_s
  |> compools_state_of_report

let response_has_result_uri (resp : Yojson.Safe.t) ~(uri : string) : bool =
  let want = Lsp_test_helpers.normalize_uri_for_compare uri in
  match resp with
  | `Assoc fields -> (
      match assoc_field "result" fields with
      | Some (`List xs) ->
          List.exists
            (function
              | `Assoc locf -> (
                  match assoc_field "uri" locf with
                  | Some (`String got) ->
                      Lsp_test_helpers.normalize_uri_for_compare got = want
                  | _ -> false)
              | _ -> false)
            xs
      | _ -> false)
  | _ -> false

let request_definition ~(srv : Lsp_test_helpers.server_proc) ~(id : int)
    ~(uri : string) ~(line : int) ~(character : int) ~(timeout_s : float) :
    Yojson.Safe.t * float =
  Lsp_test_helpers.request_timed srv ~id ~method_:"textDocument/definition"
    ~params:(Lsp_test_helpers.definition_request_params ~uri ~line ~character)
    ~timeout_s

let wait_until_index_complete ~(srv : Lsp_test_helpers.server_proc)
    ~(doc_uri : string) ~(timeout_s : float) : unit =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop req_id =
    if Unix.gettimeofday () >= deadline then
      failf "index did not report complete within %.1fs" timeout_s;
    let state =
      request_debug_report ~srv ~id:req_id ~uri:doc_uri
        ~timeout_s:(min 2.0 (max 0.5 (deadline -. Unix.gettimeofday ())))
    in
    if state.complete then ()
    else (
      Thread.delay 0.08;
      loop (req_id + 1))
  in
  loop 100

let wait_for_converged_definition ~(srv : Lsp_test_helpers.server_proc)
    ~(doc_uri : string) ~(line : int) ~(character : int) ~(target_uri : string)
    ~(timeout_s : float) ~(start_id : int) : unit =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop req_id =
    if Unix.gettimeofday () >= deadline then
      failf "definition did not converge to %s within %.1fs" target_uri
        timeout_s;
    let resp, _ =
      request_definition ~srv ~id:req_id ~uri:doc_uri ~line ~character
        ~timeout_s:2.0
    in
    if response_has_result_uri resp ~uri:target_uri then ()
    else (
      Thread.delay 0.1;
      loop (req_id + 1))
  in
  loop start_id

let create_old_workspace ~(root : string) : string * string * string =
  let old_pool = Filename.concat root "OLDPOOL.j73" in
  let old_pool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL OLDPOOL;";
        "DEF BEGIN";
        "  ITEM OLD'VAL U 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text old_pool old_pool_text;
  let main = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('OLDPOOL');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  OLD'VAL = OLD'VAL + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main main_text;
  (main, main_text, old_pool)

let replace_with_new_workspace ~(root : string) ~(old_pool : string) :
    string * string * string =
  (try Sys.remove old_pool with _ -> ());
  let new_pool = Filename.concat root "NEWPOOL.j73" in
  let new_pool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL NEWPOOL;";
        "DEF BEGIN";
        "  ITEM NEW'VAL U 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text new_pool new_pool_text;
  let main = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('NEWPOOL');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  NEW'VAL = NEW'VAL + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main main_text;
  (main, main_text, new_pool)

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
            "JOVIAL_TEST_STALE_RECONCILE_HARD_TIMEOUT_S" ~default:180))
  in
  let converge_timeout_s =
    max 1.0
      (match
         Sys.getenv_opt "JOVIAL_TEST_STALE_RECONCILE_CONVERGE_TIMEOUT_S"
       with
      | None -> 12.0
      | Some raw -> ( try float_of_string (String.trim raw) with _ -> 12.0))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase : string) =
    if remaining_budget () <= 0.0 then
      failf
        "stale checkpoint reconcile test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-stale-reconcile" in
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let old_main, old_main_text, old_pool = create_old_workspace ~root in
  let old_main_uri = Lsp_test_helpers.lsp_doc_uri_of_path old_main in

  Lsp_test_helpers.with_server
    ~env:[ ("JOVIAL_WORKSPACE_DIAGS_MODE", "off") ]
    ~server_path
    (fun srv ->
      ensure_budget "seed checkpoint initialize";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri
           ~doc_uri:old_main_uri ~doc_text:old_main_text ~timeout_s:4.0);
      wait_until_index_complete ~srv ~doc_uri:old_main_uri
        ~timeout_s:(min 8.0 (max 1.0 (remaining_budget ())));
      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:3.0;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  let new_main, new_main_text, new_pool =
    replace_with_new_workspace ~root ~old_pool
  in
  let new_main_uri = Lsp_test_helpers.lsp_doc_uri_of_path new_main in
  let new_pool_uri = Lsp_test_helpers.lsp_doc_uri_of_path new_pool in
  let line, col =
    Lsp_test_helpers.line_col_of_first new_main_text
      ~needle:"NEW'VAL = NEW'VAL + 1;"
  in

  Lsp_test_helpers.with_server
    ~env:
      [
        ("JOVIAL_WORKSPACE_DIAGS_MODE", "off");
        ("JOVIAL_BG_TICK_BUDGET_MS", "12");
      ]
    ~server_path
    (fun srv ->
      ensure_budget "reconcile initialize";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri
           ~doc_uri:new_main_uri ~doc_text:new_main_text ~timeout_s:4.0);

      let first_resp, _ =
        request_definition ~srv ~id:2 ~uri:new_main_uri ~line
          ~character:(col + 2) ~timeout_s:2.0
      in
      if not (response_has_result_uri first_resp ~uri:new_pool_uri) then
        wait_for_converged_definition ~srv ~doc_uri:new_main_uri ~line
          ~character:(col + 2) ~target_uri:new_pool_uri
          ~timeout_s:(min converge_timeout_s (max 1.0 (remaining_budget ())))
          ~start_id:3;

      let rec wait_reconcile_done req_id =
        ensure_budget "wait reconcile complete";
        let state =
          request_debug_report ~srv ~id:req_id ~uri:new_main_uri ~timeout_s:2.0
        in
        if state.reconcile_pending then (
          Thread.delay 0.1;
          wait_reconcile_done (req_id + 1))
        else state
      in
      let final_state = wait_reconcile_done 200 in
      if not final_state.checkpoint_loaded then
        failf "expected checkpointLoaded=true on second server start";
      if final_state.stale_pruned_count <= 0 then
        failf
          "expected stalePrunedCount > 0 after stale checkpoint reconciliation";
      if final_state.count < 1 || final_state.sources < 2 then
        failf
          "unexpected compools/sources after reconciliation (count=%d \
           sources=%d)"
          final_state.count final_state.sources;
      if
        not
          (List.exists
             (fun n -> String.uppercase_ascii n = "NEWPOOL")
             final_state.sample_names)
      then failf "reconciled compool sample did not include NEWPOOL";
      if
        List.exists
          (fun n -> String.uppercase_ascii n = "OLDPOOL")
          final_state.sample_names
      then failf "reconciled compool sample still contains stale OLDPOOL entry";

      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:3.0;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_index_checkpoint_stale_reconcile_test: ok"
