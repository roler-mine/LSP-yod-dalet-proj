let failf = Lsp_test_helpers.failf

let assoc_field (name : string) (fields : (string * Yojson.Safe.t) list) :
    Yojson.Safe.t option =
  List.assoc_opt name fields

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

let debug_checkpoint_loaded ~(srv : Lsp_test_helpers.server_proc) ~(id : int)
    ~(uri : string) ~(timeout_s : float) : bool =
  let report =
    command_result_assoc ~srv ~id ~command:"jovial.debugReport"
      ~arguments:[ `String uri ]
      ~timeout_s
  in
  match assoc_field "compools" report with
  | Some (`Assoc compools) -> (
      match assoc_field "checkpointLoaded" compools with
      | Some v -> ( match bool_of_json v with Some b -> b | None -> false)
      | None -> false)
  | _ -> false

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

let create_checkpoint_seed_workspace ~(root : string) : string * string * string
    =
  let old_pool = Filename.concat root "STALEPOOL.j73" in
  let old_pool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL STALEPOOL;";
        "DEF BEGIN";
        "  ITEM STALE'VAL U 1;";
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
        "!COMPOOL ('STALEPOOL');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  STALE'VAL = STALE'VAL + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main main_text;
  (main, main_text, old_pool)

let replace_workspace_with_fresh_targets ~(root : string) ~(old_pool : string) :
    string * string * string =
  (try Sys.remove old_pool with _ -> ());
  let new_pool = Filename.concat root "FRESHPOOL.j73" in
  let new_pool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL FRESHPOOL;";
        "DEF BEGIN";
        "  ITEM FRESH'VAL U 1;";
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
        "!COMPOOL ('FRESHPOOL');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  FRESH'VAL = FRESH'VAL + 1;";
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
         (Lsp_test_helpers.getenv_int "JOVIAL_TEST_NAV_RECONCILE_HARD_TIMEOUT_S"
            ~default:180))
  in
  let first_max_ms =
    float_of_int
      (max 1
         (Lsp_test_helpers.getenv_int "JOVIAL_TEST_NAV_RECONCILE_FIRST_MAX_MS"
            ~default:2000))
  in
  let settle_timeout_s =
    max 1.0
      (match Sys.getenv_opt "JOVIAL_TEST_NAV_RECONCILE_SETTLE_TIMEOUT_S" with
      | None -> 15.0
      | Some raw -> ( try float_of_string (String.trim raw) with _ -> 15.0))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase : string) =
    if remaining_budget () <= 0.0 then
      failf
        "nav convergence after reconcile test exceeded hard timeout (%.1fs) at \
         %s"
        hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-nav-reconcile" in
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let old_main, old_text, old_pool = create_checkpoint_seed_workspace ~root in
  let old_main_uri = Lsp_test_helpers.lsp_doc_uri_of_path old_main in

  Lsp_test_helpers.with_server
    ~env:[ ("JOVIAL_WORKSPACE_DIAGS_MODE", "off") ]
    ~server_path
    (fun srv ->
      ensure_budget "seed initialize/open";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri
           ~doc_uri:old_main_uri ~doc_text:old_text ~timeout_s:4.0);
      Thread.delay 0.6;
      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:3.0;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  let main_path, main_text, target_path =
    replace_workspace_with_fresh_targets ~root ~old_pool
  in
  let main_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let target_uri = Lsp_test_helpers.lsp_doc_uri_of_path target_path in
  let line, col =
    Lsp_test_helpers.line_col_of_first main_text
      ~needle:"FRESH'VAL = FRESH'VAL + 1;"
  in

  Lsp_test_helpers.with_server
    ~env:
      [
        ("JOVIAL_WORKSPACE_DIAGS_MODE", "off");
        ("JOVIAL_BG_TICK_BUDGET_MS", "12");
      ]
    ~server_path
    (fun srv ->
      ensure_budget "reconcile initialize/open";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri:main_uri
           ~doc_text:main_text ~timeout_s:4.0);

      if not (debug_checkpoint_loaded ~srv ~id:2 ~uri:main_uri ~timeout_s:2.0)
      then failf "expected checkpointLoaded=true on stale-index run";

      let first_resp, first_ms =
        request_definition ~srv ~id:3 ~uri:main_uri ~line ~character:(col + 3)
          ~timeout_s:2.0
      in
      if first_ms > first_max_ms then
        failf "first definition response was too slow: %.1fms (limit %.1fms)"
          first_ms first_max_ms;

      (if not (response_has_result_uri first_resp ~uri:target_uri) then
         let deadline = Unix.gettimeofday () +. settle_timeout_s in
         let rec settle req_id =
           if Unix.gettimeofday () >= deadline then
             failf
               "definition did not converge to %s within %.1fs after reconcile \
                scheduling"
               target_uri settle_timeout_s;
           ensure_budget "wait convergence";
           Thread.delay 0.1;
           let resp, _ =
             request_definition ~srv ~id:req_id ~uri:main_uri ~line
               ~character:(col + 3) ~timeout_s:2.0
           in
           if response_has_result_uri resp ~uri:target_uri then ()
           else settle (req_id + 1)
         in
         settle 4);

      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:3.0;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_nav_converges_after_reconcile_test: ok"
