let failf = Lsp_test_helpers.failf

let assoc_field (name:string) (fields:(string * Yojson.Safe.t) list) : Yojson.Safe.t option =
  List.assoc_opt name fields

let int_of_json = function
  | `Int n -> Some n
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let bool_of_json = function
  | `Bool b -> Some b
  | _ -> None

type ready_stage =
  | StageDiagHoverReady
  | StageFullyNavigable

let stage_wire_name = function
  | StageDiagHoverReady -> "diagHoverReady"
  | StageFullyNavigable -> "fullyNavigable"

let wait_for_workspace_ready_stage
    ~(srv:Lsp_test_helpers.server_proc)
    ~(stage:ready_stage)
    ~(timeout_s:float)
  : Yojson.Safe.t =
  let wanted = stage_wire_name stage in
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then
      failf "timed out waiting for jovial/workspaceReady stage=%s notification" wanted;
    let chunk = min 0.25 remaining in
    if chunk <= 0.0 then
      failf "timed out waiting for jovial/workspaceReady stage=%s notification" wanted;
    try
      let msg = Lsp_test_helpers.wait_for_message srv ~timeout_s:chunk in
      match msg with
      | `Assoc fields -> (
          match assoc_field "method" fields, assoc_field "params" fields with
          | Some (`String "jovial/workspaceReady"), Some (`Assoc params) -> (
              match assoc_field "stage" params with
              | Some (`String got) when got = wanted -> `Assoc params
              | _ -> loop ())
          | _ -> loop ())
      | _ ->
          loop ()
    with
    | Failure m when Lsp_test_helpers.is_timeout_failure_message m ->
        loop ()
    | exn ->
        raise exn
  in
  loop ()

let assert_stage_ready_payload ~(stage:ready_stage) (params:Yojson.Safe.t) : unit =
  let stage_key = stage_wire_name stage in
  match params with
  | `Assoc fields -> (
      match assoc_field "readiness" fields with
      | Some (`Assoc readiness_fields) ->
          let stages =
            match assoc_field "stages" readiness_fields with
            | Some (`Assoc xs) -> xs
            | _ -> failf "workspaceReady payload missing readiness.stages"
          in
          let stage_obj =
            match assoc_field stage_key stages with
            | Some (`Assoc xs) -> xs
            | _ -> failf "workspaceReady payload missing readiness.stages.%s" stage_key
          in
          let elapsed_ms =
            match assoc_field "elapsedMs" stage_obj with
            | Some v -> (match int_of_json v with Some n -> n | None -> -1)
            | None -> -1
          in
          if elapsed_ms < 0 then
            failf "workspaceReady payload missing readiness.stages.%s.elapsedMs" stage_key;
          let is_ready =
            match assoc_field "isReady" stage_obj with
            | Some v -> (match bool_of_json v with Some b -> b | None -> false)
            | None -> false
          in
          if not is_ready then
            failf "workspaceReady payload reported readiness.stages.%s.isReady=false" stage_key;
          (match stage with
           | StageFullyNavigable ->
               let top_ready =
                 match assoc_field "isReady" readiness_fields with
                 | Some v -> bool_of_json v = Some true
                 | None -> false
               in
               if not top_ready then
                 failf "workspaceReady fullyNavigable payload reported readiness.isReady=false"
           | StageDiagHoverReady -> ())
      | _ ->
          failf "workspaceReady payload missing readiness object")
  | _ ->
      failf "workspaceReady payload is not an object"

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
            "JOVIAL_TEST_WORKSPACE_READY_HARD_TIMEOUT_S"
            ~default:120))
  in
  let notif_timeout_s =
    max 0.5
      (match Sys.getenv_opt "JOVIAL_TEST_WORKSPACE_READY_TIMEOUT_S" with
       | None -> 8.0
       | Some raw ->
           (try float_of_string (String.trim raw) with _ -> 8.0))
  in
  let started = Unix.gettimeofday () in
  let ensure_budget phase =
    if Unix.gettimeofday () -. started > hard_timeout_s then
      failf
        "workspace-ready notification test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s
        phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-workspace-ready" in
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [ "START"; "DEF PROC MAIN RENT;"; "BEGIN"; "  STOP 0;"; "END"; "TERM"; "" ]
  in
  Lsp_test_helpers.write_text main_path main_text;
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in

  Lsp_test_helpers.with_server
    ~env:
      [
        "JOVIAL_DIDOPEN_DEFER_PARSE", "false";
        "JOVIAL_WORKSPACE_DIAGS_MODE", "errors";
        "JOVIAL_STARTUP_TARGET_MS", "15000";
      ]
    ~server_path
    (fun srv ->
      ensure_budget "initialize/open";
      ignore
        (Lsp_test_helpers.initialize_and_open
           srv
           ~root_uri
           ~doc_uri
           ~doc_text:main_text
           ~timeout_s:4.0);
      ensure_budget "wait workspaceReady";
      let diag_params =
        wait_for_workspace_ready_stage
          ~srv
          ~stage:StageDiagHoverReady
          ~timeout_s:notif_timeout_s
      in
      assert_stage_ready_payload ~stage:StageDiagHoverReady diag_params;
      let full_params =
        wait_for_workspace_ready_stage
          ~srv
          ~stage:StageFullyNavigable
          ~timeout_s:notif_timeout_s
      in
      assert_stage_ready_payload ~stage:StageFullyNavigable full_params;
      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:3.0;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));
  print_endline "lsp_workspace_ready_notification_test: ok"
