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

let create_workspace ~(root : string) ~(extra_files : int) : string * string =
  for i = 0 to max 0 (extra_files - 1) do
    let path = Filename.concat root (Printf.sprintf "LIB_%04d.j73" i) in
    let text =
      String.concat "\n"
        [
          "START";
          Printf.sprintf "DEF PROC LIB_%04d RENT;" i;
          "BEGIN";
          "  ITEM V U 1;";
          "  V = 1;";
          "END";
          "TERM";
          "";
        ]
    in
    Lsp_test_helpers.write_text path text
  done;
  let main = Filename.concat root "MAINBIG.j73" in
  let body_lines = 28000 in
  let body = Buffer.create (body_lines * 24) in
  for i = 0 to body_lines - 1 do
    Buffer.add_string body (Printf.sprintf "  ITEM X_%05d U 1;\n" i)
  done;
  Buffer.add_string body "  STOP 0;\n";
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAINBIG RENT;";
        "BEGIN";
        Buffer.contents body;
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main text;
  (main, text)

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

type startup_state = {
  is_ready : bool;
  open_docs_pending_parse : int;
  index_reconcile_pending : bool;
}

let startup_state_of_report (report_fields : (string * Yojson.Safe.t) list) :
    startup_state =
  let startup =
    match assoc_field "startup" report_fields with
    | Some (`Assoc sf) -> sf
    | _ -> failf "debugReport missing startup object"
  in
  let components =
    match assoc_field "components" startup with
    | Some (`Assoc cf) -> cf
    | _ -> []
  in
  let is_ready =
    match assoc_field "isReady" startup with
    | Some v -> ( match bool_of_json v with Some b -> b | None -> false)
    | None -> false
  in
  let open_docs_pending_parse =
    match assoc_field "openDocsPendingParse" components with
    | Some v -> ( match int_of_json v with Some n -> n | None -> 0)
    | None -> 0
  in
  let index_reconcile_pending =
    match assoc_field "indexReconcilePending" components with
    | Some v -> ( match bool_of_json v with Some b -> b | None -> false)
    | None -> false
  in
  { is_ready; open_docs_pending_parse; index_reconcile_pending }

let request_debug_startup_state ~(srv : Lsp_test_helpers.server_proc)
    ~(id : int) ~(uri : string) ~(timeout_s : float) : startup_state =
  command_result_assoc ~srv ~id ~command:"jovial.debugReport"
    ~arguments:[ `String uri ]
    ~timeout_s
  |> startup_state_of_report

let wait_for_notification_opt ~(srv : Lsp_test_helpers.server_proc)
    ~(method_ : string) ~(timeout_s : float) : Yojson.Safe.t option =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then None
    else
      let chunk = min 0.2 remaining in
      try
        let msg = Lsp_test_helpers.wait_for_message srv ~timeout_s:chunk in
        match msg with
        | `Assoc fields -> (
            match
              (assoc_field "method" fields, assoc_field "params" fields)
            with
            | Some (`String m), Some params when m = method_ -> Some params
            | _ -> loop ())
        | _ -> loop ()
      with
      | Failure m when Lsp_test_helpers.is_timeout_failure_message m -> None
      | exn -> raise exn
  in
  loop ()

let parse_workspace_ready_payload (params : Yojson.Safe.t) : startup_state =
  match params with
  | `Assoc fields -> (
      match assoc_field "readiness" fields with
      | Some (`Assoc readiness) ->
          let components =
            match assoc_field "components" readiness with
            | Some (`Assoc cf) -> cf
            | _ -> []
          in
          let is_ready =
            match assoc_field "isReady" readiness with
            | Some v -> (
                match bool_of_json v with Some b -> b | None -> false)
            | None -> false
          in
          let open_docs_pending_parse =
            match assoc_field "openDocsPendingParse" components with
            | Some v -> ( match int_of_json v with Some n -> n | None -> 0)
            | None -> 0
          in
          let index_reconcile_pending =
            match assoc_field "indexReconcilePending" components with
            | Some v -> (
                match bool_of_json v with Some b -> b | None -> false)
            | None -> false
          in
          { is_ready; open_docs_pending_parse; index_reconcile_pending }
      | _ -> failf "workspaceReady payload missing readiness object")
  | _ -> failf "workspaceReady payload was not a JSON object"

let wait_for_workspace_ready ~(srv : Lsp_test_helpers.server_proc)
    ~(timeout_s : float) : startup_state =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then failf "timed out waiting for jovial/workspaceReady";
    match
      wait_for_notification_opt ~srv ~method_:"jovial/workspaceReady"
        ~timeout_s:(min 0.4 remaining)
    with
    | Some params -> parse_workspace_ready_payload params
    | None -> loop ()
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
            "JOVIAL_TEST_READY_NOT_PREMATURE_HARD_TIMEOUT_S" ~default:180))
  in
  let short_guard_timeout_s =
    max 0.2
      (match
         Sys.getenv_opt "JOVIAL_TEST_READY_NOT_PREMATURE_GUARD_TIMEOUT_S"
       with
      | None -> 1.0
      | Some raw -> ( try float_of_string (String.trim raw) with _ -> 1.0))
  in
  let ready_timeout_s =
    max 3.0
      (match
         Sys.getenv_opt "JOVIAL_TEST_READY_NOT_PREMATURE_READY_TIMEOUT_S"
       with
      | None -> 25.0
      | Some raw -> ( try float_of_string (String.trim raw) with _ -> 25.0))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase : string) =
    if remaining_budget () <= 0.0 then
      failf
        "startup ready-not-premature test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-ready-not-premature" in
  let main_path, main_text = create_workspace ~root ~extra_files:120 in
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in

  Lsp_test_helpers.with_server
    ~env:
      [
        ("JOVIAL_WORKSPACE_DIAGS_MODE", "off");
        ("JOVIAL_DIDOPEN_DEFER_PARSE", "true");
        ("JOVIAL_DIDOPEN_DEFER_MIN_DOC_CHARS", "1");
        ("JOVIAL_DIDOPEN_DISABLE_FOREGROUND_TICK", "true");
        ("JOVIAL_BG_TICK_BUDGET_MS", "1");
      ]
    ~server_path
    (fun srv ->
      ensure_budget "initialize/open";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri
           ~doc_text:main_text ~timeout_s:4.0);

      ensure_budget "initial debug report";
      let early_state =
        request_debug_startup_state ~srv ~id:2 ~uri:doc_uri ~timeout_s:2.0
      in
      if early_state.is_ready then
        failf "startup marked ready immediately after deferred didOpen";
      if early_state.open_docs_pending_parse <= 0 then
        failf
          "expected openDocsPendingParse > 0 right after deferred didOpen (got \
           %d)"
          early_state.open_docs_pending_parse;

      ensure_budget "premature-ready guard window";
      let maybe_early_ready =
        wait_for_notification_opt ~srv ~method_:"jovial/workspaceReady"
          ~timeout_s:(min short_guard_timeout_s (max 0.3 (remaining_budget ())))
      in
      (match maybe_early_ready with
      | None -> ()
      | Some params ->
          let st = parse_workspace_ready_payload params in
          if st.open_docs_pending_parse > 0 then
            failf
              "received premature workspaceReady with openDocsPendingParse=%d"
              st.open_docs_pending_parse;
          if st.index_reconcile_pending then
            failf
              "received premature workspaceReady while index reconcile was \
               still pending";
          if not st.is_ready then
            failf "workspaceReady payload reported isReady=false");

      ensure_budget "wait final ready";
      let final_state =
        match maybe_early_ready with
        | Some params -> parse_workspace_ready_payload params
        | None ->
            wait_for_workspace_ready ~srv
              ~timeout_s:(min ready_timeout_s (max 1.0 (remaining_budget ())))
      in
      if not final_state.is_ready then
        failf "final workspaceReady payload reported isReady=false";
      if final_state.open_docs_pending_parse <> 0 then
        failf "final workspaceReady payload has openDocsPendingParse=%d"
          final_state.open_docs_pending_parse;
      if final_state.index_reconcile_pending then
        failf "final workspaceReady payload has indexReconcilePending=true";

      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:3.0;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_startup_ready_not_premature_test: ok"
