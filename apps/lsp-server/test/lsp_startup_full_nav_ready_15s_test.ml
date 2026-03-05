let failf = Lsp_test_helpers.failf

let int_of_json = function
  | `Int n -> Some n
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let bool_of_json = function
  | `Bool b -> Some b
  | _ -> None

let assoc_field (name:string) (fields:(string * Yojson.Safe.t) list) : Yojson.Safe.t option =
  List.assoc_opt name fields

type ready_stage =
  | StageDiagHoverReady
  | StageFullyNavigable

let ready_stage_wire_name = function
  | StageDiagHoverReady -> "diagHoverReady"
  | StageFullyNavigable -> "fullyNavigable"

let create_workspace ~(root:string) ~(file_count:int) : string * string =
  let bucket_count = 16 in
  for i = 0 to bucket_count - 1 do
    Lsp_test_helpers.ensure_dir (Filename.concat root (Printf.sprintf "pkg_%02d" i))
  done;
  for i = 0 to file_count - 1 do
    let dir = Filename.concat root (Printf.sprintf "pkg_%02d" (i mod bucket_count)) in
    let name = Printf.sprintf "LIB_%04d" i in
    let path = Filename.concat dir (name ^ ".j73") in
    let text =
      String.concat "\n"
        [
          "START";
          (Printf.sprintf "DEF PROC %s RENT;" name);
          "  ITEM INVAL U 1;";
          "  ITEM OUTVAL U 1;";
          "BEGIN";
          "  OUTVAL = INVAL + 1;";
          "END";
          "TERM";
          "";
        ]
    in
    Lsp_test_helpers.write_text path text
  done;
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [
        "START";
        "REF PROC LIB_0000;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM TARGET U 1;";
        "  TARGET = 1;";
        "  LIB_0000(TARGET);";
        "  TAR";
        "  STOP 0;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main_path main_text;
  (main_path, main_text)

let wait_for_workspace_ready_stage
    ~(srv:Lsp_test_helpers.server_proc)
    ~(stage:ready_stage)
    ~(timeout_s:float)
    ~(on_tick:unit -> unit)
  : Yojson.Safe.t =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let wanted = ready_stage_wire_name stage in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then
      failf "timed out waiting for jovial/workspaceReady stage=%s" wanted;
    let chunk = min 0.25 remaining in
    if chunk <= 0.0 then
      failf "timed out waiting for jovial/workspaceReady stage=%s" wanted;
    try
      let msg = Lsp_test_helpers.wait_for_message srv ~timeout_s:chunk in
      match msg with
      | `Assoc fields -> (
          match assoc_field "method" fields, assoc_field "params" fields with
          | Some (`String "jovial/workspaceReady"), Some (`Assoc params) -> (
              match assoc_field "stage" params with
              | Some (`String got) when got = wanted ->
                  `Assoc params
              | _ ->
                  loop ())
          | _ -> loop ())
      | _ ->
          loop ()
    with
    | Failure m when Lsp_test_helpers.is_timeout_failure_message m ->
        on_tick ();
        loop ()
    | exn ->
        raise exn
  in
  loop ()

let parse_stage_readiness_payload
    ~(stage:ready_stage)
    (params:Yojson.Safe.t)
  : int * int * bool * bool =
  let stage_name = ready_stage_wire_name stage in
  match params with
  | `Assoc fields -> (
      match assoc_field "readiness" fields with
      | Some (`Assoc readiness) ->
          let stages =
            match assoc_field "stages" readiness with
            | Some (`Assoc xs) -> xs
            | _ -> failf "workspaceReady payload missing readiness.stages"
          in
          let stage_obj =
            match assoc_field stage_name stages with
            | Some (`Assoc xs) -> xs
            | _ ->
                failf "workspaceReady payload missing readiness.stages.%s" stage_name
          in
          let elapsed =
            match assoc_field "elapsedMs" stage_obj with
            | Some v -> (match int_of_json v with Some n -> n | None -> -1)
            | None -> -1
          in
          let target =
            match assoc_field "targetMs" stage_obj with
            | Some v -> (match int_of_json v with Some n -> n | None -> -1)
            | None -> -1
          in
          let ready_within_target =
            match assoc_field "readyWithinTarget" stage_obj with
            | Some v -> (match bool_of_json v with Some b -> b | None -> false)
            | None -> false
          in
          let nav_prereqs_ready =
            match assoc_field "components" readiness with
            | Some (`Assoc components) -> (
                match assoc_field "navPrereqsReady" components with
                | Some v -> (match bool_of_json v with Some b -> b | None -> false)
                | None -> false)
            | _ -> false
          in
          (elapsed, target, ready_within_target, nav_prereqs_ready)
      | _ ->
          failf "workspaceReady payload missing readiness object")
  | _ ->
      failf "workspaceReady payload is not a JSON object"

let result_field (resp:Yojson.Safe.t) : Yojson.Safe.t option =
  match resp with
  | `Assoc fields -> List.assoc_opt "result" fields
  | _ -> None

let ensure_no_error ~(method_:string) (resp:Yojson.Safe.t) : unit =
  match resp with
  | `Assoc fields -> (
      match List.assoc_opt "error" fields with
      | None -> ()
      | Some err ->
          failf "request %s returned error: %s" method_ (Yojson.Safe.to_string err))
  | _ ->
      failf "request %s response is not a JSON object" method_

let expect_non_null_result ~(method_:string) (resp:Yojson.Safe.t) : unit =
  match result_field resp with
  | Some `Null ->
      failf "request %s returned null after startup-ready" method_
  | Some _ ->
      ()
  | None ->
      failf "request %s response missing result" method_

let completion_params ~(uri:string) ~(line:int) ~(character:int) : Yojson.Safe.t =
  `Assoc
    [
      "textDocument", `Assoc [ "uri", `String uri ];
      "position", `Assoc [ "line", `Int line; "character", `Int character ];
    ]

let hover_params ~(uri:string) ~(line:int) ~(character:int) : Yojson.Safe.t =
  `Assoc
    [
      "textDocument", `Assoc [ "uri", `String uri ];
      "position", `Assoc [ "line", `Int line; "character", `Int character ];
    ]

let references_params ~(uri:string) ~(line:int) ~(character:int) : Yojson.Safe.t =
  `Assoc
    [
      "textDocument", `Assoc [ "uri", `String uri ];
      "position", `Assoc [ "line", `Int line; "character", `Int character ];
      "context", `Assoc [ "includeDeclaration", `Bool true ];
    ]

let prepare_rename_params ~(uri:string) ~(line:int) ~(character:int) : Yojson.Safe.t =
  `Assoc
    [
      "textDocument", `Assoc [ "uri", `String uri ];
      "position", `Assoc [ "line", `Int line; "character", `Int character ];
    ]

let rename_params ~(uri:string) ~(line:int) ~(character:int) ~(new_name:string) : Yojson.Safe.t =
  `Assoc
    [
      "textDocument", `Assoc [ "uri", `String uri ];
      "position", `Assoc [ "line", `Int line; "character", `Int character ];
      "newName", `String new_name;
    ]

let run_nav_request
    ~(srv:Lsp_test_helpers.server_proc)
    ~(id:int)
    ~(method_:string)
    ~(params:Yojson.Safe.t)
    ~(timeout_s:float)
  : unit =
  let resp, elapsed_ms =
    Lsp_test_helpers.request_timed srv ~id ~method_ ~params ~timeout_s
  in
  ensure_no_error ~method_ resp;
  expect_non_null_result ~method_ resp;
  if elapsed_ms > 2000.0 then
    failf
      "%s too slow post-ready: %.1fms (limit 2000ms)"
      method_
      elapsed_ms

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
            "JOVIAL_TEST_STARTUP_FULL_NAV_HARD_TIMEOUT_S"
            ~default:180))
  in
  let workspace_files =
    max 1
      (Lsp_test_helpers.getenv_int
         "JOVIAL_TEST_STARTUP_FULL_NAV_FILES"
         ~default:120)
  in
  let diag_ready_timeout_s =
    max 5.0
      (match Sys.getenv_opt "JOVIAL_TEST_STARTUP_FULL_NAV_DIAG_TIMEOUT_S" with
       | None -> 20.0
       | Some raw -> (try float_of_string (String.trim raw) with _ -> 20.0))
  in
  let nav_ready_timeout_s =
    max diag_ready_timeout_s
      (match Sys.getenv_opt "JOVIAL_TEST_STARTUP_FULL_NAV_READY_TIMEOUT_S" with
       | None -> 35.0
       | Some raw -> (try float_of_string (String.trim raw) with _ -> 35.0))
  in
  let request_timeout_s = 2.0 in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) =
    if remaining_budget () <= 0.0 then
      failf
        "startup full-nav ready test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s
        phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-startup-full-nav-ready" in
  let main_path, main_text = create_workspace ~root ~file_count:workspace_files in
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in

  let lib_line, lib_col =
    Lsp_test_helpers.line_col_of_first main_text ~needle:"LIB_0000(TARGET);"
  in
  let target_line, target_col =
    Lsp_test_helpers.line_col_of_first main_text ~needle:"TARGET = 1;"
  in
  let completion_line, completion_col =
    Lsp_test_helpers.line_col_of_first main_text ~needle:"TAR"
  in

  Lsp_test_helpers.with_server
    ~env:
      [
        "JOVIAL_STARTUP_DIAG_HOVER_TARGET_MS", "15000";
        "JOVIAL_STARTUP_NAV_TARGET_MS", "30000";
        "JOVIAL_STARTUP_AGGRESSIVE_WINDOW_MS", "3000";
        "JOVIAL_STARTUP_AGGRESSIVE_BG_BUDGET_MS", "300";
        "JOVIAL_WORKSPACE_DIAGS_MODE", "off";
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
           ~timeout_s:(min 4.0 (remaining_budget ())));
      let req_id = ref 2 in
      let drive_startup_traffic () =
        let id1 = !req_id in
        incr req_id;
        (try
           ignore
             (Lsp_test_helpers.request_timed
                srv
                ~id:id1
                ~method_:"textDocument/hover"
                ~params:(hover_params ~uri:doc_uri ~line:lib_line ~character:(lib_col + 1))
                ~timeout_s:1.0)
         with _ -> ());
        let id2 = !req_id in
        incr req_id;
        (try
           ignore
             (Lsp_test_helpers.request_timed
                srv
                ~id:id2
                ~method_:"textDocument/definition"
                ~params:
                  (Lsp_test_helpers.definition_request_params
                     ~uri:doc_uri
                     ~line:lib_line
                     ~character:(lib_col + 1))
                ~timeout_s:1.0)
         with _ -> ())
      in
      ensure_budget "wait diagHoverReady";
      let diag_params =
        wait_for_workspace_ready_stage
          ~srv
          ~stage:StageDiagHoverReady
          ~timeout_s:(min diag_ready_timeout_s (remaining_budget ()))
          ~on_tick:drive_startup_traffic
      in
      let diag_elapsed_ms, diag_target_ms, diag_ready_within_target, _ =
        parse_stage_readiness_payload ~stage:StageDiagHoverReady diag_params
      in
      if diag_elapsed_ms < 0 || diag_target_ms < 0 then
        failf "diagHoverReady payload missing elapsed/target fields";
      if diag_elapsed_ms > 15000 then
        failf "diagHoverReady exceeded 15s target: elapsed=%dms" diag_elapsed_ms;
      if not diag_ready_within_target then
        failf "diagHoverReady reported readyWithinTarget=false";

      ensure_budget "wait fullyNavigable";
      let full_params =
        wait_for_workspace_ready_stage
          ~srv
          ~stage:StageFullyNavigable
          ~timeout_s:(min nav_ready_timeout_s (remaining_budget ()))
          ~on_tick:drive_startup_traffic
      in
      let elapsed_ms, target_ms, ready_within_target, nav_prereqs_ready =
        parse_stage_readiness_payload ~stage:StageFullyNavigable full_params
      in
      if elapsed_ms < 0 || target_ms < 0 then
        failf "fullyNavigable payload missing elapsed/target fields";
      if elapsed_ms > 30000 then
        failf "fullyNavigable exceeded 30s target: elapsed=%dms" elapsed_ms;
      if not ready_within_target then
        failf "fullyNavigable reported readyWithinTarget=false";
      if not nav_prereqs_ready then
        failf "fullyNavigable reported navPrereqsReady=false";

      ensure_budget "post-ready nav";
      run_nav_request
        ~srv
        ~id:(!req_id)
        ~method_:"textDocument/definition"
        ~params:
          (Lsp_test_helpers.definition_request_params
             ~uri:doc_uri
             ~line:lib_line
             ~character:(lib_col + 1))
        ~timeout_s:request_timeout_s;
      incr req_id;
      run_nav_request
        ~srv
        ~id:(!req_id)
        ~method_:"textDocument/implementation"
        ~params:
          (Lsp_test_helpers.definition_request_params
             ~uri:doc_uri
             ~line:lib_line
             ~character:(lib_col + 1))
        ~timeout_s:request_timeout_s;
      incr req_id;
      run_nav_request
        ~srv
        ~id:(!req_id)
        ~method_:"textDocument/references"
        ~params:(references_params ~uri:doc_uri ~line:lib_line ~character:(lib_col + 1))
        ~timeout_s:request_timeout_s;
      incr req_id;
      run_nav_request
        ~srv
        ~id:(!req_id)
        ~method_:"textDocument/hover"
        ~params:(hover_params ~uri:doc_uri ~line:lib_line ~character:(lib_col + 1))
        ~timeout_s:request_timeout_s;
      incr req_id;
      run_nav_request
        ~srv
        ~id:(!req_id)
        ~method_:"textDocument/completion"
        ~params:(completion_params ~uri:doc_uri ~line:completion_line ~character:(completion_col + 3))
        ~timeout_s:request_timeout_s;
      incr req_id;
      run_nav_request
        ~srv
        ~id:(!req_id)
        ~method_:"textDocument/prepareRename"
        ~params:(prepare_rename_params ~uri:doc_uri ~line:target_line ~character:(target_col + 1))
        ~timeout_s:request_timeout_s;
      incr req_id;
      run_nav_request
        ~srv
        ~id:(!req_id)
        ~method_:"textDocument/rename"
        ~params:
          (rename_params
             ~uri:doc_uri
             ~line:target_line
             ~character:(target_col + 1)
             ~new_name:"TARGET_RENAMED")
        ~timeout_s:request_timeout_s;

      ensure_budget "shutdown";
      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min 3.0 (remaining_budget ()));
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));
  print_endline "lsp_startup_full_nav_ready_15s_test: ok"
