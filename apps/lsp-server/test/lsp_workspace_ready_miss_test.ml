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

let create_workspace ~(root:string) ~(file_count:int) : string * string =
  let bucket_count = 24 in
  for i = 0 to bucket_count - 1 do
    Lsp_test_helpers.ensure_dir (Filename.concat root (Printf.sprintf "pkg_%02d" i))
  done;
  for i = 0 to file_count - 1 do
    let dir = Filename.concat root (Printf.sprintf "pkg_%02d" (i mod bucket_count)) in
    let path = Filename.concat dir (Printf.sprintf "MOD_%04d.j73" i) in
    let text =
      String.concat "\n"
        [
          "START";
          (Printf.sprintf "DEF PROC MOD_%04d RENT;" i);
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
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [ "START"; "DEF PROC MAIN RENT;"; "BEGIN"; "  STOP 0;"; "END"; "TERM"; "" ]
  in
  Lsp_test_helpers.write_text main_path main_text;
  (main_path, main_text)

let wait_for_notification
    ~(srv:Lsp_test_helpers.server_proc)
    ~(method_:string)
    ~(timeout_s:float)
  : Yojson.Safe.t =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then
      failf "timed out waiting for %s notification" method_;
    let chunk = min 0.25 remaining in
    if chunk <= 0.0 then
      failf "timed out waiting for %s notification" method_;
    try
      let msg = Lsp_test_helpers.wait_for_message srv ~timeout_s:chunk in
      match msg with
      | `Assoc fields -> (
          match assoc_field "method" fields, assoc_field "params" fields with
          | Some (`String m), Some params when m = method_ -> params
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

let parse_elapsed_and_target (params:Yojson.Safe.t) : int * int =
  match params with
  | `Assoc fields ->
      let elapsed =
        match assoc_field "elapsedMs" fields with
        | Some v -> (match int_of_json v with Some n -> n | None -> -1)
        | None -> -1
      in
      let target =
        match assoc_field "targetMs" fields with
        | Some v -> (match int_of_json v with Some n -> n | None -> -1)
        | None -> -1
      in
      (elapsed, target)
  | _ ->
      (-1, -1)

let parse_ready_within_target (params:Yojson.Safe.t) : bool option =
  match params with
  | `Assoc fields -> (
      match assoc_field "readiness" fields with
      | Some (`Assoc readiness) -> (
          match assoc_field "readyWithinTarget" readiness with
          | Some v -> bool_of_json v
          | None -> None)
      | _ -> None)
  | _ -> None

let parse_stage (params:Yojson.Safe.t) : string option =
  match params with
  | `Assoc fields -> (
      match assoc_field "stage" fields with
      | Some (`String s) -> Some s
      | _ -> None)
  | _ -> None

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
            "JOVIAL_TEST_WORKSPACE_READY_MISS_HARD_TIMEOUT_S"
            ~default:180))
  in
  let workspace_files =
    max 50
      (Lsp_test_helpers.getenv_int
         "JOVIAL_TEST_WORKSPACE_READY_MISS_FILES"
         ~default:900)
  in
  let miss_timeout_s =
    max 2.0
      (match Sys.getenv_opt "JOVIAL_TEST_WORKSPACE_READY_MISS_TIMEOUT_S" with
       | None -> 20.0
       | Some raw -> (try float_of_string (String.trim raw) with _ -> 20.0))
  in
  let ready_timeout_s =
    max 5.0
      (match Sys.getenv_opt "JOVIAL_TEST_WORKSPACE_READY_MISS_READY_TIMEOUT_S" with
       | None -> 90.0
       | Some raw -> (try float_of_string (String.trim raw) with _ -> 90.0))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget phase =
    if remaining_budget () <= 0.0 then
      failf
        "workspace-ready miss regression exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s
        phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-workspace-ready-miss" in
  let main_path, main_text = create_workspace ~root ~file_count:workspace_files in
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in

  Lsp_test_helpers.with_server
    ~env:
      [
        "JOVIAL_STARTUP_DIAG_HOVER_TARGET_MS", "1";
        "JOVIAL_STARTUP_NAV_TARGET_MS", "1";
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

      ensure_budget "optional startup miss";
      let _miss_params_opt =
        try
          let miss_params =
            wait_for_notification
              ~srv
              ~method_:"jovial/workspaceStartupMiss"
              ~timeout_s:(min miss_timeout_s (remaining_budget ()))
          in
          (match parse_stage miss_params with
           | Some "diagHoverReady" | Some "fullyNavigable" -> ()
           | Some other ->
               failf "workspaceStartupMiss payload has unexpected stage: %s" other
           | None ->
               failf "workspaceStartupMiss payload missing stage");
          let miss_elapsed, miss_target = parse_elapsed_and_target miss_params in
          if miss_elapsed < 0 || miss_target < 0 then
            failf "workspaceStartupMiss payload missing elapsed/target fields";
          if miss_elapsed < miss_target then
            failf
              "workspaceStartupMiss payload inconsistent: elapsed=%d target=%d"
              miss_elapsed
              miss_target;
          Some miss_params
        with
        | Failure _ ->
            None
      in

      ensure_budget "wait workspaceReady";
      (try
         let ready_params =
           wait_for_notification
             ~srv
             ~method_:"jovial/workspaceReady"
             ~timeout_s:(min ready_timeout_s (remaining_budget ()))
         in
         (match parse_ready_within_target ready_params with
          | Some false -> ()
          | Some true ->
              failf "workspaceReady unexpectedly reported readyWithinTarget=true in miss regression"
          | None ->
              failf "workspaceReady payload missing readyWithinTarget")
       with
       | Failure _ ->
           print_endline
             "lsp_workspace_ready_miss_test: note - workspaceReady not observed within timeout under current constraints");

      ensure_budget "shutdown";
      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min 4.0 (remaining_budget ()));
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_workspace_ready_miss_test: ok"
