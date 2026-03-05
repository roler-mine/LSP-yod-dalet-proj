let failf = Lsp_test_helpers.failf

let contains_substring ~(haystack:string) ~(needle:string) : bool =
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

let is_false_control_keyword_undefined (msg:string) : bool =
  contains_substring ~haystack:msg ~needle:"Undefined procedure \"EXIT\""
  || contains_substring ~haystack:msg ~needle:"Undefined procedure \"ABORT\""
  || contains_substring ~haystack:msg ~needle:"Undefined procedure \"STOP\""

let has_source_ext (path:string) : bool =
  let lower = String.lowercase_ascii path in
  Filename.check_suffix lower ".jov" || Filename.check_suffix lower ".j73"

let is_dir (path:string) : bool =
  try (Unix.stat path).st_kind = Unix.S_DIR with _ -> false

let rec collect_source_files (root:string) : string list =
  if not (is_dir root) then []
  else
    let entries =
      try Sys.readdir root |> Array.to_list with _ -> []
    in
    entries
    |> List.sort String.compare
    |> List.concat_map (fun name ->
         let path = Filename.concat root name in
         if is_dir path then collect_source_files path
         else if has_source_ext path then [path]
         else [])

let read_text (path:string) : string =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let file_size_bytes (path:string) : int option =
  try Some (Unix.stat path).st_size with _ -> None

let getenv_bool (name:string) ~(default:bool) : bool =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw ->
      (match String.lowercase_ascii (String.trim raw) with
       | "1" | "true" | "yes" | "on" -> true
       | "0" | "false" | "no" | "off" -> false
       | _ -> default)

let wait_uri_diags_or_fail
    ~(srv:Lsp_test_helpers.server_proc)
    ~(uri:string)
    ~(timeout_s:float)
    ~(phase:string)
  : Yojson.Safe.t list =
  let t0 = Unix.gettimeofday () in
  match Lsp_test_helpers.wait_for_publish_diagnostics_for_uri ~srv ~target_uri:uri ~timeout_s with
  | None ->
      failf
        "did not receive publishDiagnostics for %s within %.2fs at %s"
        uri
        timeout_s
        phase
  | Some diags ->
      let elapsed = Unix.gettimeofday () -. t0 in
      if elapsed > timeout_s +. 0.001 then
        failf
          "publishDiagnostics for %s exceeded %.2fs at %s (got %.3fs)"
          uri
          timeout_s
          phase
          elapsed;
      diags

let did_open_params ~(uri:string) ~(version:int) ~(text:string) : Yojson.Safe.t =
  `Assoc [
    "textDocument",
    `Assoc [
      "uri", `String uri;
      "languageId", `String "jovial";
      "version", `Int version;
      "text", `String text;
    ];
  ]

let debug_report_assoc
    ~(srv:Lsp_test_helpers.server_proc)
    ~(id:int)
    ~(uri:string)
    ~(timeout_s:float)
  : (string * Yojson.Safe.t) list =
  let params =
    `Assoc
      [
        "command", `String "jovial.debugReport";
        "arguments", `List [ `String uri ];
      ]
  in
  let resp, _ =
    Lsp_test_helpers.request_timed
      srv
      ~id
      ~method_:"workspace/executeCommand"
      ~params
      ~timeout_s
  in
  match resp with
  | `Assoc fields -> (
      match List.assoc_opt "result" fields with
      | Some (`Assoc result_fields) -> result_fields
      | Some other ->
          failf
            "debugReport returned non-object result: %s"
            (Yojson.Safe.to_string other)
      | None -> failf "debugReport response missing result")
  | _ -> failf "debugReport response was not an object"

let startup_open_docs_pending_parse_of_report
    (report_fields:(string * Yojson.Safe.t) list)
  : int =
  match List.assoc_opt "startup" report_fields with
  | Some (`Assoc startup_fields) -> (
      match List.assoc_opt "components" startup_fields with
      | Some (`Assoc components) -> (
          match List.assoc_opt "openDocsPendingParse" components with
          | Some (`Int n) -> n
          | Some (`Intlit s) -> (try int_of_string s with _ -> 0)
          | _ -> 0)
      | _ -> 0)
  | _ -> 0

let startup_xmodule_diag_ready_of_report
    (report_fields:(string * Yojson.Safe.t) list)
  : bool =
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
    (report_fields:(string * Yojson.Safe.t) list)
  : bool =
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

let assert_no_false_control_keyword_diag ~(path:string) (diags:Yojson.Safe.t list) : unit =
  let offending =
    diags
    |> List.filter_map Lsp_test_helpers.diag_message
    |> List.filter is_false_control_keyword_undefined
  in
  if offending <> [] then
    failf
      "false control-keyword diagnostics in %s: %s"
      path
      (String.concat " | " offending)

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let ws_path_opt =
    match Sys.getenv_opt "JOVIAL_TEST_EXTERNAL_WS_PATH" with
    | None -> None
    | Some raw ->
        let trimmed = String.trim raw in
        if trimmed = "" then None else Some trimmed
  in
  match ws_path_opt with
  | None ->
      print_endline
        "lsp_external_workspace_open_diag_test: skipped (set JOVIAL_TEST_EXTERNAL_WS_PATH to run)";
      exit 0
  | Some ws_path ->
      if not (is_dir ws_path) then
        failf "external workspace path is not a directory: %s" ws_path;
      let open_timeout_s =
        max 0.2
          (match Sys.getenv_opt "JOVIAL_TEST_EXTERNAL_WS_TIMEOUT_S" with
           | None -> 2.0
           | Some raw ->
               (try float_of_string (String.trim raw) with _ -> 2.0))
      in
      let open_timeout_large_s =
        max open_timeout_s
          (match Sys.getenv_opt "JOVIAL_TEST_EXTERNAL_WS_TIMEOUT_LARGE_S" with
           | None -> 10.0
           | Some raw ->
               (try float_of_string (String.trim raw) with _ -> 10.0))
      in
      let large_file_threshold_bytes =
        max 1
          (Lsp_test_helpers.getenv_int
             "JOVIAL_TEST_EXTERNAL_WS_LARGE_FILE_THRESHOLD_BYTES"
             ~default:(1024 * 1024))
      in
      let hard_timeout_s =
        float_of_int
          (max 1
             (Lsp_test_helpers.getenv_int
                "JOVIAL_TEST_EXTERNAL_WS_HARD_TIMEOUT_S"
                ~default:900))
      in
      let ready_timeout_s =
        max 2.0
          (match Sys.getenv_opt "JOVIAL_TEST_EXTERNAL_WS_READY_TIMEOUT_S" with
           | None -> 45.0
           | Some raw ->
               (try float_of_string (String.trim raw) with _ -> 45.0))
      in
      let require_ready_convergence =
        getenv_bool "JOVIAL_TEST_EXTERNAL_WS_REQUIRE_READY_CONVERGENCE" ~default:false
      in
      let started = Unix.gettimeofday () in
      let ensure_budget (phase:string) : unit =
        if Unix.gettimeofday () -. started > hard_timeout_s then
          failf
            "external workspace open-diagnostics test exceeded hard timeout (%.1fs) at %s"
            hard_timeout_s
            phase
      in
      let files = collect_source_files ws_path in
      if files = [] then
        failf "no .jov/.j73 files found in external workspace path: %s" ws_path;
      let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path ws_path in
      let first = List.hd files in
      let first_uri = Lsp_test_helpers.lsp_doc_uri_of_path first in
      let first_text = read_text first in
      let timeout_for_path (path:string) : float =
        match file_size_bytes path with
        | Some n when n > large_file_threshold_bytes -> open_timeout_large_s
        | _ -> open_timeout_s
      in
      Lsp_test_helpers.with_server
        ~env:[ "JOVIAL_WORKSPACE_DIAGS_MODE", "off" ]
        ~server_path
        (fun srv ->
        ensure_budget "initialize+first open";
        ignore
          (Lsp_test_helpers.initialize_and_open
             srv
             ~root_uri
             ~doc_uri:first_uri
             ~doc_text:first_text
             ~timeout_s:(timeout_for_path first));
        let first_diags =
          wait_uri_diags_or_fail ~srv ~uri:first_uri ~timeout_s:(timeout_for_path first) ~phase:first
        in
        assert_no_false_control_keyword_diag ~path:first first_diags;

        files
        |> List.tl
        |> List.iteri (fun idx path ->
             ensure_budget (Printf.sprintf "didOpen %s" path);
             let uri = Lsp_test_helpers.lsp_doc_uri_of_path path in
             let text = read_text path in
             Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen"
               ~params:(did_open_params ~uri ~version:(idx + 2) ~text);
             let diags =
               wait_uri_diags_or_fail
                 ~srv
                 ~uri
                 ~timeout_s:(timeout_for_path path)
                 ~phase:(Printf.sprintf "didOpen %s" path)
             in
             assert_no_false_control_keyword_diag ~path diags);

        ensure_budget "wait diagnostics stage ready";
        let ready_deadline =
          Unix.gettimeofday ()
          +. (min ready_timeout_s
                (max 2.0 (hard_timeout_s -. (Unix.gettimeofday () -. started))))
        in
        let rec wait_ready req_id =
          if Unix.gettimeofday () >= ready_deadline then
            if require_ready_convergence then
              failf "timed out waiting for diagnostics-stage convergence in debugReport"
            else
              (
                prerr_endline
                  "lsp_external_workspace_open_diag_test: note - diagnostics-stage convergence was not reached within timeout";
                debug_report_assoc ~srv ~id:req_id ~uri:first_uri ~timeout_s:4.0
              )
          else
            let report =
              debug_report_assoc ~srv ~id:req_id ~uri:first_uri ~timeout_s:4.0
            in
            let pending = startup_open_docs_pending_parse_of_report report in
            let xmodule_ready = startup_xmodule_diag_ready_of_report report in
            let diag_ready = startup_diag_hover_ready_of_report report in
            if pending = 0 && xmodule_ready && diag_ready then
              report
            else (
              Lsp_test_helpers.sleep_seconds 0.2;
              wait_ready (req_id + 1))
        in
        let report = wait_ready 9801 in

        ensure_budget "debug report convergence check";
        if require_ready_convergence then (
          let pending = startup_open_docs_pending_parse_of_report report in
          if pending <> 0 then
            failf
              "expected openDocsPendingParse=0 after diagHoverReady, got %d"
              pending;
          if not (startup_xmodule_diag_ready_of_report report) then
            failf
              "expected startup.components.xmoduleDiagReady=true after diagHoverReady"
        );

        let shutdown_timeout_s = max 10.0 open_timeout_large_s in
        Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:shutdown_timeout_s;
        Lsp_test_helpers.close_stdin srv;
        ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:6.0));
      print_endline
        (Printf.sprintf
           "lsp_external_workspace_open_diag_test: ok (%d files)"
           (List.length files))
