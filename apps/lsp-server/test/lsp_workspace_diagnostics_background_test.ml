let failf = Lsp_test_helpers.failf

type diag_counts = {
  errors : int;
  warnings : int;
}

let empty_counts = { errors = 0; warnings = 0 }

let counts_of_severities (severities:int list) : diag_counts =
  List.fold_left (fun acc sev ->
    if sev = 1 then { acc with errors = acc.errors + 1 }
    else if sev = 2 then { acc with warnings = acc.warnings + 1 }
    else acc
  ) empty_counts severities

let parse_publish_diagnostics (msg:Yojson.Safe.t) : (string * int list) option =
  match msg with
  | `Assoc fields ->
      (match List.assoc_opt "method" fields, List.assoc_opt "params" fields with
       | Some (`String "textDocument/publishDiagnostics"), Some (`Assoc params) ->
           (match List.assoc_opt "uri" params, List.assoc_opt "diagnostics" params with
            | Some (`String uri), Some (`List ds) ->
                let severities =
                  ds
                  |> List.filter_map (function
                       | `Assoc dfields ->
                           (match List.assoc_opt "severity" dfields with
                            | Some (`Int n) -> Some n
                            | Some (`Intlit s) ->
                                (try Some (int_of_string s) with _ -> Some 1)
                            | _ -> Some 1)
                       | _ -> None)
                in
                Some (uri, severities)
            | _ -> None)
       | _ -> None)
  | _ ->
      None

let is_timeout_failure (msg:string) : bool =
  let needle = "timed out waiting for LSP message" in
  let n = String.length msg in
  let m = String.length needle in
  let rec has_at i =
    if i + m > n then false
    else if String.sub msg i m = needle then true
    else has_at (i + 1)
  in
  has_at 0

let wait_for_uri_diagnostics
    ~(srv:Lsp_test_helpers.server_proc)
    ~(target_uri:string)
    ~(timeout_s:float)
  : int list option =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop (last:int list option) =
    let now = Unix.gettimeofday () in
    if now >= deadline then last
    else
      let chunk = min 0.25 (deadline -. now) in
      if chunk <= 0.0 then last
      else
        try
          let msg = Lsp_test_helpers.wait_for_message srv ~timeout_s:chunk in
          (match parse_publish_diagnostics msg with
           | Some (uri, severities) when uri = target_uri ->
               loop (Some severities)
           | _ ->
               loop last)
        with
        | Failure m when is_timeout_failure m -> loop last
        | exn -> raise exn
  in
  loop None

let run_mode_case
    ~(server_path:string)
    ~(mode:string)
    ~(expect_errors:bool)
    ~(expect_warnings:bool)
  : unit =
  let root = Lsp_test_helpers.mk_temp_dir ("jovial-lsp-workspace-diags-" ^ mode) in
  let pool_path = Filename.concat root "DIAGPOOL.j73" in
  let pool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL DIAGPOOL;";
        "DEF BEGIN";
        "  ITEM FROM'POOL U 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let broken_path = Filename.concat root "BROKEN.j73" in
  let broken_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC BROKEN RENT;";
        "BEGIN";
        "  ITEM X U 1;";
        "  X = FROM'POOL;";
        "  X = UNKNOWN'ERR;";
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
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM X U 1;";
        "  X = X + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text pool_path pool_text;
  Lsp_test_helpers.write_text broken_path broken_text;
  Lsp_test_helpers.write_text main_path main_text;
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let broken_uri = Lsp_test_helpers.lsp_doc_uri_of_path broken_path in
  let timeout_s = 10.0 in
  Lsp_test_helpers.with_server
    ~env:[
      "JOVIAL_WORKSPACE_DIAGS_MODE", mode;
      "JOVIAL_BG_TICK_BUDGET_MS", "20";
      "JOVIAL_BG_DIAG_BATCH_SIZE", "128";
    ]
    ~server_path
    (fun srv ->
      ignore (Lsp_test_helpers.initialize_and_open
        srv
        ~root_uri
        ~doc_uri
        ~doc_text:main_text
        ~timeout_s);

      let severities_opt =
        wait_for_uri_diagnostics
          ~srv
          ~target_uri:broken_uri
          ~timeout_s:6.0
      in
      let counts =
        match severities_opt with
        | None -> empty_counts
        | Some severities -> counts_of_severities severities
      in

      if expect_errors then (
        if counts.errors <= 0 then
          failf
            "workspace diagnostics mode=%s expected errors for unopened file, got none"
            mode
      ) else if counts.errors > 0 then
        failf
          "workspace diagnostics mode=%s expected no unopened-file errors, got %d"
          mode
          counts.errors;

      if expect_warnings then (
        if counts.warnings <= 0 then
          failf
            "workspace diagnostics mode=%s expected warnings for unopened file, got none"
            mode
      ) else if counts.warnings > 0 then
        failf
          "workspace diagnostics mode=%s expected no unopened-file warnings, got %d"
          mode
          counts.warnings;

      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s;
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
    )

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300))
  in
  let started = Unix.gettimeofday () in
  let ensure_budget (phase:string) : unit =
    let left = hard_timeout_s -. (Unix.gettimeofday () -. started) in
    if left <= 0.0 then
      failf
        "workspace diagnostics background test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s
        phase
  in
  ensure_budget "before mode=off";
  run_mode_case ~server_path ~mode:"off" ~expect_errors:false ~expect_warnings:false;
  ensure_budget "before mode=errors";
  run_mode_case ~server_path ~mode:"errors" ~expect_errors:true ~expect_warnings:false;
  ensure_budget "before mode=all";
  run_mode_case ~server_path ~mode:"all" ~expect_errors:true ~expect_warnings:true;
  print_endline "lsp_workspace_diagnostics_background_test: ok"

