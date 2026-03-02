let failf = Lsp_test_helpers.failf

let create_workspace ~(root:string) ~(file_count:int) : string * string * string =
  let bucket_count = 24 in
  for i = 0 to bucket_count - 1 do
    Lsp_test_helpers.ensure_dir (Filename.concat root (Printf.sprintf "pkg_%02d" i))
  done;
  for i = 0 to file_count - 1 do
    let dir = Filename.concat root (Printf.sprintf "pkg_%02d" (i mod bucket_count)) in
    let path = Filename.concat dir (Printf.sprintf "LIB_%04d.j73" i) in
    let text =
      String.concat "\n"
        [
          "START";
          (Printf.sprintf "DEF PROC LIB_%04d RENT;" i);
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
  let target_path = Filename.concat root "TARGETPOOL.j73" in
  let target_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGETPOOL;";
        "DEF BEGIN";
        "  ITEM TARGET'VAL U 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text target_path target_text;
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('TARGETPOOL');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  TARGET'VAL = TARGET'VAL + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main_path main_text;
  (main_path, main_text, target_path)

let response_has_result_uri (resp:Yojson.Safe.t) ~(uri:string) : bool =
  let normalize_uri_for_compare (u:string) : string =
    match Jovial_lsp_lib.Uri_path.file_path_of_uri_string u with
    | Some p ->
        let p = String.map (fun c -> if c = '\\' then '/' else c) p in
        if Sys.win32 then String.lowercase_ascii p else p
    | None ->
        String.lowercase_ascii (String.trim u)
  in
  let want = normalize_uri_for_compare uri in
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`List xs) ->
           List.exists (function
             | `Assoc lf ->
                 (match List.assoc_opt "uri" lf with
                  | Some (`String u) -> normalize_uri_for_compare u = want
                  | _ -> false)
             | _ -> false
           ) xs
       | _ -> false)
  | _ ->
      false

let request_definition
    ~(srv:Lsp_test_helpers.server_proc)
    ~(id:int)
    ~(uri:string)
    ~(line:int)
    ~(character:int)
    ~(timeout_s:float)
  : Yojson.Safe.t * float =
  let params =
    Lsp_test_helpers.definition_request_params
      ~uri
      ~line
      ~character
  in
  Lsp_test_helpers.request_timed srv
    ~id
    ~method_:"textDocument/definition"
    ~params
    ~timeout_s

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300))
  in
  let index_files =
    max 0 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_CROSSFILE_INDEX_FILES" ~default:1800)
  in
  let timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_CROSSFILE_TIMEOUT_S" ~default:12))
  in
  let first_max_ms =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_CROSSFILE_FIRST_MAX_MS" ~default:2000))
  in
  let settle_timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_CROSSFILE_SETTLE_TIMEOUT_S" ~default:4))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    let left = remaining_budget () in
    if left <= 0.0 then
      failf "cross-file goto latency test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in
  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-crossfile-goto" in
  let main_path, main_text, target_path = create_workspace ~root ~file_count:index_files in
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let target_uri = Lsp_test_helpers.lsp_doc_uri_of_path target_path in
  let line, col = Lsp_test_helpers.line_col_of_first main_text ~needle:"TARGET'VAL = TARGET'VAL + 1;" in
  let def_char = col + 15 in
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
    ensure_budget "after initialize";

    let first_resp, first_ms =
      request_definition
        ~srv
        ~id:2
        ~uri:doc_uri
        ~line
        ~character:def_char
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    if first_ms > first_max_ms then
      failf
        "first cross-file definition too slow: %.1fms (limit %.1fms)"
        first_ms
        first_max_ms;

    let found_first = response_has_result_uri first_resp ~uri:target_uri in
    let found_eventually =
      if found_first then true
      else (
        let deadline = Unix.gettimeofday () +. settle_timeout_s in
        let rec loop req_id =
          if Unix.gettimeofday () >= deadline then false
          else (
            Lsp_test_helpers.sleep_seconds 0.1;
            let resp, _ =
              request_definition
                ~srv
                ~id:req_id
                ~uri:doc_uri
                ~line
                ~character:def_char
                ~timeout_s:(min timeout_s (remaining_budget ()))
            in
            if response_has_result_uri resp ~uri:target_uri then true
            else loop (req_id + 1)
          )
        in
        loop 3
      )
    in
    if not found_eventually then
      failf
        "cross-file definition did not converge to target within %.1fs (target=%s)"
        settle_timeout_s
        target_uri;

    ensure_budget "after cross-file checks";
    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );
  print_endline "lsp_crossfile_goto_latency_test: ok"

