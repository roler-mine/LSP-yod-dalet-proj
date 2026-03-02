let failf = Lsp_test_helpers.failf

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
  | _ -> false

let workspace_files ~(root:string) ~(suffix:string) : string * string * string =
  let target_path = Filename.concat root "TARGETPOOL.j73" in
  let target_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGETPOOL;";
        "DEF BEGIN";
        (Printf.sprintf "  ITEM TARGET'VAL_%s U 1;" suffix);
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
        (Printf.sprintf "  TARGET'VAL_%s = TARGET'VAL_%s + 1;" suffix suffix);
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main_path main_text;
  (main_path, main_text, target_path)

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300))
  in
  let timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_MULTI_ROOT_TIMEOUT_S" ~default:10))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    if remaining_budget () <= 0.0 then
      failf "multi-root routing test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  let root_a = Lsp_test_helpers.mk_temp_dir "jovial-lsp-root-a" in
  let root_b = Lsp_test_helpers.mk_temp_dir "jovial-lsp-root-b" in
  let _a_main_path, _a_main_text, _a_target_path = workspace_files ~root:root_a ~suffix:"A" in
  let b_main_path, b_main_text, b_target_path = workspace_files ~root:root_b ~suffix:"B" in

  let root_a_uri = Lsp_test_helpers.lsp_doc_uri_of_path root_a in
  let root_b_uri = Lsp_test_helpers.lsp_doc_uri_of_path root_b in
  let b_main_uri = Lsp_test_helpers.lsp_doc_uri_of_path b_main_path in
  let b_target_uri = Lsp_test_helpers.lsp_doc_uri_of_path b_target_path in
  let line, col = Lsp_test_helpers.line_col_of_first b_main_text ~needle:"TARGET'VAL_B = TARGET'VAL_B + 1;" in
  let def_char = col + 16 in

  Lsp_test_helpers.with_server ~server_path (fun srv ->
    ensure_budget "before initialize";
    let init_params =
      `Assoc [
        "processId", `Null;
        "workspaceFolders",
        `List [
          `Assoc [ "uri", `String root_a_uri; "name", `String "A" ];
          `Assoc [ "uri", `String root_b_uri; "name", `String "B" ];
        ];
        "capabilities", `Assoc [];
      ]
    in
    Lsp_test_helpers.send_request srv ~id:1 ~method_:"initialize" ~params:init_params;
    let init_resp =
      Lsp_test_helpers.wait_for_response srv ~id:1 ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    if not (Lsp_test_helpers.has_non_null_result init_resp) then
      failf "initialize returned null/empty result";
    Lsp_test_helpers.send_notification srv ~method_:"initialized" ~params:(`Assoc []);

    Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen"
      ~params:(`Assoc [
        "textDocument",
        `Assoc [
          "uri", `String b_main_uri;
          "languageId", `String "jovial";
          "version", `Int 1;
          "text", `String b_main_text;
        ];
      ]);

    let def_resp, _ =
      Lsp_test_helpers.request_timed srv
        ~id:2
        ~method_:"textDocument/definition"
        ~params:(Lsp_test_helpers.definition_request_params
          ~uri:b_main_uri
          ~line
          ~character:def_char)
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    if not (response_has_result_uri def_resp ~uri:b_target_uri) then
      failf
        "expected definition to resolve in root-B target (%s), got: %s"
        b_target_uri
        (Yojson.Safe.to_string def_resp);

    ensure_budget "before shutdown";
    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );

  print_endline "lsp_multi_root_routing_test: ok"
