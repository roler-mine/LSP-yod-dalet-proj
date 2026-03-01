let failf = Lsp_test_helpers.failf

let build_large_main_text ~(filler_lines:int) : string =
  let b = Buffer.create (max 65536 (filler_lines * 28)) in
  Buffer.add_string b "START\n";
  Buffer.add_string b "DEF PROC MAIN RENT;\n";
  Buffer.add_string b "BEGIN\n";
  Buffer.add_string b "  ITEM TARGET U 1;\n";
  Buffer.add_string b "  TARGET = TARGET + 1;\n";
  for i = 1 to filler_lines do
    Buffer.add_string b (Printf.sprintf "  ITEM PAD_%05d U 1;\n" i)
  done;
  Buffer.add_string b "END\n";
  Buffer.add_string b "TERM\n";
  Buffer.contents b

let expect_definition_result (resp:Yojson.Safe.t) : unit =
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`List (_ :: _)) -> ()
       | Some `Null -> failf "definition returned null result"
       | _ -> failf "definition returned empty/invalid result")
  | _ ->
      failf "definition response is not a JSON object"

let expect_hover_result (resp:Yojson.Safe.t) : unit =
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`Assoc _) -> ()
       | Some `Null -> failf "hover returned null result"
       | _ -> failf "hover returned unexpected result shape")
  | _ ->
      failf "hover response is not a JSON object"

let hover_params ~(uri:string) ~(line:int) ~(character:int) : Yojson.Safe.t =
  `Assoc [
    "textDocument", `Assoc [ "uri", `String uri ];
    "position", `Assoc [ "line", `Int line; "character", `Int character ];
  ]

let did_change_insert_params
    ~(uri:string)
    ~(version:int)
    ~(line:int)
    ~(text:string)
  : Yojson.Safe.t =
  `Assoc [
    "textDocument", `Assoc [ "uri", `String uri; "version", `Int version ];
    "contentChanges",
    `List [
      `Assoc [
        "range",
        `Assoc [
          "start", `Assoc [ "line", `Int line; "character", `Int 0 ];
          "end", `Assoc [ "line", `Int line; "character", `Int 0 ];
        ];
        "text", `String text;
      ]
    ];
  ]

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_EDIT_HARD_TIMEOUT_S" ~default:300))
  in
  let filler_lines =
    max 2000 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_EDIT_LARGE_FILE_LINES" ~default:10000)
  in
  let timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_EDIT_TIMEOUT_S" ~default:20))
  in
  let max_nav_after_edit_ms =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_EDIT_MAX_NAV_MS" ~default:1500))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    let left = remaining_budget () in
    if left <= 0.0 then
      failf "didChange latency test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in
  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-edit-latency" in
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text = build_large_main_text ~filler_lines in
  Lsp_test_helpers.write_text main_path main_text;
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let use_line, use_col = Lsp_test_helpers.line_col_of_first main_text ~needle:"TARGET = TARGET + 1;" in
  let edit_line = 5 + (filler_lines / 2) in

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

    let def_params =
      Lsp_test_helpers.definition_request_params
        ~uri:doc_uri
        ~line:use_line
        ~character:(use_col + 1)
    in
    let warm_resp, _ =
      Lsp_test_helpers.request_timed srv
        ~id:2
        ~method_:"textDocument/definition"
        ~params:def_params
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    expect_definition_result warm_resp;

    Lsp_test_helpers.send_notification srv
      ~method_:"textDocument/didChange"
      ~params:(did_change_insert_params
        ~uri:doc_uri
        ~version:2
        ~line:edit_line
        ~text:"  ITEM FAST_EDIT U 1;\n");

    let def_resp_after, def_ms_after =
      Lsp_test_helpers.request_timed srv
        ~id:3
        ~method_:"textDocument/definition"
        ~params:def_params
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    expect_definition_result def_resp_after;

    let hover_resp_after, hover_ms_after =
      Lsp_test_helpers.request_timed srv
        ~id:4
        ~method_:"textDocument/hover"
        ~params:(hover_params ~uri:doc_uri ~line:use_line ~character:(use_col + 1))
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    expect_hover_result hover_resp_after;

    if def_ms_after > max_nav_after_edit_ms then
      failf
        "definition after didChange too slow: %.1fms (limit %.1fms, filler lines=%d)"
        def_ms_after
        max_nav_after_edit_ms
        filler_lines;

    if hover_ms_after > max_nav_after_edit_ms then
      failf
        "hover after didChange too slow: %.1fms (limit %.1fms, filler lines=%d)"
        hover_ms_after
        max_nav_after_edit_ms
        filler_lines;

    ensure_budget "after nav checks";
    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );

  print_endline "lsp_didchange_latency_test: ok"
