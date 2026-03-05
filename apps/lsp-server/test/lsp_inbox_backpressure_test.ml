let failf = Lsp_test_helpers.failf

let int_of_json (j:Yojson.Safe.t) : int option =
  match j with
  | `Int n -> Some n
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let result_field (resp:Yojson.Safe.t) : Yojson.Safe.t option =
  match resp with
  | `Assoc fields -> List.assoc_opt "result" fields
  | _ -> None

let get_assoc_field (obj:Yojson.Safe.t) (key:string) : Yojson.Safe.t option =
  match obj with
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let server_inbox_stats (resp:Yojson.Safe.t) : int * int =
  match result_field resp with
  | Some result ->
      (match get_assoc_field result "server" with
       | Some (`Assoc server_fields) ->
           let max_items =
             match List.assoc_opt "inboxMaxItems" server_fields with
             | Some j -> (match int_of_json j with Some n -> n | None -> 0)
             | None -> 0
           in
           let max_seen =
             match List.assoc_opt "inboxMaxDepthSeen" server_fields with
             | Some j -> (match int_of_json j with Some n -> n | None -> 0)
             | None -> 0
           in
           (max_items, max_seen)
       | _ ->
           failf "debugReport result is missing server inbox stats")
  | _ ->
      failf "debugReport response missing result field"

let debug_report_params ~(uri:string) : Yojson.Safe.t =
  `Assoc [
    "command", `String "jovial.debugReport";
    "arguments", `List [ `String uri; `Int 32 ];
  ]

let didchange_full_text_params
    ~(uri:string)
    ~(version:int)
    ~(text:string)
  : Yojson.Safe.t =
  `Assoc [
    "textDocument",
    `Assoc [
      "uri", `String uri;
      "version", `Int version;
    ];
    "contentChanges",
    `List [
      `Assoc [ "text", `String text ];
    ];
  ]

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
    float_of_int (max 2 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_INBOX_BP_TIMEOUT_S" ~default:20))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    if remaining_budget () <= 0.0 then
      failf "inbox backpressure test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-inbox-backpressure" in
  let main_path = Filename.concat root "MAIN.j73" in
  let text_a =
    String.concat "\n"
      [ "START"; "DEF PROC MAIN RENT;"; "BEGIN"; "  ITEM X U 1;"; "  X = X + 1;"; "END"; "TERM"; "" ]
  in
  let text_b =
    String.concat "\n"
      [ "START"; "DEF PROC MAIN RENT;"; "BEGIN"; "  ITEM X U 1;"; "  X = X + 2;"; "END"; "TERM"; "" ]
  in
  Lsp_test_helpers.write_text main_path text_a;
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let line, col = Lsp_test_helpers.line_col_of_first text_a ~needle:"X = X + 1;" in
  let x_char = col + 1 in

  Lsp_test_helpers.with_server
    ~env:[ "JOVIAL_INBOX_MAX_ITEMS", "64" ]
    ~server_path
    (fun srv ->
      ensure_budget "before initialize";
      ignore (
        Lsp_test_helpers.initialize_and_open
          srv
          ~root_uri
          ~doc_uri
          ~doc_text:text_a
          ~timeout_s:(min timeout_s (remaining_budget ()))
      );

      for i = 2 to 520 do
        let txt = if i mod 2 = 0 then text_a else text_b in
        Lsp_test_helpers.send_notification
          srv
          ~method_:"textDocument/didChange"
          ~params:(didchange_full_text_params ~uri:doc_uri ~version:i ~text:txt)
      done;
      ensure_budget "after notification flood";

      let report_resp, _ =
        Lsp_test_helpers.request_timed srv
          ~id:700
          ~method_:"workspace/executeCommand"
          ~params:(debug_report_params ~uri:doc_uri)
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      let max_items, max_seen = server_inbox_stats report_resp in
      if max_items <= 0 then failf "invalid inbox max items from debugReport: %d" max_items;
      if max_seen > max_items then
        failf
          "inbox depth watermark exceeded cap: seen=%d cap=%d"
          max_seen
          max_items;
      ensure_budget "after debug report";

      let def_resp, _ =
        Lsp_test_helpers.request_timed srv
          ~id:701
          ~method_:"textDocument/definition"
          ~params:(Lsp_test_helpers.definition_request_params
            ~uri:doc_uri
            ~line
            ~character:x_char)
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      if not (Lsp_test_helpers.has_non_null_result def_resp) then
        failf "definition returned null/empty after inbox flood";

      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
    );

  print_endline "lsp_inbox_backpressure_test: ok"
