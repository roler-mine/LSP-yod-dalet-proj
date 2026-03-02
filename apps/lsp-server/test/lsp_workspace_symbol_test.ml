let failf = Lsp_test_helpers.failf

let result_names (resp:Yojson.Safe.t) : string list =
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`List xs) ->
           xs
           |> List.filter_map (function
                | `Assoc sf ->
                    (match List.assoc_opt "name" sf with
                     | Some (`String s) -> Some s
                     | _ -> None)
                | _ -> None)
       | _ -> [])
  | _ -> []

let has_error (resp:Yojson.Safe.t) : bool =
  match resp with
  | `Assoc fields -> List.assoc_opt "error" fields <> None
  | _ -> true

let open_doc_params ~(uri:string) ~(text:string) : Yojson.Safe.t =
  `Assoc [
    "textDocument",
    `Assoc [
      "uri", `String uri;
      "languageId", `String "jovial";
      "version", `Int 1;
      "text", `String text;
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
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_WORKSPACE_SYMBOL_TIMEOUT_S" ~default:10))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    if remaining_budget () <= 0.0 then
      failf "workspace/symbol test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-workspace-symbol" in
  let a_path = Filename.concat root "A.j73" in
  let b_path = Filename.concat root "B.j73" in
  let a_text =
    String.concat "\n"
      [ "START"; "DEF PROC ALPHA RENT;"; "BEGIN"; "END"; "TERM"; "" ]
  in
  let b_text =
    String.concat "\n"
      [ "START"; "DEF PROC BETA RENT;"; "BEGIN"; "END"; "TERM"; "" ]
  in
  Lsp_test_helpers.write_text a_path a_text;
  Lsp_test_helpers.write_text b_path b_text;

  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let a_uri = Lsp_test_helpers.lsp_doc_uri_of_path a_path in
  let b_uri = Lsp_test_helpers.lsp_doc_uri_of_path b_path in

  Lsp_test_helpers.with_server ~server_path (fun srv ->
    ensure_budget "before initialize";
    let init_params =
      `Assoc [
        "processId", `Null;
        "rootUri", `String root_uri;
        "capabilities", `Assoc [];
      ]
    in
    ignore (
      Lsp_test_helpers.request_timed srv
        ~id:1
        ~method_:"initialize"
        ~params:init_params
        ~timeout_s:(min timeout_s (remaining_budget ()))
    );
    Lsp_test_helpers.send_notification srv ~method_:"initialized" ~params:(`Assoc []);
    Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen" ~params:(open_doc_params ~uri:a_uri ~text:a_text);
    Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen" ~params:(open_doc_params ~uri:b_uri ~text:b_text);

    let ws_symbol_params = `Assoc [ "query", `String "ALP" ] in
    let resp, _ =
      Lsp_test_helpers.request_timed srv
        ~id:2
        ~method_:"workspace/symbol"
        ~params:ws_symbol_params
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    if has_error resp then
      failf "workspace/symbol returned error: %s" (Yojson.Safe.to_string resp);
    let names = result_names resp in
    if not (List.exists (fun n -> String.uppercase_ascii n = "ALPHA") names) then
      failf "workspace/symbol missing ALPHA result: %s" (Yojson.Safe.to_string resp);
    if List.exists (fun n -> String.uppercase_ascii n = "BETA") names then
      failf "workspace/symbol query ALP unexpectedly returned BETA";

    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );

  print_endline "lsp_workspace_symbol_test: ok"
