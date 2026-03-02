let failf = Lsp_test_helpers.failf

let build_large_main_text ~(lines:int) : string =
  let b = Buffer.create (max 4096 (lines * 16)) in
  Buffer.add_string b "START\n";
  Buffer.add_string b "DEF PROC MAIN RENT;\n";
  Buffer.add_string b "BEGIN\n";
  Buffer.add_string b "  ITEM X U 1;\n";
  for _ = 1 to lines do
    Buffer.add_string b "  X = X + 1;\n"
  done;
  Buffer.add_string b "END\n";
  Buffer.add_string b "TERM\n";
  Buffer.contents b

let result_field (resp:Yojson.Safe.t) : Yojson.Safe.t option =
  match resp with
  | `Assoc fields -> List.assoc_opt "result" fields
  | _ -> None

let ensure_no_error ~(method_:string) (resp:Yojson.Safe.t) : unit =
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "error" fields with
       | None -> ()
       | Some err ->
           failf "request %s returned error: %s" method_ (Yojson.Safe.to_string err))
  | _ ->
      failf "request %s response is not a JSON object" method_

let expect_definition_shape (resp:Yojson.Safe.t) : unit =
  match result_field resp with
  | Some `Null -> ()
  | Some (`List _) -> ()
  | Some (`Assoc _) -> ()
  | _ -> failf "definition returned unexpected result shape"

let expect_references_shape (resp:Yojson.Safe.t) : unit =
  match result_field resp with
  | Some (`List _) -> ()
  | _ -> failf "references returned unexpected result shape"

let expect_completion_shape (resp:Yojson.Safe.t) : unit =
  match result_field resp with
  | Some (`List _) -> ()
  | Some (`Assoc _) -> ()
  | _ -> failf "completion returned unexpected result shape"

let expect_rename_null (resp:Yojson.Safe.t) : unit =
  match result_field resp with
  | Some `Null -> ()
  | Some v ->
      failf "rename expected null under soft budget exceed, got: %s" (Yojson.Safe.to_string v)
  | None ->
      failf "rename response missing result field"

let int_of_json (j:Yojson.Safe.t) : int option =
  match j with
  | `Int n -> Some n
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let metric_calls (resp:Yojson.Safe.t) ~(name:string) : int =
  match result_field resp with
  | Some (`Assoc fields) ->
      (match List.assoc_opt "metrics" fields with
       | Some (`List entries) ->
           entries
           |> List.find_map (function
                | `Assoc mf ->
                    (match List.assoc_opt "name" mf with
                     | Some (`String metric_name) when metric_name = name ->
                         (match List.assoc_opt "calls" mf with
                          | Some calls_json ->
                              int_of_json calls_json
                          | None -> Some 0)
                     | _ -> None)
                | _ -> None)
           |> (function Some n -> n | None -> 0)
       | _ -> 0)
  | _ -> 0

let references_params ~(uri:string) ~(line:int) ~(character:int) : Yojson.Safe.t =
  `Assoc [
    "textDocument", `Assoc [ "uri", `String uri ];
    "position", `Assoc [ "line", `Int line; "character", `Int character ];
    "context", `Assoc [ "includeDeclaration", `Bool true ];
  ]

let completion_params ~(uri:string) ~(line:int) ~(character:int) : Yojson.Safe.t =
  `Assoc [
    "textDocument", `Assoc [ "uri", `String uri ];
    "position", `Assoc [ "line", `Int line; "character", `Int character ];
  ]

let rename_params ~(uri:string) ~(line:int) ~(character:int) ~(new_name:string) : Yojson.Safe.t =
  `Assoc [
    "textDocument", `Assoc [ "uri", `String uri ];
    "position", `Assoc [ "line", `Int line; "character", `Int character ];
    "newName", `String new_name;
  ]

let debug_perf_params : Yojson.Safe.t =
  `Assoc [
    "command", `String "jovial.debugPerfStats";
    "arguments", `List [];
  ]

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_default =
    max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300)
  in
  let hard_timeout_s =
    float_of_int
      (max 1
         (Lsp_test_helpers.getenv_int
            "JOVIAL_TEST_NAV_SOFT_BUDGET_HARD_TIMEOUT_S"
            ~default:hard_timeout_default))
  in
  let lines =
    max 5000 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_NAV_SOFT_BUDGET_LINES" ~default:30000)
  in
  let timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_NAV_SOFT_BUDGET_TIMEOUT_S" ~default:20))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    let left = remaining_budget () in
    if left <= 0.0 then
      failf "nav soft-budget test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-nav-soft-budget" in
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text = build_large_main_text ~lines in
  Lsp_test_helpers.write_text main_path main_text;

  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let line, col = Lsp_test_helpers.line_col_of_first main_text ~needle:"X = X + 1;" in
  let x_char = col + 1 in

  Lsp_test_helpers.with_server
    ~env:[ "JOVIAL_NAV_SOFT_BUDGET_MS", "1" ]
    ~server_path
    (fun srv ->
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

      let def_resp, _ =
        Lsp_test_helpers.request_timed srv
          ~id:2
          ~method_:"textDocument/definition"
          ~params:(Lsp_test_helpers.definition_request_params
            ~uri:doc_uri
            ~line
            ~character:x_char)
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      ensure_no_error ~method_:"textDocument/definition" def_resp;
      expect_definition_shape def_resp;
      ensure_budget "after definition";

      let refs_resp, _ =
        Lsp_test_helpers.request_timed srv
          ~id:3
          ~method_:"textDocument/references"
          ~params:(references_params
            ~uri:doc_uri
            ~line
            ~character:x_char)
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      ensure_no_error ~method_:"textDocument/references" refs_resp;
      expect_references_shape refs_resp;
      ensure_budget "after references";

      let completion_resp, _ =
        Lsp_test_helpers.request_timed srv
          ~id:4
          ~method_:"textDocument/completion"
          ~params:(completion_params
            ~uri:doc_uri
            ~line
            ~character:x_char)
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      ensure_no_error ~method_:"textDocument/completion" completion_resp;
      expect_completion_shape completion_resp;
      ensure_budget "after completion";

      let rename_resp, _ =
        Lsp_test_helpers.request_timed srv
          ~id:5
          ~method_:"textDocument/rename"
          ~params:(rename_params
            ~uri:doc_uri
            ~line
            ~character:x_char
            ~new_name:"X_RENAMED")
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      ensure_no_error ~method_:"textDocument/rename" rename_resp;
      expect_rename_null rename_resp;
      ensure_budget "after rename";

      let perf_resp, _ =
        Lsp_test_helpers.request_timed srv
          ~id:6
          ~method_:"workspace/executeCommand"
          ~params:debug_perf_params
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      ensure_no_error ~method_:"workspace/executeCommand" perf_resp;
      let soft_budget_calls = metric_calls perf_resp ~name:"nav.soft_budget_exceeded" in
      if soft_budget_calls <= 0 then
        failf
          "expected nav.soft_budget_exceeded metric calls > 0, got %d"
          soft_budget_calls;
      ensure_budget "after perf stats";

      ensure_budget "before shutdown";
      Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
    );

  print_endline "lsp_nav_soft_budget_guard_test: ok"
