let failf = Lsp_test_helpers.failf

let normalize_uri_for_compare (u : string) : string =
  match Jovial_lsp_lib.Uri_path.file_path_of_uri_string u with
  | Some p ->
      let p = String.map (fun c -> if c = '\\' then '/' else c) p in
      if Sys.win32 then String.lowercase_ascii p else p
  | None -> String.lowercase_ascii (String.trim u)

let response_has_result_uri (resp : Yojson.Safe.t) ~(uri : string) : bool =
  let target = normalize_uri_for_compare uri in
  match resp with
  | `Assoc fields -> (
      match List.assoc_opt "result" fields with
      | Some (`List xs) ->
          List.exists
            (function
              | `Assoc lf -> (
                  match List.assoc_opt "uri" lf with
                  | Some (`String u) -> normalize_uri_for_compare u = target
                  | _ -> false)
              | _ -> false)
            xs
      | Some (`Assoc lf) -> (
          match List.assoc_opt "uri" lf with
          | Some (`String u) -> normalize_uri_for_compare u = target
          | _ -> false)
      | _ -> false)
  | _ -> false

let no_error (resp : Yojson.Safe.t) : bool =
  match resp with
  | `Assoc fields -> List.assoc_opt "error" fields = None
  | _ -> false

let method_params ~(uri : string) ~(line : int) ~(character : int) :
    Yojson.Safe.t =
  `Assoc
    [
      ("textDocument", `Assoc [ ("uri", `String uri) ]);
      ("position", `Assoc [ ("line", `Int line); ("character", `Int character) ]);
    ]

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int
      (max 1
         (Lsp_test_helpers.getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S"
            ~default:300))
  in
  let timeout_s =
    float_of_int
      (max 1
         (Lsp_test_helpers.getenv_int "JOVIAL_TEST_DECL_TYPEDEF_TIMEOUT_S"
            ~default:10))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase : string) : unit =
    if remaining_budget () <= 0.0 then
      failf
        "declaration/typeDefinition test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-decl-typedef" in
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [
        "START";
        "TYPE MYTYPE U 1;";
        "DEF PROC HELPER RENT;";
        "BEGIN";
        "END";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM V MYTYPE;";
        "  HELPER();";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main_path main_text;

  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let decl_line, decl_col =
    Lsp_test_helpers.line_col_of_first main_text ~needle:"HELPER();"
  in
  let ty_line, ty_col =
    Lsp_test_helpers.line_col_of_first main_text ~needle:"ITEM V MYTYPE;"
  in
  let type_char = ty_col + 8 in

  Lsp_test_helpers.with_server ~server_path (fun srv ->
      ensure_budget "before initialize";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri
           ~doc_text:main_text
           ~timeout_s:(min timeout_s (remaining_budget ())));

      let decl_resp, _ =
        Lsp_test_helpers.request_timed srv ~id:2
          ~method_:"textDocument/declaration"
          ~params:
            (method_params ~uri:doc_uri ~line:decl_line ~character:decl_col)
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      if not (no_error decl_resp) then
        failf "declaration returned error: %s" (Yojson.Safe.to_string decl_resp);
      if not (response_has_result_uri decl_resp ~uri:doc_uri) then
        failf "declaration did not resolve to current document: %s"
          (Yojson.Safe.to_string decl_resp);

      let td_resp, _ =
        Lsp_test_helpers.request_timed srv ~id:3
          ~method_:"textDocument/typeDefinition"
          ~params:
            (method_params ~uri:doc_uri ~line:ty_line ~character:type_char)
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      if not (no_error td_resp) then
        failf "typeDefinition returned error: %s"
          (Yojson.Safe.to_string td_resp);
      if not (response_has_result_uri td_resp ~uri:doc_uri) then
        failf "typeDefinition did not resolve to current document: %s"
          (Yojson.Safe.to_string td_resp);

      Lsp_test_helpers.shutdown_and_exit srv
        ~timeout_s:(min timeout_s (remaining_budget ()));
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_declaration_type_definition_test: ok"
