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

let response_has_error (resp:Yojson.Safe.t) : bool =
  match resp with
  | `Assoc fields -> List.assoc_opt "error" fields <> None
  | _ -> true

let response_has_result_uri (resp:Yojson.Safe.t) ~(uri:string) : bool =
  let target = Lsp_test_helpers.normalize_uri_for_compare uri in
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`List xs) ->
           List.exists (function
             | `Assoc lf ->
                 (match List.assoc_opt "uri" lf with
                  | Some (`String u) -> Lsp_test_helpers.normalize_uri_for_compare u = target
                  | _ -> false)
             | _ -> false)
             xs
       | Some (`Assoc lf) ->
           (match List.assoc_opt "uri" lf with
            | Some (`String u) -> Lsp_test_helpers.normalize_uri_for_compare u = target
            | _ -> false)
       | _ -> false)
  | _ -> false

let hover_markdown (resp:Yojson.Safe.t) : string option =
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`Assoc result_fields) ->
           (match List.assoc_opt "contents" result_fields with
            | Some (`Assoc content_fields) ->
                (match List.assoc_opt "value" content_fields with
                 | Some (`String s) -> Some s
                 | _ -> None)
            | Some (`String s) -> Some s
            | _ -> None)
       | _ -> None)
  | _ -> None

let hover_params ~(uri:string) ~(line:int) ~(character:int) : Yojson.Safe.t =
  `Assoc
    [
      "textDocument", `Assoc [ "uri", `String uri ];
      "position", `Assoc [ "line", `Int line; "character", `Int character ];
    ]

let request_definition
    ~(srv:Lsp_test_helpers.server_proc)
    ~(id:int)
    ~(uri:string)
    ~(line:int)
    ~(character:int)
    ~(timeout_s:float)
  : Yojson.Safe.t * float =
  Lsp_test_helpers.request_timed
    srv
    ~id
    ~method_:"textDocument/definition"
    ~params:(Lsp_test_helpers.definition_request_params ~uri ~line ~character)
    ~timeout_s

let request_hover
    ~(srv:Lsp_test_helpers.server_proc)
    ~(id:int)
    ~(uri:string)
    ~(line:int)
    ~(character:int)
    ~(timeout_s:float)
  : Yojson.Safe.t * float =
  Lsp_test_helpers.request_timed
    srv
    ~id
    ~method_:"textDocument/hover"
    ~params:(hover_params ~uri ~line ~character)
    ~timeout_s

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let hard_timeout_s =
    float_of_int
      (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:180))
  in
  let timeout_s =
    float_of_int
      (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_REF_PROC_NAV_TIMEOUT_S" ~default:10))
  in
  let settle_timeout_s =
    float_of_int
      (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_REF_PROC_NAV_SETTLE_TIMEOUT_S" ~default:8))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    if remaining_budget () <= 0.0 then
      failf
        "ref-proc nav test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s
        phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-ref-proc-nav" in
  let lib_path = Filename.concat root "LIB.j73" in
  let pool_path = Filename.concat root "DUMMYPOOL.j73" in
  let main_path = Filename.concat root "MAIN.j73" in
  let lib_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC HELPER RENT;";
        "BEGIN";
        "END";
        "TERM";
        "";
      ]
  in
  let main_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('DUMMYPOOL');";
        "REF PROC HELPER RENT;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  HELPER();";
        "END";
        "TERM";
        "";
      ]
  in
  let pool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL DUMMYPOOL;";
        "DEF BEGIN";
        "  ITEM DUMMY U 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text lib_path lib_text;
  Lsp_test_helpers.write_text pool_path pool_text;
  Lsp_test_helpers.write_text main_path main_text;
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let lib_uri = Lsp_test_helpers.lsp_doc_uri_of_path lib_path in
  let call_line, call_col = Lsp_test_helpers.line_col_of_first main_text ~needle:"HELPER();" in
  let hover_char = call_col + 1 in
  let lib_decl_line0, _ = Lsp_test_helpers.line_col_of_first lib_text ~needle:"DEF PROC HELPER RENT;" in
  let ref_decl_line0, _ = Lsp_test_helpers.line_col_of_first main_text ~needle:"REF PROC HELPER RENT;" in
  let expected_hover_loc =
    Printf.sprintf "%s:%d" (Filename.basename lib_path) (lib_decl_line0 + 1)
  in
  let ref_hover_loc =
    Printf.sprintf "%s:%d" (Filename.basename main_path) (ref_decl_line0 + 1)
  in

  Lsp_test_helpers.with_server ~server_path (fun srv ->
    ensure_budget "before initialize";
    ignore
      (Lsp_test_helpers.initialize_and_open
         srv
         ~root_uri
         ~doc_uri
         ~doc_text:main_text
         ~timeout_s:(min timeout_s (remaining_budget ())));
    ensure_budget "after initialize";

    let def_deadline = Unix.gettimeofday () +. min settle_timeout_s (remaining_budget ()) in
    let rec wait_for_definition req_id last_payload =
      if Unix.gettimeofday () >= def_deadline then
        failf
          "definition did not converge to implementation %s within %.1fs; last payload=%s"
          lib_uri
          settle_timeout_s
          last_payload;
      let resp, _ =
        request_definition
          ~srv
          ~id:req_id
          ~uri:doc_uri
          ~line:call_line
          ~character:hover_char
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      if response_has_error resp then
        failf "definition returned error: %s" (Yojson.Safe.to_string resp);
      if response_has_result_uri resp ~uri:lib_uri then ()
      else (
        Lsp_test_helpers.sleep_seconds 0.1;
        wait_for_definition (req_id + 1) (Yojson.Safe.to_string resp)
      )
    in
    wait_for_definition 2 "<no response>";

    ensure_budget "after definition convergence";

    let hover_deadline = Unix.gettimeofday () +. min settle_timeout_s (remaining_budget ()) in
    let rec wait_for_hover req_id last_payload =
      if Unix.gettimeofday () >= hover_deadline then
        failf
          "hover did not converge to implementation location %s within %.1fs; last payload=%s"
          expected_hover_loc
          settle_timeout_s
          last_payload;
      let resp, _ =
        request_hover
          ~srv
          ~id:req_id
          ~uri:doc_uri
          ~line:call_line
          ~character:hover_char
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      if response_has_error resp then
        failf "hover returned error: %s" (Yojson.Safe.to_string resp);
      match hover_markdown resp with
      | Some md
        when contains_substring ~haystack:md ~needle:expected_hover_loc
             && not (contains_substring ~haystack:md ~needle:ref_hover_loc) ->
          ()
      | _ ->
          Lsp_test_helpers.sleep_seconds 0.1;
          wait_for_hover (req_id + 1) (Yojson.Safe.to_string resp)
    in
    wait_for_hover 100 "<no response>";

    ensure_budget "before shutdown";
    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );

  print_endline "lsp_ref_proc_nav_prefers_impl_test: ok"
