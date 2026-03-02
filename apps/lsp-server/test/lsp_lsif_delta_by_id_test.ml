let failf = Lsp_test_helpers.failf

let exec_params ~(command:string) ~(arguments:Yojson.Safe.t list) : Yojson.Safe.t =
  `Assoc [
    "command", `String command;
    "arguments", `List arguments;
  ]

let result_assoc (resp:Yojson.Safe.t) : (string * Yojson.Safe.t) list =
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`Assoc rf) -> rf
       | _ -> failf "workspace/executeCommand result was not an object: %s" (Yojson.Safe.to_string resp))
  | _ ->
      failf "response is not an object: %s" (Yojson.Safe.to_string resp)

let int_field_default (fields:(string * Yojson.Safe.t) list) ~(name:string) ~(default:int) : int =
  match List.assoc_opt name fields with
  | Some (`Int n) -> n
  | Some (`Intlit s) -> (try int_of_string s with _ -> default)
  | _ -> default

let bool_field_default (fields:(string * Yojson.Safe.t) list) ~(name:string) ~(default:bool) : bool =
  match List.assoc_opt name fields with
  | Some (`Bool b) -> b
  | _ -> default

let find_symbol_id_by_key (fields:(string * Yojson.Safe.t) list) ~(key:string) : string option =
  let key = String.uppercase_ascii key in
  match List.assoc_opt "symbols" fields with
  | Some (`List xs) ->
      xs
      |> List.find_map (function
           | `Assoc sf ->
               (match List.assoc_opt "key" sf, List.assoc_opt "id" sf with
                | Some (`String k), Some (`String id)
                  when String.uppercase_ascii (String.trim k) = key && String.trim id <> "" ->
                    Some id
                | _ -> None)
           | _ -> None)
  | _ -> None

let text_end_line_col (text:string) : int * int =
  let line = ref 0 in
  let col = ref 0 in
  String.iter (fun ch ->
    if ch = '\n' then (
      incr line;
      col := 0
    ) else incr col
  ) text;
  (!line, !col)

let did_change_replace_params
    ~(uri:string)
    ~(version:int)
    ~(old_text:string)
    ~(new_text:string)
  : Yojson.Safe.t =
  let end_line, end_char = text_end_line_col old_text in
  `Assoc [
    "textDocument", `Assoc [ "uri", `String uri; "version", `Int version ];
    "contentChanges",
    `List [
      `Assoc [
        "range",
        `Assoc [
          "start", `Assoc [ "line", `Int 0; "character", `Int 0 ];
          "end", `Assoc [ "line", `Int end_line; "character", `Int end_char ];
        ];
        "text", `String new_text;
      ]
    ];
  ]

let deletes_of_delta (fields:(string * Yojson.Safe.t) list) : string list =
  match List.assoc_opt "deletes" fields with
  | Some (`List xs) ->
      xs
      |> List.filter_map (function
           | `String s -> Some s
           | _ -> None)
  | _ -> []

let upsert_has_key (fields:(string * Yojson.Safe.t) list) ~(key:string) : bool =
  let key = String.uppercase_ascii key in
  match List.assoc_opt "upserts" fields with
  | Some (`List xs) ->
      xs
      |> List.exists (function
           | `Assoc sf ->
               (match List.assoc_opt "key" sf, List.assoc_opt "id" sf with
                | Some (`String k), Some (`String id) ->
                    String.uppercase_ascii (String.trim k) = key && String.trim id <> ""
                | _ -> false)
           | _ -> false)
  | _ -> false

let rec poll_for_delta
    ~(srv:Lsp_test_helpers.server_proc)
    ~(doc_uri:string)
    ~(base_revision:int)
    ~(expect_delete_id:string)
    ~(expect_key:string)
    ~(timeout_s:float)
    ~(deadline:float)
    ~(next_id:int)
    ~(last_payload:string option)
  : unit =
  if Unix.gettimeofday () >= deadline then
    failf
      "lsif delta by-id did not converge before timeout; last payload=%s"
      (match last_payload with Some s -> s | None -> "<none>");
  let resp, _ =
    Lsp_test_helpers.request_timed srv
      ~id:next_id
      ~method_:"workspace/executeCommand"
      ~params:(exec_params ~command:"jovial.dumpLsifDelta" ~arguments:[ `String doc_uri; `Int base_revision ])
      ~timeout_s
  in
  let fields = result_assoc resp in
  let payload_s = Yojson.Safe.to_string (`Assoc fields) in
  let reset = bool_field_default fields ~name:"reset" ~default:true in
  let deletes = deletes_of_delta fields in
  let has_expected_delete = List.exists (fun x -> x = expect_delete_id) deletes in
  let has_expected_upsert = upsert_has_key fields ~key:expect_key in
  if (not reset) && has_expected_delete && has_expected_upsert then ()
  else (
    Thread.delay 0.15;
    poll_for_delta
      ~srv
      ~doc_uri
      ~base_revision
      ~expect_delete_id
      ~expect_key
      ~timeout_s
      ~deadline
      ~next_id:(next_id + 1)
      ~last_payload:(Some payload_s)
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
  let timeout_s =
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_LSIF_DELTA_ID_TIMEOUT_S" ~default:10))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    if remaining_budget () <= 0.0 then
      failf "lsif delta by-id test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-lsif-delta-id" in
  let main_path = Filename.concat root "MAIN.j73" in
  let before_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM VALUE U 1;";
        "  VALUE = VALUE + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let after_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM VALUE2 U 1;";
        "  VALUE2 = VALUE2 + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main_path before_text;
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let before_line, before_col = Lsp_test_helpers.line_col_of_first before_text ~needle:"VALUE = VALUE + 1;" in
  let after_line, after_col = Lsp_test_helpers.line_col_of_first after_text ~needle:"VALUE2 = VALUE2 + 1;" in

  Lsp_test_helpers.with_server ~server_path (fun srv ->
    ensure_budget "before initialize";
    ignore (
      Lsp_test_helpers.initialize_and_open
        srv
        ~root_uri
        ~doc_uri
        ~doc_text:before_text
        ~timeout_s:(min timeout_s (remaining_budget ()))
    );

    ignore (
      Lsp_test_helpers.request_timed srv
        ~id:20
        ~method_:"textDocument/definition"
        ~params:(Lsp_test_helpers.definition_request_params
          ~uri:doc_uri
          ~line:before_line
          ~character:(before_col + 1))
        ~timeout_s:(min timeout_s (remaining_budget ()))
    );

    let idx_resp, _ =
      Lsp_test_helpers.request_timed srv
        ~id:2
        ~method_:"workspace/executeCommand"
        ~params:(exec_params ~command:"jovial.dumpLsifIndex" ~arguments:[ `String doc_uri ])
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    let idx_fields = result_assoc idx_resp in
    let base_revision = int_field_default idx_fields ~name:"revision" ~default:(-1) in
    if base_revision < 0 then failf "lsif index revision was missing/invalid";
    let old_value_id =
      match find_symbol_id_by_key idx_fields ~key:"VALUE" with
      | Some id -> id
      | None -> failf "baseline lsif index missing VALUE symbol id"
    in

    Lsp_test_helpers.send_notification srv
      ~method_:"textDocument/didChange"
      ~params:(did_change_replace_params
        ~uri:doc_uri
        ~version:2
        ~old_text:before_text
        ~new_text:after_text);

    ignore (
      Lsp_test_helpers.request_timed srv
        ~id:21
        ~method_:"textDocument/definition"
        ~params:(Lsp_test_helpers.definition_request_params
          ~uri:doc_uri
          ~line:after_line
          ~character:(after_col + 1))
        ~timeout_s:(min timeout_s (remaining_budget ()))
    );

    ensure_budget "after didChange";
    poll_for_delta
      ~srv
      ~doc_uri
      ~base_revision
      ~expect_delete_id:old_value_id
      ~expect_key:"VALUE2"
      ~timeout_s:(min timeout_s (remaining_budget ()))
      ~deadline:(Unix.gettimeofday () +. min 4.0 (max 1.0 (remaining_budget ())))
      ~next_id:3
      ~last_payload:None;

    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );

  print_endline "lsp_lsif_delta_by_id_test: ok"
