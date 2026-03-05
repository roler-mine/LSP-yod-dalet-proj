let failf = Lsp_test_helpers.failf

let exec_params ~(command:string) ~(arguments:Yojson.Safe.t list) : Yojson.Safe.t =
  `Assoc [
    "command", `String command;
    "arguments", `List arguments;
  ]

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

let symbol_keys_of_index (fields:(string * Yojson.Safe.t) list) : string list =
  match List.assoc_opt "symbols" fields with
  | Some (`List xs) ->
      xs
      |> List.filter_map (function
           | `Assoc sf ->
               (match List.assoc_opt "key" sf with
                | Some (`String k) when String.trim k <> "" ->
                    Some (String.uppercase_ascii (String.trim k))
                | _ -> None)
           | _ -> None)
  | _ -> []

let index_has_symbols (fields:(string * Yojson.Safe.t) list) : bool =
  match List.assoc_opt "symbols" fields with
  | Some (`List (_ :: _)) -> true
  | _ -> false

let contains_key (keys:string list) (key:string) : bool =
  let key = String.uppercase_ascii key in
  List.exists (fun k -> k = key) keys

let assert_index_contains_only_root_symbol
    ~(index_fields:(string * Yojson.Safe.t) list)
    ~(must_have:string)
    ~(must_not_have:string)
  : unit =
  let keys = symbol_keys_of_index index_fields in
  if not (contains_key keys must_have) then
    failf "lsif index missing expected key %s" must_have;
  if contains_key keys must_not_have then
    failf "lsif index leaked foreign root key %s" must_not_have

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
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_LSIF_ROOT_TIMEOUT_S" ~default:10))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    if remaining_budget () <= 0.0 then
      failf "lsif root routing test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  let base = Lsp_test_helpers.mk_temp_dir "jovial-lsp-lsif-root-routing" in
  let root_a = Filename.concat base "rootA" in
  let root_b = Filename.concat base "rootB" in
  Lsp_test_helpers.ensure_dir root_a;
  Lsp_test_helpers.ensure_dir root_b;
  let a_path = Filename.concat root_a "A_MAIN.j73" in
  let b_path = Filename.concat root_b "B_MAIN.j73" in
  let a_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM ALPHA_ROOT U 1;";
        "  ALPHA_ROOT = ALPHA_ROOT + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let b_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM BETA_ROOT U 1;";
        "  BETA_ROOT = BETA_ROOT + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text a_path a_text;
  Lsp_test_helpers.write_text b_path b_text;

  let a_uri = Lsp_test_helpers.lsp_doc_uri_of_path a_path in
  let b_uri = Lsp_test_helpers.lsp_doc_uri_of_path b_path in
  let root_a_uri = Lsp_test_helpers.lsp_doc_uri_of_path root_a in
  let root_b_uri = Lsp_test_helpers.lsp_doc_uri_of_path root_b in

  Lsp_test_helpers.with_server ~server_path (fun srv ->
    ensure_budget "before initialize";
    let init_params =
      `Assoc [
        "processId", `Null;
        "rootUri", `String root_a_uri;
        "workspaceFolders",
        `List [
          `Assoc [ "uri", `String root_a_uri; "name", `String "rootA" ];
          `Assoc [ "uri", `String root_b_uri; "name", `String "rootB" ];
        ];
        "capabilities", `Assoc [];
      ]
    in
    let init_resp, _ =
      Lsp_test_helpers.request_timed srv
        ~id:1
        ~method_:"initialize"
        ~params:init_params
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    if not (Lsp_test_helpers.has_non_null_result init_resp) then
      failf "initialize returned null/empty result";
    Lsp_test_helpers.send_notification srv ~method_:"initialized" ~params:(`Assoc []);
    Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen"
      ~params:(open_doc_params ~uri:a_uri ~text:a_text);
    Lsp_test_helpers.send_notification srv ~method_:"textDocument/didOpen"
      ~params:(open_doc_params ~uri:b_uri ~text:b_text);

    let rec poll_index ~uri next_id deadline =
      if Unix.gettimeofday () >= deadline then
        failf "lsif root routing test timed out waiting for non-empty index (%s)" uri;
      let resp, _ =
        Lsp_test_helpers.request_timed srv
          ~id:next_id
          ~method_:"workspace/executeCommand"
          ~params:(exec_params ~command:"jovial.dumpLsifIndex" ~arguments:[ `String uri ])
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      let fields = result_assoc resp in
      if index_has_symbols fields then (fields, next_id + 1)
      else (
        Thread.delay 0.1;
        poll_index ~uri (next_id + 1) deadline
      )
    in
    let deadline = Unix.gettimeofday () +. min 4.0 (max 1.0 (remaining_budget ())) in
    let idx_a_fields, next_id = poll_index ~uri:a_uri 2 deadline in
    let idx_b_fields, next_id = poll_index ~uri:b_uri next_id deadline in
    assert_index_contains_only_root_symbol
      ~index_fields:idx_a_fields
      ~must_have:"ALPHA_ROOT"
      ~must_not_have:"BETA_ROOT";
    assert_index_contains_only_root_symbol
      ~index_fields:idx_b_fields
      ~must_have:"BETA_ROOT"
      ~must_not_have:"ALPHA_ROOT";

    let rev_a = int_field_default idx_a_fields ~name:"revision" ~default:(-1) in
    let rev_b = int_field_default idx_b_fields ~name:"revision" ~default:(-1) in
    if rev_a < 0 || rev_b < 0 then failf "lsif index revision missing in root routing test";

    let delta_a_resp, _ =
      Lsp_test_helpers.request_timed srv
        ~id:next_id
        ~method_:"workspace/executeCommand"
        ~params:(exec_params ~command:"jovial.dumpLsifDelta" ~arguments:[ `String a_uri; `Int rev_a ])
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    let delta_b_resp, _ =
      Lsp_test_helpers.request_timed srv
        ~id:(next_id + 1)
        ~method_:"workspace/executeCommand"
        ~params:(exec_params ~command:"jovial.dumpLsifDelta" ~arguments:[ `String b_uri; `Int rev_b ])
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    let delta_a_fields = result_assoc delta_a_resp in
    let delta_b_fields = result_assoc delta_b_resp in
    if bool_field_default delta_a_fields ~name:"reset" ~default:true then
      failf "root A delta unexpectedly requested reset at current revision";
    if bool_field_default delta_b_fields ~name:"reset" ~default:true then
      failf "root B delta unexpectedly requested reset at current revision";

    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );

  print_endline "lsp_lsif_root_routing_test: ok"
