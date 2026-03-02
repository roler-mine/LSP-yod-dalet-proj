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

let symbols_and_ids (result_fields:(string * Yojson.Safe.t) list) : Yojson.Safe.t list * (string, bool) Hashtbl.t =
  let symbols =
    match List.assoc_opt "symbols" result_fields with
    | Some (`List xs) -> xs
    | _ -> failf "lsif index payload missing symbols[]"
  in
  let ids = Hashtbl.create 128 in
  List.iter (function
    | `Assoc sf ->
        (match List.assoc_opt "id" sf with
         | Some (`String id) when String.trim id <> "" -> Hashtbl.replace ids id true
         | _ -> failf "lsif symbol missing non-empty id: %s" (Yojson.Safe.to_string (`Assoc sf)))
    | other ->
        failf "lsif symbol entry is not an object: %s" (Yojson.Safe.to_string other)
  ) symbols;
  (symbols, ids)

let assert_key_index_shape
    ~(ids:(string, bool) Hashtbl.t)
    (result_fields:(string * Yojson.Safe.t) list)
  : unit =
  let key_index =
    match List.assoc_opt "keyIndex" result_fields with
    | Some (`List xs) -> xs
    | _ -> failf "lsif index payload missing keyIndex[]"
  in
  if key_index = [] then failf "lsif keyIndex[] was empty";
  List.iter (function
    | `Assoc kf ->
        let key =
          match List.assoc_opt "key" kf with
          | Some (`String k) when String.trim k <> "" -> k
          | _ -> failf "lsif keyIndex entry missing key: %s" (Yojson.Safe.to_string (`Assoc kf))
        in
        ignore key;
        let sym_ids =
          match List.assoc_opt "symbolIds" kf with
          | Some (`List xs) -> xs
          | _ -> failf "lsif keyIndex entry missing symbolIds[]: %s" (Yojson.Safe.to_string (`Assoc kf))
        in
        if sym_ids = [] then
          failf "lsif keyIndex entry had empty symbolIds[]: %s" (Yojson.Safe.to_string (`Assoc kf));
        List.iter (function
          | `String id when Hashtbl.mem ids id -> ()
          | `String id -> failf "keyIndex referenced unknown symbol id %s" id
          | other -> failf "keyIndex symbolIds contained non-string: %s" (Yojson.Safe.to_string other)
        ) sym_ids
    | other ->
        failf "lsif keyIndex entry is not an object: %s" (Yojson.Safe.to_string other)
  ) key_index

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
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_LSIF_PAYLOAD_TIMEOUT_S" ~default:10))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    if remaining_budget () <= 0.0 then
      failf "lsif payload shape test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-lsif-payload-shape" in
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [
        "START";
        "TYPE MYTYPE U 1;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM VALUE U 1;";
        "  VALUE = VALUE + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main_path main_text;
  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in

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

    let lsif_resp, _ =
      Lsp_test_helpers.request_timed srv
        ~id:2
        ~method_:"workspace/executeCommand"
        ~params:(exec_params ~command:"jovial.dumpLsifIndex" ~arguments:[ `String doc_uri ])
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    let result_fields = result_assoc lsif_resp in
    if List.assoc_opt "version" result_fields <> None then
      failf "lsif index payload should not include explicit version field";
    let symbols, ids = symbols_and_ids result_fields in
    if symbols = [] then failf "lsif symbols[] was empty";
    assert_key_index_shape ~ids result_fields;

    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );

  print_endline "lsp_lsif_payload_shape_test: ok"
