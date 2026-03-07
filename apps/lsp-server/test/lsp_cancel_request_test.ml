let failf = Lsp_test_helpers.failf

let build_large_text ~(lines : int) : string =
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

let error_code (resp : Yojson.Safe.t) : int option =
  let int_of_json = function
    | `Int n -> Some n
    | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  match resp with
  | `Assoc fields -> (
      match List.assoc_opt "error" fields with
      | Some (`Assoc ef) -> (
          match List.assoc_opt "code" ef with
          | Some j -> int_of_json j
          | None -> None)
      | _ -> None)
  | _ -> None

let references_params ~(uri : string) ~(line : int) ~(character : int) :
    Yojson.Safe.t =
  `Assoc
    [
      ("textDocument", `Assoc [ ("uri", `String uri) ]);
      ("position", `Assoc [ ("line", `Int line); ("character", `Int character) ]);
      ("context", `Assoc [ ("includeDeclaration", `Bool true) ]);
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
  let lines =
    max 20000
      (Lsp_test_helpers.getenv_int "JOVIAL_TEST_CANCEL_LINES" ~default:80000)
  in
  let timeout_s =
    float_of_int
      (max 2
         (Lsp_test_helpers.getenv_int "JOVIAL_TEST_CANCEL_TIMEOUT_S" ~default:12))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase : string) : unit =
    if remaining_budget () <= 0.0 then
      failf "cancel-request test exceeded hard timeout (%.1fs) at %s"
        hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-cancel" in
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text = build_large_text ~lines in
  Lsp_test_helpers.write_text main_path main_text;

  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let line, col =
    Lsp_test_helpers.line_col_of_first main_text ~needle:"X = X + 1;"
  in
  let x_char = col + 1 in

  Lsp_test_helpers.with_server ~server_path (fun srv ->
      ensure_budget "before initialize";
      ignore
        (Lsp_test_helpers.initialize_and_open srv ~root_uri ~doc_uri
           ~doc_text:main_text
           ~timeout_s:(min timeout_s (remaining_budget ())));

      Lsp_test_helpers.send_request srv ~id:2 ~method_:"textDocument/references"
        ~params:(references_params ~uri:doc_uri ~line ~character:x_char);
      Lsp_test_helpers.send_notification srv ~method_:"$/cancelRequest"
        ~params:(`Assoc [ ("id", `Int 2) ]);

      let resp =
        Lsp_test_helpers.wait_for_response srv ~id:2
          ~timeout_s:(min timeout_s (remaining_budget ()))
      in
      (match error_code resp with
      | Some -32800 -> ()
      | Some c -> failf "expected cancellation error code -32800, got %d" c
      | None -> failf "expected cancelled request to return JSON-RPC error");

      ensure_budget "before shutdown";
      Lsp_test_helpers.shutdown_and_exit srv
        ~timeout_s:(min timeout_s (remaining_budget ()));
      Lsp_test_helpers.close_stdin srv;
      ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0));

  print_endline "lsp_cancel_request_test: ok"
