let failf = Lsp_test_helpers.failf

type completion_item = {
  label : string;
  sort_text : string option;
}

let completion_items (resp:Yojson.Safe.t) : completion_item list =
  let item_of_json = function
    | `Assoc fields ->
        (match List.assoc_opt "label" fields with
         | Some (`String label) ->
             let sort_text =
               match List.assoc_opt "sortText" fields with
               | Some (`String s) -> Some s
               | _ -> None
             in
             Some { label; sort_text }
         | _ -> None)
    | _ -> None
  in
  match resp with
  | `Assoc fields ->
      (match List.assoc_opt "error" fields, List.assoc_opt "result" fields with
       | Some _, _ -> []
       | _, Some (`List xs) -> List.filter_map item_of_json xs
       | _, Some (`Assoc rf) ->
           (match List.assoc_opt "items" rf with
            | Some (`List xs) -> List.filter_map item_of_json xs
            | _ -> [])
       | _ -> [])
  | _ -> []

let completion_params ~(uri:string) ~(line:int) ~(character:int) : Yojson.Safe.t =
  `Assoc [
    "textDocument", `Assoc [ "uri", `String uri ];
    "position", `Assoc [ "line", `Int line; "character", `Int character ];
  ]

let starts_with ~(prefix:string) (s:string) : bool =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

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
    float_of_int (max 1 (Lsp_test_helpers.getenv_int "JOVIAL_TEST_COMPLETION_RANK_TIMEOUT_S" ~default:10))
  in
  let started = Unix.gettimeofday () in
  let remaining_budget () =
    hard_timeout_s -. (Unix.gettimeofday () -. started)
  in
  let ensure_budget (phase:string) : unit =
    if remaining_budget () <= 0.0 then
      failf "completion ranking test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  let root = Lsp_test_helpers.mk_temp_dir "jovial-lsp-completion-rank" in
  let main_path = Filename.concat root "MAIN.j73" in
  let main_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM LOCALX U 1;";
        "  LOCALX = LO;";
        "END";
        "TERM";
        "";
      ]
  in
  Lsp_test_helpers.write_text main_path main_text;

  let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path root in
  let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
  let line, col = Lsp_test_helpers.line_col_of_first main_text ~needle:"LOCALX = LO;" in
  let pos_char = col + 10 in

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

    let resp, _ =
      Lsp_test_helpers.request_timed srv
        ~id:2
        ~method_:"textDocument/completion"
        ~params:(completion_params ~uri:doc_uri ~line ~character:pos_char)
        ~timeout_s:(min timeout_s (remaining_budget ()))
    in
    let items = completion_items resp in
    if items = [] then
      failf "completion returned no items: %s" (Yojson.Safe.to_string resp);

    let find_item label =
      items
      |> List.find_map (fun it -> if String.uppercase_ascii it.label = label then Some it else None)
    in
    let local =
      match find_item "LOCALX" with
      | Some it -> it
      | None -> failf "completion missing LOCALX item"
    in
    let builtin_loc =
      match find_item "LOC" with
      | Some it -> it
      | None -> failf "completion missing LOC builtin item"
    in
    (match local.sort_text with
     | Some st when starts_with ~prefix:"0_" st -> ()
     | Some st -> failf "LOCALX sortText expected prefix 0_, got %s" st
     | None -> failf "LOCALX missing sortText");
    (match builtin_loc.sort_text with
     | Some st when starts_with ~prefix:"3_" st -> ()
     | Some st -> failf "LOC sortText expected prefix 3_, got %s" st
     | None -> failf "LOC missing sortText");

    let idx_of label =
      let rec loop i = function
        | [] -> None
        | it :: tl ->
            if String.uppercase_ascii it.label = label then Some i else loop (i + 1) tl
      in
      loop 0 items
    in
    let local_idx = match idx_of "LOCALX" with Some i -> i | None -> max_int in
    let loc_idx = match idx_of "LOC" with Some i -> i | None -> max_int in
    if not (local_idx < loc_idx) then
      failf "expected LOCALX to rank before LOC (idx %d vs %d)" local_idx loc_idx;

    Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min timeout_s (remaining_budget ()));
    Lsp_test_helpers.close_stdin srv;
    ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:2.0)
  );

  print_endline "lsp_completion_ranking_test: ok"
