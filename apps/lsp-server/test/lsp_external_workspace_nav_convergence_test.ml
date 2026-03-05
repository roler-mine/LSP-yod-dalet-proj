let failf = Lsp_test_helpers.failf

let has_source_ext (path:string) : bool =
  let lower = String.lowercase_ascii path in
  Filename.check_suffix lower ".jov" || Filename.check_suffix lower ".j73"

let is_dir (path:string) : bool =
  try (Unix.stat path).st_kind = Unix.S_DIR with _ -> false

let rec collect_source_files (root:string) : string list =
  if not (is_dir root) then []
  else
    let entries =
      try Sys.readdir root |> Array.to_list with _ -> []
    in
    entries
    |> List.sort String.compare
    |> List.concat_map (fun name ->
         let path = Filename.concat root name in
         if is_dir path then collect_source_files path
         else if has_source_ext path then [ path ]
         else [])

let read_text (path:string) : string =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let file_size_bytes (path:string) : int =
  try (Unix.stat path).st_size with _ -> 0

let largest_file (files:string list) : string =
  match files with
  | [] -> failf "no files"
  | first :: rest ->
      List.fold_left
        (fun best path ->
          if file_size_bytes path > file_size_bytes best then path else best)
        first
        rest

let is_symbol_char (c:char) : bool =
  (c >= 'A' && c <= 'Z')
  || (c >= 'a' && c <= 'z')
  || (c >= '0' && c <= '9')
  || c = '_'
  || c = '\''

let is_ident_char (c:char) : bool =
  (c >= 'A' && c <= 'Z')
  || (c >= 'a' && c <= 'z')
  || (c >= '0' && c <= '9')
  || c = '_'

let is_reserved_like (tok:string) : bool =
  match String.uppercase_ascii tok with
  | "START" | "TERM" | "PROGRAM" | "COMPOOL"
  | "DEF" | "REF" | "PROC" | "BEGIN" | "END" | "ITEM"
  | "IF" | "THEN" | "ELSE" | "WHILE" | "FOR" | "STOP"
  | "ABORT" | "RETURN" | "EXIT" -> true
  | _ -> false

let collect_call_like_tokens ~(limit:int) (text:string) : string list =
  let n = String.length text in
  let seen : (string, bool) Hashtbl.t = Hashtbl.create 64 in
  let add acc tok =
    if tok = "" || is_reserved_like tok || Hashtbl.mem seen tok then acc
    else (
      Hashtbl.replace seen tok true;
      tok :: acc)
  in
  let rec scan i acc =
    if i >= n || List.length acc >= limit then List.rev acc
    else if is_ident_char text.[i] then
      let j = ref (i + 1) in
      while !j < n && is_ident_char text.[!j] do
        incr j
      done;
      let tok = String.sub text i (!j - i) in
      let k = ref !j in
      while !k < n && (text.[!k] = ' ' || text.[!k] = '\t') do
        incr k
      done;
      if !k < n && text.[!k] = '(' then
        scan (!j + 1) (add acc tok)
      else
        scan (!j + 1) acc
    else
      scan (i + 1) acc
  in
  scan 0 []

let is_identifier_apostrophe_token (tok:string) : bool =
  let n = String.length tok in
  if n < 3 then false
  else
    let rec first_quote i =
      if i >= n then None
      else if tok.[i] = '\'' then Some i
      else first_quote (i + 1)
    in
    match first_quote 0 with
    | None -> false
    | Some i ->
        i > 0
        && i < (n - 1)
        && tok.[0] <> '\''
        && tok.[n - 1] <> '\''

let find_first_apostrophe_token (text:string) : string option =
  let n = String.length text in
  let rec next_token i =
    if i >= n then None
    else if is_symbol_char text.[i] then read_token i (i + 1) false
    else next_token (i + 1)
  and read_token start i has_quote =
    if i >= n || not (is_symbol_char text.[i]) then
      if has_quote then
        let tok = String.sub text start (i - start) in
        if is_identifier_apostrophe_token tok then Some tok
        else next_token i
      else next_token i
    else
      read_token start (i + 1) (has_quote || text.[i] = '\'')
  in
  next_token 0

let assoc_field (name:string) (fields:(string * Yojson.Safe.t) list) : Yojson.Safe.t option =
  List.assoc_opt name fields

let response_result_uris (resp:Yojson.Safe.t) : string list =
  match resp with
  | `Assoc fields -> (
      match assoc_field "result" fields with
      | Some (`List xs) ->
          xs
          |> List.filter_map (function
               | `Assoc lf -> (
                   match assoc_field "uri" lf with
                   | Some (`String u) -> Some u
                   | _ -> None)
               | _ -> None)
      | _ -> [])
  | _ -> []

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

let uri_matches_hint ~(uri:string) ~(hint:string) : bool =
  let norm = Lsp_test_helpers.normalize_uri_for_compare uri in
  let hint_norm =
    hint
    |> String.trim
    |> String.lowercase_ascii
    |> String.map (fun c -> if c = '\\' then '/' else c)
  in
  hint_norm = "" || contains_substring ~haystack:norm ~needle:hint_norm

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

let warmup_responsive_probe
    ~(srv:Lsp_test_helpers.server_proc)
    ~(id:int)
    ~(uri:string)
    ~(timeout_s:float)
  : unit =
  let params =
    `Assoc
      [
        "command", `String "jovial.debugReport";
        "arguments", `List [ `String uri; `Int 24 ];
      ]
  in
  ignore
    (Lsp_test_helpers.request_timed
       srv
       ~id
       ~method_:"workspace/executeCommand"
       ~params
       ~timeout_s)

let wait_open_diagnostics
    ~(srv:Lsp_test_helpers.server_proc)
    ~(uri:string)
    ~(timeout_s:float)
  : unit =
  match Lsp_test_helpers.wait_for_publish_diagnostics_for_uri ~srv ~target_uri:uri ~timeout_s with
  | Some _ -> ()
  | None ->
      failf
        "did not receive publishDiagnostics for opened doc %s within %.2fs"
        uri
        timeout_s

let () =
  Random.self_init ();
  let server_path =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failf "missing server executable path argument"
  in
  let ws_path_opt =
    match Sys.getenv_opt "JOVIAL_TEST_EXTERNAL_WS_PATH" with
    | None -> None
    | Some raw ->
        let trimmed = String.trim raw in
        if trimmed = "" then None else Some trimmed
  in
  match ws_path_opt with
  | None ->
      print_endline
        "lsp_external_workspace_nav_convergence_test: skipped (set JOVIAL_TEST_EXTERNAL_WS_PATH to run)";
      exit 0
  | Some ws_path ->
      if not (is_dir ws_path) then
        failf "external workspace path is not a directory: %s" ws_path;
      let hard_timeout_s =
        float_of_int
          (max 1
             (Lsp_test_helpers.getenv_int
                "JOVIAL_TEST_EXTERNAL_NAV_HARD_TIMEOUT_S"
                ~default:900))
      in
      let first_timeout_s =
        max 0.5
          (match Sys.getenv_opt "JOVIAL_TEST_EXTERNAL_NAV_FIRST_TIMEOUT_S" with
           | None -> 2.0
           | Some raw -> (try float_of_string (String.trim raw) with _ -> 2.0))
      in
      let converge_timeout_s =
        max first_timeout_s
          (match Sys.getenv_opt "JOVIAL_TEST_EXTERNAL_NAV_CONVERGE_TIMEOUT_S" with
           | None -> 60.0
           | Some raw -> (try float_of_string (String.trim raw) with _ -> 60.0))
      in
      let open_diag_timeout_s =
        max 2.0
          (match Sys.getenv_opt "JOVIAL_TEST_EXTERNAL_NAV_OPEN_TIMEOUT_S" with
           | None -> 12.0
           | Some raw -> (try float_of_string (String.trim raw) with _ -> 12.0))
      in
      let target_hint =
        match Sys.getenv_opt "JOVIAL_TEST_EXTERNAL_NAV_TARGET_HINT" with
        | None -> ""
        | Some raw -> String.trim raw
      in
      let files = collect_source_files ws_path in
      if files = [] then
        failf "no .jov/.j73 files found in external workspace path: %s" ws_path;
      let main_path =
        match Sys.getenv_opt "JOVIAL_TEST_EXTERNAL_NAV_FILE" with
        | Some p when String.trim p <> "" -> String.trim p
        | _ -> largest_file files
      in
      if not (Sys.file_exists main_path) then
        failf "selected nav source file does not exist: %s" main_path;
      let main_text = read_text main_path in
      let needle =
        match Sys.getenv_opt "JOVIAL_TEST_EXTERNAL_NAV_NEEDLE" with
        | Some raw when String.trim raw <> "" -> String.trim raw
        | _ ->
            (match collect_call_like_tokens ~limit:16 main_text with
             | tok :: _ -> tok
             | [] -> (
                 match find_first_apostrophe_token main_text with
                 | Some tok -> tok
                 | None ->
                     print_endline
                       "lsp_external_workspace_nav_convergence_test: skipped (no call-like or apostrophe identifier token found; set JOVIAL_TEST_EXTERNAL_NAV_NEEDLE)";
                     exit 0))
      in
      let line, col =
        try Lsp_test_helpers.line_col_of_first main_text ~needle
        with _ ->
          failf "needle %S not found in selected file %s" needle main_path
      in
      let started = Unix.gettimeofday () in
      let remaining_budget () =
        hard_timeout_s -. (Unix.gettimeofday () -. started)
      in
      let ensure_budget (phase:string) =
        if remaining_budget () <= 0.0 then
          failf
            "external nav convergence test exceeded hard timeout (%.1fs) at %s"
            hard_timeout_s
            phase
      in
      let root_uri = Lsp_test_helpers.lsp_doc_uri_of_path ws_path in
      let doc_uri = Lsp_test_helpers.lsp_doc_uri_of_path main_path in
      let doc_norm = Lsp_test_helpers.normalize_uri_for_compare doc_uri in

      Lsp_test_helpers.with_server
        ~env:[ "JOVIAL_WORKSPACE_DIAGS_MODE", "off" ]
        ~server_path
        (fun srv ->
          ensure_budget "initialize/open";
          ignore
            (Lsp_test_helpers.initialize_and_open
               srv
               ~root_uri
               ~doc_uri
               ~doc_text:main_text
               ~timeout_s:(min 8.0 (max 2.0 (remaining_budget ()))));
          ensure_budget "open diagnostics";
          wait_open_diagnostics
            ~srv
            ~uri:doc_uri
            ~timeout_s:(min open_diag_timeout_s (max 2.0 (remaining_budget ())));

          ensure_budget "warmup probe";
          (try
             warmup_responsive_probe
               ~srv
               ~id:900
               ~uri:doc_uri
               ~timeout_s:(min 6.0 (max 1.0 (remaining_budget ())))
           with _ -> ());

          ensure_budget "first definition";
          let first_timeout = min first_timeout_s (max 0.5 (remaining_budget ())) in
          let first_resp_opt, first_ms =
            try
              let resp, ms =
                request_definition
                  ~srv
                  ~id:2
                  ~uri:doc_uri
                  ~line
                  ~character:(col + 1)
                  ~timeout_s:first_timeout
              in
              (Some resp, ms)
            with
            | Failure _ ->
                (None, first_timeout *. 1000.0)
          in
          let first_matches =
            match first_resp_opt with
            | None -> false
            | Some first_resp ->
                let first_result_uris = response_result_uris first_resp in
                List.exists
                  (fun u ->
                    let norm = Lsp_test_helpers.normalize_uri_for_compare u in
                    norm <> doc_norm && uri_matches_hint ~uri:u ~hint:target_hint)
                  first_result_uris
          in

          let converged =
            if first_matches then true
            else
              let deadline = Unix.gettimeofday () +. converge_timeout_s in
              let rec loop req_id =
                if Unix.gettimeofday () >= deadline then false
                else (
                  ensure_budget "definition convergence";
                  Thread.delay 0.12;
                  let uris =
                    try
                      let resp, _ =
                        request_definition
                          ~srv
                          ~id:req_id
                          ~uri:doc_uri
                          ~line
                          ~character:(col + 1)
                          ~timeout_s:(min 2.0 (max 0.5 (remaining_budget ())))
                      in
                      response_result_uris resp
                    with
                    | Failure _ ->
                        []
                  in
                  if List.exists
                       (fun u ->
                         let norm = Lsp_test_helpers.normalize_uri_for_compare u in
                         norm <> doc_norm && uri_matches_hint ~uri:u ~hint:target_hint)
                       uris
                  then true
                  else loop (req_id + 1))
              in
              loop 3
          in

          if not converged then
            failf
              "external nav did not converge from %s:%d:%d for needle %S within %.1fs"
              main_path
              line
              col
              needle
              converge_timeout_s;

          Lsp_test_helpers.shutdown_and_exit srv ~timeout_s:(min 8.0 (max 2.0 (remaining_budget ())));
          Lsp_test_helpers.close_stdin srv;
          ignore (Lsp_test_helpers.wait_for_exit srv ~timeout_s:6.0);

          print_endline
            (Printf.sprintf
               "lsp_external_workspace_nav_convergence_test: ok (file=%s needle=%s first=%.1fms)"
               main_path
               needle
               first_ms))
