module Lib = Jovial_lsp_lib

let failf fmt = Printf.ksprintf failwith fmt

let getenv_int (name:string) ~(default:int) : int =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw ->
      (try int_of_string (String.trim raw) with _ -> default)

let sleep_seconds (secs:float) : unit =
  ignore (Unix.select [] [] [] secs)

let mk_temp_dir (prefix:string) : string =
  let root = Filename.get_temp_dir_name () in
  let rec pick attempts =
    if attempts <= 0 then failf "failed to create temporary directory for %s" prefix;
    let name =
      Printf.sprintf "%s-%d-%06x"
        prefix
        (Unix.getpid ())
        (Random.bits () land 0xFFFFFF)
    in
    let path = Filename.concat root name in
    try
      Unix.mkdir path 0o755;
      path
    with _ ->
      pick (attempts - 1)
  in
  pick 32

let ensure_dir (path:string) : unit =
  if Sys.file_exists path then ()
  else Unix.mkdir path 0o755

let write_text (path:string) (text:string) : unit =
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc

let json_assoc_find (k:string) (fields:(string * Yojson.Safe.t) list) : Yojson.Safe.t =
  match List.assoc_opt k fields with
  | Some v -> v
  | None -> failf "missing JSON key %S" k

let has_non_null_result (resp:Yojson.Safe.t) : bool =
  match resp with
  | `Assoc fields -> (
      match List.assoc_opt "result" fields with
      | Some `Null | None -> false
      | _ -> true)
  | _ -> false

type server_proc = {
  pid : int;
  stdin_w : out_channel;
  stdout_r : in_channel;
}

let env_with_overrides (overrides:(string * string) list) : string array =
  let tbl = Hashtbl.create 128 in
  Array.iter (fun kv ->
    match String.index_opt kv '=' with
    | None -> ()
    | Some idx ->
        let key = String.sub kv 0 idx in
        let value = String.sub kv (idx + 1) (String.length kv - idx - 1) in
        Hashtbl.replace tbl key value
  ) (Unix.environment ());
  List.iter (fun (k, v) -> Hashtbl.replace tbl k v) overrides;
  Hashtbl.fold (fun k v acc -> (k ^ "=" ^ v) :: acc) tbl []
  |> Array.of_list

let start_server ~(server_path:string) ~(env:(string * string) list) : server_proc =
  let child_stdin_r, child_stdin_w = Unix.pipe () in
  let child_stdout_r, child_stdout_w = Unix.pipe () in
  let null_path = if Sys.win32 then "NUL" else "/dev/null" in
  let child_stderr_w = Unix.openfile null_path [Unix.O_WRONLY] 0o666 in
  let argv = [| server_path |] in
  let envp = env_with_overrides env in
  let pid =
    Unix.create_process_env
      server_path
      argv
      envp
      child_stdin_r
      child_stdout_w
      child_stderr_w
  in
  Unix.close child_stdin_r;
  Unix.close child_stdout_w;
  Unix.close child_stderr_w;
  let stdin_w = Unix.out_channel_of_descr child_stdin_w in
  let stdout_r = Unix.in_channel_of_descr child_stdout_r in
  if Sys.win32 then (
    set_binary_mode_out stdin_w true;
    set_binary_mode_in stdout_r true
  );
  { pid; stdin_w; stdout_r }

let send_json (srv:server_proc) (j:Yojson.Safe.t) : unit =
  Lib.Lsp_io.write_message srv.stdin_w j;
  flush srv.stdin_w

let send_request (srv:server_proc) ~(id:int) ~(method_:string) ~(params:Yojson.Safe.t) : unit =
  send_json srv
    (`Assoc [
      "jsonrpc", `String "2.0";
      "id", `Int id;
      "method", `String method_;
      "params", params;
    ])

let send_notification (srv:server_proc) ~(method_:string) ~(params:Yojson.Safe.t) : unit =
  send_json srv
    (`Assoc [
      "jsonrpc", `String "2.0";
      "method", `String method_;
      "params", params;
    ])

let wait_for_message (srv:server_proc) ~(timeout_s:float) : Yojson.Safe.t =
  let lock = Mutex.create () in
  let result : (Yojson.Safe.t, string) result option ref = ref None in
  let _reader_thread =
    Thread.create (fun () ->
      let parsed =
        try
          match Lib.Lsp_io.read_message srv.stdout_r with
          | None ->
              Error "server closed stdout while waiting for LSP message"
          | Some txt ->
              (try Ok (Yojson.Safe.from_string txt)
               with exn ->
                 Error (Printf.sprintf "invalid LSP JSON payload: %s" (Printexc.to_string exn)))
        with exn ->
          Error (Printf.sprintf "failed reading LSP message: %s" (Printexc.to_string exn))
      in
      Mutex.lock lock;
      result := Some parsed;
      Mutex.unlock lock
    ) ()
  in
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec wait_loop () =
    Mutex.lock lock;
    let current = !result in
    Mutex.unlock lock;
    match current with
    | Some (Ok j) ->
        j
    | Some (Error msg) ->
        failf "%s" msg
    | None ->
        if Unix.gettimeofday () >= deadline then
          failf "timed out waiting for LSP message";
        Thread.delay 0.01;
        wait_loop ()
  in
  wait_loop ()

let int_of_json_id (j:Yojson.Safe.t) : int option =
  match j with
  | `Int n -> Some n
  | `Intlit s ->
      (try Some (int_of_string s) with _ -> None)
  | _ -> None

let wait_for_response (srv:server_proc) ~(id:int) ~(timeout_s:float) : Yojson.Safe.t =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then
      failf "timed out waiting for response id=%d" id;
    let msg = wait_for_message srv ~timeout_s:remaining in
    match msg with
    | `Assoc fields ->
        (match List.assoc_opt "id" fields with
         | Some id_json ->
             (match int_of_json_id id_json with
              | Some got when got = id -> msg
              | _ -> loop ())
         | None ->
             loop ())
    | _ ->
        loop ()
  in
  loop ()

let request_timed
    (srv:server_proc)
    ~(id:int)
    ~(method_:string)
    ~(params:Yojson.Safe.t)
    ~(timeout_s:float)
  : Yojson.Safe.t * float =
  let t0 = Unix.gettimeofday () in
  send_request srv ~id ~method_ ~params;
  let resp = wait_for_response srv ~id ~timeout_s in
  let elapsed_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  if elapsed_ms > (timeout_s *. 1000.0) then
    failf
      "request %s(id=%d) exceeded timeout: %.1fms > %.1fms"
      method_
      id
      elapsed_ms
      (timeout_s *. 1000.0);
  (resp, elapsed_ms)

let initialize_and_open
    (srv:server_proc)
    ~(root_uri:string)
    ~(doc_uri:string)
    ~(doc_text:string)
    ~(timeout_s:float)
  : float =
  let init_params =
    `Assoc [
      "processId", `Null;
      "rootUri", `String root_uri;
      "capabilities", `Assoc [];
    ]
  in
  let (init_resp, init_ms) =
    request_timed srv ~id:1 ~method_:"initialize" ~params:init_params ~timeout_s
  in
  if not (has_non_null_result init_resp) then
    failf "initialize returned null/empty result";
  send_notification srv ~method_:"initialized" ~params:(`Assoc []);
  send_notification srv ~method_:"textDocument/didOpen"
    ~params:(`Assoc [
      "textDocument",
      `Assoc [
        "uri", `String doc_uri;
        "languageId", `String "jovial";
        "version", `Int 1;
        "text", `String doc_text;
      ];
    ]);
  init_ms

let shutdown_and_exit (srv:server_proc) ~(timeout_s:float) : unit =
  ignore
    (request_timed srv ~id:9901 ~method_:"shutdown" ~params:`Null ~timeout_s);
  send_notification srv ~method_:"exit" ~params:`Null

let close_stdin (srv:server_proc) : unit =
  close_out_noerr srv.stdin_w

let wait_for_exit (srv:server_proc) ~(timeout_s:float) : Unix.process_status =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    match Unix.waitpid [Unix.WNOHANG] srv.pid with
    | 0, _ ->
        if Unix.gettimeofday () >= deadline then
          failf "server pid=%d did not exit within %.2fs" srv.pid timeout_s;
        sleep_seconds 0.02;
        loop ()
    | _, st ->
        st
  in
  loop ()

let force_kill (srv:server_proc) : unit =
  (try Unix.kill srv.pid Sys.sigterm with _ -> ());
  sleep_seconds 0.05;
  (try Unix.kill srv.pid Sys.sigkill with _ -> ())

let with_server ?(env:(string * string) list = []) ~(server_path:string) (f:server_proc -> 'a) : 'a =
  let srv = start_server ~server_path ~env in
  try
    let out = f srv in
    out
  with exn ->
    (try close_out_noerr srv.stdin_w with _ -> ());
    (try ignore (wait_for_exit srv ~timeout_s:1.0) with _ ->
       force_kill srv;
       (try ignore (wait_for_exit srv ~timeout_s:0.5) with _ -> ()));
    (try close_in_noerr srv.stdout_r with _ -> ());
    raise exn

let lsp_doc_uri_of_path (path:string) : string =
  Lib.Uri_path.file_uri_of_path path

let line_col_of_first (text:string) ~(needle:string) : int * int =
  let n = String.length text in
  let m = String.length needle in
  if m = 0 then failf "needle must not be empty";
  let rec find i =
    if i + m > n then failf "needle %S not found" needle
    else if String.sub text i m = needle then i
    else find (i + 1)
  in
  let off = find 0 in
  let rec lc i line col =
    if i >= off then (line, col)
    else if text.[i] = '\n' then lc (i + 1) (line + 1) 0
    else lc (i + 1) line (col + 1)
  in
  lc 0 0 0

let definition_request_params ~(uri:string) ~(line:int) ~(character:int) : Yojson.Safe.t =
  `Assoc [
    "textDocument", `Assoc [ "uri", `String uri ];
    "position", `Assoc [ "line", `Int line; "character", `Int character ];
  ]
