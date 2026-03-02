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
  msg_q : Yojson.Safe.t Queue.t;
  msg_mtx : Mutex.t;
  mutable msg_error : string option;
  mutable msg_reader_done : bool;
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
  let inherit_stderr =
    match Sys.getenv_opt "JOVIAL_TEST_SERVER_STDERR" with
    | Some "1" | Some "true" | Some "TRUE" -> true
    | _ -> false
  in
  let parent_stderr = Unix.descr_of_out_channel stderr in
  let child_stderr_w =
    if inherit_stderr then parent_stderr
    else Unix.openfile null_path [Unix.O_WRONLY] 0o666
  in
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
  if child_stderr_w <> parent_stderr then Unix.close child_stderr_w;
  let stdin_w = Unix.out_channel_of_descr child_stdin_w in
  let stdout_r = Unix.in_channel_of_descr child_stdout_r in
  if Sys.win32 then (
    set_binary_mode_out stdin_w true;
    set_binary_mode_in stdout_r true
  );
  let msg_q : Yojson.Safe.t Queue.t = Queue.create () in
  let msg_mtx = Mutex.create () in
  let srv =
    {
      pid;
      stdin_w;
      stdout_r;
      msg_q;
      msg_mtx;
      msg_error = None;
      msg_reader_done = false;
    }
  in
  ignore (Thread.create (fun () ->
    let set_error msg =
      Mutex.lock srv.msg_mtx;
      if srv.msg_error = None then srv.msg_error <- Some msg;
      srv.msg_reader_done <- true;
      Mutex.unlock srv.msg_mtx
    in
    let rec loop () =
      match Lib.Lsp_io.read_message srv.stdout_r with
      | None ->
          set_error "server closed stdout while waiting for LSP message"
      | Some txt ->
          (match
             try Ok (Yojson.Safe.from_string txt)
             with exn ->
               Error (Printf.sprintf "invalid LSP JSON payload: %s" (Printexc.to_string exn))
           with
           | Ok msg ->
               Mutex.lock srv.msg_mtx;
               Queue.add msg srv.msg_q;
               Mutex.unlock srv.msg_mtx;
               loop ()
           | Error msg ->
               set_error msg)
    in
    (try loop () with exn ->
       set_error (Printf.sprintf "failed reading LSP message: %s" (Printexc.to_string exn)))
  ) ());
  srv

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
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    Mutex.lock srv.msg_mtx;
    let next_msg =
      if Queue.is_empty srv.msg_q then None else Some (Queue.pop srv.msg_q)
    in
    let err = srv.msg_error in
    let done_ = srv.msg_reader_done in
    Mutex.unlock srv.msg_mtx;
    match next_msg with
    | Some msg -> msg
    | None ->
        (match err with
         | Some msg -> failf "%s" msg
         | None ->
             if done_ then failf "server closed stdout while waiting for LSP message"
             else if Unix.gettimeofday () >= deadline then
               failf "timed out waiting for LSP message"
             else (
               Thread.delay 0.01;
               loop ()))
  in
  loop ()

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
