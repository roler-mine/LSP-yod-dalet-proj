open Unix
open Yojson.Safe
open Yojson.Safe.Util

(* Minimal JSON shell server for VSCode extension over stdio.
   Requires yojson package available at runtime. *)

let read_all ic =
  let buf = Buffer.create 1024 in
  (try while true do
     Buffer.add_channel buf ic 1024
   done with End_of_file -> ());
  Buffer.contents buf

let exit_code_of_status = function
  | WEXITED n -> n
  | WSIGNALED n -> 128 + n
  | WSTOPPED n -> 128 + n

let run_cmd cmd =
  let proc = open_process_full ("/bin/sh -c " ^ cmd) (environment ()) in
  let (out_ic, in_oc, err_ic) = proc in
  let stdout = read_all out_ic in
  let stderr = read_all err_ic in
  let status = close_process_full proc in
  (stdout, stderr, exit_code_of_status status)

(* Replace simple line-based send_json with LSP-framed sender *)
let send_lsp_message j =
  let body = Yojson.Safe.to_string j in
  let header = Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body) in
  output_string stdout header;
  output_string stdout body;
  flush stdout

(* Helper to read a single LSP message (headers + body) *)
let read_lsp_message () =
  try
    let rec read_headers acc =
      let line = input_line stdin in
      let line =
        if String.length line > 0 && line.[String.length line - 1] = '\r'
        then String.sub line 0 (String.length line - 1)
        else line
      in
      if line = "" then List.rev acc else read_headers (line :: acc)
    in
    let headers = read_headers [] in
    let find_header_prefix pref =
      let len = String.length pref in
      let rec aux = function
        | [] -> None
        | h :: t =>
            if String.length h >= len &&
               String.lowercase_ascii (String.sub h 0 len) = String.lowercase_ascii pref
            then Some h else aux t
      in
      aux headers
    in
    match find_header_prefix "Content-Length:" with
    | None -> None
    | Some h =>
        let v = String.trim (String.sub h (String.length "Content-Length:") (String.length h - String.length "Content-Length:")) in
        let len = int_of_string v in
        let body = really_input_string stdin len in
        Some body
  with End_of_file -> None

let make_response ?(id=`Null) result =
  `Assoc [("jsonrpc", `String "2.0"); ("id", id); ("result", result)]

let make_error_response ?(id=`Null) msg =
  `Assoc [("jsonrpc", `String "2.0"); ("id", id); ("error", `Assoc [("message", `String msg)])]

let handle_request json =
  try
    let id_json = match json |> member "id" with `Null -> None | v -> Some v in
    let meth = json |> member "method" |> to_string in
    match meth with
    | "initialize" ->
        let caps = `Assoc [] in
        let result = `Assoc [("capabilities", caps)] in
        send_lsp_message (make_response ?id:(id_json) result);
        let init_notif = `Assoc [("jsonrpc", `String "2.0"); ("method", `String "initialized"); ("params", `Assoc [])] in
        send_lsp_message init_notif
    | "exec" ->
        let cmd = json |> member "params" |> member "cmd" |> to_string in
        let (stdout_s, stderr_s, code) = run_cmd cmd in
        let result = `Assoc [
          ("stdout", `String stdout_s);
          ("stderr", `String stderr_s);
          ("code", `Int code)
        ] in
        send_lsp_message (make_response ?id:(id_json) result)
    | "shutdown" ->
        send_lsp_message (make_response ?id:(id_json) `Null);
        `Assoc [("action", `String "shutdown-requested")]
    | "exit" ->
        send_lsp_message (make_response ?id:(id_json) (`String "exiting"));
        exit 0
    | _ ->
        let msg = "unknown method: " ^ meth in
        send_lsp_message (make_error_response ?id:(id_json) msg);
        `Assoc [("action", `String "none")]
  with
  | Yojson.Json_error msg
  | Type_error (msg, _) 
  | Failure msg ->
      send_lsp_message (make_error_response ~id:(`String "0") ("parse/exec error: " ^ msg));
      `Assoc [("action", `String "none")]

let rec loop ~shutdown_requested =
  match read_lsp_message () with
  | None -> ()
  | Some body ->
      (try
         let json = Yojson.Safe.from_string body in
         let id_present = match json |> member "id" with `Null -> false | _ -> true in
         if id_present then
           let result = handle_request json in
           let shutdown_req =
             match result with
             | `Assoc [("action", `String "shutdown-requested")] -> true
             | _ -> shutdown_requested
           in
           loop ~shutdown_requested:shutdown_req
         else
           let meth = json |> member "method" |> to_string in
           if meth = "exit" then exit 0 else loop ~shutdown_requested
       with Yojson.Json_error _ -> loop ~shutdown_requested)

let () = let ready = `Assoc [("event", `String "ready")] in send_lsp_message ready;
  loop ~shutdown_requested:false
