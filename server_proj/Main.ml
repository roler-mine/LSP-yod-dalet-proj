open Unix
open Yojson.Safe.Util

(* Minimal JSON shell server for VSCode extension over stdio. *)

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
  let proc = open_process_full ("cmd /c " ^ cmd) (environment ()) in
  let (out_ic, in_oc, err_ic) = proc in
  close_out in_oc; 
  let stdout_str = read_all out_ic in
  let stderr_str = read_all err_ic in
  let status = close_process_full proc in
  (stdout_str, stderr_str, exit_code_of_status status)

(* --- NEW: Parsing Logic --- *)
let do_parse (text : string) =
  let lexbuf = Lexing.from_string text in
  (* Initialize position tracking *)
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_lnum = 1; Lexing.pos_bol = 0 };
  
  try
    (* Call the parser entry point defined in parser.mly *)
    let _ast = Parser.compilation_unit Lexer.token lexbuf in
    `Assoc [("status", `String "success"); ("message", `String "Parsed successfully!")]
  with
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol in
      let msg = Printf.sprintf "Syntax error at line %d, column %d" line col in
      `Assoc [("status", `String "error"); ("message", `String msg)]
  | Lexer.Internal_lexer_bug msg ->
       `Assoc [("status", `String "error"); ("message", `String ("Lexer error: " ^ msg))]
  | Failure msg ->
       `Assoc [("status", `String "error"); ("message", `String ("Failure: " ^ msg))]
  | exn ->
       `Assoc [("status", `String "error"); ("message", `String ("Unknown error: " ^ Printexc.to_string exn))]

(* --- LSP Communication --- *)

let send_lsp_message j = 
  let body = Yojson.Safe.to_string j in
  let header = Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body) in
  output_string Stdlib.stdout header;
  output_string Stdlib.stdout body;
  flush Stdlib.stdout

let read_lsp_message () = 
  try
    let rec read_headers acc = 
      let line = input_line Stdlib.stdin in
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
        | h :: t -> 
            if String.length h >= len &&
               String.lowercase_ascii (String.sub h 0 len) = String.lowercase_ascii pref
            then Some h else aux t
      in 
      aux headers
    in 
    match find_header_prefix "Content-Length:" with 
    | None -> None
    | Some h -> 
        let v = String.trim (String.sub h (String.length "Content-Length:") (String.length h - String.length "Content-Length:")) in
        let len = int_of_string v in 
        let body = really_input_string Stdlib.stdin len in 
        Some body
  with End_of_file -> None

let make_response ?(id=`Null) result = 
  `Assoc [("jsonrpc", `String "2.0");
          ("id", id); ("result", result)]

let make_error_response ?(id=`Null) msg = 
  `Assoc [("jsonrpc", `String "2.0"); ("id", id);
          ("error", `Assoc [("message", `String msg)])]

let handle_request json = 
  try
    let id_json = match json |> member "id" with `Null -> None | v -> Some v in
    let meth = json |> member "method" |> to_string in
    match meth with 
    | "initialize" -> 
        let caps = `Assoc [] in 
        let result = `Assoc [("capabilities", caps)] in 
        send_lsp_message (make_response ?id:(id_json) result);
        let init_notif = `Assoc [("jsonrpc", `String "2.0"); ("method", `String "initialized");
                                 ("params", `Assoc [])] in 
        send_lsp_message init_notif;
        `Assoc [("action", `String "none")]
    
    (* NEW: Parse command *)
    | "parse" ->
        let text = json |> member "params" |> member "text" |> to_string in
        let result = do_parse text in
        send_lsp_message (make_response ?id:(id_json) result);
        `Assoc [("action", `String "none")]

    | "exec" -> 
        let cmd = json |> member "params" |> member "cmd" |> to_string in 
        let (stdout_s, stderr_s, code) = run_cmd cmd in 
        let result = `Assoc [
          ("stdout", `String stdout_s);
          ("stderr", `String stderr_s);
          ("code", `Int code)
        ] in 
        send_lsp_message (make_response ?id:(id_json) result);
        `Assoc [("action", `String "none")]
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

let () = 
  let ready = `Assoc [("event", `String "ready")] in 
  send_lsp_message ready;
  loop ~shutdown_requested:false