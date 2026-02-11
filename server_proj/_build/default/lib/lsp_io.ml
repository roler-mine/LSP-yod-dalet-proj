let strip_cr (s : string) =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

let rec read_headers ic content_length =
  match input_line ic with
  | exception End_of_file -> None
  | line ->
      let line = strip_cr line in
      if line = "" then Some content_length
      else (
        let content_length =
          if String.length line >= 15 && String.sub line 0 15 = "Content-Length:" then
            let v = String.trim (String.sub line 15 (String.length line - 15)) in
            (try int_of_string v with _ -> content_length)
          else content_length
        in
        read_headers ic content_length
      )

let read_message (ic : in_channel) : string option =
  match read_headers ic 0 with
  | None -> None
  | Some len ->
      if len <= 0 then None
      else
        let buf = really_input_string ic len in
        Some buf

let write_message (oc : out_channel) (json : Yojson.Safe.t) : unit =
  let body = Yojson.Safe.to_string json in
  let len = String.length body in
  output_string oc ("Content-Length: " ^ string_of_int len ^ "\r\n\r\n");
  output_string oc body;
  flush oc
