let strip_cr (s : string) =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

let lowercase_ascii (s:string) : string =
  String.map (fun c ->
    if c >= 'A' && c <= 'Z' then Char.chr (Char.code c + 32) else c
  ) s

let parse_content_length_header (line:string) : int option =
  match String.index_opt line ':' with
  | None -> None
  | Some i ->
      let key = String.sub line 0 i |> String.trim |> lowercase_ascii in
      if key <> "content-length" then None
      else
        let v = String.sub line (i + 1) (String.length line - i - 1) |> String.trim in
        try Some (int_of_string v) with _ -> None

let read_headers ic : int option =
  let rec loop (len:int option) =
    match input_line ic with
    | exception End_of_file -> None
    | line ->
        let line = strip_cr line in
        if line = "" then len
        else
          let len =
            match parse_content_length_header line with
            | Some n when n >= 0 -> Some n
            | _ -> len
          in
          loop len
  in
  loop None

let read_message (ic : in_channel) : string option =
  match read_headers ic with
  | None -> None
  | Some len ->
      if len <= 0 then None
      else
        Some (really_input_string ic len)

let write_message (oc : out_channel) (json : Yojson.Safe.t) : unit =
  let body = Yojson.Safe.to_string json in
  let len = String.length body in
  output_string oc ("Content-Length: " ^ string_of_int len ^ "\r\n\r\n");
  output_string oc body
