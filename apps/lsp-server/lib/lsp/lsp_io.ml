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

type read_result =
  [ `Eof
  | `Message of string
  | `Oversize of int
  | `Invalid of string
  ]

type header_result =
  | HeaderEof
  | HeaderLength of int
  | HeaderInvalid of string

let read_headers ic : header_result =
  let rec loop ~(seen_any:bool) (len:int option) =
    match input_line ic with
    | exception End_of_file ->
        if seen_any then HeaderInvalid "unexpected EOF while reading LSP headers"
        else HeaderEof
    | line ->
        let line = strip_cr line in
        let parsed_len = parse_content_length_header line in
        if line = "" then
          (match len with
           | Some n when n > 0 -> HeaderLength n
           | Some n -> HeaderInvalid (Printf.sprintf "invalid Content-Length: %d" n)
           | None -> HeaderInvalid "missing Content-Length header")
        else
          let len =
            match parsed_len with
            | Some n when n >= 0 -> Some n
            | _ -> len
          in
          loop ~seen_any:true len
  in
  loop ~seen_any:false None

let discard_bytes (ic:in_channel) (len:int) : bool =
  let chunk = Bytes.create 8192 in
  let remaining = ref len in
  let ok = ref true in
  while !remaining > 0 && !ok do
    let want = min !remaining (Bytes.length chunk) in
    let got = input ic chunk 0 want in
    if got <= 0 then ok := false
    else remaining := !remaining - got
  done;
  !ok

let read_message_with_limit (ic:in_channel) ~(max_len:int) : read_result =
  match read_headers ic with
  | HeaderEof -> `Eof
  | HeaderInvalid msg -> `Invalid msg
  | HeaderLength len ->
      if len > max_len then
        if discard_bytes ic len then `Oversize len
        else `Invalid "unexpected EOF while discarding oversized frame"
      else
        (try `Message (really_input_string ic len)
         with End_of_file ->
           `Invalid "unexpected EOF while reading LSP body")

let read_message (ic : in_channel) : string option =
  match read_message_with_limit ic ~max_len:max_int with
  | `Message body -> Some body
  | `Eof | `Oversize _ | `Invalid _ -> None

let write_message (oc : out_channel) (json : Yojson.Safe.t) : unit =
  let body = Yojson.Safe.to_string json in
  let len = String.length body in
  output_string oc ("Content-Length: " ^ string_of_int len ^ "\r\n\r\n");
  output_string oc body
