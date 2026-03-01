module T = Lsp.Types

let starts_with ~(s:string) ~(prefix:string) : bool =
  let n = String.length s and m = String.length prefix in
  n >= m && String.sub s 0 m = prefix

let drop_prefix ~(s:string) ~(prefix:string) : string option =
  if starts_with ~s ~prefix then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else
    None

let hex_val = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> -1

let pct_decode (s:string) : string =
  let n = String.length s in
  let b = Buffer.create n in
  let rec loop i =
    if i >= n then ()
    else if s.[i] = '%' && i + 2 < n then
      let a = hex_val s.[i + 1] and c = hex_val s.[i + 2] in
      if a >= 0 && c >= 0 then (
        Buffer.add_char b (Char.chr (a * 16 + c));
        loop (i + 3)
      ) else (
        Buffer.add_char b s.[i];
        loop (i + 1)
      )
    else (
      Buffer.add_char b s.[i];
      loop (i + 1)
    )
  in
  loop 0;
  Buffer.contents b

let is_unreserved = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' -> true
  | _ -> false

let pct_encode_path (s:string) : string =
  let b = Buffer.create (String.length s) in
  let emit_hex x =
    let digits = "0123456789ABCDEF" in
    Buffer.add_char b digits.[(x lsr 4) land 0xF];
    Buffer.add_char b digits.[x land 0xF]
  in
  String.iter (fun c ->
    if is_unreserved c || c = '/' || c = ':' then
      Buffer.add_char b c
    else (
      Buffer.add_char b '%';
      emit_hex (Char.code c)
    )
  ) s;
  Buffer.contents b

let docuri_to_string (u:T.DocumentUri.t) : string =
  try
    match T.DocumentUri.yojson_of_t u with
    | `String s -> s
    | _ -> ""
  with _ -> ""

let docuri_of_string (s:string) : T.DocumentUri.t option =
  try Some (T.DocumentUri.t_of_yojson (`String s)) with _ -> None

let is_drive_path (s:string) : bool =
  String.length s >= 2
  && ((s.[0] >= 'A' && s.[0] <= 'Z') || (s.[0] >= 'a' && s.[0] <= 'z'))
  && s.[1] = ':'

let split_authority_path (rest:string) : string * string =
  if rest = "" then ("", "")
  else if rest.[0] = '/' then ("", rest)
  else
    match String.index_opt rest '/' with
    | None -> (rest, "")
    | Some i ->
        let a = String.sub rest 0 i in
        let p = String.sub rest i (String.length rest - i) in
        (a, p)

let file_path_of_uri_string (u:string) : string option =
  match drop_prefix ~s:u ~prefix:"file://" with
  | None -> None
  | Some rest ->
      let authority, raw_path = split_authority_path rest in
      let dec_authority = pct_decode authority in
      let dec_path = pct_decode raw_path in
      if Sys.win32 then (
        if dec_authority <> "" && dec_authority <> "localhost" then
          let p = String.map (fun c -> if c = '/' then '\\' else c) dec_path in
          Some ("\\\\" ^ dec_authority ^ p)
        else
          let p =
            if String.length dec_path >= 3
               && dec_path.[0] = '/'
               && is_drive_path (String.sub dec_path 1 (String.length dec_path - 1))
            then String.sub dec_path 1 (String.length dec_path - 1)
            else dec_path
          in
          Some (String.map (fun c -> if c = '/' then '\\' else c) p)
      ) else (
        if dec_authority <> "" && dec_authority <> "localhost" then
          Some ("/" ^ dec_authority ^ dec_path)
        else
          Some dec_path
      )

let file_path_of_uri (u:T.DocumentUri.t) : string option =
  file_path_of_uri_string (docuri_to_string u)

let file_uri_of_path (path:string) : string =
  let p0 = String.map (fun c -> if c = '\\' then '/' else c) path in
  let p1 =
    if Sys.win32 then (
      if starts_with ~s:p0 ~prefix:"//" then p0
      else if is_drive_path p0 then "/" ^ p0
      else if starts_with ~s:p0 ~prefix:"/" then p0
      else "/" ^ p0
    ) else if starts_with ~s:p0 ~prefix:"/" then p0 else "/" ^ p0
  in
  "file://" ^ pct_encode_path p1

let docuri_of_path (path:string) : T.DocumentUri.t option =
  docuri_of_string (file_uri_of_path path)
