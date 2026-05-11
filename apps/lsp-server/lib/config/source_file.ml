let default_extensions = [ ".jov"; ".j73"; ".jvl" ]

let is_extension_char = function
  | 'a' .. 'z' | '0' .. '9' | '_' -> true
  | _ -> false

let normalize_extension (raw : string) : string option =
  let s = String.trim raw |> String.lowercase_ascii in
  if s = "" then None
  else
    let s = if s.[0] = '.' then s else "." ^ s in
    if String.length s <= 1 then None
    else
      let rec valid i =
        if i >= String.length s then true
        else is_extension_char s.[i] && valid (i + 1)
      in
      if valid 1 then Some s else None

let normalize_extensions (extensions : string list) : string list =
  let seen = Hashtbl.create 8 in
  let out = ref [] in
  let add raw =
    match normalize_extension raw with
    | None -> ()
    | Some ext ->
        if not (Hashtbl.mem seen ext) then (
          Hashtbl.replace seen ext true;
          out := ext :: !out)
  in
  List.iter add extensions;
  List.rev !out

let with_defaults (extra_extensions : string list) : string list =
  normalize_extensions (default_extensions @ extra_extensions)

let has_extension ~(extensions : string list) (file : string) : bool =
  let lower = String.lowercase_ascii file in
  let n = String.length lower in
  List.exists
    (fun ext ->
      let m = String.length ext in
      n >= m && String.sub lower (n - m) m = ext)
    extensions

let strip_known_extension ~(extensions : string list) (file : string) : string =
  let lower = String.lowercase_ascii file in
  let n = String.length lower in
  match
    List.find_opt
      (fun ext ->
        let m = String.length ext in
        n >= m && String.sub lower (n - m) m = ext)
      extensions
  with
  | None -> file
  | Some ext -> String.sub file 0 (n - String.length ext)
