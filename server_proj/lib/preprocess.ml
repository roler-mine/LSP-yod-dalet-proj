module T = Lsp.Types

type import_kind = Compool

type import = {
  kind : import_kind;
  name : string;
  loc  : Ast.Loc.t;
}

type result = {
  text : string;
  imports : import list;
  compool_def : string option;
  diags : T.Diagnostic.t list;
}

let uppercase = String.uppercase_ascii

let contains_substring ~(haystack:string) ~(needle:string) : bool =
  let n = String.length haystack in
  let m = String.length needle in
  if m = 0 then true
  else if m > n then false
  else
    let rec go i =
      if i + m > n then false
      else if String.sub haystack i m = needle then true
      else go (i + 1)
    in
    go 0

let normalize_spaces (s:string) : string =
  s |> String.map (fun c -> if c = '\t' then ' ' else c)

(* 1-based line numbers in Ast.Loc, columns 0-based; offset unused here *)
let mk_loc ~(file:string option) ~(line:int) ~(col0:int) ~(col1:int) : Ast.Loc.t =
  let mkp col : Ast.Loc.pos = { Ast.Loc.line = line; col; offset = 0 } in
  { Ast.Loc.file = file; start_pos = mkp col0; end_pos = mkp col1 }

let extract_quoted (line:string) : (string * (int*int)) list =
  (* returns list of (contents, (start_col, end_col_exclusive)) for each '...' *)
  let n = String.length line in
  let rec loop i acc =
    if i >= n then List.rev acc
    else if line.[i] = '\'' then (
      let j = ref (i + 1) in
      while !j < n && line.[!j] <> '\'' do incr j done;
      if !j < n then
        let contents = String.sub line (i + 1) (!j - (i + 1)) in
        loop (!j + 1) ((contents, (i, !j + 1)) :: acc)
      else
        List.rev acc
    ) else
      loop (i + 1) acc
  in
  loop 0 []

let is_ident_char = function
  | 'A'..'Z' | '0'..'9' | '_' | '$' | '\'' -> true
  | _ -> false

let uppercase = String.uppercase_ascii
let normalize_spaces s = String.map (fun c -> if c = '\t' then ' ' else c) s

let take_ident_from (s:string) (i:int) : string option =
  let n = String.length s in
  let j = ref i in
  while !j < n && s.[!j] = ' ' do incr j done;
  if !j >= n || not (is_ident_char s.[!j]) then None
  else (
    let k = ref !j in
    while !k < n && is_ident_char s.[!k] do incr k done;
    Some (String.sub s !j (!k - !j))
  )

let contains_substring ~(haystack:string) ~(needle:string) : bool =
  let n = String.length haystack and m = String.length needle in
  let rec go i =
    if i + m > n then false
    else if String.sub haystack i m = needle then true
    else go (i+1)
  in
  m = 0 || go 0

let scan_compool_def ~(text:string) : string option =
  let lines = String.split_on_char '\n' text in
  let rec go = function
    | [] -> None
    | line :: rest ->
        let u = uppercase (normalize_spaces line) in
        (* ignore directive lines/segments *)
        if contains_substring ~haystack:u ~needle:"!COMPOOL"
           || contains_substring ~haystack:u ~needle:"ICOMPOOL"
        then go rest
        else
          (* look for "COMPOOL <ident>" *)
          let rec find_at i =
            if i + 6 > String.length u then None
            else if String.sub u i 6 = "COMPOOL" then
              take_ident_from u (i+6)
            else find_at (i+1)
          in
          match find_at 0 with
          | Some nm when String.trim nm <> "" -> Some (String.trim nm)
          | _ -> go rest
  in
  go lines


let run ~(file:string option) ~(text:string) : result =
  let lines = String.split_on_char '\n' text in
  let compool_def = scan_compool_def ~text in

  let imports =
    lines
    |> List.mapi (fun idx line -> (idx + 1, line))
    |> List.fold_left (fun acc (line_no, line) ->
         let u = uppercase (normalize_spaces line) in
         if contains_substring ~haystack:u ~needle:"ICOMPOOL" then (
           (* Priority 1: quoted imports ICOMPOOL 'NAME'; or ICOMPOOL ('A','B'); *)
           let qs = extract_quoted line in
           let from_quotes =
             qs
             |> List.map (fun (nm, (c0, c1)) ->
                  let name = uppercase (String.trim nm) in
                  let loc = mk_loc ~file ~line:line_no ~col0:c0 ~col1:c1 in
                  { kind = Compool; name; loc })
           in
           if from_quotes <> [] then from_quotes @ acc
           else (
             (* Priority 2: unquoted variant: ICOMPOOL NAME; *)
             match
               let i =
                 (* find start index of ICOMPOOL to look after it *)
                 let needle = "ICOMPOOL" in
                 let rec scan j =
                   if j + String.length needle > String.length u then None
                   else if String.sub u j (String.length needle) = needle then Some (j + String.length needle)
                   else scan (j + 1)
                 in
                 scan 0
               in
               match i with
               | None -> None
               | Some j -> take_ident_from u j
             with
             | None -> acc
             | Some nm ->
                 let name = uppercase (String.trim nm) in
                 let c0 = 0 and c1 = min (String.length line) (String.length line) in
                 let loc = mk_loc ~file ~line:line_no ~col0:c0 ~col1:c1 in
                 { kind = Compool; name; loc } :: acc
           )
         ) else acc
       ) []
    |> List.rev
  in

  { text; imports; compool_def; diags = [] }
