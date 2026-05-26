(* Module overview: Lightweight assembly label scanning for Jovial procedure linkage. *)

open Workspace_foundation

type label_hit = asm_label_hit = {
  label_name : string;
  label_key : string;
  label_path : string;
  label_loc : Ast.Loc.t;
  label_source : asm_label_source;
}

type compool_import = {
  compool_name : string;
  compool_key : string;
  compool_path : string;
  compool_loc : Ast.Loc.t;
}

let normalize_name (s : string) : string =
  String.uppercase_ascii (String.trim s)

let is_ident_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' | '$' -> true
  | _ -> false

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' | '\'' -> true
  | _ -> false

let trim_cr (s : string) : string =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

let label_marker (s : string) : bool =
  match normalize_name s with
  | "PROC" | "CSECT" | "ENTRY" | "EQU" | "START" | "EXPORT" | "GLOBAL"
  | "PUBLIC" ->
      true
  | _ -> false

let export_directive (s : string) : bool =
  match normalize_name s with
  | "EXTDEF" | "XDEF" | "EXPORT" | "GLOBAL" | "PUBLIC" -> true
  | _ -> false

let token_chars line i =
  let n = String.length line in
  let stop j =
    j >= n
    ||
    match line.[j] with
    | ' ' | '\t' | '\r' | '\n' | '(' | ')' | ',' | ';' -> true
    | _ -> false
  in
  let j = ref i in
  while not (stop !j) do
    incr j
  done;
  (String.sub line i (!j - i), !j)

let tokens_of_line (line : string) : (string * int * int) list =
  let n = String.length line in
  let rec loop i acc =
    if i >= n then List.rev acc
    else
      match line.[i] with
      | ';' | '#' -> List.rev acc
      | ' ' | '\t' | '\r' | '\n' | '(' | ')' | ',' -> loop (i + 1) acc
      | _ ->
          let tok, j = token_chars line i in
          loop j ((tok, i, j) :: acc)
  in
  loop 0 []

let canonical_directive_name (s : string) = Preprocess.canonical_directive_name s

let compool_import_of_line ~(file : string option) ~(line_no0 : int)
    ~(line_start : int) (raw_line : string) : compool_import option =
  let line = trim_cr raw_line in
  let tokens = tokens_of_line line in
  let rec marker = function
    | [] -> None
    | (raw, c0, _c1) :: rest ->
        let name = canonical_directive_name raw in
        if name = "COMPOOL" then Some (c0, rest)
        else marker rest
  in
  match marker tokens with
  | None -> None
  | Some (_marker_col, (raw_compool, c0, c1) :: _) ->
      let compool_name =
        raw_compool |> String.trim |> fun s ->
        let n = String.length s in
        if
          n >= 2
          && ((s.[0] = '\'' && s.[n - 1] = '\'')
             || (s.[0] = '"' && s.[n - 1] = '"'))
        then String.sub s 1 (n - 2)
        else s
      in
      let compool_key = normalize_name compool_name in
      if compool_key = "" then None
      else
        let start_pos =
          { Ast.Loc.line = line_no0 + 1; col = c0; offset = line_start + c0 }
        in
        let end_pos =
          { Ast.Loc.line = line_no0 + 1; col = c1; offset = line_start + c1 }
        in
        Some
          {
            compool_name;
            compool_key;
            compool_path = Option.value file ~default:"";
            compool_loc = Ast.Loc.make ~file ~start_pos ~end_pos;
          }
  | Some (_, []) -> None

let first_word_from (line : string) (i : int) : string option =
  let n = String.length line in
  let rec skip j =
    if j < n && (line.[j] = ' ' || line.[j] = '\t') then skip (j + 1) else j
  in
  let i = skip i in
  if i >= n || not (is_ident_start line.[i]) then None
  else
    let j = ref (i + 1) in
    while !j < n && is_ident_char line.[!j] do
      incr j
    done;
    Some (String.sub line i (!j - i))

let label_of_line ~(file : string option) ~(line_no0 : int) ~(line_start : int)
    (raw_line : string) : label_hit option =
  let line = trim_cr raw_line in
  let n = String.length line in
  let rec skip_ws i =
    if i < n && (line.[i] = ' ' || line.[i] = '\t') then skip_ws (i + 1) else i
  in
  let c0 = skip_ws 0 in
  if c0 >= n || line.[c0] = ';' || line.[c0] = '#' then None
  else if not (is_ident_start line.[c0]) then None
  else
    let j = ref (c0 + 1) in
    while !j < n && is_ident_char line.[!j] do
      incr j
    done;
    let name = String.sub line c0 (!j - c0) in
    let has_colon = !j < n && line.[!j] = ':' in
    let marker =
      if has_colon then true
      else c0 = 0
           &&
           match first_word_from line !j with
           | Some word -> label_marker word
           | None -> false
    in
    if not marker then None
    else
      let start_pos =
        { Ast.Loc.line = line_no0 + 1; col = c0; offset = line_start + c0 }
      in
      let end_pos =
        { Ast.Loc.line = line_no0 + 1; col = !j; offset = line_start + !j }
      in
      let loc = Ast.Loc.make ~file ~start_pos ~end_pos in
      Some
        {
          label_name = name;
          label_key = normalize_name name;
          label_path = Option.value file ~default:"";
          label_loc = loc;
          label_source = AsmConcreteLabel;
        }

let export_directive_hits_of_line ~(file : string option) ~(line_no0 : int)
    ~(line_start : int) (raw_line : string) : label_hit list =
  let line = trim_cr raw_line in
  let tokens = tokens_of_line line in
  let rec after_marker = function
    | [] -> []
    | (raw, _c0, _c1) :: rest ->
        if export_directive raw then rest else after_marker rest
  in
  after_marker tokens
  |> List.filter_map (fun (raw, c0, c1) ->
         let name = String.trim raw in
         if name = "" || not (is_ident_start name.[0]) then None
         else
           let start_pos =
             { Ast.Loc.line = line_no0 + 1; col = c0; offset = line_start + c0 }
           in
           let end_pos =
             { Ast.Loc.line = line_no0 + 1; col = c1; offset = line_start + c1 }
           in
           let loc = Ast.Loc.make ~file ~start_pos ~end_pos in
           Some
             {
               label_name = name;
               label_key = normalize_name name;
               label_path = Option.value file ~default:"";
               label_loc = loc;
               label_source = AsmExportDirective;
             })

let read_file_text (path : string) : string option =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        Some (really_input_string ic len))
  with _ -> None

let compool_imports_in_text ~(path : string) ~(text : string) :
    compool_import list =
  let file = Some path in
  let lines = String.split_on_char '\n' text in
  let _line_no0, _offset, imports =
    List.fold_left
      (fun (line_no0, offset, acc) raw_line ->
        let hit =
          compool_import_of_line ~file ~line_no0 ~line_start:offset raw_line
        in
        let next_offset = offset + String.length raw_line + 1 in
        ( line_no0 + 1,
          next_offset,
          match hit with
          | Some h -> { h with compool_path = path } :: acc
          | None -> acc ))
      (0, 0, []) lines
  in
  List.rev imports

let unique_strings (xs : string list) : string list =
  let seen = Hashtbl.create (max 16 (List.length xs)) in
  let out = ref [] in
  List.iter
    (fun x ->
      if x <> "" && not (Hashtbl.mem seen x) then (
        Hashtbl.replace seen x true;
        out := x :: !out))
    xs;
  List.rev !out

let file_index_in_text ~(path : string) ~(text : string) : asm_file_index =
  let file = Some path in
  let lines = String.split_on_char '\n' text in
  let _line_no0, _offset, labels, exports =
    List.fold_left
      (fun (line_no0, offset, labels, exports) raw_line ->
        let label_hit =
          label_of_line ~file ~line_no0 ~line_start:offset raw_line
        in
        let export_hits =
          export_directive_hits_of_line ~file ~line_no0 ~line_start:offset
            raw_line
        in
        let next_offset = offset + String.length raw_line + 1 in
        ( line_no0 + 1,
          next_offset,
          (match label_hit with
          | Some h -> { h with label_path = path } :: labels
          | None -> labels),
          List.rev_append
            (List.map (fun hit -> { hit with label_path = path }) export_hits)
            exports ))
      (0, 0, [], []) lines
  in
  let imports =
    compool_imports_in_text ~path ~text
    |> List.map (fun (imp : compool_import) -> imp.compool_key)
    |> unique_strings
  in
  {
    asm_path = path;
    asm_path_key = Uri_path.normalize_path_key path;
    asm_import_keys = imports;
    asm_labels = List.rev labels;
    asm_export_directives = List.rev exports;
    asm_content_hash = Digest.to_hex (Digest.string text);
  }

let visible_for_compools ~(visible_compools : string list)
    (imports : compool_import list) : bool =
  match imports with
  | [] -> true
  | _ ->
      let visible = Hashtbl.create (max 16 (List.length visible_compools)) in
      List.iter
        (fun c ->
          let k = normalize_name c in
          if k <> "" then Hashtbl.replace visible k true)
        visible_compools;
      List.exists
        (fun imp -> Hashtbl.mem visible imp.compool_key)
        imports

let visible_for_import_keys ~(visible_compools : string list)
    (imports : string list) : bool =
  match imports with
  | [] -> true
  | _ ->
      let visible = Hashtbl.create (max 16 (List.length visible_compools)) in
      List.iter
        (fun c ->
          let k = normalize_name c in
          if k <> "" then Hashtbl.replace visible k true)
        visible_compools;
      List.exists (fun key -> Hashtbl.mem visible key) imports

let add_label_hit tbl (hit : asm_label_hit) =
  let prev = Option.value (Hashtbl.find_opt tbl hit.label_key) ~default:[] in
  Hashtbl.replace tbl hit.label_key (hit :: prev)

let rebuild_label_index (ws : t) : unit =
  Hashtbl.clear ws.asm_index_by_path;
  Hashtbl.clear ws.asm_label_hits_by_key;
  let path_keys =
    ws.assembly_file_paths
    |> List.filter_map (fun path ->
           let key = Uri_path.normalize_path_key path in
           if key = "" then None else Some key)
    |> List.sort_uniq String.compare
  in
  let seen_paths = Hashtbl.create (max 16 (List.length ws.assembly_file_paths)) in
  List.iter
    (fun path ->
      let path_key = Uri_path.normalize_path_key path in
      if path_key <> "" && not (Hashtbl.mem seen_paths path_key) then (
        Hashtbl.replace seen_paths path_key true;
        match read_file_text path with
        | None -> ()
        | Some text ->
            let idx = file_index_in_text ~path ~text in
            Hashtbl.replace ws.asm_index_by_path path_key idx;
            List.iter (add_label_hit ws.asm_label_hits_by_key) idx.asm_labels;
            List.iter (add_label_hit ws.asm_label_hits_by_key)
              idx.asm_export_directives))
    ws.assembly_file_paths;
  ws.asm_index_paths_key <- path_keys;
  ws.asm_index_dirty <- false

let ensure_label_index (ws : t) : unit =
  let current_paths =
    ws.assembly_file_paths
    |> List.filter_map (fun path ->
           let key = Uri_path.normalize_path_key path in
           if key = "" then None else Some key)
    |> List.sort_uniq String.compare
  in
  if ws.asm_index_dirty || current_paths <> ws.asm_index_paths_key then
    rebuild_label_index ws

let label_hits_in_text ~(visible_compools : string list option) ~(path : string)
    ~(text : string) ~(key : string) : label_hit list =
  let key = normalize_name key in
  if key = "" then []
  else
    match visible_compools with
    | Some visible
      when not
             (visible_for_compools ~visible_compools:visible
                (compool_imports_in_text ~path ~text)) ->
        []
    | _ ->
        let idx = file_index_in_text ~path ~text in
        let label_hits =
          idx.asm_labels |> List.filter (fun hit -> hit.label_key = key)
        in
        if label_hits <> [] then label_hits
        else
          idx.asm_export_directives
          |> List.filter (fun hit -> hit.label_key = key)

let label_hits_for_key ?visible_compools (ws : t) ~(key : string) :
    label_hit list =
  let key = normalize_name key in
  if key = "" then []
  else (
    ensure_label_index ws;
    let hits =
      Option.value (Hashtbl.find_opt ws.asm_label_hits_by_key key) ~default:[]
      |> List.filter (fun (hit : asm_label_hit) ->
             match visible_compools with
             | None -> true
             | Some visible -> (
                 let path_key = Uri_path.normalize_path_key hit.label_path in
                 match Hashtbl.find_opt ws.asm_index_by_path path_key with
                 | None -> true
                 | Some idx ->
                     visible_for_import_keys ~visible_compools:visible
                       idx.asm_import_keys))
    in
    let labels, exports =
      List.partition
        (fun hit ->
          match hit.label_source with
          | AsmConcreteLabel -> true
          | AsmExportDirective -> false)
        hits
    in
    let sort_hits =
      List.sort (fun a b ->
          match String.compare a.label_path b.label_path with
          | 0 ->
              compare a.label_loc.Ast.Loc.start_pos.offset
                b.label_loc.Ast.Loc.start_pos.offset
          | n -> n)
    in
    match sort_hits labels with [] -> sort_hits exports | xs -> xs)

let label_exists_for_key ?visible_compools (ws : t) ~(key : string) : bool =
  match label_hits_for_key ?visible_compools ws ~key with
  | [] -> false
  | _ :: _ -> true
