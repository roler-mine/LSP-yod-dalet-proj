(* Module overview: Models include targets and normalized include keys for workspace graphing. *)

type source_map_record = {
  expanded_range : Ast.Loc.t;
  original_source_file : string option;
  original_range : Ast.Loc.t;
}

type include_target = {
  target : string;
  normalized_target : string;
  directive_loc : Ast.Loc.t;
  target_loc : Ast.Loc.t;
  resolved_path : string option;
  source_map : source_map_record list;
}

let normalize_target (s : string) : string =
  s |> String.trim |> String.map (fun c -> if c = '\\' then '/' else c)

let normalize_key (s : string) : string =
  normalize_target s |> String.uppercase_ascii

let loc_of_token ~(file : string option) (tok : Preprocess.lex_tok) :
    Ast.Loc.t =
  Ast.Loc.of_lexing_positions
    (Parser.token_span_start_p ~file tok)
    (Parser.token_span_end_p ~file tok)
    ~file

let make_source_map_record ~(expanded_range : Ast.Loc.t)
    ~(original_source_file : string option) ~(original_range : Ast.Loc.t) :
    source_map_record =
  { expanded_range; original_source_file; original_range }

let with_resolved_path (target : include_target) (resolved_path : string option)
    : include_target =
  { target with resolved_path }

let include_targets_of_tokens ~(file : string option)
    (tokens : Preprocess.lex_tok array) : include_target list =
  let len = Array.length tokens in
  let target_at i =
    if i < 0 || i >= len then None
    else
      match tokens.(i).Parser.tok with
      | Parser.ID raw | Parser.FIXED_A raw | Parser.STRINGLIT raw ->
          Some (raw, tokens.(i))
      | _ -> None
  in
  let legacy_copy_marker raw =
    String.uppercase_ascii (String.trim raw) = "ICOPY"
  in
  let copy_marker_at i =
    if i < 0 || i >= len then None
    else
      match tokens.(i).Parser.tok with
      | Parser.BANG when i + 1 < len -> (
          match tokens.(i + 1).Parser.tok with
          | Parser.ID raw | Parser.FIXED_A raw
            when Preprocess.canonical_directive_name raw = "COPY" ->
              Some (i, i + 1)
          | _ -> None)
      | Parser.ID raw | Parser.FIXED_A raw when legacy_copy_marker raw ->
          Some (i, i)
      | _ -> None
  in
  let rec find_target i steps =
    if i >= len || steps > 16 then None
    else
      match tokens.(i).Parser.tok with
      | Parser.LPAREN | Parser.RPAREN | Parser.COMMA ->
          find_target (i + 1) (steps + 1)
      | Parser.SEMI | Parser.TERM | Parser.EOF -> None
      | _ -> (
          match target_at i with
          | Some _ as hit -> hit
          | None -> find_target (i + 1) (steps + 1))
  in
  let seen = Hashtbl.create 8 in
  let out = ref [] in
  for i = 0 to len - 1 do
    match copy_marker_at i with
    | Some (directive_i, marker_i) -> (
        match find_target (marker_i + 1) 0 with
        | None -> ()
        | Some (target, target_tok) ->
            let normalized_target = normalize_key target in
            if normalized_target <> "" && not (Hashtbl.mem seen normalized_target)
            then (
              Hashtbl.replace seen normalized_target true;
              out :=
                {
                  target = normalize_target target;
                  normalized_target;
                  directive_loc = loc_of_token ~file tokens.(directive_i);
                  target_loc = loc_of_token ~file target_tok;
                  resolved_path = None;
                  source_map = [];
                }
                :: !out))
    | None -> ()
  done;
  List.rev !out

let include_targets_of_text ~(file : string option) ~(text : string) :
    include_target list =
  try
    let tokens = Preprocess.lex_all_tokens ~file ~text in
    include_targets_of_tokens ~file tokens
  with _ -> []

let include_targets_of_doc (doc : Document.t) : include_target list =
  include_targets_of_text ~file:doc.Document.file ~text:doc.Document.text

let json_string_opt = function None -> `Null | Some s -> `String s

let json_of_pos (p : Ast.Loc.pos) : Yojson.Safe.t =
  `Assoc
    [
      ("line", `Int p.line);
      ("col", `Int p.col);
      ("offset", `Int p.offset);
    ]

let json_of_loc (loc : Ast.Loc.t) : Yojson.Safe.t =
  `Assoc
    [
      ("file", json_string_opt loc.file);
      ("start", json_of_pos loc.start_pos);
      ("end", json_of_pos loc.end_pos);
    ]

let source_map_record_to_yojson (record : source_map_record) : Yojson.Safe.t =
  `Assoc
    [
      ("expandedRange", json_of_loc record.expanded_range);
      ("originalSourceFile", json_string_opt record.original_source_file);
      ("originalRange", json_of_loc record.original_range);
    ]

let include_target_to_yojson (target : include_target) : Yojson.Safe.t =
  `Assoc
    [
      ("target", `String target.target);
      ("normalizedTarget", `String target.normalized_target);
      ("directiveLocation", json_of_loc target.directive_loc);
      ("targetLocation", json_of_loc target.target_loc);
      ("resolvedPath", json_string_opt target.resolved_path);
      ( "sourceMap",
        `List (List.map source_map_record_to_yojson target.source_map) );
    ]

let field name fields = List.assoc_opt name fields

let string_of_json = function `String s -> Some s | _ -> None

let option_string_of_json = function
  | `Null -> Some None
  | `String s -> Some (Some s)
  | _ -> None

let int_of_json = function
  | `Int i -> Some i
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let field_bind name fields f =
  match field name fields with None -> None | Some v -> f v

let pos_of_yojson = function
  | `Assoc fields -> (
      match
        ( field_bind "line" fields int_of_json,
          field_bind "col" fields int_of_json,
          field_bind "offset" fields int_of_json )
      with
      | Some line, Some col, Some offset -> Some { Ast.Loc.line; col; offset }
      | _ -> None)
  | _ -> None

let loc_of_yojson = function
  | `Assoc fields -> (
      match
        ( field_bind "start" fields pos_of_yojson,
          field_bind "end" fields pos_of_yojson )
      with
      | Some start_pos, Some end_pos ->
          let file =
            field_bind "file" fields option_string_of_json
            |> Option.value ~default:None
          in
          Some { Ast.Loc.file; start_pos; end_pos }
      | _ -> None)
  | _ -> None

let source_map_record_of_yojson = function
  | `Assoc fields -> (
      match
        ( field_bind "expandedRange" fields loc_of_yojson,
          field_bind "originalRange" fields loc_of_yojson )
      with
      | Some expanded_range, Some original_range ->
          let original_source_file =
            field_bind "originalSourceFile" fields option_string_of_json
            |> Option.value ~default:None
          in
          Some { expanded_range; original_source_file; original_range }
      | _ -> None)
  | _ -> None

let include_target_of_yojson = function
  | `Assoc fields -> (
      match
        ( field_bind "target" fields string_of_json,
          field_bind "normalizedTarget" fields string_of_json,
          field_bind "directiveLocation" fields loc_of_yojson,
          field_bind "targetLocation" fields loc_of_yojson )
      with
      | Some target, Some normalized_target, Some directive_loc, Some target_loc
        ->
          let resolved_path =
            field_bind "resolvedPath" fields option_string_of_json
            |> Option.value ~default:None
          in
          let source_map =
            match field "sourceMap" fields with
            | Some (`List values) ->
                List.filter_map source_map_record_of_yojson values
            | _ -> []
          in
          Some
            {
              target;
              normalized_target;
              directive_loc;
              target_loc;
              resolved_path;
              source_map;
            }
      | _ -> None)
  | _ -> None
