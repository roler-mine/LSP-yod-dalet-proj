module T = Lsp.Types
open Workspace_state
open Workspace_nav_lookup

type options = { tab_size : int; insert_spaces : bool }

let default_options = { tab_size = 2; insert_spaces = true }

let normalize_options options =
  { options with tab_size = max 1 (min 16 options.tab_size) }

let indent_unit options =
  let options = normalize_options options in
  if options.insert_spaces then String.make options.tab_size ' ' else "\t"

let indent_string options level =
  let level = max 0 level in
  let unit = indent_unit options in
  let buf = Buffer.create (level * String.length unit) in
  for _ = 1 to level do
    Buffer.add_string buf unit
  done;
  Buffer.contents buf

let trim_trailing_cr s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

let line_text (doc : Document.t) ~(line : int) : string option =
  match Text_index.line_start_offset doc.Document.index ~line with
  | None -> None
  | Some start -> (
      match Text_index.line_length doc.Document.index ~line with
      | None -> None
      | Some len ->
          if start < 0 || len < 0 || start + len > String.length doc.text then
            None
          else Some (String.sub doc.text start len |> trim_trailing_cr))

let is_ws = function ' ' | '\t' -> true | _ -> false

let leading_ws_len s =
  let n = String.length s in
  let rec loop i = if i < n && is_ws s.[i] then loop (i + 1) else i in
  loop 0

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' | '\'' -> true
  | _ -> false

let normalize_word = Workspace_state.normalize_name

let words_of_line s =
  let n = String.length s in
  let rec scan i acc =
    if i >= n then List.rev acc
    else if is_ident_char s.[i] then (
      let j = ref (i + 1) in
      while !j < n && is_ident_char s.[!j] do
        incr j
      done;
      let word = String.sub s i (!j - i) |> normalize_word in
      scan !j (word :: acc))
    else scan (i + 1) acc
  in
  scan 0 []

let starts_with s prefix =
  let n = String.length s in
  let p = String.length prefix in
  n >= p && String.sub s 0 p = prefix

let is_comment_only trimmed =
  starts_with trimmed "%" || starts_with trimmed "\""

let is_define_line = function "DEFINE" :: _ -> true | _ -> false

let pre_outdent words =
  match words with
  | "END" :: _ | "TERM" :: _ | "ELSE" :: _ -> true
  | _ -> false

let post_indent words =
  match words with
  | "START" :: _ | "BEGIN" :: _ | "ELSE" :: _ | "IF" :: _ | "CASE" :: _
  | "FOR" :: _ | "WHILE" :: _ ->
      true
  | _ -> false

let parse_is_format_safe (doc : Document.t) : bool =
  match Document.current_parse doc with
  | Some { Document.parsed_diags = []; _ } -> true
  | _ -> false

let format_line_edits ?range (doc : Document.t) ~(options : options) :
    T.TextEdit.t list =
  if not (parse_is_format_safe doc) then []
  else
    let line_count = Text_index.line_count doc.Document.index in
    let range_start, range_end =
      match range with
      | None -> (0, max 0 (line_count - 1))
      | Some (range : T.Range.t) ->
          let start_line = max 0 range.start.line in
          let end_line =
            if range.end_.character = 0 then range.end_.line - 1
            else range.end_.line
          in
          (start_line, min (line_count - 1) (max start_line end_line))
    in
    let edits = ref [] in
    let level = ref 0 in
    for line = 0 to line_count - 1 do
      match line_text doc ~line with
      | None -> ()
      | Some raw ->
          let leading_len = leading_ws_len raw in
          let trimmed = String.trim raw in
          if trimmed = "" || is_comment_only trimmed then ()
          else
            let words = words_of_line trimmed in
            if is_define_line words then ()
            else (
              if pre_outdent words then level := max 0 (!level - 1);
              if line >= range_start && line <= range_end then (
                let wanted = indent_string options !level in
                let current = String.sub raw 0 leading_len in
                if current <> wanted then
                  let range =
                    {
                      T.Range.start = { line; character = 0 };
                      end_ = { line; character = leading_len };
                    }
                  in
                  edits := T.TextEdit.create ~range ~newText:wanted :: !edits);
              if post_indent words then incr level)
    done;
    List.rev !edits

let document_edits_for (ws : t) ~(uri : T.DocumentUri.t) ~(options : options) :
    T.TextEdit.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc -> format_line_edits doc ~options

let range_edits_for (ws : t) ~(uri : T.DocumentUri.t) ~(range : T.Range.t)
    ~(options : options) : T.TextEdit.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc -> format_line_edits ~range doc ~options
