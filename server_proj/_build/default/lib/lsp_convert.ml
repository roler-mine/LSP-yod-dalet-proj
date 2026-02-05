(* lib/lsp_convert.ml
   Drop-in LSP conversion helpers for:
   - Lexing.position / lexbuf -> LSP positions & ranges
   - Ast.span / Ast.located -> LSP ranges
   - Uri.t <-> string/path utilities (robust enough for Windows file:// URIs)
*)

module T = Lsp.Types

(* ---------- URI helpers ---------- *)

let uri_of_string (s : string) : Lsp.Uri.t = Lsp.Uri.of_string s
let string_of_uri (u : Lsp.Uri.t) : string = Lsp.Uri.to_string u

let starts_with ~prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let hex_val = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> -1

let percent_decode (s : string) : string =
  let b = Bytes.create (String.length s) in
  let rec loop i j =
    if i >= String.length s then Bytes.sub_string b 0 j
    else
      match s.[i] with
      | '%' when i + 2 < String.length s ->
          let h1 = hex_val s.[i + 1] in
          let h2 = hex_val s.[i + 2] in
          if h1 >= 0 && h2 >= 0 then (
            Bytes.set b j (Char.chr ((h1 lsl 4) lor h2));
            loop (i + 3) (j + 1)
          ) else (
            Bytes.set b j s.[i];
            loop (i + 1) (j + 1)
          )
      | c ->
          Bytes.set b j c;
          loop (i + 1) (j + 1)
  in
  loop 0 0

let is_drive_path (s : string) =
  String.length s >= 2
  && ((s.[0] >= 'A' && s.[0] <= 'Z') || (s.[0] >= 'a' && s.[0] <= 'z'))
  && s.[1] = ':'

let normalize_slashes (s : string) =
  String.map (fun c -> if c = '\\' then '/' else c) s

(* Best-effort: file:// URI -> OS-ish path string.
   Works well for "file:///c:/Users/..." and typical unix "file:///home/...".
*)
let path_of_uri (u : Lsp.Uri.t) : string =
  let s = string_of_uri u in
  if starts_with ~prefix:"file://" s then (
    (* strip "file://" *)
    let rest = String.sub s 7 (String.length s - 7) in
    (* file:///c:/... usually yields "/c:/..." *)
    let rest =
      if String.length rest >= 3
         && rest.[0] = '/'
         && is_drive_path (String.sub rest 1 (String.length rest - 1))
      then String.sub rest 1 (String.length rest - 1)
      else rest
    in
    percent_decode rest
  ) else s

let percent_encode_uri_path (s : string) : string =
  (* Minimal encoding: spaces and '%' only (enough to be safe-ish). *)
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | ' ' -> Buffer.add_string buf "%20"
      | '%' -> Buffer.add_string buf "%25"
      | _ -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* Best-effort: OS-ish path -> file:// URI.
   Handles Windows "C:\\..." and "C:/..." by producing "file:///c:/...".
*)
let uri_of_path (path : string) : Lsp.Uri.t =
  let p = normalize_slashes path in
  let uri_str =
    if is_drive_path p then
      "file:///" ^ percent_encode_uri_path p
    else if starts_with ~prefix:"/" p then
      "file://" ^ percent_encode_uri_path p
    else
      (* relative path: still make it a file URI-ish *)
      "file://" ^ percent_encode_uri_path ("/" ^ p)
  in
  uri_of_string uri_str

(* ---------- Position & Range helpers ---------- *)

let clamp_nonneg i = if i < 0 then 0 else i

let position_of_lexing (p : Lexing.position) : T.Position.t =
  let line = clamp_nonneg (p.pos_lnum - 1) in
  let character = clamp_nonneg (p.pos_cnum - p.pos_bol) in
  T.Position.create ~line ~character

let range_of_positions (startp : Lexing.position) (endp : Lexing.position) : T.Range.t =
  T.Range.create
    ~start:(position_of_lexing startp)
    ~end_:(position_of_lexing endp)

(* "range_of_lex" in two flavors, so call sites stop fighting the typechecker. *)
let range_of_lex (startp : Lexing.position) (endp : Lexing.position) : T.Range.t =
  range_of_positions startp endp

let range_of_lex_pair ((startp, endp) : Lexing.position * Lexing.position) : T.Range.t =
  range_of_positions startp endp

(* The one you asked for: based on current lexeme bounds. *)
let range_of_lexbuf (lexbuf : Lexing.lexbuf) : T.Range.t =
  range_of_positions (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)

(* Sometimes you want the buffer cursor span instead of lexeme span. *)
let range_of_lexbuf_cursor (lexbuf : Lexing.lexbuf) : T.Range.t =
  range_of_positions lexbuf.lex_start_p lexbuf.lex_curr_p

(* ---------- AST span helpers (matches your ast.ml) ---------- *)

let range_of_span (sp : Ast.span) : T.Range.t =
  range_of_positions sp.Ast.sp_start sp.Ast.sp_end

let range_of_located (x : 'a Ast.located) : T.Range.t =
  range_of_span x.Ast.loc_span

(* ---------- Location helpers ---------- *)

let location ~uri ~range : T.Location.t =
  T.Location.create ~uri ~range

let location_of_span ~uri (sp : Ast.span) : T.Location.t =
  location ~uri ~range:(range_of_span sp)

let location_of_located ~uri (x : 'a Ast.located) : T.Location.t =
  location ~uri ~range:(range_of_located x)

(* ---------- Diagnostics / Hover convenience ---------- *)

let diagnostic
    ?severity
    ?code
    ?source
    ~(range : T.Range.t)
    ~(message : string)
    ()
  : T.Diagnostic.t =
  (* In this LSP lib, these are OPTIONAL PARAMETERS (don’t wrap in Some). *)
  T.Diagnostic.create ?severity ?code ?source ~range ~message:(`String message) ()

let hover_contents_string (s : string) : [ `MarkupContent of T.MarkupContent.t | `String of string ] =
  `MarkupContent (T.MarkupContent.create ~kind:T.MarkupKind.PlainText ~value:s)

let hover_contents_markdown (md : string) : [ `MarkupContent of T.MarkupContent.t | `String of string ] =
  `MarkupContent (T.MarkupContent.create ~kind:T.MarkupKind.Markdown ~value:md)

let hover ?range (contents : [ `List of T.MarkedString.t list | `MarkedString of T.MarkedString.t | `MarkupContent of T.MarkupContent.t ]) : T.Hover.t =
  T.Hover.create ?range ~contents ()

(* Small helpers to stop “string option where string expected” nonsense. *)
let string_of_opt ?(default = "") = function
  | None -> default
  | Some s -> s
