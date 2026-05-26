(* Module overview: Central diagnostic gate that prevents parse damage from
   leaking stale or low-confidence semantic cascades to the client. *)

module T = Lsp.Types

type parse_status = {
  health : Parser.parse_health;
  confidence : float;
  tainted_ranges : Parser.tainted_range list;
}

let parse_status_of_output (out : Parser.output) : parse_status =
  {
    health = out.parse_health;
    confidence = out.parse_confidence;
    tainted_ranges = out.tainted_ranges;
  }

let default_parse_status =
  {
    health = Parser.ParseLexicalOnly;
    confidence = 0.25;
    tainted_ranges = [];
  }

let compare_pos (a : T.Position.t) (b : T.Position.t) : int =
  if a.line <> b.line then compare a.line b.line
  else compare a.character b.character

let range_intersects (a : T.Range.t) (b : T.Range.t) : bool =
  compare_pos a.start b.end_ <= 0 && compare_pos b.start a.end_ <= 0

let range_of_taint (taint : Parser.tainted_range) : T.Range.t =
  Lsp_conv.range_of_loc taint.taint_loc

let source_of (diag : T.Diagnostic.t) : string =
  match diag.source with None -> "" | Some s -> String.lowercase_ascii s

let message_text (diag : T.Diagnostic.t) : string =
  match diag.message with
  | `String s -> s
  | `MarkupContent mc -> mc.value

let contains ~(needle : string) (s : string) : bool =
  let s = String.lowercase_ascii s in
  let needle = String.lowercase_ascii needle in
  let sn = String.length s in
  let nn = String.length needle in
  if nn = 0 then true
  else if sn < nn then false
  else
    let rec loop i =
      if i + nn > sn then false
      else if String.sub s i nn = needle then true
      else loop (i + 1)
    in
    loop 0

let is_semantic_source = function
  | "semantic" | "import" | "include" | "cross-file" | "cross_file" -> true
  | _ -> false

let is_internal_source = function
  | "internal" | "server" -> true
  | _ -> false

let is_cascade_prone (diag : T.Diagnostic.t) : bool =
  let msg = message_text diag in
  List.exists
    (fun needle -> contains ~needle msg)
    [
      "undefined";
      "missing compool";
      "unresolved";
      "may require";
      "no matching def";
      "duplicate def";
      "ref/def";
      "not visible";
    ]

let intersects_blocking_taint (status : parse_status) (diag : T.Diagnostic.t) :
    bool =
  List.exists
    (fun taint ->
      (not taint.Parser.taint_allows_semantic)
      && range_intersects diag.range (range_of_taint taint))
    status.tainted_ranges

let semantic_publishable (status : parse_status) (diag : T.Diagnostic.t) :
    bool =
  let source = source_of diag in
  if is_internal_source source then true
  else if not (is_semantic_source source) then true
  else
    match status.health with
    | Parser.ParseSkeletonOnly | Parser.ParseLexicalOnly
    | Parser.ParseFailedInternal ->
        false
    | Parser.ParseClean ->
        not (intersects_blocking_taint status diag)
    | Parser.ParseRecovered | Parser.ParsePartial ->
        if status.confidence >= 0.80 then
          not (intersects_blocking_taint status diag)
        else if status.confidence >= 0.50 then
          (not (intersects_blocking_taint status diag))
          && (not (is_cascade_prone diag))
          &&
          match diag.severity with
          | Some T.DiagnosticSeverity.Error -> false
          | _ -> true
        else false

let diagnostic_key (diag : T.Diagnostic.t) : string =
  Printf.sprintf "%d:%d:%d:%d:%s:%s"
    diag.range.start.line diag.range.start.character diag.range.end_.line
    diag.range.end_.character (source_of diag) (message_text diag)

let dedup (diags : T.Diagnostic.t list) : T.Diagnostic.t list =
  let seen = Hashtbl.create (max 16 (List.length diags)) in
  let out = ref [] in
  List.iter
    (fun diag ->
      let key = diagnostic_key diag in
      if not (Hashtbl.mem seen key) then (
        Hashtbl.replace seen key true;
        out := diag :: !out))
    diags;
  List.rev !out

let merge ?parse_output ~(lexer : T.Diagnostic.t list)
    ~(syntax : T.Diagnostic.t list) ~(semantic : T.Diagnostic.t list) () :
    T.Diagnostic.t list =
  let status =
    match parse_output with
    | Some out -> parse_status_of_output out
    | None -> default_parse_status
  in
  let semantic =
    List.filter (semantic_publishable status) semantic
  in
  dedup (lexer @ syntax @ semantic)

let semantic_analysis_allowed ?parse_output () : bool =
  match parse_output with
  | None -> false
  | Some out -> (
      match out.Parser.parse_health with
      | Parser.ParseClean -> true
      | Parser.ParseRecovered | Parser.ParsePartial ->
          out.Parser.parse_confidence >= 0.80
      | Parser.ParseSkeletonOnly | Parser.ParseLexicalOnly
      | Parser.ParseFailedInternal ->
          false)

let status_json ?parse_output ~(semantic_suppressed : bool) () :
    Yojson.Safe.t =
  let status =
    match parse_output with
    | Some out -> parse_status_of_output out
    | None -> default_parse_status
  in
  let health =
    match status.health with
    | Parser.ParseClean -> "clean"
    | Parser.ParseRecovered -> "recovered"
    | Parser.ParsePartial -> "partial"
    | Parser.ParseSkeletonOnly -> "skeletonOnly"
    | Parser.ParseLexicalOnly -> "lexicalOnly"
    | Parser.ParseFailedInternal -> "failedInternal"
  in
  `Assoc
    [
      ("health", `String health);
      ("confidence", `Float status.confidence);
      ("taintedRanges", `Int (List.length status.tainted_ranges));
      ("semanticDiagnosticsSuppressed", `Bool semantic_suppressed);
    ]
