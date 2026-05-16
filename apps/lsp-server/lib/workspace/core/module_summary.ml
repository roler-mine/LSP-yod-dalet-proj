(* Module overview: Summarizes parsed modules and compools for change detection and indexing. *)

type public_symbol = {
  name : string;
  key : string;
  kind : string;
  loc : Ast.Loc.t;
  signature : string;
  exported : bool;
  imported : bool;
}

type public_define = {
  name : string;
  key : string;
  formals : string list;
  requires_call : bool;
  body_signature : string;
}

type t = {
  source_uri : string;
  source_file : string option;
  content_hash : string;
  compool_name : string option;
  exported_symbols : public_symbol list;
  exported_types : public_symbol list;
  imported_compools : string list;
  icopy_targets : string list;
  define_public_macros : public_define list;
  public_signature_hash : string;
  conservative : bool;
  reasons : string list;
}

let normalize_name (s : string) : string =
  String.uppercase_ascii (String.trim s)

let normalize_include_target (s : string) : string =
  s |> String.trim |> String.map (fun c -> if c = '\\' then '/' else c)

let hash_text (text : string) : string = Digest.to_hex (Digest.string text)

let string_of_kind = function
  | Syntax_cache.SkModule -> "module"
  | Syntax_cache.SkCompool -> "compool"
  | Syntax_cache.SkProcedure -> "procedure"
  | Syntax_cache.SkFunction -> "function"
  | Syntax_cache.SkItem -> "item"
  | Syntax_cache.SkTable -> "table"
  | Syntax_cache.SkBlock -> "block"
  | Syntax_cache.SkType -> "type"
  | Syntax_cache.SkLabel -> "label"
  | Syntax_cache.SkDefineMacro -> "define"

let is_type_like = function
  | Syntax_cache.SkType | Syntax_cache.SkTable | Syntax_cache.SkBlock -> true
  | _ -> false

let sort_uniq_strings values =
  values |> List.filter (fun s -> s <> "") |> List.sort_uniq String.compare

let json_string_opt = function None -> `Null | Some s -> `String s

let field name fields = List.assoc_opt name fields

let field_bind name fields f =
  match field name fields with None -> None | Some v -> f v

let string_of_json = function `String s -> Some s | _ -> None
let bool_of_json = function `Bool b -> Some b | _ -> None

let int_of_json = function
  | `Int i -> Some i
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let option_string_of_json = function
  | `Null -> Some None
  | `String s -> Some (Some s)
  | _ -> None

let string_list_of_json = function
  | `List values ->
      values |> List.filter_map (function `String s -> Some s | _ -> None)
  | _ -> []

let token_payload = function
  | Parser.ID s -> "ID:" ^ normalize_name s
  | Parser.INTLIT s -> "INT:" ^ String.trim s
  | Parser.FLOATLIT s -> "FLOAT:" ^ String.trim s
  | Parser.STRINGLIT s -> "STRING:" ^ s
  | tok -> Parser.Debug.string_of_token tok

let canonical_tokens (tokens : Preprocess.lex_tok array) ~(start_i : int)
    ~(end_i : int) : string =
  let len = Array.length tokens in
  if len = 0 || start_i < 0 || end_i < start_i then ""
  else
    let start_i = max 0 start_i in
    let end_i = min (len - 1) end_i in
    let parts = ref [] in
    for i = start_i to end_i do
      match tokens.(i).Parser.tok with
      | Parser.EOF -> ()
      | tok -> parts := token_payload tok :: !parts
    done;
    String.concat " " (List.rev !parts)

let is_decl_start = function
  | Parser.BANG | Parser.COMPOOL | Parser.ICOMPOOL | Parser.DEFINE
  | Parser.TYPE | Parser.BLOCK | Parser.DEF | Parser.REF | Parser.PROC
  | Parser.ITEM | Parser.TABLE | Parser.READONLY | Parser.INLINE
  | Parser.OVERLAY | Parser.STATIC | Parser.CONSTANT ->
      true
  | _ -> false

let is_signature_boundary = function
  | Parser.START | Parser.SEMI | Parser.TERM | Parser.BEGIN | Parser.END
  | Parser.EOF ->
      true
  | _ -> false

let is_signature_end = function
  | Parser.SEMI | Parser.TERM | Parser.EOF -> true
  | _ -> false

let token_index_for_offset (tokens : Preprocess.lex_tok array) ~(offset : int) :
    int option =
  let found = ref None in
  let len = Array.length tokens in
  let i = ref 0 in
  while !found = None && !i < len do
    let tok = tokens.(!i) in
    if tok.Parser.start_off <= offset && offset <= tok.Parser.end_off then
      found := Some !i;
    incr i
  done;
  !found

let declaration_start_index (tokens : Preprocess.lex_tok array) (name_i : int) :
    int =
  let rec loop i best =
    if i < 0 then Option.value best ~default:name_i
    else
      let tok = tokens.(i).Parser.tok in
      if i <> name_i && is_signature_boundary tok then
        Option.value best ~default:name_i
      else
        let best = if is_decl_start tok then Some i else best in
        loop (i - 1) best
  in
  loop name_i None

let declaration_end_index (tokens : Preprocess.lex_tok array) ~(start_i : int) :
    int =
  let len = Array.length tokens in
  let rec loop i =
    if i >= len then max start_i (len - 1)
    else if i > start_i && is_signature_end tokens.(i).Parser.tok then i
    else loop (i + 1)
  in
  loop start_i

let type_like_end_index (tokens : Preprocess.lex_tok array) ~(decl_end_i : int) :
    int =
  let len = Array.length tokens in
  let rec skip_noise i =
    if i >= len then i
    else
      match tokens.(i).Parser.tok with
      | Parser.SEMI -> skip_noise (i + 1)
      | _ -> i
  in
  let start_i = skip_noise (decl_end_i + 1) in
  if start_i >= len || tokens.(start_i).Parser.tok <> Parser.BEGIN then decl_end_i
  else
    let rec loop i depth =
      if i >= len then decl_end_i
      else
        match tokens.(i).Parser.tok with
        | Parser.BEGIN -> loop (i + 1) (depth + 1)
        | Parser.END ->
            let depth = depth - 1 in
            if depth <= 0 then i else loop (i + 1) depth
        | Parser.EOF | Parser.TERM when depth <= 0 -> decl_end_i
        | _ -> loop (i + 1) depth
    in
    loop start_i 0

let signature_for_symbol (tokens : Preprocess.lex_tok array)
    (symbol : Syntax_cache.skeleton_symbol) : string =
  let loc = symbol.Syntax_cache.sk_loc in
  match
    token_index_for_offset tokens
      ~offset:loc.Ast.Loc.start_pos.offset
  with
  | None -> "symbol:" ^ normalize_name symbol.sk_name
  | Some name_i ->
      let start_i = declaration_start_index tokens name_i in
      let decl_end_i = declaration_end_index tokens ~start_i in
      let end_i =
        if is_type_like symbol.sk_kind then
          type_like_end_index tokens ~decl_end_i
        else decl_end_i
      in
      canonical_tokens tokens ~start_i ~end_i

let icopy_targets_of_tokens (tokens : Preprocess.lex_tok array) : string list =
  let len = Array.length tokens in
  let rec find_target i steps =
    if i >= len || steps > 16 then None
    else
      match tokens.(i).Parser.tok with
      | Parser.LPAREN | Parser.RPAREN | Parser.COMMA ->
          find_target (i + 1) (steps + 1)
      | Parser.ID raw | Parser.STRINGLIT raw -> Some raw
      | Parser.SEMI | Parser.TERM | Parser.EOF -> None
      | _ -> find_target (i + 1) (steps + 1)
  in
  let out = ref [] in
  for i = 0 to len - 1 do
    match tokens.(i).Parser.tok with
    | Parser.ID raw when normalize_name raw = "ICOPY" -> (
        match find_target (i + 1) 0 with
        | None -> ()
        | Some raw ->
            out := (raw |> normalize_include_target |> normalize_name) :: !out)
    | _ -> ()
  done;
  sort_uniq_strings (List.rev !out)

let define_body_signature (body : string) : string =
  try
    let tokens = Preprocess.lex_all_tokens_with_lexemes ~file:None ~text:body in
    canonical_tokens tokens ~start_i:0 ~end_i:(Array.length tokens - 1)
  with _ -> String.trim body

let define_summary (d : Preprocess.define) : public_define =
  {
    name = d.name;
    key = normalize_name d.key;
    formals = List.map normalize_name d.formals;
    requires_call = d.requires_call;
    body_signature = define_body_signature d.body;
  }

let compare_public_symbol (a : public_symbol) (b : public_symbol) : int =
  compare
    (a.key, a.kind, a.signature, a.exported, a.imported)
    (b.key, b.kind, b.signature, b.exported, b.imported)

let compare_public_define (a : public_define) (b : public_define) : int =
  compare
    (a.key, a.formals, a.requires_call, a.body_signature)
    (b.key, b.formals, b.requires_call, b.body_signature)

let public_symbol_to_yojson (s : public_symbol) : Yojson.Safe.t =
  let loc_pos (p : Ast.Loc.pos) : Yojson.Safe.t =
    `Assoc
      [
        ("line", `Int p.line);
        ("col", `Int p.col);
        ("offset", `Int p.offset);
      ]
  in
  `Assoc
    [
      ("name", `String s.name);
      ("key", `String s.key);
      ("kind", `String s.kind);
      ( "loc",
        `Assoc
          [
            ("file", json_string_opt s.loc.Ast.Loc.file);
            ("start", loc_pos s.loc.Ast.Loc.start_pos);
            ("end", loc_pos s.loc.Ast.Loc.end_pos);
          ] );
      ("signature", `String s.signature);
      ("exported", `Bool s.exported);
      ("imported", `Bool s.imported);
    ]

let public_symbol_signature_to_yojson (s : public_symbol) : Yojson.Safe.t =
  `Assoc
    [
      ("name", `String s.name);
      ("key", `String s.key);
      ("kind", `String s.kind);
      ("signature", `String s.signature);
      ("exported", `Bool s.exported);
      ("imported", `Bool s.imported);
    ]

let public_define_to_yojson (d : public_define) : Yojson.Safe.t =
  `Assoc
    [
      ("name", `String d.name);
      ("key", `String d.key);
      ("formals", `List (List.map (fun s -> `String s) d.formals));
      ("requiresCall", `Bool d.requires_call);
      ("bodySignature", `String d.body_signature);
    ]

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

let loc_of_yojson ?default_file = function
  | `Assoc fields -> (
      match
        ( field_bind "start" fields pos_of_yojson,
          field_bind "end" fields pos_of_yojson )
      with
      | Some start_pos, Some end_pos ->
          let file =
            field_bind "file" fields option_string_of_json
            |> Option.value ~default:default_file
          in
          Some { Ast.Loc.file; start_pos; end_pos }
      | _ -> None)
  | _ -> None

let public_symbol_of_yojson ?default_file = function
  | `Assoc fields -> (
      match
        ( field_bind "name" fields string_of_json,
          field_bind "key" fields string_of_json,
          field_bind "kind" fields string_of_json,
          field_bind "signature" fields string_of_json,
          field_bind "exported" fields bool_of_json,
          field_bind "imported" fields bool_of_json )
      with
      | ( Some name,
          Some key,
          Some kind,
          Some signature,
          Some exported,
          Some imported ) ->
          let loc =
            field_bind "loc" fields (loc_of_yojson ?default_file)
            |> Option.value
                 ~default:
                   {
                     Ast.Loc.file = default_file;
                     start_pos = { Ast.Loc.line = 1; col = 0; offset = 0 };
                     end_pos = { Ast.Loc.line = 1; col = 0; offset = 0 };
                   }
          in
          Some
            {
              name;
              key = normalize_name key;
              kind;
              loc;
              signature;
              exported;
              imported;
            }
      | _ -> None)
  | _ -> None

let public_define_of_yojson = function
  | `Assoc fields -> (
      match
        ( field_bind "name" fields string_of_json,
          field_bind "key" fields string_of_json,
          field_bind "requiresCall" fields bool_of_json,
          field_bind "bodySignature" fields string_of_json )
      with
      | Some name, Some key, Some requires_call, Some body_signature ->
          let formals =
            field "formals" fields
            |> Option.map string_list_of_json
            |> Option.value ~default:[]
            |> List.map normalize_name
          in
          Some
            {
              name;
              key = normalize_name key;
              formals;
              requires_call;
              body_signature;
            }
      | _ -> None)
  | _ -> None

let signature_payload_json (summary : t) : Yojson.Safe.t =
  `Assoc
    [
      ("compoolName", json_string_opt summary.compool_name);
      ( "exportedSymbols",
        `List
          (List.map public_symbol_signature_to_yojson
             summary.exported_symbols) );
      ( "exportedTypes",
        `List
          (List.map public_symbol_signature_to_yojson summary.exported_types) );
      ( "importedCompools",
        `List (List.map (fun s -> `String s) summary.imported_compools) );
      ( "icopyTargets",
        `List (List.map (fun s -> `String s) summary.icopy_targets) );
      ( "definePublicMacros",
        `List
          (List.map public_define_to_yojson summary.define_public_macros) );
      ("conservative", `Bool summary.conservative);
      ( "conservativeContentHash",
        if summary.conservative then `String summary.content_hash else `Null );
    ]

let public_hash_for_summary (summary : t) : string =
  summary |> signature_payload_json |> Yojson.Safe.to_string |> hash_text

let tokens_for_summary (doc : Document.t) (syntax : Syntax_cache.t option) :
    Preprocess.lex_tok array option * string list =
  match syntax with
  | Some { Syntax_cache.raw_tokens = Some tokens; _ } -> (Some tokens, [])
  | _ -> (
      try
        ( Some
            (Preprocess.lex_all_tokens_with_lexemes ~file:doc.Document.file
               ~text:doc.Document.text),
          [] )
      with _ -> (None, [ "lex_failed" ]))

let fallback_symbols_from_syntax (syntax : Syntax_cache.t option) =
  match syntax with None -> [] | Some syntax -> syntax.Syntax_cache.skeleton.symbols

let imported_compools_from_doc (doc : Document.t) : string list =
  doc.Document.imports
  |> List.filter_map (fun (imp : Preprocess.import) ->
         match imp.kind with
         | Preprocess.Compool ->
             let key = normalize_name imp.name in
             if key = "" then None else Some key)
  |> sort_uniq_strings

let of_document (doc : Document.t) : t =
  let source_uri = Uri_path.docuri_to_string doc.Document.uri in
  let content_hash = hash_text doc.Document.text in
  let syntax =
    match Document.current_parse doc with
    | Some { Document.parsed_syntax = Some syntax; _ } -> Some syntax
    | _ -> None
  in
  let tokens_opt, token_reasons = tokens_for_summary doc syntax in
  let base_reasons =
    if syntax = None then "no_current_syntax" :: token_reasons else token_reasons
  in
  let symbols = fallback_symbols_from_syntax syntax in
  let public_symbols =
    symbols
    |> List.filter (fun (symbol : Syntax_cache.skeleton_symbol) ->
           symbol.sk_exported || symbol.sk_imported)
    |> List.map (fun (symbol : Syntax_cache.skeleton_symbol) ->
           let signature =
             match tokens_opt with
             | Some tokens -> signature_for_symbol tokens symbol
             | None -> "symbol:" ^ normalize_name symbol.sk_name
           in
           {
             name = symbol.sk_name;
             key = normalize_name symbol.sk_name;
             kind = string_of_kind symbol.sk_kind;
             loc = symbol.sk_loc;
             signature;
             exported = symbol.sk_exported;
             imported = symbol.sk_imported;
           })
    |> List.sort compare_public_symbol
  in
  let exported_types =
    public_symbols
    |> List.filter (fun s ->
           s.exported
           &&
           match s.kind with
           | "type" | "table" | "block" -> true
           | _ -> false)
    |> List.sort compare_public_symbol
  in
  let imported_compools = imported_compools_from_doc doc in
  let icopy_targets =
    match tokens_opt with Some tokens -> icopy_targets_of_tokens tokens | None -> []
  in
  let define_public_macros =
    doc.Document.defines |> List.map define_summary
    |> List.sort compare_public_define
  in
  let conservative = syntax = None || tokens_opt = None in
  let reasons = if conservative then base_reasons else [] in
  let summary =
    {
      source_uri;
      source_file = doc.Document.file;
      content_hash;
      compool_name = Option.map normalize_name doc.Document.compool_def;
      exported_symbols = public_symbols;
      exported_types;
      imported_compools;
      icopy_targets;
      define_public_macros;
      public_signature_hash = "";
      conservative;
      reasons;
    }
  in
  { summary with public_signature_hash = public_hash_for_summary summary }

let public_signature_unchanged (old_summary : t) (new_summary : t) : bool =
  old_summary.public_signature_hash = new_summary.public_signature_hash

let to_yojson (summary : t) : Yojson.Safe.t =
  `Assoc
    [
      ("sourceUri", `String summary.source_uri);
      ("sourceFile", json_string_opt summary.source_file);
      ("contentHash", `String summary.content_hash);
      ("compoolName", json_string_opt summary.compool_name);
      ( "exportedSymbols",
        `List (List.map public_symbol_to_yojson summary.exported_symbols) );
      ( "exportedTypes",
        `List (List.map public_symbol_to_yojson summary.exported_types) );
      ( "importedCompools",
        `List (List.map (fun s -> `String s) summary.imported_compools) );
      ( "icopyTargets",
        `List (List.map (fun s -> `String s) summary.icopy_targets) );
      ( "definePublicMacros",
        `List
          (List.map public_define_to_yojson summary.define_public_macros) );
      ("publicSignatureHash", `String summary.public_signature_hash);
      ("conservative", `Bool summary.conservative);
      ("reasons", `List (List.map (fun s -> `String s) summary.reasons));
    ]

let of_yojson = function
  | `Assoc fields -> (
      match
        ( field_bind "sourceUri" fields string_of_json,
          field_bind "contentHash" fields string_of_json,
          field_bind "publicSignatureHash" fields string_of_json )
      with
      | Some source_uri, Some content_hash, Some public_signature_hash ->
          let source_file =
            field_bind "sourceFile" fields option_string_of_json
            |> Option.value ~default:None
          in
          let compool_name =
            field_bind "compoolName" fields option_string_of_json
            |> Option.value ~default:None
            |> Option.map normalize_name
          in
          let exported_symbols =
            match field "exportedSymbols" fields with
            | Some (`List xs) ->
                xs
                |> List.filter_map
                     (public_symbol_of_yojson ?default_file:source_file)
                |> List.sort compare_public_symbol
            | _ -> []
          in
          let exported_types =
            match field "exportedTypes" fields with
            | Some (`List xs) ->
                xs
                |> List.filter_map
                     (public_symbol_of_yojson ?default_file:source_file)
                |> List.sort compare_public_symbol
            | _ -> []
          in
          let imported_compools =
            field "importedCompools" fields
            |> Option.map string_list_of_json
            |> Option.value ~default:[]
            |> List.map normalize_name |> sort_uniq_strings
          in
          let icopy_targets =
            field "icopyTargets" fields
            |> Option.map string_list_of_json
            |> Option.value ~default:[]
            |> List.map normalize_name |> sort_uniq_strings
          in
          let define_public_macros =
            match field "definePublicMacros" fields with
            | Some (`List xs) ->
                xs |> List.filter_map public_define_of_yojson
                |> List.sort compare_public_define
            | _ -> []
          in
          let conservative =
            field_bind "conservative" fields bool_of_json
            |> Option.value ~default:false
          in
          let reasons =
            field "reasons" fields
            |> Option.map string_list_of_json
            |> Option.value ~default:[]
          in
          Some
            {
              source_uri;
              source_file;
              content_hash;
              compool_name;
              exported_symbols;
              exported_types;
              imported_compools;
              icopy_targets;
              define_public_macros;
              public_signature_hash;
              conservative;
              reasons;
            }
      | _ -> None)
  | _ -> None
