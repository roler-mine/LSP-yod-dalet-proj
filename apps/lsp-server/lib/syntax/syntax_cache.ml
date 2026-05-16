(* Module overview: Cached parse/skeleton representation used by workspace indexing and features. *)

type syntax_unit_kind =
  | Program
  | Module_item
  | Declaration
  | Procedure_body
  | Statement
  | Expression

type syntax_unit = {
  kind : syntax_unit_kind;
  start_off : int;
  end_off : int;
  digest : string;
}

type edit_summary = Token_cache.edit_summary = {
  full_sync : bool;
  start_off : int;
  old_end_off : int;
  new_end_off : int;
  inserted_chars : int;
  change_count : int;
}

type skeleton = {
  imports : Preprocess.import list;
  compool_def : string option;
  defines : Preprocess.define list;
  proc_names : (string * Ast.Loc.t) list;
  symbols : skeleton_symbol list;
  symbol_keys : string list;
}

and skeleton_symbol_kind =
  | SkModule
  | SkCompool
  | SkProcedure
  | SkFunction
  | SkItem
  | SkTable
  | SkBlock
  | SkType
  | SkLabel
  | SkDefineMacro

and skeleton_symbol = {
  sk_name : string;
  sk_kind : skeleton_symbol_kind;
  sk_loc : Ast.Loc.t;
  sk_container : string option;
  sk_exported : bool;
  sk_imported : bool;
}

type metrics = {
  lexed_token_count : int;
  reused_prefix_tokens : int;
  reused_suffix_tokens : int;
  checkpoint_count : int;
  checkpoint_reused : bool;
  checkpoint_fallback_reason : string option;
  parse_duration_ms : float;
}

type t = {
  raw_text : string;
  raw_hash : string;
  token_cache : Token_cache.t;
  token_reuse : Token_cache.reuse_stats;
  raw_tokens : Preprocess.lex_tok array option;
  preprocess : Preprocess.result;
  expanded_changed : bool;
  expanded_hash : string;
  expanded_tokens : Preprocess.lex_tok array option;
  parse : Parser.output;
  parse_duration_ms : float;
  checkpoint_cache : Parser.checkpoint_cache;
  checkpoint_stats : Parser.checkpoint_stats;
  skeleton : skeleton;
  units : syntax_unit list;
}

let hash_text (text : string) : string = Digest.to_hex (Digest.string text)
let now_ms () : float = Unix.gettimeofday () *. 1000.0
let normalize_name s = String.uppercase_ascii (String.trim s)

let clamp_span ~(text : string) ~(start_off : int) ~(end_off : int) : int * int
    =
  let n = String.length text in
  let start_off = max 0 (min n start_off) in
  let end_off = max start_off (min n end_off) in
  (start_off, end_off)

let digest_span ~(text : string) ~(start_off : int) ~(end_off : int) : string =
  let start_off, end_off = clamp_span ~text ~start_off ~end_off in
  Digest.to_hex (Digest.substring text start_off (end_off - start_off))

let unit_of_span ~(text : string) (kind : syntax_unit_kind) ~(start_off : int)
    ~(end_off : int) : syntax_unit =
  let start_off, end_off = clamp_span ~text ~start_off ~end_off in
  { kind; start_off; end_off; digest = digest_span ~text ~start_off ~end_off }

let token_start (tok : Preprocess.lex_tok) = tok.Parser.start_off
let token_end (tok : Preprocess.lex_tok) = tok.Parser.end_off

let is_boundary = function
  | Parser.SEMI | Parser.TERM | Parser.END | Parser.EOF -> true
  | _ -> false

let is_decl_start = function
  | Parser.BANG | Parser.COMPOOL | Parser.ICOMPOOL | Parser.DEFINE
  | Parser.TYPE | Parser.BLOCK | Parser.DEF | Parser.REF | Parser.PROC
  | Parser.ITEM | Parser.TABLE | Parser.READONLY | Parser.INLINE
  | Parser.OVERLAY | Parser.STATIC | Parser.CONSTANT ->
      true
  | _ -> false

let is_stmt_start = function
  | Parser.ID _ | Parser.IF | Parser.WHILE | Parser.FOR | Parser.CASE
  | Parser.EXIT | Parser.GOTO | Parser.RETURN | Parser.ABORT | Parser.STOP ->
      true
  | _ -> false

let classify_segment (first_token : Parser.token) : syntax_unit_kind =
  if is_decl_start first_token then Declaration
  else if is_stmt_start first_token then Statement
  else Expression

let segment_units ~(text : string) (tokens : Preprocess.lex_tok array) :
    syntax_unit list =
  let len = Array.length tokens in
  let rec loop i start_idx first_token acc =
    if i >= len then List.rev acc
    else
      let tok = tokens.(i).Parser.tok in
      match (start_idx, tok) with
      | None, Parser.EOF -> List.rev acc
      | None, _ when is_boundary tok -> loop (i + 1) None None acc
      | None, _ -> loop (i + 1) (Some i) (Some tok) acc
      | Some start_i, _ when is_boundary tok ->
          let first =
            match first_token with Some t -> t | None -> tok
          in
          let start_off = token_start tokens.(start_i) in
          let end_off = token_end tokens.(i) in
          let kind = classify_segment first in
          let item = unit_of_span ~text kind ~start_off ~end_off in
          let module_item = { item with kind = Module_item } in
          loop (i + 1) None None (item :: module_item :: acc)
      | _ -> loop (i + 1) start_idx first_token acc
  in
  loop 0 None None []

let procedure_body_units ~(text : string) (tokens : Preprocess.lex_tok array) :
    syntax_unit list =
  let len = Array.length tokens in
  let rec seek_proc i acc =
    if i >= len then List.rev acc
    else
      match tokens.(i).Parser.tok with
      | Parser.PROC -> seek_begin (i + 1) acc
      | _ -> seek_proc (i + 1) acc
  and seek_begin i acc =
    if i >= len then List.rev acc
    else
      match tokens.(i).Parser.tok with
      | Parser.BEGIN -> seek_end i (i + 1) 1 acc
      | Parser.PROC -> seek_begin (i + 1) acc
      | Parser.TERM | Parser.EOF -> seek_proc (i + 1) acc
      | _ -> seek_begin (i + 1) acc
  and seek_end begin_i i depth acc =
    if i >= len then List.rev acc
    else
      match tokens.(i).Parser.tok with
      | Parser.BEGIN -> seek_end begin_i (i + 1) (depth + 1) acc
      | Parser.END ->
          let depth = depth - 1 in
          if depth <= 0 then
            let start_off = token_start tokens.(begin_i) in
            let end_off = token_end tokens.(i) in
            let unit =
              unit_of_span ~text Procedure_body ~start_off ~end_off
            in
            seek_proc (i + 1) (unit :: acc)
          else seek_end begin_i (i + 1) depth acc
      | _ -> seek_end begin_i (i + 1) depth acc
  in
  seek_proc 0 []

let program_unit ~(text : string) (tokens : Preprocess.lex_tok array) :
    syntax_unit option =
  let len = Array.length tokens in
  if len = 0 then None
  else
    let first_non_eof i =
      if i >= len then None
      else match tokens.(i).Parser.tok with Parser.EOF -> None | _ -> Some i
    in
    match first_non_eof 0 with
    | None -> None
    | Some first_i ->
        let last_i = max first_i (len - 1) in
        Some
          (unit_of_span ~text Program ~start_off:(token_start tokens.(first_i))
             ~end_off:(token_end tokens.(last_i)))

let syntax_units ~(text : string) (tokens : Preprocess.lex_tok array) :
    syntax_unit list =
  let units = procedure_body_units ~text tokens @ segment_units ~text tokens in
  match program_unit ~text tokens with None -> units | Some u -> u :: units

let loc_of_token ~(file : string option) (tok : Preprocess.lex_tok) :
    Ast.Loc.t =
  Ast.Loc.of_lexing_positions
    (Parser.token_span_start_p ~file tok)
    (Parser.token_span_end_p ~file tok)
    ~file

let skeleton_of_preprocess ~(file : string option)
    ~(tokens : Preprocess.lex_tok array) (pre : Preprocess.result) :
    skeleton =
  let seen = Hashtbl.create 32 in
  let symbol_keys_rev = ref [] in
  let add_symbol_key key =
    let key = String.uppercase_ascii (String.trim key) in
    if key <> "" && not (Hashtbl.mem seen key) then (
      Hashtbl.replace seen key true;
      symbol_keys_rev := key :: !symbol_keys_rev)
  in
  let symbols_rev = ref [] in
  let seen_symbols = Hashtbl.create 64 in
  let add_symbol ?container ?(exported = false) ?(imported = false) name kind loc
      =
    let key = String.uppercase_ascii (String.trim name) in
    let symbol_key =
      Printf.sprintf "%s|%d|%d|%d"
        key loc.Ast.Loc.start_pos.offset loc.Ast.Loc.end_pos.offset
        (match kind with
        | SkModule -> 1
        | SkCompool -> 2
        | SkProcedure | SkFunction -> 3
        | SkItem -> 4
        | SkTable -> 5
        | SkBlock -> 6
        | SkType -> 7
        | SkLabel -> 8
        | SkDefineMacro -> 9)
    in
    if key <> "" && not (Hashtbl.mem seen_symbols symbol_key) then (
      Hashtbl.replace seen_symbols symbol_key true;
      add_symbol_key name;
      symbols_rev :=
        {
          sk_name = name;
          sk_kind = kind;
          sk_loc = loc;
          sk_container = container;
          sk_exported = exported;
          sk_imported = imported;
        }
        :: !symbols_rev)
  in
  List.iter
    (fun (d : Preprocess.define) ->
      add_symbol d.name SkDefineMacro d.loc)
    pre.defines;
  (match pre.compool_def with None -> () | Some name -> add_symbol_key name);
  let proc_names_rev = ref [] in
  let len = Array.length tokens in
  let name_at i =
    if i < 0 || i >= len then None
    else
      match tokens.(i).Parser.tok with
      | Parser.ID name | Parser.STRINGLIT name -> Some (name, tokens.(i))
      | Parser.PROGRAM -> Some ("PROGRAM", tokens.(i))
      | Parser.TYPE -> Some ("TYPE", tokens.(i))
      | Parser.BLOCK -> Some ("BLOCK", tokens.(i))
      | Parser.DEFAULT -> Some ("DEFAULT", tokens.(i))
      | _ -> None
  in
  let skip_noise i =
    let rec loop j =
      if j >= len then j
      else
        match tokens.(j).Parser.tok with
        | Parser.LPAREN | Parser.RPAREN | Parser.COMMA -> loop (j + 1)
        | _ -> j
    in
    loop i
  in
  let find_name_after i =
    let rec loop j steps =
      if j >= len || steps > 12 then None
      else
        let j = skip_noise j in
        match name_at j with
        | Some hit -> Some hit
        | None -> (
            match tokens.(j).Parser.tok with
            | Parser.SEMI | Parser.TERM | Parser.END | Parser.BEGIN -> None
            | _ -> loop (j + 1) (steps + 1))
    in
    loop i 0
  in
  let symbol_after ?container ?(exported = false) ?(imported = false) i kind =
    match find_name_after (i + 1) with
    | None -> ()
    | Some (name, tok) ->
        let loc = loc_of_token ~file tok in
        add_symbol ?container ~exported ~imported name kind loc;
        if kind = SkProcedure || kind = SkFunction then
          proc_names_rev := (name, loc) :: !proc_names_rev
  in
  let current_container = ref None in
  let current_group_modifier = ref None in
  let is_decl_modifier = function
    | Parser.DEF -> Some (`Exported true)
    | Parser.REF -> Some (`Imported true)
    | _ -> None
  in
  let exported_imported_at i =
    if i <= 0 then (false, false, i)
    else
      match is_decl_modifier tokens.(i - 1).Parser.tok with
      | Some (`Exported true) -> (true, false, i - 1)
      | Some (`Imported true) -> (false, true, i - 1)
      | _ -> (
          match !current_group_modifier with
          | Some `Def -> (true, false, i)
          | Some `Ref -> (false, true, i)
          | None -> (false, false, i))
  in
  let proc_has_return i =
    let is_return_name = function
      | Parser.ID s ->
          let k = normalize_name s in
          k <> "REC" && k <> "RECURSIVE" && k <> "RENT" && k <> "REENTRANT"
      | Parser.TYPE | Parser.BLOCK -> true
      | _ -> false
    in
    let rec scan_after_paren j depth =
      if j >= len then false
      else
        match tokens.(j).Parser.tok with
        | Parser.SEMI | Parser.BEGIN | Parser.END | Parser.TERM | Parser.EOF ->
            false
        | Parser.LPAREN -> scan_after_paren (j + 1) (depth + 1)
        | Parser.RPAREN ->
            let depth = max 0 (depth - 1) in
            scan_after_paren (j + 1) depth
        | tok when depth = 0 && is_return_name tok -> true
        | _ -> scan_after_paren (j + 1) depth
    in
    match find_name_after (i + 1) with
    | None -> false
    | Some (_, name_tok) ->
        let start =
          let rec find_idx j =
            if j >= len then len
            else if tokens.(j).Parser.start_off = name_tok.Parser.start_off then j
            else find_idx (j + 1)
          in
          find_idx (i + 1) + 1
        in
        scan_after_paren start 0
  in
  for i = 0 to len - 1 do
    match tokens.(i).Parser.tok with
    | Parser.DEF
      when i + 1 < len && tokens.(i + 1).Parser.tok = Parser.BEGIN ->
        current_group_modifier := Some `Def
    | Parser.REF
      when i + 1 < len && tokens.(i + 1).Parser.tok = Parser.BEGIN ->
        current_group_modifier := Some `Ref
    | Parser.START -> (
        let next_tok =
          if i + 1 < len then Some tokens.(i + 1).Parser.tok else None
        in
        match (next_tok, find_name_after (i + 2)) with
        | Some Parser.PROGRAM, Some (name, tok) ->
            add_symbol name SkModule (loc_of_token ~file tok)
        | Some Parser.COMPOOL, Some (name, tok) ->
            add_symbol name SkCompool (loc_of_token ~file tok)
        | _ -> ())
    | Parser.PROGRAM ->
        if i = 0 || tokens.(i - 1).Parser.tok <> Parser.START then
          symbol_after i SkModule
    | Parser.COMPOOL ->
        if i = 0 || tokens.(i - 1).Parser.tok <> Parser.START then
          symbol_after i SkCompool
    | Parser.ICOMPOOL -> symbol_after ~imported:true i SkCompool
    | Parser.PROC ->
        let exported, imported, _decl_start = exported_imported_at i in
        let kind = if proc_has_return i then SkFunction else SkProcedure in
        symbol_after ?container:!current_container ~exported ~imported i
          kind
    | Parser.ITEM ->
        let exported, imported, _decl_start = exported_imported_at i in
        symbol_after ?container:!current_container ~exported ~imported i SkItem
    | Parser.TABLE ->
        let exported, imported, _decl_start = exported_imported_at i in
        symbol_after ?container:!current_container ~exported ~imported i
          SkTable
    | Parser.TYPE ->
        let exported, imported, _decl_start = exported_imported_at i in
        symbol_after ?container:!current_container ~exported ~imported i SkType
    | Parser.BLOCK -> (
        let exported, imported, _decl_start = exported_imported_at i in
        match find_name_after (i + 1) with
        | None -> ()
        | Some (name, tok) ->
            current_container := Some name;
            add_symbol ~exported ~imported name SkBlock (loc_of_token ~file tok))
    | Parser.ID name
      when i + 1 < len
           && tokens.(i + 1).Parser.tok = Parser.COLON
           && (i = 0 || tokens.(i - 1).Parser.tok <> Parser.FOR) ->
        add_symbol ?container:!current_container name SkLabel
          (loc_of_token ~file tokens.(i))
    | Parser.END ->
        current_container := None;
        current_group_modifier := None
    | _ -> ()
  done;
  {
    imports = pre.imports;
    compool_def = pre.compool_def;
    defines = pre.defines;
    proc_names = List.rev !proc_names_rev;
    symbols = List.rev !symbols_rev;
    symbol_keys = List.rev !symbol_keys_rev;
  }

let build_with_profile ?previous ?edit_summary ~(profile : Parser.profile)
    ~(file : string option) ~(text : string) () : t =
  let raw_hash = hash_text text in
  let previous_token_cache = Option.map (fun p -> p.token_cache) previous in
  let token_cache =
    Token_cache.build ?previous:previous_token_cache ?edit:edit_summary
      ~text_hash:raw_hash
      ~lex_from:(fun ~start_off ~start_line ~start_col ->
        Preprocess.lex_all_tokens_from_offset ~file ~text ~start_off
          ~start_line ~start_col)
      ~lex:(fun () -> Preprocess.lex_all_tokens ~file ~text)
      ()
  in
  let raw_tokens = token_cache.tokens in
  let preprocess, expanded_changed =
    Preprocess.run_from_tokens ~file ~text ~tokens:raw_tokens
  in
  let parse_tokens =
    if expanded_changed then Preprocess.lex_all_tokens ~file ~text:preprocess.text
    else raw_tokens
  in
  let dirty_token = token_cache.stats.Token_cache.dirty_start_token in
  let previous_expanded_changed =
    match previous with Some p -> p.expanded_changed | None -> false
  in
  let can_reuse_checkpoints =
    (not expanded_changed) && not previous_expanded_changed
  in
  let previous_checkpoint_cache =
    if can_reuse_checkpoints then
      Option.map (fun p -> p.checkpoint_cache) previous
    else None
  in
  let dirty_token = if can_reuse_checkpoints then dirty_token else None in
  let parse_start_ms = now_ms () in
  let checkpointed =
    Parser.parse_tokens_checkpointed ?previous:previous_checkpoint_cache
      ?dirty_token ~file ~dump_ast:false ~profile ~tokens:parse_tokens
      ()
  in
  let parse_output =
    if expanded_changed then
      {
        checkpointed.output with
        diags =
          Preprocess.diagnostics_through_source_map
            ~generated_text:preprocess.text preprocess.source_map
            checkpointed.output.diags;
      }
    else checkpointed.output
  in
  let parse_duration_ms = max 0.0 (now_ms () -. parse_start_ms) in
  let expanded_tokens =
    if expanded_changed then Some parse_tokens else None
  in
  let skeleton = skeleton_of_preprocess ~file ~tokens:parse_tokens preprocess in
  {
    raw_text = text;
    raw_hash;
    token_cache;
    token_reuse = token_cache.stats;
    raw_tokens = Some raw_tokens;
    preprocess;
    expanded_changed;
    expanded_hash = hash_text preprocess.text;
    expanded_tokens;
    parse = parse_output;
    parse_duration_ms;
    checkpoint_cache = checkpointed.checkpoint_cache;
    checkpoint_stats = checkpointed.checkpoint_stats;
    skeleton;
    units = syntax_units ~text:preprocess.text parse_tokens;
  }

let build ~(file : string option) ~(text : string) : t =
  build_with_profile ~profile:Parser.Interactive ~file ~text ()

let metrics (cache : t) : metrics =
  {
    lexed_token_count = Array.length cache.token_cache.tokens;
    reused_prefix_tokens = cache.token_reuse.reused_prefix_tokens;
    reused_suffix_tokens = cache.token_reuse.reused_suffix_tokens;
    checkpoint_count = cache.checkpoint_stats.checkpoint_count;
    checkpoint_reused = cache.checkpoint_stats.checkpoint_reused;
    checkpoint_fallback_reason = cache.checkpoint_stats.fallback_reason;
    parse_duration_ms = cache.parse_duration_ms;
  }

let drop_ast (cache : t) : t =
  {
    cache with
    parse = { cache.parse with ast = None; ast_dump = None };
    raw_tokens = None;
    expanded_tokens = None;
  }
