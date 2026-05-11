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

val build : file:string option -> text:string -> t

val metrics : t -> metrics

val build_with_profile :
  ?previous:t ->
  ?edit_summary:edit_summary ->
  profile:Parser.profile ->
  file:string option ->
  text:string ->
  unit ->
  t

val drop_ast : t -> t
