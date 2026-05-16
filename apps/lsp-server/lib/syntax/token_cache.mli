(** Module overview: Token stream cache used by syntax display, semantic tokens, and incremental features. *)

type edit_summary = {
  full_sync : bool;
  start_off : int;
  old_end_off : int;
  new_end_off : int;
  inserted_chars : int;
  change_count : int;
}

type reuse_stats = {
  attempted : bool;
  old_token_count : int;
  new_token_count : int;
  reused_prefix_tokens : int;
  reused_suffix_tokens : int;
  dirty_start_token : int option;
  dirty_old_end_token : int option;
  fallback_reason : string option;
}

type t = {
  text_hash : string;
  tokens : Parser.token_span array;
  stats : reuse_stats;
}

val empty_stats : reuse_stats

val build :
  ?previous:t ->
  ?edit:edit_summary ->
  ?lex_from:
    (start_off:int ->
    start_line:int ->
    start_col:int ->
    Parser.token_span array) ->
  text_hash:string ->
  lex:(unit -> Parser.token_span array) ->
  unit ->
  t
