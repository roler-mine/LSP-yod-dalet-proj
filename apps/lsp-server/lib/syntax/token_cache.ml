(* Module overview: Token stream cache used by syntax display, semantic tokens, and incremental features. *)

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

let empty_stats =
  {
    attempted = false;
    old_token_count = 0;
    new_token_count = 0;
    reused_prefix_tokens = 0;
    reused_suffix_tokens = 0;
    dirty_start_token = None;
    dirty_old_end_token = None;
    fallback_reason = None;
  }

let token_equiv (a : Parser.token_span) (b : Parser.token_span) : bool =
  a.tok = b.tok && a.lexeme = b.lexeme

let common_prefix (old_tokens : Parser.token_span array)
    (new_tokens : Parser.token_span array) ~(limit_off : int option) : int =
  let old_len = Array.length old_tokens in
  let new_len = Array.length new_tokens in
  let max_len = min old_len new_len in
  let rec loop i =
    if i >= max_len then i
    else
      match limit_off with
      | Some off when old_tokens.(i).start_off >= off -> i
      | _ ->
          if token_equiv old_tokens.(i) new_tokens.(i) then loop (i + 1)
          else i
  in
  loop 0

let common_suffix (old_tokens : Parser.token_span array)
    (new_tokens : Parser.token_span array) ~(prefix : int) : int =
  let old_len = Array.length old_tokens in
  let new_len = Array.length new_tokens in
  let max_len = min (old_len - prefix) (new_len - prefix) in
  let rec loop n =
    if n >= max_len then n
    else
      let old_i = old_len - 1 - n in
      let new_i = new_len - 1 - n in
      if token_equiv old_tokens.(old_i) new_tokens.(new_i) then loop (n + 1)
      else n
  in
  max 0 (loop 0)

let shifted_col ~(delta : int) ~(edit_line : int) ~(line : int) ~(col : int) :
    int =
  if line = edit_line then max 0 (col + delta) else col

let same_position_shifted ~(delta : int) ~(edit_line : int)
    (old_tok : Parser.token_span) (new_tok : Parser.token_span) : bool =
  new_tok.start_off = old_tok.start_off + delta
  && new_tok.end_off = old_tok.end_off + delta
  && new_tok.start_line = old_tok.start_line
  && new_tok.end_line = old_tok.end_line
  && new_tok.start_col
     = shifted_col ~delta ~edit_line ~line:old_tok.start_line
         ~col:old_tok.start_col
  && new_tok.end_col
     = shifted_col ~delta ~edit_line ~line:old_tok.end_line
         ~col:old_tok.end_col

let same_stable_token_shifted ~(delta : int) ~(edit_line : int)
    (old_tok : Parser.token_span) (new_tok : Parser.token_span) : bool =
  token_equiv old_tok new_tok
  && same_position_shifted ~delta ~edit_line old_tok new_tok

let shift_token ~(delta : int) ~(edit_line : int)
    (tok : Parser.token_span) : Parser.token_span =
  {
    tok with
    start_off = tok.start_off + delta;
    end_off = tok.end_off + delta;
    start_col =
      shifted_col ~delta ~edit_line ~line:tok.start_line ~col:tok.start_col;
    end_col = shifted_col ~delta ~edit_line ~line:tok.end_line ~col:tok.end_col;
  }

let is_safe_restart_token (tok : Parser.token) : bool =
  match tok with
  | Parser.SEMI | Parser.TERM | Parser.END | Parser.START | Parser.BEGIN
  | Parser.PROC | Parser.DEF | Parser.REF | Parser.ITEM | Parser.TABLE
  | Parser.READONLY | Parser.INLINE | Parser.OVERLAY | Parser.TYPE
  | Parser.BLOCK | Parser.COMPOOL | Parser.ICOMPOOL | Parser.DEFINE ->
      true
  | _ -> false

let eof_index (tokens : Parser.token_span array) : int =
  let len = Array.length tokens in
  let rec loop i =
    if i >= len then len
    else match tokens.(i).tok with Parser.EOF -> i | _ -> loop (i + 1)
  in
  loop 0

let find_restart_anchor (tokens : Parser.token_span array) ~(edit_start : int) :
    int * int * int * int =
  let len = eof_index tokens in
  let best = ref None in
  for i = 0 to len - 1 do
    let span = tokens.(i) in
    if span.end_off <= edit_start && is_safe_restart_token span.tok then
      best := Some i
  done;
  match !best with
  | None -> (0, 0, 1, 0)
  | Some i ->
      let span = tokens.(i) in
      (i + 1, span.end_off, span.end_line, span.end_col)

let line_at_offset (tokens : Parser.token_span array) ~(off : int) : int =
  let len = eof_index tokens in
  let rec loop i last_line last_off =
    if i >= len then last_line
    else
      let span = tokens.(i) in
      if off < span.start_off then
        if span.start_line = last_line && off >= last_off then last_line
        else last_line
      else if off <= span.end_off then span.start_line
      else loop (i + 1) span.end_line span.end_off
  in
  loop 0 1 0

let array_drop (arr : 'a array) (n : int) : 'a array =
  let len = Array.length arr in
  let n = max 0 (min len n) in
  Array.sub arr n (len - n)

let array_take (arr : 'a array) (n : int) : 'a array =
  Array.sub arr 0 (max 0 (min (Array.length arr) n))

let rec matching_run (old_tokens : Parser.token_span array)
    (new_tokens : Parser.token_span array) ~(old_i : int) ~(new_i : int)
    ~(need : int) ~(delta : int) ~(edit_line : int) : bool =
  if need <= 0 then true
  else if old_i >= Array.length old_tokens || new_i >= Array.length new_tokens
  then false
  else if
    same_stable_token_shifted ~delta ~edit_line old_tokens.(old_i)
      new_tokens.(new_i)
  then
    matching_run old_tokens new_tokens ~old_i:(old_i + 1) ~new_i:(new_i + 1)
      ~need:(need - 1) ~delta ~edit_line
  else false

let find_suffix_rejoin (old_tokens : Parser.token_span array)
    (fresh_tail : Parser.token_span array) ~(old_start : int) ~(delta : int)
    ~(edit_line : int) : (int * int) option =
  let old_len = Array.length old_tokens in
  let fresh_len = Array.length fresh_tail in
  let needed old_i = min 4 (old_len - old_i) in
  let rec scan_old old_i =
    if old_i >= old_len then None
    else
      let rec scan_fresh new_i =
        if new_i >= fresh_len then None
        else if
          same_stable_token_shifted ~delta ~edit_line old_tokens.(old_i)
            fresh_tail.(new_i)
          && matching_run old_tokens fresh_tail ~old_i ~new_i
               ~need:(needed old_i) ~delta ~edit_line
        then Some (old_i, new_i)
        else scan_fresh (new_i + 1)
      in
      match scan_fresh 0 with
      | Some _ as hit -> hit
      | None -> scan_old (old_i + 1)
  in
  scan_old (max 0 old_start)

let first_token_starting_at_or_after (tokens : Parser.token_span array)
    ~(off : int) ~(from_i : int) : int =
  let len = Array.length tokens in
  let rec loop i =
    if i >= len then len
    else if tokens.(i).start_off >= off then i
    else loop (i + 1)
  in
  loop (max 0 from_i)

let build_window ~(previous : t) ~(edit : edit_summary)
    ~(text_hash : string)
    ~(lex_from :
       start_off:int ->
       start_line:int ->
       start_col:int ->
       Parser.token_span array) : t =
  let old_tokens = previous.tokens in
  let old_len = Array.length old_tokens in
  let prefix_len, start_off, start_line, start_col =
    find_restart_anchor old_tokens ~edit_start:edit.start_off
  in
  let fresh_tail = lex_from ~start_off ~start_line ~start_col in
  let prefix = array_take old_tokens prefix_len in
  let delta = edit.new_end_off - edit.old_end_off in
  let edit_line = line_at_offset old_tokens ~off:edit.start_off in
  let old_suffix_start =
    first_token_starting_at_or_after old_tokens ~off:edit.old_end_off
      ~from_i:prefix_len
  in
  let tokens, reused_suffix_tokens, dirty_old_end_token =
    if edit.change_count = 1 then
      match
        find_suffix_rejoin old_tokens fresh_tail ~old_start:old_suffix_start
          ~delta ~edit_line
      with
      | Some (old_i, fresh_i) when old_i >= prefix_len ->
          let fresh_dirty = array_take fresh_tail fresh_i in
          let suffix =
            array_drop old_tokens old_i
            |> Array.map (shift_token ~delta ~edit_line)
          in
          ( Array.concat [ prefix; fresh_dirty; suffix ],
            Array.length suffix,
            Some old_i )
      | _ ->
          (Array.append prefix fresh_tail, 0, Some old_len)
    else (Array.append prefix fresh_tail, 0, Some old_len)
  in
  let new_len = Array.length tokens in
  {
    text_hash;
    tokens;
    stats =
      {
        attempted = true;
        old_token_count = old_len;
        new_token_count = new_len;
        reused_prefix_tokens = prefix_len;
        reused_suffix_tokens;
        dirty_start_token = Some prefix_len;
        dirty_old_end_token;
        fallback_reason = None;
      };
  }

let stats_for_update ~(previous : t option) ~(edit : edit_summary option)
    ~(tokens : Parser.token_span array) : reuse_stats =
  match (previous, edit) with
  | None, _ ->
      { empty_stats with new_token_count = Array.length tokens }
  | Some prev, Some edit when not edit.full_sync ->
      let old_tokens = prev.tokens in
      let prefix =
        common_prefix old_tokens tokens ~limit_off:(Some edit.start_off)
      in
      let suffix = common_suffix old_tokens tokens ~prefix in
      let old_len = Array.length old_tokens in
      let new_len = Array.length tokens in
      let dirty_old_end = max prefix (old_len - suffix) in
      {
        attempted = true;
        old_token_count = old_len;
        new_token_count = new_len;
        reused_prefix_tokens = prefix;
        reused_suffix_tokens = suffix;
        dirty_start_token = Some prefix;
        dirty_old_end_token = Some dirty_old_end;
        fallback_reason =
          Some "validated_with_full_lex_until_window_relex_is_enabled";
      }
  | Some prev, _ ->
      {
        empty_stats with
        attempted = true;
        old_token_count = Array.length prev.tokens;
        new_token_count = Array.length tokens;
        fallback_reason = Some "full_sync_or_missing_edit_summary";
      }

let build ?previous ?edit ?lex_from ~(text_hash : string)
    ~(lex : unit -> Parser.token_span array) () : t =
  match (previous, edit) with
  | Some prev, _ when prev.text_hash = text_hash ->
      { prev with stats = { prev.stats with fallback_reason = None } }
  | Some prev, Some edit when not edit.full_sync -> (
      match lex_from with
      | Some lex_from -> build_window ~previous:prev ~edit ~text_hash ~lex_from
      | None ->
          let tokens = lex () in
          {
            text_hash;
            tokens;
            stats =
              {
                (stats_for_update ~previous ~edit:(Some edit) ~tokens) with
                fallback_reason = Some "missing_window_lexer";
              };
          })
  | _ ->
      let tokens = lex () in
      { text_hash; tokens; stats = stats_for_update ~previous ~edit ~tokens }
