module T = Lsp.Types
module R = Workspace_readiness

type macro_actual = {
  formal : string option;
  loc : Ast.Loc.t;
  text : string;
}

type source_map = { generated_to_source : (Ast.Loc.t * Ast.Loc.t) list }

type expansion = {
  define_symbol_id : Symbol_id.t option;
  define : Preprocess.define;
  define_def : Workspace_nav_model.def;
  call_site_uri : T.DocumentUri.t;
  call_site_loc : Ast.Loc.t;
  call_name_loc : Ast.Loc.t;
  expanded_loc : Ast.Loc.t option;
  actuals : macro_actual list;
  source_map : source_map;
  provisional : bool;
  reason : R.reason option;
}

type t = {
  uri : T.DocumentUri.t option;
  expansions : expansion list;
  source_map : source_map;
}

let empty = { uri = None; expansions = []; source_map = { generated_to_source = [] } }
let expansions g = g.expansions
let source_map g = g.source_map

let normalize_name = Workspace_state.normalize_name

let uri_key (uri : T.DocumentUri.t) : string = Uri_path.docuri_to_string uri

let same_uri (a : T.DocumentUri.t) (b : T.DocumentUri.t) : bool =
  uri_key a = uri_key b

let loc_fragment (loc : Ast.Loc.t) : string =
  Printf.sprintf "%d:%d:%d-%d:%d:%d" loc.Ast.Loc.start_pos.line
    loc.Ast.Loc.start_pos.col loc.Ast.Loc.start_pos.offset
    loc.Ast.Loc.end_pos.line loc.Ast.Loc.end_pos.col
    loc.Ast.Loc.end_pos.offset

let same_loc (a : Ast.Loc.t) (b : Ast.Loc.t) : bool =
  a.Ast.Loc.start_pos.line = b.Ast.Loc.start_pos.line
  && a.Ast.Loc.start_pos.col = b.Ast.Loc.start_pos.col
  && a.Ast.Loc.start_pos.offset = b.Ast.Loc.start_pos.offset
  && a.Ast.Loc.end_pos.line = b.Ast.Loc.end_pos.line
  && a.Ast.Loc.end_pos.col = b.Ast.Loc.end_pos.col
  && a.Ast.Loc.end_pos.offset = b.Ast.Loc.end_pos.offset

let loc_contains_loc (outer : Ast.Loc.t) (inner : Ast.Loc.t) : bool =
  outer.Ast.Loc.start_pos.offset <= inner.Ast.Loc.start_pos.offset
  && inner.Ast.Loc.end_pos.offset <= outer.Ast.Loc.end_pos.offset

let position_in_loc = Workspace_nav_model.position_in_loc

let nth_opt xs n =
  let rec go i = function
    | [] -> None
    | x :: tl -> if i = n then Some x else go (i + 1) tl
  in
  if n < 0 then None else go 0 xs

let pos_of_offset ~(text : string) (off : int) : Ast.Loc.pos =
  let n = String.length text in
  let target = max 0 (min n off) in
  let rec loop i line line_start =
    if i >= target then
      { Ast.Loc.line; col = target - line_start; offset = target }
    else if text.[i] = '\n' then loop (i + 1) (line + 1) (i + 1)
    else loop (i + 1) line line_start
  in
  loop 0 1 0

let loc_of_offsets ~(file : string option) ~(text : string) ~(start_off : int)
    ~(end_off : int) : Ast.Loc.t =
  let n = String.length text in
  let start_off = max 0 (min n start_off) in
  let end_off = max start_off (min n end_off) in
  Ast.Loc.make ~file
    ~start_pos:(pos_of_offset ~text start_off)
    ~end_pos:(pos_of_offset ~text end_off)

let loc_of_doc_offsets (doc : Document.t) ~(start_off : int) ~(end_off : int) :
    Ast.Loc.t =
  Workspace_nav_model.loc_of_offsets ~file:doc.Document.file
    ~idx:doc.Document.index ~s:start_off ~e:end_off

let segment_generated_loc ~(file : string option) ~(generated_text : string)
    (seg : Preprocess.expansion_segment) : Ast.Loc.t =
  loc_of_offsets ~file ~text:generated_text
    ~start_off:seg.generated_start_off ~end_off:seg.generated_end_off

let source_loc_of_origin = function
  | Preprocess.Original span -> span.Preprocess.source_loc
  | Preprocess.MacroExpansion { call_site; _ } -> call_site

let segment_source_pair ~(file : string option) ~(generated_text : string)
    (seg : Preprocess.expansion_segment) : (Ast.Loc.t * Ast.Loc.t) option =
  if seg.generated_end_off < seg.generated_start_off then None
  else
    Some
      ( segment_generated_loc ~file ~generated_text seg,
        source_loc_of_origin seg.origin )

let source_map_of_preprocess ~(file : string option) (pre : Preprocess.result) :
    source_map =
  {
    generated_to_source =
      List.filter_map
        (segment_source_pair ~file ~generated_text:pre.Preprocess.text)
        pre.Preprocess.source_map;
  }

let is_ws = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let skip_ws_forward text i =
  let n = String.length text in
  let rec loop j = if j < n && is_ws text.[j] then loop (j + 1) else j in
  loop i

let trim_bounds text a b =
  let n = String.length text in
  let a = max 0 (min n a) in
  let b = max a (min n b) in
  let rec left i = if i < b && is_ws text.[i] then left (i + 1) else i in
  let rec right i =
    if i > a && is_ws text.[i - 1] then right (i - 1) else i
  in
  let a = left a in
  let b = right b in
  (a, b)

let slice text a b =
  let n = String.length text in
  let a = max 0 (min n a) in
  let b = max a (min n b) in
  String.sub text a (b - a)

let parse_call_arguments_with_locs (doc : Document.t) ~(open_idx : int) :
    (string * Ast.Loc.t) list option =
  let text = doc.Document.text in
  let n = String.length text in
  if open_idx < 0 || open_idx >= n || text.[open_idx] <> '(' then None
  else
    let push_arg args_rev arg_start stop =
      let a, b = trim_bounds text arg_start stop in
      let loc = loc_of_doc_offsets doc ~start_off:a ~end_off:b in
      (slice text a b, loc) :: args_rev
    in
    let rec loop i depth in_single in_double arg_start args_rev =
      if i >= n then None
      else
        let c = text.[i] in
        if in_single then
          if c = '\'' then
            if i + 1 < n && text.[i + 1] = '\'' then
              loop (i + 2) depth true in_double arg_start args_rev
            else loop (i + 1) depth false in_double arg_start args_rev
          else loop (i + 1) depth true in_double arg_start args_rev
        else if in_double then
          if c = '"' then
            if i + 1 < n && text.[i + 1] = '"' then
              loop (i + 2) depth in_single true arg_start args_rev
            else loop (i + 1) depth in_single false arg_start args_rev
          else loop (i + 1) depth in_single true arg_start args_rev
        else
          match c with
          | '\'' -> loop (i + 1) depth true false arg_start args_rev
          | '"' -> loop (i + 1) depth false true arg_start args_rev
          | '(' -> loop (i + 1) (depth + 1) false false arg_start args_rev
          | ')' ->
              let depth = depth - 1 in
              if depth = 0 then
                let args = List.rev (push_arg args_rev arg_start i) in
                let a, b = trim_bounds text (open_idx + 1) i in
                if a = b then Some [] else Some args
              else loop (i + 1) depth false false arg_start args_rev
          | ',' when depth = 1 ->
              let args_rev = push_arg args_rev arg_start i in
              loop (i + 1) depth false false (i + 1) args_rev
          | _ -> loop (i + 1) depth false false arg_start args_rev
    in
    loop (open_idx + 1) 1 false false (open_idx + 1) []

let call_name_loc (doc : Document.t) (d : Preprocess.define)
    (call_site : Ast.Loc.t) : Ast.Loc.t =
  let start_off = call_site.Ast.Loc.start_pos.offset in
  let end_off = start_off + String.length d.Preprocess.name in
  loc_of_doc_offsets doc ~start_off ~end_off

let formals_from_define_decl (doc : Document.t) (d : Preprocess.define) :
    string list =
  let text = doc.Document.text in
  let open_idx = skip_ws_forward text d.Preprocess.loc.Ast.Loc.end_pos.offset in
  match parse_call_arguments_with_locs doc ~open_idx with
  | None -> []
  | Some args ->
      args
      |> List.filter_map (fun (name, _) ->
             let key = normalize_name name in
             if key = "" then None else Some key)

let actuals_for_call (doc : Document.t) (d : Preprocess.define)
    (call_site : Ast.Loc.t) : macro_actual list * bool =
  let text = doc.Document.text in
  let formals =
    match d.Preprocess.formals with
    | [] -> formals_from_define_decl doc d
    | xs -> xs
  in
  let name_end =
    call_site.Ast.Loc.start_pos.offset + String.length d.Preprocess.name
  in
  let open_idx = skip_ws_forward text name_end in
  let call_site_end = call_site.Ast.Loc.end_pos.offset in
  let has_call_syntax =
    open_idx < String.length text && text.[open_idx] = '('
    && open_idx < call_site_end
  in
  if (not d.Preprocess.requires_call) && formals = [] && not has_call_syntax
  then ([], true)
  else
    match parse_call_arguments_with_locs doc ~open_idx with
    | None -> ([], false)
    | Some args ->
        let actuals =
          args
          |> List.mapi (fun i (text, loc) ->
                 { formal = nth_opt formals i; loc; text })
        in
        (actuals, true)

let find_define_for_macro (doc : Document.t) ~(macro_name : string)
    ~(macro_decl : Ast.Loc.t) : Preprocess.define option =
  let key = normalize_name macro_name in
  let candidates =
    doc.Document.defines
    |> List.filter (fun (d : Preprocess.define) -> d.key = key)
  in
  match
    List.find_opt
      (fun (d : Preprocess.define) -> same_loc d.Preprocess.loc macro_decl)
      candidates
  with
  | Some _ as hit -> hit
  | None -> (
      match List.rev candidates with [] -> None | d :: _ -> Some d)

type expansion_builder = {
  define_symbol_id : Symbol_id.t option;
  define : Preprocess.define;
  define_def : Workspace_nav_model.def;
  call_site_uri : T.DocumentUri.t;
  call_site_loc : Ast.Loc.t;
  call_name_loc : Ast.Loc.t;
  actuals : macro_actual list;
  mutable generated_locs : Ast.Loc.t list;
  mutable generated_to_source : (Ast.Loc.t * Ast.Loc.t) list;
  provisional : bool;
  reason : R.reason option;
}

let combine_locs ~(file : string option) ~(text : string) (locs : Ast.Loc.t list)
    : Ast.Loc.t option =
  match locs with
  | [] -> None
  | loc :: rest ->
      let start_off =
        List.fold_left
          (fun acc loc -> min acc loc.Ast.Loc.start_pos.offset)
          loc.Ast.Loc.start_pos.offset rest
      in
      let end_off =
        List.fold_left
          (fun acc loc -> max acc loc.Ast.Loc.end_pos.offset)
          loc.Ast.Loc.end_pos.offset rest
      in
      Some (loc_of_offsets ~file ~text ~start_off ~end_off)

let expansion_of_builder ~(file : string option) ~(generated_text : string)
    (b : expansion_builder) : expansion =
  {
    define_symbol_id = b.define_symbol_id;
    define = b.define;
    define_def = b.define_def;
    call_site_uri = b.call_site_uri;
    call_site_loc = b.call_site_loc;
    call_name_loc = b.call_name_loc;
    expanded_loc = combine_locs ~file ~text:generated_text b.generated_locs;
    actuals = b.actuals;
    source_map = { generated_to_source = List.rev b.generated_to_source };
    provisional = b.provisional;
    reason = b.reason;
  }

let expansion_sort_key (e : expansion) =
  Printf.sprintf "%s|%s|%s" (uri_key e.call_site_uri)
    (loc_fragment e.call_site_loc) e.define_def.Workspace_nav_model.key

let of_document (doc : Document.t) : t =
  match Document.current_parse doc with
  | None -> empty
  | Some { Document.parsed_syntax = None; _ } -> empty
  | Some { Document.parsed_syntax = Some syntax; _ } ->
      let pre = syntax.Syntax_cache.preprocess in
      let graph_source_map = source_map_of_preprocess ~file:doc.Document.file pre in
      let builders : (string, expansion_builder) Hashtbl.t =
        Hashtbl.create 32
      in
      let add_expansion (seg : Preprocess.expansion_segment)
          ~(macro_name : string) ~(macro_decl : Ast.Loc.t)
          ~(call_site : Ast.Loc.t) : unit =
        match find_define_for_macro doc ~macro_name ~macro_decl with
        | None -> ()
        | Some define ->
            let generated_loc =
              segment_generated_loc ~file:doc.Document.file
                ~generated_text:pre.Preprocess.text seg
            in
            let key =
              String.concat "|"
                [
                  normalize_name macro_name;
                  loc_fragment macro_decl;
                  loc_fragment call_site;
                ]
            in
            let source_pair = (generated_loc, call_site) in
            let builder =
              match Hashtbl.find_opt builders key with
              | Some b -> b
              | None ->
                  let define_def =
                    Workspace_nav_model.def_of_preprocess_define doc define
                  in
                  let actuals, complete_actuals =
                    actuals_for_call doc define call_site
                  in
                  let b =
                    {
                      define_symbol_id =
                        Some (Semantic_graph.symbol_id_of_def define_def);
                      define;
                      define_def;
                      call_site_uri = doc.Document.uri;
                      call_site_loc = call_site;
                      call_name_loc = call_name_loc doc define call_site;
                      actuals;
                      generated_locs = [];
                      generated_to_source = [];
                      provisional = not complete_actuals;
                      reason =
                        (if complete_actuals then None
                         else Some R.MacroExpansionUnavailable);
                    }
                  in
                  Hashtbl.replace builders key b;
                  b
            in
            builder.generated_locs <- generated_loc :: builder.generated_locs;
            builder.generated_to_source <-
              source_pair :: builder.generated_to_source
      in
      List.iter
        (fun (seg : Preprocess.expansion_segment) ->
          match seg.origin with
          | Preprocess.MacroExpansion { macro_name; macro_decl; call_site; _ }
            ->
              add_expansion seg ~macro_name ~macro_decl ~call_site
          | Preprocess.Original _ -> ())
        pre.Preprocess.source_map;
      let expansions =
        Hashtbl.fold
          (fun _ b acc ->
            expansion_of_builder ~file:doc.Document.file
              ~generated_text:pre.Preprocess.text b
            :: acc)
          builders []
        |> List.sort (fun a b -> compare (expansion_sort_key a) (expansion_sort_key b))
      in
      { uri = Some doc.Document.uri; expansions; source_map = graph_source_map }

let expansion_at_position (g : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : expansion option =
  match g.uri with
  | Some graph_uri when not (same_uri graph_uri uri) -> None
  | _ ->
      g.expansions
      |> List.find_opt (fun (exp : expansion) ->
             same_uri exp.call_site_uri uri && position_in_loc pos exp.call_site_loc)

let macro_use_at_position (g : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : expansion option =
  match g.uri with
  | Some graph_uri when not (same_uri graph_uri uri) -> None
  | _ ->
      g.expansions
      |> List.find_opt (fun (exp : expansion) ->
             same_uri exp.call_site_uri uri && position_in_loc pos exp.call_name_loc)

let definition_of_macro_use (g : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : Workspace_nav_model.def option =
  macro_use_at_position g ~uri ~pos
  |> Option.map (fun (e : expansion) -> e.define_def)

let same_def (a : Workspace_nav_model.def) (b : Workspace_nav_model.def) : bool
    =
  same_uri a.uri b.uri && same_loc a.loc b.loc && a.key = b.key

let uses_of_define (g : t) (d : Workspace_nav_model.def) : expansion list =
  g.expansions |> List.filter (fun (exp : expansion) -> same_def exp.define_def d)

let source_loc_for_generated_loc (g : t) (loc : Ast.Loc.t) : Ast.Loc.t option =
  g.source_map.generated_to_source
  |> List.find_map (fun (generated, source) ->
         if loc_contains_loc generated loc then Some source else None)
