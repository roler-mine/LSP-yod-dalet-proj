module T = Lsp.Types
open Workspace_hover_markdown
open Workspace_symbol_kinds

let location_of_symbol ~(uri : string) (loc : Ast.Loc.t) : T.Location.t option =
  match Uri_path.docuri_of_string uri with
  | None -> None
  | Some doc_uri -> Some (Lsp_conv.location_of_loc ~uri:doc_uri loc)

let find_symbol snap ~uri ~pos =
  match Workspace_snapshot.file snap ~uri with
  | Some { Workspace_snapshot.skeleton = Some sk; _ } ->
      Skeleton_index.symbol_at_position sk pos
  | _ -> None

let hover snap ~uri ~pos =
  match find_symbol snap ~uri ~pos with
  | None -> None
  | Some sym ->
      let range = Lsp_conv.range_of_loc sym.Skeleton_index.loc in
      Some
        (hover_markdown ~range
           (let kind =
              Workspace_symbol_metadata.symbol_kind_label
                sym.Skeleton_index.metadata.jovial_kind
            in
            let role =
              Workspace_symbol_metadata.decl_role_label
                sym.Skeleton_index.metadata.decl_role
            in
            hover_panel ~name:sym.name
              ~summary:(Printf.sprintf "JOVIAL %s - syntax snapshot" kind)
              ~facts:
                [
                  Printf.sprintf "Classification: %s" kind;
                  Printf.sprintf "Declaration role: %s" role;
                  Printf.sprintf "Exported: %s"
                    (if sym.exported then "yes" else "no");
                  Printf.sprintf "Imported: %s"
                    (if sym.imported then "yes" else "no");
                  Printf.sprintf "Scope id: %d" sym.scope_id;
                  Printf.sprintf "Location: line %d, column %d"
                    sym.loc.Ast.Loc.start_pos.line
                    (sym.loc.Ast.Loc.start_pos.col + 1);
                  "Status: semantic details pending";
                ]
              ~sections:
                [
                  hover_inline_section "Details"
                    "The snapshot has syntax-level information for this \
                     symbol. Full declaration text, implementation preview, \
                     references, and cross-file details will appear when \
                     workspace indexing catches up.";
                ]))

let completion snap ~uri:_ ~pos:_ =
  Symbol_index.all snap.Workspace_snapshot.symbols
  |> List.map (fun (sym : Symbol_index.symbol_record) ->
         let fallback = completion_kind_of_skeleton_kind sym.kind in
         T.CompletionItem.create ~label:sym.Symbol_index.name
           ~kind:(completion_kind_of_metadata sym.metadata ~fallback)
           ~detail:"JOVIAL symbol" ())

let definition snap ~uri ~pos =
  match find_symbol snap ~uri ~pos with
  | None -> []
  | Some sym ->
      Symbol_index.by_name snap.Workspace_snapshot.symbols sym.normalized_name
      |> List.filter_map (fun hit ->
             location_of_symbol ~uri
               (Option.value hit.Symbol_index.definition
                  ~default:hit.declaration))

let references snap ~uri ~pos =
  match find_symbol snap ~uri ~pos with
  | None -> Seq.empty
  | Some sym ->
      Reference_index.by_name snap.Workspace_snapshot.refs sym.normalized_name
      |> List.filter_map (fun occ -> location_of_symbol ~uri occ.Reference_index.loc)
      |> List.to_seq

let document_symbols snap ~uri =
  match Workspace_snapshot.file snap ~uri with
  | Some { Workspace_snapshot.skeleton = Some sk; _ } ->
      Skeleton_index.symbols sk
      |> List.map (fun (sym : Skeleton_index.symbol_decl) ->
             let fallback = lsp_symbol_kind_of_skeleton_kind sym.kind in
             T.DocumentSymbol.create ~name:sym.Skeleton_index.name
               ~kind:(lsp_symbol_kind_of_metadata sym.metadata ~fallback)
               ~range:(Lsp_conv.range_of_loc sym.loc)
               ~selectionRange:(Lsp_conv.range_of_loc sym.loc) ~children:[] ())
  | _ -> []

let workspace_symbols snap ~query =
  let hits =
    if String.trim query = "" then Symbol_index.all snap.Workspace_snapshot.symbols
    else Symbol_index.by_prefix snap.Workspace_snapshot.symbols query
  in
  hits
  |> List.filter_map (fun (sym : Symbol_index.symbol_record) ->
         let uri =
           match sym.Symbol_index.declaration.Ast.Loc.file with
           | Some file -> Uri_path.file_uri_of_path file
           | None -> ""
         in
         match location_of_symbol ~uri sym.declaration with
         | None -> None
         | Some location ->
             let fallback = lsp_symbol_kind_of_skeleton_kind sym.kind in
             Some
               (T.SymbolInformation.create ~name:sym.name
                  ~kind:(lsp_symbol_kind_of_metadata sym.metadata ~fallback)
                  ~location ()))

let semantic_token_type_of_parser_token = function
  | Parser.ID _ -> Some 3
  | Parser.STRINGLIT _ -> Some 6
  | Parser.INTLIT _ | Parser.FLOATLIT _ -> Some 7
  | Parser.PLUS
  | Parser.MINUS
  | Parser.STAR
  | Parser.SLASH
  | Parser.POW
  | Parser.MOD
  | Parser.EQ
  | Parser.NE
  | Parser.LT
  | Parser.LE
  | Parser.GT
  | Parser.GE
  | Parser.AND
  | Parser.OR
  | Parser.XOR
  | Parser.NOT
  | Parser.EQV
  | Parser.CONV_R
  | Parser.CONV_L
  | Parser.AT ->
      Some 8
  | Parser.EOF
  | Parser.SEMI
  | Parser.COMMA
  | Parser.COLON
  | Parser.DOT
  | Parser.LPAREN
  | Parser.RPAREN ->
      None
  | Parser.DEFINE -> Some 9
  | _ -> Some 5

let token_span_intersects_range (span : Parser.token_span) (range : T.Range.t)
    : bool =
  let start_line = max 0 (span.start_line - 1) in
  let end_line = max start_line (span.end_line - 1) in
  let r_start = range.start.line in
  let r_end = range.end_.line in
  end_line >= r_start && start_line <= r_end

let semantic_tokens_data_of_spans (spans : Parser.token_span list) : int array =
  let data_rev = ref [] in
  let prev_line = ref 0 in
  let prev_start = ref 0 in
  let emit (span : Parser.token_span) typ =
    let line = max 0 (span.start_line - 1) in
    let start = max 0 span.start_col in
    let len =
      if span.end_line = span.start_line then
        max 1 (span.end_col - span.start_col)
      else max 1 (span.end_off - span.start_off)
    in
    let delta_line = line - !prev_line in
    let delta_start = if delta_line = 0 then start - !prev_start else start in
    data_rev := 0 :: typ :: len :: delta_start :: delta_line :: !data_rev;
    prev_line := line;
    prev_start := start
  in
  spans
  |> List.sort (fun (a : Parser.token_span) (b : Parser.token_span) ->
         match compare a.start_line b.start_line with
         | 0 -> compare a.start_col b.start_col
         | n -> n)
  |> List.iter (fun span ->
         match semantic_token_type_of_parser_token span.Parser.tok with
         | None -> ()
         | Some typ -> emit span typ);
  Array.of_list (List.rev !data_rev)

let semantic_tokens_range snap ~uri ~range =
  match Workspace_snapshot.file snap ~uri with
  | None -> [||]
  | Some { Workspace_snapshot.tokens = None; _ } -> [||]
  | Some { Workspace_snapshot.tokens = Some cache; _ } ->
      cache.Token_cache.tokens
      |> Array.to_list
      |> List.filter (fun span -> token_span_intersects_range span range)
      |> semantic_tokens_data_of_spans
