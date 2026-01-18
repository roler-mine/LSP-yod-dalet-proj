module T = Lsp.Types
module Uri = Lsp.Uri

type position_encoding = [ `UTF8 | `UTF16 ]

type doc =
  { mutable td : Lsp.Text_document.t
  ; mutable analysis : Analyze.t
  }

type t =
  { docs : (string, doc) Hashtbl.t
  ; position_encoding : position_encoding
  }

let create ?(position_encoding = `UTF16) () =
  { docs = Hashtbl.create 16; position_encoding }

let uri_key (uri : T.DocumentUri.t) : string =
  Uri.to_string uri

let publish_diagnostics ~(uri : T.DocumentUri.t) ?version (diagnostics : T.Diagnostic.t list) :
    T.PublishDiagnosticsParams.t =
  T.PublishDiagnosticsParams.create ~uri ~diagnostics ?version ()

let analyze_text ~(uri : T.DocumentUri.t) ~(text : string) : Analyze.t =
  Analyze.analyze ~uri ~text

let analyze_doc (td : Lsp.Text_document.t) : Analyze.t =
  let uri = Lsp.Text_document.documentUri td in
  let text = Lsp.Text_document.text td in
  analyze_text ~uri ~text

let get_doc (st : t) (uri : T.DocumentUri.t) : doc option =
  Hashtbl.find_opt st.docs (uri_key uri)

let set_doc (st : t) (d : doc) : unit =
  let uri = Lsp.Text_document.documentUri d.td in
  Hashtbl.replace st.docs (uri_key uri) d

let abs_pos (td : Lsp.Text_document.t) (pos : T.Position.t) : int =
  Lsp.Text_document.absolute_position td pos

let abs_range (td : Lsp.Text_document.t) (r : T.Range.t) : int * int =
  Lsp.Text_document.absolute_range td r

let range_contains (td : Lsp.Text_document.t) (r : T.Range.t) (pos : T.Position.t) : bool =
  let p = abs_pos td pos in
  let a, b = abs_range td r in
  a <= p && p <= b

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let identifier_at_position (td : Lsp.Text_document.t) (pos : T.Position.t) : string option =
  let text = Lsp.Text_document.text td in
  let n = String.length text in
  let p = abs_pos td pos |> min n |> max 0 in
  let i0 = min (max 0 (p - 1)) (max 0 (n - 1)) in
  if n = 0 then None
  else if not (is_ident_char text.[i0]) then None
  else (
    let l = ref i0 in
    while !l > 0 && is_ident_char text.[!l - 1] do
      decr l
    done;
    let r = ref i0 in
    while !r + 1 < n && is_ident_char text.[!r + 1] do
      incr r
    done;
    Some (String.sub text !l (!r - !l + 1))
  )

let proc_name_at (td : Lsp.Text_document.t) (a : Analyze.t) (pos : T.Position.t) : string option =
  Symbols.all a.symbols
  |> List.find_opt (fun s ->
         match s.Symbols.kind with
         | Symbols.Proc ->
             range_contains td (Symbols.symbol_decl_range s) pos
         | _ -> false)
  |> Option.map (fun s -> s.Symbols.name)

let lookup_symbol (a : Analyze.t) ~(proc_name : string option) (name : string) : Symbols.symbol option =
  match proc_name with
  | None -> Symbols.find a.symbols name
  | Some pn -> (
      match Symbols.find ~proc_name:pn a.symbols name with
      | Some s -> Some s
      | None -> Symbols.find a.symbols name)

let symbol_kind (k : Symbols.kind) : T.SymbolKind.t =
  match k with
  | Symbols.Proc -> T.SymbolKind.Function
  | Symbols.Table -> T.SymbolKind.Struct
  | Symbols.Param -> T.SymbolKind.Variable
  | Symbols.Local -> T.SymbolKind.Variable
  | Symbols.Item -> T.SymbolKind.Variable

let completion_kind (k : Symbols.kind) : T.CompletionItemKind.t =
  match k with
  | Symbols.Proc -> T.CompletionItemKind.Function
  | Symbols.Table -> T.CompletionItemKind.Struct
  | Symbols.Param -> T.CompletionItemKind.Variable
  | Symbols.Local -> T.CompletionItemKind.Variable
  | Symbols.Item -> T.CompletionItemKind.Variable

let did_open (st : t) (p : T.DidOpenTextDocumentParams.t) : T.PublishDiagnosticsParams.t =
  let td = Lsp.Text_document.make ~position_encoding:st.position_encoding p in
  let analysis = analyze_doc td in
  set_doc st { td; analysis };
  publish_diagnostics ~uri:p.textDocument.uri ~version:p.textDocument.version analysis.diagnostics

let did_change (st : t) (p : T.DidChangeTextDocumentParams.t)
  : T.PublishDiagnosticsParams.t
=
  let uri = p.textDocument.uri in
  match get_doc st uri with
  | None ->
      (* If we somehow get a change before open, clear diagnostics. *)
      publish_diagnostics ~uri []
  | Some d ->
      let td' =
        Lsp.Text_document.apply_content_changes
          ~version:p.textDocument.version
          d.td
          p.contentChanges
      in
      d.td <- td';
      d.analysis <- analyze_doc td';
      publish_diagnostics ~uri ~version:p.textDocument.version d.analysis.diagnostics


let did_close (st : t) (p : T.DidCloseTextDocumentParams.t) : T.PublishDiagnosticsParams.t =
  Hashtbl.remove st.docs (uri_key p.textDocument.uri);
  publish_diagnostics ~uri:p.textDocument.uri []

let hover (st : t) (p : T.HoverParams.t) : T.Hover.t option =
  match get_doc st p.textDocument.uri with
  | None -> None
  | Some d -> (
      match identifier_at_position d.td p.position with
      | None -> None
      | Some name ->
          let pn = proc_name_at d.td d.analysis p.position in
          match lookup_symbol d.analysis ~proc_name:pn name with
          | None -> None
          | Some s ->
              let code =
                match s.Symbols.ty with
                | None -> s.Symbols.name
                | Some ty -> s.Symbols.name ^ " : " ^ ty
              in
              let body =
                match s.Symbols.doc with
                | None -> "```jovial\n" ^ code ^ "\n```"
                | Some doc -> "```jovial\n" ^ code ^ "\n```\n\n" ^ doc
              in
              let mc = T.MarkupContent.create ~kind:T.MarkupKind.Markdown ~value:body in
              Some
                (T.Hover.create
                   ~contents:(`MarkupContent mc)
                   ~range:(Symbols.symbol_name_range s)
                   ()))

let definition (st : t) (p : T.DefinitionParams.t) : T.Locations.t option =
  match get_doc st p.textDocument.uri with
  | None -> None
  | Some d -> (
      match identifier_at_position d.td p.position with
      | None -> None
      | Some name ->
          let pn = proc_name_at d.td d.analysis p.position in
          match lookup_symbol d.analysis ~proc_name:pn name with
          | None -> None
          | Some s ->
              let uri = Lsp.Text_document.documentUri d.td in
              let range = Symbols.symbol_name_range s in
              let loc = T.Location.create ~uri ~range in
              Some (`Location [ loc ]))

let document_symbols (st : t) (p : T.DocumentSymbolParams.t) :
    [ `DocumentSymbol of T.DocumentSymbol.t list
    | `SymbolInformation of T.SymbolInformation.t list
    ] option =
  match get_doc st p.textDocument.uri with
  | None -> None
  | Some d ->
      let syms = Symbols.all d.analysis.symbols in
      let by_container : (string, Symbols.symbol list) Hashtbl.t = Hashtbl.create 32 in
      List.iter
        (fun s ->
          match s.Symbols.container with
          | None -> ()
          | Some c ->
              let prev = Option.value (Hashtbl.find_opt by_container c) ~default:[] in
              Hashtbl.replace by_container c (s :: prev))
        syms;

      let rec build (s : Symbols.symbol) : T.DocumentSymbol.t =
        let children =
          Hashtbl.find_opt by_container s.Symbols.name
          |> Option.map (fun xs -> xs |> List.rev |> List.map build)
        in
        T.DocumentSymbol.create
          ~name:s.Symbols.name
          ~kind:(symbol_kind s.Symbols.kind)
          ~range:(Symbols.symbol_decl_range s)
          ~selectionRange:(Symbols.symbol_name_range s)
          ?detail:s.Symbols.ty
          ?children
          ()
      in

      let tops = List.filter (fun s -> s.Symbols.container = None) syms |> List.rev in
      Some (`DocumentSymbol (List.map build tops))

let completion (st : t) (p : T.CompletionParams.t) :
    [ `CompletionList of T.CompletionList.t | `List of T.CompletionItem.t list ] option =
  match get_doc st p.textDocument.uri with
  | None -> None
  | Some d ->
      let prefix =
        match identifier_at_position d.td p.position with
        | None -> ""
        | Some x -> x
      in
      let starts_with ~prefix s =
        let lp = String.length prefix in
        String.length s >= lp && String.sub s 0 lp = prefix
      in
      let items =
        Symbols.all d.analysis.symbols
        |> List.filter (fun s -> prefix = "" || starts_with ~prefix s.Symbols.name)
        |> List.map (fun s ->
               let documentation =
                 match s.Symbols.doc with
                 | None -> None
                 | Some txt -> Some (`String txt)
               in
               T.CompletionItem.create
                 ~label:s.Symbols.name
                 ~kind:(completion_kind s.Symbols.kind)
                 ?detail:s.Symbols.ty
                 ?documentation
                 ())
      in
      let cl = T.CompletionList.create ~isIncomplete:false ~items () in
      Some (`CompletionList cl)
