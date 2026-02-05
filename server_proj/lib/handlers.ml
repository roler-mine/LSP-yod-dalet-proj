module T = Lsp.Types
module Uri = Lsp.Uri

type position_encoding = [ `UTF8 | `UTF16 ]

type doc =
  { mutable td : Lsp.Text_document.t
  ; mutable analysis : Analyze.t
  }

type ws_entry =
  { uri : T.DocumentUri.t
  ; path : string option
  ; analysis : Analyze.t
  ; refs : (string, T.Range.t list) Hashtbl.t
  }

type sym_loc =
  { loc : T.Location.t
  ; ty  : string option
  }

type t =
  { docs : (string, doc) Hashtbl.t
  ; position_encoding : position_encoding
  ; mutable root_uris : string list

  (* workspace index *)
  ; ws_files : (string, ws_entry) Hashtbl.t            (* key = uri string *)
  ; def_index : (string, sym_loc list) Hashtbl.t       (* name -> decl locations (+ty) *)
  }

let create ?(position_encoding = `UTF16) () =
  { docs = Hashtbl.create 16
  ; position_encoding
  ; root_uris = []
  ; ws_files = Hashtbl.create 251
  ; def_index = Hashtbl.create 251
  }

let uri_key (uri : T.DocumentUri.t) : string =
  Uri.to_string uri

let publish_diagnostics ~(uri : T.DocumentUri.t) ?version (diagnostics : T.Diagnostic.t list) :
    T.PublishDiagnosticsParams.t =
  T.PublishDiagnosticsParams.create ~uri ~diagnostics ?version ()

let analyze_text ~(uri : T.DocumentUri.t) ~(text : string) : Analyze.t =
  Analyze.analyze ~uri ~text

let analyze_doc (td : Lsp.Text_document.t) : Analyze.t =
  analyze_text ~uri:(Lsp.Text_document.documentUri td) ~text:(Lsp.Text_document.text td)

let get_doc (st : t) (uri : T.DocumentUri.t) : doc option =
  Hashtbl.find_opt st.docs (uri_key uri)

let set_doc (st : t) (d : doc) : unit =
  Hashtbl.replace st.docs (uri_key (Lsp.Text_document.documentUri d.td)) d

let abs_pos (td : Lsp.Text_document.t) (pos : T.Position.t) : int =
  Lsp.Text_document.absolute_position td pos

let abs_range (td : Lsp.Text_document.t) (r : T.Range.t) : int * int =
  Lsp.Text_document.absolute_range td r

let range_contains (td : Lsp.Text_document.t) (r : T.Range.t) (pos : T.Position.t) : bool =
  let p = abs_pos td pos in
  let a, b = abs_range td r in
  a <= p && p <= b

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' | '$' -> true
  | _ -> false

let identifier_at_position (td : Lsp.Text_document.t) (pos : T.Position.t) : string option =
  let text = Lsp.Text_document.text td in
  let n = String.length text in
  if n = 0 then None
  else (
    let p = abs_pos td pos |> max 0 |> min (n - 1) in
    let i0_opt =
      if is_ident_char text.[p] then Some p
      else if p > 0 && is_ident_char text.[p - 1] then Some (p - 1)
      else None
    in
    match i0_opt with
    | None -> None
    | Some i0 ->
        let l = ref i0 in
        while !l > 0 && is_ident_char text.[!l - 1] do decr l done;
        let r = ref i0 in
        while !r + 1 < n && is_ident_char text.[!r + 1] do incr r done;
        Some (String.sub text !l (!r - !l + 1))
  )

let proc_name_at (td : Lsp.Text_document.t) (a : Analyze.t) (pos : T.Position.t) : string option =
  Symbols.all a.symbols
  |> List.find_opt (fun s ->
         match Symbols.sym_kind s with
         | Symbols.Proc -> range_contains td (Symbols.symbol_decl_range s) pos
         | _ -> false)
  |> Option.map Symbols.sym_name

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

(* ---------------- URI/path helpers ---------------- *)

let hex_value = function
  | '0'..'9' as c -> Char.code c - Char.code '0'
  | 'a'..'f' as c -> 10 + (Char.code c - Char.code 'a')
  | 'A'..'F' as c -> 10 + (Char.code c - Char.code 'A')
  | _ -> -1

let pct_decode (s:string) : string =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then ()
    else if s.[i] = '%' && i + 2 < String.length s then
      let a = hex_value s.[i+1] in
      let c = hex_value s.[i+2] in
      if a >= 0 && c >= 0 then (
        Buffer.add_char b (Char.chr (a * 16 + c));
        loop (i+3)
      ) else (
        Buffer.add_char b s.[i];
        loop (i+1)
      )
    else (
      Buffer.add_char b s.[i];
      loop (i+1)
    )
  in
  loop 0;
  Buffer.contents b

let path_of_file_uri (u:string) : string option =
  if not (String.length u >= 7 && String.sub u 0 7 = "file://") then None
  else
    let rest = String.sub u 7 (String.length u - 7) |> pct_decode in
    let rest =
      if String.length rest >= 3 && rest.[0] = '/' && rest.[2] = ':' then
        String.sub rest 1 (String.length rest - 1)
      else rest
    in
    Some rest

let uri_of_path (path:string) : T.DocumentUri.t =
  let p = String.map (fun c -> if c = '\\' then '/' else c) path in
  let p =
    let b = Buffer.create (String.length p) in
    String.iter (fun c ->
      match c with
      | ' ' -> Buffer.add_string b "%20"
      | '#' -> Buffer.add_string b "%23"
      | '%' -> Buffer.add_string b "%25"
      | _ -> Buffer.add_char b c
    ) p;
    Buffer.contents b
  in
  let s =
    if String.length p >= 2 && p.[1] = ':' then "file:///" ^ p else "file://" ^ p
  in
  Uri.of_string s

let is_dir (p:string) = try Sys.is_directory p with _ -> false

let has_ext_j73 (p:string) =
  let p = String.lowercase_ascii p in
  String.length p >= 4 && String.sub p (String.length p - 4) 4 = ".j73"

let should_skip_dir = function
  | ".git" | "_build" | "node_modules" | ".vs" | ".vscode" -> true
  | _ -> false

let rec walk_j73_files (root:string) (acc:string list) : string list =
  if not (is_dir root) then acc
  else
    let items = try Sys.readdir root |> Array.to_list with _ -> [] in
    List.fold_left
      (fun acc name ->
         if should_skip_dir name then acc
         else
           let path = Filename.concat root name in
           if is_dir path then walk_j73_files path acc
           else if has_ext_j73 path then path :: acc
           else acc)
      acc items

let read_file (path:string) : string option =
  try
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    if len > 15_000_000 then (close_in_noerr ic; None)
    else
      let s = really_input_string ic len in
      close_in_noerr ic;
      Some s
  with _ -> None

(* ---------------- reference scanner ---------------- *)

let scan_ident_ranges ~(text:string) : (string, T.Range.t list) Hashtbl.t =
  let tbl : (string, T.Range.t list) Hashtbl.t = Hashtbl.create 251 in
  let add name range =
    let prev = Option.value (Hashtbl.find_opt tbl name) ~default:[] in
    Hashtbl.replace tbl name (range :: prev)
  in
  let n = String.length text in
  let line = ref 0 in
  let col  = ref 0 in
  let i    = ref 0 in

  let bump c =
    match c with
    | '\n' -> incr line; col := 0
    | '\r' -> ()
    | _ -> incr col
  in

  let is_ident_no_quote = function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '$' -> true
    | _ -> false
  in

  let rec skip_single_quote_string () =
    bump '\''; incr i;
    while !i < n do
      let c = text.[!i] in
      bump c; incr i;
      if c = '\'' then break ()
    done
  and break () = () in

  while !i < n do
    let c = text.[!i] in
    if c = '\'' then (
      let prev_ok = (!i > 0 && is_ident_no_quote text.[!i - 1]) in
      let next_ok = (!i + 1 < n && is_ident_no_quote text.[!i + 1]) in
      if (not prev_ok) && next_ok then skip_single_quote_string ()
      else (bump c; incr i)
    )
    else if is_ident_char c then (
      let start_line = !line in
      let start_col  = !col in
      let j = ref !i in
      while !j < n && is_ident_char text.[!j] do
        bump text.[!j];
        incr j
      done;
      let name = String.sub text !i (!j - !i) in
      let end_line = !line in
      let end_col  = !col in
      let range =
        T.Range.create
          ~start:(T.Position.create ~line:start_line ~character:start_col)
          ~end_:(T.Position.create ~line:end_line ~character:end_col)
      in
      add name range;
      i := !j
    ) else (
      bump c;
      incr i
    )
  done;

  Hashtbl.iter (fun k xs -> Hashtbl.replace tbl k (List.rev xs)) tbl;
  tbl

(* ---------------- workspace indexing ---------------- *)

let rebuild_def_index (st:t) : unit =
  Hashtbl.clear st.def_index;
  Hashtbl.iter
    (fun _ entry ->
      let syms = Symbols.globals_in_order entry.analysis.symbols in
      List.iter
        (fun s ->
          let name = Symbols.sym_name s in
          let loc =
            T.Location.create ~uri:entry.uri ~range:(Symbols.symbol_name_range s)
          in
          let item = { loc; ty = Symbols.sym_ty s } in
          let prev = Option.value (Hashtbl.find_opt st.def_index name) ~default:[] in
          Hashtbl.replace st.def_index name (item :: prev))
        syms)
    st.ws_files

let index_entry (st:t) ~(uri:T.DocumentUri.t) ~(path:string option) ~(text:string) : unit =
  let analysis = analyze_text ~uri ~text in
  let refs = scan_ident_ranges ~text in
  Hashtbl.replace st.ws_files (uri_key uri) { uri; path; analysis; refs }

let reindex_workspace (st:t) : unit =
  Hashtbl.clear st.ws_files;
  Hashtbl.clear st.def_index;

  let roots = st.root_uris |> List.filter_map path_of_file_uri in
  let files = List.fold_left (fun acc r -> walk_j73_files r acc) [] roots in

  List.iter
    (fun path ->
      match read_file path with
      | None -> ()
      | Some text ->
          let uri = uri_of_path path in
          index_entry st ~uri ~path:(Some path) ~text)
    files;

  (* overlay any open docs *)
  Hashtbl.iter
    (fun _ d ->
      let uri = Lsp.Text_document.documentUri d.td in
      let text = Lsp.Text_document.text d.td in
      let path = path_of_file_uri (uri_key uri) in
      index_entry st ~uri ~path ~text)
    st.docs;

  rebuild_def_index st

let set_workspace_roots (st:t) (roots:string list) : unit =
  st.root_uris <- roots;
  reindex_workspace st

let set_workspace_root (st:t) (root:string) : unit =
  set_workspace_roots st [root]

(* ---------------- didOpen / didChange / didClose ---------------- *)

let did_open (st : t) (p : T.DidOpenTextDocumentParams.t) : T.PublishDiagnosticsParams.t =
  let td = Lsp.Text_document.make ~position_encoding:st.position_encoding p in
  let uri = p.textDocument.uri in
  let analysis = analyze_doc td in
  let diags = analysis.diagnostics in

  set_doc st { td; analysis };

  (* update workspace index for this file *)
  let path = path_of_file_uri (uri_key uri) in
  index_entry st ~uri ~path ~text:(Lsp.Text_document.text td);
  rebuild_def_index st;

  publish_diagnostics ~uri ~version:p.textDocument.version diags

let full_text_from_changes (changes : T.TextDocumentContentChangeEvent.t list) : string option =
  let rec loop = function
    | [] -> None
    | c :: tl ->
        match c.range with
        | None -> Some c.text
        | Some _ -> loop tl
  in
  loop changes

let did_change (st : t) (p : T.DidChangeTextDocumentParams.t)
  : T.PublishDiagnosticsParams.t
=
  let uri = p.textDocument.uri in
  match get_doc st uri with
  | None ->
      publish_diagnostics ~uri []
  | Some d ->
      let td' =
        try
          Lsp.Text_document.apply_content_changes
            ~version:p.textDocument.version
            d.td
            p.contentChanges
        with _ ->
          (* Don't crash; attempt full-text replacement if available *)
          match full_text_from_changes p.contentChanges with
          | None -> d.td
          | Some txt ->
              let open_params =
                T.DidOpenTextDocumentParams.create
                  ~textDocument:(T.TextDocumentItem.create
                                   ~uri
                                   ~languageId:"jovial"
                                   ~version:p.textDocument.version
                                   ~text:txt)
              in
              Lsp.Text_document.make ~position_encoding:st.position_encoding open_params
      in
      d.td <- td';

      let analysis = analyze_doc td' in
      d.analysis <- analysis;
      let diags = analysis.diagnostics in

      let path = path_of_file_uri (uri_key uri) in
      index_entry st ~uri ~path ~text:(Lsp.Text_document.text td');
      rebuild_def_index st;

      publish_diagnostics ~uri ~version:p.textDocument.version diags

let did_close (st : t) (p : T.DidCloseTextDocumentParams.t) : T.PublishDiagnosticsParams.t =
  Hashtbl.remove st.docs (uri_key p.textDocument.uri);
  publish_diagnostics ~uri:p.textDocument.uri []

(* ---------------- Hover / Definition / References ---------------- *)

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
                match Symbols.sym_ty s with
                | None -> Symbols.sym_name s
                | Some ty -> Symbols.sym_name s ^ " : " ^ ty
              in
              let body =
                match Symbols.sym_doc s with
                | None -> "```jovial\n" ^ code ^ "\n```"
                | Some doc -> "```jovial\n" ^ code ^ "\n```\n\n" ^ doc
              in
              let mc = T.MarkupContent.create ~kind:T.MarkupKind.Markdown ~value:body in
              Some
                (T.Hover.create
                   ~contents:(`MarkupContent mc)
                   ~range:(Symbols.symbol_name_range s)
                   ())
    )

let definition (st : t) (p : T.DefinitionParams.t) : T.Locations.t option =
  match get_doc st p.textDocument.uri with
  | None -> None
  | Some d -> (
      match identifier_at_position d.td p.position with
      | None -> None
      | Some name ->
          let pn = proc_name_at d.td d.analysis p.position in
          match lookup_symbol d.analysis ~proc_name:pn name with
          | Some s ->
              let uri = Lsp.Text_document.documentUri d.td in
              let loc = T.Location.create ~uri ~range:(Symbols.symbol_name_range s) in
              Some (`Location [ loc ])
          | None ->
              (match Hashtbl.find_opt st.def_index name with
               | None -> None
               | Some items ->
                   Some (`Location (List.rev_map (fun x -> x.loc) items)))
    )

let references_at (st:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : T.Location.t list =
  match get_doc st uri with
  | None -> []
  | Some d ->
      match identifier_at_position d.td pos with
      | None -> []
      | Some name ->
          let acc = ref [] in
          Hashtbl.iter
            (fun _ entry ->
              match Hashtbl.find_opt entry.refs name with
              | None -> ()
              | Some ranges ->
                  List.iter
                    (fun r ->
                      acc := T.Location.create ~uri:entry.uri ~range:r :: !acc)
                    ranges)
            st.ws_files;
          List.rev !acc

let references (st:t) (p:T.ReferenceParams.t) : T.Location.t list option =
  Some (references_at st ~uri:p.textDocument.uri ~pos:p.position)

(* ---------------- Document symbols / Completion ---------------- *)

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
          match Symbols.sym_container s with
          | None -> ()
          | Some c ->
              let prev = Option.value (Hashtbl.find_opt by_container c) ~default:[] in
              Hashtbl.replace by_container c (s :: prev))
        syms;

      let rec build (s : Symbols.symbol) : T.DocumentSymbol.t =
        let children =
          Hashtbl.find_opt by_container (Symbols.sym_name s)
          |> Option.map (fun xs -> xs |> List.rev |> List.map build)
        in
        T.DocumentSymbol.create
          ~name:(Symbols.sym_name s)
          ~kind:(symbol_kind (Symbols.sym_kind s))
          ~range:(Symbols.symbol_decl_range s)
          ~selectionRange:(Symbols.symbol_name_range s)
          ?detail:(Symbols.sym_ty s)
          ?children
          ()
      in

      let tops = List.filter (fun s -> Symbols.sym_container s = None) syms |> List.rev in
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
        |> List.filter (fun s -> prefix = "" || starts_with ~prefix (Symbols.sym_name s))
        |> List.map (fun s ->
               let documentation =
                 match Symbols.sym_doc s with
                 | None -> None
                 | Some txt -> Some (`String txt)
               in
               T.CompletionItem.create
                 ~label:(Symbols.sym_name s)
                 ~kind:(completion_kind (Symbols.sym_kind s))
                 ?detail:(Symbols.sym_ty s)
                 ?documentation
                 ())
      in
      let cl = T.CompletionList.create ~isIncomplete:false ~items () in
      Some (`CompletionList cl)

(* ---------------- Code actions (stub) ---------------- *)

let code_actions_json (_st:t) ~uri:(_uri:T.DocumentUri.t) : Yojson.Safe.t =
  `List []
