module T = Lsp.Types

type t = {
  docs : (T.DocumentUri.t, Document.t) Hashtbl.t;
  mutable root_path : string option;
  mutable index : Workspace_index.t option;
}

let create () : t =
  { docs = Hashtbl.create 32; root_path = None; index = None }

let docuri_to_string (u:T.DocumentUri.t) : string =
  (* Most LSP OCaml libs define DocumentUri.t as Uri.t under the hood.
     This uses the generated yojson converter which is stable across versions. *)
  try
    match T.DocumentUri.yojson_of_t u with
    | `String s -> s
    | _ -> ""
  with _ -> ""

let file_path_of_uri (u:T.DocumentUri.t) : string option =
  let s = docuri_to_string u in
  let prefix = "file://" in
  if String.length s >= String.length prefix
     && String.sub s 0 (String.length prefix) = prefix
  then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else
    None

let set_root_uri (ws:t) (root_uri:T.DocumentUri.t option) : unit =
  ws.root_path <- (match root_uri with None -> None | Some u -> file_path_of_uri u)

let rescan (ws:t) : unit =
  match ws.root_path with
  | None -> ws.index <- None
  | Some root ->
      ws.index <- Some (Workspace_index.build ~root)

let compool_count (ws:t) : int =
  match ws.index with None -> 0 | Some idx -> Workspace_index.compool_count idx

let diag_missing_compool (loc:Ast.Loc.t) (name:string) : T.Diagnostic.t =
  Lsp_conv.diagnostic
    ~severity:T.DiagnosticSeverity.Error
    ~source:"import"
    ~message:("Missing COMPOOL: " ^ name)
    loc

let validate_imports (ws:t) (doc:Document.t) : T.Diagnostic.t list =
  match ws.index with
  | None -> []
  | Some idx ->
      doc
      |> Document.imports
      |> List.filter_map (fun (imp:Preprocess.import) ->
           match imp.kind with
           | Preprocess.Compool ->
               match Workspace_index.find_compool idx ~name:imp.name with
               | Some _ -> None
               | None -> Some (diag_missing_compool imp.loc imp.name))

let store_doc (ws:t) (uri:T.DocumentUri.t) (doc:Document.t) : unit =
  let import_diags = validate_imports ws doc in
  let doc = Document.with_import_diags import_diags doc in
  Hashtbl.replace ws.docs uri doc

let open_doc (ws:t) ~(uri:T.DocumentUri.t) ~(file:string option) ~(text:string) : unit =
  let doc = Document.make ~uri ~file ~text in
  store_doc ws uri doc

let change_doc (ws:t) ~(uri:T.DocumentUri.t) ~(changes:T.TextDocumentContentChangeEvent.t list) : unit =
  match Hashtbl.find_opt ws.docs uri with
  | None ->
      let base = Document.make ~uri ~file:None ~text:"" in
      let doc = Document.apply_changes_and_reparse ~changes base in
      store_doc ws uri doc
  | Some doc ->
      let doc' = Document.apply_changes_and_reparse ~changes doc in
      store_doc ws uri doc'

let close_doc (ws:t) ~(uri:T.DocumentUri.t) : unit =
  Hashtbl.remove ws.docs uri

let diagnostics_for (ws:t) ~(uri:T.DocumentUri.t) : T.Diagnostic.t list =
  match Hashtbl.find_opt ws.docs uri with
  | None -> []
  | Some doc -> Document.diagnostics doc

let ast_dump_for (ws:t) ~(uri:T.DocumentUri.t) : string option =
  match Hashtbl.find_opt ws.docs uri with
  | None -> None
  | Some doc -> Document.ast_dump doc

let lsp_pos_of_lex (p:Lexing.position) : Yojson.Safe.t =
  let line0 = max 0 (p.pos_lnum - 1) in
  let col0 = max 0 (p.pos_cnum - p.pos_bol) in
  `Assoc [ "line", `Int line0; "character", `Int col0 ]

let lsp_range_of_lex (sp:Lexing.position) (ep:Lexing.position) : Yojson.Safe.t =
  `Assoc [ "start", lsp_pos_of_lex sp; "end", lsp_pos_of_lex ep ]

let debug_report_for (ws:t) ~(uri:T.DocumentUri.t) ~(max_tokens:int) : Yojson.Safe.t =
  match Hashtbl.find_opt ws.docs uri with
  | None -> `Assoc [ "error", `String "document not open" ]
  | Some doc ->
      let root =
        match ws.root_path with
        | None -> `Null
        | Some p -> `String p
      in

      let compools =
        match ws.index with
        | None -> `Assoc [ "count", `Int 0; "sample", `List [] ]
        | Some idx ->
            let sample =
              Workspace_index.sample idx 10
              |> List.map (fun (k,v) -> `Assoc [ "name", `String k; "path", `String v ])
            in
            `Assoc [ "count", `Int (Workspace_index.compool_count idx); "sample", `List sample ]
      in

      let imports =
        Document.imports doc
        |> List.map (fun (imp:Preprocess.import) ->
             let resolved =
               match ws.index with
               | None -> None
               | Some idx -> Workspace_index.find_compool idx ~name:imp.name
             in
             `Assoc [
               "name", `String imp.name;
               "resolved", (match resolved with None -> `Null | Some p -> `String p);
               "loc",
               `Assoc [
                 "line", `Int imp.loc.start_pos.line;
                 "col", `Int imp.loc.start_pos.col;
               ];
             ])
      in

      let tokens =
        let text = Document.text doc in
        let lexbuf = Lexing.from_string text in
        let rec loop i acc =
          if i >= max_tokens then List.rev acc
          else
            let t = Lexer.token lexbuf in
            let sp = Lexing.lexeme_start_p lexbuf in
            let ep = Lexing.lexeme_end_p lexbuf in
            let lex = Lexing.lexeme lexbuf in
            let item =
              `Assoc [
                "token", `String (Parse.Debug.string_of_token t);
                "lexeme", `String lex;
                "range", lsp_range_of_lex sp ep;
              ]
            in
            match t with
            | Parser.EOF -> List.rev (item :: acc)
            | _ -> loop (i+1) (item :: acc)
        in
        loop 0 []
      in

      `Assoc [
        "uri", `String (docuri_to_string uri);
        "root", root;
        "compools", compools;
        "imports", `List imports;
        "tokens", `List tokens;
      ]
