(* Module overview: Produces code actions from diagnostics and workspace context. *)

module T = Lsp.Types
open Workspace_state
open Workspace_nav_lookup
open Workspace_navigation_support
open Workspace_imports

type import_hint = {
  symbol : string option;
  symbol_kind : string option;
  compools : string list;
}

let nonempty_string (s : string) : string option =
  let s = String.trim s in
  if s = "" then None else Some s

let normalized_nonempty (s : string) : string option =
  let k = normalize_name s in
  if k = "" then None else Some k

let unique_normalized (names : string list) : string list =
  let seen = Hashtbl.create 8 in
  let acc = ref [] in
  names
  |> List.iter (fun raw ->
         match normalized_nonempty raw with
         | None -> ()
         | Some key ->
             if not (Hashtbl.mem seen key) then (
               Hashtbl.replace seen key true;
               acc := key :: !acc));
  List.rev !acc

let assoc_string (fields : (string * Yojson.Safe.t) list) (key : string) :
    string option =
  match List.assoc_opt key fields with
  | Some (`String s) -> nonempty_string s
  | _ -> None

let assoc_string_list (fields : (string * Yojson.Safe.t) list) (key : string) :
    string list =
  match List.assoc_opt key fields with
  | Some (`List xs) ->
      xs
      |> List.filter_map (function `String s -> nonempty_string s | _ -> None)
  | Some (`String s) -> (
      match nonempty_string s with Some s -> [ s ] | None -> [])
  | _ -> []

let diagnostic_message (diag : T.Diagnostic.t) : string =
  match diag.message with `String s -> s | `MarkupContent mc -> mc.value

let between_matching_quotes (msg : string) (quote : char) : string option =
  let n = String.length msg in
  let rec first i =
    if i >= n then None
    else if msg.[i] = quote then Some i
    else first (i + 1)
  in
  let rec second i =
    if i >= n then None
    else if msg.[i] = quote then Some i
    else second (i + 1)
  in
  match first 0 with
  | None -> None
  | Some s -> (
      match second (s + 1) with
      | None -> None
      | Some e when e <= s + 1 -> None
      | Some e -> nonempty_string (String.sub msg (s + 1) (e - s - 1)))

let quoted_symbol_from_message (msg : string) : string option =
  match between_matching_quotes msg '"' with
  | Some s -> Some s
  | None -> between_matching_quotes msg '\''

let looks_like_import_hint (msg : string) : bool =
  let upper = String.uppercase_ascii msg in
  find_substring_index ~haystack:upper ~needle:"COMPOOL" <> None
  || find_substring_index ~haystack:upper ~needle:"IMPORT" <> None

let import_hint_of_data (diag : T.Diagnostic.t) : import_hint option =
  match diag.T.Diagnostic.data with
  | Some (`Assoc fields) -> (
      match assoc_string fields "kind" with
      | Some "missingCompool" ->
          let compools =
            match assoc_string fields "compool" with Some c -> [ c ] | None -> []
          in
          Some { symbol = None; symbol_kind = None; compools }
      | Some "missingImportHint" ->
          Some
            {
              symbol = assoc_string fields "symbol";
              symbol_kind = assoc_string fields "symbolKind";
              compools = assoc_string_list fields "compools";
            }
      | _ -> None)
  | _ -> None

let import_hint_of_message (msg : string) : import_hint option =
  if not (looks_like_import_hint msg) then None
  else
    let compools =
      match parse_missing_compool_name msg with
      | Some c -> [ c ]
      | None -> (
          match parse_compool_name_from_hint msg with
          | Some c -> [ c ]
          | None -> [])
    in
    let symbol = quoted_symbol_from_message msg in
    match compools with
    | [] -> None
    | _ -> Some { symbol; symbol_kind = None; compools }

let import_hint_of_diagnostic (diag : T.Diagnostic.t) : import_hint option =
  match import_hint_of_data diag with
  | Some h -> Some { h with compools = unique_normalized h.compools }
  | None -> (
      match import_hint_of_message (diagnostic_message diag) with
      | None -> None
      | Some h -> Some { h with compools = unique_normalized h.compools })

let code_action_data (fields : (string * Yojson.Safe.t) list) : Yojson.Safe.t =
  `Assoc fields

let quickfix_add_compool_code_action ~(doc : Document.t)
    ~(diag : T.Diagnostic.t) ~(symbol : string option) ~(compool : string) :
    T.CodeAction.t option =
  let key = normalize_name compool in
  if key = "" || has_import_for_compool doc key then None
  else
    let pos, append_after_line = import_insert_position doc in
    let text =
      if append_after_line then Printf.sprintf "\n!COMPOOL ('%s');" key
      else Printf.sprintf "!COMPOOL ('%s');\n" key
    in
    let data =
      let base =
        [
          ("kind", `String "jovial.addCompoolImport");
          ("compool", `String key);
        ]
      in
      let fields =
        match symbol with
        | Some s when String.trim s <> "" -> ("symbol", `String s) :: base
        | _ -> base
      in
      code_action_data fields
    in
    Some
      (T.CodeAction.create
         ~title:(Printf.sprintf "Add !COMPOOL ('%s')" key)
         ~kind:T.CodeActionKind.QuickFix ~isPreferred:true ~diagnostics:[ diag ]
         ~edit:(workspace_single_edit ~uri:doc.uri ~pos ~new_text:text)
         ~data ())

let compool_doc_uri (ws : t) ~(compool : string) : T.DocumentUri.t option =
  match resolve_compool_doc_uncached ws ~name:compool with
  | Some doc -> Some doc.uri
  | None -> None

let open_compool_code_action (ws : t) ~(diag : T.Diagnostic.t)
    ~(compool : string) : T.CodeAction.t option =
  let key = normalize_name compool in
  if key = "" then None
  else
    match compool_doc_uri ws ~compool:key with
    | None -> None
    | Some uri ->
        let uri_s = Uri_path.docuri_to_string uri in
        let title = Printf.sprintf "Open COMPOOL %s" key in
        let command =
          T.Command.create ~title ~command:"vscode.open"
            ~arguments:[ `String uri_s ] ()
        in
        Some
          (T.CodeAction.create ~title ~kind:T.CodeActionKind.QuickFix
             ~diagnostics:[ diag ] ~command
             ~data:
               (code_action_data
                  [
                    ("kind", `String "jovial.openCompool");
                    ("compool", `String key);
                    ("uri", `String uri_s);
                  ])
             ())

let search_workspace_code_action ~(diag : T.Diagnostic.t) ~(symbol : string) :
    T.CodeAction.t option =
  match nonempty_string symbol with
  | None -> None
  | Some symbol ->
      let title = Printf.sprintf "Search workspace for %s" symbol in
      let command =
        T.Command.create ~title ~command:"workbench.action.findInFiles"
          ~arguments:
            [
              `Assoc
                [
                  ("query", `String symbol);
                  ("triggerSearch", `Bool true);
                ];
            ]
          ()
      in
      Some
        (T.CodeAction.create ~title ~kind:T.CodeActionKind.QuickFix
           ~diagnostics:[ diag ] ~command
           ~data:
             (code_action_data
                [
                  ("kind", `String "jovial.searchWorkspaceSymbol");
                  ("symbol", `String symbol);
                ])
           ())

let actions_for_hint (ws : t) ~(doc : Document.t) ~(diag : T.Diagnostic.t)
    (hint : import_hint) : T.CodeAction.t list =
  let _hint_kind = hint.symbol_kind in
  let actions = ref [] in
  let add action =
    match action with None -> () | Some a -> actions := a :: !actions
  in
  let compools = unique_normalized hint.compools in
  compools
  |> List.iter (fun compool ->
         add
           (quickfix_add_compool_code_action ~doc ~diag ~symbol:hint.symbol
              ~compool);
         add (open_compool_code_action ws ~diag ~compool));
  if compools = [] || List.exists (has_import_for_compool doc) compools then
    Option.iter
      (fun symbol -> add (search_workspace_code_action ~diag ~symbol))
      hint.symbol;
  List.rev !actions

let code_actions_for (ws : t) ~(uri : T.DocumentUri.t) ~(range : T.Range.t) :
    T.CodeAction.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let diag_in_range (d : T.Diagnostic.t) : bool =
        range_intersects d.range range
      in
      let seen = Hashtbl.create 32 in
      let actions = ref [] in
      let add_action (action : T.CodeAction.t) =
        let key =
          match action.T.CodeAction.data with
          | Some data -> Yojson.Safe.to_string data
          | None -> action.T.CodeAction.title
        in
        if not (Hashtbl.mem seen key) then (
          Hashtbl.replace seen key true;
          actions := action :: !actions)
      in
      Document.diagnostics doc |> List.filter diag_in_range
      |> List.iter (fun diag ->
             match import_hint_of_diagnostic diag with
             | None -> ()
             | Some hint ->
                 actions_for_hint ws ~doc ~diag hint |> List.iter add_action);
      List.rev !actions
