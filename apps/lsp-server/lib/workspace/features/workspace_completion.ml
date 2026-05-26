(* Module overview: Completion provider for symbols, keywords, fields, and context-sensitive Jovial forms. *)

module T = Lsp.Types
open Workspace_state
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_navigation_support
module Metadata = Workspace_symbol_metadata

let completion_item_kind_of_def_kind (k : int) : int =
  if k = sym_kind_module then 9
  else if k = sym_kind_type then 7
  else if k = sym_kind_field then 10
  else if k = sym_kind_func then 3
  else if k = sym_kind_const then 21
  else 6

let completion_item_kind_of_metadata (d : def) : int =
  match d.metadata.Metadata.jovial_kind with
  | Metadata.JovialProgram | Metadata.JovialModule | Metadata.JovialCompool
  | Metadata.JovialCompoolImport | Metadata.JovialBlock ->
      9
  | Metadata.JovialOverlay -> 6
  | Metadata.JovialType | Metadata.JovialBuiltinType -> 7
  | Metadata.JovialField -> 10
  | Metadata.JovialProcedure | Metadata.JovialFunction -> 3
  | Metadata.JovialDefine | Metadata.JovialConstantItem
  | Metadata.JovialConstantTable | Metadata.JovialStatusConstant ->
      21
  | _ -> completion_item_kind_of_def_kind d.kind

let completion_keywords : (string * int * string option) list =
  [
    ("START", 14, None);
    ("TERM", 14, None);
    ("BEGIN", 14, None);
    ("END", 14, None);
    ("DEF", 14, None);
    ("REF", 14, None);
    ("STATIC", 14, None);
    ("CONSTANT", 14, None);
    ("READONLY", 14, Some "readonly data declaration");
    ("PROC", 14, Some "procedure declaration");
    ("ITEM", 14, Some "item declaration");
    ("TABLE", 14, Some "table declaration");
    ("TYPE", 14, Some "type declaration");
    ("IF", 14, None);
    ("THEN", 14, None);
    ("ELSE", 14, None);
    ("WHILE", 14, None);
    ("FOR", 14, None);
    ("BY", 14, None);
    ("FALLTHRU", 14, None);
    ("RETURN", 14, None);
    ("GOTO", 14, None);
    ("EXIT", 14, None);
    ("ABORT", 14, None);
    ("STOP", 14, None);
    ("CASE", 14, None);
    ("DEFAULT", 14, None);
    ("COMPOOL", 14, Some "compool directive");
    ("COPY", 14, Some "copy directive");
    ("SKIP", 14, Some "skip directive");
    ("BEGIN", 14, Some "directive scope begin");
    ("END", 14, Some "directive scope end");
    ("LINKAGE", 14, Some "linkage directive");
    ("TRACE", 14, Some "trace directive");
    ("INTERFERENCE", 14, Some "interference directive");
    ("REDUCIBLE", 14, Some "reducible directive");
    ("LIST", 14, Some "listing directive");
    ("NOLIST", 14, Some "listing directive");
    ("EJECT", 14, Some "listing directive");
    ("BASE", 14, Some "base directive");
    ("ISBASE", 14, Some "base directive");
    ("DROP", 14, Some "drop directive");
    ("LEFTRIGHT", 14, Some "layout directive");
    ("REARRANGE", 14, Some "rearrange directive");
    ("INITIALIZE", 14, Some "initialize directive");
    ("ORDER", 14, Some "order directive");
    ("DEFINE", 14, Some "macro directive");
    ("PROGRAM", 14, Some "program directive");
    ("BLOCK", 14, Some "block directive");
    ("REC", 14, Some "recursive subroutine");
    ("RENT", 14, Some "reentrant subroutine");
    ("LISTEXP", 14, Some "define list option");
    ("LISTINV", 14, Some "define list option");
    ("LISTBOTH", 14, Some "define list option");
    ("INLINE", 14, Some "inline declaration");
    ("LABEL", 14, Some "statement-name declaration");
    ("LIKE", 14, Some "table type option");
    ("OVERLAY", 14, Some "overlay declaration");
    ("PARALLEL", 14, Some "table structure option");
    ("POS", 14, Some "preset positioner");
    ("INSTANCE", 14, Some "def block instance");
    ("NULL", 14, Some "pointer literal");
    ("TRUE", 14, None);
    ("FALSE", 14, None);
    ("MOD", 14, None);
    ("AND", 14, None);
    ("OR", 14, None);
    ("NOT", 14, None);
    ("XOR", 14, None);
    ("EQV", 14, None);
  ]

let completion_types_builtin : (string * int * string option) list =
  [
    ("A", 7, Some "fixed type indicator");
    ("B", 7, Some "built-in type");
    ("U", 7, Some "built-in type");
    ("S", 7, Some "built-in type");
    ("F", 7, Some "built-in type");
    ("C", 7, Some "built-in type");
    ("P", 7, Some "built-in type");
    ("W", 7, Some "compatibility type marker");
    ("V", 7, Some "compatibility/status marker");
    ("STATUS", 7, Some "built-in type");
  ]

let completion_functions_builtin : (string * int * string option) list =
  [
    ("ABS", 3, Some "built-in function");
    ("BIT", 3, Some "built-in function");
    ("BITSIZE", 3, Some "built-in function");
    ("BYTE", 3, Some "built-in function");
    ("BYTESIZE", 3, Some "built-in function");
    ("FIRST", 3, Some "built-in function");
    ("LAST", 3, Some "built-in function");
    ("LBOUND", 3, Some "built-in function");
    ("LOC", 3, Some "built-in function");
    ("NEXT", 3, Some "built-in function");
    ("NWDSEN", 3, Some "built-in function");
    ("REP", 3, Some "built-in function");
    ("SGN", 3, Some "built-in function");
    ("SHIFTL", 3, Some "built-in function");
    ("SHIFTR", 3, Some "built-in function");
    ("UBOUND", 3, Some "built-in function");
    ("V", 3, Some "built-in status constructor");
    ("WORDSIZE", 3, Some "built-in function");
  ]

let completion_snippets : (string * string * int * string option) list =
  [
    ("!COMPOOL", "!COMPOOL(\"COMP\");", 15, Some "import compool");
    ("!COPY", "!COPY(\"INC.j73\");", 15, Some "copy include");
    ("!LINKAGE", "!LINKAGE BIF;", 15, Some "linkage directive");
  ]

let completion_item_kind_of_lsp_int (kind : int) : T.CompletionItemKind.t =
  if kind = 3 then T.CompletionItemKind.Function
  else if kind = 7 then T.CompletionItemKind.Class
  else if kind = 9 then T.CompletionItemKind.Module
  else if kind = 10 then T.CompletionItemKind.Property
  else if kind = 14 then T.CompletionItemKind.Keyword
  else if kind = 15 then T.CompletionItemKind.Snippet
  else if kind = 21 then T.CompletionItemKind.Constant
  else T.CompletionItemKind.Variable

let completion_item_t ~(label : string) ~(kind : T.CompletionItemKind.t) ?detail
    ?insert_text ?sort_text () : T.CompletionItem.t =
  T.CompletionItem.create ~label ~kind ?detail ?insertText:insert_text
    ?sortText:sort_text ()

let completion_items_for (ws : t) ~(uri : T.DocumentUri.t) ~(pos : T.Position.t)
    : T.CompletionItem.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        let prefix =
          match word_at_position doc pos with None -> "" | Some (nm, _) -> nm
        in
        let seen = Hashtbl.create 512 in
        let out = ref [] in
        let count = ref 0 in
        let max_items = 500 in
        let add_item ~(uniq_key : string) (item : T.CompletionItem.t) : unit =
          if nav_budget_check budget then ()
          else if !count < max_items && not (Hashtbl.mem seen uniq_key) then (
            Hashtbl.replace seen uniq_key true;
            out := item :: !out;
            incr count)
        in
        let add_symbol_item (d : def) : unit =
          if starts_with_ci ~prefix d.name then
            let detail =
              match d.container with
              | None -> Some (kind_name_of_def d)
              | Some c ->
                  Some (Printf.sprintf "%s in %s" (kind_name_of_def d) c)
            in
            let kind =
              completion_item_kind_of_lsp_int
                (completion_item_kind_of_metadata d)
            in
            let uniq_key =
              Printf.sprintf "sym|%s|%d" (normalize_name d.name) d.kind
            in
            let sort_text =
              if same_uri d.uri doc.Document.uri then
                Some ("0_" ^ normalize_name d.name)
              else Some ("1_" ^ normalize_name d.name)
            in
            add_item ~uniq_key
              (completion_item_t ~label:d.name ~kind ?detail ?sort_text ())
        in
        let add_keyword (label : string) (kind : int) (detail : string option) :
            unit =
          if starts_with_ci ~prefix label then
            let uniq_key = "kw|" ^ normalize_name label in
            add_item ~uniq_key
              (completion_item_t ~label
                 ~kind:(completion_item_kind_of_lsp_int kind)
                 ?detail
                 ~sort_text:("2_" ^ normalize_name label)
                 ())
        in
        let add_builtin_function (label : string) (kind : int)
            (detail : string option) : unit =
          if starts_with_ci ~prefix label then
            let uniq_key = "fn|" ^ normalize_name label in
            add_item ~uniq_key
              (completion_item_t ~label
                 ~kind:(completion_item_kind_of_lsp_int kind)
                 ?detail
                 ~sort_text:("3_" ^ normalize_name label)
                 ())
        in
        let add_snippet (label : string) (insert_text : string) (kind : int)
            (detail : string option) : unit =
          if starts_with_ci ~prefix label || starts_with_ci ~prefix insert_text
          then
            let uniq_key = "snip|" ^ normalize_name label in
            add_item ~uniq_key
              (completion_item_t ~label
                 ~kind:(completion_item_kind_of_lsp_int kind)
                 ?detail ~insert_text
                 ~sort_text:("4_" ^ normalize_name label)
                 ())
        in
        docs_for_lookup ws doc
        |> List.iter (fun d ->
               if not (nav_budget_check budget) then
                 collect_doc_defs d
                 |> List.iter (fun defn ->
                        if not (nav_budget_check budget) then
                          add_symbol_item defn));
        List.iter
          (fun (label, kind, detail) ->
            if not (nav_budget_check budget) then add_keyword label kind detail)
          completion_keywords;
        List.iter
          (fun (label, kind, detail) ->
            if not (nav_budget_check budget) then add_keyword label kind detail)
          completion_types_builtin;
        List.iter
          (fun (label, kind, detail) ->
            if not (nav_budget_check budget) then
              add_builtin_function label kind detail)
          completion_functions_builtin;
        List.iter
          (fun (label, insert_text, kind, detail) ->
            if not (nav_budget_check budget) then
              add_snippet label insert_text kind detail)
          completion_snippets;
        List.rev !out
      in
      nav_compute_with_budget_value budget compute
