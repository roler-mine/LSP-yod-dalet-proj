module T = Lsp.Types
open Workspace_state
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_navigation_support

let split_signature_params (label : string) : string list =
  match String.index_opt label '(' with
  | None -> []
  | Some i0 -> (
      match String.rindex_opt label ')' with
      | None -> []
      | Some i1 when i1 <= i0 -> []
      | Some i1 ->
          let inside = String.sub label (i0 + 1) (i1 - i0 - 1) |> String.trim in
          if inside = "" then []
          else
            inside |> String.split_on_char ',' |> List.map String.trim
            |> List.filter (fun s -> s <> ""))

let call_context_at_position (doc : Document.t) (pos : T.Position.t) :
    (string * int) option =
  match
    Text_index.offset_of_line_col doc.Document.index ~line:pos.line
      ~col:pos.character
  with
  | None -> None
  | Some raw_cursor -> (
      let text = doc.Document.text in
      let n = String.length text in
      let cursor = max 0 (min n raw_cursor) in
      let stack : int list ref = ref [] in
      let in_single = ref false in
      let in_double = ref false in
      let i = ref 0 in
      while !i < cursor do
        let c = text.[!i] in
        (if !in_single then (
           if c = '\'' then
             if !i + 1 < cursor && text.[!i + 1] = '\'' then i := !i + 1
             else in_single := false)
         else if !in_double then (
           if c = '"' then
             if !i + 1 < cursor && text.[!i + 1] = '"' then i := !i + 1
             else in_double := false)
         else
           match c with
           | '\'' -> in_single := true
           | '"' -> in_double := true
           | '(' -> stack := !i :: !stack
           | ')' -> ( match !stack with _ :: tl -> stack := tl | [] -> ())
           | _ -> ());
        incr i
      done;
      match !stack with
      | [] -> None
      | open_idx :: _ ->
          let rec skip_ws_left j =
            if j < 0 then -1
            else
              match text.[j] with
              | ' ' | '\t' | '\r' | '\n' -> skip_ws_left (j - 1)
              | _ -> j
          in
          let j0 = skip_ws_left (open_idx - 1) in
          if j0 < 0 then None
          else
            let rec start_ident j =
              if j >= 0 && is_ident_char text.[j] then start_ident (j - 1)
              else j + 1
            in
            let start = start_ident j0 in
            if start > j0 then None
            else
              let name = String.sub text start (j0 - start + 1) in
              let key = normalize_name name in
              if key = "" then None
              else
                let depth = ref 1 in
                let in_single = ref false in
                let in_double = ref false in
                let commas = ref 0 in
                let k = ref (open_idx + 1) in
                while !k < cursor do
                  let c = text.[!k] in
                  (if !in_single then (
                     if c = '\'' then
                       if !k + 1 < cursor && text.[!k + 1] = '\'' then
                         k := !k + 1
                       else in_single := false)
                   else if !in_double then (
                     if c = '"' then
                       if !k + 1 < cursor && text.[!k + 1] = '"' then
                         k := !k + 1
                       else in_double := false)
                   else
                     match c with
                     | '\'' -> in_single := true
                     | '"' -> in_double := true
                     | '(' -> incr depth
                     | ')' -> if !depth > 0 then decr depth
                     | ',' when !depth = 1 -> incr commas
                     | _ -> ());
                  incr k
                done;
                Some (key, max 0 !commas))

let signature_help_for (ws : t) ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    T.SignatureHelp.t option =
  match doc_of_uri ws uri with
  | None -> None
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then None
        else
          match call_context_at_position doc pos with
          | None -> None
          | Some (key, active_param) ->
              let defs =
                if nav_budget_check budget then []
                else
                  let docs = docs_for_lookup ws doc in
                  let from_docs =
                    docs
                    |> List.concat_map collect_doc_defs
                    |> List.filter (fun d -> d.kind = sym_kind_func && d.key = key)
                    |> uniq_defs
                  in
                  if from_docs <> [] then from_docs
                  else if allow_fallback_for_ws ws doc then
                    proc_defs_by_key ws doc ~key
                  else []
              in
              if defs = [] then None
              else
                let signatures =
                  defs
                  |> List.filter_map (fun d ->
                         if nav_budget_check budget then None
                         else
                           let parameters_of_label label =
                             split_signature_params label
                             |> List.map (fun p ->
                                    T.ParameterInformation.create
                                      ~label:(`String p) ())
                           in
                           match proc_signature_for_def ws d with
                           | Some label ->
                               Some
                                 (T.SignatureInformation.create ~label
                                    ~parameters:(parameters_of_label label)
                                    ())
                           | None ->
                               Some
                                 (T.SignatureInformation.create ~label:d.name
                                    ~parameters:[] ()))
                in
                if signatures = [] then None
                else
                  Some
                    (T.SignatureHelp.create ~signatures ~activeSignature:0
                       ~activeParameter:(Some active_param) ())
      in
      nav_compute_with_budget_value budget compute
