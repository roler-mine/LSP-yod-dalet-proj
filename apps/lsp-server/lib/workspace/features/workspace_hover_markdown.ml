(* Module overview: Markdown rendering helpers for hover responses. *)

module T = Lsp.Types

let hover_markdown ?range (value : string) : T.Hover.t =
  let contents =
    `MarkupContent (T.MarkupContent.create ~kind:T.MarkupKind.Markdown ~value)
  in
  T.Hover.create ~contents ?range ()

let split_fact (fact : string) : (string * string) option =
  match String.index_opt fact ':' with
  | None -> None
  | Some i ->
      let label = String.sub fact 0 i |> String.trim in
      let value =
        String.sub fact (i + 1) (String.length fact - i - 1) |> String.trim
      in
      if label = "" || value = "" then None else Some (label, value)

let hover_table rows =
  let rows =
    rows
    |> List.filter (fun (_, value) -> String.trim value <> "")
    |> List.map (fun (field, value) ->
           Printf.sprintf "| %s | %s |" field value)
  in
  match rows with
  | [] -> ""
  | _ -> "\n\n| Field | Value |\n|---|---|\n" ^ String.concat "\n" rows

let hover_fact_block facts =
  match facts with
  | [] -> ""
  | _ ->
      let rows, bullets =
        facts
        |> List.fold_left
             (fun (rows, bullets) fact ->
               match split_fact fact with
               | Some row -> (row :: rows, bullets)
               | None -> (rows, fact :: bullets))
             ([], [])
      in
      let table = hover_table (List.rev rows) in
      let bullet_block =
        match List.rev bullets with
        | [] -> ""
        | xs -> "\n\n" ^ String.concat "\n" (List.map (fun fact -> "- " ^ fact) xs)
      in
      table ^ bullet_block

let hover_section_block sections =
  match sections with [] -> "" | _ -> "\n\n" ^ String.concat "\n\n" sections

let hover_panel ~name ~summary ~facts ~sections =
  Printf.sprintf "### `%s`\n\n_%s_%s%s" name summary (hover_fact_block facts)
    (hover_section_block sections)

let hover_code_section label text =
  Printf.sprintf "**%s:**\n```jovial\n%s\n```" label text

let hover_inline_section label text = Printf.sprintf "**%s:** %s" label text
