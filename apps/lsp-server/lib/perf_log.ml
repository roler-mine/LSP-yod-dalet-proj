(* Module overview: Shared performance logging helpers for server scheduling and request timing. *)

type parse_policy =
  | Forbid_sync_parse
  | Allow_sync_parse_if_small
  | Force_background

type request_kind =
  | Hover
  | Completion
  | Definition
  | References
  | DocumentSymbol
  | SemanticTokensRange
  | SemanticTokensFull
  | Diagnostics
  | BackgroundIndex

let string_of_request_kind = function
  | Hover -> "hover"
  | Completion -> "completion"
  | Definition -> "definition"
  | References -> "references"
  | DocumentSymbol -> "documentSymbol"
  | SemanticTokensRange -> "semanticTokensRange"
  | SemanticTokensFull -> "semanticTokensFull"
  | Diagnostics -> "diagnostics"
  | BackgroundIndex -> "backgroundIndex"

let request_allows_sync_parse = function
  | Hover | Completion | SemanticTokensRange -> false
  | Definition | References | DocumentSymbol -> false
  | SemanticTokensFull | Diagnostics | BackgroundIndex ->
      true

let request_kind_of_lsp_method = function
  | "textDocument/hover" -> Some Hover
  | "textDocument/completion" -> Some Completion
  | "textDocument/definition" | "textDocument/declaration"
  | "textDocument/typeDefinition" | "textDocument/implementation" ->
      Some Definition
  | "textDocument/references" -> Some References
  | "textDocument/documentSymbol" -> Some DocumentSymbol
  | "textDocument/semanticTokens/range" -> Some SemanticTokensRange
  | "textDocument/semanticTokens/full"
  | "textDocument/semanticTokens/full/delta" ->
      Some SemanticTokensFull
  | "textDocument/diagnostic" -> Some Diagnostics
  | _ -> None

let now_ms () : float = Unix.gettimeofday () *. 1000.0

let request_mtx = Mutex.create ()
let request_by_thread : (int, request_kind) Hashtbl.t = Hashtbl.create 16

let current_thread_id () = Thread.id (Thread.self ())

let current_request_kind () : request_kind option =
  Mutex.lock request_mtx;
  let out = Hashtbl.find_opt request_by_thread (current_thread_id ()) in
  Mutex.unlock request_mtx;
  out

let with_request_kind (kind : request_kind) (f : unit -> 'a) : 'a =
  let tid = current_thread_id () in
  Mutex.lock request_mtx;
  let previous = Hashtbl.find_opt request_by_thread tid in
  Hashtbl.replace request_by_thread tid kind;
  Mutex.unlock request_mtx;
  Fun.protect
    ~finally:(fun () ->
      Mutex.lock request_mtx;
      (match previous with
      | None -> Hashtbl.remove request_by_thread tid
      | Some old -> Hashtbl.replace request_by_thread tid old);
      Mutex.unlock request_mtx)
    f

let with_request_kind_opt (kind : request_kind option) (f : unit -> 'a) : 'a =
  match kind with None -> f () | Some kind -> with_request_kind kind f

let counter_mtx = Mutex.create ()
let counters : (string, int) Hashtbl.t = Hashtbl.create 32

let tick_counter (name : string) : unit =
  Mutex.lock counter_mtx;
  let next =
    match Hashtbl.find_opt counters name with
    | None -> 1
    | Some n -> n + 1
  in
  Hashtbl.replace counters name next;
  Mutex.unlock counter_mtx

let counter_value (name : string) : int =
  Mutex.lock counter_mtx;
  let out = Option.value (Hashtbl.find_opt counters name) ~default:0 in
  Mutex.unlock counter_mtx;
  out

let reset () : unit =
  Mutex.lock counter_mtx;
  Hashtbl.clear counters;
  Mutex.unlock counter_mtx

let counters_json () : Yojson.Safe.t =
  Mutex.lock counter_mtx;
  let entries =
    Hashtbl.fold
      (fun name count acc ->
        `Assoc [ ("name", `String name); ("count", `Int count) ] :: acc)
      counters []
    |> List.sort (fun a b ->
           let name = function
             | `Assoc fields -> (
                 match List.assoc_opt "name" fields with
                 | Some (`String s) -> s
                 | _ -> "")
             | _ -> ""
           in
           String.compare (name a) (name b))
  in
  Mutex.unlock counter_mtx;
  `List entries

let fmt_opt_string = function None -> "none" | Some s -> s
let fmt_opt_int = function None -> "none" | Some n -> string_of_int n

let fmt_opt_float = function
  | None -> "none"
  | Some f -> Printf.sprintf "%.2f" f

let log_event ?uri ?bytes ?rev ?ms ?queue (event : string) : unit =
  prerr_endline
    (Printf.sprintf
       "[perf] event=%s uri=%s bytes=%s rev=%s ms=%s queue=%s" event
       (fmt_opt_string uri) (fmt_opt_int bytes) (fmt_opt_int rev)
       (fmt_opt_float ms) (fmt_opt_int queue))

let counter_name_for_sync_full_parse = function
  | Hover -> Some "sync_full_parse_from_hover"
  | Completion -> Some "sync_full_parse_from_completion"
  | SemanticTokensRange -> Some "sync_full_parse_from_semantic_tokens_range"
  | Definition -> Some "sync_full_parse_from_definition"
  | References -> Some "sync_full_parse_from_references"
  | DocumentSymbol -> Some "sync_full_parse_from_document_symbol"
  | SemanticTokensFull -> Some "sync_full_parse_from_semantic_tokens_full"
  | Diagnostics -> Some "sync_full_parse_from_diagnostics"
  | BackgroundIndex -> None

let record_sync_full_parse ?uri ?bytes ?rev () : unit =
  match current_request_kind () with
  | None -> ()
  | Some kind -> (
      match counter_name_for_sync_full_parse kind with
      | None -> ()
      | Some counter ->
          tick_counter counter;
          log_event ?uri ?bytes ?rev
            ~queue:(counter_value counter)
            ("sync_full_parse_from_" ^ string_of_request_kind kind))

let record_sync_workspace_nav_rebuild ?uri ?bytes ?rev () : unit =
  match current_request_kind () with
  | Some Hover ->
      let counter = "sync_workspace_nav_rebuild_from_hover" in
      tick_counter counter;
      log_event ?uri ?bytes ?rev
        ~queue:(counter_value counter)
        "sync_workspace_nav_rebuild_from_hover"
  | _ -> ()
