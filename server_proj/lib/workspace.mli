module T = Lsp.Types

type t

val create : unit -> t

val set_root_uri : t -> T.DocumentUri.t option -> unit
val rescan : t -> unit
val compool_count : t -> int

val open_doc : t -> uri:T.DocumentUri.t -> file:string option -> text:string -> unit
val change_doc : t -> uri:T.DocumentUri.t -> changes:T.TextDocumentContentChangeEvent.t list -> unit
val close_doc : t -> uri:T.DocumentUri.t -> unit

val diagnostics_for : t -> uri:T.DocumentUri.t -> T.Diagnostic.t list
val ast_dump_for : t -> uri:T.DocumentUri.t -> string option

val debug_report_for : t -> uri:T.DocumentUri.t -> max_tokens:int -> Yojson.Safe.t
