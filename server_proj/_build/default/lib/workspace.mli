module T = Lsp.Types

type t

val create : unit -> t

val set_root_uri : t -> T.DocumentUri.t option -> unit
val set_root_path : t -> string option -> unit
val rescan : t -> unit
val revalidate_all : t -> T.DocumentUri.t list
val compool_count : t -> int

val open_doc : t -> uri:T.DocumentUri.t -> file:string option -> text:string -> unit
val change_doc : t -> uri:T.DocumentUri.t -> changes:T.TextDocumentContentChangeEvent.t list -> unit
val close_doc : t -> uri:T.DocumentUri.t -> unit

val diagnostics_for : t -> uri:T.DocumentUri.t -> T.Diagnostic.t list
val ast_dump_for : t -> uri:T.DocumentUri.t -> string option
val cst_dump_for : t -> uri:T.DocumentUri.t -> string option

val document_symbols_json_for : t -> uri:T.DocumentUri.t -> Yojson.Safe.t
val definition_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val implementation_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val references_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> include_decl:bool -> Yojson.Safe.t
val hover_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val prepare_rename_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Yojson.Safe.t
val rename_json_for : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> new_name:string -> Yojson.Safe.t
val inlay_hints_json_for : t -> uri:T.DocumentUri.t -> range:T.Range.t -> Yojson.Safe.t
val semantic_tokens_full_json_for : t -> uri:T.DocumentUri.t -> Yojson.Safe.t
val semantic_tokens_range_json_for : t -> uri:T.DocumentUri.t -> range:T.Range.t -> Yojson.Safe.t

val debug_report_for : t -> uri:T.DocumentUri.t -> max_tokens:int -> Yojson.Safe.t
