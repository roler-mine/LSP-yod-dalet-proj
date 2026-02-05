(* lib/symbols.mli *)

module T = Lsp.Types

type kind =
  | Item
  | Table
  | Proc
  | Param
  | Local
  | Type
  | Label

type symbol = private
  { name : string
  ; kind : kind
  ; decl_span : Ast.span
  ; name_span : Ast.span
  ; ty : string option
  ; doc : string option
  ; container : string option
  ; uri : T.DocumentUri.t option
  }

type add_error =
  { existing : symbol
  ; attempted : symbol
  }

type add_result = (unit, add_error) result

type t

val create : unit -> t

val make :
  name:string ->
  kind:kind ->
  decl_span:Ast.span ->
  name_span:Ast.span ->
  ?ty:string ->
  ?doc:string ->
  ?container:string ->
  ?uri:T.DocumentUri.t ->
  unit ->
  symbol

val with_uri : symbol -> T.DocumentUri.t -> symbol
val with_container : symbol -> string -> symbol

val add_global : t -> symbol -> add_result
val add_proc : t -> symbol -> add_result
val add_in_proc : t -> proc_name:string -> symbol -> add_result

val find : ?proc_name:string -> t -> string -> symbol option

val globals_in_order : t -> symbol list
val all : t -> symbol list

val symbol_decl_range : symbol -> T.Range.t
val symbol_name_range : symbol -> T.Range.t

val lsp_symbol_kind : kind -> T.SymbolKind.t

(* accessors *)
val sym_name : symbol -> string
val sym_kind : symbol -> kind
val sym_decl_span : symbol -> Ast.span
val sym_name_span : symbol -> Ast.span
val sym_ty : symbol -> string option
val sym_doc : symbol -> string option
val sym_container : symbol -> string option
val sym_uri : symbol -> T.DocumentUri.t option

module Workspace : sig
  type doc =
    { d_uri : T.DocumentUri.t
    ; d_symbols : t
    }

  type ws

  val create : unit -> ws
  val upsert_doc : ws -> doc -> unit
  val remove_doc : ws -> T.DocumentUri.t -> unit
  val clear : ws -> unit

  val find_def : ws -> string -> (T.DocumentUri.t * symbol) option
  val find_defs : ws -> string -> (T.DocumentUri.t * symbol) list
  val all_symbols : ws -> (T.DocumentUri.t * symbol) list
  val search : ws -> query:string -> (T.DocumentUri.t * symbol) list
end
