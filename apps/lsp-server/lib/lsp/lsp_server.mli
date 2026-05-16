(** Module overview: Top-level LSP event loop that dispatches client messages into workspace operations. *)

val run : in_channel -> out_channel -> unit

module Private_for_tests : sig
  val reorder_raw_messages_for_dispatch : string list -> string list
  val priority_queue_order : string list -> string list

  val incoming_preempts_active_method :
    active:string -> incoming:string -> bool

  val open_document_workspace_survives_source_set_replacement :
    workspace_root:string -> source_root:string -> source_file:string -> bool
end
