(** Module overview: Reads and writes JSON-RPC/LSP messages over Content-Length framed stdio. *)

type read_result =
  [ `Eof | `Message of string | `Oversize of int | `Invalid of string ]

val read_message_with_limit : in_channel -> max_len:int -> read_result
val read_message : in_channel -> string option
val write_message : out_channel -> Yojson.Safe.t -> unit
