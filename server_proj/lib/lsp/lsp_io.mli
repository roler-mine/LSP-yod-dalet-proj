val read_message : in_channel -> string option
val write_message : out_channel -> Yojson.Safe.t -> unit
