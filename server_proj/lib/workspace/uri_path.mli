module T = Lsp.Types

val docuri_to_string : T.DocumentUri.t -> string
val docuri_of_string : string -> T.DocumentUri.t option

val file_path_of_uri_string : string -> string option
val file_path_of_uri : T.DocumentUri.t -> string option

val file_uri_of_path : string -> string
val docuri_of_path : string -> T.DocumentUri.t option
