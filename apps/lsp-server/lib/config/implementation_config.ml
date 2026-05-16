(* Module overview: Target-implementation profile settings for machine-specific Jovial semantics. *)

type t = {
  dialect : string option;
  bits_in_word : int option;
  bytes_in_word : int option;
  float_precision : int option;
  fixed_precision : int option;
  max_int_size : int option;
  max_bits : int option;
  max_bytes : int option;
  system_subroutines : string list;
}

type client_overrides = {
  dialect : string option;
  bits_in_word : int option;
  bytes_in_word : int option;
  float_precision : int option;
  fixed_precision : int option;
  max_int_size : int option;
  max_bits : int option;
  max_bytes : int option;
  system_subroutines : string list option;
}

let empty : t =
  {
    dialect = None;
    bits_in_word = None;
    bytes_in_word = None;
    float_precision = None;
    fixed_precision = None;
    max_int_size = None;
    max_bits = None;
    max_bytes = None;
    system_subroutines = [];
  }

let empty_client_overrides : client_overrides =
  {
    dialect = None;
    bits_in_word = None;
    bytes_in_word = None;
    float_precision = None;
    fixed_precision = None;
    max_int_size = None;
    max_bits = None;
    max_bytes = None;
    system_subroutines = None;
  }

let normalize_name name = String.uppercase_ascii (String.trim name)

let nonempty_string name =
  match Env_utils.nonempty_string name with
  | Some s -> Some s
  | None -> None

let positive_env_int ?legacy name =
  let read name =
    match Env_utils.nonempty_string name with
    | None -> None
    | Some raw -> (
        match int_of_string_opt (String.trim raw) with
        | Some n when n > 0 -> Some n
        | _ -> None)
  in
  match read name with
  | Some _ as hit -> hit
  | None -> Option.bind legacy read

let parse_system_subroutines raw =
  raw |> String.split_on_char ';'
  |> List.concat_map (fun s -> String.split_on_char ',' s)
  |> List.map normalize_name
  |> List.filter (fun s -> s <> "")
  |> List.fold_left
       (fun acc name -> if List.mem name acc then acc else name :: acc)
       []
  |> List.rev

let from_env () : t =
  {
    dialect =
      (match nonempty_string "JOVIAL_IMPLEMENTATION_DIALECT" with
      | Some _ as hit -> hit
      | None -> nonempty_string "JOVIAL_DIALECT");
    bits_in_word =
      positive_env_int ~legacy:"JOVIAL_LAYOUT_WORD_BITS"
        "JOVIAL_BITS_IN_WORD";
    bytes_in_word = positive_env_int "JOVIAL_BYTES_IN_WORD";
    float_precision = positive_env_int "JOVIAL_FLOAT_PRECISION";
    fixed_precision = positive_env_int "JOVIAL_FIXED_PRECISION";
    max_int_size = positive_env_int "JOVIAL_MAX_INT_SIZE";
    max_bits = positive_env_int "JOVIAL_MAX_BITS";
    max_bytes = positive_env_int "JOVIAL_MAX_BYTES";
    system_subroutines =
      (match Env_utils.nonempty_string "JOVIAL_SYSTEM_SUBROUTINES" with
      | None -> []
      | Some raw -> parse_system_subroutines raw);
  }

let choose override current =
  match override with Some value -> Some value | None -> current

let apply_client_overrides (base : t) (overrides : client_overrides) : t =
  {
    dialect = choose overrides.dialect base.dialect;
    bits_in_word = choose overrides.bits_in_word base.bits_in_word;
    bytes_in_word = choose overrides.bytes_in_word base.bytes_in_word;
    float_precision = choose overrides.float_precision base.float_precision;
    fixed_precision = choose overrides.fixed_precision base.fixed_precision;
    max_int_size = choose overrides.max_int_size base.max_int_size;
    max_bits = choose overrides.max_bits base.max_bits;
    max_bytes = choose overrides.max_bytes base.max_bytes;
    system_subroutines =
      (match overrides.system_subroutines with
      | None -> base.system_subroutines
      | Some names ->
          names |> List.map normalize_name |> List.filter (fun s -> s <> ""));
  }

let byte_size_bits (config : t) : int option =
  match (config.bits_in_word, config.bytes_in_word) with
  | Some bits, Some bytes when bits > 0 && bytes > 0 && bits mod bytes = 0 ->
      Some (bits / bytes)
  | _ -> None

let is_system_subroutine (config : t) name =
  let key = normalize_name name in
  key <> "" && List.mem key config.system_subroutines

let to_yojson (config : t) =
  let opt_int = function None -> `Null | Some n -> `Int n in
  `Assoc
    [
      ("dialect", (match config.dialect with None -> `Null | Some s -> `String s));
      ("bitsInWord", opt_int config.bits_in_word);
      ("bytesInWord", opt_int config.bytes_in_word);
      ("byteSizeBits", opt_int (byte_size_bits config));
      ("floatPrecision", opt_int config.float_precision);
      ("fixedPrecision", opt_int config.fixed_precision);
      ("maxIntSize", opt_int config.max_int_size);
      ("maxBits", opt_int config.max_bits);
      ("maxBytes", opt_int config.max_bytes);
      ( "systemSubroutines",
        `List (List.map (fun name -> `String name) config.system_subroutines) );
    ]
