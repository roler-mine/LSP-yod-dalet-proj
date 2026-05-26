(* Module overview: Computes implementation-dependent Jovial storage layout and size information. *)

open Ast

type implementation_config = {
  word_size_bits : int option;
  byte_size_bits : int option;
  float_precision : int option;
  fixed_precision : int option;
}

type size_bits = KnownBits of int64 | UnknownBits of string

type field_layout = {
  field_name : string;
  field_loc : Ast.Loc.t;
  field_type_display : string;
  start_bit : int64 option;
  start_word : int64 option;
  absolute_bit_offset : int64 option;
  size_bits : size_bits;
}

type table_kind = NormalTable | SpecifiedTable of string

type table_layout = {
  table_kind : table_kind;
  entry_size_bits : size_bits;
  total_size_bits : size_bits;
  fields : field_layout list;
  notes : string list;
}

type issue_kind = FieldOverlap | FieldExceedsEntry

type issue = {
  kind : issue_kind;
  loc : Ast.Loc.t;
  message : string;
}

let default_config =
  {
    word_size_bits = None;
    byte_size_bits = None;
    float_precision = None;
    fixed_precision = None;
  }

let positive_env_int name =
  match Sys.getenv_opt name with
  | None -> None
  | Some raw -> (
      match int_of_string_opt (String.trim raw) with
      | Some n when n > 0 -> Some n
      | _ -> None)

let config_from_env () =
  {
    word_size_bits =
      (match positive_env_int "JOVIAL_BITS_IN_WORD" with
      | Some _ as hit -> hit
      | None -> positive_env_int "JOVIAL_LAYOUT_WORD_BITS");
    byte_size_bits =
      (match
         ( positive_env_int "JOVIAL_BITS_IN_WORD",
           positive_env_int "JOVIAL_BYTES_IN_WORD" )
       with
      | Some bits, Some bytes when bits > 0 && bytes > 0 && bits mod bytes = 0
        ->
          Some (bits / bytes)
      | _ -> positive_env_int "JOVIAL_LAYOUT_BYTE_BITS");
    float_precision = positive_env_int "JOVIAL_FLOAT_PRECISION";
    fixed_precision = positive_env_int "JOVIAL_FIXED_PRECISION";
  }

let config_of_implementation_config (config : Implementation_config.t) =
  {
    word_size_bits = config.bits_in_word;
    byte_size_bits = Implementation_config.byte_size_bits config;
    float_precision = config.float_precision;
    fixed_precision = config.fixed_precision;
  }

let unknown reason = UnknownBits reason

let known n = if n >= 0L then KnownBits n else unknown "negative size"

let size_to_yojson = function
  | KnownBits n -> `Assoc [ ("knownBits", `Intlit (Int64.to_string n)) ]
  | UnknownBits reason ->
      `Assoc [ ("knownBits", `Null); ("unknownReason", `String reason) ]

let size_display = function
  | KnownBits n -> Printf.sprintf "%Ld bits" n
  | UnknownBits reason -> "unknown (" ^ reason ^ ")"

let kind_display = function
  | NormalTable -> "normal"
  | SpecifiedTable kind -> "specified " ^ kind

let eval_int (env : Jovial_compile_time.env) (e : Ast.expr Ast.node) :
    int64 option =
  match Jovial_compile_time.eval_expr ~env e with
  | Jovial_compile_time.Known (Jovial_compile_time.CtfInt n) -> Some n
  | _ -> None

let first_dim_int env dims =
  match dims with dim :: _ -> eval_int env dim | [] -> None

let dim_entry_count env (e : Ast.expr Ast.node) : int64 option =
  match e.v with
  | Ast.EName id when String.uppercase_ascii (String.trim id.v) = "*" -> None
  | Ast.ERange { lo; hi } -> (
      match (eval_int env lo, eval_int env hi) with
      | Some lo_n, Some hi_n when hi_n >= lo_n ->
          Some (Int64.succ (Int64.sub hi_n lo_n))
      | _ -> None)
  | _ -> (
      match eval_int env e with Some n when n > 0L -> Some n | _ -> None)

let dim_product env dims =
  let rec loop acc = function
    | [] -> Some acc
    | dim :: rest -> (
        match dim_entry_count env dim with
        | None -> None
        | Some n ->
            let next = Int64.mul acc n in
            if acc <> 0L && Int64.div next acc <> n then None
            else loop next rest)
  in
  match dims with [] -> Some 1L | _ -> loop 1L dims

let multiply_size size count =
  match (size, count) with
  | KnownBits bits, Some n when bits >= 0L && n >= 0L ->
      let total = Int64.mul bits n in
      if bits <> 0L && Int64.div total bits <> n then
        unknown "table size overflow"
      else KnownBits total
  | UnknownBits reason, _ -> UnknownBits reason
  | _, None -> unknown "table dimension is not a known compile-time integer"
  | _ -> unknown "negative table size"

let add_known_sizes lhs rhs =
  match (lhs, rhs) with
  | KnownBits a, KnownBits b -> known (Int64.add a b)
  | UnknownBits reason, _ | _, UnknownBits reason -> UnknownBits reason

let type_display (ty : Ast.type_expr Ast.node) =
  Jovial_type.display
    (Jovial_type.of_ast_type_expr (Jovial_type.empty_type_env ()) ty)

let builtin_logical_size (config : implementation_config)
    (env : Jovial_compile_time.env) (name : string)
    (dims : Ast.expr Ast.node list) : size_bits =
  let key = String.uppercase_ascii (String.trim name) in
  match key with
  | "U" | "B" -> (
      match first_dim_int env dims with
      | Some n when n >= 0L -> KnownBits n
      | _ -> unknown (key ^ " size is not a known compile-time integer"))
  | "S" -> (
      match first_dim_int env dims with
      | Some n when n >= 0L -> KnownBits (Int64.succ n)
      | _ ->
          unknown
            "signed integer magnitude size is not a known compile-time integer")
  | "W" -> (
      match first_dim_int env dims with
      | Some n when n >= 0L -> KnownBits n
      | _ -> (
          match config.word_size_bits with
          | Some n when n > 0 -> KnownBits (Int64.of_int n)
          | _ -> unknown "implementation word size is unknown"))
  | "F" -> (
      match first_dim_int env dims with
      | Some n when n >= 0L -> KnownBits n
      | _ -> (
          match config.float_precision with
          | Some n when n > 0 -> KnownBits (Int64.of_int n)
          | _ -> unknown "floating storage size is target-specific"))
  | "C" -> (
      match (first_dim_int env dims, config.byte_size_bits) with
      | Some chars, Some byte_bits when chars >= 0L && byte_bits > 0 ->
          KnownBits (Int64.mul chars (Int64.of_int byte_bits))
      | Some _, None -> unknown "implementation byte size is unknown"
      | _ -> unknown "character size is not a known compile-time integer")
  | "A" -> (
      match config.fixed_precision with
      | Some n when n > 0 -> KnownBits (Int64.of_int n)
      | _ -> unknown "fixed-point layout is target-specific")
  | "STATUS" | "V" -> unknown "status representation is target-specific"
  | "P" -> unknown "pointer size is target-specific"
  | _ -> unknown "named type layout is unknown"

let rec logical_size_of_type (config : implementation_config)
    (env : Jovial_compile_time.env) (ty : Ast.type_expr Ast.node) : size_bits =
  match ty.v with
  | Ast.TName id -> builtin_logical_size config env id.v []
  | Ast.TScalar { base; sizes; _ } ->
      let name =
        match base with
        | Ast.ScalarUnsigned -> "U"
        | Ast.ScalarSigned -> "S"
        | Ast.ScalarFloat -> "F"
        | Ast.ScalarFixed -> "A"
        | Ast.ScalarBit -> "B"
        | Ast.ScalarChar -> "C"
      in
      builtin_logical_size config env name sizes
  | Ast.TPointer _ -> unknown "pointer size is target-specific"
  | Ast.TStatus _ -> unknown "status representation is target-specific"
  | Ast.TFunc _ -> unknown "procedure layout is not data layout"
  | Ast.TArray { elem = { v = Ast.TName id; _ }; dims }
    when Workspace_symbol_metadata.is_builtin_type_name id.v ->
      builtin_logical_size config env id.v dims
  | Ast.TArray { elem; dims } ->
      multiply_size (logical_size_of_type config env elem) (dim_product env dims)
  | Ast.TSpecifiedTable { elem = _; dims; kind } ->
      let entry_size =
        match kind with
        | Ast.SpecTableW entry_size | Ast.SpecTableV (Some entry_size) -> (
            match eval_int env entry_size with
            | Some n when n >= 0L -> KnownBits n
            | _ ->
                unknown
                  "specified table entry size is not a known compile-time integer"
            )
        | Ast.SpecTableV None -> unknown "specified V table entry size is unknown"
      in
      multiply_size entry_size (dim_product env dims)
  | Ast.TRecord fields ->
      fields
      |> List.fold_left
           (fun acc field ->
             add_known_sizes acc
               (logical_size_of_type config env field.v.ftype))
           (KnownBits 0L)

let absolute_offset ~(config : implementation_config) ~(start_bit : int64)
    ~(start_word : int64) : int64 option =
  if start_bit < 0L || start_word <= 0L then None
  else if start_word = 1L then Some start_bit
  else
    match config.word_size_bits with
    | Some word_bits when word_bits > 0 ->
        Some
          (Int64.add
             (Int64.mul (Int64.pred start_word) (Int64.of_int word_bits))
             start_bit)
    | _ -> None

let positioned_field_layouts (config : implementation_config)
    (env : Jovial_compile_time.env) (fields : Ast.field_decl Ast.node list) :
    field_layout list =
  fields
  |> List.filter_map (fun field ->
         match field.v.fpos with
         | None -> None
         | Some pos ->
             let start_bit = eval_int env pos.pos_start_bit in
             let start_word = eval_int env pos.pos_start_word in
             let absolute_bit_offset =
               match (start_bit, start_word) with
               | Some start_bit, Some start_word ->
                   absolute_offset ~config ~start_bit ~start_word
               | _ -> None
             in
             Some
               {
                 field_name = field.v.fname.v;
                 field_loc = field.v.fname.loc;
                 field_type_display = type_display field.v.ftype;
                 start_bit;
                 start_word;
                 absolute_bit_offset;
                 size_bits = logical_size_of_type config env field.v.ftype;
               })

let sequential_field_layouts (config : implementation_config)
    (env : Jovial_compile_time.env) (fields : Ast.field_decl Ast.node list) :
    field_layout list =
  let _, layouts =
    fields
    |> List.fold_left
         (fun (offset, acc) field ->
           let size = logical_size_of_type config env field.v.ftype in
           let field_offset =
             match offset with KnownBits n -> Some n | UnknownBits _ -> None
           in
           let next_offset = add_known_sizes offset size in
           let layout =
             {
               field_name = field.v.fname.v;
               field_loc = field.v.fname.loc;
               field_type_display = type_display field.v.ftype;
               start_bit = field_offset;
               start_word = Some 1L;
               absolute_bit_offset = field_offset;
               size_bits = size;
             }
           in
           (next_offset, layout :: acc))
         (KnownBits 0L, [])
  in
  List.rev layouts

let issues_for_layout (layout : table_layout) : issue list =
  let range_of_field field =
    match (field.absolute_bit_offset, field.size_bits) with
    | Some start, KnownBits size when start >= 0L && size >= 0L ->
        Some (start, Int64.add start size)
    | _ -> None
  in
  let exceeds =
    match layout.entry_size_bits with
    | UnknownBits _ -> []
    | KnownBits entry_size ->
        layout.fields
        |> List.filter_map (fun field ->
               match range_of_field field with
               | Some (_, end_exclusive) when end_exclusive > entry_size ->
                   Some
                     {
                       kind = FieldExceedsEntry;
                       loc = field.field_loc;
                       message =
                         Printf.sprintf
                           "Table layout field %S exceeds entry size %Ld bits."
                           field.field_name entry_size;
                     }
               | _ -> None)
  in
  let overlaps =
    let rec loop acc = function
      | [] -> acc
      | field :: rest ->
          let acc =
            match range_of_field field with
            | None -> acc
            | Some (start_a, end_a) ->
                rest
                |> List.fold_left
                     (fun acc other ->
                       match range_of_field other with
                       | Some (start_b, end_b)
                         when start_a < end_b && start_b < end_a ->
                           {
                             kind = FieldOverlap;
                             loc = other.field_loc;
                             message =
                               Printf.sprintf
                                 "Table layout field %S overlaps %S."
                                 other.field_name field.field_name;
                           }
                           :: acc
                       | _ -> acc)
                     acc
          in
          loop acc rest
    in
    loop [] layout.fields
  in
  List.rev_append overlaps exceeds

let table_layout_of_type ?(config = default_config)
    (env : Jovial_compile_time.env) (ty : Ast.type_expr Ast.node) :
    table_layout option =
  match ty.v with
  | Ast.TSpecifiedTable { elem; dims; kind } ->
      let kind_name, entry_size =
        match kind with
        | Ast.SpecTableW entry_size -> ("W", Some entry_size)
        | Ast.SpecTableV None -> ("V", None)
        | Ast.SpecTableV (Some entry_size) -> ("V", Some entry_size)
      in
      let entry_size_bits =
        match entry_size with
        | Some expr -> (
            match eval_int env expr with
            | Some n when n >= 0L -> KnownBits n
            | _ ->
                unknown
                  "specified table entry size is not a known compile-time integer"
            )
        | None -> unknown "specified V table entry size is unknown"
      in
      let fields =
        match elem.v with
        | Ast.TRecord fields -> positioned_field_layouts config env fields
        | _ -> []
      in
      Some
        {
          table_kind = SpecifiedTable kind_name;
          entry_size_bits;
          total_size_bits = multiply_size entry_size_bits (dim_product env dims);
          fields;
          notes =
            [
              "Logical layout only; target-specific packing/code generation is \
               not modeled.";
            ];
        }
  | Ast.TArray { elem; dims } ->
      let entry_size_bits = logical_size_of_type config env elem in
      let fields =
        match elem.v with
        | Ast.TRecord fields -> sequential_field_layouts config env fields
        | _ -> []
      in
      Some
        {
          table_kind = NormalTable;
          entry_size_bits;
          total_size_bits = multiply_size entry_size_bits (dim_product env dims);
          fields;
          notes =
            [
              "Logical layout only; target-specific packing/code generation is \
               not modeled.";
            ];
        }
  | _ -> None

let issue_to_yojson issue =
  `Assoc
    [
      ( "kind",
        `String
          (match issue.kind with
          | FieldOverlap -> "fieldOverlap"
          | FieldExceedsEntry -> "fieldExceedsEntry") );
      ("message", `String issue.message);
      ( "range",
        `Assoc
          [
            ("line", `Int issue.loc.start_pos.line);
            ("col", `Int issue.loc.start_pos.col);
          ] );
    ]

let field_to_yojson field =
  `Assoc
    [
      ("name", `String field.field_name);
      ("type", `String field.field_type_display);
      ( "startBit",
        match field.start_bit with
        | None -> `Null
        | Some n -> `Intlit (Int64.to_string n) );
      ( "startWord",
        match field.start_word with
        | None -> `Null
        | Some n -> `Intlit (Int64.to_string n) );
      ( "absoluteBitOffset",
        match field.absolute_bit_offset with
        | None -> `Null
        | Some n -> `Intlit (Int64.to_string n) );
      ("sizeBits", size_to_yojson field.size_bits);
    ]

let table_layout_to_yojson layout =
  `Assoc
    [
      ("kind", `String (kind_display layout.table_kind));
      ("entrySizeBits", size_to_yojson layout.entry_size_bits);
      ("totalSizeBits", size_to_yojson layout.total_size_bits);
      ("fields", `List (List.map field_to_yojson layout.fields));
      ("issues", `List (List.map issue_to_yojson (issues_for_layout layout)));
      ("notes", `List (List.map (fun note -> `String note) layout.notes));
    ]
