(* Module overview: Binary worker message protocol for internal parse-worker
   traffic. LSP client traffic remains JSON-RPC in Lsp_io; this module is only
   for server-internal worker queues and future process transports. *)

type job_kind = JobHighLarge | JobRootLarge | JobNormalLarge

type parse_job =
  | JobOpen of {
      kind : job_kind;
      epoch : int;
      path_key : string;
      uri : string;
      generation : int;
      text_hash : string;
      parse_profile : string;
      started_ms : float;
      doc_slot : int;
      file : string option;
      rev : int;
      lsp_version : int option;
    }
  | JobPath of {
      kind : job_kind;
      epoch : int;
      path : string;
      path_key : string;
    }

type parse_result =
  | ResultOpen of {
      kind : job_kind;
      epoch : int;
      path_key : string;
      uri : string;
      generation : int;
      text_hash : string;
      parse_profile : string;
      started_ms : float;
      doc_slot : int;
    }
  | ResultPath of {
      kind : job_kind;
      epoch : int;
      path : string;
      path_key : string;
      doc_slot : int option;
    }
  | ResultStale of { epoch : int; path_key : string }

type result_message_kind = ResultMessageOpen | ResultMessagePath | ResultMessageStale

let protocol_name = "j73-worker-binary"
let magic = "J73B"
let version = 2
let header_len = 11

let op_job_open = 0x01
let op_job_path = 0x02
let op_result_open = 0x11
let op_result_path = 0x12
let op_result_stale = 0x13

let job_kind_code = function
  | JobHighLarge -> 1
  | JobRootLarge -> 2
  | JobNormalLarge -> 3

let job_kind_of_code = function
  | 1 -> Ok JobHighLarge
  | 2 -> Ok JobRootLarge
  | 3 -> Ok JobNormalLarge
  | n -> Error (Printf.sprintf "unknown job kind %d" n)

let job_kind_to_string = function
  | JobHighLarge -> "highLarge"
  | JobRootLarge -> "rootLarge"
  | JobNormalLarge -> "normalLarge"

let lane_of_job_kind = function
  | JobHighLarge -> "open"
  | JobRootLarge -> "root"
  | JobNormalLarge -> "sweep"

let size_class_of_job_kind (_ : job_kind) = "large"

let put_u8 (b : Buffer.t) (n : int) : unit =
  if n < 0 || n > 0xFF then invalid_arg "Worker_ipc.put_u8";
  Buffer.add_char b (Char.chr n)

let put_i32_le (b : Buffer.t) (n : int) : unit =
  if n < -0x80000000 || n > 0x7FFFFFFF then
    invalid_arg "Worker_ipc.put_i32_le";
  let v = Int32.of_int n in
  put_u8 b (Int32.to_int (Int32.logand v 0xFFl));
  put_u8 b (Int32.to_int (Int32.logand (Int32.shift_right_logical v 8) 0xFFl));
  put_u8 b (Int32.to_int (Int32.logand (Int32.shift_right_logical v 16) 0xFFl));
  put_u8 b (Int32.to_int (Int32.logand (Int32.shift_right_logical v 24) 0xFFl))

let put_f64_le (b : Buffer.t) (f : float) : unit =
  let v = Int64.bits_of_float f in
  for shift = 0 to 7 do
    put_u8 b
      (Int64.to_int
         (Int64.logand (Int64.shift_right_logical v (shift * 8)) 0xFFL))
  done

let put_u16_le (b : Buffer.t) (n : int) : unit =
  if n < 0 || n > 0xFFFF then invalid_arg "Worker_ipc.put_u16_le";
  put_u8 b (n land 0xFF);
  put_u8 b ((n lsr 8) land 0xFF)

let put_string (b : Buffer.t) (s : string) : unit =
  let len = String.length s in
  put_i32_le b len;
  Buffer.add_string b s

let put_string_option (b : Buffer.t) = function
  | None -> put_u8 b 0
  | Some s ->
      put_u8 b 1;
      put_string b s

let put_int_option (b : Buffer.t) = function
  | None -> put_u8 b 0
  | Some n ->
      put_u8 b 1;
      put_i32_le b n

let encode_frame ~(opcode : int) (payload : string) : bytes =
  let b = Buffer.create (header_len + String.length payload) in
  Buffer.add_string b magic;
  put_u16_le b version;
  put_u8 b opcode;
  put_i32_le b (String.length payload);
  Buffer.add_string b payload;
  Bytes.of_string (Buffer.contents b)

type cursor = { bytes : bytes; mutable off : int; limit : int }

let fail msg = Error msg

let get_u8 (c : cursor) : (int, string) result =
  if c.off >= c.limit then fail "unexpected end of worker frame"
  else
    let n = Char.code (Bytes.get c.bytes c.off) in
    c.off <- c.off + 1;
    Ok n

let bind r f = match r with Ok x -> f x | Error _ as e -> e
let ( let* ) = bind

let get_i32_le (c : cursor) : (int, string) result =
  let* b0 = get_u8 c in
  let* b1 = get_u8 c in
  let* b2 = get_u8 c in
  let* b3 = get_u8 c in
  let v =
    Int32.logor (Int32.of_int b0)
      (Int32.logor
         (Int32.shift_left (Int32.of_int b1) 8)
         (Int32.logor
            (Int32.shift_left (Int32.of_int b2) 16)
            (Int32.shift_left (Int32.of_int b3) 24)))
  in
  Ok (Int32.to_int v)

let get_f64_le (c : cursor) : (float, string) result =
  let rec loop i acc =
    if i >= 8 then Ok (Int64.float_of_bits acc)
    else
      let* byte = get_u8 c in
      loop (i + 1)
        (Int64.logor acc
           (Int64.shift_left (Int64.of_int byte) (i * 8)))
  in
  loop 0 0L

let get_u16_le (c : cursor) : (int, string) result =
  let* b0 = get_u8 c in
  let* b1 = get_u8 c in
  Ok (b0 lor (b1 lsl 8))

let get_string (c : cursor) : (string, string) result =
  let* len = get_i32_le c in
  if len < 0 then fail "negative string length in worker frame"
  else if c.off + len > c.limit then fail "truncated string in worker frame"
  else
    let s = Bytes.sub_string c.bytes c.off len in
    c.off <- c.off + len;
    Ok s

let get_string_option (c : cursor) : (string option, string) result =
  let* tag = get_u8 c in
  match tag with
  | 0 -> Ok None
  | 1 ->
      let* s = get_string c in
      Ok (Some s)
  | n -> fail (Printf.sprintf "invalid optional string tag %d" n)

let get_int_option (c : cursor) : (int option, string) result =
  let* tag = get_u8 c in
  match tag with
  | 0 -> Ok None
  | 1 ->
      let* n = get_i32_le c in
      Ok (Some n)
  | n -> fail (Printf.sprintf "invalid optional int tag %d" n)

let decode_frame (frame : bytes) : ((int * cursor), string) result =
  let len = Bytes.length frame in
  if len < header_len then fail "worker frame too short"
  else if Bytes.sub_string frame 0 4 <> magic then fail "bad worker frame magic"
  else
    let header = { bytes = frame; off = 4; limit = header_len } in
    let* got_version = get_u16_le header in
    if got_version <> version then
      fail
        (Printf.sprintf "unsupported worker frame version %d" got_version)
    else
      let* opcode = get_u8 header in
      let* payload_len = get_i32_le header in
      if payload_len < 0 then fail "negative worker payload length"
      else if header_len + payload_len <> len then
        fail "worker frame payload length mismatch"
      else Ok (opcode, { bytes = frame; off = header_len; limit = len })

let ensure_consumed (c : cursor) : (unit, string) result =
  if c.off = c.limit then Ok () else fail "trailing bytes in worker frame"

let encode_parse_job (job : parse_job) : bytes =
  let b = Buffer.create 128 in
  let opcode =
    match job with
    | JobOpen x ->
        put_u8 b (job_kind_code x.kind);
        put_i32_le b x.epoch;
        put_string b x.path_key;
        put_string b x.uri;
        put_i32_le b x.generation;
        put_string b x.text_hash;
        put_string b x.parse_profile;
        put_f64_le b x.started_ms;
        put_i32_le b x.doc_slot;
        put_string_option b x.file;
        put_i32_le b x.rev;
        put_int_option b x.lsp_version;
        op_job_open
    | JobPath x ->
        put_u8 b (job_kind_code x.kind);
        put_i32_le b x.epoch;
        put_string b x.path;
        put_string b x.path_key;
        op_job_path
  in
  encode_frame ~opcode (Buffer.contents b)

let decode_parse_job (frame : bytes) : (parse_job, string) result =
  let* opcode, c = decode_frame frame in
  match opcode with
  | n when n = op_job_open ->
      let* kind_code = get_u8 c in
      let* kind = job_kind_of_code kind_code in
      let* epoch = get_i32_le c in
      let* path_key = get_string c in
      let* uri = get_string c in
      let* generation = get_i32_le c in
      let* text_hash = get_string c in
      let* parse_profile = get_string c in
      let* started_ms = get_f64_le c in
      let* doc_slot = get_i32_le c in
      let* file = get_string_option c in
      let* rev = get_i32_le c in
      let* lsp_version = get_int_option c in
      let* () = ensure_consumed c in
      Ok
        (JobOpen
           {
             kind;
             epoch;
             path_key;
             uri;
             generation;
             text_hash;
             parse_profile;
             started_ms;
             doc_slot;
             file;
             rev;
             lsp_version;
           })
  | n when n = op_job_path ->
      let* kind_code = get_u8 c in
      let* kind = job_kind_of_code kind_code in
      let* epoch = get_i32_le c in
      let* path = get_string c in
      let* path_key = get_string c in
      let* () = ensure_consumed c in
      Ok (JobPath { kind; epoch; path; path_key })
  | n -> fail (Printf.sprintf "worker frame opcode %d is not a parse job" n)

let encode_parse_result (res : parse_result) : bytes =
  let b = Buffer.create 128 in
  let opcode =
    match res with
    | ResultOpen x ->
        put_u8 b (job_kind_code x.kind);
        put_i32_le b x.epoch;
        put_string b x.path_key;
        put_string b x.uri;
        put_i32_le b x.generation;
        put_string b x.text_hash;
        put_string b x.parse_profile;
        put_f64_le b x.started_ms;
        put_i32_le b x.doc_slot;
        op_result_open
    | ResultPath x ->
        put_u8 b (job_kind_code x.kind);
        put_i32_le b x.epoch;
        put_string b x.path;
        put_string b x.path_key;
        put_int_option b x.doc_slot;
        op_result_path
    | ResultStale x ->
        put_i32_le b x.epoch;
        put_string b x.path_key;
        op_result_stale
  in
  encode_frame ~opcode (Buffer.contents b)

let decode_parse_result (frame : bytes) : (parse_result, string) result =
  let* opcode, c = decode_frame frame in
  match opcode with
  | n when n = op_result_open ->
      let* kind_code = get_u8 c in
      let* kind = job_kind_of_code kind_code in
      let* epoch = get_i32_le c in
      let* path_key = get_string c in
      let* uri = get_string c in
      let* generation = get_i32_le c in
      let* text_hash = get_string c in
      let* parse_profile = get_string c in
      let* started_ms = get_f64_le c in
      let* doc_slot = get_i32_le c in
      let* () = ensure_consumed c in
      Ok
        (ResultOpen
           {
             kind;
             epoch;
             path_key;
             uri;
             generation;
             text_hash;
             parse_profile;
             started_ms;
             doc_slot;
           })
  | n when n = op_result_path ->
      let* kind_code = get_u8 c in
      let* kind = job_kind_of_code kind_code in
      let* epoch = get_i32_le c in
      let* path = get_string c in
      let* path_key = get_string c in
      let* doc_slot = get_int_option c in
      let* () = ensure_consumed c in
      Ok (ResultPath { kind; epoch; path; path_key; doc_slot })
  | n when n = op_result_stale ->
      let* epoch = get_i32_le c in
      let* path_key = get_string c in
      let* () = ensure_consumed c in
      Ok (ResultStale { epoch; path_key })
  | n -> fail (Printf.sprintf "worker frame opcode %d is not a parse result" n)

let parse_job_path_key = function
  | JobOpen x -> x.path_key
  | JobPath x -> x.path_key

let parse_job_epoch = function JobOpen x -> x.epoch | JobPath x -> x.epoch
let parse_job_kind = function JobOpen x -> x.kind | JobPath x -> x.kind

let parse_result_message_kind (frame : bytes) :
    (result_message_kind, string) result =
  let* opcode, _ = decode_frame frame in
  match opcode with
  | n when n = op_result_open -> Ok ResultMessageOpen
  | n when n = op_result_path -> Ok ResultMessagePath
  | n when n = op_result_stale -> Ok ResultMessageStale
  | n -> fail (Printf.sprintf "worker frame opcode %d is not a parse result" n)

let is_binary_frame (frame : bytes) : bool =
  Bytes.length frame >= header_len && Bytes.sub_string frame 0 4 = magic
