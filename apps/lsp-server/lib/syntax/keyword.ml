(* Module overview: Case-insensitive Jovial keyword classification shared by the lexer and parser. *)

open Parser

type class_ = Hard | Soft

let upper_char = function
  | 'a' .. 'z' as c -> Char.chr (Char.code c - 32)
  | c -> c

let equal_upper_ascii (s : string) (kw : string) : bool =
  let n = String.length s in
  n = String.length kw
  &&
  let rec loop i =
    i >= n || (upper_char s.[i] = kw.[i] && loop (i + 1))
  in
  loop 0

let match1 s a tok = if equal_upper_ascii s a then Some tok else None

let match2 s a toka b tokb =
  if equal_upper_ascii s a then Some toka
  else if equal_upper_ascii s b then Some tokb
  else None

let match3 s a toka b tokb c tokc =
  if equal_upper_ascii s a then Some toka
  else if equal_upper_ascii s b then Some tokb
  else if equal_upper_ascii s c then Some tokc
  else None

let match4 s a toka b tokb c tokc d tokd =
  if equal_upper_ascii s a then Some toka
  else if equal_upper_ascii s b then Some tokb
  else if equal_upper_ascii s c then Some tokc
  else if equal_upper_ascii s d then Some tokd
  else None

let classify (s : string) : Parser.token option =
  match String.length s with
  | 2 -> match3 s "BY" BY "IF" IF "OR" OR
  | 3 -> (
      match upper_char s.[0] with
      | 'A' -> match1 s "AND" AND
      | 'D' -> match1 s "DEF" DEF
      | 'E' -> match2 s "END" END "EQV" EQV
      | 'F' -> match1 s "FOR" FOR
      | 'M' -> match1 s "MOD" MOD
      | 'N' -> match1 s "NOT" NOT
      | 'P' -> match1 s "POS" POS
      | 'R' -> match1 s "REF" REF
      | 'X' -> match1 s "XOR" XOR
      | _ -> None)
  | 4 -> (
      match upper_char s.[0] with
      | 'C' -> match1 s "CASE" CASE
      | 'E' -> match2 s "ELSE" ELSE "EXIT" EXIT
      | 'G' -> match1 s "GOTO" GOTO
      | 'I' -> match1 s "ITEM" ITEM
      | 'P' -> match1 s "PROC" PROC
      | 'S' -> match1 s "STOP" STOP
      | 'T' -> match4 s "TERM" TERM "THEN" THEN "TRUE" TRUE "TYPE" TYPE
      | _ -> None)
  | 5 -> (
      match upper_char s.[0] with
      | 'A' -> match1 s "ABORT" ABORT
      | 'B' -> match2 s "BEGIN" BEGIN "BLOCK" BLOCK
      | 'F' -> match1 s "FALSE" FALSE
      | 'S' -> match1 s "START" START
      | 'T' -> match1 s "TABLE" TABLE
      | 'W' -> match1 s "WHILE" WHILE
      | _ -> None)
  | 6 -> (
      match upper_char s.[0] with
      | 'D' -> match1 s "DEFINE" DEFINE
      | 'I' -> match1 s "INLINE" INLINE
      | 'R' -> match1 s "RETURN" RETURN
      | 'S' -> match1 s "STATIC" STATIC
      | _ -> None)
  | 7 -> (
      match upper_char s.[0] with
      | 'C' -> match1 s "COMPOOL" COMPOOL
      | 'D' -> match1 s "DEFAULT" DEFAULT
      | 'O' -> match1 s "OVERLAY" OVERLAY
      | 'P' -> match1 s "PROGRAM" PROGRAM
      | _ -> None)
  | 8 ->
      match4 s "CONSTANT" CONSTANT "FALLTHRU" FALLTHRU "ICOMPOOL" ICOMPOOL
        "READONLY" READONLY
  | _ -> None

let class_of_token = function
  | PROGRAM | TYPE | BLOCK | DEFAULT -> Some Soft
  | START | TERM | BEGIN | END | DEF | REF | PROC | ITEM | TABLE | STATIC
  | CONSTANT | READONLY | INLINE | OVERLAY | IF | ELSE | WHILE | FOR | BY | THEN | CASE
  | FALLTHRU | EXIT | GOTO | RETURN | ABORT | STOP | TRUE | FALSE | NOT
  | AND | OR | XOR | EQV | MOD | POS | COMPOOL | ICOMPOOL | DEFINE ->
      Some Hard
  | _ -> None

let is_soft_token tok = match class_of_token tok with Some Soft -> true | _ -> false

let is_builtin_type_name (name : string) : bool =
  match String.trim name with
  | "" -> false
  | s -> (
      match String.length s with
      | 1 -> (
          match upper_char s.[0] with
          | 'A' | 'B' | 'U' | 'S' | 'F' | 'C' | 'P' | 'W' | 'V' -> true
          | _ -> false)
      | 6 -> equal_upper_ascii s "STATUS"
      | _ -> false)
