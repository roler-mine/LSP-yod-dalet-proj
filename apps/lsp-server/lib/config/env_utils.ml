let flag (name : string) ~(default : bool) : bool =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw -> (
      match String.lowercase_ascii (String.trim raw) with
      | "" -> default
      | "1" | "true" | "yes" | "on" -> true
      | "0" | "false" | "no" | "off" -> false
      | _ -> default)

let nonneg_int (name : string) ~(default : int) : int =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw -> (
      try
        let n = int_of_string (String.trim raw) in
        if n < 0 then default else n
      with _ -> default)

let nonempty_string (name : string) : string option =
  match Sys.getenv_opt name with
  | None -> None
  | Some raw ->
      let trimmed = String.trim raw in
      if trimmed = "" then None else Some trimmed
