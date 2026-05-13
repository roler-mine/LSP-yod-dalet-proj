type t = int

let of_int n = n land max_int

let stable_int (s : string) : int =
  let digest = Digest.to_hex (Digest.string s) in
  let acc = ref 0 in
  String.iter
    (fun c -> acc := ((!acc * 131) + Char.code c) land max_int)
    digest;
  !acc

let of_stable_string s = of_int (stable_int s)
let compare = Int.compare
let equal (a : t) (b : t) = a = b
let to_int x = x
let to_string x = string_of_int x
