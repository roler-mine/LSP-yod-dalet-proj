type t = {
  mutable revision : int;
  symbols : (string, Yojson.Safe.t) Hashtbl.t;
}

type delta = {
  base_revision : int;
  revision : int;
  reset : bool;
  deletes : string list;
  upserts : Yojson.Safe.t list;
}

let create () : t =
  { revision = 0; symbols = Hashtbl.create 2048 }

let revision (t:t) : int =
  t.revision

let reset (t:t) : unit =
  t.revision <- 0;
  Hashtbl.clear t.symbols

let symbols_of_index_json (j:Yojson.Safe.t) : (string, Yojson.Safe.t) Hashtbl.t =
  let out = Hashtbl.create 2048 in
  let fields =
    match j with
    | `Assoc xs -> xs
    | _ -> []
  in
  let symbols =
    match List.assoc_opt "symbols" fields with
    | Some (`List xs) -> xs
    | _ -> []
  in
  List.iter (fun item ->
    match item with
    | `Assoc sfields ->
        (match List.assoc_opt "id" sfields with
         | Some (`String sym_id) when String.trim sym_id <> "" ->
             Hashtbl.replace out sym_id item
         | _ -> ())
    | _ -> ()
  ) symbols;
  out

let update_full
    (t:t)
    ~(revision:int)
    ~(symbols:(string, Yojson.Safe.t) Hashtbl.t)
  : unit =
  t.revision <- revision;
  Hashtbl.clear t.symbols;
  Hashtbl.iter (fun k v -> Hashtbl.replace t.symbols k v) symbols

let to_json_string (j:Yojson.Safe.t) : string =
  Yojson.Safe.to_string j

let diff
    (t:t)
    ~(base_revision:int)
    ~(current_revision:int)
    ~(current_symbols:(string, Yojson.Safe.t) Hashtbl.t)
  : delta =
  if base_revision <> t.revision then
    { base_revision; revision = current_revision; reset = true; deletes = []; upserts = [] }
  else
    let deletes = ref [] in
    let upserts = ref [] in
    Hashtbl.iter (fun key _ ->
      if not (Hashtbl.mem current_symbols key) then deletes := key :: !deletes
    ) t.symbols;
    Hashtbl.iter (fun key cur ->
      match Hashtbl.find_opt t.symbols key with
      | None ->
          upserts := cur :: !upserts
      | Some prev ->
          if to_json_string prev <> to_json_string cur then
            upserts := cur :: !upserts
    ) current_symbols;
    {
      base_revision;
      revision = current_revision;
      reset = false;
      deletes = List.sort String.compare !deletes;
      upserts = List.rev !upserts;
    }

let delta_json (d:delta) : Yojson.Safe.t =
  `Assoc [
    "format", `String "jovial-lsif-lite";
    "baseRevision", `Int d.base_revision;
    "revision", `Int d.revision;
    "reset", `Bool d.reset;
    "deletes", `List (List.map (fun k -> `String k) d.deletes);
    "upserts", `List d.upserts;
  ]
