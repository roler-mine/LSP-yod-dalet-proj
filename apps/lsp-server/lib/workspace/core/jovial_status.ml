(* Module overview: Status and diagnostic helpers for Jovial semantic analysis results. *)

let normalize name = String.uppercase_ascii (String.trim name)

type value = {
  name : string;
  key : string;
  loc : Ast.Loc.t;
  ordinal : int;
  representation : Ast.expr Ast.node option;
}

type owner = {
  owner_name : string option;
  owner_key : string option;
  owner_loc : Ast.Loc.t option;
  values : value list;
}

let value_of_ast ordinal (sv : Ast.status_value Ast.node) : value =
  {
    name = sv.v.Ast.sv_name.v;
    key = normalize sv.v.Ast.sv_name.v;
    loc = sv.v.Ast.sv_name.loc;
    ordinal;
    representation = sv.v.Ast.sv_representation;
  }

let values_of_ast values = List.mapi value_of_ast values

let owner_of_values ?owner_name ?owner_loc values : owner =
  {
    owner_name;
    owner_key = Option.map normalize owner_name;
    owner_loc;
    values = values_of_ast values;
  }

let status_value_of_expr ordinal (e : Ast.expr Ast.node) : value option =
  match e.v with
  | Ast.EName id ->
      Some
        {
          name = id.v;
          key = normalize id.v;
          loc = id.loc;
          ordinal;
          representation = None;
        }
  | Ast.ECall { callee; args = [ { v = Ast.EName id; _ } ] }
    when normalize callee.v = "V" ->
      Some
        {
          name = id.v;
          key = normalize id.v;
          loc = id.loc;
          ordinal;
          representation = None;
        }
  | _ -> None

let legacy_owner_of_status_array ?owner_name ?owner_loc elem dims =
  match elem.Ast.v with
  | Ast.TName id when normalize id.v = "STATUS" || normalize id.v = "V" ->
      let values =
        dims |> List.mapi status_value_of_expr |> List.filter_map (fun x -> x)
      in
      if values = [] then None
      else
        Some
          {
            owner_name;
            owner_key = Option.map normalize owner_name;
            owner_loc;
            values;
          }
  | _ -> None

let rec owner_of_type_expr ?owner_name ?owner_loc (t : Ast.type_expr Ast.node) :
    owner option =
  match t.v with
  | Ast.TStatus values -> Some (owner_of_values ?owner_name ?owner_loc values)
  | Ast.TArray { elem; dims } ->
      legacy_owner_of_status_array ?owner_name ?owner_loc elem dims
  | Ast.TSpecifiedTable { elem; _ } ->
      owner_of_type_expr ?owner_name ?owner_loc elem
  | Ast.TName _ | Ast.TScalar _ | Ast.TPointer _ | Ast.TRecord _ | Ast.TFunc _
    ->
      None

let rec owners_of_type_expr ?owner_name ?owner_loc
    (t : Ast.type_expr Ast.node) : owner list =
  match t.v with
  | Ast.TStatus _ -> (
      match owner_of_type_expr ?owner_name ?owner_loc t with
      | Some owner -> [ owner ]
      | None -> [])
  | Ast.TArray { elem; dims = _ } -> (
      match owner_of_type_expr ?owner_name ?owner_loc t with
      | Some owner -> [ owner ]
      | None -> owners_of_type_expr ?owner_name ?owner_loc elem)
  | Ast.TSpecifiedTable { elem; _ } ->
      owners_of_type_expr ?owner_name ?owner_loc elem
  | Ast.TScalar _ -> []
  | Ast.TPointer inner -> owners_of_type_expr ?owner_name ?owner_loc inner
  | Ast.TRecord fields ->
      List.concat_map
        (fun (field : Ast.field_decl Ast.node) ->
          owners_of_type_expr ~owner_name:field.v.fname.v
            ~owner_loc:field.v.fname.loc field.v.ftype)
        fields
  | Ast.TFunc { params; returns } ->
      let param_owners =
        List.concat_map
          (fun (param : Ast.param Ast.node) ->
            owners_of_type_expr ~owner_name:param.v.pname.v
              ~owner_loc:param.v.pname.loc param.v.ptype)
          params
      in
      let return_owners =
        match returns with
        | None -> []
        | Some r -> owners_of_type_expr ?owner_name ?owner_loc r
      in
      param_owners @ return_owners
  | Ast.TName _ -> []

let rec owners_of_decl (d : Ast.decl Ast.node) : owner list =
  match d.v with
  | Ast.DType { name; defn; _ } ->
      owners_of_type_expr ~owner_name:name.v ~owner_loc:name.loc defn
  | Ast.DVar { name; dtype; _ } ->
      owners_of_type_expr ~owner_name:name.v ~owner_loc:name.loc dtype
  | Ast.DConst { name; dtype = Some dtype; _ } ->
      owners_of_type_expr ~owner_name:name.v ~owner_loc:name.loc dtype
  | Ast.DConst { dtype = None; _ } | Ast.DOverlay _ | Ast.DDirective _ -> []
  | Ast.DError _ -> []
  | Ast.DProc p ->
      List.concat_map owners_of_decl p.v.locals @ owners_of_stmt p.v.body

and owners_of_stmt (s : Ast.stmt Ast.node) : owner list =
  match s.v with
  | Ast.SDecl d -> owners_of_decl d
  | Ast.SBlock xs -> List.concat_map owners_of_stmt xs
  | Ast.SIf { then_; else_; _ } ->
      owners_of_stmt then_
      @ (match else_ with None -> [] | Some e -> owners_of_stmt e)
  | Ast.SWhile { body; _ } -> owners_of_stmt body
  | Ast.SFor { init; step; body; _ } ->
      (match init with None -> [] | Some i -> owners_of_stmt i)
      @ (match step with None -> [] | Some s -> owners_of_stmt s)
      @ owners_of_stmt body
  | Ast.SCase { options; _ } ->
      List.concat_map
        (fun (opt : Ast.case_option Ast.node) -> owners_of_stmt opt.v.case_body)
        options
  | Ast.SLabel { body; _ } -> owners_of_stmt body
  | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _ | Ast.SGoto _
    ->
      []
  | Ast.SError _ -> []

let rec owners_of_program (prog : Ast.program) : owner list =
  List.concat_map
    (function
      | Ast.TopDecl d -> owners_of_decl d
      | Ast.TopStmt s -> owners_of_stmt s
      | Ast.TopModule m -> owners_of_program m.v.module_items
      | Ast.TopError _ -> [])
    prog

let duplicate_values (owner : owner) : (value * value) list =
  let seen = Hashtbl.create 16 in
  let out = ref [] in
  List.iter
    (fun value ->
      if value.key <> "" then
        match Hashtbl.find_opt seen value.key with
        | None -> Hashtbl.replace seen value.key value
        | Some first -> out := (first, value) :: !out)
    owner.values;
  List.rev !out

let value_is_member owner name =
  let key = normalize name in
  key <> "" && List.exists (fun value -> value.key = key) owner.values

let owner_display owner =
  match owner.owner_name with
  | Some name when String.trim name <> "" -> name
  | _ -> "anonymous STATUS"

let rec status_constructor_arg (e : Ast.expr Ast.node) : Ast.ident option =
  match e.v with
  | Ast.ECall { callee; args = [ { v = Ast.EName id; _ } ] }
    when normalize callee.v = "V" ->
      Some id
  | Ast.EParen inner -> status_constructor_arg inner
  | _ -> None
