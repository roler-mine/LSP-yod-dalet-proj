(* Module overview: Semantic graph representation of symbols, scopes, definitions, and references. *)

module T = Lsp.Types
module Metadata = Workspace_symbol_metadata
open Ast

type symbol = {
  id : Symbol_id.t;
  name : string;
  key : string;
  kind : Metadata.jovial_symbol_kind;
  metadata : Metadata.jovial_symbol_metadata;
  decl_uri : T.DocumentUri.t;
  decl_loc : Ast.Loc.t;
  scope_id : Scope_id.t option;
  type_id : Type_id.t option;
  body_range : Ast.Loc.t option;
}

type reference = {
  symbol_id : Symbol_id.t;
  uri : T.DocumentUri.t;
  loc : Ast.Loc.t;
  role : Metadata.jovial_decl_role;
}

type scope_kind =
  | SystemScope
  | CompoolScope
  | ModuleScope
  | ProcedureScope
  | BlockScope
  | TableScope
  | DefineScope

type scope = {
  id : Scope_id.t;
  kind : scope_kind;
  uri : T.DocumentUri.t;
  parent : Scope_id.t option;
  owner_symbol : Symbol_id.t option;
  range : Ast.Loc.t;
  mutable declarations : Symbol_id.t list;
  mutable imports : string list;
}

type duplicate_declaration = {
  scope_id : Scope_id.t;
  key : string;
  declarations : Symbol_id.t list;
}

type t = {
  symbols_by_id : (Symbol_id.t, symbol) Hashtbl.t;
  symbol_ids_by_decl_key : (string, Symbol_id.t) Hashtbl.t;
  references_by_symbol : (Symbol_id.t, reference list) Hashtbl.t;
  scopes_by_id : (Scope_id.t, scope) Hashtbl.t;
  duplicate_declarations_by_key : (string, duplicate_declaration) Hashtbl.t;
}

let create () =
  {
    symbols_by_id = Hashtbl.create 256;
    symbol_ids_by_decl_key = Hashtbl.create 256;
    references_by_symbol = Hashtbl.create 512;
    scopes_by_id = Hashtbl.create 64;
    duplicate_declarations_by_key = Hashtbl.create 64;
  }

let uri_key (uri : T.DocumentUri.t) : string = Uri_path.docuri_to_string uri

let scope_kind_label = function
  | SystemScope -> "system"
  | CompoolScope -> "compool"
  | ModuleScope -> "module"
  | ProcedureScope -> "procedure"
  | BlockScope -> "block"
  | TableScope -> "table"
  | DefineScope -> "define"

let loc_fragment (loc : Ast.Loc.t) : string =
  Printf.sprintf "%d:%d:%d-%d:%d:%d" loc.Ast.Loc.start_pos.line
    loc.Ast.Loc.start_pos.col loc.Ast.Loc.start_pos.offset
    loc.Ast.Loc.end_pos.line loc.Ast.Loc.end_pos.col
    loc.Ast.Loc.end_pos.offset

let same_uri (a : T.DocumentUri.t) (b : T.DocumentUri.t) : bool =
  uri_key a = uri_key b

let same_loc (a : Ast.Loc.t) (b : Ast.Loc.t) : bool =
  a.Ast.Loc.start_pos.line = b.Ast.Loc.start_pos.line
  && a.Ast.Loc.start_pos.col = b.Ast.Loc.start_pos.col
  && a.Ast.Loc.start_pos.offset = b.Ast.Loc.start_pos.offset
  && a.Ast.Loc.end_pos.line = b.Ast.Loc.end_pos.line
  && a.Ast.Loc.end_pos.col = b.Ast.Loc.end_pos.col
  && a.Ast.Loc.end_pos.offset = b.Ast.Loc.end_pos.offset

let loc_contains (outer : Ast.Loc.t) (inner : Ast.Loc.t) : bool =
  outer.Ast.Loc.start_pos.offset <= inner.Ast.Loc.start_pos.offset
  && inner.Ast.Loc.end_pos.offset <= outer.Ast.Loc.end_pos.offset

let loc_span_width (loc : Ast.Loc.t) : int =
  max 0 (loc.Ast.Loc.end_pos.offset - loc.Ast.Loc.start_pos.offset)

let normalize_key = Workspace_state.normalize_name

let doc_range (doc : Document.t) : Ast.Loc.t =
  let line0, col0 =
    Text_index.line_col_of_offset doc.Document.index
      (String.length doc.Document.text)
  in
  let start_pos = { Ast.Loc.line = 1; col = 0; offset = 0 } in
  let end_pos =
    {
      Ast.Loc.line = line0 + 1;
      col = col0;
      offset = String.length doc.Document.text;
    }
  in
  Ast.Loc.make ~file:doc.Document.file ~start_pos ~end_pos

let stable_decl_key ~(uri : T.DocumentUri.t) ~(loc : Ast.Loc.t) ~(key : string)
    ~(kind : Metadata.jovial_symbol_kind) : string =
  Printf.sprintf "%s|%s|%s|%s" (uri_key uri) (loc_fragment loc)
    (Workspace_state.normalize_name key)
    (Metadata.symbol_kind_label kind)

let symbol_id_for_decl ~(uri : T.DocumentUri.t) ~(loc : Ast.Loc.t)
    ~(key : string) ~(kind : Metadata.jovial_symbol_kind) : Symbol_id.t =
  stable_decl_key ~uri ~loc ~key ~kind |> Symbol_id.of_stable_string

let kind_of_def (d : Workspace_nav_model.def) : Metadata.jovial_symbol_kind =
  match d.metadata.Metadata.jovial_kind with
  | Metadata.JovialUnknownSymbol ->
      if d.kind = Workspace_nav_model.sym_kind_module then Metadata.JovialModule
      else if d.kind = Workspace_nav_model.sym_kind_type then Metadata.JovialType
      else if d.kind = Workspace_nav_model.sym_kind_field then
        Metadata.JovialField
      else if d.kind = Workspace_nav_model.sym_kind_func then
        Metadata.JovialProcedure
      else if d.kind = Workspace_nav_model.sym_kind_const then
        Metadata.JovialDefine
      else Metadata.JovialItem
  | kind -> kind

let type_origin_fragment = function
  | Metadata.BuiltinType -> "builtin"
  | Metadata.UserDefinedType name -> "user:" ^ Workspace_state.normalize_name name
  | Metadata.InferredType -> "inferred"
  | Metadata.UnknownType -> "unknown"

let type_id_of_def (d : Workspace_nav_model.def)
    ~(kind : Metadata.jovial_symbol_kind) : Type_id.t option =
  match d.metadata.Metadata.type_info with
  | Some info ->
      Some
        (Type_id.of_stable_string
           (String.concat "|"
              [
                info.Metadata.display;
                type_origin_fragment info.Metadata.origin;
                Option.value info.Metadata.resolved_display ~default:"";
              ]))
  | None -> (
      match kind with
      | Metadata.JovialType ->
          Some
            (Type_id.of_stable_string
               (stable_decl_key ~uri:d.uri ~loc:d.loc ~key:d.key ~kind))
      | _ -> None)

let scope_id_of_def (d : Workspace_nav_model.def) : Scope_id.t option =
  match d.container with
  | None -> None
  | Some container ->
      Some
        (Scope_id.of_stable_string
           (uri_key d.uri ^ "|scope|" ^ Workspace_state.normalize_name container))

let symbol_of_def (d : Workspace_nav_model.def) : symbol =
  let kind = kind_of_def d in
  {
    id = symbol_id_for_decl ~uri:d.uri ~loc:d.loc ~key:d.key ~kind;
    name = d.name;
    key = Workspace_state.normalize_name d.key;
    kind;
    metadata = d.metadata;
    decl_uri = d.uri;
    decl_loc = d.loc;
    scope_id = scope_id_of_def d;
    type_id = type_id_of_def d ~kind;
    body_range = None;
  }

let symbol_id_of_def d = (symbol_of_def d).id

let add_symbol (graph : t) (sym : symbol) : unit =
  Hashtbl.replace graph.symbols_by_id sym.id sym;
  Hashtbl.replace graph.symbol_ids_by_decl_key
    (stable_decl_key ~uri:sym.decl_uri ~loc:sym.decl_loc ~key:sym.key
       ~kind:sym.kind)
    sym.id

let same_reference (a : reference) (b : reference) : bool =
  Symbol_id.equal a.symbol_id b.symbol_id
  && uri_key a.uri = uri_key b.uri
  && a.loc.Ast.Loc.start_pos.line = b.loc.Ast.Loc.start_pos.line
  && a.loc.Ast.Loc.start_pos.col = b.loc.Ast.Loc.start_pos.col
  && a.loc.Ast.Loc.start_pos.offset = b.loc.Ast.Loc.start_pos.offset
  && a.loc.Ast.Loc.end_pos.line = b.loc.Ast.Loc.end_pos.line
  && a.loc.Ast.Loc.end_pos.col = b.loc.Ast.Loc.end_pos.col
  && a.loc.Ast.Loc.end_pos.offset = b.loc.Ast.Loc.end_pos.offset
  && a.role = b.role

let add_reference (graph : t) (refn : reference) : unit =
  let prev =
    match Hashtbl.find_opt graph.references_by_symbol refn.symbol_id with
    | None -> []
    | Some xs -> xs
  in
  if not (List.exists (same_reference refn) prev) then
    Hashtbl.replace graph.references_by_symbol refn.symbol_id (refn :: prev)

let add_def_symbol (graph : t) (d : Workspace_nav_model.def) : symbol =
  let sym = symbol_of_def d in
  add_symbol graph sym;
  add_reference graph
    {
      symbol_id = sym.id;
      uri = sym.decl_uri;
      loc = sym.decl_loc;
      role = sym.metadata.Metadata.decl_role;
    };
  sym

let find_symbol (graph : t) (id : Symbol_id.t) : symbol option =
  Hashtbl.find_opt graph.symbols_by_id id

let replace_symbol (graph : t) (sym : symbol) : unit =
  Hashtbl.replace graph.symbols_by_id sym.id sym

let set_symbol_scope (graph : t) (id : Symbol_id.t) (scope_id : Scope_id.t) :
    unit =
  match find_symbol graph id with
  | None -> ()
  | Some sym -> replace_symbol graph { sym with scope_id = Some scope_id }

let set_symbol_body_range (graph : t) (id : Symbol_id.t) (body_range : Ast.Loc.t)
    : unit =
  match find_symbol graph id with
  | None -> ()
  | Some sym -> replace_symbol graph { sym with body_range = Some body_range }

let scope_id_for ~(uri : T.DocumentUri.t) ~(kind : scope_kind)
    ~(range : Ast.Loc.t) ~(parent : Scope_id.t option)
    ~(owner_symbol : Symbol_id.t option) : Scope_id.t =
  Scope_id.of_stable_string
    (String.concat "|"
       [
         uri_key uri;
         "scope";
         scope_kind_label kind;
         loc_fragment range;
         (match parent with None -> "" | Some id -> Scope_id.to_string id);
         (match owner_symbol with
         | None -> ""
         | Some id -> Symbol_id.to_string id);
       ])

let add_scope (graph : t) ~(uri : T.DocumentUri.t) ~(kind : scope_kind)
    ~(range : Ast.Loc.t) ~(parent : Scope_id.t option)
    ~(owner_symbol : Symbol_id.t option) ~(imports : string list) : scope =
  let id = scope_id_for ~uri ~kind ~range ~parent ~owner_symbol in
  let scope =
    {
      id;
      kind;
      uri;
      parent;
      owner_symbol;
      range;
      declarations = [];
      imports = List.map normalize_key imports;
    }
  in
  Hashtbl.replace graph.scopes_by_id id scope;
  scope

let find_scope (graph : t) (id : Scope_id.t) : scope option =
  Hashtbl.find_opt graph.scopes_by_id id

let sorted_symbol_ids (ids : Symbol_id.t list) : Symbol_id.t list =
  ids |> List.sort_uniq Symbol_id.compare

let duplicate_key (scope_id : Scope_id.t) (key : string) : string =
  Scope_id.to_string scope_id ^ "|" ^ normalize_key key

let record_duplicate (graph : t) (scope_id : Scope_id.t) (key : string)
    (ids : Symbol_id.t list) : unit =
  let key = normalize_key key in
  if key <> "" then
    Hashtbl.replace graph.duplicate_declarations_by_key
      (duplicate_key scope_id key)
      { scope_id; key; declarations = sorted_symbol_ids ids }

let declare_symbol (graph : t) (scope : scope) (symbol_id : Symbol_id.t) : unit
    =
  match find_symbol graph symbol_id with
  | None -> ()
  | Some sym ->
      if not (List.exists (Symbol_id.equal symbol_id) scope.declarations) then
        scope.declarations <- scope.declarations @ [ symbol_id ];
      set_symbol_scope graph symbol_id scope.id;
      let same_key =
        scope.declarations
        |> List.filter (fun id ->
               match find_symbol graph id with
               | Some other -> other.key = sym.key
               | None -> false)
      in
      if List.length same_key > 1 then record_duplicate graph scope.id sym.key same_key

let symbols_matching_decl (graph : t) ~(uri : T.DocumentUri.t)
    ~(loc : Ast.Loc.t) ~(key : string) : symbol list =
  let key = normalize_key key in
  Hashtbl.fold
    (fun _ sym acc ->
      if same_uri sym.decl_uri uri && sym.key = key && same_loc sym.decl_loc loc
      then sym :: acc
      else acc)
    graph.symbols_by_id []

let declare_decl (graph : t) (scope : scope) ~(uri : T.DocumentUri.t)
    ~(loc : Ast.Loc.t) ~(key : string) : Symbol_id.t option =
  match symbols_matching_decl graph ~uri ~loc ~key with
  | [] -> None
  | syms ->
      List.iter (fun (sym : symbol) -> declare_symbol graph scope sym.id) syms;
      Some (List.hd syms : symbol).id

let find_symbol_by_def (graph : t) (d : Workspace_nav_model.def) :
    symbol option =
  let sym = symbol_of_def d in
  Hashtbl.find_opt graph.symbol_ids_by_decl_key
    (stable_decl_key ~uri:sym.decl_uri ~loc:sym.decl_loc ~key:sym.key
       ~kind:sym.kind)
  |> function None -> None | Some id -> find_symbol graph id

let references_for_symbol (graph : t) (id : Symbol_id.t) : reference list =
  match Hashtbl.find_opt graph.references_by_symbol id with
  | None -> []
  | Some refs -> List.rev refs

let symbols (graph : t) : symbol list =
  Hashtbl.fold (fun _ (sym : symbol) acc -> sym :: acc) graph.symbols_by_id []
  |> List.sort (fun (a : symbol) (b : symbol) ->
         Symbol_id.compare a.id b.id)

let references (graph : t) : reference list =
  Hashtbl.fold (fun _ refs acc -> List.rev_append refs acc)
    graph.references_by_symbol []
  |> List.sort (fun a b ->
         let c0 = Symbol_id.compare a.symbol_id b.symbol_id in
         if c0 <> 0 then c0
         else
           let c1 = String.compare (uri_key a.uri) (uri_key b.uri) in
           if c1 <> 0 then c1
           else
             let c2 =
               compare a.loc.Ast.Loc.start_pos.line
                 b.loc.Ast.Loc.start_pos.line
             in
             if c2 <> 0 then c2
             else
               compare a.loc.Ast.Loc.start_pos.col
                 b.loc.Ast.Loc.start_pos.col)

let scopes (graph : t) : scope list =
  Hashtbl.fold (fun _ scope acc -> scope :: acc) graph.scopes_by_id []
  |> List.sort (fun a b ->
         let c0 = compare a.range.Ast.Loc.start_pos.offset
             b.range.Ast.Loc.start_pos.offset
         in
         if c0 <> 0 then c0
         else
           let c1 = compare (loc_span_width b.range) (loc_span_width a.range) in
           if c1 <> 0 then c1 else Scope_id.compare a.id b.id)

let duplicate_declarations (graph : t) : duplicate_declaration list =
  Hashtbl.fold (fun _ d acc -> d :: acc) graph.duplicate_declarations_by_key []
  |> List.sort (fun a b ->
         let c0 = Scope_id.compare a.scope_id b.scope_id in
         if c0 <> 0 then c0 else String.compare a.key b.key)

let scope_depth (graph : t) (scope : scope) : int =
  let rec go n = function
    | None -> n
    | Some id -> (
        match find_scope graph id with
        | None -> n
        | Some parent -> go (n + 1) parent.parent)
  in
  go 0 scope.parent

let scope_at_loc (graph : t) ~(uri : T.DocumentUri.t) (loc : Ast.Loc.t) :
    Scope_id.t option =
  Hashtbl.fold
    (fun _ scope best ->
      if (not (same_uri scope.uri uri)) || not (loc_contains scope.range loc)
      then best
      else
        match best with
        | None -> Some scope
        | Some cur ->
            let w = loc_span_width scope.range in
            let cw = loc_span_width cur.range in
            if w < cw || (w = cw && scope_depth graph scope > scope_depth graph cur)
            then Some scope
            else best)
    graph.scopes_by_id None
  |> Option.map (fun scope -> scope.id)

let scope_chain (graph : t) (scope_id : Scope_id.t) : scope list =
  let rec go acc id =
    match find_scope graph id with
    | None -> List.rev acc
    | Some scope -> (
        match scope.parent with
        | None -> List.rev (scope :: acc)
        | Some parent -> go (scope :: acc) parent)
  in
  go [] scope_id

let ids_for_key_in_scope (graph : t) (scope : scope) (key : string) :
    Symbol_id.t list =
  let key = normalize_key key in
  scope.declarations
  |> List.filter (fun id ->
         match find_symbol graph id with
         | Some sym -> sym.key = key
         | None -> false)

let resolve_name (graph : t) (scope_id : Scope_id.t) (name : string) :
    Symbol_id.t list =
  let key = normalize_key name in
  if key = "" then []
  else
    let rec go = function
      | [] -> []
      | (scope : scope) :: rest ->
          let hits = ids_for_key_in_scope graph scope key in
          if hits <> [] then sorted_symbol_ids hits else go rest
    in
    go (scope_chain graph scope_id)

let visible_symbols (graph : t) (scope_id : Scope_id.t) : Symbol_id.t list =
  let seen = Hashtbl.create 128 in
  let out = ref [] in
  scope_chain graph scope_id
  |> List.iter (fun (scope : scope) ->
         let by_key : (string, Symbol_id.t list) Hashtbl.t =
           Hashtbl.create 64
         in
         List.iter
           (fun id ->
             match find_symbol graph id with
             | None -> ()
             | Some sym ->
                 let prev =
                   match Hashtbl.find_opt by_key sym.key with
                   | None -> []
                   | Some ids -> ids
                 in
                 Hashtbl.replace by_key sym.key (id :: prev))
           scope.declarations;
         Hashtbl.iter
           (fun key ids ->
             if not (Hashtbl.mem seen key) then (
               Hashtbl.replace seen key true;
               out := sorted_symbol_ids ids @ !out))
           by_key);
  List.rev !out

let symbol_is_declared (graph : t) (symbol_id : Symbol_id.t) : bool =
  Hashtbl.fold
    (fun _ (scope : scope) found ->
      found
      || List.exists (fun id -> Symbol_id.equal id symbol_id) scope.declarations)
    graph.scopes_by_id false

let nearest_ancestor_scope (graph : t) (scope_id : Scope_id.t)
    ~(kind : scope_kind) : scope option =
  scope_chain graph scope_id
  |> List.find_opt (fun (scope : scope) -> scope.kind = kind)

let attach_unscoped_symbols_by_location (graph : t) : unit =
  symbols graph
  |> List.iter (fun (sym : symbol) ->
         if not (symbol_is_declared graph sym.id) then
           match scope_at_loc graph ~uri:sym.decl_uri sym.decl_loc with
           | None -> ()
           | Some scope_id -> (
               let scope_id =
                 match sym.kind with
                 | Metadata.JovialLabel -> (
                     match
                       nearest_ancestor_scope graph scope_id ~kind:ProcedureScope
                     with
                     | Some proc_scope -> proc_scope.id
                     | None -> scope_id)
                 | _ -> scope_id
               in
               match find_scope graph scope_id with
               | None -> ()
               | Some scope -> declare_symbol graph scope sym.id))

let json_of_loc (loc : Ast.Loc.t) : Yojson.Safe.t =
  `Assoc
    [
      ("startLine", `Int loc.Ast.Loc.start_pos.line);
      ("startCol", `Int loc.Ast.Loc.start_pos.col);
      ("endLine", `Int loc.Ast.Loc.end_pos.line);
      ("endCol", `Int loc.Ast.Loc.end_pos.col);
    ]

let debug_json (graph : t) : Yojson.Safe.t =
  let scope_json (scope : scope) =
    `Assoc
      [
        ("id", `Int (Scope_id.to_int scope.id));
        ("kind", `String (scope_kind_label scope.kind));
        ( "parent",
          match scope.parent with
          | None -> `Null
          | Some id -> `Int (Scope_id.to_int id) );
        ( "ownerSymbol",
          match scope.owner_symbol with
          | None -> `Null
          | Some id -> `Int (Symbol_id.to_int id) );
        ("declarationCount", `Int (List.length scope.declarations));
        ("range", json_of_loc scope.range);
      ]
  in
  `Assoc
    [
      ("symbolCount", `Int (Hashtbl.length graph.symbols_by_id));
      ("scopeCount", `Int (Hashtbl.length graph.scopes_by_id));
      ( "duplicateDeclarationCount",
        `Int (Hashtbl.length graph.duplicate_declarations_by_key) );
      ("scopes", `List (List.map scope_json (scopes graph)));
    ]

let import_names (doc : Document.t) : string list =
  doc.Document.imports
  |> List.filter_map (fun (imp : Preprocess.import) ->
         let name = normalize_key imp.name in
         if name = "" then None else Some name)
  |> List.sort_uniq String.compare

let declare_ident (graph : t) (scope : scope) ~(uri : T.DocumentUri.t)
    (id : Ast.ident) : Symbol_id.t option =
  declare_decl graph scope ~uri ~loc:id.loc ~key:id.v

let declare_define (graph : t) (scope : scope) ~(uri : T.DocumentUri.t)
    (dm : Preprocess.define) : Symbol_id.t option =
  declare_decl graph scope ~uri ~loc:dm.loc ~key:dm.name

let add_child_scope (graph : t) (parent : scope) ~(kind : scope_kind)
    ~(range : Ast.Loc.t) ~(owner_symbol : Symbol_id.t option) : scope =
  add_scope graph ~uri:parent.uri ~kind ~range ~parent:(Some parent.id)
    ~owner_symbol ~imports:[]

let type_contains_field_scope (t : Ast.type_expr Ast.node) : bool =
  let rec go (t : Ast.type_expr Ast.node) =
    match t.v with
    | Ast.TRecord _ -> true
    | Ast.TArray { elem; _ } -> go elem
    | Ast.TSpecifiedTable { elem; _ } -> go elem
    | Ast.TPointer inner -> go inner
    | Ast.TFunc { params; returns } ->
        List.exists (fun p -> go p.v.ptype) params
        || (match returns with None -> false | Some r -> go r)
    | Ast.TName _ | Ast.TStatus _ -> false
  in
  go t

let rec declare_type_fields (graph : t) (scope : scope) ~(uri : T.DocumentUri.t)
    (t : Ast.type_expr Ast.node) : unit =
  match t.v with
  | Ast.TRecord fields ->
      List.iter
        (fun (field : Ast.field_decl Ast.node) ->
          ignore (declare_ident graph scope ~uri field.v.fname);
          declare_type_fields graph scope ~uri field.v.ftype)
        fields
  | Ast.TArray { elem; _ } | Ast.TPointer elem ->
      declare_type_fields graph scope ~uri elem
  | Ast.TSpecifiedTable { elem; _ } ->
      declare_type_fields graph scope ~uri elem
  | Ast.TFunc { params; returns } ->
      List.iter
        (fun (param : Ast.param Ast.node) ->
          ignore (declare_ident graph scope ~uri param.v.pname);
          declare_type_fields graph scope ~uri param.v.ptype)
        params;
      (match returns with
      | None -> ()
      | Some ret -> declare_type_fields graph scope ~uri ret)
  | Ast.TName _ | Ast.TStatus _ -> ()

let add_type_member_scope_if_needed (graph : t) (parent : scope)
    ~(kind : scope_kind) ~(range : Ast.Loc.t)
    ~(owner_symbol : Symbol_id.t option) ~(uri : T.DocumentUri.t)
    (t : Ast.type_expr Ast.node) : unit =
  if type_contains_field_scope t then
    let member_scope = add_child_scope graph parent ~kind ~range ~owner_symbol in
    declare_type_fields graph member_scope ~uri t

let build_skeleton_scopes (graph : t) (doc : Document.t) : unit =
  let range = doc_range doc in
  let system =
    add_scope graph ~uri:doc.Document.uri ~kind:SystemScope ~range ~parent:None
      ~owner_symbol:None ~imports:[]
  in
  let module_kind =
    match doc.Document.compool_def with
    | Some _ -> CompoolScope
    | None -> ModuleScope
  in
  let module_scope =
    add_scope graph ~uri:doc.Document.uri ~kind:module_kind ~range
      ~parent:(Some system.id) ~owner_symbol:None ~imports:(import_names doc)
  in
  symbols graph
  |> List.iter (fun sym ->
         if same_uri sym.decl_uri doc.Document.uri then
           declare_symbol graph module_scope sym.id)

let build_ast_scopes (graph : t) (doc : Document.t) (prog : Ast.program) : unit =
  let uri = doc.Document.uri in
  let range = doc_range doc in
  let system =
    add_scope graph ~uri ~kind:SystemScope ~range ~parent:None
      ~owner_symbol:None ~imports:[]
  in
  let module_kind =
    match doc.Document.compool_def with
    | Some _ -> CompoolScope
    | None -> ModuleScope
  in
  let module_scope =
    add_scope graph ~uri ~kind:module_kind ~range ~parent:(Some system.id)
      ~owner_symbol:None ~imports:(import_names doc)
  in
  List.iter
    (fun dm ->
      let owner_symbol = declare_define graph module_scope ~uri dm in
      ignore
        (add_child_scope graph module_scope ~kind:DefineScope ~range:dm.loc
           ~owner_symbol))
    doc.Document.defines;

  let rec walk_decl (scope : scope) (proc_scope : scope option)
      (d : Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DVar { name; dtype; data_decl_kind; _ } ->
        let owner_symbol = declare_ident graph scope ~uri name in
        let scope_kind =
          match data_decl_kind with
          | Ast.DataTable -> Some TableScope
          | Ast.DataBlock -> Some BlockScope
          | Ast.DataItem | Ast.DataUnknown ->
              if type_contains_field_scope dtype then Some BlockScope else None
        in
        (match scope_kind with
        | None -> ()
        | Some kind ->
            let member_scope =
              add_child_scope graph scope ~kind ~range:d.loc ~owner_symbol
            in
            declare_type_fields graph member_scope ~uri dtype)
    | Ast.DConst { name; dtype; data_decl_kind; _ } ->
        let owner_symbol = declare_ident graph scope ~uri name in
        let scope_kind =
          match data_decl_kind with
          | Ast.DataTable -> Some TableScope
          | Ast.DataBlock -> Some BlockScope
          | Ast.DataItem | Ast.DataUnknown ->
              Option.bind dtype (fun ty ->
                  if type_contains_field_scope ty then Some BlockScope else None)
        in
        (match (scope_kind, dtype) with
        | Some kind, Some ty ->
            let member_scope =
              add_child_scope graph scope ~kind ~range:d.loc ~owner_symbol
            in
            declare_type_fields graph member_scope ~uri ty
        | _ -> ())
    | Ast.DType { name; defn; _ } ->
        let owner_symbol = declare_ident graph scope ~uri name in
        add_type_member_scope_if_needed graph scope ~kind:TableScope
          ~range:d.loc ~owner_symbol ~uri defn
    | Ast.DProc p ->
        let owner_symbol = declare_ident graph scope ~uri p.v.name in
        let proc =
          add_child_scope graph scope ~kind:ProcedureScope ~range:p.loc
            ~owner_symbol
        in
        (match owner_symbol with
        | None -> ()
        | Some id -> set_symbol_body_range graph id p.v.body.loc);
        List.iter
          (fun (param : Ast.param Ast.node) ->
            ignore (declare_ident graph proc ~uri param.v.pname);
            declare_type_fields graph proc ~uri param.v.ptype)
          p.v.params;
        List.iter (walk_decl proc (Some proc)) p.v.locals;
        walk_stmt proc (Some proc) p.v.body
    | Ast.DOverlay overlay ->
        ignore (declare_ident graph scope ~uri overlay.overlay_name)
    | Ast.DDirective _ -> (
        match proc_scope with None -> () | Some _ -> ())
  and walk_stmt (scope : scope) (proc_scope : scope option)
      (s : Ast.stmt Ast.node) : unit =
    match s.v with
    | Ast.SEmpty | Ast.SAssign _ | Ast.SCallStmt _ | Ast.SReturn _
    | Ast.SGoto _ ->
        ()
    | Ast.SDecl d -> walk_decl scope proc_scope d
    | Ast.SBlock xs ->
        let block = add_child_scope graph scope ~kind:BlockScope ~range:s.loc
          ~owner_symbol:None in
        List.iter (walk_stmt block proc_scope) xs
    | Ast.SIf { then_; else_; _ } ->
        walk_stmt scope proc_scope then_;
        (match else_ with None -> () | Some e -> walk_stmt scope proc_scope e)
    | Ast.SWhile { body; _ } -> walk_stmt scope proc_scope body
    | Ast.SFor { init; step; body; _ } ->
        (match init with None -> () | Some i -> walk_stmt scope proc_scope i);
        (match step with None -> () | Some st -> walk_stmt scope proc_scope st);
        walk_stmt scope proc_scope body
    | Ast.SLabel { label; body } ->
        let label_scope = match proc_scope with Some p -> p | None -> scope in
        ignore (declare_ident graph label_scope ~uri label);
        walk_stmt scope proc_scope body
  in

  List.iter
    (function
      | Ast.TopDecl d -> walk_decl module_scope None d
      | Ast.TopStmt s -> walk_stmt module_scope None s)
    prog

let of_defs (defs : Workspace_nav_model.def list) : t =
  let graph = create () in
  List.iter (fun d -> ignore (add_def_symbol graph d)) defs;
  graph

let of_doc_defs (doc : Document.t) : t =
  let doc = Document.ensure_parsed doc in
  let graph = Workspace_nav_model.collect_doc_defs doc |> of_defs in
  (match Document.current_parse doc with
  | Some { Document.parsed_ast = Some prog; _ } -> build_ast_scopes graph doc prog
  | _ -> build_skeleton_scopes graph doc);
  attach_unscoped_symbols_by_location graph;
  graph
