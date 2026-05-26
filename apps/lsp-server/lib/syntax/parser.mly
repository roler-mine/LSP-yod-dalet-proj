%{
  (* Module overview: Menhir grammar and interface for building the recoverable Jovial syntax tree. *)

  open Ast

  let loc sp ep = Ast.Loc.of_lexing_positions_no_file sp ep
  let n sp ep v = Ast.node ~loc:(loc sp ep) v
  let nid sp ep s = Ast.node ~loc:(loc sp ep) s

  let loc_span (a : Ast.Loc.t) (b : Ast.Loc.t) : Ast.Loc.t =
    {
      Ast.Loc.file =
        (match a.file with Some _ as f -> f | None -> b.file);
      start_pos = a.start_pos;
      end_pos = b.end_pos;
    }

  let mk_block sp ep (ss : Ast.stmt Ast.node list) : Ast.stmt Ast.node =
    n sp ep (Ast.SBlock ss)

  let wrap_labels (labels : Ast.ident list) (s : Ast.stmt Ast.node) : Ast.stmt Ast.node =
    List.fold_right
      (fun (lab : Ast.ident) acc ->
        Ast.node ~loc:(loc_span lab.loc acc.loc)
          (Ast.SLabel { label = lab; body = acc }))
      labels
      s

  let recovered_message kind =
    "Recovered from damaged syntax in " ^ kind

  let parse_error_info ?(expected = []) ?actual ~(kind : string)
      ~(message : string) () : Ast.parse_error =
    { Ast.message; expected; actual; recovery_kind = kind }

  let note_recovery sp ep kind detail =
    Parse_diags.add (loc sp ep) (recovered_message kind ^ ": " ^ detail)

  let bad_stmt sp ep : Ast.stmt Ast.node =
    note_recovery sp ep "statement" "skipped to the next statement boundary";
    n sp ep
      (Ast.SError
         (parse_error_info ~kind:"statementSyncSkip"
            ~message:"Could not parse this statement." ()))

  let bad_top sp ep : Ast.toplevel list =
    note_recovery sp ep "module item"
      "skipped to the next module or declaration boundary";
    [
      Ast.TopError
        (n sp ep
           (parse_error_info ~kind:"moduleItemSyncSkip"
              ~message:"Could not parse this module item." ()));
    ]

  let mk_assign_stmt sp ep (lhses : Ast.expr Ast.node list)
      (rhs : Ast.expr Ast.node) : Ast.stmt Ast.node =
    match lhses with
    | [lhs] -> n sp ep (Ast.SAssign { lhs; rhs })
    | _ ->
        mk_block sp ep
          (List.map
             (fun lhs ->
               Ast.node ~loc:(loc sp ep) (Ast.SAssign { lhs; rhs }))
             lhses)

  let mk_conv sp ep (ty : Ast.type_expr Ast.node) (rhs : Ast.expr Ast.node) : Ast.expr Ast.node =
    n sp ep (Ast.EConvert { ty; expr = rhs })

  let mk_preset sp ep (base_num : string) (items : Ast.expr Ast.node list) : Ast.expr Ast.node =
    let base = n sp ep (Ast.ELit (Ast.LInt base_num)) in
    n sp ep (Ast.EPreset { base; items })

  let mk_empty_table_preset sp ep : Ast.expr Ast.node =
    mk_preset sp ep "0" []

  let specified_table_kind_of_elem_type (ty : Ast.type_expr Ast.node option)
      : Ast.specified_table_kind option =
    match ty with
    | Some { v = Ast.TArray { elem = { v = Ast.TName id; _ }; dims = [entry_size] }; _ }
      when String.uppercase_ascii id.v = "W" ->
        Some (Ast.SpecTableW entry_size)
    | Some { v = Ast.TArray { elem = { v = Ast.TName id; _ }; dims = [entry_size] }; _ }
      when String.uppercase_ascii id.v = "V" ->
        Some (Ast.SpecTableV (Some entry_size))
    | Some { v = Ast.TName id; _ } when String.uppercase_ascii id.v = "V" ->
        Some (Ast.SpecTableV None)
    | _ -> None

  let mk_table_type sp ep dims elem_ty_opt recopt_before recopt_after : Ast.type_expr Ast.node =
    let record_fields =
      match recopt_after with
      | Some _ as fields -> fields
      | None -> recopt_before
    in
    let elem_ty =
      match record_fields with
      | Some fields ->
          n sp ep (Ast.TRecord fields)
      | None ->
          (match elem_ty_opt with
           | Some ty -> ty
           | None -> n sp ep (Ast.TName (nid sp ep "__table_elem__")))
    in
    match (record_fields, specified_table_kind_of_elem_type elem_ty_opt) with
    | Some _, Some kind ->
        n sp ep (Ast.TSpecifiedTable { elem = elem_ty; dims; kind })
    | _ -> n sp ep (Ast.TArray { elem = elem_ty; dims })

  let proc_use_from_flags (seen_rec:bool) (seen_rent:bool) : Ast.proc_use =
    if seen_rec then Ast.UseRec else if seen_rent then Ast.UseRent else Ast.UseNormal

  let external_modifier_of_string_opt = function
    | Some "DEF" -> Ast.DefDecl
    | Some "REF" -> Ast.RefDecl
    | _ -> Ast.LocalDecl

  let external_modifier_of_req = function
    | "DEF" -> Ast.DefDecl
    | "REF" -> Ast.RefDecl
    | _ -> Ast.LocalDecl

  let warn_ref_preset (m : string option) (init : Ast.expr Ast.node option) (where : Ast.Loc.t) =
    match m, init with
    | Some "REF", Some _ ->
        Parse_diags.add where
          "J73 REF item/table specifications cannot contain presets; remove the preset or use a DEF specification."
    | _ -> ()

  let warn_external_constant (m : string option) (where : Ast.Loc.t) =
    match m with
    | Some x when x = "DEF" || x = "REF" ->
        Parse_diags.add where
          "J73 CONSTANT declarations cannot be directly DEF/REF; wrap the constant inside an external BLOCK."
    | _ -> ()

  let apply_external_modifier_to_proc (m : Ast.external_modifier)
      (p : Ast.proc Ast.node) : Ast.proc Ast.node =
    { p with v = { p.v with external_modifier = m } }

  let apply_external_modifier_to_decl (m : Ast.external_modifier)
      (d : Ast.decl Ast.node) : Ast.decl Ast.node =
    let v =
      match d.v with
      | Ast.DVar x -> Ast.DVar { x with external_modifier = m }
      | Ast.DConst x -> Ast.DConst { x with external_modifier = m }
      | Ast.DType x -> Ast.DType { x with external_modifier = m }
      | Ast.DProc p -> Ast.DProc (apply_external_modifier_to_proc m p)
      | Ast.DOverlay _ as x -> x
      | Ast.DDirective _ as x -> x
      | Ast.DError _ as x -> x
    in
    { d with v }

  let field_of_block_decl (d : Ast.decl Ast.node) : Ast.field_decl Ast.node option =
    match d.v with
    | Ast.DVar { name; dtype; _ } ->
        Some (Ast.node ~loc:d.loc { Ast.fname = name; ftype = dtype; fpos = None })
    | Ast.DConst { name; dtype = Some dtype; _ } ->
        Some (Ast.node ~loc:d.loc { Ast.fname = name; ftype = dtype; fpos = None })
    | _ -> None

  let block_fields_of_body = function
    | None -> []
    | Some (decls, _body_stmt) -> List.filter_map field_of_block_decl decls

  let empty_proc_header_info =
    (false, false, false, None)

  let merge_proc_header_info (a_rec, a_rent, a_inline, a_ret)
      (b_rec, b_rent, b_inline, b_ret) =
    let ret =
      match a_ret, b_ret with
      | Some x, _ -> Some x
      | None, Some y -> Some y
      | None, None -> None
    in
    (a_rec || b_rec, a_rent || b_rent, a_inline || b_inline, ret)

  let scalar_base_of_ident (id : Ast.ident) : Ast.scalar_base option =
    match String.uppercase_ascii id.v with
    | "U" -> Some Ast.ScalarUnsigned
    | "S" -> Some Ast.ScalarSigned
    | "F" -> Some Ast.ScalarFloat
    | "A" -> Some Ast.ScalarFixed
    | "B" -> Some Ast.ScalarBit
    | "C" -> Some Ast.ScalarChar
    | _ -> None

  let round_mode_of_ident (id : Ast.ident) : Ast.round_mode option =
    match String.uppercase_ascii id.v with
    | "R" -> Some Ast.Round
    | "T" -> Some Ast.Truncate
    | _ -> None

  let mk_type_expr_with_round sp ep (base : Ast.ident)
      (round : Ast.round_mode option) (sizes : Ast.expr Ast.node list) :
      Ast.type_expr Ast.node =
    match String.uppercase_ascii base.v, sizes with
    | "P", [{ v = Ast.EName pointed; loc = pointed_loc; _ }] ->
        let inner = Ast.node ~loc:pointed_loc (Ast.TName pointed) in
        n sp ep (Ast.TPointer inner)
    | "P", [] ->
        let inner = n sp ep (Ast.TName (nid sp ep "__untyped_pointer__")) in
        n sp ep (Ast.TPointer inner)
    | _, _ ->
        (match scalar_base_of_ident base with
         | Some scalar -> n sp ep (Ast.TScalar { base = scalar; round; sizes })
         | None ->
             match sizes with
             | [] -> n sp ep (Ast.TName base)
             | dims ->
                 let elem = Ast.node ~loc:base.loc (Ast.TName base) in
                 n sp ep (Ast.TArray { elem; dims }))

  let mk_type_expr sp ep (base : Ast.ident) (sizes : Ast.expr Ast.node list) :
      Ast.type_expr Ast.node =
    mk_type_expr_with_round sp ep base None sizes

  let for_step_stmt sp ep lhs (by_expr, then_expr) =
    match by_expr, then_expr with
    | Some inc, _ ->
        let rhs = n sp ep (Ast.EBinop { op = Ast.BAdd; lhs; rhs = inc }) in
        Some (n sp ep (Ast.SAssign { lhs; rhs }))
    | None, Some nextv ->
        Some (n sp ep (Ast.SAssign { lhs; rhs = nextv }))
    | None, None ->
        None

  let module_kind_of_header (items : Ast.toplevel list) : Ast.module_kind =
    let rec first_program = function
      | Ast.TopDecl { v = Ast.DDirective { name; args }; _ } :: _
        when String.uppercase_ascii name.v = "PROGRAM" ->
          Some (Ast.MainProgram (match args with x :: _ -> Some x | [] -> None))
      | _ :: xs -> first_program xs
      | [] -> None
    in
    let rec first_compool_module = function
      | Ast.TopDecl { v = Ast.DDirective { name; args }; _ } :: _
        when String.uppercase_ascii name.v = "COMPOOL_MODULE" ->
          Some (Ast.CompoolModule (match args with x :: _ -> Some x | [] -> None))
      | _ :: xs -> first_compool_module xs
      | [] -> None
    in
    match first_program items, first_compool_module items with
    | Some k, _ -> k
    | None, Some k -> k
    | None, None -> Ast.ProcedureModule
%}

/* identifiers + literals */
%token <string> ID
%token <string> FIXED_A
%token <string> INTLIT
%token <string> FLOATLIT
%token <string> STRINGLIT
%token <int * string * string> BITLIT
%token <string> BAD_CHAR
%token <string> BAD_STRING
%token <string> BAD_COMMENT
%token <string> BAD_DIRECTIVE
%token <string> BAD_LITERAL
%token TRUE FALSE NULL

/* keywords */
%token START TERM BEGIN END
%token DEF REF PROC
%token ITEM TABLE STATIC CONSTANT READONLY
%token INLINE OVERLAY
%token IF ELSE WHILE FOR BY THEN
%token CASE DEFAULT FALLTHRU
%token EXIT GOTO RETURN ABORT STOP
%token NOT AND OR XOR EQV MOD

/* “header / meta” keywords (safe even if used as IDs) */
%token PROGRAM COMPOOL ICOMPOOL DEFINE TYPE BLOCK LINKAGE ILINKAGE CODE ICODE
%token POS

/* conversion brackets */
%token CONV_L CONV_R

/* operators / punctuation */
%token PLUS MINUS STAR SLASH POW
%token EQ NE LT LE GT GE
%token LPAREN RPAREN COMMA SEMI COLON DOT BANG AT
%token EOF

%left OR
%left XOR EQV
%left AND
%right NOT
%nonassoc EQ NE LT LE GT GE
%left PLUS MINUS
%left STAR SLASH MOD
%right POW
%right UMINUS UPLUS

%start <Ast.program> program
%start <Ast.toplevel list> module_item_entry
%start <Ast.decl Ast.node list> declaration_entry
%start <(Ast.decl Ast.node list * Ast.stmt Ast.node) option> procedure_body_entry
%start <Ast.stmt Ast.node> statement_entry
%start <Ast.expr Ast.node> expression_entry

%%

/* =========================================================
   Program structure (avoid OCaml keyword `module`)
   ========================================================= */

program:
  | ms=jmodules EOF { List.concat ms }
  ;

module_item_entry:
  | x=module_item EOF { x }
  ;

declaration_entry:
  | ds=decl_item EOF { ds }
  ;

procedure_body_entry:
  | body=proc_body_opt EOF { body }
  ;

statement_entry:
  | s=statement EOF { s }
  ;

expression_entry:
  | e=expr EOF { e }
  ;

jmodules:
  | ms=rev_jmodules { List.rev ms }
  ;

rev_jmodules:
  | /* empty */ { [] }
  | ms=rev_jmodules m=jmodule { m :: ms }
  ;

jmodule:
  | START _t=terminator_opt hs=header_items body=module_items_opt TERM
      {
        let items = hs @ body in
        [Ast.TopModule (n $startpos $endpos
          { module_kind = module_kind_of_header items; module_items = items })]
      }
  | body=module_items TERM
      {
        [Ast.TopModule (n $startpos $endpos
          { module_kind = Ast.UnknownModule; module_items = body })]
      }
  | body=module_items
      {
        [Ast.TopModule (n $startpos $endpos
          { module_kind = Ast.UnknownModule; module_items = body })]
      }
  ;

/* Accept PROGRAM/COMPOOL lines in the header area. */
header_items:
  | hs=rev_header_items { List.concat (List.rev hs) }
  ;

rev_header_items:
  | /* empty */ { [] }
  | hs=rev_header_items h=header_item { h :: hs }
  ;

header_item:
  | _skip=ignored_bang_directive { [] }
  | PROGRAM nm=ident _t=terminator_opt
      {
        let d = n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "PROGRAM"; args = [nm] }) in
        [Ast.TopDecl d]
      }
  | COMPOOL nm=ident _t=terminator_opt
      {
        let d = n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "COMPOOL_MODULE"; args = [nm] }) in
        [Ast.TopDecl d]
      }
  | d=directive_decl { [Ast.TopDecl d] }
  ;

/* =========================================================
   Top-level items
   ========================================================= */

module_items_opt:
  | /* empty */ { [] }
  | body=module_items { body }
  ;

module_items:
  | xs=rev_module_items { List.concat (List.rev xs) }
  ;

rev_module_items:
  | x=module_item { [x] }
  | xs=rev_module_items x=module_item { x :: xs }
  ;

module_item:
  | ds=decl_item     { List.map (fun d -> Ast.TopDecl d) ds }
  | BEGIN ds=module_body_decl_section ss=block_list_opt END
      {
        let tops = List.map (fun d -> Ast.TopDecl d) ds in
        match ss with
        | [] -> tops
        | _ -> tops @ [Ast.TopStmt (mk_block $startpos $endpos ss)]
      }
  | s=statement      { [Ast.TopStmt s] }
  | error _t=terminator
      { bad_top $startpos $endpos }
  ;

module_body_decl_section:
  | ds=decl_item { ds }
  | ds=decl_item rest=module_body_decl_section { ds @ rest }
  ;

/* =========================================================
   Declarations
   ========================================================= */

decl_item:
  | _skip=ignored_bang_directive { [] }
  | d=directive_decl { [d] }
  | ds=group_decl    { ds }
  | ds=inline_proc_decl { ds }
  | ds=proc_decl     { ds }
  | ds=inline_invalid_data_decl { ds }
  | ds=data_decl     { ds }
  | ds=define_decl   { ds }
  | ds=type_decl     { ds }
  | ds=block_decl    { ds }
  | ds=overlay_decl  { ds }
  ;

inline_invalid_data_decl:
  | INLINE ITEM nm=ident st=static_opt ty=type_spec init=item_init_opt attrs=item_attrs_opt _t=terminator
      {
        Parse_diags.add (loc $startpos $endpos)
          "INLINE applies only to PROC declarations; ignoring INLINE on data declaration.";
        let storage = if st then Ast.Static else Ast.Automatic in
        [n $startpos $endpos
          (Ast.DVar {
            name = nm;
            dtype = ty;
            init;
            storage;
            external_modifier = Ast.LocalDecl;
            data_decl_kind = Ast.DataItem;
            is_readonly = attrs;
          })]
      }
  | INLINE TABLE nm=ident st=static_opt dims=table_dims_opt elem_ty_opt=table_elem_type_opt preset=table_preset_opt recopt_before=record_opt _t=terminator recopt_after=table_record_after_term_opt
      {
        Parse_diags.add (loc $startpos $endpos)
          "INLINE applies only to PROC declarations; ignoring INLINE on data declaration.";
        let storage = if st then Ast.Static else Ast.Automatic in
        let ty =
          mk_table_type $startpos $endpos dims elem_ty_opt recopt_before
            recopt_after
        in
        [n $startpos $endpos
          (Ast.DVar {
            name = nm;
            dtype = ty;
            init = preset;
            storage;
            external_modifier = Ast.LocalDecl;
            data_decl_kind = Ast.DataTable;
            is_readonly = false;
          })]
      }
  ;

inline_proc_decl:
  | INLINE PROC nm=ident pre=proc_header_tail_opt formals=formals_opt post=proc_header_tail_opt _t=terminator linkage=proc_linkage_opt _skip=ignored_bang_directives_opt body=proc_body_opt
      {
        let seen_rec, seen_rent, _seen_inline, ret =
          merge_proc_header_info pre post
        in
        let use_attr = proc_use_from_flags seen_rec seen_rent in
        let params = formals in
        let locals, body_stmt =
          match body with
          | None -> ([], mk_block $startpos $endpos [])
          | Some (ds, st) -> (ds, st)
        in
        let proc =
          n $startpos $endpos
            {
              Ast.name = nm;
              params;
              returns = ret;
              use_attr;
              linkage;
              locals;
              body = body_stmt;
              external_modifier = Ast.LocalDecl;
              has_body = body <> None;
              is_inline = true;
            }
        in
        [n $startpos $endpos (Ast.DProc proc)]
      }
  ;

/* Directives: COMPOOL imports participate in the workspace model.  Other bang
   compiler controls are skipped through their semicolon so they cannot create
   stray parse errors or synthetic declarations. */
directive_decl:
  | BANG COMPOOL LPAREN arg=compool_arg RPAREN rest=directive_args_opt _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "COMPOOL"; args = arg :: rest }) }
  | BANG ICOMPOOL LPAREN arg=compool_arg RPAREN rest=directive_args_opt _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "COMPOOL"; args = arg :: rest }) }
  | BANG COMPOOL arg=compool_arg rest=directive_args_opt _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "COMPOOL"; args = arg :: rest }) }
  | BANG ICOMPOOL arg=compool_arg rest=directive_args_opt _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "COMPOOL"; args = arg :: rest }) }
  | COMPOOL arg=compool_arg _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "COMPOOL"; args = [arg] }) }
  | ICOMPOOL arg=compool_arg _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "COMPOOL"; args = [arg] }) }
  ;

compool_arg:
  | nm=ident_or_soft_keyword { nm }
  | s=STRINGLIT { nid $startpos $endpos s }
  ;

ignored_bang_directives_opt:
  | /* empty */ { () }
  | ignored_bang_directives_opt _skip=ignored_bang_directive { () }
  ;

ignored_bang_directive:
  | BANG SEMI { () }
  | BANG _first=ignored_bang_initial _rest=ignored_bang_payload SEMI { () }
  ;

ignored_bang_payload:
  | /* empty */ { () }
  | ignored_bang_payload _tok=ignored_bang_token { () }
  ;

ignored_bang_initial:
  | _tok=ignored_bang_token_non_compool { () }
  ;

ignored_bang_token_non_compool:
  | _tok=ignored_bang_token_non_compool_keyword { () }
  | _id=ID { () }
  | _id=FIXED_A { () }
  | _lit=INTLIT { () }
  | _lit=FLOATLIT { () }
  | _lit=STRINGLIT { () }
  | _lit=BITLIT { () }
  | TRUE { () }
  | FALSE { () }
  | NULL { () }
  | CONV_L { () }
  | CONV_R { () }
  | PLUS { () }
  | MINUS { () }
  | STAR { () }
  | SLASH { () }
  | POW { () }
  | EQ { () }
  | NE { () }
  | LT { () }
  | LE { () }
  | GT { () }
  | GE { () }
  | LPAREN { () }
  | RPAREN { () }
  | COMMA { () }
  | COLON { () }
  | DOT { () }
  | BANG { () }
  | AT { () }
  ;

ignored_bang_token_non_compool_keyword:
  | START { () }
  | TERM { () }
  | BEGIN { () }
  | END { () }
  | DEF { () }
  | REF { () }
  | PROC { () }
  | ITEM { () }
  | TABLE { () }
  | STATIC { () }
  | CONSTANT { () }
  | READONLY { () }
  | INLINE { () }
  | OVERLAY { () }
  | IF { () }
  | ELSE { () }
  | WHILE { () }
  | FOR { () }
  | BY { () }
  | THEN { () }
  | CASE { () }
  | DEFAULT { () }
  | FALLTHRU { () }
  | EXIT { () }
  | GOTO { () }
  | RETURN { () }
  | ABORT { () }
  | STOP { () }
  | NOT { () }
  | AND { () }
  | OR { () }
  | XOR { () }
  | EQV { () }
  | MOD { () }
  | PROGRAM { () }
  | DEFINE { () }
  | TYPE { () }
  | BLOCK { () }
  | LINKAGE { () }
  | ILINKAGE { () }
  | CODE { () }
  | ICODE { () }
  | POS { () }
  ;

ignored_bang_token:
  | _tok=ignored_bang_token_non_compool { () }
  | COMPOOL { () }
  | ICOMPOOL { () }
  ;

directive_args_opt:
  | /* empty */ { [] }
  | args=directive_args { args }
  | LPAREN args=directive_args_opt RPAREN { args }
  ;

directive_args:
  | xs=rev_directive_args { List.rev xs }
  ;

rev_directive_args:
  | a=directive_arg { [a] }
  | xs=rev_directive_args COMMA a=directive_arg { a :: xs }
  | xs=rev_directive_args a=directive_arg { a :: xs }
  ;

directive_arg:
  | s=STRINGLIT { nid $startpos $endpos s }
  | i=INTLIT    { nid $startpos $endpos i }
  | f=FLOATLIT  { nid $startpos $endpos f }
  | b=BITLIT    { let _, _, raw = b in nid $startpos $endpos raw }
  | id=ident_or_soft_keyword { id }
  ;

ident_or_directive_keyword:
  | id=ident { id }
  | START { nid $startpos $endpos "START" }
  | TERM { nid $startpos $endpos "TERM" }
  | BEGIN { nid $startpos $endpos "BEGIN" }
  | END { nid $startpos $endpos "END" }
  | DEF { nid $startpos $endpos "DEF" }
  | REF { nid $startpos $endpos "REF" }
  | PROC { nid $startpos $endpos "PROC" }
  | ITEM { nid $startpos $endpos "ITEM" }
  | TABLE { nid $startpos $endpos "TABLE" }
  | STATIC { nid $startpos $endpos "STATIC" }
  | CONSTANT { nid $startpos $endpos "CONSTANT" }
  | READONLY { nid $startpos $endpos "READONLY" }
  | INLINE { nid $startpos $endpos "INLINE" }
  | OVERLAY { nid $startpos $endpos "OVERLAY" }
  | IF { nid $startpos $endpos "IF" }
  | ELSE { nid $startpos $endpos "ELSE" }
  | WHILE { nid $startpos $endpos "WHILE" }
  | FOR { nid $startpos $endpos "FOR" }
  | BY { nid $startpos $endpos "BY" }
  | THEN { nid $startpos $endpos "THEN" }
  | CASE { nid $startpos $endpos "CASE" }
  | DEFAULT { nid $startpos $endpos "DEFAULT" }
  | FALLTHRU { nid $startpos $endpos "FALLTHRU" }
  | EXIT { nid $startpos $endpos "EXIT" }
  | GOTO { nid $startpos $endpos "GOTO" }
  | RETURN { nid $startpos $endpos "RETURN" }
  | ABORT { nid $startpos $endpos "ABORT" }
  | STOP { nid $startpos $endpos "STOP" }
  | TRUE { nid $startpos $endpos "TRUE" }
  | FALSE { nid $startpos $endpos "FALSE" }
  | NULL { nid $startpos $endpos "NULL" }
  | PROGRAM { nid $startpos $endpos "PROGRAM" }
  | TYPE { nid $startpos $endpos "TYPE" }
  | BLOCK { nid $startpos $endpos "BLOCK" }
  | LINKAGE { nid $startpos $endpos "LINKAGE" }
  | ILINKAGE { nid $startpos $endpos "ILINKAGE" }
  ;

ident_or_soft_keyword:
  | id=ident { id }
  | PROGRAM { nid $startpos $endpos "PROGRAM" }
  | TYPE { nid $startpos $endpos "TYPE" }
  | BLOCK { nid $startpos $endpos "BLOCK" }
  | DEFAULT { nid $startpos $endpos "DEFAULT" }
  | NULL { nid $startpos $endpos "NULL" }
  ;

/* DEFINE declarations (Chapter 18):
   DEFINE name "string" ;
   DEFINE name(A,B) "string" ;
   DEFINE name(A,B) LISTEXP|LISTINV|LISTBOTH "string" ;
   DEFINE name LISTEXP|LISTINV|LISTBOTH "string" ;
   The lexer ensures the first "..." after DEFINE is STRINGLIT; later "..." on the
   same declaration are treated as comments.
*/
define_decl:
  | DEFINE nm=ident formals=define_formals_opt list_opt=define_list_opt s=STRINGLIT _t=terminator
      {
        let args =
          let xs = nm :: formals in
          let xs = match list_opt with None -> xs | Some lo -> xs @ [lo] in
          xs @ [nid $startpos(s) $endpos(s) s]
        in
        let d = n $startpos $endpos
          (Ast.DDirective { name = nid $startpos $endpos "DEFINE"; args })
        in
        [d]
      }
  ;

define_formals_opt:
  | /* empty */ { [] }
  | LPAREN RPAREN { [] }
  | LPAREN xs=define_formals RPAREN { xs }
  ;

define_formals:
  | xs=rev_define_formals { List.rev xs }
  ;

rev_define_formals:
  | x=ident { [x] }
  | xs=rev_define_formals COMMA x=ident { x :: xs }
  ;

define_list_opt:
  | /* empty */ { None }
  | lo=define_list_option { Some lo }
  ;

define_list_option:
  | id=ident
      {
        let k = String.uppercase_ascii id.v in
        if k = "LISTEXP" || k = "LISTINV" || k = "LISTBOTH" then id
        else (
          Parse_diags.add id.loc
            (Printf.sprintf
               "Unknown DEFINE list option %S (expected LISTEXP, LISTINV, or LISTBOTH)."
               id.v);
          id
        )
      }
  ;

/* TYPE declarations */
type_decl:
  | TYPE nm=ident ty=type_spec _t=terminator
      {
        [n $startpos $endpos (Ast.DType { name = nm; defn = ty; external_modifier = Ast.LocalDecl })]
      }
  | TYPE nm=ident ITEM ty=type_spec _t=terminator
      {
        [n $startpos $endpos (Ast.DType { name = nm; defn = ty; external_modifier = Ast.LocalDecl })]
      }
  /* TYPE T TABLE [dims] BASETYPE; */
  | TYPE nm=ident TABLE dims=table_dims_opt base=ident _t=terminator
      {
        let defn =
          match dims with
          | [] ->
              n $startpos(base) $endpos(base) (Ast.TName base)
          | _ ->
              let elem = n $startpos(base) $endpos(base) (Ast.TName base) in
              n $startpos $endpos (Ast.TArray { elem; dims })
        in
        [n $startpos $endpos (Ast.DType { name = nm; defn; external_modifier = Ast.LocalDecl })]
      }
  /* TYPE T TABLE [dims] [;] BEGIN ... END [;] */
  | TYPE nm=ident TABLE dims=table_dims_opt _t=terminator_opt BEGIN fs=field_decl_list END _tail=terminator_opt
      {
        let entry = n $startpos $endpos (Ast.TRecord fs) in
        let defn =
          match dims with
          | [] -> entry
          | _ -> n $startpos $endpos (Ast.TArray { elem = entry; dims })
        in
        [n $startpos $endpos (Ast.DType { name = nm; defn; external_modifier = Ast.LocalDecl })]
      }
  /* TYPE T BLOCK [;] BEGIN ... END [;] */
  | TYPE nm=ident BLOCK _t=terminator_opt body=proc_body_opt
      {
        let defn =
          n $startpos $endpos (Ast.TRecord (block_fields_of_body body))
        in
        [n $startpos $endpos (Ast.DType { name = nm; defn; external_modifier = Ast.LocalDecl })]
      }
  /* Legacy form used by existing examples: TYPE T TABLE W 1; BEGIN ... END */
  | TYPE nm=ident TABLE base=ident sizes=type_sizes_opt _t=terminator BEGIN fs=field_decl_list END
      {
        let elem = n $startpos $endpos (Ast.TRecord fs) in
        let defn =
          match String.uppercase_ascii base.v, sizes with
          | "W", [entry_size] ->
              n $startpos $endpos
                (Ast.TSpecifiedTable {
                  elem;
                  dims = [];
                  kind = Ast.SpecTableW entry_size;
                })
          | "V", [] ->
              n $startpos $endpos
                (Ast.TSpecifiedTable {
                  elem;
                  dims = [];
                  kind = Ast.SpecTableV None;
                })
          | "V", [entry_size] ->
              n $startpos $endpos
                (Ast.TSpecifiedTable {
                  elem;
                  dims = [];
                  kind = Ast.SpecTableV (Some entry_size);
                })
          | _ -> elem
        in
        [n $startpos $endpos (Ast.DType { name = nm; defn; external_modifier = Ast.LocalDecl })]
      }
  ;

/* BLOCK name [STATIC] ; BEGIN ... END
   Also accepts BLOCK name [STATIC] type-name [preset]; for block type usage. */
block_decl:
  | mod_=modifier_opt BLOCK nm=ident st=static_opt _t=terminator body=proc_body_opt
      {
        let storage =
          match mod_ with
          | None -> if st then Ast.Static else Ast.Automatic
          | Some _ -> Ast.External
        in
        let dtype =
          n $startpos $endpos (Ast.TRecord (block_fields_of_body body))
        in
        let block =
          n $startpos $endpos
            (Ast.DVar {
              name = nm;
              dtype;
              init = None;
              storage;
              external_modifier = external_modifier_of_string_opt mod_;
              data_decl_kind = Ast.DataBlock;
              is_readonly = false;
            })
        in
        match mod_ with
        | None -> [block]
        | Some m ->
            let md =
              n $startpos(mod_) $endpos(mod_)
                (Ast.DDirective {
                  name = nid $startpos(mod_) $endpos(mod_) m;
                  args = [nid $startpos(nm) $endpos(nm) nm.v];
                })
            in
            [md; block]
      }
  | mod_=modifier_opt BLOCK nm=ident st=static_opt ty=ident init=table_preset_opt _t=terminator
      {
        let storage =
          match mod_ with
          | None -> if st then Ast.Static else Ast.Automatic
          | Some _ -> Ast.External
        in
        let dtype = n $startpos(ty) $endpos(ty) (Ast.TName ty) in
        let block =
          n $startpos $endpos
            (Ast.DVar {
              name = nm;
              dtype;
              init;
              storage;
              external_modifier = external_modifier_of_string_opt mod_;
              data_decl_kind = Ast.DataBlock;
              is_readonly = false;
            })
        in
        match mod_ with
        | None -> [block]
        | Some m ->
            let md =
              n $startpos(mod_) $endpos(mod_)
                (Ast.DDirective {
                  name = nid $startpos(mod_) $endpos(mod_) m;
                  args = [nid $startpos(nm) $endpos(nm) nm.v];
                })
            in
            [md; block]
      }
  ;

modifier_opt:
  | /* empty */ { None }
  | DEF         { Some "DEF" }
  | REF         { Some "REF" }
  ;

modifier_req:
  | DEF { "DEF" }
  | REF { "REF" }
  ;

/* DEF/REF BEGIN ... END declaration groups */
group_decl:
  | mod_=modifier_req BEGIN ds=decl_section END
      {
        let md =
          n $startpos(mod_) $endpos(mod_)
            (Ast.DDirective { name = nid $startpos(mod_) $endpos(mod_) mod_; args = [] })
        in
        let m = external_modifier_of_req mod_ in
        md :: List.map (apply_external_modifier_to_decl m) ds
      }
  ;

overlay_decl:
  | OVERLAY nm=ident pos=overlay_pos_opt _eq=overlay_eq_opt items=overlay_items_clause _t=terminator_opt
      {
        [n $startpos $endpos
          (Ast.DOverlay {
            overlay_name = nm;
            overlay_items = items;
            overlay_pos = pos;
          })]
      }
  ;

overlay_eq_opt:
  | /* empty */ { () }
  | EQ { () }
  ;

overlay_pos_opt:
  | /* empty */ { None }
  | POS LPAREN e=expr RPAREN { Some e }
  ;

overlay_items_clause:
  | LPAREN items=overlay_items_opt RPAREN { items }
  | BEGIN items=overlay_items_opt END { items }
  ;

overlay_items_opt:
  | /* empty */ { [] }
  | xs=overlay_items { xs }
  ;

overlay_items:
  | x=overlay_item { [x] }
  | x=overlay_item COMMA xs=overlay_items { x :: xs }
  ;

overlay_item:
  | id=ident
      { n $startpos $endpos (Ast.OverlayTarget id) }
  | id=ident LPAREN e=expr RPAREN
      {
        let key = String.uppercase_ascii id.v in
        if key = "SPACER" || key = "SPACE" then
          n $startpos $endpos (Ast.OverlaySpacer e)
        else (
          Parse_diags.add id.loc
            (Printf.sprintf
               "Unknown OVERLAY item constructor %S; expected SPACER(...)."
               id.v);
          n $startpos $endpos (Ast.OverlayTarget id))
      }
  | STAR e=expr
      { n $startpos $endpos (Ast.OverlaySpacer e) }
  | LPAREN items=overlay_items_opt RPAREN
      { n $startpos $endpos (Ast.OverlayGroup items) }
  ;

/* ITEM name type [ - init ] ; */
data_decl:
  | mod_=modifier_opt ro=readonly_opt ITEM nm=ident st=static_opt ty=type_spec init=item_init_opt attrs=item_attrs_opt _t=terminator
      {
        let storage =
          match mod_ with
          | None -> Ast.Automatic
          | Some _ -> Ast.External
        in
        let storage =
          match storage with
          | Ast.Automatic when st -> Ast.Static
          | x -> x
        in
        let external_modifier = external_modifier_of_string_opt mod_ in
        warn_ref_preset mod_ init (loc $startpos $endpos);
        let var =
          n $startpos $endpos
            (Ast.DVar {
              name = nm;
              dtype = ty;
              init;
              storage;
              external_modifier;
              data_decl_kind = Ast.DataItem;
              is_readonly = ro || attrs;
            })
        in
        match mod_ with
        | None -> [var]
        | Some m ->
            let md =
              n $startpos(mod_) $endpos(mod_)
                (Ast.DDirective {
                  name = nid $startpos(mod_) $endpos(mod_) m;
                  args = [nid $startpos(nm) $endpos(nm) nm.v];
                })
            in
            [md; var]
      }

  | mod_=modifier_opt _ro=readonly_opt CONSTANT ITEM nm=ident st=static_opt ty=type_spec value=const_item_init _attrs=item_attrs_opt _t=terminator
      {
        let storage =
          match mod_ with
          | None -> Ast.Automatic
          | Some _ -> Ast.External
        in
        let storage =
          match storage with
          | Ast.Automatic when st -> Ast.Static
          | x -> x
        in
        let external_modifier = external_modifier_of_string_opt mod_ in
        warn_external_constant mod_ (loc $startpos $endpos);
        let c =
          n $startpos $endpos
            (Ast.DConst {
              name = nm;
              dtype = Some ty;
              value;
              external_modifier;
              data_decl_kind = Ast.DataItem;
            })
        in
        (match storage with
         | Ast.Static | Ast.External -> ()
         | Ast.Automatic ->
             Parse_diags.add (loc $startpos $endpos)
               "CONSTANT items should have static or external allocation.");
        match mod_ with
        | None -> [c]
        | Some m ->
            let md =
              n $startpos(mod_) $endpos(mod_)
                (Ast.DDirective {
                  name = nid $startpos(mod_) $endpos(mod_) m;
                  args = [nid $startpos(nm) $endpos(nm) nm.v];
                })
            in
            [md; c]
      }

  | mod_=modifier_opt _ro=readonly_opt CONSTANT TABLE nm=ident st=static_opt dims=table_dims_opt elem_ty_opt=table_elem_type_opt preset=table_preset_opt recopt_before=record_opt _t=terminator recopt_after=table_record_after_term_opt
      {
        let _storage =
          match mod_ with
          | None -> if st then Ast.Static else Ast.Automatic
          | Some _ -> Ast.External
        in
        let external_modifier = external_modifier_of_string_opt mod_ in
        warn_external_constant mod_ (loc $startpos $endpos);
        let ty =
          mk_table_type $startpos $endpos dims elem_ty_opt recopt_before
            recopt_after
        in
        let value =
          match preset with
          | Some p -> p
          | None -> mk_empty_table_preset $startpos $endpos
        in
        let c =
          n $startpos $endpos
            (Ast.DConst {
              name = nm;
              dtype = Some ty;
              value;
              external_modifier;
              data_decl_kind = Ast.DataTable;
            })
        in
        match mod_ with
        | None -> [c]
        | Some m ->
            let md =
              n $startpos(mod_) $endpos(mod_)
                (Ast.DDirective {
                  name = nid $startpos(mod_) $endpos(mod_) m;
                  args = [nid $startpos(nm) $endpos(nm) nm.v];
                })
            in
            [md; c]
      }

  | mod_=modifier_opt _ro=readonly_opt CONSTANT TABLE nm=ident st=static_opt dims=table_dims_opt BEGIN fs=field_decl_list END _tail=terminator_opt
      {
        let _storage =
          match mod_ with
          | None -> if st then Ast.Static else Ast.Automatic
          | Some _ -> Ast.External
        in
        let external_modifier = external_modifier_of_string_opt mod_ in
        warn_external_constant mod_ (loc $startpos $endpos);
        let elem_ty = n $startpos $endpos (Ast.TRecord fs) in
        let ty = n $startpos $endpos (Ast.TArray { elem = elem_ty; dims }) in
        let c =
          n $startpos $endpos
            (Ast.DConst {
              name = nm;
              dtype = Some ty;
              value = mk_empty_table_preset $startpos $endpos;
              external_modifier;
              data_decl_kind = Ast.DataTable;
            })
        in
        match mod_ with
        | None -> [c]
        | Some m ->
            let md =
              n $startpos(mod_) $endpos(mod_)
                (Ast.DDirective {
                  name = nid $startpos(mod_) $endpos(mod_) m;
                  args = [nid $startpos(nm) $endpos(nm) nm.v];
                })
            in
            [md; c]
      }

  /* TABLE name [ (dims) ] [elem-type] [ - preset ] [ , BEGIN fielddecls END ] ; [BEGIN fielddecls END] */
  | mod_=modifier_opt ro=readonly_opt TABLE nm=ident st=static_opt dims=table_dims_opt elem_ty_opt=table_elem_type_opt preset=table_preset_opt recopt_before=record_opt _t=terminator recopt_after=table_record_after_term_opt
      {
        let storage =
          match mod_ with
          | None -> Ast.Automatic
          | Some _ -> Ast.External
        in
        let storage =
          match storage with
          | Ast.Automatic when st -> Ast.Static
          | x -> x
        in
        let ty =
          mk_table_type $startpos $endpos dims elem_ty_opt recopt_before
            recopt_after
        in

        (* keep preset as init placeholder if you want; otherwise ignore *)
        let init = preset in

        let external_modifier = external_modifier_of_string_opt mod_ in
        warn_ref_preset mod_ init (loc $startpos $endpos);
        let var =
          n $startpos $endpos
            (Ast.DVar {
              name = nm;
              dtype = ty;
              init;
              storage;
              external_modifier;
              data_decl_kind = Ast.DataTable;
              is_readonly = ro;
            })
        in
        match mod_ with
        | None -> [var]
        | Some m ->
            let md =
              n $startpos(mod_) $endpos(mod_)
                (Ast.DDirective {
                  name = nid $startpos(mod_) $endpos(mod_) m;
                  args = [nid $startpos(nm) $endpos(nm) nm.v];
                })
            in
            [md; var]
      }
  ;

readonly_opt:
  | /* empty */ { false }
  | READONLY { true }
  ;

static_opt:
  | /* empty */ { false }
  | STATIC { true }
  ;

const_item_init:
  | MINUS e=expr { e }
  | EQ e=expr { e }
  ;

item_init_opt:
  | /* empty */ { None }
  | MINUS e=expr { Some e }
  | EQ e=expr { Some e }
  ;

item_attrs_opt:
  | /* empty */ { false }
  | xs=item_attrs { xs }
  ;

item_attrs:
  | x=item_attr { x }
  | x=item_attr xs=item_attrs { x || xs }
  ;

item_attr:
  | STATIC { false }
  | READONLY { true }
  | _id=ident { false }
  | _id=ident _p=attr_paren_payload { false }
  | _p=attr_paren_payload { false }
  ;

attr_paren_payload:
  | LPAREN _xs=attr_arg_list_opt RPAREN { () }
  ;

attr_arg_list_opt:
  | /* empty */ { () }
  | _xs=attr_arg_list { () }
  ;

attr_arg_list:
  | _x=attr_arg { () }
  | _x=attr_arg COMMA _xs=attr_arg_list { () }
  ;

attr_arg:
  | _i=INTLIT { () }
  | _f=FLOATLIT { () }
  | _s=STRINGLIT { () }
  | _id=ident { () }
  | _id=ident _p=attr_paren_payload { () }
  | _p=attr_paren_payload { () }
  ;

table_elem_type_opt:
  | /* empty */ { None }
  | ty=type_spec { Some ty }
  ;

/* Table/block preset support.
   J73 presets are comma lists and may contain omitted values, POS(...) positioners,
   and repetition counts such as 20(4(26),22).  We keep them as expression-shaped
   AST nodes so existing analyzer paths can still accept `expr node option`. */
table_preset_opt:
  | /* empty */ { None }
  | MINUS p=preset_expr { Some p }
  | EQ p=preset_expr    { Some p }
  ;

preset_expr:
  | items=preset_items
      {
        let base = n $startpos $endpos (Ast.ELit (Ast.LInt "0")) in
        n $startpos $endpos (Ast.EPreset { base; items })
      }
  ;

preset_items_opt:
  | /* empty */ { [] }
  | xs=preset_items { xs }
  ;

preset_items:
  | x=preset_item { [x] }
  | x=preset_item COMMA xs=preset_items { x :: xs }
  ;

preset_item:
  | /* empty (omitted preset value) */ { n $startpos $endpos Ast.EOmitted }
  | POS LPAREN idxs=expr_list RPAREN COLON v=preset_item
      { n $startpos $endpos (Ast.EPositioned { indexes = idxs; values = [v] }) }
  | i=INTLIT LPAREN items=preset_items_opt RPAREN
      {
        let count = n $startpos(i) $endpos(i) (Ast.ELit (Ast.LInt i)) in
        n $startpos $endpos (Ast.ERepeat { count; items })
      }
  | id=ident LPAREN items=preset_items_opt RPAREN
      {
        let count = n $startpos(id) $endpos(id) (Ast.EName id) in
        n $startpos $endpos (Ast.ERepeat { count; items })
      }
  | LPAREN count=expr RPAREN LPAREN items=preset_items_opt RPAREN
      { n $startpos $endpos (Ast.ERepeat { count; items }) }
  | LPAREN items=preset_items_opt RPAREN
      {
        let base = n $startpos $endpos (Ast.ELit (Ast.LInt "0")) in
        n $startpos $endpos (Ast.EPreset { base; items })
      }
  | e=expr { e }
  ;

table_dims:
  | LPAREN ds=dim_list_opt RPAREN { ds }
  | _open=CONV_L rest=leading_star_dim_tail RPAREN
      {
        let star =
          n $startpos(_open) $endpos(_open)
            (Ast.EName (nid $startpos(_open) $endpos(_open) "*"))
        in
        star :: rest
      }
  ;

leading_star_dim_tail:
  | /* empty */ { [] }
  | COMMA rest=dim_list { rest }
  ;

table_dims_opt:
  | /* empty */ { [] }
  | ds=table_dims { ds }
  ;

dim_list_opt:
  | /* empty */ { [] }
  | ds=dim_list { ds }
  ;

dim_list:
  | d=dim { [d] }
  | d=dim COMMA rest=dim_list { d :: rest }
  ;

dim:
  | STAR { n $startpos $endpos (Ast.EName (nid $startpos $endpos "*")) }
  | e=expr { e }
  | lo=expr COLON hi=expr
      {
        n $startpos $endpos (Ast.ERange { lo; hi })
      }
  | lo=expr MINUS hi=expr
      {
        n $startpos $endpos (Ast.ERange { lo; hi })
      }
  ;

record_opt:
  | /* empty */ { None }
  | COMMA BEGIN fs=field_decl_list END { Some fs }
  ;

table_record_after_term_opt:
  | /* empty */ { None }
  | BEGIN fs=field_decl_list END { Some fs }
  ;

field_decl_list:
  | /* empty */ { [] }
  | xs=rev_field_decl_items { List.filter_map (fun x -> x) (List.rev xs) }
  ;

rev_field_decl_items:
  | f=field_decl_item { [f] }
  | xs=rev_field_decl_items f=field_decl_item { f :: xs }
  ;

field_decl_item:
  | f=field_decl { Some f }
  | _skip=ignored_bang_directive { None }
  | _d=directive_decl { None }
  ;

field_decl:
  | ITEM nm=ident ty=type_spec _init=item_init_opt pos=field_attrs_opt _t=terminator
      { n $startpos $endpos { Ast.fname = nm; ftype = ty; fpos = pos } }
  ;

field_attrs_opt:
  | /* empty */ { None }
  | attrs=field_attrs { attrs }
  ;

field_attrs:
  | x=field_attr { x }
  | x=field_attr xs=field_attrs { match x with Some _ -> x | None -> xs }
  ;

field_attr:
  | POS LPAREN start_bit=expr COMMA start_word=expr RPAREN
      { Some { Ast.pos_start_bit = start_bit; pos_start_word = start_word } }
  | STATIC { None }
  | _id=ident { None }
  | _id=ident _p=attr_paren_payload { None }
  | _p=attr_paren_payload { None }
  ;

type_spec:
  | base=fixed_base round=round_attr_opt sizes=fixed_type_sizes_opt
      { mk_type_expr_with_round $startpos $endpos base round sizes }
  | base=type_base_ident tail=type_spec_tail
      { tail $startpos $endpos base }
  ;

type_spec_tail:
  | /* empty */
      { fun sp ep base -> mk_type_expr_with_round sp ep base None [] }
  | round=round_attr sizes=type_sizes_opt
      { fun sp ep base -> mk_type_expr_with_round sp ep base round sizes }
  | sizes=nonempty_type_sizes values=type_values_after_sizes_opt
      {
        fun sp ep base ->
          match values with
          | None -> mk_type_expr_with_round sp ep base None sizes
          | Some values ->
              if String.uppercase_ascii base.v = "STATUS" then
                n sp ep (Ast.TStatus values)
              else
                let elem = n sp ep (Ast.TName base) in
                let dims =
                  sizes
                  @ List.map
                      (fun (value : Ast.status_value Ast.node) ->
                        Ast.node ~loc:value.loc (Ast.EName value.v.sv_name))
                      values
                in
                n sp ep (Ast.TArray { elem; dims })
      }
  | LPAREN values=status_value_list RPAREN
      {
        fun sp ep base ->
          if String.uppercase_ascii base.v = "STATUS" then
            n sp ep (Ast.TStatus values)
          else
            let elem = n sp ep (Ast.TName base) in
            let dims =
              List.map
                (fun (value : Ast.status_value Ast.node) ->
                  Ast.node ~loc:value.loc (Ast.EName value.v.sv_name))
                values
            in
            n sp ep (Ast.TArray { elem; dims })
      }
  ;

type_values_after_sizes_opt:
  | /* empty */ { None }
  | LPAREN values=status_value_list RPAREN { Some values }
  ;

round_attr:
  | COMMA id=ident
      {
        match round_mode_of_ident id with
        | Some mode -> Some mode
        | None ->
            Parse_diags.add id.loc
              (Printf.sprintf
                 "Unknown round/truncate attribute %S; expected R or T." id.v);
            None
      }
  ;

round_attr_opt:
  | /* empty */ { None }
  | mode=round_attr { mode }
  ;

type_base_ident:
  | s=ID { nid $startpos $endpos s }
  ;

fixed_base:
  | s=FIXED_A { nid $startpos $endpos s }
  ;

fixed_type_sizes_opt:
  | /* empty */ { [] }
  | s=type_size { [s] }
  | whole=type_size fraction=type_size { [whole; fraction] }
  | whole=type_size COMMA fraction=type_size { [whole; fraction] }
  ;

status_value_list:
  | xs=rev_status_value_list { List.rev xs }
  ;

rev_status_value_list:
  | x=status_value { [x] }
  | xs=rev_status_value_list COMMA x=status_value { x :: xs }
  ;

status_value:
  | nm=status_name rep=status_representation_opt
      { n $startpos $endpos { Ast.sv_name = nm; sv_representation = rep } }
  | ctor=ident LPAREN nm=status_name RPAREN rep=status_representation_opt
      {
        if String.uppercase_ascii ctor.v <> "V" then
          Parse_diags.add ctor.loc
            (Printf.sprintf
               "Unknown STATUS value constructor %S (expected V)."
               ctor.v);
        n $startpos $endpos { Ast.sv_name = nm; sv_representation = rep }
      }
  ;

status_representation_opt:
  | /* empty */ { None }
  | EQ e=expr { Some e }
  | MINUS e=expr { Some (n $startpos $endpos (Ast.EUnop { op = Ast.UMinus; rhs = e })) }
  ;

status_name:
  | id=ident_or_directive_keyword { id }
  | NOT { nid $startpos $endpos "NOT" }
  | AND { nid $startpos $endpos "AND" }
  | OR { nid $startpos $endpos "OR" }
  | XOR { nid $startpos $endpos "XOR" }
  | EQV { nid $startpos $endpos "EQV" }
  | MOD { nid $startpos $endpos "MOD" }
  | COMPOOL { nid $startpos $endpos "COMPOOL" }
  | ICOMPOOL { nid $startpos $endpos "ICOMPOOL" }
  | POS { nid $startpos $endpos "POS" }
  ;

type_sizes_opt:
  | /* empty */ { [] }
  | s=type_size rest=type_sizes_opt { s :: rest }
  ;

type_sizes_no_paren_opt:
  | /* empty */ { [] }
  | s=type_size_nonparen rest=type_sizes_no_paren_opt { s :: rest }
  ;

nonempty_type_sizes:
  | s=type_size_nonparen { [s] }
  | s=type_size_nonparen rest=nonempty_type_sizes { s :: rest }
  ;

type_size_nonparen:
  | i=INTLIT
      { n $startpos $endpos (Ast.ELit (Ast.LInt i)) }
  | f=FLOATLIT
      { n $startpos $endpos (Ast.ELit (Ast.LFloat f)) }
  | id=ident
      { n $startpos $endpos (Ast.EName id) }
  ;

type_size:
  | i=INTLIT
      { n $startpos $endpos (Ast.ELit (Ast.LInt i)) }
  | f=FLOATLIT
      { n $startpos $endpos (Ast.ELit (Ast.LFloat f)) }
  | id=ident
      { n $startpos $endpos (Ast.EName id) }
  | LPAREN e=expr RPAREN
      { e }
  ;

proc_decl:
  | mod_=modifier_opt inline_prefix=inline_opt PROC nm=ident pre=proc_header_tail_opt formals=formals_opt post=proc_header_tail_opt _t=terminator linkage=proc_linkage_opt _skip=ignored_bang_directives_opt body=proc_body_opt
      {
        let seen_rec, seen_rent, seen_inline, ret =
          merge_proc_header_info pre post
        in
        let use_attr = proc_use_from_flags seen_rec seen_rent in
        let params = formals in
        let locals, body_stmt =
          match body with
          | None -> ([], mk_block $startpos $endpos [])
          | Some (ds, st) -> (ds, st)
        in
        let proc =
          n $startpos $endpos
            {
              Ast.name = nm;
              params;
              returns = ret;
              use_attr;
              linkage;
              locals;
              body = body_stmt;
              external_modifier = external_modifier_of_string_opt mod_;
              has_body = body <> None;
              is_inline = inline_prefix || seen_inline;
            }
        in
        let dproc = n $startpos $endpos (Ast.DProc proc) in
        match mod_ with
        | None -> [dproc]
        | Some m ->
            let md =
              n $startpos(mod_) $endpos(mod_)
                (Ast.DDirective {
                  name = nid $startpos(mod_) $endpos(mod_) m;
                  args = [nid $startpos(nm) $endpos(nm) nm.v];
                })
            in
            [md; dproc]
      }
  ;

proc_linkage_opt:
  | /* empty */ { None }
  | ds=proc_linkage_directives
      {
        match List.rev ds with
        | last :: _ -> Some last
        | [] -> None
      }
  ;

proc_linkage_directives:
  | d=proc_linkage_directive { [d] }
  | ds=proc_linkage_directives d=proc_linkage_directive { d :: ds }
  ;

proc_linkage_directive:
  | ILINKAGE sym=linkage_symbol _t=terminator
      { sym }
  | LINKAGE sym=linkage_symbol _t=terminator
      { sym }
  | ILINKAGE LPAREN sym=linkage_symbol RPAREN _t=terminator
      { sym }
  | LINKAGE LPAREN sym=linkage_symbol RPAREN _t=terminator
      { sym }
  ;

linkage_symbol:
  | id=ident_or_soft_keyword { id }
  | s=STRINGLIT { nid $startpos $endpos s }
  ;

inline_opt:
  | /* empty */ { false }
  | INLINE { true }
  ;

proc_header_tail_opt:
  | /* empty */ { empty_proc_header_info }
  | xs=proc_header_tail { xs }
  ;

proc_header_tail:
  | x=proc_header_atom { x }
  | x=proc_header_atom xs=proc_header_tail { merge_proc_header_info x xs }
  ;

proc_header_atom:
  | INLINE { (false, false, true, None) }
  | ty=proc_header_type_spec
      {
        let attr_with_trailing_return seen_rec seen_rent dims =
          match dims with
          | { v = Ast.EName base; _ } :: rest ->
              ( seen_rec,
                seen_rent,
                false,
                Some (mk_type_expr $startpos $endpos base rest) )
          | _ -> (seen_rec, seen_rent, false, None)
        in
        match ty.v with
        | Ast.TName id ->
            let k = String.uppercase_ascii id.v in
            if k = "REC" || k = "RECURSIVE" then
              (true, false, false, None)
            else if k = "RENT" || k = "REENTRANT" then
              (false, true, false, None)
            else
              (false, false, false, Some ty)
        | Ast.TArray { elem = { v = Ast.TName id; _ }; dims } ->
            let k = String.uppercase_ascii id.v in
            if k = "REC" || k = "RECURSIVE" then
              attr_with_trailing_return true false dims
            else if k = "RENT" || k = "REENTRANT" then
              attr_with_trailing_return false true dims
            else
              (false, false, false, Some ty)
        | _ ->
            (false, false, false, Some ty)
      }
  | _i=INTLIT { empty_proc_header_info }
  | _f=FLOATLIT { empty_proc_header_info }
  | _s=STRINGLIT { empty_proc_header_info }
  ;

proc_header_type_spec:
  | base=fixed_base sizes=fixed_type_sizes_opt
      { mk_type_expr $startpos $endpos base sizes }
  | base=type_base_ident sizes=type_sizes_no_paren_opt
      { mk_type_expr $startpos $endpos base sizes }
  ;

formals_opt:
  | /* empty */ { [] }
  | LPAREN RPAREN { [] }
  | LPAREN ps=formal_param_groups RPAREN { ps }
  ;

formal_param_groups:
  | ins=id_list outs=outs_opt
      {
        let unknown_ty =
          n $startpos $endpos (Ast.TName (nid $startpos $endpos "__implicit__"))
        in
        let mkp mode (id : Ast.ident) =
          Ast.node ~loc:id.loc { Ast.pname = id; pmode = mode; ptype = unknown_ty }
        in
        (List.map (mkp Ast.In) ins) @ (List.map (mkp Ast.Out) outs)
      }
  | COLON outs=id_list
      {
        let unknown_ty =
          n $startpos $endpos (Ast.TName (nid $startpos $endpos "__implicit__"))
        in
        let mkp mode (id : Ast.ident) =
          Ast.node ~loc:id.loc { Ast.pname = id; pmode = mode; ptype = unknown_ty }
        in
        List.map (mkp Ast.Out) outs
      }
  ;

outs_opt:
  | /* empty */ { [] }
  | COLON outs=id_list { outs }
  ;

id_list:
  | x=ident { [x] }
  | x=ident COMMA xs=id_list { x :: xs }
  | x=ident xs=id_list { x :: xs }
  ;

proc_body_opt:
  | /* empty */ { None }
  | BEGIN ds=decl_section ss=block_list_opt END
      { Some (ds, mk_block $startpos $endpos ss) }
  | s=simple_or_control_stmt
      { Some ([], s) }
  ;

decl_section:
  | xs=rev_decl_section { List.concat (List.rev xs) }
  ;

rev_decl_section:
  | /* empty */ { [] }
  | xs=rev_decl_section ds=data_decl { ds :: xs }
  | xs=rev_decl_section ds=proc_decl { ds :: xs }
  | xs=rev_decl_section _skip=ignored_bang_directive { [] :: xs }
  | xs=rev_decl_section d=directive_decl { [d] :: xs }
  | xs=rev_decl_section ds=define_decl { ds :: xs }
  | xs=rev_decl_section ds=type_decl   { ds :: xs }
  | xs=rev_decl_section ds=block_decl  { ds :: xs }
  | xs=rev_decl_section ds=overlay_decl { ds :: xs }
  ;

/* =========================================================
   Statements + Recovery (sync on terminator)
   ========================================================= */

statement:
  | _skip=ignored_bang_directive
      { n $startpos $endpos Ast.SEmpty }
  | d=directive_decl
      { n $startpos $endpos (Ast.SDecl d) }
  | labs=labels s=simple_or_control_stmt { wrap_labels labs s }
  | callee=ident LPAREN args=postfix_call_args_opt RPAREN _t=terminator_opt
      { n $startpos $endpos (Ast.SCallStmt { callee; args; abort_label = None }) }
  | callee=ident LPAREN args=postfix_call_args_opt RPAREN ABORT lab=ident _t=terminator_opt
      { n $startpos $endpos (Ast.SCallStmt { callee; args; abort_label = Some lab }) }
  | callee=ident LPAREN args=postfix_call_args_opt RPAREN EQ rhs=expr _t=terminator_opt
      {
        let lhs = n $startpos(callee) $endpos(callee) (Ast.ECall { callee; args }) in
        n $startpos $endpos (Ast.SAssign { lhs; rhs })
      }
  | callee=ident LPAREN args=postfix_call_args_opt RPAREN MINUS rhs=expr _t=terminator_opt
      {
        let lhs = n $startpos(callee) $endpos(callee) (Ast.ECall { callee; args }) in
        n $startpos $endpos (Ast.SAssign { lhs; rhs })
      }
  | lhs=lvalue EQ rhs=expr _t=terminator_opt
      { n $startpos $endpos (Ast.SAssign { lhs; rhs }) }
  | lhs=lvalue MINUS rhs=expr _t=terminator_opt
      { n $startpos $endpos (Ast.SAssign { lhs; rhs }) }
  | s=compound_stmt { s }
  | e=postfix _t=terminator
      {
        match e.v with
        | Ast.ECall { callee; args } ->
            n $startpos $endpos
              (Ast.SCallStmt { callee; args; abort_label = None })
        | Ast.EName callee ->
            n $startpos $endpos
              (Ast.SCallStmt { callee; args = []; abort_label = None })
        | _ -> n $startpos $endpos Ast.SEmpty
      }
  | e=postfix ABORT lab=ident _t=terminator
      {
        match e.v with
        | Ast.ECall { callee; args } ->
            n $startpos $endpos
              (Ast.SCallStmt { callee; args; abort_label = Some lab })
        | Ast.EName callee ->
            n $startpos $endpos
              (Ast.SCallStmt { callee; args = []; abort_label = Some lab })
        | _ -> n $startpos $endpos Ast.SEmpty
      }
  | s=simple_or_control_stmt { s }
  | error _t=terminator { bad_stmt $startpos $endpos }
  ;

labels:
  | labs=rev_labels { List.rev labs }
  ;

rev_labels:
  | l=label { [l] }
  | labs=rev_labels l=label { l :: labs }
  ;

label:
  | nm=ident COLON { nm }
  ;

compound_stmt:
  | BEGIN ss=block_list_opt END
      { mk_block $startpos $endpos ss }
  ;

block_list_opt:
  | /* empty */ { [] }
  | ss=block_list { ss }
  ;

block_list:
  | s=statement { [s] }
  | s=statement ss=block_list { s :: ss }
  | error _t=terminator ss=block_list { bad_stmt $startpos $endpos :: ss }
  ;

simple_or_control_stmt:
  | s=if_stmt       { s }
  | s=while_stmt    { s }
  | s=for_stmt      { s }
  | s=case_stmt     { s }
  | s=assign_stmt _t=terminator_opt { s }
  | s=goto_stmt _t=terminator_opt { s }
  | s=return_stmt _t=terminator_opt { s }
  | s=exit_stmt _t=terminator_opt { s }
  | s=abort_stmt _t=terminator_opt { s }
  | s=stop_stmt _t=terminator_opt { s }
  ;

terminator:
  | SEMI { () }
  | COMMA { () }
  ;

terminator_opt:
  | /* empty */ { () }
  | _t=terminator { () }
  ;

assign_stmt:
  | lhs=lvalue MINUS rhs=expr
      { mk_assign_stmt $startpos $endpos [lhs] rhs }
  | lhs=lvalue EQ rhs=expr
      { mk_assign_stmt $startpos $endpos [lhs] rhs }
  | lhs=lvalue COMMA lhses=lvalue_list MINUS rhs=expr
      { mk_assign_stmt $startpos $endpos (lhs :: lhses) rhs }
  | lhs=lvalue COMMA lhses=lvalue_list EQ rhs=expr
      { mk_assign_stmt $startpos $endpos (lhs :: lhses) rhs }
  | lhses=lvalue_list MINUS rhs=expr
      { mk_assign_stmt $startpos $endpos lhses rhs }
  | lhses=lvalue_list EQ rhs=expr
      { mk_assign_stmt $startpos $endpos lhses rhs }
  ;

lvalue_list:
  | x=lvalue { [x] }
  | x=lvalue COMMA xs=lvalue_list { x :: xs }
  ;

goto_stmt:
  | GOTO nm=ident { n $startpos $endpos (Ast.SGoto nm) }
  ;

return_stmt:
  | RETURN eo=expr_opt { n $startpos $endpos (Ast.SReturn eo) }
  ;

exit_stmt:
  | EXIT { n $startpos $endpos (Ast.SCallStmt { callee = nid $startpos $endpos "EXIT"; args = []; abort_label = None }) }
  ;

abort_stmt:
  | ABORT { n $startpos $endpos (Ast.SCallStmt { callee = nid $startpos $endpos "ABORT"; args = []; abort_label = None }) }
  ;

stop_stmt:
  | STOP eo=expr_opt
      {
        let args = match eo with None -> [] | Some e -> [e] in
        n $startpos $endpos (Ast.SCallStmt { callee = nid $startpos $endpos "STOP"; args; abort_label = None })
      }
  ;

actual_list:
  | ins=expr_list outs=actual_outs_opt { ins @ outs }
  | COLON outs=expr_list { outs }
  ;

actual_outs_opt:
  | /* empty */ { [] }
  | COLON outs=expr_list { outs }
  ;

expr_list:
  | e=expr { [e] }
  | e=expr COMMA es=expr_list { e :: es }
  ;

if_stmt:
  | IF test=expr SEMI then_=statement else_=else_opt
      { n $startpos $endpos (Ast.SIf { cond = test; then_; else_ }) }
  | IF test=expr then_=statement else_=else_opt
      { n $startpos $endpos (Ast.SIf { cond = test; then_; else_ }) }
  ;

else_opt:
  | /* empty */ { None }
  | ELSE s=statement { Some s }
  ;

while_stmt:
  | WHILE cond=expr SEMI body=statement
      { n $startpos $endpos (Ast.SWhile { cond; body }) }
  ;

for_stmt:
  | FOR lc=ident COLON initv=expr tail=for_tail SEMI body=statement
      {
        let lhs = n $startpos(lc) $endpos(lc) (Ast.EName lc) in
        let init_stmt = n $startpos $endpos (Ast.SAssign { lhs; rhs = initv }) in
        let cond, update = tail in
        let step_stmt = for_step_stmt $startpos $endpos lhs update in
        n $startpos $endpos (Ast.SFor { init = Some init_stmt; cond; step = step_stmt; body })
      }
  ;

for_tail:
  | /* empty */ { (None, (None, None)) }
  | wh=while_phrase { (Some wh, (None, None)) }
  | up=for_update wh=while_opt { (wh, up) }
  | wh=while_phrase up=for_update { (Some wh, up) }
  ;

for_update:
  | BY e=expr   { (Some e, None) }
  | THEN e=expr { (None, Some e) }
  ;

while_phrase:
  | WHILE e=expr { e }
  ;

while_opt:
  | /* empty */ { None }
  | wh=while_phrase { Some wh }
  ;

case_stmt:
  | CASE scrut=expr _sel_term=terminator_opt BEGIN opts=case_options END _tail=terminator_opt
      {
        n $startpos $endpos
          (Ast.SCase { selector = scrut; options = opts })
      }
  ;

case_options:
  | /* empty */ { [] }
  | o=case_option os=case_options { o :: os }
  ;

case_option:
  | DEFAULT _sep=case_sep body=statement fall=fallthru_opt
      {
        let idx = n $startpos $endpos Ast.CaseDefault in
        n $startpos $endpos
          { Ast.case_indexes = [idx]; case_body = body; case_fallthru = fall }
      }
  | LPAREN DEFAULT RPAREN _sep=case_sep body=statement fall=fallthru_opt
      {
        let idx = n $startpos $endpos Ast.CaseDefault in
        n $startpos $endpos
          { Ast.case_indexes = [idx]; case_body = body; case_fallthru = fall }
      }
  | LPAREN idxs=case_index_list RPAREN _sep=case_sep body=statement fall=fallthru_opt
      {
        n $startpos $endpos
          { Ast.case_indexes = idxs; case_body = body; case_fallthru = fall }
      }
  | error _t=terminator
      {
        let body = bad_stmt $startpos $endpos in
        n $startpos $endpos
          { Ast.case_indexes = []; case_body = body; case_fallthru = false }
      }
  ;

case_sep:
  | COLON { () }
  | BANG  { () }
  | SEMI  { () }
  ;

fallthru_opt:
  | /* empty */ { false }
  | FALLTHRU _t=terminator_opt { true }
  ;

case_index_list:
  | i=case_index { [i] }
  | i=case_index COMMA is=case_index_list { i :: is }
  ;

case_index:
  | lo=expr COLON hi=expr { n $startpos $endpos (Ast.CaseRange (lo, hi)) }
  | DEFAULT { n $startpos $endpos Ast.CaseDefault }
  | e=expr  { n $startpos $endpos (Ast.CaseValue e) }
  ;

/* =========================================================
   Expressions (include conversion (* type *)expr)
   ========================================================= */

expr_opt:
  | /* empty */ { None }
  | e=expr      { Some e }
  ;

ident:
  | s=ID { nid $startpos $endpos s }
  | s=FIXED_A { nid $startpos $endpos s }
  | LINKAGE { nid $startpos $endpos "LINKAGE" }
  | CODE {nid $startpos $endpos "CODE"}
  ;

literal:
  | i=INTLIT    { Ast.LInt i }
  | f=FLOATLIT  { Ast.LFloat f }
  | b=BITLIT    { let bead_size, beads, raw = b in Ast.LBit { bead_size; beads; raw } }
  | s=STRINGLIT { Ast.LString s }
  | TRUE        { Ast.LBool true }
  | FALSE       { Ast.LBool false }
  | NULL        { Ast.LNull }
  ;

primary:
  | id=ident { n $startpos $endpos (Ast.EName id) }
  | lit=literal { n $startpos $endpos (Ast.ELit lit) }
  | LPAREN e=expr RPAREN { n $startpos $endpos (Ast.EParen e) }
  | CONV_L ty=type_spec CONV_R rhs=primary
      { mk_conv $startpos $endpos ty rhs }
  ;

postfix_atom:
  | p=primary { p }
  | base=postfix_atom DOT fld=ident
      { n $startpos $endpos (Ast.EField { base; field = fld }) }
  | base=postfix_atom LPAREN args=postfix_call_args_opt RPAREN
      {
        match base.v with
        | Ast.EName callee -> n $startpos $endpos (Ast.ECall { callee; args })
        | _ -> n $startpos $endpos (Ast.EIndex { base; index = args })
      }
  ;

postfix_call_args_opt:
  | /* empty */ { [] }
  | es=actual_list { es }
  ;

postfix:
  | p=postfix_atom { p }
  | AT ptr=postfix_atom
      { n $startpos $endpos (Ast.EDeref { ptr }) }
  | field=postfix AT ptr=postfix_atom
      { n $startpos $endpos (Ast.EAt { field; ptr }) }
  ;

lvalue:
  | p=postfix { p }
  ;

expr:
  | e=postfix { e }
  | NOT rhs=expr { n $startpos $endpos (Ast.EUnop { op = Ast.UNot; rhs }) }
  | PLUS rhs=expr %prec UPLUS { n $startpos $endpos (Ast.EUnop { op = Ast.UPlus; rhs }) }
  | MINUS rhs=expr %prec UMINUS { n $startpos $endpos (Ast.EUnop { op = Ast.UMinus; rhs }) }

  | lhs=expr POW rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BPow; lhs; rhs }) }

  | lhs=expr STAR rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BMul; lhs; rhs }) }
  | lhs=expr SLASH rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BDiv; lhs; rhs }) }
  | lhs=expr MOD rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BMod; lhs; rhs }) }

  | lhs=expr PLUS rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BAdd; lhs; rhs }) }
  | lhs=expr MINUS rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BSub; lhs; rhs }) }

  | lhs=expr LT rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BLt; lhs; rhs }) }
  | lhs=expr LE rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BLe; lhs; rhs }) }
  | lhs=expr GT rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BGt; lhs; rhs }) }
  | lhs=expr GE rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BGe; lhs; rhs }) }
  | lhs=expr EQ rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BEq; lhs; rhs }) }
  | lhs=expr NE rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BNe; lhs; rhs }) }

  | lhs=expr AND rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BAnd; lhs; rhs }) }
  | lhs=expr OR rhs=expr  { n $startpos $endpos (Ast.EBinop { op = Ast.BOr; lhs; rhs }) }
  | lhs=expr XOR rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BBitXor; lhs; rhs }) }
  | lhs=expr EQV rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BEqv; lhs; rhs }) }
  ;

%%

module T = Lsp.Types
module I = MenhirInterpreter

type output = {
  ast : Ast.program option;
  diags : T.Diagnostic.t list;
  recovery_diags : T.Diagnostic.t list;
  tainted_ranges : tainted_range list;
  parse_health : parse_health;
  parse_confidence : float;
  ast_dump : string option;
}

and parse_health =
  | ParseClean
  | ParseRecovered
  | ParsePartial
  | ParseSkeletonOnly
  | ParseLexicalOnly
  | ParseFailedInternal

and recovery_kind =
  | RecoverTokenInsertion
  | RecoverTokenDeletion
  | RecoverBlockCloseInsertion
  | RecoverSyncSkip
  | RecoverIslandFallback
  | RecoverSkeletonFallback
  | RecoverGrammarError
  | RecoverInternalFailure

and tainted_range = {
  taint_loc : Ast.Loc.t;
  taint_reason : string;
  taint_recovery_kind : recovery_kind;
  taint_confidence_penalty : float;
  taint_allows_semantic : bool;
}

type profile = Interactive | Background | Batch | Debug

type checkpoint_entry = {
  token_index : int;
  token_start_off : int;
  token_start_line : int;
  token_start_col : int;
  checkpoint : Ast.program I.checkpoint;
}

type checkpoint_cache = {
  token_count : int;
  token_hash : string;
  profile : profile;
  checkpoint_count : int;
  landmark_count : int;
  entries : checkpoint_entry list;
  output : output option;
}

type checkpoint_stats = {
  cache_hit : bool;
  checkpoint_count : int;
  checkpoint_reused : bool;
  fallback_reason : string option;
}

type checkpointed_output = {
  output : output;
  checkpoint_cache : checkpoint_cache;
  checkpoint_stats : checkpoint_stats;
}

type token_span = {
  tok : token;
  start_off : int;
  end_off : int;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
  lexeme : string option;
}

let token_span_of_lexing_positions ?lexeme (tok : token)
    (sp : Lexing.position) (ep : Lexing.position) : token_span =
  {
    tok;
    start_off = sp.pos_cnum;
    end_off = ep.pos_cnum;
    start_line = sp.pos_lnum;
    start_col = sp.pos_cnum - sp.pos_bol;
    end_line = ep.pos_lnum;
    end_col = ep.pos_cnum - ep.pos_bol;
    lexeme;
  }

let lexing_position_of_span ~(file : string option) ~(line : int) ~(col : int)
    ~(off : int) : Lexing.position =
  {
    Lexing.pos_fname = Option.value file ~default:"";
    pos_lnum = line;
    pos_bol = off - col;
    pos_cnum = off;
  }

let token_span_start_p ~(file : string option) (s : token_span) :
    Lexing.position =
  lexing_position_of_span ~file ~line:s.start_line ~col:s.start_col
    ~off:s.start_off

let token_span_end_p ~(file : string option) (s : token_span) :
    Lexing.position =
  lexing_position_of_span ~file ~line:s.end_line ~col:s.end_col
    ~off:s.end_off

let loc_of_lex (file : string option) (sp : Lexing.position)
    (ep : Lexing.position) : Ast.Loc.t =
  let mk (p : Lexing.position) : Ast.Loc.pos =
    let col = p.pos_cnum - p.pos_bol in
    { Ast.Loc.line = p.pos_lnum; col; offset = p.pos_cnum }
  in
  let file =
    match file with
    | Some f -> Some f
    | None -> if sp.pos_fname = "" then None else Some sp.pos_fname
  in
  { Ast.Loc.file; start_pos = mk sp; end_pos = mk ep }

let attach_file (file : string option) (loc : Ast.Loc.t) : Ast.Loc.t =
  match (file, loc.Ast.Loc.file) with
  | Some f, None -> { loc with Ast.Loc.file = Some f }
  | _ -> loc

let diag ~sev ~source (loc : Ast.Loc.t) (msg : string) : T.Diagnostic.t =
  Lsp_conv.diagnostic ~severity:sev ~source ~message:msg loc

let diag_error (loc : Ast.Loc.t) (msg : string) : T.Diagnostic.t =
  diag ~sev:T.DiagnosticSeverity.Error ~source:"parse" loc msg

let diag_warn (loc : Ast.Loc.t) (msg : string) : T.Diagnostic.t =
  diag ~sev:T.DiagnosticSeverity.Warning ~source:"parse" loc msg

let recovery_message_prefix = "Recovered from damaged syntax"

let diagnostic_message_text (diag : T.Diagnostic.t) : string =
  match diag.T.Diagnostic.message with
  | `String s -> s
  | `MarkupContent mc -> mc.value

let is_recovery_message (msg : string) : bool =
  let n = String.length recovery_message_prefix in
  String.length msg >= n && String.sub msg 0 n = recovery_message_prefix

let taint ?(kind = RecoverSyncSkip) ?(penalty = 0.2)
    ?(allows_semantic = false) ~(reason : string) (loc : Ast.Loc.t) :
    tainted_range =
  {
    taint_loc = loc;
    taint_reason = reason;
    taint_recovery_kind = kind;
    taint_confidence_penalty = penalty;
    taint_allows_semantic = allows_semantic;
  }

let clamp_confidence x = max 0.0 (min 1.0 x)

let output ?ast ?ast_dump ~(diags : T.Diagnostic.t list)
    ~(recovery_diags : T.Diagnostic.t list)
    ~(tainted_ranges : tainted_range list) ~(parse_health : parse_health)
    ~(parse_confidence : float) () : output =
  {
    ast;
    diags;
    recovery_diags;
    tainted_ranges;
    parse_health;
    parse_confidence = clamp_confidence parse_confidence;
    ast_dump;
  }

module Debug = struct
  let string_of_token (t : token) : string =
    match t with
    | EOF -> "EOF"
    | ID _ -> "ID"
    | FIXED_A _ -> "ID"
    | INTLIT _ -> "INTLIT"
    | FLOATLIT _ -> "FLOATLIT"
    | STRINGLIT _ -> "STRINGLIT"
    | BITLIT _ -> "BITLIT"
    | BAD_CHAR _ -> "BAD_CHAR"
    | BAD_STRING _ -> "BAD_STRING"
    | BAD_COMMENT _ -> "BAD_COMMENT"
    | BAD_DIRECTIVE _ -> "BAD_DIRECTIVE"
    | BAD_LITERAL _ -> "BAD_LITERAL"
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | NULL -> "NULL"
    | START -> "START"
    | TERM -> "TERM"
    | BEGIN -> "BEGIN"
    | END -> "END"
    | DEF -> "DEF"
    | REF -> "REF"
    | PROC -> "PROC"
    | ITEM -> "ITEM"
    | TABLE -> "TABLE"
    | READONLY -> "READONLY"
    | INLINE -> "INLINE"
    | OVERLAY -> "OVERLAY"
    | STATIC -> "STATIC"
    | CONSTANT -> "CONSTANT"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | WHILE -> "WHILE"
    | FOR -> "FOR"
    | BY -> "BY"
    | THEN -> "THEN"
    | CASE -> "CASE"
    | DEFAULT -> "DEFAULT"
    | FALLTHRU -> "FALLTHRU"
    | EXIT -> "EXIT"
    | GOTO -> "GOTO"
    | RETURN -> "RETURN"
    | ABORT -> "ABORT"
    | STOP -> "STOP"
    | PROGRAM -> "PROGRAM"
    | COMPOOL -> "COMPOOL"
    | ICOMPOOL -> "ICOMPOOL"
    | LINKAGE -> "LINKAGE"
    | ILINKAGE -> "ILINKAGE"
    | CODE -> "CODE"
    | ICODE -> "ICODE"
    | DEFINE -> "DEFINE"
    | TYPE -> "TYPE"
    | BLOCK -> "BLOCK"
    | NOT -> "NOT"
    | AND -> "AND"
    | OR -> "OR"
    | XOR -> "XOR"
    | EQV -> "EQV"
    | MOD -> "MOD"
    | POS -> "POS"
    | LPAREN -> "("
    | RPAREN -> ")"
    | COMMA -> ","
    | SEMI -> ";"
    | COLON -> ":"
    | DOT -> "."
    | BANG -> "!"
    | AT -> "@"
    | CONV_L -> "(*"
    | CONV_R -> "*)"
    | EQ -> "="
    | LT -> "<"
    | GT -> ">"
    | LE -> "<="
    | GE -> ">="
    | NE -> "<>"
    | PLUS -> "+"
    | MINUS -> "-"
    | STAR -> "*"
    | SLASH -> "/"
    | POW -> "^"
end

let take n xs =
  let rec go i acc = function
    | [] -> List.rev acc
    | _ when i = 0 -> List.rev acc
    | x :: tl -> go (i - 1) (x :: acc) tl
  in
  go n [] xs

let uniq_sorted (xs : string list) : string list =
  let rec go prev acc = function
    | [] -> List.rev acc
    | x :: tl ->
        if Some x = prev then go prev acc tl else go (Some x) (x :: acc) tl
  in
  go None [] xs

let expected_candidates : token list =
  [
    ID "_";
    INTLIT "0";
    FLOATLIT "0.0";
    STRINGLIT "";
    BITLIT (1, "0", "1B'0'");
    NULL;
    START;
    TERM;
    BEGIN;
    END;
    DEF;
    REF;
    PROC;
    ITEM;
    TABLE;
    STATIC;
    CONSTANT;
    READONLY;
    INLINE;
    OVERLAY;
    PROGRAM;
    COMPOOL;
    ICOMPOOL;
    LINKAGE;
    ILINKAGE;
    CODE;
    ICODE;
    DEFINE;
    TYPE;
    BLOCK;
    POS;
    IF;
    ELSE;
    WHILE;
    FOR;
    BY;
    THEN;
    CASE;
    DEFAULT;
    FALLTHRU;
    RETURN;
    EXIT;
    GOTO;
    ABORT;
    STOP;
    TRUE;
    FALSE;
    NOT;
    AND;
    OR;
    XOR;
    EQV;
    MOD;
    LPAREN;
    RPAREN;
    COMMA;
    SEMI;
    COLON;
    DOT;
    BANG;
    AT;
    CONV_L;
    CONV_R;
    EQ;
    LT;
    GT;
    LE;
    GE;
    NE;
    PLUS;
    MINUS;
    STAR;
    SLASH;
    POW;
    EOF;
  ]

let expected_tokens_hint (chk : 'a I.checkpoint) (pos : Lexing.position) :
    string list =
  expected_candidates
  |> List.filter (fun tok -> I.acceptable chk tok pos)
  |> List.map Debug.string_of_token
  |> List.sort String.compare |> uniq_sorted |> take 12

let constant_storage_warning =
  "CONSTANT items should have static or external allocation."

let parse_entries_to_lsp ~(file : string option)
    (entries : (Ast.Loc.t * string) list) : T.Diagnostic.t list =
  let seen_const_storage = ref false in
  entries
  |> List.filter_map (fun (loc, msg) ->
      if msg = constant_storage_warning then
        if !seen_const_storage then None
        else (
          seen_const_storage := true;
          Some
            (diag_warn (attach_file file loc)
               "CONSTANT item uses automatic allocation; consider \
                STATIC/DEF/REF where required."))
      else Some (diag_warn (attach_file file loc) msg))

let parse_diags_to_lsp ~(file : string option) : T.Diagnostic.t list =
  parse_entries_to_lsp ~file (Parse_diags.take ())

type token_item = token * Lexing.position * Lexing.position * string

let token_lexeme (t : token) : string =
  match t with
  | ID s | INTLIT s | FLOATLIT s | STRINGLIT s -> s
  | BAD_CHAR s | BAD_STRING s | BAD_COMMENT s | BAD_DIRECTIVE s
  | BAD_LITERAL s ->
      s
  | BITLIT (_, _, raw) -> raw
  | EOF -> ""
  | _ -> Debug.string_of_token t

let file_position (file : string option) : Lexing.position =
  { Lexing.dummy_pos with pos_fname = Option.value file ~default:"" }

let is_checkpoint_landmark = function
  | START | TERM | PROC | DEFINE | COMPOOL | ICOMPOOL | LINKAGE | ILINKAGE
  | BEGIN | END -> true
  | _ -> false

let checkpoint_interval_for_profile = function
  | Interactive | Debug -> 32
  | Background -> 96
  | Batch -> 192

type token_metadata = {
  metadata_token_count : int;
  metadata_token_hash : string;
  metadata_landmark_count : int;
}

let token_metadata_of_tokens (tokens : token_span array) : token_metadata =
  let token_count = Array.length tokens in
  let landmark_count = ref 0 in
  let digest = Buffer.create (max 16 (token_count * 8)) in
  Array.iteri
    (fun i (span : token_span) ->
      ignore i;
      if is_checkpoint_landmark span.tok then incr landmark_count;
      Buffer.add_string digest (Debug.string_of_token span.tok);
      Buffer.add_char digest ':';
      Buffer.add_string digest (token_lexeme span.tok);
      Buffer.add_char digest ';')
    tokens;
  {
    metadata_token_count = token_count;
    metadata_token_hash = Digest.to_hex (Digest.string (Buffer.contents digest));
    metadata_landmark_count = !landmark_count;
  }

let parse_stream ~(initial_checkpoint : Ast.program I.checkpoint option)
    ~(on_input_needed : (Ast.program I.checkpoint -> unit) option)
    ~(file : string option) ~(dump_ast : bool) ~(profile : profile)
    ~(start_pos : Lexing.position)
    ~(next_raw : unit -> token_item) : output =
  Parse_diags.clear ();
  let expected_hints, max_errors, resume_fuel, sync_fuel =
    match profile with
    | Interactive -> (true, 12, 1024, 4096)
    | Debug -> (true, 32, 2048, 8192)
    | Background -> (false, 1, 128, 512)
    | Batch -> (false, 1, 64, 256)
  in
  let last_tok : token option ref = ref None in
  let last_sp : Lexing.position ref = ref start_pos in
  let last_ep : Lexing.position ref = ref start_pos in
  let last_lex : string ref = ref "" in
  let prev_tok : token option ref = ref None in
  let last_expected : string list ref = ref [] in
  let pending_tok : token_item option ref = ref None in
  let parse_errors : T.Diagnostic.t list ref = ref [] in
  let parse_taints : tainted_range list ref = ref [] in
  let parse_error_count : int ref = ref 0 in
  let bad_token_error_seen : bool ref = ref false in
  let recovery_attempt_count : int ref = ref 0 in
  let last_error_span : (int * int) option ref = ref None in

  let remember_token (t : token) (sp : Lexing.position)
      (ep : Lexing.position) (lexeme : string) : unit =
    prev_tok := !last_tok;
    last_tok := Some t;
    last_sp := sp;
    last_ep := ep;
    last_lex := lexeme
  in

  let next_token () : token * Lexing.position * Lexing.position =
    let t, sp, ep, lexeme =
      match !pending_tok with
      | Some item ->
          pending_tok := None;
          item
      | None -> next_raw ()
    in
    remember_token t sp ep lexeme;
    (t, sp, ep)
  in

  let mk_error_diag () : T.Diagnostic.t =
    let loc = loc_of_lex file !last_sp !last_ep in
    let expected_hint =
      match !last_expected with
      | [] -> ""
      | xs -> " Expected: " ^ String.concat ", " xs
    in
    match !last_tok with
    | None -> diag_error loc ("Parse error (no token info)." ^ expected_hint)
    | Some EOF ->
        diag_error loc ("Parse error: unexpected end of file." ^ expected_hint)
    | Some t ->
      let tok_s = Debug.string_of_token t in
      let base =
        match (!prev_tok, t) with
        | Some RPAREN, ID name ->
            Printf.sprintf
              "Expected ';' or ',' after the previous call before %S." name
        | _ ->
            if !last_lex = "" then
              Printf.sprintf "Parse error near token %s" tok_s
            else
              Printf.sprintf "Parse error near token %s (lexeme: %S)" tok_s
                !last_lex
      in
      diag_error loc (base ^ expected_hint)
  in

  let add_parse_error () : unit =
    let key = (!last_sp.Lexing.pos_cnum, !last_ep.Lexing.pos_cnum) in
    if !parse_error_count < max_errors && Some key <> !last_error_span then (
      last_error_span := Some key;
      incr parse_error_count;
      (match !last_tok with
      | Some (BAD_CHAR _ | BAD_STRING _ | BAD_COMMENT _ | BAD_DIRECTIVE _
             | BAD_LITERAL _) ->
          bad_token_error_seen := true
      | _ -> ());
      let diag = mk_error_diag () in
      parse_errors := diag :: !parse_errors;
      let loc = loc_of_lex file !last_sp !last_ep in
      parse_taints :=
        taint ~reason:(diagnostic_message_text diag) loc :: !parse_taints)
  in

  let is_sync_token = function
    | SEMI | COMMA | END | TERM | EOF -> true
    | _ -> false
  in

  let rec resume_to_input_or_done (chk : Ast.program I.checkpoint)
      (fuel : int) :
      [ `NeedInput of Ast.program I.checkpoint
      | `Accepted of Ast.program
      | `Rejected ] =
    if fuel <= 0 then `Rejected
    else
      match chk with
      | I.InputNeeded _ -> `NeedInput chk
      | I.Accepted ast -> `Accepted ast
      | I.Rejected -> `Rejected
      | I.Shifting _ | I.AboutToReduce _ | I.HandlingError _ ->
          resume_to_input_or_done (I.resume chk) (fuel - 1)
  in

  let rec skip_to_sync (fuel : int) : bool =
    if fuel <= 0 then false
    else
      let t, _, _, _ as item = next_raw () in
      if is_sync_token t then (
        pending_tok := Some item;
        true)
      else skip_to_sync (fuel - 1)
  in

  let checkpoint =
    match initial_checkpoint with
    | Some checkpoint -> checkpoint
    | None -> Incremental.program start_pos
  in

  let rec drive (chk : Ast.program I.checkpoint) : Ast.program option =
    match chk with
    | I.InputNeeded _env ->
        (match on_input_needed with Some f -> f chk | None -> ());
        last_expected := [];
        let t, sp, ep = next_token () in
        drive (I.offer chk (t, sp, ep))
    | I.Shifting _ | I.AboutToReduce _ -> drive (I.resume chk)
    | I.Accepted ast -> Some ast
    | I.HandlingError _env ->
        if !recovery_attempt_count >= max_errors then None
        else (
        incr recovery_attempt_count;
        let resumed = resume_to_input_or_done chk resume_fuel in
        (last_expected :=
           if expected_hints then
             match resumed with
             | `NeedInput chk' -> expected_tokens_hint chk' !last_ep
             | `Accepted _ | `Rejected -> []
           else []);
        add_parse_error ();
        begin
          match resumed with
          | `Accepted ast -> Some ast
          | `NeedInput chk' -> if skip_to_sync sync_fuel then drive chk' else None
          | `Rejected -> None
        end)
    | I.Rejected -> None
  in

  try
    let ast_opt = drive checkpoint in
    let parse_entries = Parse_diags.take () in
    let extra = parse_entries_to_lsp ~file parse_entries in
    let recovery_diags =
      List.filter
        (fun diag -> is_recovery_message (diagnostic_message_text diag))
        extra
    in
    let syntax_extra =
      List.filter
        (fun diag -> not (is_recovery_message (diagnostic_message_text diag)))
        extra
    in
    let grammar_taints =
      parse_entries
      |> List.filter_map (fun (loc, msg) ->
             if is_recovery_message msg then
               Some
                 (taint ~kind:RecoverGrammarError ~penalty:0.25
                    ~reason:msg (attach_file file loc))
             else None)
    in
    let errs = List.rev !parse_errors in
    let taints = List.rev_append !parse_taints grammar_taints in
    let recovery_count = List.length recovery_diags + !parse_error_count in
    let confidence_for_ast =
      clamp_confidence
        (1.0 -. (0.06 *. float_of_int !parse_error_count)
        -. (0.03 *. float_of_int (List.length recovery_diags))
        -. (0.10
           *. List.fold_left
                (fun acc t -> acc +. t.taint_confidence_penalty)
                0.0 taints))
    in
    match ast_opt with
    | Some ast ->
        let ast_dump = if dump_ast then Some (Ast.Debug.to_string ast) else None in
        let health =
          if recovery_count = 0 then ParseClean else ParseRecovered
        in
        let confidence =
          if recovery_count = 0 then 1.0
          else if !bad_token_error_seen then
            max 0.30 (min 0.49 confidence_for_ast)
          else if recovery_count <= 2 then max 0.80 (min 0.95 confidence_for_ast)
          else max 0.55 (min 0.79 confidence_for_ast)
        in
        output ~ast ~diags:(errs @ syntax_extra) ~recovery_diags
          ~tainted_ranges:taints ~parse_health:health
          ~parse_confidence:confidence ?ast_dump ()
    | None ->
        let errs = match errs with [] -> [ mk_error_diag () ] | xs -> xs in
        let taints =
          if taints <> [] then taints
          else
            let loc = loc_of_lex file !last_sp !last_ep in
            [ taint ~kind:RecoverSkeletonFallback ~penalty:0.55
                ~reason:"Full parse failed; using skeleton facts only." loc ]
        in
        output ~diags:(errs @ syntax_extra) ~recovery_diags
          ~tainted_ranges:taints ~parse_health:ParseSkeletonOnly
          ~parse_confidence:0.45 ()
  with
  | exn ->
      let parse_entries = Parse_diags.take () in
      let extra = parse_entries_to_lsp ~file parse_entries in
      let errs = List.rev !parse_errors in
      let loc = loc_of_lex file !last_ep !last_ep in
      let internal =
        diag ~sev:T.DiagnosticSeverity.Error ~source:"internal" loc
          ("Internal parser failure: " ^ Printexc.to_string exn)
      in
      output ~diags:(errs @ extra @ [ internal ]) ~recovery_diags:[]
        ~tainted_ranges:
          [
            taint ~kind:RecoverInternalFailure ~penalty:1.0
              ~reason:"Internal parser failure." loc;
          ]
        ~parse_health:ParseFailedInternal ~parse_confidence:0.0 ()

let parse_tokens ~(file : string option) ~(dump_ast : bool)
    ~(profile : profile) ~(tokens : token_span array) : output =
  let len = Array.length tokens in
  if len = 0 then
    output ~diags:[] ~recovery_diags:[] ~tainted_ranges:[]
      ~parse_health:ParseLexicalOnly ~parse_confidence:0.25 ()
  else
    let start_pos =
      try token_span_start_p ~file tokens.(0) with _ -> file_position file
    in
    let cursor = ref 0 in
    let last_pos = ref start_pos in
    let next_raw () : token_item =
      if !cursor < len then (
        let span = tokens.(!cursor) in
        incr cursor;
        let t = span.tok in
        let sp = token_span_start_p ~file span in
        let ep = token_span_end_p ~file span in
        last_pos := ep;
        let lexeme = Option.value span.lexeme ~default:(token_lexeme t) in
        (t, sp, ep, lexeme))
      else (EOF, !last_pos, !last_pos, "")
    in
    let dump_ast = dump_ast || match profile with Debug -> true | _ -> false in
    parse_stream ~initial_checkpoint:None ~on_input_needed:None ~file ~dump_ast
      ~profile ~start_pos ~next_raw

let span_start_for_index ~(file : string option) (tokens : token_span array)
    (index : int) : Lexing.position =
  let len = Array.length tokens in
  if len = 0 then file_position file
  else if index < len then token_span_start_p ~file tokens.(index)
  else token_span_end_p ~file tokens.(len - 1)

let entry_for_checkpoint (tokens : token_span array) ~(index : int)
    ~(checkpoint : Ast.program I.checkpoint) : checkpoint_entry =
  let len = Array.length tokens in
  if len = 0 then
    {
      token_index = 0;
      token_start_off = 0;
      token_start_line = 1;
      token_start_col = 0;
      checkpoint;
    }
  else if index < len then
    let span = tokens.(index) in
    {
      token_index = index;
      token_start_off = span.start_off;
      token_start_line = span.start_line;
      token_start_col = span.start_col;
      checkpoint;
    }
  else
    let span = tokens.(len - 1) in
    {
      token_index = index;
      token_start_off = span.end_off;
      token_start_line = span.end_line;
      token_start_col = span.end_col;
      checkpoint;
    }

let should_record_checkpoint ~(profile : profile) ~(tokens : token_span array)
    ~(index : int) : bool =
  let interval = checkpoint_interval_for_profile profile in
  index = 0
  || (interval > 0 && index mod interval = 0)
  ||
  let len = Array.length tokens in
  index < len && is_checkpoint_landmark tokens.(index).tok

let output_with_requested_dump ~(dump_ast : bool) ~(profile : profile)
    (output : output) : output =
  let want_dump = dump_ast || match profile with Debug -> true | _ -> false in
  if not want_dump then output
  else
    match (output.ast, output.ast_dump) with
    | Some ast, None ->
        { output with ast_dump = Some (Ast.Debug.to_string ast) }
    | _ -> output

let diag_ends_before_checkpoint (entry : checkpoint_entry)
    (diag : T.Diagnostic.t) : bool =
  let line = max 0 (entry.token_start_line - 1) in
  let col = max 0 entry.token_start_col in
  let p = diag.range.end_ in
  p.line < line || (p.line = line && p.character <= col)

let prefix_diags_for_checkpoint (output : output) (entry : checkpoint_entry) :
    T.Diagnostic.t list =
  List.filter (diag_ends_before_checkpoint entry) output.diags

let parse_tokens_with_cache
    ~(initial_checkpoint : Ast.program I.checkpoint option)
    ~(prefix_entries : checkpoint_entry list)
    ~(prefix_diags : T.Diagnostic.t list) ~(metadata : token_metadata)
    ~(file : string option) ~(dump_ast : bool) ~(profile : profile)
    ~(tokens : token_span array) ~(start_index : int) : checkpointed_output =
  let len = Array.length tokens in
  let start_pos = span_start_for_index ~file tokens start_index in
  let cursor = ref (max 0 (min len start_index)) in
  let last_pos = ref start_pos in
  let next_raw () : token_item =
    if !cursor < len then (
      let span = tokens.(!cursor) in
      incr cursor;
      let t = span.tok in
      let sp = token_span_start_p ~file span in
      let ep = token_span_end_p ~file span in
      last_pos := ep;
      let lexeme = Option.value span.lexeme ~default:(token_lexeme t) in
      (t, sp, ep, lexeme))
    else (EOF, !last_pos, !last_pos, "")
  in
  let entries_rev = ref (List.rev prefix_entries) in
  let recorded = Hashtbl.create 128 in
  List.iter
    (fun (entry : checkpoint_entry) ->
      Hashtbl.replace recorded entry.token_index true)
    prefix_entries;
  let on_input_needed checkpoint =
    let index = !cursor in
    if
      should_record_checkpoint ~profile ~tokens ~index
      && not (Hashtbl.mem recorded index)
    then (
      Hashtbl.replace recorded index true;
      entries_rev :=
        entry_for_checkpoint tokens ~index ~checkpoint :: !entries_rev)
  in
  let dump_ast = dump_ast || match profile with Debug -> true | _ -> false in
  let output =
    parse_stream ~initial_checkpoint ~on_input_needed:(Some on_input_needed)
      ~file ~dump_ast ~profile ~start_pos ~next_raw
  in
  let output = { output with diags = prefix_diags @ output.diags } in
  let entries = List.rev !entries_rev in
  let checkpoint_cache =
    {
      token_count = metadata.metadata_token_count;
      token_hash = metadata.metadata_token_hash;
      profile;
      checkpoint_count = List.length entries;
      landmark_count = metadata.metadata_landmark_count;
      entries;
      output = Some output;
    }
  in
  {
    output;
    checkpoint_cache;
    checkpoint_stats =
      {
        cache_hit = false;
        checkpoint_count = checkpoint_cache.checkpoint_count;
        checkpoint_reused =
          (match initial_checkpoint with Some _ -> true | None -> false);
        fallback_reason = None;
      };
  }

let full_parse_checkpointed ~(metadata : token_metadata) ~(file : string option)
    ~(dump_ast : bool) ~(profile : profile) ~(tokens : token_span array) :
    checkpointed_output =
  if Array.length tokens = 0 then
    let output =
      output ~diags:[] ~recovery_diags:[] ~tainted_ranges:[]
        ~parse_health:ParseLexicalOnly ~parse_confidence:0.25 ()
    in
    let checkpoint_cache =
      {
        token_count = metadata.metadata_token_count;
        token_hash = metadata.metadata_token_hash;
        profile;
        checkpoint_count = 0;
        landmark_count = metadata.metadata_landmark_count;
        entries = [];
        output = Some output;
      }
    in
    {
      output;
      checkpoint_cache;
      checkpoint_stats =
        {
          cache_hit = false;
          checkpoint_count = 0;
          checkpoint_reused = false;
          fallback_reason = None;
        };
    }
  else
    parse_tokens_with_cache ~prefix_entries:[] ~prefix_diags:[] ~metadata ~file
      ~dump_ast ~profile ~tokens ~start_index:0 ~initial_checkpoint:None

let reusable_checkpoint (previous : checkpoint_cache) ~(dirty_token : int) :
    checkpoint_entry option =
  let rec loop best = function
    | [] -> best
    | (entry : checkpoint_entry) :: rest ->
        let best =
          if entry.token_index > 0 && entry.token_index <= dirty_token then
            match best with
            | None -> Some entry
            | Some old when entry.token_index > old.token_index -> Some entry
            | Some _ -> best
          else best
        in
        loop best rest
  in
  loop None previous.entries

let parse_tokens_checkpointed ?previous ?dirty_token ~(file : string option)
    ~(dump_ast : bool) ~(profile : profile) ~(tokens : token_span array) :
    unit -> checkpointed_output =
 fun () ->
  let metadata = token_metadata_of_tokens tokens in
  match previous with
  | Some prev
    when prev.profile = profile && prev.token_hash = metadata.metadata_token_hash
    -> (
      match prev.output with
      | Some output ->
          let output = output_with_requested_dump ~dump_ast ~profile output in
          let checkpoint_cache = { prev with output = Some output } in
          {
            output;
            checkpoint_cache;
            checkpoint_stats =
              {
                cache_hit = true;
                checkpoint_count = checkpoint_cache.checkpoint_count;
                checkpoint_reused = false;
                fallback_reason = None;
              };
          }
      | None ->
          let parsed =
            full_parse_checkpointed ~metadata ~file ~dump_ast ~profile ~tokens
          in
          {
            parsed with
            checkpoint_stats =
              {
                parsed.checkpoint_stats with
                fallback_reason = Some "missing_cached_output";
              };
          })
  | Some prev -> (
      match dirty_token with
      | Some dirty_token when prev.profile = profile -> (
          match (prev.output, reusable_checkpoint prev ~dirty_token) with
          | Some previous_output, Some entry ->
              let prefix_entries =
                List.filter
                  (fun (e : checkpoint_entry) ->
                    e.token_index < entry.token_index)
                  prev.entries
              in
              let prefix_diags =
                prefix_diags_for_checkpoint previous_output entry
              in
              parse_tokens_with_cache
                ~initial_checkpoint:(Some entry.checkpoint) ~prefix_entries
                ~prefix_diags ~metadata ~file ~dump_ast ~profile ~tokens
                ~start_index:entry.token_index
          | _ ->
              let parsed =
                full_parse_checkpointed ~metadata ~file ~dump_ast ~profile
                  ~tokens
              in
              {
                parsed with
                checkpoint_stats =
                  {
                    parsed.checkpoint_stats with
                    fallback_reason = Some "no_reusable_checkpoint";
                  };
              })
      | Some _ ->
          let parsed =
            full_parse_checkpointed ~metadata ~file ~dump_ast ~profile ~tokens
          in
          {
            parsed with
            checkpoint_stats =
              {
                parsed.checkpoint_stats with
                fallback_reason = Some "profile_changed";
              };
          }
      | None ->
          let parsed =
            full_parse_checkpointed ~metadata ~file ~dump_ast ~profile ~tokens
          in
          {
            parsed with
            checkpoint_stats =
              {
                parsed.checkpoint_stats with
                fallback_reason = Some "missing_dirty_token";
              };
          })
  | None ->
      full_parse_checkpointed ~metadata ~file ~dump_ast ~profile ~tokens
