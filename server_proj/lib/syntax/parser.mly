%{
  open Ast

  let loc sp ep = Ast.Loc.of_lexing_positions_no_file sp ep
  let n sp ep v = Ast.node ~loc:(loc sp ep) v
  let nid sp ep s = Ast.node ~loc:(loc sp ep) s

  let mk_block sp ep (ss : Ast.stmt Ast.node list) : Ast.stmt Ast.node =
    n sp ep (Ast.SBlock ss)

  let wrap_labels (labels : Ast.ident list) (s : Ast.stmt Ast.node) : Ast.stmt Ast.node =
    List.fold_right
      (fun (lab : Ast.ident) acc ->
        Ast.node ~loc:acc.loc (Ast.SLabel { label = lab; body = acc }))
      labels
      s

  let bad_stmt sp ep : Ast.stmt Ast.node =
    mk_block sp ep []

  (* A tiny type pretty-printer so conversion keeps useful info in the AST. *)
  let rec type_to_string (t : Ast.type_expr Ast.node) : string =
    match t.v with
    | Ast.TName id -> id.v
    | Ast.TArray { elem; dims } ->
        Printf.sprintf "%s[%d]" (type_to_string elem) (List.length dims)
    | Ast.TRecord _ -> "RECORD"
    | _ -> "TYPE"

  let mk_conv sp ep (ty : Ast.type_expr Ast.node) (rhs : Ast.expr Ast.node) : Ast.expr Ast.node =
    let ty_s = type_to_string ty in
    let callee = nid sp ep "__conv__" in
    let ty_arg = n sp ep (Ast.ELit (Ast.LString ty_s)) in
    n sp ep (Ast.ECall { callee; args = [ty_arg; rhs] })

  let mk_preset sp ep (base_num : string) (items : Ast.expr Ast.node list) : Ast.expr Ast.node =
    let callee = nid sp ep "__preset__" in
    let base = n sp ep (Ast.ELit (Ast.LInt base_num)) in
    n sp ep (Ast.ECall { callee; args = base :: items })

  let proc_use_from_flags (seen_rec:bool) (seen_rent:bool) : Ast.proc_use =
    if seen_rec then Ast.UseRec else if seen_rent then Ast.UseRent else Ast.UseNormal

  let empty_proc_header_info =
    (false, false, None)

  let merge_proc_header_info (a_rec, a_rent, a_ret) (b_rec, b_rent, b_ret) =
    let ret =
      match a_ret, b_ret with
      | Some x, _ -> Some x
      | None, Some y -> Some y
      | None, None -> None
    in
    (a_rec || b_rec, a_rent || b_rent, ret)

  let for_step_stmt sp ep lhs (by_expr, then_expr) =
    match by_expr, then_expr with
    | Some inc, _ ->
        let rhs = n sp ep (Ast.EBinop { op = Ast.BAdd; lhs; rhs = inc }) in
        Some (n sp ep (Ast.SAssign { lhs; rhs }))
    | None, Some nextv ->
        Some (n sp ep (Ast.SAssign { lhs; rhs = nextv }))
    | None, None ->
        None
%}

/* identifiers + literals */
%token <string> ID
%token <string> INTLIT
%token <string> FLOATLIT
%token <string> STRINGLIT
%token TRUE FALSE

/* keywords */
%token START TERM BEGIN END
%token DEF REF PROC
%token ITEM TABLE
%token IF ELSE WHILE FOR BY THEN
%token CASE DEFAULT
%token EXIT GOTO RETURN ABORT STOP
%token NOT AND OR XOR EQV MOD

/* “header / meta” keywords (safe even if used as IDs) */
%token PROGRAM COMPOOL ICOMPOOL DEFINE TYPE BLOCK

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

%%

/* =========================================================
   Program structure (avoid OCaml keyword `module`)
   ========================================================= */

program:
  | ms=jmodules EOF { List.concat ms }
  ;

jmodules:
  | /* empty */ { [] }
  | m=jmodule ms=jmodules { m :: ms }
  ;

jmodule:
  | START hs=header_items body=module_items_opt TERM { hs @ body }
  | body=module_items TERM                       { body }
  | body=module_items                            { body }
  ;

/* Accept PROGRAM/COMPOOL lines in the header area. */
header_items:
  | /* empty */ { [] }
  | hs=header_items h=header_item { hs @ h }
  ;

header_item:
  | d=directive_decl { [Ast.TopDecl d] }
  | PROGRAM nm=ident _t=terminator_opt
      {
        let d = n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "PROGRAM"; args = [nm] }) in
        [Ast.TopDecl d]
      }
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
  | s=statement      { [Ast.TopStmt s] }
  | error _t=terminator
      { [] }
  ;

/* =========================================================
   Declarations
   ========================================================= */

decl_item:
  | d=directive_decl { [d] }
  | ds=group_decl    { ds }
  | ds=data_decl     { ds }
  | ds=proc_decl     { ds }
  | ds=define_decl   { ds }
  | ds=type_decl     { ds }
  | ds=block_decl    { ds }
  ;

/* Directives: !NAME [args] ;  (and allow COMPOOL/ICOMPOOL without ! too) */
directive_decl:
  | BANG COMPOOL LPAREN arg=compool_arg RPAREN rest=directive_args_opt _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "COMPOOL"; args = arg :: rest }) }
  | BANG ICOMPOOL LPAREN arg=compool_arg RPAREN rest=directive_args_opt _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "ICOMPOOL"; args = arg :: rest }) }
  | BANG COMPOOL arg=compool_arg rest=directive_args_opt _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "COMPOOL"; args = arg :: rest }) }
  | BANG ICOMPOOL arg=compool_arg rest=directive_args_opt _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "ICOMPOOL"; args = arg :: rest }) }
  | BANG name=directive_name args=directive_args_opt _t=terminator
      { n $startpos $endpos (Ast.DDirective { name; args }) }
  | COMPOOL arg=compool_arg rest=directive_args_opt _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "COMPOOL"; args = arg :: rest }) }
  | ICOMPOOL arg=compool_arg rest=directive_args_opt _t=terminator_opt
      { n $startpos $endpos (Ast.DDirective { name = nid $startpos $endpos "ICOMPOOL"; args = arg :: rest }) }
  ;

compool_arg:
  | nm=ident { nm }
  | s=STRINGLIT { nid $startpos $endpos s }
  ;

directive_name:
  | nm=ident { nm }
  | PROGRAM  { nid $startpos $endpos "PROGRAM" }
  | DEFINE   { nid $startpos $endpos "DEFINE" }
  | TYPE     { nid $startpos $endpos "TYPE" }
  | BLOCK    { nid $startpos $endpos "BLOCK" }
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
  | id=ID       { nid $startpos $endpos id }
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
          let xs =
            match list_opt with
            | None -> xs
            | Some lo -> xs @ [lo]
          in
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
  | x=ident { [x] }
  | x=ident COMMA xs=define_formals { x :: xs }
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
        [n $startpos $endpos (Ast.DType { name = nm; defn = ty })]
      }
  | TYPE nm=ident ITEM ty=type_spec _t=terminator
      {
        [n $startpos $endpos (Ast.DType { name = nm; defn = ty })]
      }
  | TYPE nm=ident TABLE _base=ident _sizes=type_sizes_opt _t=terminator BEGIN fs=field_decl_list END
      {
        let defn = n $startpos $endpos (Ast.TRecord fs) in
        [n $startpos $endpos (Ast.DType { name = nm; defn })]
      }
  ;

/* BLOCK name ; BEGIN ... END  (encoded as: BLOCK directive + a proc-like body) */
block_decl:
  | mod_=modifier_opt BLOCK nm=ident _t=terminator body=proc_body_opt
      {
        let bdir = n $startpos $endpos
          (Ast.DDirective { name = nid $startpos $endpos "BLOCK"; args = [nm] })
        in
        (* represent as a proc for now so you keep scoped locals+body in the AST *)
        let locals, body_stmt =
          match body with
          | None -> ([], mk_block $startpos $endpos [])
          | Some (ds, st) -> (ds, st)
        in
        let proc =
          n $startpos $endpos
            { Ast.name = nm; params = []; returns = None; use_attr = Ast.UseNormal; locals; body = body_stmt }
        in
        let dproc = n $startpos $endpos (Ast.DProc proc) in
        match mod_ with
        | None -> [bdir; dproc]
        | Some m ->
            let md =
              n $startpos(mod_) $endpos(mod_)
                (Ast.DDirective {
                  name = nid $startpos(mod_) $endpos(mod_) m;
                  args = [nid $startpos(nm) $endpos(nm) nm.v];
                })
            in
            [md; bdir; dproc]
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
        md :: ds
      }
  ;

/* ITEM name type [ - init ] ; */
data_decl:
  | mod_=modifier_opt ITEM nm=ident ty=type_spec init=item_init_opt _attrs=item_attrs_opt _t=terminator
      {
        let storage =
          match mod_ with
          | None -> Ast.Automatic
          | Some _ -> Ast.External
        in
        let var =
          n $startpos $endpos (Ast.DVar { name = nm; dtype = ty; init; storage })
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

  /* TABLE name(dims) [ - preset ] [ , BEGIN fielddecls END ] ; [BEGIN fielddecls END] */
  | mod_=modifier_opt TABLE nm=ident dims=table_dims elem_ty_opt=table_elem_type_opt preset=table_preset_opt recopt_before=record_opt _t=terminator recopt_after=table_record_after_term_opt
      {
        let storage =
          match mod_ with
          | None -> Ast.Automatic
          | Some _ -> Ast.External
        in
        let elem_ty =
          match recopt_after with
          | Some fields ->
              n $startpos $endpos (Ast.TRecord fields)
          | None ->
              (match recopt_before with
               | Some fields ->
                   n $startpos $endpos (Ast.TRecord fields)
               | None ->
                   (match elem_ty_opt with
                    | Some ty -> ty
                    | None -> n $startpos $endpos (Ast.TName (nid $startpos $endpos "__table_elem__"))))
        in
        let ty = n $startpos $endpos (Ast.TArray { elem = elem_ty; dims }) in

        (* keep preset as init placeholder if you want; otherwise ignore *)
        let init = preset in

        let var =
          n $startpos $endpos (Ast.DVar { name = nm; dtype = ty; init; storage })
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

item_init_opt:
  | /* empty */ { None }
  | MINUS e=expr { Some e }
  | EQ e=expr { Some e }
  ;

item_attrs_opt:
  | /* empty */ { () }
  | _xs=item_attrs { () }
  ;

item_attrs:
  | _x=item_attr { () }
  | _x=item_attr _xs=item_attrs { () }
  ;

item_attr:
  | _id=ident { () }
  | _id=ident _p=attr_paren_payload { () }
  | _p=attr_paren_payload { () }
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

/* Table preset like: - 1000(,"XXXXX",O)  */
table_preset_opt:
  | /* empty */ { None }
  | MINUS p=preset_expr { Some p }
  ;

preset_expr:
  | base=INTLIT LPAREN items=preset_items_opt RPAREN
      { mk_preset $startpos $endpos base items }
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
  | /* empty (hole) */ { n $startpos $endpos (Ast.ELit (Ast.LString "")) }
  | s=STRINGLIT        { n $startpos $endpos (Ast.ELit (Ast.LString s)) }
  | i=INTLIT           { n $startpos $endpos (Ast.ELit (Ast.LInt i)) }
  | id=ident           { n $startpos $endpos (Ast.EName id) }
  ;

table_dims:
  | LPAREN ds=dim_list_opt RPAREN { ds }
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
        let callee = nid $startpos $endpos "__range__" in
        n $startpos $endpos (Ast.ECall { callee; args = [lo; hi] })
      }
  | lo=expr MINUS hi=expr
      {
        let callee = nid $startpos $endpos "__range__" in
        n $startpos $endpos (Ast.ECall { callee; args = [lo; hi] })
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
  | xs=rev_field_decl_list { List.rev xs }
  ;

rev_field_decl_list:
  | f=field_decl { [f] }
  | xs=rev_field_decl_list f=field_decl { f :: xs }
  ;

field_decl:
  | ITEM nm=ident ty=type_spec _init=item_init_opt _attrs=item_attrs_opt _t=terminator
      { n $startpos $endpos { Ast.fname = nm; ftype = ty } }
  ;

type_spec:
  | base=ident sizes=type_sizes_opt
      {
        match sizes with
        | [] -> n $startpos $endpos (Ast.TName base)
        | dims ->
            let elem = n $startpos(base) $endpos(base) (Ast.TName base) in
            n $startpos $endpos (Ast.TArray { elem; dims })
      }
  ;

type_sizes_opt:
  | /* empty */ { [] }
  | s=type_size rest=type_sizes_opt { s :: rest }
  ;

type_size:
  | i=INTLIT
      { n $startpos $endpos (Ast.ELit (Ast.LInt i)) }
  | f=FLOATLIT
      { n $startpos $endpos (Ast.ELit (Ast.LFloat f)) }
  | id=ident
      { n $startpos $endpos (Ast.EName id) }
  ;

proc_decl:
  | mod_=modifier_opt PROC nm=ident pre=proc_header_tail_opt formals=formals_opt post=proc_header_tail_opt _t=terminator body=proc_body_opt
      {
        let seen_rec, seen_rent, ret =
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
            { Ast.name = nm; params; returns = ret; use_attr; locals; body = body_stmt }
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

proc_header_tail_opt:
  | /* empty */ { empty_proc_header_info }
  | xs=proc_header_tail { xs }
  ;

proc_header_tail:
  | x=proc_header_atom { x }
  | x=proc_header_atom xs=proc_header_tail { merge_proc_header_info x xs }
  ;

proc_header_atom:
  | ty=type_spec
      {
        match ty.v with
        | Ast.TName id ->
            let k = String.uppercase_ascii id.v in
            if k = "REC" || k = "RECURSIVE" then
              (true, false, None)
            else if k = "RENT" || k = "REENTRANT" then
              (false, true, None)
            else
              (false, false, Some ty)
        | _ ->
            (false, false, Some ty)
      }
  | _i=INTLIT { empty_proc_header_info }
  | _f=FLOATLIT { empty_proc_header_info }
  | _s=STRINGLIT { empty_proc_header_info }
  ;

formals_opt:
  | /* empty */ { [] }
  | LPAREN ps=formal_param_groups_opt RPAREN { ps }
  ;

formal_param_groups_opt:
  | /* empty */ { [] }
  | ps=formal_param_groups { ps }
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
  ;

decl_section:
  | xs=rev_decl_section { List.concat (List.rev xs) }
  ;

rev_decl_section:
  | /* empty */ { [] }
  | xs=rev_decl_section ds=data_decl { ds :: xs }
  | xs=rev_decl_section ds=proc_decl { ds :: xs }
  | xs=rev_decl_section d=directive_decl { [d] :: xs }
  | xs=rev_decl_section ds=define_decl { ds :: xs }
  | xs=rev_decl_section ds=type_decl   { ds :: xs }
  | xs=rev_decl_section ds=block_decl  { ds :: xs }
  | xs=rev_decl_section error _t=terminator
      { [] :: xs }
  ;

/* =========================================================
   Statements + Recovery (sync on terminator)
   ========================================================= */

statement:
  | s=compound_stmt { s }
  | labs=labels_opt s=simple_or_control_stmt { wrap_labels labs s }
  | error _t=terminator { bad_stmt $startpos $endpos }
  ;

labels_opt:
  | /* empty */ { [] }
  | labs=labels_opt l=label { labs @ [l] }
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
  | s=simple_stmt _t=terminator_opt { s }
  ;

terminator:
  | SEMI { () }
  | COMMA { () }
  ;

terminator_opt:
  | /* empty */ { () }
  | _t=terminator { () }
  ;

simple_stmt:
  | s=assign_stmt { s }
  | s=goto_stmt   { s }
  | s=return_stmt { s }
  | s=call_stmt   { s }
  | s=exit_stmt   { s }
  | s=abort_stmt  { s }
  | s=stop_stmt   { s }
  ;

assign_stmt:
  | lhses=lvalue_list MINUS rhs=expr
      {
        match lhses with
        | [lhs] -> n $startpos $endpos (Ast.SAssign { lhs; rhs })
        | _ ->
            mk_block $startpos $endpos
              (List.map (fun lhs ->
                 Ast.node ~loc:(loc $startpos $endpos) (Ast.SAssign { lhs; rhs })
               ) lhses)
      }
  | lhses=lvalue_list EQ rhs=expr
      {
        match lhses with
        | [lhs] -> n $startpos $endpos (Ast.SAssign { lhs; rhs })
        | _ ->
            mk_block $startpos $endpos
              (List.map (fun lhs ->
                 Ast.node ~loc:(loc $startpos $endpos) (Ast.SAssign { lhs; rhs })
               ) lhses)
      }
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
  | EXIT { n $startpos $endpos (Ast.SCallStmt { callee = nid $startpos $endpos "EXIT"; args = [] }) }
  ;

abort_stmt:
  | ABORT { n $startpos $endpos (Ast.SCallStmt { callee = nid $startpos $endpos "ABORT"; args = [] }) }
  ;

stop_stmt:
  | STOP { n $startpos $endpos (Ast.SCallStmt { callee = nid $startpos $endpos "STOP"; args = [] }) }
  ;

call_stmt:
  | callee=ident args=actuals_opt
      { n $startpos $endpos (Ast.SCallStmt { callee; args }) }
  ;

actuals_opt:
  | /* empty */ { [] }
  | LPAREN es=expr_list_opt RPAREN { es }
  ;

expr_list_opt:
  | /* empty */ { [] }
  | es=expr_list { es }
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
  | CASE _scrut=expr SEMI BEGIN opts=case_options END
      {
        (* placeholder: keep bodies so you at least get an AST *)
        mk_block $startpos $endpos (List.map snd opts)
      }
  ;

case_options:
  | /* empty */ { [] }
  | o=case_option os=case_options { o :: os }
  ;

case_option:
  | LPAREN _idxs=case_index_list RPAREN _sep=case_sep body=simple_or_control_stmt
      { ([], body) }
  | error _t=terminator
      { ([], bad_stmt $startpos $endpos) }
  ;

case_sep:
  | COLON { () }
  | BANG  { () }
  ;

case_index_list:
  | i=case_index { [i] }
  | i=case_index COMMA is=case_index_list { i :: is }
  ;

case_index:
  | DEFAULT { n $startpos $endpos (Ast.EName (nid $startpos $endpos "DEFAULT")) }
  | e=expr  { e }
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
  ;

literal:
  | i=INTLIT    { Ast.LInt i }
  | f=FLOATLIT  { Ast.LFloat f }
  | s=STRINGLIT { Ast.LString s }
  | TRUE        { Ast.LBool true }
  | FALSE       { Ast.LBool false }
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
  | base=postfix_atom LPAREN args=expr_list_opt RPAREN
      {
        match base.v with
        | Ast.EName callee -> n $startpos $endpos (Ast.ECall { callee; args })
        | _ -> n $startpos $endpos (Ast.EIndex { base; index = args })
      }
  ;

postfix:
  | p=postfix_atom { p }
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
      { n $startpos $endpos (Ast.ECall { callee = nid $startpos $endpos "__pow__"; args = [lhs; rhs] }) }

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
  | lhs=expr EQV rhs=expr { n $startpos $endpos (Ast.EBinop { op = Ast.BBitXor; lhs; rhs }) }
  ;
