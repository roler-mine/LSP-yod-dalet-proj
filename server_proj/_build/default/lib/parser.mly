/* lib/parser.mly */

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
%}


/* ---- Tokens ---- */

/* identifiers + literals */
%token <string> ID
%token <string> INTLIT
%token <string> FLOATLIT
%token <string> STRINGLIT
%token <char>   CHARLIT
%token TRUE FALSE

/* keywords */
%token START TERM BEGIN END
%token DEF REF
%token PROC
%token ITEM TABLE
%token IF ELSE
%token WHILE FOR BY
%token CASE DEFAULT
%token EXIT GOTO RETURN ABORT STOP

/* operators */
%token PLUS MINUS STAR SLASH POW MOD
%token NOT AND OR XOR EQV
%token EQ NE LT LE GT GE

/* punctuation */
%token LPAREN RPAREN
%token COMMA SEMI COLON DOT BANG
%token AT
%token EOF

/* ---- Precedence ---- */
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
   Top level
   ========================================================= */

program:
  | START items=module_items TERM EOF { items }
  | items=module_items EOF            { items }
  | START items=module_items EOF      { items }
  ;

module_items:
  | xs=rev_module_items { List.concat (List.rev xs) }
  ;

rev_module_items:
  | /* empty */ { [] }
  | xs=rev_module_items x=module_item { x :: xs }
  ;

module_item:
  | ds=decl_item     { List.map (fun d -> Ast.TopDecl d) ds }
  | s=statement      { [Ast.TopStmt s] }
  ;

/* =========================================================
   Declarations
   ========================================================= */

decl_item:
  | d=directive_decl { [d] }
  | ds=data_decl     { ds }
  | ds=proc_decl     { ds }
  ;

modifier_opt:
  | /* empty */ { None }
  | DEF         { Some "DEF" }
  | REF         { Some "REF" }
  ;

/* ---------- directives ----------
   permissive: ID [args] terminator
*/
directive_decl:
  | name=ident args=directive_args_opt _t=terminator
      {
        let d = Ast.DDirective { name; args } in
        n $startpos $endpos d
      }
  ;

directive_args_opt:
  | /* empty */ { [] }
  | args=directive_args { args }
  ;

directive_args:
  | a=directive_arg { [a] }
  | a=directive_arg COMMA rest=directive_args { a :: rest }
  ;

directive_arg:
  | s=STRINGLIT { nid $startpos $endpos s }
  | i=INTLIT    { nid $startpos $endpos i }
  | f=FLOATLIT  { nid $startpos $endpos f }
  | id=ID       { nid $startpos $endpos id }
  ;

/* ---------- data declarations ---------- */

/* ITEM name type [init] terminator */
data_decl:
  | mod_=modifier_opt ITEM nm=ident ty=type_spec init=init_opt _t=terminator
      {
        let storage =
          match mod_ with
          | None -> Ast.Automatic
          | Some _ -> Ast.External
        in
        let var =
          n $startpos $endpos
            (Ast.DVar { name = nm; dtype = ty; init; storage })
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

  /* TABLE name(dims) [ , BEGIN fielddecls END ] terminator */
  | mod_=modifier_opt TABLE nm=ident dims=table_dims recopt=record_opt _t=terminator
      {
        let storage =
          match mod_ with
          | None -> Ast.Automatic
          | Some _ -> Ast.External
        in
        let elem_ty =
          match recopt with
          | None ->
              n $startpos $endpos (Ast.TName (nid $startpos $endpos "__table_elem__"))
          | Some fields ->
              n $startpos $endpos (Ast.TRecord fields)
        in
        let ty = n $startpos $endpos (Ast.TArray { elem = elem_ty; dims }) in
        let var = n $startpos $endpos (Ast.DVar { name = nm; dtype = ty; init = None; storage }) in
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

init_opt:
  | /* empty */ { None }
  | e=expr      { Some e }
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

field_decl_list:
  | /* empty */ { [] }
  | xs=rev_field_decl_list { List.rev xs }
  ;

rev_field_decl_list:
  | f=field_decl { [f] }
  | xs=rev_field_decl_list f=field_decl { f :: xs }
  ;

field_decl:
  | ITEM nm=ident ty=type_spec _init=init_opt _t=terminator
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
  | s=expr rest=type_sizes_opt { s :: rest }
  ;

/* ---------- procedure declarations ---------- */

proc_decl:
  | mod_=modifier_opt PROC nm=ident formals=formals_opt _t=terminator body=proc_body_opt
      {
        let params = formals in
        let locals, body_stmt =
          match body with
          | None -> ([], mk_block $startpos $endpos [])
          | Some (ds, st) -> (ds, st)
        in
        let proc =
          n $startpos $endpos
            { Ast.name = nm; params; returns = None; locals; body = body_stmt }
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
        let ins_ps = List.map (mkp Ast.In) ins in
        let outs_ps = List.map (mkp Ast.Out) outs in
        ins_ps @ outs_ps
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
      {
        let body = mk_block $startpos $endpos ss in
        Some (ds, body)
      }
  ;

decl_section:
  | xs=rev_decl_section { List.concat (List.rev xs) }
  ;

rev_decl_section:
  | /* empty */ { [] }
  | xs=rev_decl_section ds=data_decl { ds :: xs }
  | xs=rev_decl_section d=directive_decl { [d] :: xs }
  ;

/* =========================================================
   Statements
   ========================================================= */

statement:
  | s=compound_stmt { s }
  | labs=labels_opt s=simple_or_control_stmt { wrap_labels labs s }
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
  ;

simple_or_control_stmt:
  | s=if_stmt       { s }
  | s=while_stmt    { s }
  | s=for_stmt      { s }
  | s=case_stmt     { s }
  | s=simple_stmt _t=terminator { s }
  ;

terminator:
  | SEMI { () }
  | COMMA { () }
  ;

/* ---- simple statements ---- */

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
  | lhses=lvalue_list EQ rhs=expr
      {
        match lhses with
        | [lhs] -> n $startpos $endpos (Ast.SAssign { lhs; rhs })
        | _ ->
            let assigns =
              List.map (fun lhs -> Ast.node ~loc:(loc $startpos $endpos) (Ast.SAssign { lhs; rhs })) lhses
            in
            mk_block $startpos $endpos assigns
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
  | EXIT
      {
        let callee = nid $startpos $endpos "EXIT" in
        n $startpos $endpos (Ast.SCallStmt { callee; args = [] })
      }
  ;

abort_stmt:
  | ABORT
      {
        let callee = nid $startpos $endpos "ABORT" in
        n $startpos $endpos (Ast.SCallStmt { callee; args = [] })
      }
  ;

stop_stmt:
  | STOP
      {
        let callee = nid $startpos $endpos "STOP" in
        n $startpos $endpos (Ast.SCallStmt { callee; args = [] })
      }
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

/* ---- control statements ---- */

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
  | FOR lc=ident COLON initv=expr by=by_opt wh=while_opt SEMI body=statement
      {
        let lhs = n $startpos(lc) $endpos(lc) (Ast.EName lc) in
        let init_stmt = n $startpos $endpos (Ast.SAssign { lhs; rhs = initv }) in

        let step_stmt =
          match by with
          | None -> None
          | Some inc ->
              let rhs =
                n $startpos $endpos (Ast.EBinop { op = Ast.BAdd; lhs; rhs = inc })
              in
              Some (n $startpos $endpos (Ast.SAssign { lhs; rhs }))
        in

        n $startpos $endpos (Ast.SFor { init = Some init_stmt; cond = wh; step = step_stmt; body })
      }
  ;

by_opt:
  | /* empty */ { None }
  | BY e=expr   { Some e }
  ;

while_opt:
  | /* empty */ { None }
  | WHILE e=expr { Some e }
  ;

case_stmt:
  | CASE _scrut=expr SEMI BEGIN opts=case_options END
      { 
        let bodies = List.map snd opts in
        mk_block $startpos $endpos bodies
      }
;

case_options:
  | /* empty */ { [] }
  | o=case_option os=case_options { o :: os }
  ;

case_option:
  | LPAREN idxs=case_index_list RPAREN _sep=case_sep body=simple_or_control_stmt
      { (idxs, body) }
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

/* =========================================================
   Expressions
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
  | c=CHARLIT   { Ast.LChar c }
  | TRUE        { Ast.LBool true }
  | FALSE       { Ast.LBool false }
  ;

primary:
  | id=ident { n $startpos $endpos (Ast.EName id) }
  | lit=literal { n $startpos $endpos (Ast.ELit lit) }
  | LPAREN e=expr RPAREN { n $startpos $endpos (Ast.EParen e) }
  ;

postfix_atom:
  | p=primary { p }
  | base=postfix_atom DOT fld=ident
      { n $startpos $endpos (Ast.EField { base; field = fld }) }
  | base=postfix_atom LPAREN args=expr_list_opt RPAREN
      {
        match base.v with
        | Ast.EName callee ->
            n $startpos $endpos (Ast.ECall { callee; args })
        | _ ->
            n $startpos $endpos (Ast.EIndex { base; index = args })
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

  | NOT rhs=expr
      { n $startpos $endpos (Ast.EUnop { op = Ast.UNot; rhs }) }

  | PLUS rhs=expr %prec UPLUS
      { n $startpos $endpos (Ast.EUnop { op = Ast.UPlus; rhs }) }

  | MINUS rhs=expr %prec UMINUS
      { n $startpos $endpos (Ast.EUnop { op = Ast.UMinus; rhs }) }

  | lhs=expr POW rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BMul; lhs; rhs }) } /* placeholder */

  | lhs=expr STAR rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BMul; lhs; rhs }) }

  | lhs=expr SLASH rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BDiv; lhs; rhs }) }

  | lhs=expr MOD rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BMod; lhs; rhs }) }

  | lhs=expr PLUS rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BAdd; lhs; rhs }) }

  | lhs=expr MINUS rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BSub; lhs; rhs }) }

  | lhs=expr LT rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BLt; lhs; rhs }) }

  | lhs=expr LE rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BLe; lhs; rhs }) }

  | lhs=expr GT rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BGt; lhs; rhs }) }

  | lhs=expr GE rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BGe; lhs; rhs }) }

  | lhs=expr EQ rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BEq; lhs; rhs }) }

  | lhs=expr NE rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BNe; lhs; rhs }) }

  | lhs=expr AND rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BAnd; lhs; rhs }) }

  | lhs=expr OR rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BOr; lhs; rhs }) }

  | lhs=expr XOR rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BBitXor; lhs; rhs }) }

  | lhs=expr EQV rhs=expr
      { n $startpos $endpos (Ast.EBinop { op = Ast.BBitXor; lhs; rhs }) }
  ;
