
(* The type of tokens. *)

type token = 
  | XOR
  | WHILE
  | TYPE
  | TRUE
  | THEN
  | TERM
  | TABLE
  | STRINGLIT of (string)
  | STOP
  | START
  | STAR
  | SLASH
  | SEMI
  | RPAREN
  | RETURN
  | REF
  | PROGRAM
  | PROC
  | POW
  | PLUS
  | OR
  | NOT
  | NE
  | MOD
  | MINUS
  | LT
  | LPAREN
  | LE
  | ITEM
  | INTLIT of (string)
  | IF
  | ID of (string)
  | ICOMPOOL
  | GT
  | GOTO
  | GE
  | FOR
  | FLOATLIT of (string)
  | FALSE
  | EXIT
  | EQV
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | DEFINE
  | DEFAULT
  | DEF
  | CONV_R
  | CONV_L
  | COMPOOL
  | COMMA
  | COLON
  | CASE
  | BY
  | BLOCK
  | BEGIN
  | BANG
  | AT
  | AND
  | ABORT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
  (* The indexed type of terminal symbols. *)
  
  type _ terminal = 
    | T_error : unit terminal
    | T_XOR : unit terminal
    | T_WHILE : unit terminal
    | T_TYPE : unit terminal
    | T_TRUE : unit terminal
    | T_THEN : unit terminal
    | T_TERM : unit terminal
    | T_TABLE : unit terminal
    | T_STRINGLIT : (string) terminal
    | T_STOP : unit terminal
    | T_START : unit terminal
    | T_STAR : unit terminal
    | T_SLASH : unit terminal
    | T_SEMI : unit terminal
    | T_RPAREN : unit terminal
    | T_RETURN : unit terminal
    | T_REF : unit terminal
    | T_PROGRAM : unit terminal
    | T_PROC : unit terminal
    | T_POW : unit terminal
    | T_PLUS : unit terminal
    | T_OR : unit terminal
    | T_NOT : unit terminal
    | T_NE : unit terminal
    | T_MOD : unit terminal
    | T_MINUS : unit terminal
    | T_LT : unit terminal
    | T_LPAREN : unit terminal
    | T_LE : unit terminal
    | T_ITEM : unit terminal
    | T_INTLIT : (string) terminal
    | T_IF : unit terminal
    | T_ID : (string) terminal
    | T_ICOMPOOL : unit terminal
    | T_GT : unit terminal
    | T_GOTO : unit terminal
    | T_GE : unit terminal
    | T_FOR : unit terminal
    | T_FLOATLIT : (string) terminal
    | T_FALSE : unit terminal
    | T_EXIT : unit terminal
    | T_EQV : unit terminal
    | T_EQ : unit terminal
    | T_EOF : unit terminal
    | T_END : unit terminal
    | T_ELSE : unit terminal
    | T_DOT : unit terminal
    | T_DEFINE : unit terminal
    | T_DEFAULT : unit terminal
    | T_DEF : unit terminal
    | T_CONV_R : unit terminal
    | T_CONV_L : unit terminal
    | T_COMPOOL : unit terminal
    | T_COMMA : unit terminal
    | T_COLON : unit terminal
    | T_CASE : unit terminal
    | T_BY : unit terminal
    | T_BLOCK : unit terminal
    | T_BEGIN : unit terminal
    | T_BANG : unit terminal
    | T_AT : unit terminal
    | T_AND : unit terminal
    | T_ABORT : unit terminal
  
  (* The indexed type of nonterminal symbols. *)
  
  type _ nonterminal = 
    | N_while_stmt : (Ast.stmt Ast.node) nonterminal
    | N_while_phrase : (Ast.expr Ast.node) nonterminal
    | N_while_opt : (Ast.expr Ast.node option) nonterminal
    | N_type_spec : (Ast.type_expr Ast.node) nonterminal
    | N_type_sizes_opt : (Ast.expr Ast.node list) nonterminal
    | N_type_size : (Ast.expr Ast.node) nonterminal
    | N_type_decl : (Ast.decl Ast.node list) nonterminal
    | N_terminator_opt : (unit) nonterminal
    | N_terminator : (unit) nonterminal
    | N_table_record_after_term_opt : (Ast.field_decl Ast.node list option) nonterminal
    | N_table_preset_opt : (Ast.expr Ast.node option) nonterminal
    | N_table_elem_type_opt : (Ast.type_expr Ast.node option) nonterminal
    | N_table_dims : (Ast.expr Ast.node list) nonterminal
    | N_stop_stmt : (Ast.stmt Ast.node) nonterminal
    | N_statement : (Ast.stmt Ast.node) nonterminal
    | N_simple_stmt : (Ast.stmt Ast.node) nonterminal
    | N_simple_or_control_stmt : (Ast.stmt Ast.node) nonterminal
    | N_rev_module_items : (Ast.toplevel list list) nonterminal
    | N_rev_field_decl_list : (Ast.field_decl Ast.node list) nonterminal
    | N_rev_directive_args : (string Ast.node list) nonterminal
    | N_rev_decl_section : (Ast.decl Ast.node list list) nonterminal
    | N_return_stmt : (Ast.stmt Ast.node) nonterminal
    | N_record_opt : (Ast.field_decl Ast.node list option) nonterminal
    | N_program : (Ast.program) nonterminal
    | N_proc_header_tail_opt : (bool * bool * Ast.type_expr Ast.node option) nonterminal
    | N_proc_header_tail : (bool * bool * Ast.type_expr Ast.node option) nonterminal
    | N_proc_header_atom : (bool * bool * Ast.type_expr Ast.node option) nonterminal
    | N_proc_decl : (Ast.decl Ast.node list) nonterminal
    | N_proc_body_opt : ((Ast.decl Ast.node list * Ast.stmt Ast.node) option) nonterminal
    | N_primary : (Ast.expr Ast.node) nonterminal
    | N_preset_items_opt : (Ast.expr Ast.node list) nonterminal
    | N_preset_items : (Ast.expr Ast.node list) nonterminal
    | N_preset_item : (Ast.expr Ast.node) nonterminal
    | N_preset_expr : (Ast.expr Ast.node) nonterminal
    | N_postfix_atom : (Ast.expr Ast.node) nonterminal
    | N_postfix : (Ast.expr Ast.node) nonterminal
    | N_outs_opt : (Ast.ident list) nonterminal
    | N_module_items_opt : (Ast.toplevel list) nonterminal
    | N_module_items : (Ast.toplevel list) nonterminal
    | N_module_item : (Ast.toplevel list) nonterminal
    | N_modifier_req : (string) nonterminal
    | N_modifier_opt : (string option) nonterminal
    | N_lvalue_list : (Ast.expr Ast.node list) nonterminal
    | N_lvalue : (Ast.expr Ast.node) nonterminal
    | N_literal : (Ast.literal) nonterminal
    | N_labels_opt : (Ast.ident list) nonterminal
    | N_label : (Ast.ident) nonterminal
    | N_jmodules : (Ast.toplevel list list) nonterminal
    | N_jmodule : (Ast.toplevel list) nonterminal
    | N_item_init_opt : (Ast.expr Ast.node option) nonterminal
    | N_item_attrs_opt : (unit) nonterminal
    | N_item_attrs : (unit) nonterminal
    | N_item_attr : (unit) nonterminal
    | N_if_stmt : (Ast.stmt Ast.node) nonterminal
    | N_ident : (Ast.ident) nonterminal
    | N_id_list : (Ast.ident list) nonterminal
    | N_header_items : (Ast.toplevel list) nonterminal
    | N_header_item : (Ast.toplevel list) nonterminal
    | N_group_decl : (Ast.decl Ast.node list) nonterminal
    | N_goto_stmt : (Ast.stmt Ast.node) nonterminal
    | N_formals_opt : (Ast.param Ast.node list) nonterminal
    | N_formal_param_groups_opt : (Ast.param Ast.node list) nonterminal
    | N_formal_param_groups : (Ast.param Ast.node list) nonterminal
    | N_for_update : (Ast.expr Ast.node option * Ast.expr Ast.node option) nonterminal
    | N_for_tail : (Ast.expr Ast.node option *
  (Ast.expr Ast.node option * Ast.expr Ast.node option)) nonterminal
    | N_for_stmt : (Ast.stmt Ast.node) nonterminal
    | N_field_decl_list : (Ast.field_decl Ast.node list) nonterminal
    | N_field_decl : (Ast.field_decl Ast.node) nonterminal
    | N_expr_opt : (Ast.expr Ast.node option) nonterminal
    | N_expr_list_opt : (Ast.expr Ast.node list) nonterminal
    | N_expr_list : (Ast.expr Ast.node list) nonterminal
    | N_expr : (Ast.expr Ast.node) nonterminal
    | N_exit_stmt : (Ast.stmt Ast.node) nonterminal
    | N_else_opt : (Ast.stmt Ast.node option) nonterminal
    | N_directive_name : (string Ast.node) nonterminal
    | N_directive_decl : (Ast.decl Ast.node) nonterminal
    | N_directive_args_opt : (string Ast.node list) nonterminal
    | N_directive_args : (string Ast.node list) nonterminal
    | N_directive_arg : (string Ast.node) nonterminal
    | N_dim_list_opt : (Ast.expr Ast.node list) nonterminal
    | N_dim_list : (Ast.expr Ast.node list) nonterminal
    | N_dim : (Ast.expr Ast.node) nonterminal
    | N_define_list_option : (Ast.ident) nonterminal
    | N_define_list_opt : (Ast.ident option) nonterminal
    | N_define_formals_opt : (Ast.ident list) nonterminal
    | N_define_formals : (Ast.ident list) nonterminal
    | N_define_decl : (Ast.decl Ast.node list) nonterminal
    | N_decl_section : (Ast.decl Ast.node list) nonterminal
    | N_decl_item : (Ast.decl Ast.node list) nonterminal
    | N_data_decl : (Ast.decl Ast.node list) nonterminal
    | N_compound_stmt : (Ast.stmt Ast.node) nonterminal
    | N_compool_arg : (string Ast.node) nonterminal
    | N_case_stmt : (Ast.stmt Ast.node) nonterminal
    | N_case_sep : (unit) nonterminal
    | N_case_options : (('a list * Ast.stmt Ast.node) list) nonterminal
    | N_case_option : ('a list * Ast.stmt Ast.node) nonterminal
    | N_case_index_list : (Ast.expr Ast.node list) nonterminal
    | N_case_index : (Ast.expr Ast.node) nonterminal
    | N_call_stmt : (Ast.stmt Ast.node) nonterminal
    | N_block_list_opt : (Ast.stmt Ast.node list) nonterminal
    | N_block_list : (Ast.stmt Ast.node list) nonterminal
    | N_block_decl : (Ast.decl Ast.node list) nonterminal
    | N_attr_paren_payload : (unit) nonterminal
    | N_attr_arg_list_opt : (unit) nonterminal
    | N_attr_arg_list : (unit) nonterminal
    | N_attr_arg : (unit) nonterminal
    | N_assign_stmt : (Ast.stmt Ast.node) nonterminal
    | N_actuals_opt : (Ast.expr Ast.node list) nonterminal
    | N_abort_stmt : (Ast.stmt Ast.node) nonterminal
  
  (* The inspection API. *)
  
  include MenhirLib.IncrementalEngine.INSPECTION
    with type 'a lr1state := 'a lr1state
    with type production := production
    with type 'a terminal := 'a terminal
    with type 'a nonterminal := 'a nonterminal
    with type 'a env := 'a env
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val program: Lexing.position -> (Ast.program) MenhirInterpreter.checkpoint
  
end
