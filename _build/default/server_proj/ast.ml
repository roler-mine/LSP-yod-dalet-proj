(* src/ast.ml — AST matching jovial_parser.mly (with spans for LSP). *)

type span = Lexing.position * Lexing.position

type 'a located =
  { node : 'a
  ; span : span
  }

type ident = string

(* ---------- Directives / DEFINE ---------- *)

type directive_kind =
  | D_ICOMPOOL | D_ICOPY | D_ISKIP | D_IBEGIN | D_IEND | D_ILINKAGE | D_ITRACE
  | D_IINTERFERENCE | D_IREDUCIBLE
  | D_ILIST | D_INOLIST | D_IEJECT
  | D_IINITIALIZE | D_IORDER | D_ILEFTRIGHT | D_IREARRANGE
  | D_IBASE | D_IISBASE | D_IDROP

type dir_arg =
  | ArgName of ident
  | ArgInt of int
  | ArgString of string
  | ArgRange of int * int
  | ArgStar
  | ArgList of dir_arg list

type directive =
  { kind : directive_kind
  ; args : dir_arg list
  }

type define =
  { name : ident
  ; value : string
  }

(* ---------- Types / Declarations ---------- *)

type type_desc =
  | TyName of ident
  | TyClass of char * int option       (* U 10, S 21, F 30, B 10, C 80 *)
  | TyAlpha of int * int               (* A 2,13 *)
  | TyBit of int option                (* BIT 16 *)
  | TyStatus of ident list             (* STATUS (V(RED), ...) -> ["RED"; ...] *)
  | TyPointer of ident option          (* P X / POINTER X (kept optional) *)

type dim =
  | DimSize of int
  | DimRange of int * int
  | DimStar

type decl_node =
  | DType of ident * type_desc
  | DItem of { constant : bool; name : ident; ty : type_desc; init : expr option }
  | DTable of { name : ident; dims : dim list; ty : type_desc }
  | DBlock of { name : ident; fields : decl list }
  | DLabel of ident
  | DInline of ident
  | DUnknownDecl of string

and decl = decl_node located

(* ---------- Expressions ---------- *)

and unary_op =
  | UNot
  | UNeg
  | UDeref

and bin_op =
  | BAdd | BSub | BMul | BDiv | BPow | BMod
  | BAnd | BOr | BXor | BEqv
  | BShl | BShr
  | BEq | BNe | BLt | BLe | BGt | BGe

and selector =
  | SelField of ident
  | SelSubscript of expr list
  | SelStatus of ident

and lvalue =
  { base : ident
  ; sels : selector list
  }

and expr_node =
  | EInt of int
  | EReal of string
  | EChar of string
  | EBit of int * string
  | EBool of bool
  | ENull
  | EVar of lvalue
  | ECall of ident * expr list
  | EUnary of unary_op * expr
  | EBinary of bin_op * expr * expr
  | EStatusConst of ident
  | EConv of expr

and expr = expr_node located

(* ---------- Statements ---------- *)

type loop_control =
  | LCName of ident
  | LCIndex of char

type loop_step =
  | StepBy of expr
  | StepThen of expr

type case_label =
  | CLInt of int
  | CLRange of int * int

type stmt_node =
  | SNull
  | SAssign of lvalue list * expr
  | SCallRef of lvalue
  | SIf of expr * stmt * stmt option
  | SFor of loop_control * expr * loop_step option * expr option * stmt
  | SWhile of expr * stmt
  | SCase of expr * (case_label list * stmt) list * stmt option
  | SBlock of stmt list
  | SGoto of ident
  | SReturn
  | SStop
  | SAbort
  | SExit
  | SError of string option

and stmt = stmt_node located

(* ---------- Subroutines / Modules ---------- *)

type param =
  { p_name : ident
  ; p_ty : type_desc option
  }

type subroutine_kind = Proc | Func

type subroutine =
  { kind : subroutine_kind
  ; name : ident
  ; params : param list
  ; ret : type_desc option
  ; decls : decl list
  ; body : stmt list
  }

type program_module =
  { name : ident
  ; decls : decl list
  ; body : stmt list
  ; defs : subroutine located list
  }

type compool_module =
  { name : ident
  ; decls : decl list
  }

type proc_module =
  { decls : decl list
  ; defs : subroutine located list
  }

type module_kind =
  | MProgram of program_module
  | MCompool of compool_module
  | MProcModule of proc_module

type compilation_unit =
  { pre_directives : directive located list
  ; defines : define located list
  ; kind : module_kind
  ; post_directives : directive located list
  }
