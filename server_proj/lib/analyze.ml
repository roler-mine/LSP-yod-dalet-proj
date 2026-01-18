(* lib/analyze.ml *)

module T = Lsp.Types

type t =
  { ast : Ast.compilation_unit option
  ; symbols : Symbols.t
  ; diagnostics : T.Diagnostic.t list
  }

let diag_message (s : string) =
  (`String s : [ `String of string | `MarkupContent of T.MarkupContent.t ])

let mk_diag
    ?(severity = T.DiagnosticSeverity.Error)
    ?(source = "jovial-lsp")
    ~(range : T.Range.t)
    ~(message : string)
    ()
  : T.Diagnostic.t =
  T.Diagnostic.create
    ~message:(diag_message message)
    ~range
    ~severity
    ~source
    ()

let lexbuf_init ~(fname : string) (lexbuf : Lexing.lexbuf) : unit =
  let p =
    { Lexing.pos_fname = fname
    ; pos_lnum = 1
    ; pos_bol = 0
    ; pos_cnum = 0
    }
  in
  lexbuf.lex_curr_p <- p;
  lexbuf.lex_start_p <- p

let string_of_scalar = function
  | Ast.TyInt -> "INT"
  | Ast.TyReal -> "REAL"
  | Ast.TyBool -> "BOOL"
  | Ast.TyChar -> "CHAR"
  | Ast.TyString -> "STRING"
  | Ast.TyUnknown -> "?"

let rec string_of_type_expr (t : Ast.type_expr) : string =
  match t with
  | Ast.Scalar s -> string_of_scalar s
  | Ast.Named id -> Ast.loc_value id
  | Ast.Array (inner, size_opt) ->
      let inner_s = string_of_type_expr (Ast.loc_value inner) in
      (match size_opt with
       | None -> Printf.sprintf "ARRAY[%s]" inner_s
       | Some n -> Printf.sprintf "ARRAY[%s; %d]" inner_s n)

let opt_ty_of_type (t : Ast.type_expr Ast.located option) : string option =
  match t with
  | None -> None
  | Some lt -> Some (string_of_type_expr (Ast.loc_value lt))

let dup_diag ~(attempted : Symbols.symbol) ~existing:(_existing : Symbols.symbol) : T.Diagnostic.t =
  let range = Symbols.symbol_name_range attempted in
  let msg =
    Printf.sprintf
      "Duplicate definition of '%s' (already defined earlier)."
      attempted.name
  in
  mk_diag ~range ~message:msg ()

let add_global_or_diag (syms : Symbols.t) (diags : T.Diagnostic.t list ref) (sym : Symbols.symbol) =
  match Symbols.add_global syms sym with
  | Ok () -> ()
  | Error { existing; attempted } -> diags := dup_diag ~attempted ~existing :: !diags

let add_proc_or_diag (syms : Symbols.t) (diags : T.Diagnostic.t list ref) (sym : Symbols.symbol) =
  match Symbols.add_proc syms sym with
  | Ok () -> ()
  | Error { existing; attempted } -> diags := dup_diag ~attempted ~existing :: !diags

let add_in_proc_or_diag
    (syms : Symbols.t)
    (diags : T.Diagnostic.t list ref)
    ~(proc_name : string)
    (sym : Symbols.symbol)
  =
  match Symbols.add_in_proc syms ~proc_name sym with
  | Ok () -> ()
  | Error { existing; attempted } -> diags := dup_diag ~attempted ~existing :: !diags

let index_decl (syms : Symbols.t) (diags : T.Diagnostic.t list ref) (d : Ast.decl Ast.located) =
  let decl_span = Ast.loc_span d in
  match Ast.loc_value d with
  | Ast.Item it ->
      let sym =
        Symbols.make
          ~name:(Ast.loc_value it.name)
          ~kind:Symbols.Item
          ~decl_span
          ~name_span:(Ast.loc_span it.name)
          ?ty:(opt_ty_of_type it.ty)
          ()
      in
      add_global_or_diag syms diags sym

  | Ast.Table tb ->
      let ty =
        match tb.elem_ty with
        | None -> Some "TABLE"
        | Some t -> Some ("TABLE OF " ^ string_of_type_expr (Ast.loc_value t))
      in
      let sym =
        Symbols.make
          ~name:(Ast.loc_value tb.name)
          ~kind:Symbols.Table
          ~decl_span
          ~name_span:(Ast.loc_span tb.name)
          ?ty
          ()
      in
      add_global_or_diag syms diags sym

  | Ast.Proc p ->
      let proc_name = Ast.loc_value p.name in
      let proc_sym =
        Symbols.make
          ~name:proc_name
          ~kind:Symbols.Proc
          ~decl_span
          ~name_span:(Ast.loc_span p.name)
          ?ty:(opt_ty_of_type p.returns |> Option.map (fun s -> "RETURNS " ^ s))
          ()
      in
      add_proc_or_diag syms diags proc_sym;

      List.iter
        (fun (param_id, param_ty) ->
          let pname = Ast.loc_value param_id in
          let pspan = Ast.loc_span param_id in
          let sym =
            Symbols.make
              ~name:pname
              ~kind:Symbols.Param
              ~decl_span:pspan
              ~name_span:pspan
              ?ty:(opt_ty_of_type param_ty)
              ~container:proc_name
              ()
          in
          add_in_proc_or_diag syms diags ~proc_name sym)
        p.params

let index_ast (ast : Ast.compilation_unit) : Symbols.t * T.Diagnostic.t list =
  let syms = Symbols.create () in
  let diags = ref [] in
  List.iter (index_decl syms diags) ast.decls;
  (syms, List.rev !diags)

let parse (uri : T.DocumentUri.t) (text : string)
  : (Ast.compilation_unit, T.Diagnostic.t list) result =
  let lexbuf = Lexing.from_string text in
  lexbuf_init ~fname:(Lsp.Uri.to_string uri) lexbuf;
  try
    let ast = Parser.compilation_unit Lexer.token lexbuf in
    Ok ast
  with
  | Lexer.Lexing_error (msg, startp, endp) ->
      let range = Lsp_convert.range_of_lex startp endp in
      Error [ mk_diag ~range ~message:("Lexing error: " ^ msg) () ]
  | Parser.Error ->
      let range = Lsp_convert.range_of_lexbuf lexbuf in
      Error [ mk_diag ~range ~message:"Syntax error." () ]
  | exn ->
      let range = Lsp_convert.range_of_lexbuf lexbuf in
      Error [ mk_diag ~range ~message:("Internal parse failure: " ^ Printexc.to_string exn) () ]

let analyze ~(uri : T.DocumentUri.t) ~(text : string) : t =
  match parse uri text with
  | Error diags ->
      { ast = None; symbols = Symbols.create (); diagnostics = diags }
  | Ok ast ->
      let (symbols, sym_diags) = index_ast ast in
      { ast = Some ast; symbols; diagnostics = sym_diags }
