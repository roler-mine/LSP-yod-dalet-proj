module T = Lsp.Types

type t =
  { ast : Ast.module_ option
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

let span_of_lexbuf (lexbuf : Lexing.lexbuf) : Ast.span =
  { Ast.startp = lexbuf.lex_start_p; endp = lexbuf.lex_curr_p }

let range_of_span (sp : Ast.span) : T.Range.t =
  Lsp_convert.range_of_lex sp.Ast.startp sp.Ast.endp

(* ---------- Lexer stream ---------- *)

type tok =
  { tok : Parser.token
  ; span : Ast.span
  }

type scanner =
  { toks : tok array
  ; mutable i : int
  ; n : int
  }

let sc_make (toks : tok array) : scanner =
  { toks; i = 0; n = Array.length toks }

let sc_peek (sc : scanner) : tok option =
  if sc.i < sc.n then Some sc.toks.(sc.i) else None

let sc_peek_n (sc : scanner) (k : int) : tok option =
  let j = sc.i + k in
  if 0 <= j && j < sc.n then Some sc.toks.(j) else None

let sc_next (sc : scanner) : tok option =
  let r = sc_peek sc in
  if sc.i < sc.n then sc.i <- sc.i + 1;
  r

let tok_span (t : tok) : Ast.span = t.span

let is_use_attr_tok (t : Parser.token) : bool =
  match t with
  | Parser.REC
  | Parser.RENT
  | Parser.STATIC
  | Parser.PARALLEL
  | Parser.INLINE -> true
  | _ -> false

let is_expr_op_tok (t : Parser.token) : bool =
  match t with
  | Parser.LPAREN
  | Parser.COMMA
  | Parser.EQUAL
  | Parser.PLUS
  | Parser.MINUS
  | Parser.STAR
  | Parser.SLASH
  | Parser.LT
  | Parser.LE
  | Parser.GT
  | Parser.GE
  | Parser.NEQ
  | Parser.EQ
  | Parser.NQ
  | Parser.LS
  | Parser.LQ
  | Parser.GR
  | Parser.GQ
  | Parser.AND
  | Parser.OR
  | Parser.XOR
  | Parser.EQV
  | Parser.NOT
  | Parser.TO
  | Parser.BY
  | Parser.WHILE
  | Parser.THEN
  | Parser.IF
  | Parser.CASE
  | Parser.OF
  | Parser.WHEN
  | Parser.OTHERWISE
  | Parser.RETURN
  | Parser.DEFAULT
  | Parser.POS
  | Parser.REP -> true
  | _ -> false

let is_decl_context_prev (t : Parser.token) : bool =
  match t with
  | Parser.DEF
  | Parser.REF
  | Parser.PROC
  | Parser.FUNCTION
  | Parser.ITEM
  | Parser.TABLE
  | Parser.TYPE
  | Parser.STATUS
  | Parser.BLOCK
  | Parser.LABEL
  | Parser.DEFINE
  | Parser.WITH
  | Parser.INSTANCE
  | Parser.LIKE
  | Parser.OVERLAY
  | Parser.BYREF
  | Parser.BYVAL
  | Parser.BYRES -> true
  | _ -> false

let skip_balanced_parens (sc : scanner) : unit =
  let rec loop depth =
    match sc_peek sc with
    | None -> ()
    | Some t -> (
        ignore (sc_next sc);
        match t.tok with
        | Parser.LPAREN -> loop (depth + 1)
        | Parser.RPAREN -> if depth <= 1 then () else loop (depth - 1)
        | _ -> loop depth)
  in
  match sc_peek sc with
  | Some { tok = Parser.LPAREN; _ } ->
      ignore (sc_next sc);
      loop 1
  | _ -> ()

let skip_until_semi (sc : scanner) : Ast.span option =
  let last = ref None in
  let rec loop () =
    match sc_peek sc with
    | None -> !last
    | Some t ->
        ignore (sc_next sc);
        last := Some (tok_span t);
        (match t.tok with
         | Parser.SEMI -> !last
         | _ -> loop ())
  in
  loop ()

let parse_type_spec (sc : scanner) : string option =
  match sc_peek sc with
  | Some { tok = Parser.TYPEATOM a; _ } ->
      ignore (sc_next sc);
      (match sc_peek sc with
       | Some { tok = Parser.INT n; _ } ->
           ignore (sc_next sc);
           Some (a ^ " " ^ string_of_int n)
       | _ -> Some a)
  | Some { tok = Parser.IDENT n; _ } ->
      ignore (sc_next sc);
      Some n
  | _ -> None

let parse_ident_list (sc : scanner) : (string * Ast.span) list =
  let rec loop acc =
    match sc_peek sc with
    | Some { tok = Parser.IDENT nm; span } ->
        ignore (sc_next sc);
        let acc' = (nm, span) :: acc in
        (match sc_peek sc with
         | Some { tok = Parser.COMMA; _ } ->
             ignore (sc_next sc);
             loop acc'
         | _ -> List.rev acc')
    | _ -> List.rev acc
  in
  loop []

let parse_formal_params (sc : scanner) : (string * Ast.span) list =
  match sc_peek sc with
  | Some { tok = Parser.LPAREN; _ } ->
      ignore (sc_next sc);
      let rec loop acc =
        match sc_peek sc with
        | None -> List.rev acc
        | Some { tok = Parser.RPAREN; _ } ->
            ignore (sc_next sc);
            List.rev acc
        | Some { tok = Parser.COMMA; _ }
        | Some { tok = Parser.BYREF; _ }
        | Some { tok = Parser.BYVAL; _ }
        | Some { tok = Parser.BYRES; _ } ->
            ignore (sc_next sc);
            loop acc
        | Some { tok = Parser.IDENT nm; span } ->
            ignore (sc_next sc);
            (* skip type/attrs/defaults until comma/rparen *)
            let rec skip_tail () =
              match sc_peek sc with
              | None -> ()
              | Some { tok = Parser.COMMA; _ }
              | Some { tok = Parser.RPAREN; _ } -> ()
              | _ ->
                  ignore (sc_next sc);
                  skip_tail ()
            in
            skip_tail ();
            loop ((nm, span) :: acc)
        | _ ->
            ignore (sc_next sc);
            loop acc
      in
      loop []
  | _ -> []

(* IMPORTANT: Symbols.make expects plain strings for optional args.
   This wrapper prevents accidental string option option / 'a option vs string errors. *)
let make_symbol
    ~(name : string)
    ~(kind : Symbols.kind)
    ~(decl_span : Ast.span)
    ~(name_span : Ast.span)
    ~(ty_opt : string option)
    ~(container_opt : string option)
  : Symbols.symbol =
  match ty_opt, container_opt with
  | None, None -> Symbols.make ~name ~kind ~decl_span ~name_span ()
  | Some ty, None -> Symbols.make ~name ~kind ~decl_span ~name_span ~ty ()
  | None, Some c -> Symbols.make ~name ~kind ~decl_span ~name_span ~container:c ()
  | Some ty, Some c -> Symbols.make ~name ~kind ~decl_span ~name_span ~ty ~container:c ()

let dup_diag ~(attempted : Symbols.symbol) : T.Diagnostic.t =
  let range = Symbols.symbol_name_range attempted in
  let msg =
    Printf.sprintf "Duplicate definition of '%s' (already defined earlier)." (Symbols.sym_name attempted)
  in
  mk_diag ~range ~message:msg ()

let add_global_or_diag (syms : Symbols.t) (diags : T.Diagnostic.t list ref) (sym : Symbols.symbol) : unit =
  match Symbols.add_global syms sym with
  | Ok () -> ()
  | Error { attempted; _ } -> diags := dup_diag ~attempted :: !diags

let add_proc_or_diag (syms : Symbols.t) (diags : T.Diagnostic.t list ref) (sym : Symbols.symbol) : unit =
  match Symbols.add_proc syms sym with
  | Ok () -> ()
  | Error { attempted; _ } -> diags := dup_diag ~attempted :: !diags

let add_in_proc_or_diag
    (syms : Symbols.t)
    (diags : T.Diagnostic.t list ref)
    ~(proc_name : string)
    (sym : Symbols.symbol)
  : unit =
  match Symbols.add_in_proc syms ~proc_name sym with
  | Ok () -> ()
  | Error { attempted; _ } -> diags := dup_diag ~attempted :: !diags

(* ---------- Pass 1: symbol indexing (token-based) ---------- *)

type pending_ref =
  { name : string
  ; name_span : Ast.span
  ; decl_span : Ast.span
  ; ty_opt : string option
  }

let index_item_decl
    ~(syms : Symbols.t)
    ~(diags : T.Diagnostic.t list ref)
    ~(proc_name_opt : string option)
    (sc : scanner)
  : unit =
  let item_span =
    match sc_next sc with
    | Some t -> tok_span t
    | None -> { Ast.startp = Lexing.dummy_pos; endp = Lexing.dummy_pos }
  in
  let names = parse_ident_list sc in
  let ty_opt = parse_type_spec sc in
  let last_span_opt = skip_until_semi sc in
  let endp =
    match last_span_opt with
    | Some sp -> sp.Ast.endp
    | None -> item_span.Ast.endp
  in
  let decl_span = { Ast.startp = item_span.Ast.startp; endp } in
  let mk (nm, nm_span) (kind : Symbols.kind) =
    make_symbol
      ~name:nm
      ~kind
      ~decl_span
      ~name_span:nm_span
      ~ty_opt
      ~container_opt:proc_name_opt
  in
  match proc_name_opt with
  | None ->
      List.iter (fun it -> add_global_or_diag syms diags (mk it Symbols.Item)) names
  | Some pn ->
      List.iter (fun it -> add_in_proc_or_diag syms diags ~proc_name:pn (mk it Symbols.Local)) names

let skip_use_attrs (sc : scanner) : unit =
  let rec loop () =
    match sc_peek sc with
    | Some { tok; _ } when is_use_attr_tok tok ->
        ignore (sc_next sc);
        (* Some dialects allow attribute args: RENT (X, Y). *)
        (match sc_peek sc with
         | Some { tok = Parser.LPAREN; _ } -> skip_balanced_parens sc
         | _ -> ());
        loop ()
    | _ -> ()
  in
  loop ()

let scan_begin_end_for_locals
    ~(syms : Symbols.t)
    ~(diags : T.Diagnostic.t list ref)
    ~(proc_name : string)
    (sc : scanner)
  : Ast.span option =
  (* expects current token = BEGIN *)
  let depth = ref 0 in
  let end_span = ref None in

  let rec loop () =
    match sc_peek sc with
    | None -> !end_span
    | Some t -> (
        match t.tok with
        | Parser.BEGIN ->
            ignore (sc_next sc);
            incr depth;
            loop ()
        | Parser.END ->
            ignore (sc_next sc);
            decr depth;
            if !depth <= 0 then (
              end_span := Some (tok_span t);
              !end_span
            ) else loop ()
        | Parser.ITEM when !depth = 1 ->
            index_item_decl ~syms ~diags ~proc_name_opt:(Some proc_name) sc;
            loop ()
        | _ ->
            ignore (sc_next sc);
            loop ())
  in
  loop ()

let parse_def_like
    ~(syms : Symbols.t)
    ~(diags : T.Diagnostic.t list ref)
    (sc : scanner)
  : unit =
  (* assumes current token is DEF *)
  let def_span =
    match sc_next sc with
    | Some t -> tok_span t
    | None -> { Ast.startp = Lexing.dummy_pos; endp = Lexing.dummy_pos }
  in

  let kind_is_fun =
    match sc_peek sc with
    | Some { tok = Parser.FUNCTION; _ } -> ignore (sc_next sc); true
    | Some { tok = Parser.PROC; _ } -> ignore (sc_next sc); false
    | _ -> false
  in

  let (name, name_span) =
    match sc_peek sc with
    | Some { tok = Parser.IDENT nm; span } ->
        ignore (sc_next sc);
        (nm, span)
    | _ ->
        ("<anon>", def_span)
  in

  let params = parse_formal_params sc in
  let ret_ty_opt = if kind_is_fun then parse_type_spec sc else None in
  skip_use_attrs sc;

  (* optional ';' after header *)
  let semi_span_opt =
    match sc_peek sc with
    | Some { tok = Parser.SEMI; span } ->
        ignore (sc_next sc);
        Some span
    | _ -> None
  in

  (* optional body: BEGIN ... END [name] ;* *)
  let end_span_opt =
    match sc_peek sc with
    | Some { tok = Parser.BEGIN; _ } ->
        let sp = scan_begin_end_for_locals ~syms ~diags ~proc_name:name sc in
        (* optional END name *)
        (match sc_peek sc with
         | Some { tok = Parser.IDENT _; _ } -> ignore (sc_next sc)
         | _ -> ());
        (* optional trailing semis *)
        while (match sc_peek sc with Some { tok = Parser.SEMI; _ } -> true | _ -> false) do
          ignore (sc_next sc)
        done;
        sp
    | _ -> None
  in

  let decl_endp =
    match end_span_opt with
    | Some sp -> sp.Ast.endp
    | None -> (
        match semi_span_opt with
        | Some sp -> sp.Ast.endp
        | None -> name_span.Ast.endp)
  in

  let decl_span = { Ast.startp = def_span.Ast.startp; endp = decl_endp } in

  let sig_params =
    match params with
    | [] -> ""
    | xs ->
        let names = List.map fst xs in
        "(" ^ String.concat ", " names ^ ")"
  in

  let ty_opt =
    if kind_is_fun then
      match ret_ty_opt with
      | None -> Some ("FUNCTION" ^ sig_params)
      | Some r -> Some ("FUNCTION" ^ sig_params ^ " RETURNS " ^ r)
    else
      Some ("PROC" ^ sig_params)
  in

  let proc_sym =
    make_symbol
      ~name
      ~kind:Symbols.Proc
      ~decl_span
      ~name_span
      ~ty_opt
      ~container_opt:None
  in
  add_proc_or_diag syms diags proc_sym;

  List.iter
    (fun (pname, pspan) ->
      let psym =
        make_symbol
          ~name:pname
          ~kind:Symbols.Param
          ~decl_span:pspan
          ~name_span:pspan
          ~ty_opt:None
          ~container_opt:(Some name)
      in
      add_in_proc_or_diag syms diags ~proc_name:name psym)
    params

let parse_ref_like (pending : pending_ref list ref) (sc : scanner) : unit =
  (* assumes current token is REF *)
  let ref_span =
    match sc_next sc with
    | Some t -> tok_span t
    | None -> { Ast.startp = Lexing.dummy_pos; endp = Lexing.dummy_pos }
  in

  let kind_is_fun =
    match sc_peek sc with
    | Some { tok = Parser.FUNCTION; _ } -> ignore (sc_next sc); true
    | Some { tok = Parser.PROC; _ } -> ignore (sc_next sc); false
    | _ -> false
  in

  let (name, name_span) =
    match sc_peek sc with
    | Some { tok = Parser.IDENT nm; span } ->
        ignore (sc_next sc);
        (nm, span)
    | _ -> ("<anon>", ref_span)
  in

  (* Formal params only if they appear immediately after the name. *)
  let params = parse_formal_params sc in
  let ret_ty_opt = if kind_is_fun then parse_type_spec sc else None in

  skip_use_attrs sc;

  let last_span_opt = skip_until_semi sc in
  let endp =
    match last_span_opt with
    | Some sp -> sp.Ast.endp
    | None -> name_span.Ast.endp
  in
  let decl_span = { Ast.startp = ref_span.Ast.startp; endp } in

  let sig_params =
    match params with
    | [] -> ""
    | xs ->
        let names = List.map fst xs in
        "(" ^ String.concat ", " names ^ ")"
  in

  let ty_opt =
    if kind_is_fun then
      match ret_ty_opt with
      | None -> Some ("REF FUNCTION" ^ sig_params)
      | Some r -> Some ("REF FUNCTION" ^ sig_params ^ " RETURNS " ^ r)
    else
      Some ("REF PROC" ^ sig_params)
  in

  pending := { name; name_span; decl_span; ty_opt } :: !pending

let lex_all (uri : T.DocumentUri.t) (text : string)
  : (tok array, T.Diagnostic.t list) result =
  let lexbuf = Lexing.from_string text in
  lexbuf_init ~fname:(Lsp.Uri.to_string uri) lexbuf;

  let rec loop acc =
    try
      let t = Lexer.token lexbuf in
      let wrapped = { tok = t; span = span_of_lexbuf lexbuf } in
      match t with
      | Parser.EOF -> Ok (Array.of_list (List.rev (wrapped :: acc)))
      | _ -> loop (wrapped :: acc)
    with
    | Lexer.Lexing_error msg ->
        let r = Lsp_convert.range_of_lexbuf lexbuf in
        Error [ mk_diag ~range:r ~message:("Lexing error: " ^ msg) () ]
    | exn ->
        let r = Lsp_convert.range_of_lexbuf lexbuf in
        Error [ mk_diag ~range:r ~message:("Lex failure: " ^ Printexc.to_string exn) () ]
  in
  loop []

let build_symbols (toks : tok array) : Symbols.t * T.Diagnostic.t list =
  let syms = Symbols.create () in
  let diags = ref [] in
  let pending = ref [] in
  let sc = sc_make toks in

  let rec loop () =
    match sc_peek sc with
    | None -> ()
    | Some t -> (
        match t.tok with
        | Parser.DEF ->
            (match sc_peek_n sc 1 with
             | Some { tok = Parser.PROC; _ }
             | Some { tok = Parser.FUNCTION; _ } ->
                 parse_def_like ~syms ~diags sc;
                 loop ()
             | _ ->
                 ignore (sc_next sc);
                 loop ())
        | Parser.REF ->
            (match sc_peek_n sc 1 with
             | Some { tok = Parser.PROC; _ }
             | Some { tok = Parser.FUNCTION; _ } ->
                 parse_ref_like pending sc;
                 loop ()
             | _ ->
                 ignore (sc_next sc);
                 loop ())
        | Parser.ITEM ->
            index_item_decl ~syms ~diags ~proc_name_opt:None sc;
            loop ()
        | Parser.DEFINE ->
            (* DEFINE NAME [=] rhs ; *)
            let defsp =
              match sc_next sc with
              | Some x -> tok_span x
              | None -> { Ast.startp = Lexing.dummy_pos; endp = Lexing.dummy_pos }
            in
            let name_opt =
              match sc_peek sc with
              | Some { tok = Parser.IDENT nm; span } ->
                  ignore (sc_next sc);
                  Some (nm, span)
              | _ -> None
            in
            (match sc_peek sc with
             | Some { tok = Parser.EQUAL; _ } -> ignore (sc_next sc)
             | _ -> ());
            let last_span_opt = skip_until_semi sc in
            (match name_opt with
             | None -> ()
             | Some (nm, nm_span) ->
                 let endp =
                   match last_span_opt with
                   | Some sp -> sp.Ast.endp
                   | None -> nm_span.Ast.endp
                 in
                 let decl_span = { Ast.startp = defsp.Ast.startp; endp } in
                 let sym =
                   make_symbol
                     ~name:nm
                     ~kind:Symbols.Item
                     ~decl_span
                     ~name_span:nm_span
                     ~ty_opt:(Some "DEFINE")
                     ~container_opt:None
                 in
                 add_global_or_diag syms diags sym);
            loop ()
        | _ ->
            ignore (sc_next sc);
            loop ())
  in

  loop ();

  (* Add REF procs only if not defined later by DEF. *)
  let rec add_pending = function
    | [] -> ()
    | r :: tl ->
        (match Symbols.find syms r.name with
         | Some _ -> ()
         | None ->
             let sym =
               make_symbol
                 ~name:r.name
                 ~kind:Symbols.Proc
                 ~decl_span:r.decl_span
                 ~name_span:r.name_span
                 ~ty_opt:r.ty_opt
                 ~container_opt:None
             in
             add_proc_or_diag syms diags sym);
        add_pending tl
  in
  add_pending (List.rev !pending);

  (syms, List.rev !diags)

(* ---------- Pass 2: lightweight semantic checks ---------- *)

let undefined_diag ~(span : Ast.span) ~(msg : string) : T.Diagnostic.t =
  mk_diag ~range:(range_of_span span) ~message:msg ()

let semantic_checks (toks : tok array) (syms : Symbols.t) : T.Diagnostic.t list =
  let diags = ref [] in
  let sc = sc_make toks in

  let skip_item_decl_tokens () : unit =
    (* assumes current token = ITEM; consume through ';' *)
    ignore (sc_next sc);
    ignore (parse_ident_list sc);
    ignore (parse_type_spec sc);
    ignore (skip_until_semi sc)
  in

  let has_symbol ~(proc_name_opt : string option) (nm : string) : bool =
    match proc_name_opt with
    | None -> (match Symbols.find syms nm with Some _ -> true | None -> false)
    | Some pn -> (match Symbols.find ~proc_name:pn syms nm with Some _ -> true | None -> false)
  in

  let scan_value_idents ~(proc_name_opt : string option) : unit =
    (* consume tokens until matching RPAREN; check idents in expression-ish positions *)
    let depth = ref 0 in
    let prev_tok : Parser.token option ref = ref None in

    let rec loop () =
      match sc_peek sc with
      | None -> ()
      | Some t -> (
          match t.tok with
          | Parser.LPAREN ->
              ignore (sc_next sc);
              incr depth;
              prev_tok := Some Parser.LPAREN;
              loop ()
          | Parser.RPAREN ->
              ignore (sc_next sc);
              decr depth;
              prev_tok := Some Parser.RPAREN;
              if !depth <= 0 then () else loop ()
          | Parser.IDENT nm ->
              let is_decl =
                match !prev_tok with
                | Some p when is_decl_context_prev p -> true
                | _ -> false
              in
              let is_exprish =
                match !prev_tok with
                | Some p when is_expr_op_tok p -> true
                | _ -> false
              in
              if (not is_decl) && (is_exprish || !depth > 0) then (
                if not (has_symbol ~proc_name_opt nm) then
                  diags := undefined_diag ~span:(tok_span t) ~msg:("Undefined identifier: " ^ nm) :: !diags
              );
              ignore (sc_next sc);
              prev_tok := Some (Parser.IDENT "");
              loop ()
          | Parser.ITEM ->
              (* declaration: consume without checks *)
              skip_item_decl_tokens ();
              prev_tok := Some Parser.ITEM;
              loop ()
          | _ ->
              ignore (sc_next sc);
              prev_tok := Some t.tok;
              loop ())
    in

    match sc_peek sc with
    | Some { tok = Parser.LPAREN; _ } ->
        ignore (sc_next sc);
        depth := 1;
        prev_tok := Some Parser.LPAREN;
        loop ()
    | _ -> ()
  in

  let scan_proc_body (proc_name : string) : unit =
    (* expects current token = BEGIN *)
    let depth = ref 0 in
    let prev_tok : Parser.token option ref = ref None in

    let rec loop () =
      match sc_peek sc with
      | None -> ()
      | Some t -> (
          match t.tok with
          | Parser.BEGIN ->
              ignore (sc_next sc);
              incr depth;
              prev_tok := Some Parser.BEGIN;
              loop ()
          | Parser.END ->
              ignore (sc_next sc);
              decr depth;
              prev_tok := Some Parser.END;
              if !depth <= 0 then () else loop ()
          | Parser.ITEM when !depth = 1 ->
              (* declaration: skip without side effects *)
              skip_item_decl_tokens ();
              prev_tok := Some Parser.ITEM;
              loop ()
          | Parser.IDENT nm ->
              (match sc_peek_n sc 1 with
               | Some { tok = Parser.EQUAL; _ } ->
                   if not (has_symbol ~proc_name_opt:(Some proc_name) nm) then
                     diags :=
                       undefined_diag
                         ~span:(tok_span t)
                         ~msg:("Undefined assignment target: " ^ nm)
                       :: !diags
               | Some { tok = Parser.LPAREN; _ } ->
                   if not (has_symbol ~proc_name_opt:(Some proc_name) nm) then
                     diags :=
                       undefined_diag
                         ~span:(tok_span t)
                         ~msg:("Undefined call target: " ^ nm)
                       :: !diags
               | _ ->
                   (match !prev_tok with
                    | Some p when is_expr_op_tok p ->
                        if not (has_symbol ~proc_name_opt:(Some proc_name) nm) then
                          diags := undefined_diag ~span:(tok_span t) ~msg:("Undefined identifier: " ^ nm) :: !diags
                    | _ -> ()));
              ignore (sc_next sc);
              prev_tok := Some (Parser.IDENT "");
              (match sc_peek sc with
               | Some { tok = Parser.LPAREN; _ } -> scan_value_idents ~proc_name_opt:(Some proc_name)
               | _ -> ());
              loop ()
          | Parser.LPAREN ->
              scan_value_idents ~proc_name_opt:(Some proc_name);
              prev_tok := Some Parser.RPAREN;
              loop ()
          | _ ->
              ignore (sc_next sc);
              prev_tok := Some t.tok;
              loop ())
    in
    loop ()
  in

  let rec scan_global () : unit =
    match sc_peek sc with
    | None -> ()
    | Some t -> (
        match t.tok with
        | Parser.DEF ->
            (match sc_peek_n sc 1 with
             | Some { tok = Parser.PROC; _ }
             | Some { tok = Parser.FUNCTION; _ } ->
                 ignore (sc_next sc); (* DEF *)
                 (match sc_peek sc with
                  | Some { tok = Parser.PROC; _ } -> ignore (sc_next sc)
                  | Some { tok = Parser.FUNCTION; _ } -> ignore (sc_next sc)
                  | _ -> ());
                 let proc_name_opt =
                   match sc_peek sc with
                   | Some { tok = Parser.IDENT nm; _ } -> ignore (sc_next sc); Some nm
                   | _ -> None
                 in
                 ignore (parse_formal_params sc);
                 ignore (parse_type_spec sc);
                 skip_use_attrs sc;
                 (match sc_peek sc with
                  | Some { tok = Parser.SEMI; _ } -> ignore (sc_next sc)
                  | _ -> ());
                 (match proc_name_opt, sc_peek sc with
                  | Some pn, Some { tok = Parser.BEGIN; _ } ->
                      scan_proc_body pn;
                      (match sc_peek sc with
                       | Some { tok = Parser.IDENT _; _ } -> ignore (sc_next sc)
                       | _ -> ());
                      while (match sc_peek sc with Some { tok = Parser.SEMI; _ } -> true | _ -> false) do
                        ignore (sc_next sc)
                      done;
                      scan_global ()
                  | _ -> scan_global ())
             | _ ->
                 ignore (sc_next sc);
                 scan_global ())
        | Parser.IDENT nm ->
            (match sc_peek_n sc 1 with
             | Some { tok = Parser.EQUAL; _ } ->
                 if not (has_symbol ~proc_name_opt:None nm) then
                   diags := undefined_diag ~span:(tok_span t) ~msg:("Undefined assignment target: " ^ nm) :: !diags
             | Some { tok = Parser.LPAREN; _ } ->
                 if not (has_symbol ~proc_name_opt:None nm) then
                   diags := undefined_diag ~span:(tok_span t) ~msg:("Undefined call target: " ^ nm) :: !diags
             | _ -> ());
            ignore (sc_next sc);
            (match sc_peek sc with
             | Some { tok = Parser.LPAREN; _ } -> scan_value_idents ~proc_name_opt:None
             | _ -> ());
            scan_global ()
        | _ ->
            ignore (sc_next sc);
            scan_global ())
  in

  scan_global ();
  List.rev !diags

(* ---------- Structural heuristics (multi-error-ish) ---------- *)

let structural_diags (toks : tok array) : T.Diagnostic.t list =
  let diags = ref [] in

  let begin_stack : tok list ref = ref [] in
  let paren_stack : tok list ref = ref [] in

  let push st t = st := t :: !st in
  let pop st =
    match !st with
    | [] -> None
    | x :: tl -> st := tl; Some x
  in

  let add_unmatched (t : tok) (what : string) =
    diags :=
      mk_diag
        ~range:(range_of_span t.span)
        ~message:("Unmatched " ^ what)
        ()
      :: !diags
  in

  Array.iter
    (fun t ->
      match t.tok with
      | Parser.BEGIN -> push begin_stack t
      | Parser.END ->
          (match pop begin_stack with
           | Some _ -> ()
           | None -> add_unmatched t "END")
      | Parser.LPAREN -> push paren_stack t
      | Parser.RPAREN ->
          (match pop paren_stack with
           | Some _ -> ()
           | None -> add_unmatched t "')'")
      | _ -> ())
    toks;

  let add_missing_close (t : tok) (what : string) =
    diags :=
      mk_diag
        ~range:(range_of_span t.span)
        ~message:("Missing closing " ^ what)
        ()
      :: !diags
  in

  let rec drain stack what =
    match pop stack with
    | None -> ()
    | Some t ->
        add_missing_close t what;
        drain stack what
  in

  drain begin_stack "END";
  drain paren_stack "')'";

  List.rev !diags

(* ---------- Public parse/analyze ---------- *)

let parse (uri : T.DocumentUri.t) (text : string)
  : (Ast.module_, T.Diagnostic.t list) result =
  let lexbuf = Lexing.from_string text in
  lexbuf_init ~fname:(Lsp.Uri.to_string uri) lexbuf;
  try
    Ok (Parser.compilation_unit Lexer.token lexbuf)
  with
  | Parser.Error ->
      let range = Lsp_convert.range_of_lexbuf lexbuf in
      Error [ mk_diag ~range ~message:"Syntax error." () ]
  | Lexer.Lexing_error msg ->
      let range = Lsp_convert.range_of_lexbuf lexbuf in
      Error [ mk_diag ~range ~message:("Lexing error: " ^ msg) () ]
  | exn ->
      let range = Lsp_convert.range_of_lexbuf lexbuf in
      Error [ mk_diag ~range ~message:("Parse/lex failure: " ^ Printexc.to_string exn) () ]

let analyze ~(uri : T.DocumentUri.t) ~(text : string) : t =
  (* Parse is optional: don't let an evolving grammar ruin UX. *)
  let (ast_opt, parse_diags) =
    match parse uri text with
    | Ok ast -> (Some ast, [])
    | Error _ds ->
        let range =
          T.Range.create
            ~start:(T.Position.create ~line:0 ~character:0)
            ~end_:(T.Position.create ~line:0 ~character:0)
        in
        (None,
         [ mk_diag
             ~severity:T.DiagnosticSeverity.Warning
             ~range
             ~message:"Parser could not fully parse this file; using token-based analysis."
             () ])
  in

  match lex_all uri text with
  | Error lex_diags ->
      { ast = ast_opt
      ; symbols = Symbols.create ()
      ; diagnostics = parse_diags @ lex_diags
      }
  | Ok toks ->
      let (symbols, sym_diags) = build_symbols toks in
      let sem_diags = semantic_checks toks symbols in
      let struct_diags = structural_diags toks in
      { ast = ast_opt
      ; symbols
      ; diagnostics = parse_diags @ sym_diags @ sem_diags @ struct_diags
      }