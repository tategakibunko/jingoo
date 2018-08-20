(*
  jg_interp.ml

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)
open Jg_types
open Jg_utils
open Jg_runtime

let rec filter_map fn = function
  | [] -> []
  | hd :: tl ->
    match fn hd with
    | Some x -> x :: filter_map fn tl
    | None -> filter_map fn tl

let rec value_of_expr env ctx = function
  | LiteralExpr(x) -> x
  | IdentExpr(name) -> jg_get_value ctx name
  | NotOpExpr(expr) -> jg_not @@ value_of_expr env ctx expr
  | NegativeOpExpr(expr) -> jg_negative @@ value_of_expr env ctx expr
  | PlusOpExpr(left, right) -> jg_plus (value_of_expr env ctx left) (value_of_expr env ctx right)
  | MinusOpExpr(left, right) -> jg_minus (value_of_expr env ctx left) (value_of_expr env ctx right)
  | TimesOpExpr(left, right) -> jg_times (value_of_expr env ctx left) (value_of_expr env ctx right)
  | PowerOpExpr(left, right) -> jg_power (value_of_expr env ctx left) (value_of_expr env ctx right)
  | DivOpExpr(left, right) -> jg_div (value_of_expr env ctx left) (value_of_expr env ctx right)
  | ModOpExpr(left, right) -> jg_mod (value_of_expr env ctx left) (value_of_expr env ctx right)
  | AndOpExpr(left, right) ->
    Tbool (jg_is_true (value_of_expr env ctx left) && jg_is_true (value_of_expr env ctx right))
  | OrOpExpr(left, right) ->
    Tbool (jg_is_true (value_of_expr env ctx left) || jg_is_true (value_of_expr env ctx right))
  | EqEqOpExpr(left, right) -> jg_eq_eq (value_of_expr env ctx left) (value_of_expr env ctx right)
  | NotEqOpExpr(left, right) -> jg_not_eq (value_of_expr env ctx left) (value_of_expr env ctx right)
  | LtOpExpr(left, right) -> jg_lt (value_of_expr env ctx left) (value_of_expr env ctx right)
  | GtOpExpr(left ,right) -> jg_gt (value_of_expr env ctx left) (value_of_expr env ctx right)
  | LtEqOpExpr(left, right) -> jg_lteq (value_of_expr env ctx left) (value_of_expr env ctx right)
  | GtEqOpExpr(left, right) -> jg_gteq (value_of_expr env ctx left) (value_of_expr env ctx right)
  | InOpExpr(left, right) -> jg_inop (value_of_expr env ctx left) (value_of_expr env ctx right)
  | ListExpr(expr_list) -> Tlist (List.map (value_of_expr env ctx) expr_list)
  | SetExpr(expr_list) -> Tset (List.map (value_of_expr env ctx) expr_list)
  | DotExpr(IdentExpr(name), prop) -> jg_obj_lookup_by_name ctx name prop
  | DotExpr(left, prop) -> jg_obj_lookup (value_of_expr env ctx left) prop
  | BracketExpr(left, expr) ->
    (match value_of_expr env ctx expr with
     | Tstr prop -> jg_obj_lookup (value_of_expr env ctx left) prop
     | Tint i -> jg_nth (value_of_expr env ctx left) i
     | _ -> Tnull)
  | TestOpExpr(IdentExpr(name), IdentExpr("defined")) -> jg_test_defined ctx name
  | TestOpExpr(IdentExpr(name), IdentExpr("undefined")) -> jg_test_undefined ctx name
  | TestOpExpr(DotExpr(IdentExpr(name), prop), IdentExpr("defined")) -> jg_test_obj_defined ctx name prop
  | TestOpExpr(DotExpr(IdentExpr(name), prop), IdentExpr("undefined")) -> jg_test_obj_undefined ctx name prop
  | TestOpExpr(BracketExpr(IdentExpr(name), expr), IdentExpr("defined")) ->
    (match value_of_expr env ctx expr with
     | Tstr prop -> jg_test_obj_defined ctx name prop
     | _ -> Tbool false)
  | TestOpExpr(BracketExpr(IdentExpr(name), expr), IdentExpr("undefined")) ->
    (match value_of_expr env ctx expr with
     | Tstr prop -> jg_test_obj_undefined ctx name prop
     | _ -> Tbool true)
  | TestOpExpr(IdentExpr(name), IdentExpr("none")) -> jg_test_none ctx name
  | TestOpExpr(IdentExpr(name), IdentExpr("escaped")) -> jg_test_escaped ctx
  | TestOpExpr(IdentExpr(name), IdentExpr("upper")) -> jg_test_upper (jg_get_value ctx name) []
  | TestOpExpr(IdentExpr(name), IdentExpr("lower")) -> jg_test_lower (jg_get_value ctx name) []
  | TestOpExpr(target, test) -> jg_apply (value_of_expr env ctx test) [value_of_expr env ctx target]

  | ObjExpr(expr_list) ->
    Tobj (List.map (function
      | IdentExpr(name), expr -> (name, value_of_expr env ctx expr)
      | LiteralExpr(Tstr(name)), expr -> (name, value_of_expr env ctx expr)
      | other, expr -> failwith "invalid object syntax"
    ) expr_list)

  | ApplyExpr(IdentExpr("eval"), [expr]) ->
    let buffer = Buffer.create 256 in
    let ctx = {ctx with output = Buffer.add_string buffer } in
    let str = string_of_tvalue @@ value_of_expr env ctx expr in
    let ast = ast_from_string ~env str in
    let _ = List.fold_left (eval_statement env) ctx ast in
    Tstr (Buffer.contents buffer)

  | ApplyExpr(expr, args) ->
    let name = apply_name_of expr in
    let nargs = nargs_of env ctx args in
    let kwargs = kwargs_of env ctx args in
    let callable = value_of_expr env ctx expr in
    (match callable with
     | Tfun fn ->
       (match nargs with
	| [] -> Tfun (fun args _ -> fn args kwargs)
	| _ -> jg_apply callable nargs ~kwargs ~name)
     | _ ->
       (match jg_get_macro ctx name with
	| Some macro -> ignore @@ eval_macro env ctx name nargs kwargs macro; Tnull
	| None -> Tnull))

  | expr -> failwith @@ spf "syntax error: value_of_expr:%s" (dump_expr expr)

and apply_name_of = function
  | IdentExpr(name) -> name
  | DotExpr(IdentExpr(name), prop) -> spf "%s.%s" name prop
  | ApplyExpr(expr, args) -> apply_name_of expr
  | _ -> "<lambda>"

and ident_names_of lst =
  filter_map (function
  | IdentExpr name -> Some name
  | _ -> None) lst

and alias_names_of lst =
  List.map (function
  | IdentExpr(name) -> (name, name)
  | AliasExpr(IdentExpr(name), IdentExpr(name')) -> (name, name')
  | _ -> failwith "invalid argument:alias_names_of") lst

and nargs_of env ctx args =
  filter_map (function
  | KeywordExpr _ -> None
  | x -> Some (value_of_expr env ctx x)) args

and kwargs_of env ctx args =
  filter_map (function
  | KeywordExpr(IdentExpr(name), expr) -> Some (name, value_of_expr env ctx expr)
  | _ -> None) args

and eval_macro env ctx name args kwargs macro =
  let caller = match jg_get_macro ctx "caller" with None -> false | _ -> true in
  jg_eval_macro env ctx name args kwargs macro ~caller:caller (fun ctx ast ->
    List.fold_left (eval_statement env) ctx ast
  )

and is_safe_expr = function
  | ApplyExpr(IdentExpr("safe"), _) -> true
  | ApplyExpr(expr, _) -> is_safe_expr expr
  | _ -> false

and eval_statement env ctx = function
  | Statements ast ->
    List.fold_left (eval_statement env) ctx ast

  | TextStatement(text) ->
    jg_output ctx (Tstr text) ~safe:true

  | ExpandStatement(expr) ->
    jg_output ctx (value_of_expr env ctx expr) ~autoescape:env.autoescape ~safe:(is_safe_expr expr)

  | SetStatement(SetExpr(ident_list), expr) ->
    jg_bind_names ctx (ident_names_of ident_list) (value_of_expr env ctx expr)

  | SetStatement(DotExpr(IdentExpr ns, v), expr) ->
    Hashtbl.add
      (Hashtbl.find ctx.namespace_table ns) v (value_of_expr env ctx expr) ;
    ctx

  | FilterStatement(IdentExpr(name), ast) ->
    let ctx = jg_set_filter ctx name in
    let ctx = List.fold_left (eval_statement env) ctx ast in
    jg_pop_filter ctx

  | IfStatement(if_elseif_conds, false_ast) ->
    let rec select_case = function
      | (expr, ast) :: rest when jg_is_true (value_of_expr env ctx expr) -> ast
      | h :: rest -> select_case rest
      | [] -> false_ast in
    List.fold_left (eval_statement env) ctx @@ select_case if_elseif_conds

  | ForStatement(iterator, iterable_expr, ast) ->
    let iterator =
      match iterator with
      | IdentExpr(name) -> [name]
      | SetExpr(lst) -> ident_names_of lst
      | _ -> failwith "invalid iterator" in
    let iterable = value_of_expr env ctx iterable_expr in
    let is_iterable = Jg_runtime.is_iterable iterable in
    (* [ISSUE#23] when strict_mode is enabled, raises error if loop target is not iterable. *)
    if env.strict_mode = true && is_iterable = false then
      failwith @@ spf "%s is not iterable" (string_of_tvalue iterable)
    ;
    jg_iter ctx iterator (fun ctx ->
      ignore @@ List.fold_left (eval_statement env) ctx ast
    ) iterable;
    ctx

  | BlockStatement(_, ast) ->
    List.fold_left (eval_statement env) ctx ast

  | CallStatement(IdentExpr(name), call_args_def, macro_args, call_ast) ->
    (match jg_get_macro ctx name with
     | Some(Macro(arg_names, def_kwargs, macro_ast)) ->
       let call_arg_names = ident_names_of call_args_def in
       let call_defaults = kwargs_of env ctx call_args_def in
       jg_set_macro ctx "caller" @@ Macro(call_arg_names, call_defaults, call_ast) ;
       let text = string_of_tvalue @@ value_of_expr env ctx @@ ApplyExpr(IdentExpr(name), macro_args) in
       jg_remove_macro ctx "caller" ;
       jg_output ctx (Tstr text) ~safe:true

     | None -> ctx (* do nothing *)
    )

  | IncludeStatement(IdentExpr(name), with_context) ->
    eval_statement env ctx @@ IncludeStatement(LiteralExpr(jg_get_value ctx name), with_context)

  | IncludeStatement(LiteralExpr(Tstr path), true) ->
    let ast = ast_from_file ~env path in
    List.fold_left (eval_statement env) ctx ast

  | IncludeStatement(LiteralExpr(Tstr path), false) ->
    let ast = ast_from_file ~env path in
    let ctx' = jg_init_context ctx.output env in
    let _ = List.fold_left (eval_statement env) ctx' ast in
    ctx

  | RawIncludeStatement(IdentExpr(name)) ->
    eval_statement env ctx @@ RawIncludeStatement(LiteralExpr(jg_get_value ctx name))

  | RawIncludeStatement(LiteralExpr(Tstr path)) ->
    let file_path = get_file_path env path in
    let source = Jg_utils.read_file_as_string file_path in
    jg_output ctx (Tstr source) ~safe:true

  | WithStatement(binds, ast) ->
    let kwargs = kwargs_of env ctx binds in
    let names, values = List.split kwargs in
    let ctx = jg_push_frame ctx in
    let ctx = jg_set_values ctx names values in
    let ctx = List.fold_left (eval_statement env) ctx ast in
    jg_pop_frame ctx

  | AutoEscapeStatement(expr, ast) ->
    let ctx =
      match value_of_expr env ctx expr with
      | Tbool true ->
	jg_set_filter ctx "escape"
      | Tbool false ->
	jg_set_filter ctx "safe"
      | _ ->
	failwith "invalid syntax:autoescape(bool value required)" in
    let ctx = List.fold_left (eval_statement env) ctx ast in
    jg_pop_filter ctx

  | NamespaceStatement (ns, assign) ->
    let size = match List.length assign with 0 -> 10 | x -> x in
    let h = Hashtbl.create size in
    List.iter (fun (k, v) -> Hashtbl.add h k (value_of_expr env ctx v)) assign;
    Hashtbl.add ctx.namespace_table ns h;
    ctx

  | _ -> ctx

and unfold_extends env stmts =
  let open Jg_ast_mapper in
  let statement self = function
    | ExtendsStatement path ->
      Statements (self.ast self @@ ast_from_file env path)
    | e -> default_mapper.statement self e in
  let mapper = { default_mapper with statement } in
  mapper.ast mapper stmts

and replace_blocks stmts =
  let open Jg_ast_mapper in
  let h = Hashtbl.create 10 in
  let stmts =
    let statement self = function
      | BlockStatement (IdentExpr name, ast) ->
        Hashtbl.add h name ast ;
        BlockStatement (IdentExpr name, self.ast self ast)
      | e -> default_mapper.statement self e in
    let mapper = { default_mapper with statement } in
    mapper.ast mapper stmts in
  if Hashtbl.length h = 0 then stmts
  else
    let h' = Hashtbl.create 10 in
    let statement self = function
      | BlockStatement (IdentExpr name, _) ->
        let stmts =
          if Hashtbl.mem h' name then []
          else
            let () = Hashtbl.add h' name true in
            self.ast self @@ Hashtbl.find h name in
        Statements stmts
      | e -> default_mapper.statement self e in
    let mapper = { default_mapper with statement } in
    mapper.ast mapper stmts

and inline_include env stmts =
  let open Jg_ast_mapper in
  let statement self = function
    | IncludeStatement (LiteralExpr (Tstr file), true) ->
      Statements (self.ast self @@ ast_from_file ~env file)
    | RawIncludeStatement (LiteralExpr (Tstr file)) ->
      Statements (self.ast self @@ ast_from_file ~env file)
    | e -> default_mapper.statement self e in
  let mapper = { default_mapper with statement } in
  mapper.ast mapper stmts

(* Import macros into ctx and remove it from ast *)
and import_macros env ctx stmts =
  let open Jg_ast_mapper in
  let select = ref None in
  let namespace = ref None in
  let macro_name name = match !namespace with Some namespace -> spf "%s.%s" namespace name | _ -> name in
  let alias_name name = match !select with None -> name | Some alist -> List.assoc name alist in
  let can_import name = match !select with None -> true | Some alist -> List.mem_assoc name alist in
  let statement self = function

    | MacroStatement(IdentExpr(name), def_args, ast) when can_import name ->
      let arg_names = ident_names_of def_args in
      let kwargs = kwargs_of env ctx def_args in
      jg_set_macro ctx (macro_name @@ alias_name name) @@ Macro(arg_names, kwargs, ast);
      Statements []

    | IncludeStatement(LiteralExpr(Tstr path), _) as stmt ->
      ignore @@ self.ast self @@ ast_from_file env path;
      stmt

    | ImportStatement(path, namespace') ->
      let old_namespace = !namespace in
      let () = namespace := namespace' in
      ignore @@ self.ast self @@ ast_from_file env path;
      let () = namespace := old_namespace in
      Statements []

    | FromImportStatement(path, select_macros) ->
      let alias_names = alias_names_of select_macros in
      let old_select = !select in
      let () = select := Some alias_names in
      ignore @@ self.ast self @@ ast_from_file env path;
      let () = select := old_select in
      Statements []

    | s -> default_mapper.statement self s in
  let mapper = { default_mapper with statement } in
  mapper.ast mapper stmts

and get_file_path env file_name =
  Jg_utils.get_file_path file_name ~template_dirs:env.template_dirs

and init_context ?(env=std_env) ?(models=[]) ~output () =
  let extensions = env.extensions in
  jg_load_extensions extensions;
  jg_init_context ~models output env

and ast_from_lexbuf ~env filename lexbuf =
  Jg_utils.with_lock (fun () ->
    Parsing.clear_parser ();
    Jg_lexer.reset_context ();
    Jg_lexer.init_lexer_pos filename lexbuf;
    let ast = Jg_parser.input Jg_lexer.main lexbuf in
    ast
  ) ~on_error:(fun () -> Parsing.clear_parser ())

and ast_from_file ~env filename =
  let filename = get_file_path env filename in
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  let ast = ast_from_lexbuf ~env (Some filename) lexbuf in
  close_in ch;
  ast

and ast_from_string ~env string =
  let lexbuf = Lexing.from_string string in
  ast_from_lexbuf ~env None lexbuf

and eval_aux ~env ~ctx ast =
  let ast =
    unfold_extends env ast
    |> replace_blocks
    |> import_macros env ctx in
  ignore @@ List.fold_left (eval_statement env) ctx ast

and from_file
    ?(env=std_env) ?(models=[]) ~output
    ?(ctx = init_context ~env ~models ~output ())
    file_name =
  eval_aux ~env ~ctx @@
  ast_from_file ~env file_name

and from_string ?(env=std_env) ?(models=[]) ?file_path ~output
    ?(ctx = init_context ~env ~models ~output ())
    source =
  eval_aux ~env ~ctx @@
  ast_from_string ~env source
