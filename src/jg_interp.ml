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
     | Tint i -> jg_nth_aux (value_of_expr env ctx left) i
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
  | TestOpExpr(IdentExpr(_), IdentExpr("escaped")) -> jg_test_escaped ctx
  | TestOpExpr(IdentExpr(name), IdentExpr("upper")) -> jg_test_upper (jg_get_value ctx name)
  | TestOpExpr(IdentExpr(name), IdentExpr("lower")) -> jg_test_lower (jg_get_value ctx name)
  | TestOpExpr(target, test) -> jg_apply (value_of_expr env ctx test) [value_of_expr env ctx target]

  | ObjExpr(key_values) ->
    Tobj (List.map (fun (k, v) -> (k, value_of_expr env ctx v)) key_values)

  | ApplyExpr(IdentExpr("eval"), [name, expr]) ->
    assert (name = None) ;
    let value = ref Tnull in
    let ctx = {ctx with serialize = true ; output = fun x -> value := x } in
    let str = string_of_tvalue @@ value_of_expr env ctx expr in
    let ast = ast_from_string str in
    let _ = List.fold_left (eval_statement env) ctx ast in
    !value

  | ApplyExpr(IdentExpr("safe"), [name, expr]) ->
     assert (name = None) ;
     value_of_expr env ctx expr

  | ApplyExpr(expr, args) ->
    let name = apply_name_of expr in
    let nargs = nargs_of env ctx args in
    let nargs_fun = if args = [] then [ Tnull ] else nargs in
    let kwargs = kwargs_of_app env ctx args in
    let callable = value_of_expr env ctx expr in
    (match callable with
    | Tfun _ -> jg_apply callable nargs_fun ~kwargs
    | _ ->
      (match jg_get_macro ctx name with
       | Some macro -> ignore @@ eval_macro env ctx name nargs kwargs macro; Tnull
       | None -> Tnull))

  | FunctionExpression (arg_names, body) ->
    func begin fun ?kwargs:_ args ->
      let ctx = jg_frame_table ctx (fun table ->
        List.iter2 (jg_set_value table) arg_names args
      ) in
      value_of_expr env ctx body
    end (List.length arg_names)

  | TernaryOpExpr (c, y, n) ->
    if jg_is_true (value_of_expr env ctx c) then value_of_expr env ctx y else value_of_expr env ctx n

and apply_name_of = function
  | IdentExpr(name) -> name
  | DotExpr(IdentExpr(name), prop) -> spf "%s.%s" name prop
  | ApplyExpr(expr, _) -> apply_name_of expr
  | _ -> "<lambda>"

and ident_names_of =
  filter_map @@ function
  | IdentExpr name -> Some name
  | _ -> None

and ident_names_of_def =
  filter_map @@ function
  | name, None -> Some name
  | _ -> None

and alias_names_of =
  List.map @@ function
  | (name, None) -> (name, name)
  | (name, Some name') -> (name, name')

and nargs_of env ctx =
  filter_map @@ function
  | Some _, _ -> None
  | None, x -> Some (value_of_expr env ctx x)

and kwargs_of_app env ctx =
  filter_map @@ function
  | Some name, expr -> Some (name, value_of_expr env ctx expr)
  | _ -> None

and kwargs_of_def env ctx =
  filter_map @@ function
  | name, Some expr -> Some (name, value_of_expr env ctx expr)
  | _ -> None

and eval_macro env ctx name args kwargs macro =
  let caller = match jg_get_macro ctx "caller" with None -> false | _ -> true in
  jg_eval_macro ctx name args kwargs macro ~caller:caller (fun ctx ast ->
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
    jg_frame_table ctx (fun table ->
      jg_bind_names table (ident_names_of ident_list) (value_of_expr env ctx expr)
    )

  | SetStatement(DotExpr(IdentExpr ns, v), expr) ->
    Hashtbl.replace
      (Hashtbl.find ctx.namespace_table ns) v (value_of_expr env ctx expr) ;
    ctx

  | SetStatement(BracketExpr(IdentExpr ns, k), expr) ->
    Hashtbl.replace
      (Hashtbl.find ctx.namespace_table ns)
      (match value_of_expr env ctx k with Tstr k -> k | _ -> assert false)
      (value_of_expr env ctx expr) ;
    ctx

  | SetBlockStatement(name, ast) ->
    let macro = Macro ([], [], ast) in
    let apply macro =
      let value = ref [] in
      let ctx = { ctx with output = fun x -> value := x :: !value } in
      let ctx = jg_frame_table ctx (fun table ->
        jg_eval_aux table name [] [] macro
      ) in
      let Macro (_, _, code) = macro in
      ignore @@ List.fold_left (eval_statement env) ctx code;
      String.concat "" (List.map string_of_tvalue (List.rev !value))
    in

    jg_frame_table ctx (fun table ->
      jg_set_value table name (Tsafe (apply macro))
    )

  | FilterStatement(name, ast) ->
    let ctx = jg_set_filter ctx name in
    let ctx = List.fold_left (eval_statement env) ctx ast in
    jg_pop_filter ctx

  | IfStatement (branches) ->
    let rec select_case = function
      | (None, ast) :: _ -> ast
      | (Some cond, ast) :: tl ->
        if jg_is_true (value_of_expr env ctx cond) then ast
        else select_case tl
      | [] -> []
    in
    List.fold_left (eval_statement env) ctx @@ select_case branches

  | SwitchStatement (e, cases) ->
    let e = value_of_expr env ctx e in
    let rec select_case = function
      | ([], ast) :: _ -> ast
      | (cond, ast) :: tl ->
        if List.exists (fun x -> jg_eq_eq_aux (value_of_expr env ctx x) e) cond
        then ast
        else select_case tl
      | [] -> []
    in
    List.fold_left (eval_statement env) ctx @@ select_case cases

  | ForStatement(iterator, iterable_expr, ast) ->
    let iterable = value_of_expr env ctx iterable_expr in
    let is_iterable = Jg_runtime.jg_test_iterable_aux iterable in
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

  | CallStatement(name, call_args_def, macro_args, call_ast) ->
    (match jg_get_macro ctx name with
     | Some (Macro _) ->
       let call_arg_names = ident_names_of_def call_args_def in
       let call_defaults = kwargs_of_def env ctx call_args_def in
       jg_set_macro ctx "caller" @@ Macro(call_arg_names, call_defaults, call_ast) ;
       let text = string_of_tvalue @@ value_of_expr env ctx @@ ApplyExpr(IdentExpr(name), macro_args) in
       jg_remove_macro ctx "caller" ;
       jg_output ctx (Tstr text) ~safe:true

     | None -> ctx (* do nothing *)
    )

  | IncludeStatement(e, with_ctx) ->
    begin match value_of_expr env ctx e with
      | Tstr path ->
        if with_ctx then
          let ast = ast_from_file ~env path in
          List.fold_left (eval_statement env) ctx ast
        else
          let ast = ast_from_file ~env path in
          let ctx' = jg_init_context ctx.output env in
          let _ = List.fold_left (eval_statement env) ctx' ast in
          ctx
      | x -> failwith_type_error_1 "Jg_interp:include" x
    end

  | RawIncludeStatement(e) ->
    begin match value_of_expr env ctx e with
      | Tstr path ->
        let file_path = get_file_path env path in
        let source = Jg_utils.read_file_as_string file_path in
        jg_output ctx (Tstr source) ~safe:true
      | x -> failwith_type_error_1 "Jg_interp:rawinclude" x
    end

  | WithStatement(binds, ast) ->
    let names, values = List.split binds in
    let values = List.map (value_of_expr env ctx) values in
    let ctx' = jg_frame_table ctx (fun table ->
      jg_set_values table names values
    ) in
    ignore @@ List.fold_left (eval_statement env) ctx' ast ;
    ctx

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

  | FunctionStatement(name, def_args, ast) ->
    let arg_names = ident_names_of_def def_args in
    let kwargs = kwargs_of_def env ctx def_args in
    let macro = Macro (arg_names, kwargs, ast) in
    let apply ~kwargs args =
      let value = ref Tnull in
      let ctx = { ctx with serialize = true ; output = fun x -> value := x } in
      let ctx = jg_frame_table ctx (fun table ->
        jg_eval_aux table name args kwargs macro
      ) in
      let Macro (_, _, code) = macro in
      ignore @@ List.fold_left (eval_statement env) ctx code;
      !value
    in
    let fn = func (fun ?(kwargs=kwargs) args -> apply ~kwargs args) (List.length arg_names) in
    jg_frame_table ctx (fun table ->
      jg_set_value table name fn
    )

  | _ -> ctx

and unfold_extends env stmts =
  let open Jg_ast_mapper in
  let statement self = function
    | ExtendsStatement path ->
      Statements (self.ast self @@ ast_from_file ~env path)
    | e -> default_mapper.statement self e in
  let mapper = { default_mapper with statement } in
  mapper.ast mapper stmts

and replace_blocks stmts =
  let open Jg_ast_mapper in
  let h = Hashtbl.create 10 in
  let stmts =
    let statement self = function
      | BlockStatement (name, ast) ->
        Hashtbl.add h name ast ;
        BlockStatement (name, self.ast self ast)
      | e -> default_mapper.statement self e in
    let mapper = { default_mapper with statement } in
    mapper.ast mapper stmts in
  if Hashtbl.length h = 0 then stmts
  else
    let h' = Hashtbl.create 10 in
    let statement self = function
      | BlockStatement (name, _) ->
        let stmts =
          if Hashtbl.mem h' name then []
          else
            let () = Hashtbl.add h' name true in
            self.ast self @@ Hashtbl.find h name in
        Statements stmts
      | e -> default_mapper.statement self e in
    let mapper = { default_mapper with statement } in
    mapper.ast mapper stmts

and get_file_path env file_name =
  Jg_utils.get_file_path file_name ~template_dirs:env.template_dirs

and init_context ?(env=std_env) ?(models=(fun _ -> Tnull)) ~output () =
  let extensions = env.extensions in
  jg_load_extensions extensions;
  jg_init_context ~models output env

and ast_from_lexbuf filename lexbuf =
  Parsing.clear_parser ();
  Jg_lexer.reset_context ();
  Jg_lexer.init_lexer_pos filename lexbuf;
  let ast = Jg_parser.input Jg_lexer.main lexbuf in
  ast

and error e lexbuf =
  let curr = lexbuf.Lexing.lex_curr_p in
  let l = curr.Lexing.pos_lnum in
  let c = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
  let t = Lexing.lexeme lexbuf in
  let msg = Printf.sprintf "Error line %d, col %d, token %s (%s)" l c t e in
  raise (SyntaxError msg)

and ast_from_chan filename ch =
  let lexbuf = Lexing.from_channel ch in
  try
    let ast = ast_from_lexbuf filename lexbuf in
    close_in ch;
    ast
  with
  | SyntaxError e ->
    close_in ch ;
    error e lexbuf
  | Jg_parser.Error ->
    close_in ch ;
    error "" lexbuf

and ast_from_file ~env filename =
  let filename = get_file_path env filename in
  let ch = open_in filename in
  ast_from_chan (Some filename) ch

and ast_from_string string =
  let lexbuf = Lexing.from_string string in
  try ast_from_lexbuf None lexbuf
  with SyntaxError e -> error e lexbuf

(* Remove macros from ast and return a list of them. *)
let extract_macros env stmts =
  let open Jg_ast_mapper in
  let select = ref None in
  let namespace = ref None in
  let macros = ref [] in
  let macro_name name = match !namespace with Some namespace -> spf "%s.%s" namespace name | _ -> name in
  let alias_name name = match !select with None -> name | Some alist -> List.assoc name alist in
  let can_import name = match !select with None -> true | Some alist -> List.mem_assoc name alist in
  let statement self = function

    | MacroStatement(name, def_args, ast) when can_import name ->
      macros :=
        (fun ctx ->
           let arg_names = ident_names_of_def def_args in
           let kwargs = kwargs_of_def env ctx def_args in
           (macro_name @@ alias_name name), Macro(arg_names, kwargs, ast))
        :: !macros;
      Statements []

    | IncludeStatement(LiteralExpr(Tstr path), _) as stmt ->
      ignore @@ self.ast self @@ ast_from_file ~env path;
      stmt

    | ImportStatement(path, namespace') ->
      let old_namespace = !namespace in
      let () = namespace := namespace' in
      ignore @@ self.ast self @@ ast_from_file ~env path;
      let () = namespace := old_namespace in
      Statements []

    | FromImportStatement(path, select_macros) ->
      let alias_names = alias_names_of select_macros in
      let old_select = !select in
      let () = select := Some alias_names in
      ignore @@ self.ast self @@ ast_from_file ~env path;
      let () = select := old_select in
      Statements []

    | s -> default_mapper.statement self s in
  let mapper = { default_mapper with statement } in
  let ast = mapper.ast mapper stmts in
  let macros = List.rev !macros in
  ast, macros

module Loaded = struct
  type t = environment * ast * (context -> string * macro) list

  let load_aux ~env ast =
    let ast, macros =
      unfold_extends env ast
      |> replace_blocks
      |> extract_macros env
    in
    env, ast, macros

  let from_file ?(env=std_env) file_name =
    load_aux ~env @@ ast_from_file ~env file_name

  let from_string ?(env=std_env) ?file_path:_ source =
    load_aux ~env @@ ast_from_string source

  let from_chan ?(env=std_env) ?file_path chan =
    load_aux ~env @@ ast_from_chan file_path chan

  let eval_aux (env, ast, macros) ~ctx =
    List.iter (fun f ->
      let macro_name, macro = f ctx in
      jg_set_macro ctx macro_name macro)
      macros;
    ignore @@ List.fold_left (eval_statement env) ctx ast

  let eval
        (env, ast, macros)
        ?(models=(fun _ -> Tnull)) ~output
        ?(ctx = init_context ~env ~models ~output ()) () =
    eval_aux (env, ast, macros) ~ctx
end

let select_context ?(env=std_env) ~models ~output ctx =
  match ctx with
  | Some c -> c
  | None -> init_context ~env ~models ~output ()

let from_file
    ?(env=std_env) ?(models=[]) ~output
    ?ctx
    file_name =
  let ctx = select_context ~env ~models:(fun key -> List.assoc key models) ~output ctx in
  Loaded.eval (Loaded.from_file ~env file_name) ~output ~ctx ()

let from_string ?(env=std_env) ?(models=[]) ?file_path ~output
    ?ctx
    source =
  let ctx = select_context ~env ~models:(fun key -> List.assoc key models) ~output ctx in
  Loaded.eval (Loaded.from_string ~env ?file_path source) ~output ~ctx ()

let from_chan ?(env=std_env) ?(models=[]) ?file_path ~output
    ?ctx
    chan =
  let ctx = select_context ~env ~models:(fun key -> List.assoc key models) ~output ctx in
  Loaded.eval (Loaded.from_chan ~env ?file_path chan) ~output ~ctx ()

(* Reviced version api, models is abstracted as closure(string -> tvalue). *)
module RevicedApi = struct
  let from_file
      ?(env=std_env) ?(models=fun _ -> Tnull) ~output
      ?ctx
      file_name =
    let ctx = select_context ~env ~models ~output ctx in
    Loaded.eval (Loaded.from_file ~env file_name) ~output ~ctx ()

  let from_string ?(env=std_env) ?(models=fun _ -> Tnull) ?file_path ~output
      ?ctx
      source =
    let ctx = select_context ~env ~models ~output ctx in
    Loaded.eval (Loaded.from_string ~env ?file_path source) ~output ~ctx ()

  let from_chan ?(env=std_env) ?(models=fun _ -> Tnull) ?file_path ~output
      ?ctx
      chan =
    let ctx = select_context ~env ~models ~output ctx in
    Loaded.eval (Loaded.from_chan ~env ?file_path chan) ~output ~ctx ()
end
