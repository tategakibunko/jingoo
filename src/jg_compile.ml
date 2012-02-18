(*
  jg_compile.ml

  Copyright (c) 2011- by Masaki WATANABE

  Licence: see LICENCE
*)
open Jg_types
open Jg_utils
open Jg_runtime

type macro_ast = MacroAst of (string list) * ((string * expression) list) * (statement list)

type compile_context = {
  template_dirs: string list;
  macros: (string * macro_ast) list;
}

let escape_text str =
  let str = Pcre.qreplace ~rex:(Pcre.regexp "\n") ~templ:"\\n" str in
  let str = Pcre.qreplace ~rex:(Pcre.regexp "\"") ~templ:"\\\"" str in
  str
;;

let lines lst = 
  String.concat "\n" lst
;;

let ctx_line code =
  spf "let ctx = %s in" code
;;

let let_line name value =
  spf "let %s = %s in" name value
;;

let rec code_of_tvalue = function
  | Tnull -> "Tnull"
  | Tint(x) -> spf "(Tint %d)" x
  | Tbool(x) -> spf "(Tbool %s)" (string_of_bool x)
  | Tfloat(x) -> spf "(Tfloat %f)" x
  | Tstr(x) -> spf "(Tstr \"%s\")" x
  | Tobj(x) -> "(Tobj)"
  | Tlist(x) -> "(Tlist)"
  | Tset(x) -> "(Tset)"
  | Tfun(f) -> "(Tfun)"

and code_of_expr ctx = function
  | LiteralExpr(x) -> code_of_tvalue x
  | IdentExpr(name) -> spf "(jg_get_value ctx \"%s\")" name
  | NotOpExpr(expr) -> spf "(jg_not %s)" (code_of_expr ctx expr)
  | NegativeOpExpr(expr) ->spf "(jg_negative %s)" (code_of_expr ctx expr)
  | PlusOpExpr(left, right) -> spf "(jg_plus %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | MinusOpExpr(left, right) -> spf "(jg_minus %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | TimesOpExpr(left, right) -> spf "(jg_times %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | PowerOpExpr(left, right) -> spf "(jg_power %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | DivOpExpr(left, right) -> spf "(jg_div %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | ModOpExpr(left, right) -> spf "(jg_mod %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | AndOpExpr(left, right) -> spf "(jg_and %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | OrOpExpr(left, right) -> spf "(jg_or %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | EqEqOpExpr(left, right) -> spf "(jg_eq_eq %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | NotEqOpExpr(left, right) -> spf "(jg_not_eq %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | LtOpExpr(left, right) -> spf "(jg_lt %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | GtOpExpr(left ,right) -> spf "(jg_gt %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | LtEqOpExpr(left, right) -> spf "(jg_lteq %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | GtEqOpExpr(left, right) -> spf "(jg_gteq %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | InOpExpr(left, right) -> spf "(jg_inop %s %s)" (code_of_expr ctx left) (code_of_expr ctx right)
  | ListExpr(expr_list) -> spf "(Tlist [%s])" (args_code_of ctx expr_list)
  | SetExpr(expr_list) -> spf "(Tset [%s])" (args_code_of ctx expr_list)
  | DotExpr(IdentExpr(name), IdentExpr(prop)) -> spf "(jg_obj_lookup ctx \"%s\" \"%s\")" name prop
  | TestOpExpr(IdentExpr(name), IdentExpr("defined")) -> spf "(jg_test_defined ctx \"%s\")" name
  | TestOpExpr(IdentExpr(name), IdentExpr("undefined")) -> spf "(jg_test_undefined ctx \"%s\")" name
  | TestOpExpr(DotExpr(IdentExpr(name), IdentExpr(prop)), IdentExpr("defined")) -> spf "(jg_test_obj_defined ctx \"%s\" \"%s\")" name prop
  | TestOpExpr(DotExpr(IdentExpr(name), IdentExpr(prop)), IdentExpr("undefined")) -> spf "(jg_test_obj_undefined ctx \"%s\" \"%s\")" name prop
  | TestOpExpr(IdentExpr(name), IdentExpr("none")) -> spf "(jg_test_none ctx \"%s\")" name
  | TestOpExpr(IdentExpr(name), IdentExpr("escaped")) -> "(jg_test_escaped ctx)"
  | TestOpExpr(target, test) -> spf "(jg_apply (%s) [%s])" (code_of_expr ctx test) (nargs_code_of ctx [target])

  | ObjExpr(expr_list) ->
    let nv_list = List.map (function
      | IdentExpr(name), expr -> spf "(\"%s\", %s)" name (code_of_expr ctx expr)
      | LiteralExpr(Tstr(name)), expr -> spf "(\"%s\", %s)" name (code_of_expr ctx expr)
      | other, expr -> failwith "invalid object syntax"
    ) expr_list in
    spf "(Tobj [%s])" (String.concat ";" nv_list)

  | ApplyExpr(expr, args) ->
    let name = apply_name_of expr in
    let nargs = nargs_code_of ctx args in
    let kwargs = kwargs_code_of ctx args in
    let callable = code_of_expr ctx expr in
    lines [
      "(";
      let_line "callable" callable;
      let_line "name" (spf "\"%s\"" name);
      let_line "nargs" (spf "[%s]" nargs);
      let_line "kwargs" (spf "[%s]" kwargs);
      "(match callable with";
      "| Tfun fn ->";
      "(match nargs with";
      "| [] -> Tfun (fun args _ -> fn args kwargs)";
      "| _ -> jg_apply ~name callable nargs ~kwargs)";
      "| _ ->";
      "(";
      lines [
	(match find_macro ctx name with
	  | Some macro -> eval_macro ctx name nargs kwargs macro
	  | _ -> "");
	"Tnull"
      ];
      ")))";
    ]
      
  | _ -> failwith "syntax error: code_of_expr"

and apply_name_of = function
  | IdentExpr(name) -> name
  | DotExpr(IdentExpr(name), IdentExpr(prop)) -> spf "%s.%s" name prop
  | ApplyExpr(expr, args) -> apply_name_of expr
  | _ -> "<lambda>"

and args_code_of ctx args =
  List.map (code_of_expr ctx) args +>
    String.concat ";"

and ident_names_of lst =
  List.filter (function
    | IdentExpr(_) -> true
    | _ -> false) lst +>
    List.map (function
      | IdentExpr(name) -> name
      | _-> failwith "ident_names_of"
    )

and idents_code_of lst =
  List.map (function| IdentExpr(name) -> spf "\"%s\"" name | _-> failwith "invalid id list") lst +>
    String.concat ";"

and nargs_code_of ctx args =
  List.filter (function KeywordExpr(_,_) -> false | _ -> true) args +>
    List.map (code_of_expr ctx) +>
    String.concat ";"

and kwargs_code_of ctx args = 
  List.filter (function KeywordExpr(_,_) -> true | _ -> false) args +>
    List.map (function
      | KeywordExpr(IdentExpr(name), expr) -> spf "(\"%s\", %s)" name (code_of_expr ctx expr)
      | _ -> failwith "invalid keyword args found") +>
    String.concat ";"

and kwargs_of args =
  List.filter (function KeywordExpr(_,_) -> true | _ -> false) args +>
    List.map (function
      | KeywordExpr(IdentExpr(name), expr) -> (name, expr)
      | _ -> failwith "invalid keyword args found")

and is_safe_expr = function
  | ApplyExpr(IdentExpr("safe"), _) -> true
  | ApplyExpr(expr, _) -> is_safe_expr expr
  | _ -> false

and code_of_statement ctx = function
  | TextStatement(text) ->
    ctx_line @@ spf "jg_output ctx (Tstr \"%s\") ~safe:true" (escape_text text)

  | ExpandStatement(expr) ->
    lines [
      let_line "ret" @@ code_of_expr ctx expr;
      ctx_line @@ spf "jg_output ctx ret ~autoescape:env.autoescape ~safe:%b" (is_safe_expr expr);
    ]
      
  | SetStatement(SetExpr(ident_list), expr) ->
    ctx_line @@ spf "jg_bind_names ctx [%s] %s" (idents_code_of ident_list) (code_of_expr ctx expr)

  | IfStatement((if_expr, body) :: elseif_branches, false_body) ->
    let if_branch label (expr, body) =
      spf "%s jg_is_true %s then begin %s ctx end" label (code_of_expr ctx expr) (lines @@ List.map (code_of_statement ctx) body) in
    ctx_line @@ lines [
      if_branch "if" (if_expr, body);
      lines @@ List.map (if_branch "else if") elseif_branches;
      (match false_body with
	| [] -> "else ctx"
	| _ -> spf "else begin %s ctx end" (lines @@ List.map (code_of_statement ctx) false_body)
      );
    ]

  | ForStatement(iterator, list_expr, statements) ->
    let iterator =
      match iterator with
	| IdentExpr(name) -> spf "\"%s\"" name
	| SetExpr(lst) -> String.concat ";" @@ List.map (fun s -> spf "\"%s\"" s) @@ ident_names_of lst
	| _ -> failwith "invalid iterator" in
    ctx_line @@ lines [
      spf "jg_iter ctx [%s] (fun ctx ->" iterator;
      lines @@ List.map (code_of_statement ctx) statements;
      "ctx";
      spf ") %s" (code_of_expr ctx list_expr);
    ]

  | BlockStatement(IdentExpr(name), statements) ->
    lines @@ List.map (code_of_statement ctx) statements

  | CallStatement(IdentExpr(macro_name), call_args_def, macro_args, call_body) ->
    (match find_macro ctx macro_name with
      | Some macro ->
	let call_arg_names = ident_names_of call_args_def in
	let call_arg_defaults = kwargs_of call_args_def in (* fetch as (string, expression) list *)
	let caller = MacroAst(call_arg_names, call_arg_defaults, call_body) in
	let ctx = push_macro ctx "caller" caller in
	let nargs = nargs_code_of ctx macro_args in
	let kwargs = kwargs_code_of ctx macro_args in
	eval_macro ctx macro_name nargs kwargs macro
      | None -> "")

  | IncludeStatement(path, true) ->
    lines @@ List.map (code_of_statement ctx) @@ statements_from_file ctx path

  | IncludeStatement(path, false) ->
    lines [
      "let ctx' = ctx in";
      "let ctx = jg_init_context env in";
      lines @@ List.map (code_of_statement ctx) @@ statements_from_file ctx path;
      "ctx'";
    ]

  | FilterStatement(IdentExpr(name), statements) ->
    lines @@ [
      ctx_line @@ spf "jg_set_filter ctx \"%s\"" name;
      lines @@ List.map (code_of_statement ctx) statements;
      ctx_line @@ spf "jg_pop_filter ctx";
    ]

  | WithStatement(binds, statements) ->
    let kwargs = kwargs_of binds in
    let names_code = String.concat ";" @@ List.map (fun (n,v) -> spf "\"%s\"" n) kwargs in
    let values_code = args_code_of ctx @@ List.map snd kwargs in

    lines [
      ctx_line "jg_push_frame ctx";
      ctx_line @@ spf "jg_set_values ctx [%s] [%s]" names_code values_code;
      lines @@ List.map (code_of_statement ctx) statements;
      ctx_line "jg_pop_frame ctx";
    ]

  | AutoEscapeStatement(expr, statements) ->
    lines [
      let_line "enable" (code_of_expr ctx expr);
      ctx_line @@ lines [
	"if unbox_bool enable then";
	"jg_set_filter ctx \"escape\"";
	"else";
	"jg_set_filter ctx \"safe\"";
      ];
      lines @@ List.map (code_of_statement ctx) statements;
      ctx_line "jg_pop_filter ctx";
    ]

  | _ -> ""

and unfold_extends ctx codes =
  let rec iter ret = function
    | ExtendsStatement(path) :: rest ->
      let statements = unfold_extends ctx @@ statements_from_file ctx path in
      iter (ret @ statements) rest
    | other :: rest -> iter (ret @ [other]) rest
    | [] -> ret in
  iter [] codes

and align_block codes =
  let is_same_block name = function
    | BlockStatement(IdentExpr(name'), _) -> name = name'
    | _ -> false in
  let erase_block name lst =
    List.filter (fun x ->
      not (is_same_block name x)
    ) lst in
  let rec iter ret = function
    | (BlockStatement(IdentExpr(name), _) as block) :: rest ->
      (try
	 let block' = List.find (is_same_block name) rest in
	 iter (block' :: ret) (erase_block name rest)
       with
	   Not_found -> iter (block :: ret) rest)
    | other :: rest -> iter (other :: ret) rest
    | [] -> List.rev ret in
  iter [] codes

and macro_constructor_of ctx = function
  | MacroAst(arg_names, defaults, statements) ->
    let arg_names = String.concat ";" @@ List.map (fun name ->
      spf "\"%s\"" name
    ) arg_names in
    let defaults = String.concat ";" @@ List.map (fun (name, expr) ->
      spf "(\"%s\", %s)" name (code_of_expr ctx expr)
    ) defaults in
    spf "Macro([%s], [%s], [])" arg_names defaults

and eval_macro ctx name args kwargs macro =
  match macro with
    | MacroAst(_, _, statements) ->
      let macro_code = macro_constructor_of ctx macro in
      let caller = match find_macro ctx "caller" with None -> false | _ -> true in
      lines [
	"let f ctx _ = ";
	lines @@ List.map (code_of_statement ctx) statements;
	"ctx in";
	spf "let _ = jg_eval_macro env ctx \"%s\" [%s] [%s] (%s) f ~caller:%b in" name args kwargs macro_code caller;
      ]

and import_macro ?(alias=None) ?(select=None) ctx codes =
  let macro_name name = match alias with Some alias -> spf "%s.%s" alias name | _ -> name in
  let can_import name =  match select with None -> true | Some lst -> List.exists ((=) name) lst in

  List.fold_left (fun ctx code ->
    match code with
      | MacroStatement(IdentExpr(name), args_def, statements) when can_import name ->
	let arg_names = ident_names_of args_def in
	let defaults = kwargs_of args_def in
	push_macro ctx (macro_name name) @@ MacroAst(arg_names, defaults, statements)

      | BlockStatement(_, stmts) ->
	import_macro ctx ~alias ~select stmts
	  
      | IncludeStatement(path, _) ->
	import_macro ctx ~alias ~select @@ statements_from_file ctx path

      | ImportStatement(path, alias) ->
	import_macro ctx ~alias ~select @@ statements_from_file ctx path
	  
      | FromImportStatement(path, import_macros) ->
	let names = ident_names_of import_macros in
	import_macro ctx ~select:(Some names) @@ statements_from_file ctx path
	  
      | _ -> ctx
  ) ctx codes

and find_macro ctx name =
  try Some (List.assoc name ctx.macros) with Not_found -> None

and push_macro ctx name macro =
  {ctx with macros = (name, macro) :: ctx.macros}

and statements_from_file ctx file_name =
  let file_path = Jg_utils.get_file_path file_name ~template_dirs:ctx.template_dirs in
  let source = Jg_utils.read_file_as_string file_path in
  let lexbuf = Lexing.from_string source in
  Jg_lexer.init_lexer_pos (Some file_name) lexbuf;
  try
    let lexer = Jg_lexer.gen_cached_lexer Jg_lexer.main in
    Jg_parser.input lexer lexbuf
  with
      exn ->
	raise @@ SyntaxError(Jg_utils.get_parser_error exn lexbuf)

and from_file ?(template_dirs=[]) file_name =
  let ctx = {template_dirs = template_dirs; macros = []} in
  let codes = statements_from_file ctx file_name +> (unfold_extends ctx) +> align_block in
  let ctx = import_macro ctx codes in
  let stub_key = Jg_utils.get_file_path file_name ~template_dirs in
  lines [
    "open Jg_utils";
    "open Jg_types";
    "open Jg_runtime";
    "let render ?(env=std_env) ?(models=[]) file_name =";
    "let () = jg_load_extensions env.extensions in";
    "let ctx = jg_init_context ~models env in";
    lines @@ List.map (code_of_statement ctx) codes;
    "Buffer.contents ctx.buffer\n;;\n";
    spf "let () = Jg_stub.add_tmpl_func \"%s\" render\n;;\n" stub_key;
  ]
;;
