(*
  jg_types.ml

  Copyright (c) 2011- by Masaki WATANABE

  Licence: see LICENCE
*)

exception SyntaxError of string

type environment = {
  autoescape : bool;
  template_dirs : string list;
  filters : (string * tvalue) list;
  extensions : string list;
  compiled : bool;
}

and context = {
  frame_stack : frame list;
  macro_table : (string * macro) list;
  filter_table : string list;
  buffer : Buffer.t;
}

and frame = (string * tvalue) list
and macro = Macro of macro_arg_names * macro_defaults * macro_code
and macro_arg_names = string list
and macro_defaults = kwargs
and macro_code = statement list
and tvalue =
    Tnull
  | Tint of int
  | Tbool of bool
  | Tfloat of float
  | Tstr of string
  | Tobj of (string * tvalue) list
  | Tlist of tvalue list
  | Tset of tvalue list
  | Tfun of (args -> kwargs -> tvalue)
and args = tvalue list
and kwargs = (string * tvalue) list

and ast = statement list

and statement =
    TextStatement of string
  | ExpandStatement of expression
  | IfStatement of (cond_branch list) * else_statements
  | ForStatement of expression * expression * ast
  | IncludeStatement of string * with_context
  | RawIncludeStatement of string
  | ExtendsStatement of string
  | ImportStatement of string * string option
  | FromImportStatement of string * expression list
  | SetStatement of expression * expression
  | BlockStatement of expression * ast
  | MacroStatement of expression * arguments * ast
  | FilterStatement of expression * ast
  | CallStatement of expression * arguments * arguments * ast
  | WithStatement of expression list * ast
  | AutoEscapeStatement of expression * ast

and expression =
    IdentExpr of string
  | LiteralExpr of tvalue
  | NotOpExpr of expression
  | NegativeOpExpr of expression
  | PlusOpExpr of expression * expression
  | MinusOpExpr of expression * expression
  | TimesOpExpr of expression * expression
  | PowerOpExpr of expression * expression
  | DivOpExpr of expression * expression
  | ModOpExpr of expression * expression
  | AndOpExpr of expression * expression
  | OrOpExpr of expression * expression
  | NotEqOpExpr of expression * expression
  | EqEqOpExpr of expression * expression
  | LtOpExpr of expression * expression
  | GtOpExpr of expression * expression
  | LtEqOpExpr of expression * expression
  | GtEqOpExpr of expression * expression
  | DotExpr of expression * expression
  | ApplyExpr of expression * arguments
  | ListExpr of expression list
  | SetExpr of expression list
  | ObjExpr of (expression * expression) list
  | TestOpExpr of expression * expression
  | KeywordExpr of expression * expression
  | AliasExpr of expression * expression
  | InOpExpr of expression * expression

and with_context = bool
and cond_branch = expression * ast
and else_statements = ast
and arguments = expression list

let std_env = {
  autoescape = true;
  template_dirs = [];
  filters = [];
  extensions = [];
  compiled = false;
}
;;
