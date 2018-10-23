(*
  jg_types.mli

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)

exception SyntaxError of string

type environment = {
  autoescape : bool;
  (** [default: true]
      if true, template variables are auto escaped when output.
  *)

  strict_mode : bool;
  (** [defualt: false]
      if true, strict type cheking is enabled.
      if false, some kind of invalid type usages are just ignored.
      for example, following expression throws exception if strict_mode is true,
      but skipped if strict_mode = false.

      {# 3(Tint) is not iterable #}
      {% for item in 3 %}
        {{ item }}
      {% endfor %}
  *)

  template_dirs : string list;
  (** [default: empty list]
      define template search path list.
      if empty, search current directory only.

      example:

      ["/path/to/tmpl1"; "/path/to/tmpl2"]
  *)

  filters : (string * tvalue) list;
  (** [default: empty list]
      define user own filter.

      see example/advanced.ml for sample.
  *)

  extensions : string list;
  (** [default: empty list]
      define path list of shared library module(.cms or .cmxs) which is dynamically loaded.

      example
      -------

      ["/path/to/a.cmxs"; "/path/to/b.cmxs"]
  *)
}

and context = {
  frame_stack : frame list;
  macro_table : (string, macro) Hashtbl.t;
  namespace_table : (string, frame) Hashtbl.t;
  active_filters : string list;
  output : string -> unit;
}

and frame = (string, tvalue) Hashtbl.t
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
  | Thash of (string, tvalue) Hashtbl.t (* faster object *)
  | Tpat of (string -> tvalue) (* much faster object, but not iterable, not testable *)
  | Tlist of tvalue list
  | Tset of tvalue list
  | Tfun of (?kwargs:kwargs -> args -> tvalue)
  | Tarray of tvalue array
  | Tlazy of tvalue Lazy.t
  | Tvolatile of (unit -> tvalue)
and args = tvalue list
and kwargs = (string * tvalue) list
(**
   1. About args
   -------------

   Arguments of function are defined as "tvalue list".
   And it's important to know that the filtered target is the LAST argument of filter function.
   For example, consider following expansion of "x" with filter function "foo" (with no keyword arguments)
   
   {{ x|foo(10,20) }}

   The filter function "foo" takes 3 arguments, and internally, this is evaluated like this.

   foo(10,20,x)

   2. About kwargs
   ---------------

   Keyword arguments of function are defined as (string * tvalue) list.
   And keyword label must be declared when use it, that is, implicit conversion not available.

   For example, built-in function "slice" catch two args(slice_length, target_list), and one keyword argument(fill_with).
   and following code slice the list with length 4 and fill 0 for rest space of splitted list.

     slice(4, [1,2,3,4,5], fill_with=0) 

   It return list of list [[1,2,3,4], [5,0,0,0]], works well.

   But you can't call this func like this.

     slice(4, [1,2,3,4,5], 0)

   It return list of list [[1,2,3,4], [5]], because keyword label 'fill_with' is not declared.
*)

and ast = statement list

and statement =
    TextStatement of string
  | ExpandStatement of expression
  | IfStatement of branch list
  | ForStatement of expression * expression * ast
  | IncludeStatement of expression * with_context
  | RawIncludeStatement of expression
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
  | NamespaceStatement of string * (string * expression) list
  | Statements of ast

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
  | DotExpr of expression * string
  | BracketExpr of expression * expression
  | ApplyExpr of expression * arguments
  | ListExpr of expression list
  | SetExpr of expression list
  | ObjExpr of (expression * expression) list
  | TestOpExpr of expression * expression
  | KeywordExpr of expression * expression
  | AliasExpr of expression * expression
  | InOpExpr of expression * expression

and with_context = bool
and branch = expression option * ast
and arguments = expression list

val std_env : environment

