(*
  jg_types.ml

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)

exception SyntaxError of string

type environment = {
  autoescape : bool;
  strict_mode : bool;
  template_dirs : string list;
  filters : (string * tvalue) list;
  extensions : string list;
}

and context = {
  frame_stack : frame list;
  macro_table : (string, macro) Hashtbl.t;
  namespace_table : (string, frame) Hashtbl.t;
  active_filters : string list;
  serialize: bool;
  output : tvalue -> unit
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
  | Thash of (string, tvalue) Hashtbl.t
  | Tpat of (string -> tvalue)
  | Tlist of tvalue list
  | Tset of tvalue list
  | Tfun of (?kwargs:kwargs -> args -> tvalue)
  | Tarray of tvalue array
  | Tlazy of tvalue Lazy.t
  | Tvolatile of (unit -> tvalue)
and args = tvalue list
and kwargs = (string * tvalue) list

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
  | FunctionStatement of expression * arguments * ast

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

(* If you modify this value, documentation in jg_types.mli *)
let std_env = {
  autoescape = true;
  strict_mode = false;
  template_dirs = [];
  filters = [];
  extensions = [];
}

let box_int i = Tint i
let box_float f = Tfloat f
let box_string s = Tstr s
let box_bool b = Tbool b
let box_list lst = Tlist lst
let box_set lst = Tset lst
let box_obj alist = Tobj alist
let box_hash hash = Thash hash
let box_array a = Tarray a
let box_pat fn = Tpat fn
let box_lazy z = Tlazy z
let box_fun z = Tfun z

let unbox_int = function
  | Tint x -> x
  | _ -> raise @@ Invalid_argument "unbox_int"

let unbox_float = function
  | Tfloat f -> f
  | _ -> raise @@ Invalid_argument "unbox_float"

let unbox_string = function
  | Tstr s -> s
  | _ -> raise @@ Invalid_argument "unbox_string"

let unbox_bool = function
  | Tbool b -> b
  | _ -> raise @@ Invalid_argument "unbox_bool"

let unbox_list = function
  | Tlist lst -> lst
  | _ -> raise @@ Invalid_argument "unbox_list"

let unbox_set = function
  | Tset lst -> lst
  | _ -> raise @@ Invalid_argument "unbox_set"

let unbox_array = function
  | Tarray lst -> lst
  | _ -> raise @@ Invalid_argument "unbox_array"

let unbox_obj = function
  | Tobj alist -> alist
  | _ -> raise @@ Invalid_argument "unbox_obj"

let unbox_hash = function
  | Thash hash -> hash
  | _ -> raise @@ Invalid_argument "unbox_hash"

let unbox_pat = function
  | Tpat pat -> pat
  | _ -> raise @@ Invalid_argument "unbox_pat"

let unbox_lazy = function
  | Tlazy l -> l
  | _ -> raise @@ Invalid_argument "unbox_lazy"

let rec func_arg1 (f: ?kwargs:kwargs -> tvalue -> tvalue) =
  Tfun (fun ?(kwargs=[]) args ->
    match args with
    | a1 :: _ -> f a1 ~kwargs
    | [] ->
       let f' = f ~kwargs in
       func_arg1 (fun ?kwargs:_ a1 -> f' a1)
  )

let func_arg2 (f: ?kwargs:kwargs -> tvalue -> tvalue -> tvalue) =
  Tfun (fun ?(kwargs=[]) args ->
    match args with
    | a1 :: a2 :: _ -> f a1 a2 ~kwargs
    | a1 :: _ ->
       let f' = f a1 ~kwargs in
       func_arg1 (fun ?kwargs:_ a2 -> f' a2)
    | _ -> Tnull
  )

let func_arg3 (f: ?kwargs:kwargs -> tvalue -> tvalue -> tvalue -> tvalue) =
  Tfun (fun ?(kwargs=[]) args ->
    match args with
    | a1 :: a2 :: a3 :: _ -> f a1 a2 a3 ~kwargs
    | a1 :: a2 :: _ ->
       let f' = f a1 a2 ~kwargs in
       func_arg1 (fun ?kwargs:_ a3 -> f' a3)
    | a1 :: _ ->
       let f' = f a1 ~kwargs in
       func_arg2 (fun ?kwargs:_ a2 a3 -> f' a2 a3)
    | _ -> Tnull
  )
