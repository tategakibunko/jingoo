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
  | Tfun of (?kwargs:kwargs -> tvalue -> tvalue)
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

(**/**)
let type_string_of_tvalue = function
  | Tint _ -> "int"
  | Tfloat _ -> "float"
  | Tstr _ -> "string"
  | Tbool _ -> "bool"
  | Tobj _ -> "obj"
  | Thash _ -> "hash"
  | Tlist _ -> "list"
  | Tpat _ -> "pat"
  | Tset _ -> "set"
  | Tfun _ -> "function"
  | Tnull -> "null"
  | Tarray _ -> "array"
  | Tlazy _ -> "lazy"
  | Tvolatile _ -> "volatile"

let invalid_argument x name =
  raise @@ Invalid_argument (name ^ ":" ^ type_string_of_tvalue x)

let failwith_type_error name args =
  failwith @@
  Printf.sprintf "type error: %s(%s)" name @@
  String.concat "," @@
  List.map
    (fun (k, v) -> (if k = "" then "" else k ^ "=") ^ type_string_of_tvalue v)
    args

let failwith_type_error_1 name x =
  failwith @@
  Printf.sprintf "type error: %s(%s)" name (type_string_of_tvalue x)

let failwith_type_error_2 name a b =
  failwith @@
  Printf.sprintf "type error: %s(%s, %s)"
    name (type_string_of_tvalue a) (type_string_of_tvalue b)

let failwith_type_error_3 name a b c =
  failwith @@
  Printf.sprintf "type error: %s(%s, %s, %s)"
    name
    (type_string_of_tvalue a)
    (type_string_of_tvalue b)
    (type_string_of_tvalue c)
(**/**)

let unbox_int = function
  | Tint x -> x
  | x -> invalid_argument x "unbox_int"

let unbox_float = function
  | Tfloat f -> f
  | x -> invalid_argument x "unbox_float"

let unbox_string = function
  | Tstr s -> s
  | x -> invalid_argument x "unbox_string"

let unbox_bool = function
  | Tbool b -> b
  | x -> invalid_argument x "unbox_bool"

let unbox_list = function
  | Tlist lst -> lst
  | x -> invalid_argument x "unbox_list"

let unbox_set = function
  | Tset lst -> lst
  | x -> invalid_argument x "unbox_set"

let unbox_array = function
  | Tarray lst -> lst
  | x -> invalid_argument x "unbox_array"

let unbox_obj = function
  | Tobj alist -> alist
  | x -> invalid_argument x "unbox_obj"

let unbox_hash = function
  | Thash hash -> hash
  | x -> invalid_argument x "unbox_hash"

let unbox_pat = function
  | Tpat pat -> pat
  | x -> invalid_argument x "unbox_pat"

let unbox_lazy = function
  | Tlazy l -> l
  | x -> invalid_argument x "unbox_lazy"

let unbox_fun = function
  | Tfun fn -> fn
  | x -> invalid_argument x "unbox_fun"

let func_failure ?(name = "<lambda>") ?(kwargs = []) args =
  failwith_type_error name (kwargs @ List.map (fun x -> "", x) args)

let merge_kwargs a b = match a, b with
  | None, x | x, None -> x
  | Some a, Some b -> Some ((List.filter (fun (x, _) -> not @@ List.mem_assoc x b) a) @ b)

let func_kw f n =
  if n = 0 then Tfun (fun ?kwargs _ -> f ?kwargs [])
  else
    let rec aux ?kwargs:kw acc rem =
      Tfun
        (fun ?kwargs x ->
           let kwargs = merge_kwargs kw kwargs in
           if rem = 1
           then f ?kwargs (List.rev @@ x :: acc)
           else aux ?kwargs (x :: acc) (rem - 1))
    in
    aux [] n

let func f n =
  if n = 0 then Tfun (fun ?kwargs:_ _ -> f [])
  else
    let rec aux acc rem =
      Tfun
        (fun ?kwargs:_ x ->
           if rem = 1
           then f (List.rev @@ x :: acc)
           else aux (x :: acc) (rem - 1))
    in
    aux [] n

let func_kw_1 ?name f =
  let f = fun ?kwargs -> function [ a ] -> f ?kwargs a
                                | args -> func_failure ?name ?kwargs args in
  func_kw f 1

let func_kw_2 ?name f =
  let f = fun ?kwargs -> function [ a ; b ] -> f ?kwargs a b
                                | args -> func_failure ?name ?kwargs args in
  func_kw f 2

let func_kw_3 ?name f =
  let f = fun ?kwargs -> function [ a ; b ; c ] -> f ?kwargs a b c
                                | args -> func_failure ?name ?kwargs args in
  func_kw f 3

let func_1 ?name f =
  func (function [ a ] -> f a | args -> func_failure ?name args) 1

let func_2 ?name f =
  func (function [ a ; b ] -> f a b | args -> func_failure ?name args) 2

let func_3 ?name f =
  func (function [ a ; b ; c ] -> f a b c | args -> func_failure ?name args) 3
