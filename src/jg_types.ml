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
  macro_table : (string, macro) Hashtbl.t; [@printer fun fmt _ -> fprintf fmt "macro_table"]
  namespace_table : (string, (string, tvalue) Hashtbl.t) Hashtbl.t [@printer fun fmt _ -> fprintf fmt "namespace_table"];
  active_filters : string list;
  serialize: bool;
  output : tvalue -> unit
}

and frame = (string -> tvalue)
and macro = Macro of macro_arg_names * macro_defaults * macro_code
and macro_arg_names = string list
and macro_defaults = kwargs
and macro_code = statement list
and tvalue =
  | Tnull
  | Tint of int
  | Tbool of bool
  | Tfloat of float
  | Tstr of string
  | Tobj of (string * tvalue) list
  | Thash of (string, tvalue) Hashtbl.t
        [@printer fun fmt ht ->
          let ht =
            Hashtbl.fold (fun k v acc -> (k, v) :: acc) ht []
            |> List.sort compare in
          fprintf fmt "Thash (%a)"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ';')
               (fun fmt (k, v) -> fprintf fmt "(\"%s\", %a)" k pp_tvalue v) )
            ht ]
  | Tpat of (string -> tvalue)
  | Tlist of tvalue list
  | Tset of tvalue list
  | Tfun of (?kwargs:kwargs -> tvalue -> tvalue)
  | Tarray of tvalue array
  | Tlazy of tvalue Lazy.t
  | Tvolatile of (unit -> tvalue)
  | Tsafe of string
[@@deriving show { with_path = false }]

and kwargs = (string * tvalue) list

and ast = statement list
[@@deriving show { with_path = false }]

and statement =
    TextStatement of string
  | ExpandStatement of expression
  | IfStatement of branch list
  | ForStatement of string list * expression * ast
  | IncludeStatement of expression * with_context
  | RawIncludeStatement of expression
  | ExtendsStatement of string
  | ImportStatement of string * string option
  | FromImportStatement of string * (string * string option) list
  | SetStatement of expression * expression
  | SetBlockStatement of string * ast
  | BlockStatement of string * ast
  | MacroStatement of string * arguments * ast
  | FilterStatement of string * ast
  | CallStatement of string * arguments * (string option * expression) list * ast
  | WithStatement of (string * expression) list * ast
  | AutoEscapeStatement of expression * ast
  | NamespaceStatement of string * (string * expression) list
  | Statements of ast
  | FunctionStatement of string * arguments * ast
  | SwitchStatement of expression * (expression list * ast) list
[@@deriving show { with_path = false }]

and expression =
  | IdentExpr of string
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
  | ApplyExpr of expression * (string option * expression) list
  | ListExpr of expression list
  | SetExpr of expression list
  | ObjExpr of (string * expression) list
  | TestOpExpr of expression * expression
  | InOpExpr of expression * expression
  | FunctionExpression of string list * expression
  | TernaryOpExpr of expression * expression * expression

and with_context = bool
and branch = expression option * ast
and arguments = (string * expression option) list

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
  | Tsafe _ -> "safe"

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

(*
  [https://github.com/tategakibunko/jingoo/pull/69]
  func f 3 = Tfun ?kws1 x -> Tfun ?kws2 y -> Tfun ?kws3 z -> f ?kwargs:(merge (merge (merge kws1 []) kws2) kws3) [x; y; z]
*)
let func f n =
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

let func_arg1 ?name f =
  let f = fun ?kwargs -> function [ a ] -> f ?kwargs a
                                | args -> func_failure ?name ?kwargs args in
  func f 1

let func_arg2 ?name f =
  let f = fun ?kwargs -> function [ a ; b ] -> f ?kwargs a b
                                | args -> func_failure ?name ?kwargs args in
  func f 2

let func_arg3 ?name f =
  let f = fun ?kwargs -> function [ a ; b ; c ] -> f ?kwargs a b c
                                | args -> func_failure ?name ?kwargs args in
  func f 3

let func_no_kw ?name f n =
  if n = 0 then Tfun (fun ?kwargs:_ _ -> f [])
  else
    let rec aux acc rem =
      Tfun
        (fun ?(kwargs=[]) x ->
           if kwargs <> [] then func_failure ?name ~kwargs (List.rev @@ x :: acc)
           else if rem = 1 then f (List.rev @@ x :: acc)
           else aux (x :: acc) (rem - 1))
    in
    aux [] n

let func_arg1_no_kw ?name f =
  func_no_kw ?name (function [ a ] -> f a | args -> func_failure ?name args) 1

let func_arg2_no_kw ?name f =
  func_no_kw ?name (function [ a ; b ] -> f a b | args -> func_failure ?name args) 2

let func_arg3_no_kw ?name f =
  func_no_kw ?name (function [ a ; b ; c ] -> f a b c | args -> func_failure ?name args) 3
