(*
  jg_types.mli

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)

exception SyntaxError of string

type environment = {
  autoescape : bool;
  (** If true, template variables are auto escaped when output. *)

  strict_mode : bool;
  (** If true, strict type cheking is enabled.
      If false, some kind of invalid type usages are just ignored.
      for example, following expression throws exception if [strict_mode = true],
      but is skipped if [strict_mode = false].

      {[
      {# 3(Tint) is not iterable #}
      {% for item in 3 %}
        {{ item }}
      {% endfor %}
      ]}
  *)

  template_dirs : string list;
  (** Template search path list used by [{% include %}] statements.
      Jingoo will always search in current directory in last resort.
  *)

  filters : (string * tvalue) list;
  (** User-defined filters. *)

  extensions : string list;
  (** Path list of shared library modules ([.cms] or [.cmxs] files)
      which are dynamically loaded.
   *)
}
(** See {! val:std_env} *)

and context = {
  frame_stack : frame list;
  macro_table : (string, macro) Hashtbl.t;
  namespace_table : (string, (string, tvalue) Hashtbl.t) Hashtbl.t;
  active_filters : string list;
  serialize: bool;
  output : tvalue -> unit;
}

and frame = (string -> tvalue)
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
  | Tsafe of string

and kwargs = (string * tvalue) list

(**/**)
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
(**/**)

(** {[
    let std_env = {
      autoescape = true;
      strict_mode = false;
      template_dirs = [];
      filters = [];
      extensions = [];
    }
    ]}
 *)
val std_env : environment


(** {2 Boxing OCaml values} *)

val box_int : int -> tvalue
val box_float : float -> tvalue
val box_string : string -> tvalue
val box_bool : bool -> tvalue
val box_list : tvalue list -> tvalue
val box_set : tvalue list -> tvalue
val box_obj : (string * tvalue) list -> tvalue
val box_hash : (string, tvalue) Hashtbl.t -> tvalue
val box_array : tvalue array -> tvalue
val box_pat : (string -> tvalue) -> tvalue
val box_lazy : tvalue Lazy.t -> tvalue
val box_fun : (?kwargs:kwargs -> tvalue -> tvalue) -> tvalue

(** {2 Unboxing OCaml values}
    Unboxing operations raise [Invalid_argument] in case of type error.
 *)

val unbox_int : tvalue -> int
val unbox_float : tvalue -> float
val unbox_string : tvalue -> string
val unbox_bool : tvalue -> bool
val unbox_list : tvalue -> tvalue list
val unbox_set : tvalue -> tvalue list
val unbox_array : tvalue -> tvalue array
val unbox_obj : tvalue -> (string * tvalue) list
val unbox_hash : tvalue -> (string, tvalue) Hashtbl.t
val unbox_pat : tvalue -> (string -> tvalue)
val unbox_lazy : tvalue -> tvalue Lazy.t
val unbox_fun : tvalue -> (?kwargs:kwargs -> tvalue -> tvalue)

(** {2 Helpers for function writing} *)

val func : (?kwargs:kwargs -> tvalue list -> tvalue) -> int -> tvalue
val func_arg1 : ?name:string -> (?kwargs:kwargs -> tvalue -> tvalue) -> tvalue
val func_arg2 : ?name:string -> (?kwargs:kwargs -> tvalue -> tvalue -> tvalue) -> tvalue
val func_arg3 : ?name:string -> (?kwargs:kwargs -> tvalue -> tvalue -> tvalue -> tvalue) -> tvalue

val func_no_kw : ?name:string -> (tvalue list -> tvalue) -> int -> tvalue
val func_arg1_no_kw : ?name:string -> (tvalue -> tvalue) -> tvalue
val func_arg2_no_kw : ?name:string -> (tvalue -> tvalue -> tvalue) -> tvalue
val func_arg3_no_kw : ?name:string -> (tvalue -> tvalue -> tvalue -> tvalue) -> tvalue

(** {2:notes-tvalue Notes about some data types }

    {!type-tvalue.Tobj}
    Key/value object using an associative list.

    {!type-tvalue.Thash}
    Key/value objects using a hash table.

    {!type-tvalue.Tpat}
    Key/value object using a function to map ["key"] to [value].
    Faster than {!type-tvalue.Tobj} and {!type-tvalue.Thash},
    but not iterable nor testable.

    {!type-tvalue.Tset}
    Tuples

    {!type-tvalue.Tlazy}
    Lazy values are actually computed only when needed.
    Useful for recursive some data structure.
    In the following example, your app would throw a stack overflow without lazyness.
    {[
    let rec lazy_model n =
      let prev = lazy_model (n - 1) in
      let next = lazy_model (n + 1) in
      let cur = Tint n in
      Tlazy (lazy (Tobj [ ("cur", cur) ; ("prev", prev) ; ("next", next) ]) )
    ]}

    {!type-tvalue.Tvolatile}
    You can use volatile values for variables that can not be defined at model's
    definition time or if it is subject to changes over time on ocaml's side

*)

(**
   {2:function-calls Function calls}

   Built-in functions (aka filters) expect the {b TARGET} value to be the {b LAST} argument, in
   order to be usable with the pipe notation. You are encouraged to do the same while defining your
   own functions.

   [{{ x | foo (10,20) }}] is equivalent too [{{ foo (10,20,x) }}].

   Functions support partial application. e.g. [{{ list | map (foo (10,20)) }}]

   There is two kind of arguments: {{:#type-args} unnamed arguments} and {{:#type-kwargs} keyword arguments}.

   When defining a {b keyword argument}, {b label can't be omitted}.

   You {b can't} use [slice(4, [1,2,3,4,5], 0)], because you need to explicitly bind [0] with the [fill_with] label.

   A correct usage of the [slice] function would be [slice(4, [1,2,3,4,5], fill_with=0)].

   Note that kwargs may be defined at any place: [slice(4, fill_with=0, [1,2,3,4,5])].

 *)

(**/**)
(** Function exported for internal usage but not documented *)

val type_string_of_tvalue : tvalue -> string
val failwith_type_error : string -> (string * tvalue) list -> 'a
val failwith_type_error_1 : string -> tvalue -> 'a
val failwith_type_error_2 : string -> tvalue -> tvalue -> 'a
val failwith_type_error_3 : string -> tvalue -> tvalue -> tvalue -> 'a
val merge_kwargs : kwargs option -> kwargs option -> kwargs option
val func_failure : ?name:string -> ?kwargs:kwargs -> tvalue list -> 'a
