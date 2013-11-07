(*
  jg_stub.mli

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)
open Jg_types

val add_func : string -> string -> tvalue -> unit
(** [add_func namespace func_name func]
    regist function object to stub.
*)

val get_func : string -> string -> tvalue
(** [get_func namespace func_name]
    return registered function by namespace and func_name.
    Raise [Not_found] if no entry found.
*)

type tmpl_func = (?env:environment -> ?models:(string * tvalue) list -> string -> string)
(** tmpl_func is type of compiled template renderer *)

val add_tmpl_func : string -> tmpl_func -> unit
(** [add_tmpl_func template_path func]
    regits compiled template function
*)

val get_tmpl_func : string -> tmpl_func
(** [get_tmpl_func template_path]
    return compiled template function associated with template_path
    Raise [Not_found] if no entry found.
*)
