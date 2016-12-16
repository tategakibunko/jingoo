(*
  jg_stub.mli

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)

val add_func : namespace:string -> func_name:string -> Jg_types.tvalue -> unit
(** [add_func namespace func_name func_value]
    regist function object(Tfun) to stub.
*)

val get_func : namespace:string -> func_name:string -> Jg_types.tvalue
(** [get_func namespace func_name]
    return registered function(Tfun) for namespace and func_name.
    Raise [Not_found] if no entry found.
*)
