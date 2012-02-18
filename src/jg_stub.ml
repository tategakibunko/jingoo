(*
  jg_stub.ml

  Copyright (c) 2012 - by Masaki WATANABE

  Licence: see LICENCE
*)
open Jg_utils
open Jg_types

type tmpl_func = (?env:environment -> ?models:frame -> string -> string)

let func_table : (string, tvalue) Hashtbl.t = Hashtbl.create 20
let tmpl_func_table : (string, tmpl_func) Hashtbl.t = Hashtbl.create 20

let func_name namespace name =
  spf "%s.%s" namespace name
;;

let add_func namespace name value =
  Hashtbl.add func_table (func_name namespace name) value
;;

let get_func namespace name =
  Hashtbl.find func_table (func_name namespace name)
;;

let add_tmpl_func path value =
  Hashtbl.add tmpl_func_table path value
;;

let get_tmpl_func path =
  Hashtbl.find tmpl_func_table path
;;
