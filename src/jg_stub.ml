(*
  jg_stub.ml

  Copyright (c) 2012 - by Masaki WATANABE

  License: see LICENSE
*)
open Jg_utils
open Jg_types

type tmpl_func = (?env:environment -> ?models:(string * tvalue) list -> string -> string)

let func_table : (string, tvalue) Hashtbl.t = Hashtbl.create 20

let func_path ~namespace ~func_name =
  spf "%s.%s" namespace func_name
;;

let add_func ~namespace ~func_name func_value =
  Hashtbl.add func_table (func_path ~namespace ~func_name) func_value
;;

let get_func ~namespace ~func_name =
  Hashtbl.find func_table (func_path ~namespace ~func_name)
;;
