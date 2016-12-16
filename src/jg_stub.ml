(*
  jg_stub.ml

  Copyright (c) 2012 - by Masaki WATANABE

  License: see LICENSE
*)
open Jg_utils

let func_table : (string, Jg_types.tvalue) Hashtbl.t = Hashtbl.create 20

let func_path ~namespace ~func_name =
  spf "%s.%s" namespace func_name
;;

let add_func ~namespace ~func_name func_value =
  Hashtbl.add func_table (func_path ~namespace ~func_name) func_value
;;

let get_func ~namespace ~func_name =
  Hashtbl.find func_table (func_path ~namespace ~func_name)
;;
