(*
  jg_template.ml

  Copyright (c) 2012 - by Masaki WATANABE

  License: see LICENSE
*)
open Jg_types
open Jg_utils

let from_file ?(env=std_env) ?(models=[]) file_name =
  Jg_interp.from_file ~env ~models file_name

let from_string ?(env=std_env) ?ctx ?(models=[]) source =
  Jg_interp.from_string ~env ?ctx ~models source

