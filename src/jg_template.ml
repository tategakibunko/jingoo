(*
  jg_template.ml

  Copyright (c) 2012 - by Masaki WATANABE

  License: see LICENSE
*)
open Jg_types
open Jg_utils

let get_cmxs_path file_path =
  spf "%s.cmxs" (Filename.chop_extension file_path)
;;

let rec get_compiled_renderer file_path =
  try
    Jg_stub.get_tmpl_func file_path
  with Not_found ->
    Dynlink.loadfile @@ get_cmxs_path file_path;
    get_compiled_renderer file_path
;;

let from_compiled ?(env=std_env) ?(models=[]) file_name =
  let file_path = Jg_utils.get_file_path file_name ~template_dirs:env.template_dirs in
  (get_compiled_renderer file_path) ~env:{env with compiled = true} ~models file_name
;;    

let from_file ?(env=std_env) ?(models=[]) ?(use_compiled=false) file_name =
  if not use_compiled then
    Jg_interp.from_file ~env ~models file_name
  else
    from_compiled ~env ~models file_name
;;

let from_string ?(env=std_env) ?ctx ?(models=[]) source =
  Jg_interp.from_string ~env ?ctx ~models source
;;

let compile ?(template_dirs=[]) file_name =
  Jg_compile.from_file ~template_dirs file_name
;;
