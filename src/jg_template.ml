(******************************************************************************)
(* jingoo: Template engine inspired by Jinja2.                                *)
(*                                                                            *)
(* Copyright (C) 2011-2013 by Masaki WATANABE                                 *)
(*                                                                            *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* Permission is hereby granted, free of charge, to any person obtaining a    *)
(* copy of this software and associated documentation files (the "Software"), *)
(* to deal in the Software without restriction, including without limitation  *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *)
(* and/or sell copies of the Software, and to permit persons to whom the      *)
(* Software is furnished to do so, subject to the following conditions:       *)
(*                                                                            *)
(* The above copyright notice and this permission notice shall be included in *)
(* all copies or substantial portions of the Software.                        *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *)
(* DEALINGS IN THE SOFTWARE.                                                  *)
(******************************************************************************)

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
