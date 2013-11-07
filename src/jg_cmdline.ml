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

(**
   jingoo command line compiler
   -----------------------------

   usage:

     jingoo -template_dirs [template directories] -input [input file]

   example:

     jingoo -template_dirs /path/to/tmpl1,/path/to/tmpl2 -input hello.tmpl > hello.ml
*)

let get_template_dirs = function
  | "" -> []
  | dirs ->
    let dirs = Pcre.qreplace ~rex:(Pcre.regexp "[\\s\\t]+") ~templ:"" dirs in
    Pcre.split ~rex:(Pcre.regexp ",") dirs
;;

let () =
  let usage = "jingoo -input [input_file]" in
  let filename = ref "" in
  let tmplname = ref "" in
  let dirs = ref "" in

  Arg.parse [
    ("-template_dirs", Arg.String (fun str -> dirs := str), "template dirs(split by comma)");
    ("-input", Arg.String (fun str -> filename := str), "input filename");
    ("-interp", Arg.String (fun str -> tmplname := str), "interp template_file with no models");
  ] ignore usage;
  
  try
    if !filename <> "" then
      output_string stdout (Jg_compile.from_file ~template_dirs:(get_template_dirs !dirs) !filename)
    else if !tmplname <> "" then
      output_string stdout (Jg_template.from_file !tmplname ~models:[])
  with
    | Jg_types.SyntaxError(msg) ->
      Printf.printf "syntax error:%s\n" msg
	
    | exn ->
      print_endline (Printexc.to_string exn)
;;
