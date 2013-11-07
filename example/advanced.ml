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

let spf = Printf.sprintf
let compiled = ref false
let file = ref "cheatsheet.tmpl"

(* define custom filter to_gmail *)
let to_gmail ?(defaults=[
]) value kwargs =
  let id = Jg_runtime.string_of_tvalue value in
  Tstr (spf "%s@gmail.com" id)
;;

(* define custom filter to_mail more complex *)
let to_mail ?(defaults=[
  ("domain", Tstr "gmail.com");
]) value kwargs =
  let id = Jg_runtime.string_of_tvalue value in
  let domain = Jg_runtime.string_of_tvalue (Jg_runtime.jg_get_kvalue "domain" kwargs ~defaults) in
  Tstr (spf "%s@%s" id domain)
;;    

let () =
  Arg.parse [
    ("-cmpl", Arg.Unit (fun () -> compiled := true), "use compiled template");
    ("-tmpl", Arg.String (fun f -> file := f), "template name");
  ] ignore "";

  let env =
    {std_env with

      (* add own filter *)
      filters = [
	("to_gmail", Jg_runtime.func_arg1 to_gmail);
	("to_mail", Jg_runtime.func_arg1 to_mail);
      ];
      
      (* load own extension *)
      extensions = [
	"my_ext.cmxs";
      ];
    } in

  let result_string = 
    Jg_template.from_file !file ~env ~use_compiled:(!compiled) ~models:Test_data.models in

  print_endline result_string
;;
