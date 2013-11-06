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

let compiled = ref false
let file = ref "cheatsheet.tmpl"
let count = ref 1000
  
let () =
  Arg.parse [
    ("-cmpl", Arg.Unit (fun () -> compiled := true), "use compiled template");
    ("-tmpl", Arg.String (fun f -> file := f), "template name");
    ("-count", Arg.Int (fun i -> count := max 5 i), "loop count");
  ] ignore "";

  let output () = 
    ignore @@ Jg_template.from_file !file ~use_compiled:(!compiled) ~models:Test_data.models in

  let t0 = Unix.gettimeofday () in
  for i = 1 to 5 do
    for j = 1 to (!count / 5) do
      output ()
    done;
    Printf.printf "%d/5 done\n%!" i
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "time: %f\n" (t1 -. t0)
;;
