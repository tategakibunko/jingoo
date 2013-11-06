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

(*
  my_ext.ml

  Copyright (c) 2012 - by Masaki WATANABE <lambda.watanabe@gmail.com>

  Licence: GPL
*)
open Jg_types

let to_md5 ?(defaults=[
  ("seed", Tstr "");
]) value kwargs =
  let seed = Jg_runtime.jg_get_kvalue "seed" kwargs ~defaults in
  match value, seed with
    | Tstr str, Tstr seed ->
      Tstr (Digest.to_hex (Digest.string (str ^ seed)))
    | _ -> Tnull
;;

let () =
  Jg_stub.add_func "hash" "to_md5" (Jg_runtime.func_arg1 to_md5)
;;
