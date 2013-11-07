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

val add_func : string -> string -> tvalue -> unit
(** [add_func namespace func_name func]
    regist function object to stub.
*)

val get_func : string -> string -> tvalue
(** [get_func namespace func_name]
    return registered function by namespace and func_name.
    Raise [Not_found] if no entry found.
*)

type tmpl_func = (?env:environment -> ?models:(string * tvalue) list -> string -> string)
(** tmpl_func is type of compiled template renderer *)

val add_tmpl_func : string -> tmpl_func -> unit
(** [add_tmpl_func template_path func]
    regits compiled template function
*)

val get_tmpl_func : string -> tmpl_func
(** [get_tmpl_func template_path]
    return compiled template function associated with template_path
    Raise [Not_found] if no entry found.
*)
