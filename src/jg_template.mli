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

val from_file : ?env:environment -> ?models:(string * tvalue) list -> ?use_compiled:bool -> string -> string
(** [from_file env models use_compiled template_filename]
    return result string.

    [env] is environment parametors defined in Jg_types.environment.
    enviroment parametors consist of template_dirs, autoescape_flag etc.

    [models] is variable table for template. for example,
    [("msg", Tstr "hello, world!"); ("count", Tint 100); ]

    if [use_compiled] is set true, jingoo will search template module(shared library) first,
    and if found, it's linked and call it dynamically.
    if not found, [template_filename] is loaded.
*)

val from_string : ?env:environment -> ?ctx:context -> ?models:(string * tvalue) list -> string -> string
(** [from_string env context models source_string]
    return result string.

    same as from_file but read template from source string.

    default of [ctx] is None.
    nomally, this context is used internal parsing.
*)

val compile : ?template_dirs:string list -> string -> string
(** [compile template_dirs template_filename] return compiled ocaml code.

    [template_dirs] is source path list of template files.
    default value is empty list, that means current directory.
*)
