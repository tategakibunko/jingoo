(*
  jg_template.mli

  Copyright (c) 2012 - by Masaki WATANABE

  License: see LICENSE
*)
open Jg_types

val from_file : ?env:environment -> ?models:(string * tvalue) list -> string -> string
(** [from_file env models template_filename]
    return result string.

    [env] is environment parametors defined in Jg_types.environment.
    enviroment parametors consist of template_dirs, autoescape_flag etc.

    [models] is variable table for template. for example,
    [("msg", Tstr "hello, world!"); ("count", Tint 100); ]
*)

val from_string : ?env:environment -> ?ctx:context -> ?models:(string * tvalue) list -> string -> string
(** [from_string env context models source_string]
    return result string.

    same as from_file but read template from source string.

    default of [ctx] is None.
    nomally, this context is used internal parsing.
*)

