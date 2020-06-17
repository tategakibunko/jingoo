(*
  jg_template.mli

  Copyright (c) 2012 - by Masaki WATANABE

  License: see LICENSE
*)
open Jg_types

val from_file :
  ?env:environment ->
  ?ctx:context ->
  ?models:(string * tvalue) list ->
  string ->
  string
(** [from_file env models template_filename]
    return result string.

    [env] is environment parameters defined in Jg_types.environment.
    environment parameters consist of template_dirs, autoescape_flag etc.

    default of [ctx] is None.

    [models] is variable table for template. for example,
    [("msg", Tstr "hello, world!"); ("count", Tint 100); ]
*)

val from_file_ex :
  ?env:environment ->
  ?ctx:context ->
  ?models:(string -> tvalue) ->
  string ->
  string
(** [from_file_ex env models template_filename]
    return result string.

    same as from_file but [models] is closure(model getter).
*)

val from_chan :
  ?env:environment ->
  ?ctx:context ->
  ?models:(string * tvalue) list ->
  in_channel ->
  string
(** [from_chan env models chan]
    return result string.

    same as from_file but read template from {!type:Stdlib.in_channel}.
*)

val from_chan_ex :
  ?env:environment ->
  ?ctx:context ->
  ?models:(string -> tvalue) ->
  in_channel ->
  string
(** [from_chan_ex env models chan]
    return result string.

    same as from_chan but [models] is closure(model getter).
*)

val from_string :
  ?env:environment ->
  ?ctx:context ->
  ?models:(string * tvalue) list ->
  string ->
  string
(** [from_string env context models source_string]
    return result string.

    same as from_file but read template from source string.

    nomally, this context is used internal parsing.
*)

val from_string_ex :
  ?env:environment ->
  ?ctx:context ->
  ?models:(string -> tvalue) ->
  string ->
  string
(** [from_string_ex env context models source_string]
    return result string.

    same as from_string but [models] is closure(model getter).
*)

module Loaded : sig
  type t
  (** A [Loaded.t] stores a parsed template in memory so it can be evaluated
      multiple times against different models more efficiently. *)

  val from_file : ?env:environment -> string -> t
  (** [from_file env template_filename]
      return result t.

      [env] is environment parameters defined in Jg_types.environment.
      environment parameters consist of template_dirs, autoescape_flag etc.
  *)

  val from_chan : ?env:environment -> in_channel -> t
  (** [from_chan env chan]
      return result t.

      same as from_file but read template from {!type:Stdlib.in_channel}.
  *)

  val from_string : ?env:environment -> string -> t
  (** [from_string env source_string]
      return result t.

      same as from_file but read template from source string.
  *)

  val eval : ?ctx:context -> ?models:(string * tvalue) list -> t -> string
  (** [eval context models t] evaluates the loaded template in the given context
      with the given models.
      return result string.

      [models] is variable table for template. for example,
      [("msg", Tstr "hello, world!"); ("count", Tint 100); ]
  *)
end
