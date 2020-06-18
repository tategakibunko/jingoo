(*
  This is reviced version of Jg_template.

  https://github.com/tategakibunko/jingoo/issues/112
*)
open Jg_types

val from_file :
  ?env:environment ->
  ?ctx:context ->
  ?models:(string -> tvalue) ->
  string ->
  string
(** [from_file env models template_filename]
    return result string.

    [env] is environment parameters defined in Jg_types.environment.
    environment parameters consist of template_dirs, autoescape_flag etc.

    default of [ctx] is None.

    [models] is getter function for some data-sources. For example,
    let models = fun key -> List.assoc key [("name", Tstr "taro"); ("age", Tint 20)]
    let models2 = function "name" -> Tstr "taro" | "age" -> Tint 20 | _ -> Tnull
*)

val from_chan :
  ?env:environment ->
  ?ctx:context ->
  ?models:(string -> tvalue) ->
  in_channel ->
  string
(** [from_chan env models chan]
    return result string.

    same as from_file but read template from {!type:Stdlib.in_channel}.
*)

val from_string :
  ?env:environment ->
  ?ctx:context ->
  ?models:(string -> tvalue) ->
  string ->
  string
(** [from_string env context models source_string]
    return result string.

    same as from_file but read template from source string.

    nomally, this context is used internal parsing.
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

  val eval : ?ctx:context -> ?models:(string -> tvalue) -> t -> string
  (** [eval context models t] evaluates the loaded template in the given context
      with the given models(defined by closure).
      return result string.

      [models] is getter function for some data-sources. For example,
      let models = fun key -> List.assoc key [("name", Tstr "taro"); ("age", Tint 20)]
      let models2 = function "name" -> Tstr "taro" | "age" -> Tint 20 | _ -> Tnull
  *)
end
