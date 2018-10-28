(*
  jg_template.ml

  Copyright (c) 2012 - by Masaki WATANABE

  License: see LICENSE
*)
open Jg_types

(** Internally, interpretted result is outputed to `output:(string -> unit)` interface. *)
type 'a internal_interp = ?env:Jg_types.environment ->
    ?models:(string * Jg_types.tvalue) list ->
    output:(string -> unit) ->
    ?ctx:Jg_types.context ->
    'a -> unit

(** Externally, interpretted result is outputed as string. *)
type 'a external_interp = ?env:Jg_types.environment ->
    ?ctx:Jg_types.context ->
    ?models:(string * Jg_types.tvalue) list ->
    'a -> string

let content (fn : 'a internal_interp) : 'a external_interp =
  fun ?(env=std_env) ?ctx ?(models=[]) (arg:'a) ->
    let buffer = Buffer.create 1024 in
    let () = fn ~env ~models ~output:(Buffer.add_string buffer) ?ctx arg in
    Buffer.contents buffer

let from_file = content Jg_interp.from_file

let from_string = content (Jg_interp.from_string ?file_path:None)
