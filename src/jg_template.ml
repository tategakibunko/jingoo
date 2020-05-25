(*
  jg_template.ml

  Copyright (c) 2012 - by Masaki WATANABE

  License: see LICENSE
*)
open Jg_types

(** Internally, interpretted result is outputed to `output:(string -> unit)` interface. *)
type 'a internal_interp = ?env:Jg_types.environment ->
    ?models:(string * Jg_types.tvalue) list ->
    output:(tvalue -> unit) ->
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
    let output x = Buffer.add_string buffer (Jg_runtime.string_of_tvalue x) in
    let () = fn ~env ~models ~output ?ctx arg in
    Buffer.contents buffer

let from_chan = content (Jg_interp.from_chan ?file_path:None)

let from_file = content Jg_interp.from_file

let from_string = content (Jg_interp.from_string ?file_path:None)

module Loaded = struct
  type t = Jg_interp.Loaded.t

  let from_chan ?env chan = Jg_interp.Loaded.from_chan ?env chan
  let from_file ?env file_name = Jg_interp.Loaded.from_file ?env file_name
  let from_string ?env source = Jg_interp.Loaded.from_string ?env source

  let eval ?ctx ?(models = []) t =
    let models = fun x -> List.assoc x models in
    let buffer = Buffer.create 1024 in
    let output x = Buffer.add_string buffer (Jg_runtime.string_of_tvalue x) in
    let () = Jg_interp.Loaded.eval t ~models ~output ?ctx () in
    Buffer.contents buffer
end
