(*
  jingoo_mt.ml

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)
open Jg_utils

let () =
  let mutex = Mutex.create () in
  lock_unlock.lock <- (fun () -> Mutex.lock mutex);
  lock_unlock.unlock <- (fun () -> Mutex.unlock mutex)

      
