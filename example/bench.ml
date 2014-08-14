open Jg_types
open Jg_utils

let file = ref "cheatsheet.tmpl"
let count = ref 1000
  
let () =
  Arg.parse [
    ("-tmpl", Arg.String (fun f -> file := f), "template name");
    ("-count", Arg.Int (fun i -> count := max 5 i), "loop count");
  ] ignore "";

  let output () = 
    ignore @@ Jg_template.from_file !file ~models:Test_data.models in

  let t0 = Unix.gettimeofday () in
  for i = 1 to 5 do
    for j = 1 to (!count / 5) do
      output ()
    done;
    Printf.printf "%d/5 done\n%!" i
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "time: %f\n" (t1 -. t0)
