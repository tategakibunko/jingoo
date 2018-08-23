open Jg_types
open Jg_utils

let file = ref "cheatsheet.jingoo"
let count = ref 1000
  
let () =
  Arg.parse [
    ("-tmpl", Arg.String (fun f -> file := f), "template name");
    ("-count", Arg.Int (fun i -> count := max 5 i), "loop count");
  ] ignore "";

  let template = Jg_utils.read_file_as_string !file in
  let output () = 
    ignore @@ Jg_template.from_string ~models:Test_data.models template in

  let t0 = Unix.gettimeofday () in
  for i = 1 to 5 do
    for j = 1 to (!count / 5) do
      output ()
    done;
    Printf.printf "%d/5 done\n%!" i
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "time: %f\n" (t1 -. t0)
