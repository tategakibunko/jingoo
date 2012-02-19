open Jg_types
open Jg_utils

let compiled = ref false
let file = ref "cheatsheet.tmpl"
  
let () =
  Arg.parse [
    ("-cmpl", Arg.Unit (fun () -> compiled := true), "use compiled template");
    ("-tmpl", Arg.String (fun f -> file := f), "template name");
  ] ignore "";

  let output () = 
    ignore @@ Jg_template.from_file !file ~use_compiled:(!compiled) ~models:Test_data.models in

  for i = 1 to 5 do
    for j = 1 to 200 do
      output ()
    done;
    print_endline @@ spf "%d/5 done" i
  done
;;
