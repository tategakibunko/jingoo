open Jg_types

let compiled = ref false
let file = ref "cheatsheet.tmpl"
  
let () =
  Arg.parse [
    ("-cmpl", Arg.Unit (fun () -> compiled := true), "use compiled template");
    ("-tmpl", Arg.String (fun f -> file := f), "template name");
  ] ignore "";

  let result_string = 
    Jg_template.from_file !file ~use_compiled:(!compiled) ~models:[
      ("msg", Tstr "hello world");
      ("list1", Tlist [Tint 1]);
      ("list", Tlist [Tint 10; Tint 20; Tint 30]);
      ("long_list", Tlist [Tint 10; Tint 20; Tint 30; Tint 40; Tint 50;	Tint 60; Tint 70; Tint 80; Tint 90; Tint 100]);
      ("obj", Tobj [("name", Tstr "hoge"); ("age", Tint 10)]);
      ("rows", Tlist [
	Tobj [("name", Tstr "bob"); ("age", Tint 20)];
	Tobj [("name", Tstr "ken"); ("age", Tint 25)];
      ]);
    ] in
  print_endline result_string
;;
