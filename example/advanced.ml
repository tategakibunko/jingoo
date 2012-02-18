open Jg_types

let spf = Printf.sprintf
let compiled = ref false
let file = ref "cheatsheet.tmpl"

(* define custom filter to_gmail *)
let to_gmail ?(defaults=[
]) value kwargs =
  let id = Jg_runtime.string_of_tvalue value in
  Tstr (spf "%s@gmail.com" id)
;;

(* define custom filter to_mail more complex *)
let to_mail ?(defaults=[
  ("domain", Tstr "gmail.com");
]) value kwargs =
  let id = Jg_runtime.string_of_tvalue value in
  let domain = Jg_runtime.string_of_tvalue (Jg_runtime.jg_get_kvalue "domain" kwargs ~defaults) in
  Tstr (spf "%s@%s" id domain)
;;    

let () =
  Arg.parse [
    ("-cmpl", Arg.Unit (fun () -> compiled := true), "use compiled template");
    ("-tmpl", Arg.String (fun f -> file := f), "template name");
  ] ignore "";

  let env =
    {std_env with

      (* add own filter *)
      filters = [
	("to_gmail", Jg_runtime.func_arg1 to_gmail);
	("to_mail", Jg_runtime.func_arg1 to_mail);
      ];
      
      (* load own extension *)
      extensions = [
	"my_ext.cmxs";
      ];
    } in
  let result_string = 
    Jg_template.from_file !file ~env ~use_compiled:(!compiled) ~models:[
      ("msg", Tstr "hello, world");
      ("list", Tlist [Tint 10; Tint 20;	Tint 30]);
      ("long_list", Tlist [Tint 10; Tint 20; Tint 30; Tint 40; Tint 50;	Tint 60; Tint 70; Tint 80; Tint 90; Tint 100;]);
      ("obj", Tobj [("name", Tstr "taro"); ("age", Tint 10)]);
      ("rows", Tlist [
	Tobj [("name", Tstr "jiro"); ("age", Tint 20)];
	Tobj [("name", Tstr "saburo"); ("age", Tint 25)];
       ]);
    ] in
  print_endline result_string
;;
