(*
  jg_cmdline.ml

  Copyright (c) 2011 - by Masaki WATANABE

  Licence: see LICENCE
*)

(**
   jingoo command line compiler
   -----------------------------

   usage:

     jingoo -template_dirs [template directories] -input [input file]

   example:

     jingoo -template_dirs /path/to/tmpl1,/path/to/tmpl2 -input hello.tmpl > hello.ml
*)

let get_template_dirs = function
  | "" -> []
  | dirs ->
    let dirs = Pcre.qreplace ~rex:(Pcre.regexp "[\\s\\t]+") ~templ:"" dirs in
    Pcre.split ~rex:(Pcre.regexp ",") dirs
;;

let () =
  let usage = "jingoo -input [input_file]" in
  let filename = ref "" in
  let dirs = ref "" in

  Arg.parse [
    ("-template_dirs", Arg.String (fun str -> dirs := str), "template dirs(split by comma)");
    ("-input", Arg.String (fun str -> filename := str), "input filename");
  ] ignore usage;
  
  try
    output_string stdout (Jg_compile.from_file ~template_dirs:(get_template_dirs !dirs) !filename)
  with
    | Jg_types.SyntaxError(msg) ->
      Printf.printf "syntax error:%s\n" msg
	
    | exn ->
      print_endline (Printexc.to_string exn)
;;
