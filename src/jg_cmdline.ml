(*
  jg_cmdline.ml

  Copyright (c) 2011 - by Masaki WATANABE

  License: see LICENSE
*)

(**
   jingoo command line compiler
   -----------------------------

   usage:

     jingoo -template_dirs [template directories] -input [input file]

   example:

     jingoo -template_dirs /path/to/tmpl1,/path/to/tmpl2 -input hello.tmpl > hello.ml
*)

open Jingoo

let get_template_dirs = function
  | "" -> []
  | dirs ->
    let dirs = Re.replace_string (Re.Pcre.regexp "[\\s\\t]+") ~by:"" dirs in
    Re.split (Re.Pcre.regexp ",") dirs

let () =
  let usage = "jingoo -input [input_file]" in
  let filename = ref "" in
  let tmplname = ref "" in
  let dirs = ref "" in

  Arg.parse [
    ("-template_dirs", Arg.String (fun str -> dirs := str), "template dirs(split by comma)");
    ("-input", Arg.String (fun str -> filename := str), "input filename");
    ("-interp", Arg.String (fun str -> tmplname := str), "interp template_file with no models");
  ] ignore usage;

  try
    output_string stdout (Jg_template.from_file !tmplname ~models:[])
  with
    | Jg_types.SyntaxError(msg) ->
      Printf.printf "syntax error:%s\n" msg

    | exn ->
      print_endline (Printexc.to_string exn)
