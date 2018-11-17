(*
  jg_cmdline.ml

  Copyright (c) 2011 - by Masaki WATANABE

  License: see LICENSE
*)

open Jingoo

let usage = "jingoo [OPTIONS]"
let infile = ref ""
let outfile = ref ""
let dirs = ref ""
let dynlink = ref ""
let models = ref []

let () =

  Arg.parse [

    ("-dynlink", Arg.Set_string dynlink,
     "FILE1,FILE2,... Load files via dynlink module before running main program.");

    ("-i", Arg.Set_string infile,
     "input filename. Use - for reading stdin.");

    ("-o", Arg.Set_string outfile,
     "FILENAME redirect output to FILENAME. Use - for reading stdin.");

    ("-template_dirs", Arg.Set_string dirs,
     "DIR1,DIR2,... Search these directories when {% include/extend %}");

  ] ignore usage ;

  begin
    try if !dynlink <> "" then List.iter Dynlink.loadfile (String.split_on_char ',' !dynlink) ;
    with Dynlink.Error e ->
      failwith @@ Printf.sprintf "Dynlink error: %s\n%!" (Dynlink.error_message e)
  end ;

  let outchan =
    match !outfile with "" | "-" -> stdout | outfile -> open_out outfile
  in

  let env =
    { Jg_types.std_env with template_dirs = (String.split_on_char ',' !dirs) }
  in

  try
    output_string outchan @@
      match !infile with
      | "" | "-" -> Jg_template.from_chan stdin ~env ~models:!models
      | infile -> Jg_template.from_file infile ~env ~models:!models
  with
    | Jg_types.SyntaxError(msg) ->
      Printf.printf "syntax error:%s\n" msg

    | exn ->
      print_endline (Printexc.to_string exn)
