open Jg_types
open Jg_utils

let file = ref "cheatsheet.tmpl"
let count = ref 10000

let () =
  Arg.parse [
    ("-tmpl", Arg.Set_string file, "template name");
    ("-count", Arg.Int (fun i -> count := max 5 i), "loop count");
  ] ignore "";

  let input = Jg_utils.read_file_as_string !file in
  let n = Int64.of_int !count in
  let benchmark name (fn : unit -> 'a) =
    Benchmark.tabulate (Benchmark.latency1 n ~name fn ())
  in
 benchmark "Jg_template.from_string"
   (fun () -> Jg_template.from_string ~models:Test_data.models input)
