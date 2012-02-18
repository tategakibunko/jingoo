open OUnit

let suite = "OUnit Test" >::: [
  Test_runtime.suite;
  Test_output.suite;
]
;;

(*
let _ =
  run_test_tt_main suite
;;
*)

let _ = 
  run_test_tt suite ~verbose:true
;;

