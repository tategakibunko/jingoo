open OUnit2

let () =
  run_test_tt_main ("jingoo" >::: [Test_runtime.suite; Test_output.suite])
;;
