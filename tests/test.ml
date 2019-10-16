open OUnit2

let () =
  run_test_tt_main ("jingoo" >::: [ Test_runtime.suite
                                  ; Test_output.suite
                                  ; Test_parser.suite
                                  ; Test_dead_code_elimination.suite
                                  ])
