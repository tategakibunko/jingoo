open OUnit2
open Jingoo
open Jg_types

let assert_eq expected source =
  let lexbuf = Lexing.from_string source in
  Jg_lexer.reset_context () ;
  Jg_lexer.init_lexer_pos None lexbuf ;
  let ast =
    try
      Jg_parser.input Jg_lexer.main lexbuf
      |> Jg_ast_optimize.dead_code_elimination
    with e -> failwith (Jg_utils.get_parser_error e lexbuf)
  in
  assert_equal ~printer:Jg_types.show_ast expected ast

let test_0 _ =
  assert_eq
    [ FunctionStatement ( "foo"
                        , []
                        , [ ExpandStatement (LiteralExpr (Tint 42))
                          ] )
    ; ExpandStatement (ApplyExpr (IdentExpr "foo", []) )
    ]
    "{% function foo () %}{{ 42 }}{% endfunction %}\
    {{ foo () }}"

let test_1 _ =
  assert_eq
    [ Statements [] ]
    "{% function foo () %}{{ 42 }}{% endfunction %}"

let test_2 _ =
  assert_eq
    [ Statements []
    ; Statements []
    ; Statements [] ]
    "{% function foo () %}{{ foo () }}{% endfunction %}\
     {% function bar () %}{{ baz () }}{% endfunction %}\
     {% function baz () %}{{ bar () }}{% endfunction %}\
    "

let test_3 _ =
  assert_eq
    [ Statements []
    ; ForStatement ( [ "foo" ]
                   , IdentExpr "bar"
                   , [ ExpandStatement (ApplyExpr (IdentExpr "foo", []) ) ])
    ]
    "{% function foo () %}{{ 42 }}{% endfunction %}\
     {% for foo in bar %}{{ foo () }}{% endfor %}\
    "

let test_4 _ =
  assert_eq
    [ Statements []
    ; SetStatement ( SetExpr [ IdentExpr "foo" ], LiteralExpr (Tint 42))
    ; ExpandStatement (IdentExpr "foo")
    ]
    "{% function foo () %}{{ 42 }}{% endfunction %}\
     {% set foo = 42 %}\
     {{ foo }}\
    "

let test_5 _ =
  assert_eq
    [ FunctionStatement ( "foo"
                        , []
                        , [ ExpandStatement (ObjExpr [ "val", LiteralExpr (Tint 42) ])
                          ] )
    ; IfStatement
        [ Some ( LiteralExpr (Tbool true) )
        , [ ForStatement ( [ "x" ], ListExpr []
                         , [ ExpandStatement (DotExpr (ApplyExpr (IdentExpr "foo", []), "val") ) ]) ] ]
    ]
    "{% function foo () %}{{ { val: 42 } }}{% endfunction %}\
     {% if true %}\
     {% for x in [] %}{{ foo().val }}{% endfor %}\
     {% endif %}"

let suite = "runtime test" >::: [
  "test_1" >:: test_1;
  "test_0" >:: test_0;
  "test_2" >:: test_2;
  "test_3" >:: test_3;
  "test_4" >:: test_4;
  "test_5" >:: test_5;
]
