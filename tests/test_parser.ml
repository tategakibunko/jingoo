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
    with e -> failwith (Jg_utils.get_parser_error e lexbuf)
  in
  assert_equal ~printer:Jg_types.show_ast expected ast

let suite =
  "Parser" >:::
  [ "Ternary conditionnal operator 1" >:: begin fun _ctx ->
        assert_eq
          [ ExpandStatement (TernaryOpExpr ( LiteralExpr (Tbool true)
                                           , LiteralExpr (Tint 1)
                                           , LiteralExpr (Tint 2) ) ) ]
          "{{ true ? 1 : 2 }}"
      end

  ; "Ternary conditionnal operator 2" >:: begin fun _ctx ->
        assert_eq
          [ ExpandStatement (TernaryOpExpr ( LiteralExpr (Tbool true)
                                           , TernaryOpExpr ( LiteralExpr (Tbool false)
                                                           , LiteralExpr (Tint 1)
                                                           , LiteralExpr (Tint 2) )
                                           , LiteralExpr (Tint 3) ) ) ]
          "{{ true ? false ? 1 : 2 : 3 }}"
      end

  ; "Ternary conditionnal operator 3" >:: begin fun _ctx ->
        assert_eq
          [ ExpandStatement (TernaryOpExpr ( LiteralExpr (Tbool true)
                                           , LiteralExpr (Tint 1)
                                           , TernaryOpExpr ( LiteralExpr (Tbool false)
                                                           , LiteralExpr (Tint 2)
                                                           , LiteralExpr (Tint 3) ) ) ) ]
          "{{ true ? 1 : false ? 2 : 3 }}"
      end

  ; "{% set +=, -=, *=, /=, %= %} 3" >:: begin fun _ctx ->
      let aux op expr =
        assert_eq
          [ SetStatement ( SetExpr [ IdentExpr "x" ]
                         , expr
                             (IdentExpr "x")
                             (PlusOpExpr ( LiteralExpr (Tint 2)
                                         , LiteralExpr (Tint 2) ) ) ) ]
          ("{% set x " ^ op ^ " 2 + 2 %}")
      in
      aux "+=" (fun a b -> PlusOpExpr (a , b) )
    ; aux "-=" (fun a b -> MinusOpExpr (a , b) )
    ; aux "*=" (fun a b -> TimesOpExpr (a , b) )
    ; aux "/=" (fun a b -> DivOpExpr (a , b) )
    ; aux "%=" (fun a b -> ModOpExpr (a , b) )
    end

  ]
