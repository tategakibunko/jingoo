open Jg_types
open Jg_ast_mapper

let rec ast_from_file env self file =
  let file = Jg_utils.get_file_path file ~template_dirs:env.template_dirs in
  let ch = open_in file in
  let lexbuf = Lexing.from_channel ch in
  Jg_lexer.reset_context () ;
  Jg_lexer.init_lexer_pos (Some file) lexbuf ;
  let ctx = Jg_interp.init_context ~output:(fun _ -> assert false) () in
  let ast = Jg_parser.input Jg_lexer.main lexbuf in
  let ast = Jg_interp.unfold_extends env ctx ast in
  let ast = Jg_interp.align_block ast in
  let ast = self.ast self ast in
  let ast = Jg_interp.align_block ast in
  close_in ch ;
  inline_include env ast

and inline_include env ast =
  (* How to include without context? *)
  let statement self = function
    | IncludeStatement (LiteralExpr (Tstr file), true) ->
      BlockStatement(LiteralExpr Tnull, ast_from_file env self file)
    | RawIncludeStatement (LiteralExpr (Tstr file)) ->
      BlockStatement (LiteralExpr Tnull, ast_from_file env self file)
    | e -> default_mapper.statement self e
  in
  let mapper = { Jg_ast_mapper.default_mapper with statement } in
  mapper.ast mapper ast
