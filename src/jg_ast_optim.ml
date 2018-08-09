open Jg_types
open Jg_ast_mapper

let rec inline_include env ast =
  (* How to include without context? *)
  let statement self = function
    | IncludeStatement (LiteralExpr (Tstr file), true) ->
      Statements(inline_include env @@ Jg_interp.ast_from_file ~env file)
    | RawIncludeStatement (LiteralExpr (Tstr file)) ->
      Statements (inline_include env @@ Jg_interp.ast_from_file ~env file)
    | e -> default_mapper.statement self e
  in
  let mapper = { Jg_ast_mapper.default_mapper with statement } in
  mapper.ast mapper ast
