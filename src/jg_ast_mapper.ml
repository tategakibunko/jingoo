open Jg_types

type jg_ast_mapper =
  { ast : jg_ast_mapper -> ast -> ast
  ; statement : jg_ast_mapper -> statement -> statement
  ; expression : jg_ast_mapper -> expression -> expression
  }

(** {!type:Jg_ast_mapper.jg_ast_mapper} allows to implement AST rewriting.

    A typical mapper would be based on {!val:Jg_ast_mapper.default_mapper},
    a {b deep identity} mapper, and will fall back on it for handling the syntax it does not modify.

    {{:#type-jg_ast_mapper.ast} ast}, {{:#type-jg_ast_mapper.statement} statement} and
    {{:#type-jg_ast_mapper.expression} expression} expect a first argument being the mapper
    currently used.

    For example, {!val:Jg_interp.inline_include} defines an ast mapper replacing
    [{% include %}] statements by actual code (statements) from these included files.

    {[
    let open Jg_ast_mapper in
      let statement self = function
        | IncludeStatement (LiteralExpr (Tstr file), true) ->
          Statements (self.ast self @@ ast_from_file ~env file)
        | RawIncludeStatement (LiteralExpr (Tstr file)) ->
          Statements (self.ast self @@ ast_from_file ~env file)
        | e -> default_mapper.statement self e in
      { default_mapper with statement }
    ]}
 *)

let arguments_definition self =
  List.map (function
      | (k, Some v) -> (k, Some (self.expression self v))
      | (k, None) -> (k, None))

let arguments_application self =
  List.map (fun (k, v) -> (k, self.expression self v))

(**/**)
let ast self = List.map (self.statement self)

and statement self stmt : statement = match stmt with

  | TextStatement _ ->
    stmt

  | ExpandStatement e ->
    ExpandStatement (self.expression self e)

  | IfStatement (branches) ->
    IfStatement (List.map (fun (e, ast) -> ( (match e with Some e -> Some (self.expression self e) | None -> None)
                                           , self.ast self ast) ) branches)

  | SwitchStatement (e, cases) ->
    SwitchStatement ( self.expression self e
                    , List.map (fun (e, ast) -> ( List.map (self.expression self) e
                                                , self.ast self ast) ) cases)

  | ForStatement (ids, e2, ast) ->
    ForStatement (ids, self.expression self e2, self.ast self ast)

  | IncludeStatement (e, b) ->
    IncludeStatement (self.expression self e, b)

  | RawIncludeStatement e ->
    RawIncludeStatement (self.expression self e)

  | ExtendsStatement _ ->
    stmt

  | ImportStatement _ ->
    stmt

  | FromImportStatement (str, el) ->
    FromImportStatement (str, el)

  | SetStatement (e1, e2) ->
    SetStatement (self.expression self e1, self.expression self e2)

  | SetBlockStatement (n, ast) ->
    SetBlockStatement (n, self.ast self ast)

  | BlockStatement (n, ast) ->
    BlockStatement (n, self.ast self ast)

  | MacroStatement (n, args, ast) ->
    MacroStatement ( n
                   , arguments_definition self args
                   , self.ast self ast)

  | FunctionStatement (n, args, ast) ->
    FunctionStatement ( n
                      , arguments_definition self args
                      , self.ast self ast)

  | FilterStatement (n, ast) ->
    FilterStatement (n, self.ast self ast)

  | CallStatement (n, a1, a2, ast) ->
    CallStatement ( n
                  , arguments_definition self a1
                  , arguments_application self a2
                  , self.ast self ast)

  | WithStatement (el, ast) ->
    WithStatement ( List.map (fun (n, e) -> (n, self.expression self e)) el
                  , self.ast self ast)

  | AutoEscapeStatement (e, ast) ->
    AutoEscapeStatement (self.expression self e, self.ast self ast)

  | NamespaceStatement (str, str_e_l) ->
    NamespaceStatement
      ( str
      , List.map (fun (s, e) -> (s, self.expression self e) ) str_e_l)

  | Statements ast ->
    Statements (self.ast self ast)

and expression self expr = match expr with

  | IdentExpr _ ->
    expr

  | LiteralExpr _ ->
    expr

  | NotOpExpr e ->
    NotOpExpr (self.expression self e)

  | NegativeOpExpr e ->
    NegativeOpExpr (self.expression self e)

  | PlusOpExpr (e1, e2) ->
    PlusOpExpr (self.expression self e1, self.expression self e2)

  | MinusOpExpr (e1, e2) ->
    MinusOpExpr (self.expression self e1, self.expression self e2)

  | TimesOpExpr (e1, e2) ->
    TimesOpExpr (self.expression self e1, self.expression self e2)

  | PowerOpExpr (e1, e2) ->
    PowerOpExpr (self.expression self e1, self.expression self e2)

  | DivOpExpr (e1, e2) ->
    DivOpExpr (self.expression self e1, self.expression self e2)

  | ModOpExpr (e1, e2) ->
    ModOpExpr (self.expression self e1, self.expression self e2)

  | AndOpExpr (e1, e2) ->
    AndOpExpr (self.expression self e1, self.expression self e2)

  | OrOpExpr (e1, e2) ->
    OrOpExpr (self.expression self e1, self.expression self e2)

  | NotEqOpExpr (e1, e2) ->
    NotEqOpExpr (self.expression self e1, self.expression self e2)

  | EqEqOpExpr (e1, e2) ->
    EqEqOpExpr (self.expression self e1, self.expression self e2)

  | LtOpExpr (e1, e2) ->
    LtOpExpr (self.expression self e1, self.expression self e2)

  | GtOpExpr (e1, e2) ->
    GtOpExpr (self.expression self e1, self.expression self e2)

  | LtEqOpExpr (e1, e2) ->
    LtEqOpExpr (self.expression self e1, self.expression self e2)

  | GtEqOpExpr (e1, e2) ->
    GtEqOpExpr (self.expression self e1, self.expression self e2)

  | DotExpr (e, str) ->
    DotExpr (self.expression self e, str)

  | BracketExpr (e1, e2) ->
    BracketExpr (self.expression self e1, self.expression self e2)

  | ApplyExpr (e, args) ->
    ApplyExpr ( self.expression self e
              , arguments_application self args)

  | ListExpr el ->
    ListExpr (List.map (self.expression self) el)

  | SetExpr el ->
    SetExpr (List.map (self.expression self) el)

  | ObjExpr (eel) ->
    ObjExpr (List.map (fun (k, v) -> k, self.expression self v) eel)

  | TestOpExpr (e1, e2) ->
    TestOpExpr (self.expression self e1, self.expression self e2)

  | InOpExpr (e1, e2) ->
    InOpExpr (self.expression self e1, self.expression self e2)

  | FunctionExpression (args, body) ->
    FunctionExpression (args, self.expression self body)

  | TernaryOpExpr (c, y, n) ->
    TernaryOpExpr (self.expression self c, self.expression self y, self.expression self n)

(**/**)

let default_mapper =
  { ast
  ; statement
  ; expression
  }
