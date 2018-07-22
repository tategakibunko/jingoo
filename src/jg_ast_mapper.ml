open Jg_types

type jg_ast_mapper =
  { ast : jg_ast_mapper -> ast -> ast
  ; statement : jg_ast_mapper -> statement -> statement
  ; expression : jg_ast_mapper -> expression -> expression
  }

let ast self = List.map (self.statement self)

and statement self stmt : statement = match stmt with

  | TextStatement _ ->
    stmt

  | ExpandStatement e ->
    ExpandStatement (self.expression self e)

  | IfStatement (cond_branch, ast) ->
    IfStatement ( List.map (fun (e, ast) -> ( self.expression self e
                                            , self.ast self ast) ) cond_branch
                , self.ast self ast)

  | ForStatement (e1, e2, ast) ->
    ForStatement ( self.expression self e1, self.expression self e2, self.ast self ast)

  | IncludeStatement (e, b) ->
    IncludeStatement (self.expression self e, b)

  | RawIncludeStatement e ->
    RawIncludeStatement (self.expression self e)

  | ExtendsStatement _ ->
    stmt

  | ImportStatement _ ->
    stmt

  | FromImportStatement (str, el) ->
    FromImportStatement (str, List.map (self.expression self) el)

  | SetStatement (e1, e2) ->
    SetStatement (self.expression self e1, self.expression self e2)

  | BlockStatement (e, ast) ->
    BlockStatement (self.expression self e, self.ast self ast)

  | MacroStatement (e, args, ast) ->
    MacroStatement ( self.expression self e
                   , List.map (self.expression self) args
                   , self.ast self ast)

  | FilterStatement (e, ast) ->
    FilterStatement (self.expression self e, self.ast self ast)

  | CallStatement (e, a1, a2, ast) ->
    CallStatement ( self.expression self e
                  , List.map (self.expression self) a1
                  , List.map (self.expression self) a2
                  , self.ast self ast)

  | WithStatement (el, ast) ->
    WithStatement ( List.map (self.expression self) el
                  , self.ast self ast)

  | AutoEscapeStatement (e, ast) ->
    AutoEscapeStatement (self.expression self e, self.ast self ast)

  | NamespaceStatement (str, str_e_l) ->
    NamespaceStatement
      ( str
      , List.map (fun (s, e) -> (s, self.expression self e) ) str_e_l)


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
    DotExpr (e, str)

  | BracketExpr (e1, e2) ->
    BracketExpr (self.expression self e1, self.expression self e2)

  | ApplyExpr (e, args) ->
    ApplyExpr ( self.expression self e
              , List.map (self.expression self) args)

  | ListExpr el ->
    ListExpr (List.map (self.expression self) el)

  | SetExpr el ->
    SetExpr (List.map (self.expression self) el)

  | ObjExpr (eel) ->
    ObjExpr (List.map (fun (e1, e2) -> self.expression self e1, self.expression self e2) eel)

  | TestOpExpr (e1, e2) ->
    TestOpExpr (self.expression self e1, self.expression self e2)

  | KeywordExpr (e1, e2) ->
    KeywordExpr (self.expression self e1, self.expression self e2)

  | AliasExpr (e1, e2) ->
    AliasExpr (self.expression self e1, self.expression self e2)

  | InOpExpr (e1, e2) ->
    InOpExpr (self.expression self e1, self.expression self e2)


let default_mapper =
  { ast
  ; statement
  ; expression
  }
