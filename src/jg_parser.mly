%{
(*
  jg_parser.mly

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)
  open Jg_types

  let debug = false

  let pel x = if debug then print_endline x else ()
%}

%token OPEN_EXPRESSION CLOSE_EXPRESSION
%token IF
%token ELSE
%token ELSEIF
%token ENDIF
%token FOR
%token ENDFOR
%token IN
%token SET
%token ENDSET
%token EXTENDS
%token INCLUDE
%token MACRO
%token ENDMACRO
%token FUNCTION
%token ENDFUNCTION
%token BLOCK
%token ENDBLOCK
%token FILTER
%token ENDFILTER
%token CALL
%token ENDCALL
%token IMPORT
%token AS
%token FROM
%token IS
%token WITH
%token ENDWITH
%token WITHOUT
%token CONTEXT
%token AUTOESCAPE
%token ENDAUTOESCAPE
%token RAWINCLUDE
%token EOF
%token SWITCH
%token CASE
%token DEFAULT
%token ENDSWITCH

%token FATARROW
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> TEXT
%token <string> IDENT
%token TRUE
%token FALSE
%token NULL
%token COMMA
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token QUESTION
%token COLON
%token PLUS
%token MINUS
%token TIMES
%token POWER
%token DIV
%token MOD
%token EQ
%token EQ_EQ
%token NEQ
%token LT
%token GT
%token LT_EQ
%token GT_EQ
%token AND
%token OR
%token NOT
%token DOT
%token VLINE

%right FATARROW
%right QUESTION COLON
%left OR
%left AND
%nonassoc IS IN
%left EQ_EQ NEQ
%left LT GT LT_EQ GT_EQ
%left PLUS MINUS
%left TIMES DIV MOD
%left VLINE
%right POWER
%nonassoc NOT UMINUS
%left DOT

%right LPAREN LBRACKET

%start input
%type <Jg_types.ast> input

%%

input: stmt* EOF { $1 }

%inline argument_definition: IDENT preceded(EQ, expr)? { ($1, $2) }

%inline argument_application: ioption(terminated(IDENT, EQ)) expr { ($1, $2) }

%inline alias: IDENT preceded(AS, IDENT)? { ($1, $2) }

%inline set_operator: PLUS { PLUS } | MINUS { MINUS } | DIV { DIV } | TIMES { TIMES } | MOD { MOD }


stmt:
| OPEN_EXPRESSION expr CLOSE_EXPRESSION { pel "expand expr"; ExpandStatement($2) }
| SET ident DOT IDENT set_operator? EQ expr
  { pel "set";
    let k = DotExpr ($2, $4) in
    match $5 with
    | None -> SetStatement (k, $7)
    | Some PLUS -> SetStatement (k, PlusOpExpr(k, $7))
    | Some MINUS -> SetStatement (k, MinusOpExpr(k, $7))
    | Some TIMES -> SetStatement (k, TimesOpExpr(k, $7))
    | Some DIV -> SetStatement (k, DivOpExpr(k, $7))
    | Some MOD -> SetStatement (k, ModOpExpr(k, $7))
    | Some _ -> assert false
  }
| SET ident LBRACKET expr RBRACKET set_operator? EQ expr
  { pel "set";
    let k = BracketExpr ($2, $4) in
    match $6 with
    | None -> SetStatement (k, $8)
    | Some PLUS -> SetStatement (k, PlusOpExpr(k, $8))
    | Some MINUS -> SetStatement (k, MinusOpExpr(k, $8))
    | Some TIMES -> SetStatement (k, TimesOpExpr(k, $8))
    | Some DIV -> SetStatement (k, DivOpExpr(k, $8))
    | Some MOD -> SetStatement (k, ModOpExpr(k, $8))
    | Some _ -> assert false
  }
| SET ident preceded(COMMA, ident)* set_operator? EQ expr
  {
    pel "set";
    match $2 :: $3, $6 with
    | [ IdentExpr n ], ApplyExpr (IdentExpr "namespace", init) ->
       assert ($4 = None) ;
       let extract_assign = function
         | (Some n, v) -> (n, v)
         | _ -> assert false in
       NamespaceStatement (n, List.map extract_assign init)
    | [ id ], expr ->
       begin
         let k = SetExpr [ id ] in
         match $4 with
         | None -> SetStatement (k, expr)
         | Some PLUS -> SetStatement (k, PlusOpExpr(id, expr))
         | Some MINUS -> SetStatement (k, MinusOpExpr(id, expr))
         | Some TIMES -> SetStatement (k, TimesOpExpr(id, expr))
         | Some DIV -> SetStatement (k, DivOpExpr(id, expr))
         | Some MOD -> SetStatement (k, ModOpExpr(id, expr))
         | Some _ -> assert false
       end
    | idents, exprs ->
       assert ($4 = None) ;
       pel "set sts";
       SetStatement (SetExpr idents, exprs)
  }
| SET IDENT stmt* ENDSET
  { pel "set_block";
    SetBlockStatement($2, $3)
  }
| EXTENDS STRING { pel "extends sts"; ExtendsStatement($2) }
| BLOCK IDENT stmt* ENDBLOCK { pel "block sts2"; BlockStatement($2, $3) }
| FILTER IDENT stmt* ENDFILTER { pel "filter sts"; FilterStatement($2, $3) }
| INCLUDE expr context? { pel "include sts"; IncludeStatement($2, $3 <> Some false) }
| RAWINCLUDE expr { pel "raw include sts"; RawIncludeStatement($2) }
| IMPORT STRING preceded(AS, IDENT)? { pel "import sts"; ImportStatement($2, $3) }
| FROM STRING IMPORT separated_list(COMMA, alias) { pel "from import sts"; FromImportStatement($2, $4) }
| MACRO IDENT LPAREN separated_list(COMMA, argument_definition) RPAREN stmt* ENDMACRO IDENT?
  { pel "macro sts"; (match $8 with Some n -> assert ($2 = n) | _ -> ()) ; MacroStatement($2, $4, $6) }
| FUNCTION IDENT LPAREN separated_list(COMMA, argument_definition) RPAREN stmt* ENDFUNCTION
  { pel "function sts"; FunctionStatement($2, $4, $6) }
| CALL opt_args IDENT LPAREN separated_list(COMMA, argument_application) RPAREN stmt* ENDCALL
  { pel "call sts"; CallStatement($3, $2, $5, $7) }
| IF
  i = pair(expr, stmt*)
  ei = preceded(ELSEIF, pair(expr, stmt*))*
  e = preceded(ELSE, stmt*)?
  ENDIF
  {
  pel "if sts";
  IfStatement (List.fold_right
                 (fun (a, b) acc -> (Some a, b) :: acc) (i :: ei)
                 (match e with None -> [] | Some stmts -> [ (None, stmts) ]))
  }
| SWITCH
  e=expr
  ws=TEXT?
  cases=preceded(CASE, pair(expr, stmt*))*
  default=preceded(DEFAULT, stmt*)?
  ENDSWITCH
  {
    (match ws with Some s -> assert (String.trim s = "") | None -> ()) ;
    let rec extract = function
      | OrOpExpr (e1, e2) -> extract e1 @ extract e2
      | e -> [e] in
    SwitchStatement ( e
                    , List.fold_right
                        (fun (a, b) acc -> (extract a, b) :: acc) (cases)
                        (match default with None -> [] | Some stmts -> [ ([], stmts) ]))
  }
| FOR LPAREN? separated_nonempty_list(COMMA, IDENT) RPAREN? IN expr stmt* ENDFOR
  { pel "for sts"; ForStatement($3, $6, $7) }
| WITH separated_list(COMMA, separated_pair(IDENT, EQ, expr)) stmt* ENDWITH
  { pel "with sts1"; WithStatement($2, $3) }
| AUTOESCAPE expr stmt* ENDAUTOESCAPE { pel "autoescape"; AutoEscapeStatement($2, $3) }
| TEXT { pel "text sts"; TextStatement($1) }
;

%inline context:
| WITH CONTEXT { true }
| WITHOUT CONTEXT { false }
;

%inline ident: IDENT { IdentExpr $1 }

%inline objkey: IDENT {  $1 } | STRING { $1 }

expr:
  ident { pel "ident"; $1 }
| INT { pel "int"; LiteralExpr (Tint $1) }
| FLOAT { pel "float"; LiteralExpr (Tfloat $1) }
| TRUE { pel "true"; LiteralExpr (Tbool true) }
| FALSE { pel "false"; LiteralExpr (Tbool false) }
| STRING { pel "string"; LiteralExpr (Tstr $1) }
| NULL { pel "null"; LiteralExpr Tnull }
| expr DOT IDENT { pel "dot_lookup"; DotExpr($1, $3) }
| expr LBRACKET expr RBRACKET { pel "bracket_lookup"; BracketExpr($1, $3) }
| NOT expr { pel "not expr"; NotOpExpr($2) }
| MINUS expr %prec UMINUS { pel "negative"; NegativeOpExpr($2) }
| LBRACKET separated_list(COMMA, expr) RBRACKET { pel "list expr"; ListExpr($2) }
| LBRACE o=separated_list(COMMA, separated_pair(objkey, COLON, expr)) RBRACE
  { pel "obj expr"; ObjExpr o }
| expr PLUS expr { pel "plus"; PlusOpExpr($1, $3) }
| expr MINUS expr { pel "minus"; MinusOpExpr($1, $3) }
| expr DIV expr { pel "div"; DivOpExpr($1, $3) }
| expr MOD expr { pel "mod"; ModOpExpr($1, $3) }
| expr TIMES expr { pel "times"; TimesOpExpr($1, $3) }
| expr POWER expr { pel "power"; PowerOpExpr($1, $3) }
| expr AND expr { pel "and"; AndOpExpr($1, $3) }
| expr OR expr { pel "or"; OrOpExpr($1, $3) }
| expr EQ_EQ expr { pel "eqeq"; EqEqOpExpr($1, $3) }
| expr NEQ expr { pel "noteq"; NotEqOpExpr($1, $3) }
| expr LT expr { pel "lt"; LtOpExpr($1, $3) }
| expr GT expr { pel "gt"; GtOpExpr($1, $3) }
| expr LT_EQ expr { pel "lteq"; LtEqOpExpr($1, $3) }
| expr GT_EQ expr { pel "gteq"; GtEqOpExpr($1, $3) }
| expr IN expr { pel "inop"; InOpExpr($1, $3) }
| expr VLINE expr { pel "expr|expr -> ApplyExpr"; ApplyExpr($3, [None, $1]) }
| expr IS ident expr {
  (* when expr1 is fun and expr2 is args without LPAREN and RPAREN. *)
  (* for example, 'a is divisableby 2' *)
  pel "test(apply)";
  TestOpExpr($1, ApplyExpr($3, [None, $4]))
}
| expr IS ident { pel "test"; TestOpExpr($1,$3) }
| DEFAULT LPAREN separated_list(COMMA, argument_application) RPAREN { pel "apply(expr_list)"; ApplyExpr(IdentExpr("default"), $3) }
| expr LPAREN separated_list(COMMA, argument_application) RPAREN { pel "apply(expr_list)"; ApplyExpr($1, $3) }
| LPAREN separated_list(COMMA, expr) RPAREN
  { pel "set expr"; match $2 with [ e ] -> e | _ -> SetExpr $2 }
| expr FATARROW expr
  { pel "fat arrow";
    let args = match $1 with
      | IdentExpr i -> [ i ]
      | SetExpr set -> List.map (function IdentExpr i -> i | _ -> assert false) set
      | _ -> assert false
    in
    FunctionExpression(args, $3)
}
| expr QUESTION expr COLON expr { TernaryOpExpr ($1, $3, $5) }
;

opt_args:
/* empty */ { pel "opt_args empty"; [] }
| LPAREN separated_list(COMMA, argument_definition) RPAREN { $2 }
;
