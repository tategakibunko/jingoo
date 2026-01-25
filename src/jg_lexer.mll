{
  (*
    jg_lexer.mll

    Copyright (c) 2011- by Masaki WATANABE

    License: see LICENSE
  *)
  open Jg_parser

  type lexer_mode = [ `Logic | `Html ]
  type lexer_context = {
    mutable mode : lexer_mode;
    mutable terminator : string option;
    mutable token_required : bool ;
    buf : Buffer.t;
    mutable queue : token list;
  }

  let new_context () = {
    mode = `Html;
    terminator = None;
    token_required = false;
    buf = Buffer.create 256;
    queue = [];
  }

  let (@@) f g = f g

  let spf = Printf.sprintf

  let fail
      { Lexing.lex_curr_p = { Lexing.pos_fname ; pos_lnum ; pos_bol ; pos_cnum } ; _ } str =
    failwith @@
    spf "File '%s', line %d, char %d: %s" pos_fname pos_lnum (pos_cnum - pos_bol) str

  let init_lexer_pos fname lexbuf =
    let curr = lexbuf.Lexing.lex_curr_p in
    let pos : Lexing.position = {
      Lexing.pos_fname = (match fname with Some fname -> fname | _ -> "");
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = curr.Lexing.pos_lnum;
      Lexing.pos_cnum = 0
    } in
    lexbuf.Lexing.lex_curr_p <- pos

  let update_context ctx mode terminator =
    let token_required =
      match terminator with
        Some ("%}" | "-%}") -> true
      | _ -> false in
    ctx.mode <- mode;
    ctx.terminator <- terminator;
    ctx.token_required <- token_required

  let reset_context ctx =
    ctx.mode <- `Html;
    ctx.terminator <- None;
    ctx.token_required <- false;
    Buffer.reset ctx.buf

  let get_buf ctx =
    let text = Buffer.contents ctx.buf in
    Buffer.reset ctx.buf;
    text

  let add_char ctx c1 =
    Buffer.add_char ctx.buf c1

  let add_str ctx str =
    Buffer.add_string ctx.buf str

  let token_or_str (str, token) lexer ctx lexb =
    match ctx.mode with
      | `Logic ->
        (* print_endline @@ spf "logical token:%s" str; *)
        ctx.token_required <- false ;
        token
      | `Html ->
        add_str ctx str;
        lexer ctx lexb

  let token_or_char (chr, token) lexer ctx lexb =
    match ctx.mode with
      | `Logic ->
        (* print_endline @@ spf "logical token:%c" chr; *)
        ctx.token_required <- false ;
        token
      | `Html ->
        add_char ctx chr;
        lexer ctx lexb
}

let blank = [ ' ' '\t' ]
let ident_first_char = [ 'A'-'Z' 'a'-'z' '_' ]
let ident_char =  [ 'A'-'Z' 'a'-'z' '_' '0'-'9' ]
let int_literal = ['0'-'9'] ['0'-'9']*
let float_literal = ['0'-'9']+('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
let newline = [ '\n' ]

rule main ctx = parse
| "" { match ctx.queue with [] -> main_bis ctx lexbuf | hd :: tl -> ctx.queue <- tl ; hd }
| eof {
    match get_buf ctx with
    | "" -> begin match ctx.queue with
        | [] -> EOF
        | hd :: tl -> ctx.queue <- tl ; hd
      end
    | s -> TEXT s
  }

and main_bis ctx = parse
  | '\\' '{' {
    add_char ctx '{';
    main ctx lexbuf
  }
  | '\\' '}' {
    add_char ctx '}';
    main ctx lexbuf
  }
  | ("{%" | (blank | newline)* "{%-")
       blank* "raw" blank*
     ("%}" | "-%}" (blank | newline)*) as str {
    String.iter (function '\n' -> Lexing.new_line lexbuf | _ -> () ) str;
    raw ctx lexbuf
  }
  | ("{%" | (blank | newline)* "{%-") as str {
    String.iter (function '\n' -> Lexing.new_line lexbuf | _ -> () ) str;
    if ctx.mode = `Logic then fail lexbuf @@ "Unexpected '{%' token" ;
    update_context ctx `Logic (Some "%}");
    match get_buf ctx with
      | "" -> main ctx lexbuf
      | content -> TEXT content
  }
  | "{{" {
    if ctx.mode = `Logic then fail lexbuf @@ "Unexpected '{{' token" ;
    update_context ctx `Logic (Some "}}");
    (* print_endline @@ spf "text:%s" (Buffer.contents buf); *)
    ctx.queue <- ctx.queue @ [ OPEN_EXPRESSION ] ;
    match get_buf ctx with
      | "" -> main ctx lexbuf
      | content -> TEXT content
  }
  | "{#" { comment ctx lexbuf }
  | '\n' as c {
    Lexing.new_line lexbuf;
    if ctx.mode = `Html then
      add_char ctx c
    ;
    main ctx lexbuf
  }
  | "}}" as str {
    match ctx.terminator with
      | None ->
	add_str ctx str; main ctx lexbuf
      | Some "}}" ->
	update_context ctx `Html None;
        CLOSE_EXPRESSION
      | _ -> fail lexbuf @@ spf "syntax error '%s'" str
  }
  | ("%}" | "-%}" (blank | newline)*) as str {
    String.iter (function '\n' -> Lexing.new_line lexbuf | _ -> () ) str ;
    match ctx.terminator with
      | None ->
	add_str ctx str; main ctx lexbuf
      | Some "%}" ->
	update_context ctx `Html None;
	main ctx lexbuf
      | _ -> fail lexbuf @@ spf "syntax error '%s'" str
  }
  | '\"' as c {
    match ctx.mode with
      | `Html -> add_char ctx c; main ctx lexbuf
      | _ -> string_literal c ctx lexbuf
  }
  | '\'' as c {
    match ctx.mode with
      | `Html -> add_char ctx c; main ctx lexbuf
      | _ -> string_literal c ctx lexbuf
  }
  | int_literal as str {
    match ctx.mode with
      | `Html -> add_str ctx str; main ctx lexbuf
      | _ -> INT (int_of_string str)
  }
  | float_literal as str {
    match ctx.mode with
      | `Html -> add_str ctx str; main ctx lexbuf
      | _ -> FLOAT (float_of_string str)
  }
  | "if" as s { token_or_str (s, IF) main ctx lexbuf }
  | "else" as s { token_or_str (s, ELSE) main ctx lexbuf }
  | ("elseif" | "elif" | "else if") as s { token_or_str (s, ELSEIF) main ctx lexbuf }
  | "endif" as s { token_or_str (s, ENDIF) main ctx lexbuf }
  | "switch" as s { token_or_str (s, SWITCH) main ctx lexbuf }
  | "case" as s { token_or_str (s, CASE) main ctx lexbuf }
  | "default" as s { token_or_str (s, DEFAULT) main ctx lexbuf }
  | "endswitch" as s { token_or_str (s, ENDSWITCH) main ctx lexbuf }
  | "for" as s { token_or_str (s, FOR) main ctx lexbuf }
  | "endfor" as s { token_or_str (s, ENDFOR) main ctx lexbuf }
  | "include" as s { token_or_str (s, INCLUDE) main ctx lexbuf }
  | "extends" as s { token_or_str (s, EXTENDS) main ctx lexbuf }
  | "block" as s { token_or_str (s, BLOCK) main ctx lexbuf }
  | "endblock" as s { token_or_str (s, ENDBLOCK) main ctx lexbuf }
  | "filter" as s { token_or_str (s, FILTER) main ctx lexbuf }
  | "endfilter" as s { token_or_str (s, ENDFILTER) main ctx lexbuf }
  | "macro" as s { token_or_str (s, MACRO) main ctx lexbuf }
  | "endmacro" as s { token_or_str (s, ENDMACRO) main ctx lexbuf }
  | "function" as s { token_or_str (s, FUNCTION) main ctx lexbuf }
  | "endfunction" as s { token_or_str (s, ENDFUNCTION) main ctx lexbuf }
  | "call" as s { token_or_str (s, CALL) main ctx lexbuf }
  | "endcall" as s { token_or_str (s, ENDCALL) main ctx lexbuf }
  | "import" as s { token_or_str (s, IMPORT) main ctx lexbuf }
  | "as" as s { token_or_str (s, AS) main ctx lexbuf }
  | "from" as s { token_or_str (s, FROM) main ctx lexbuf }
  | "in" as s { token_or_str (s, IN) main ctx lexbuf }
  | "set" as s { token_or_str (s, SET) main ctx lexbuf }
  | "endset" as s { token_or_str (s, ENDSET) main ctx lexbuf }
  | "not" as s { token_or_str (s, NOT) main ctx lexbuf }
  | "is" as s { token_or_str (s, IS) main ctx lexbuf }
  | "with" as s { token_or_str (s, WITH) main ctx lexbuf }
  | "endwith" as s { token_or_str (s, ENDWITH) main ctx lexbuf }
  | "without" as s { token_or_str (s, WITHOUT) main ctx lexbuf }
  | "context" as s { token_or_str (s, CONTEXT) main ctx lexbuf }
  | "autoescape" as s { token_or_str (s, AUTOESCAPE) main ctx lexbuf }
  | "endautoescape" as s { token_or_str (s, ENDAUTOESCAPE) main ctx lexbuf }
  | "rawinclude" as s { token_or_str (s, RAWINCLUDE) main ctx lexbuf }
  | "true" as s { token_or_str (s, TRUE) main ctx lexbuf }
  | "false" as s { token_or_str (s, FALSE) main ctx lexbuf }
  | "null" as s { token_or_str (s, NULL) main ctx lexbuf }
  | "==" as s { token_or_str (s, EQ_EQ) main ctx lexbuf }
  | "!=" as s { token_or_str (s, NEQ) main ctx lexbuf }
  | "<=" as s { token_or_str (s, LT_EQ) main ctx lexbuf }
  | ">=" as s { token_or_str (s, GT_EQ) main ctx lexbuf }
  | ("&&" | "and") as s { token_or_str (s, AND) main ctx lexbuf }
  | ("||" | "or") as s { token_or_str (s, OR) main ctx lexbuf }
  | "**" as s { token_or_str (s, POWER) main ctx lexbuf }
  | ","  as c { token_or_char (c, COMMA) main ctx lexbuf }
  | "=" as c { token_or_char (c, EQ) main ctx lexbuf }
  | "<" as c { token_or_char (c, LT) main ctx lexbuf }
  | ">" as c { token_or_char (c, GT) main ctx lexbuf }
  | "!" as c { token_or_char (c, NOT) main ctx lexbuf }
  | "." as c { token_or_char (c, DOT) main ctx lexbuf }
  | "+" as c { token_or_char (c, PLUS) main ctx lexbuf }
  | "-" as c { token_or_char (c, MINUS) main ctx lexbuf }
  | "*" as c { token_or_char (c, TIMES) main ctx lexbuf }
  | "/" as c { token_or_char (c, DIV) main ctx lexbuf }
  | "%" as c { token_or_char (c, MOD) main ctx lexbuf }
  | "(" as c { token_or_char (c, LPAREN) main ctx lexbuf }
  | ")" as c { token_or_char (c, RPAREN) main ctx lexbuf }
  | "[" as c { token_or_char (c, LBRACKET) main ctx lexbuf }
  | "]" as c { token_or_char (c, RBRACKET) main ctx lexbuf }
  | "{" as c { token_or_char (c, LBRACE) main ctx lexbuf }
  | "}" as c { token_or_char (c, RBRACE) main ctx lexbuf }
  | ":" as c { token_or_char (c, COLON) main ctx lexbuf }
  | "?" as c { token_or_char (c, QUESTION) main ctx lexbuf }
  | "|" as c { token_or_char (c, VLINE) main ctx lexbuf }
  | "=>" as s { token_or_str (s, FATARROW) main ctx lexbuf }
  | ident_first_char ident_char* as str {
    if ctx.token_required then
      fail lexbuf @@ spf "syntax error: expected token, got '%s'" str
    ;
    match ctx.mode with
      | `Html ->
	(* print_endline @@ spf "html ident:%s" str; *)
	add_str ctx str; main ctx lexbuf
      | _ ->
	(* print_endline @@ spf "logical ident:%s" str; *)
	IDENT str
  }
  | blank as c {
    match ctx.mode with
      | `Html -> add_char ctx c; main ctx lexbuf
      | _ -> main ctx lexbuf
  }
  | _ as c {
    if c = '\n' then Lexing.new_line lexbuf;
    match ctx.mode with
      | `Html -> add_char ctx c; main ctx lexbuf
      | _ -> fail lexbuf @@ spf "unexpected token:%c" c
  }

and comment ctx = parse
  | "#}" { main ctx lexbuf }
  | _ as c {
    if c = '\n' then Lexing.new_line lexbuf;
    comment ctx lexbuf
  }

and raw ctx = parse
  | ("{%" | (blank | newline)* "{%-")
      blank* "endraw" blank*
    ("%}" | "-%}" (blank | newline)*) as str {
    String.iter (function '\n' -> Lexing.new_line lexbuf | _ -> () ) str ;
    TEXT (get_buf ctx)
  }
  | _ as c {
    if c = '\n' then Lexing.new_line lexbuf;
    add_char ctx c;
    raw ctx lexbuf
  }

and string_literal terminator ctx = parse
  | '\\' (_ as c) {
    let chr =
      match c with
      | '\\' -> '\\'
      | 'n' -> '\n'
      | 't' -> '\t'
      | 'r' -> '\r'
      | c when c = terminator -> c
      | c -> fail lexbuf @@ spf "illegal backslash escape:%c" c
    in
    add_char ctx chr;
    string_literal terminator ctx lexbuf
  }
  | _ as c {
    if c = terminator then begin
      (* print_endline @@ spf "string literal:(%s)" (Buffer.contents buf); *)
      STRING (get_buf ctx)
    end else begin
      if c = '\n' then Lexing.new_line lexbuf;
      add_char ctx c;
      string_literal terminator ctx lexbuf
    end
  }
