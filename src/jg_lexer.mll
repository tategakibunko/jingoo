{
  (*
    jg_lexer.mll

    Copyright (c) 2011- by Masaki WATANABE

    License: see LICENSE
  *)
  open Jg_types
  open Jg_parser

  type lexer_mode = [`Logic | `Html]
  type lexer_context = {
    mutable mode : lexer_mode;
    mutable terminator : string option;
    mutable eof : bool;
    mutable token_required : bool ;
  }

  let ctx = {
    mode = `Html;
    terminator = None;
    eof = false;
    token_required = false
  }

  let (@@) f g = f g

  let spf = Printf.sprintf
  let buf = Buffer.create 256

  let init_lexer_pos fname lexbuf =
    let curr = lexbuf.Lexing.lex_curr_p in
    let pos : Lexing.position = {
      Lexing.pos_fname = (match fname with Some fname -> fname | _ -> "");
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = curr.Lexing.pos_lnum;
      Lexing.pos_cnum = 0
    } in
    lexbuf.Lexing.lex_curr_p <- pos

  let update_context mode terminator =
    let token_required =
      match terminator with
        Some ("%}" | "-%}") -> true
      | _ -> false in
    ctx.mode <- mode;
    ctx.terminator <- terminator;
    ctx.token_required <- token_required

  let reset_context () =
    ctx.eof <- false;
    ctx.mode <- `Html;
    ctx.terminator <- None;
    ctx.token_required <- false;
    Buffer.reset buf

  let get_buf () =
    let text = Buffer.contents buf in
    Buffer.reset buf;
    text

  let add_char c1 =
    Buffer.add_char buf c1

  let add_str str =
    Buffer.add_string buf str

  let token_or_str (str, token) lexer lexb =
    match ctx.mode with
      | `Logic ->
	(* print_endline @@ spf "logical token:%s" str; *)
        ctx.token_required <- false ;
	token
      | `Html ->
	add_str str;
	lexer lexb

  let token_or_char (chr, token) lexer lexb =
    match ctx.mode with
      | `Logic ->
	(* print_endline @@ spf "logical token:%c" chr; *)
        ctx.token_required <- false ;
	token
      | `Html ->
	add_char chr;
	lexer lexb
}

let blank = [ ' ' '\t' ]
let ident_first_char = [ 'A'-'Z' 'a'-'z' ]
let ident_char =  [ 'A'-'Z' 'a'-'z' '_' '0'-'9' ]
let int_literal = ['0'-'9'] ['0'-'9']*
let float_literal = ['0'-'9']+('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
let newline = [ '\n' ]

rule main = parse
  | '\\' '{' {
    add_char '{';
    main lexbuf
  }
  | '\\' '}' {
    add_char '}';
    main lexbuf
  }
  | "{%" blank+ "raw" blank+ "%}" { raw lexbuf }
  | "{%" | (blank | newline)* "{%-" {
    update_context `Logic (Some "%}");
    match get_buf () with
      | "" -> main lexbuf
      | content -> TEXT content
  }
  | "{{" {
    update_context `Logic (Some "}}");
    (* print_endline @@ spf "text:%s" (Buffer.contents buf); *)
    match get_buf () with
      | "" -> main lexbuf
      | content -> TEXT content
  }
  | "{#" { comment lexbuf }
  | '\n' as c {
    Lexing.new_line lexbuf;
    if ctx.mode = `Html then
      Buffer.add_char buf c
    ;
    main lexbuf
  }
  | "}}" as str {
    match ctx.terminator with
      | None ->
	add_str str; main lexbuf
      | Some "}}" ->
	update_context `Html None;
	main lexbuf
      | _ -> failwith @@ spf "syntax error '%s'" str
  }
  | ("%}" | "-%}" (blank | newline)*) as str {
    match ctx.terminator with
      | None ->
	add_str str; main lexbuf
      | Some "%}" ->
	update_context `Html None;
	main lexbuf
      | _ -> failwith @@ spf "syntax error '%s'" str
  }
  | '\"' as c {
    match ctx.mode with
      | `Html -> add_char c; main lexbuf
      | _ -> string_literal c lexbuf
  }
  | '\'' as c {
    match ctx.mode with
      | `Html -> add_char c; main lexbuf
      | _ -> string_literal c lexbuf
  }
  | int_literal as str {
    match ctx.mode with
      | `Html -> add_str str; main lexbuf
      | _ -> INT (int_of_string str)
  }
  | float_literal as str {
    match ctx.mode with
      | `Html -> add_str str; main lexbuf
      | _ -> FLOAT (float_of_string str)
  }
  | "if" as s { token_or_str (s, IF) main lexbuf }
  | "else" as s { token_or_str (s, ELSE) main lexbuf }
  | ("elseif" | "elif") as s { token_or_str (s, ELSEIF) main lexbuf }
  | "endif" as s { token_or_str (s, ENDIF) main lexbuf }
  | "for" as s { token_or_str (s, FOR) main lexbuf }
  | "endfor" as s { token_or_str (s, ENDFOR) main lexbuf }
  | "include" as s { token_or_str (s, INCLUDE) main lexbuf }
  | "extends" as s { token_or_str (s, EXTENDS) main lexbuf }
  | "block" as s { token_or_str (s, BLOCK) main lexbuf }
  | "endblock" as s { token_or_str (s, ENDBLOCK) main lexbuf }
  | "filter" as s { token_or_str (s, FILTER) main lexbuf }
  | "endfilter" as s { token_or_str (s, ENDFILTER) main lexbuf }
  | "macro" as s { token_or_str (s, MACRO) main lexbuf }
  | "endmacro" as s { token_or_str (s, ENDMACRO) main lexbuf }
  | "call" as s { token_or_str (s, CALL) main lexbuf }
  | "endcall" as s { token_or_str (s, ENDCALL) main lexbuf }
  | "import" as s { token_or_str (s, IMPORT) main lexbuf }
  | "as" as s { token_or_str (s, AS) main lexbuf }
  | "from" as s { token_or_str (s, FROM) main lexbuf }
  | "in" as s { token_or_str (s, IN) main lexbuf }
  | "set" as s { token_or_str (s, SET) main lexbuf }
  | "not" as s { token_or_str (s, NOT) main lexbuf }
  | "is" as s { token_or_str (s, IS) main lexbuf }
  | "with" as s { token_or_str (s, WITH) main lexbuf }
  | "endwith" as s { token_or_str (s, ENDWITH) main lexbuf }
  | "without" as s { token_or_str (s, WITHOUT) main lexbuf }
  | "context" as s { token_or_str (s, CONTEXT) main lexbuf }
  | "autoescape" as s { token_or_str (s, AUTOESCAPE) main lexbuf }
  | "endautoescape" as s { token_or_str (s, ENDAUTOESCAPE) main lexbuf }
  | "rawinclude" as s { token_or_str (s, RAWINCLUDE) main lexbuf }
  | "true" as s { token_or_str (s, TRUE) main lexbuf }
  | "false" as s { token_or_str (s, FALSE) main lexbuf }
  | "null" as s { token_or_str (s, NULL) main lexbuf }
  | "==" as s { token_or_str (s, EQ_EQ) main lexbuf }
  | "!=" as s { token_or_str (s, NEQ) main lexbuf }
  | "<=" as s { token_or_str (s, LT_EQ) main lexbuf }
  | ">=" as s { token_or_str (s, GT_EQ) main lexbuf }
  | ("&&" | "and") as s { token_or_str (s, AND) main lexbuf }
  | ("||" | "or") as s { token_or_str (s, OR) main lexbuf }
  | "**" as s { token_or_str (s, POWER) main lexbuf }
  | ","  as c { token_or_char (c, COMMA) main lexbuf }
  | "=" as c { token_or_char (c, EQ) main lexbuf }
  | "<" as c { token_or_char (c, LT) main lexbuf }
  | ">" as c { token_or_char (c, GT) main lexbuf }
  | "!" as c { token_or_char (c, NOT) main lexbuf }
  | "." as c { token_or_char (c, DOT) main lexbuf }
  | "+" as c { token_or_char (c, PLUS) main lexbuf }
  | "-" as c { token_or_char (c, MINUS) main lexbuf }
  | "*" as c { token_or_char (c, TIMES) main lexbuf }
  | "/" as c { token_or_char (c, DIV) main lexbuf }
  | "%" as c { token_or_char (c, MOD) main lexbuf }
  | "(" as c { token_or_char (c, LPAREN) main lexbuf }
  | ")" as c { token_or_char (c, RPAREN) main lexbuf }
  | "[" as c { token_or_char (c, LBRACKET) main lexbuf }
  | "]" as c { token_or_char (c, RBRACKET) main lexbuf }
  | "{" as c { token_or_char (c, LBRACE) main lexbuf }
  | "}" as c { token_or_char (c, RBRACE) main lexbuf }
  | ":" as c { token_or_char (c, COLON) main lexbuf }
  | "|" as c { token_or_char (c, VLINE) main lexbuf }
  | ident_first_char ident_char* as str {
    if ctx.token_required then
      failwith @@ spf "syntax error: expected token, got '%s'" str
    ;
    match ctx.mode with
      | `Html ->
	(* print_endline @@ spf "html ident:%s" str; *)
	add_str str; main lexbuf
      | _ ->
	(* print_endline @@ spf "logical ident:%s" str; *)
	IDENT str
  }
  | blank as c {
    match ctx.mode with
      | `Html -> add_char c; main lexbuf
      | _ -> main lexbuf
  }
  | _ as c {
    match ctx.mode with
      | `Html -> add_char c; main lexbuf
      | _ -> failwith @@ spf "unexpected token:%c" c
  }
  | eof {
    match ctx.eof with
      | true -> EOF
      | _ ->
	ctx.eof <- true;
	TEXT (get_buf ())
  }

and comment = parse
  | "#}" { main lexbuf }
  | _ { comment lexbuf }

and raw = parse
  | "{%" blank+ "endraw" blank+ "%}" { TEXT (get_buf()) }
  | _ as c {
    add_char c;
    raw lexbuf
  }

and string_literal terminator = parse
  | '\\' ['\\' '\"' 'n' 't' 'r'] {
    let chr =
      match Lexing.lexeme_char lexbuf 1 with
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'r' -> '\r'
        | c -> c in
    add_char chr;
    string_literal terminator lexbuf
  }
  | _ as c {
    if c = terminator then begin
      (* print_endline @@ spf "string literal:(%s)" (Buffer.contents buf); *)
      STRING (get_buf ())
    end else begin
      add_char c;
      string_literal terminator lexbuf
    end
  }

