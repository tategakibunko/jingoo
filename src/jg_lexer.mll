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
  }

  let ctx = {
    mode = `Html;
    terminator = None;
    eof = false
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
    ctx.mode <- mode;
    ctx.terminator <- terminator

  let reset_context () =
    ctx.eof <- false;
    ctx.mode <- `Html;
    ctx.terminator <- None;
    Buffer.reset buf

  let get_buf () =
    let text = Buffer.contents buf in
    Buffer.reset buf;
    text

  let add_char c1 =
    Buffer.add_char buf c1

  let add_str str =
    Buffer.add_string buf str

  let token_or_str (str, token) next =
    match ctx.mode with
      | `Logic ->
	(* print_endline @@ spf "logical token:%s" str; *)
	token
      | `Html ->
	add_str str; next ()

  let token_or_char (chr, token) next =
    match ctx.mode with
      | `Logic ->
	(* print_endline @@ spf "logical token:%c" chr; *)
	token
      | `Html ->
	add_char chr; next ()
}

let blank = [ ' ' '\t' ]
let ident_first_char = [ 'A'-'Z' 'a'-'z' ]
let ident_char =  [ 'A'-'Z' 'a'-'z' '_' '0'-'9' ]
let int_literal = ['0'-'9'] ['0'-'9']*
let float_literal = ['0'-'9']+('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

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
  | "{%" {
    update_context `Logic (Some "%}");
    (* print_endline @@ spf "text:%s" (Buffer.contents buf); *)
    match get_buf () with
      | "" -> main lexbuf
      | content -> TEXT content
  }
  | "{%-" {
    update_context `Logic (Some "%}");
    (* print_endline @@ spf "text:%s" (Buffer.contents buf); *)
    add_str "{%<%}";
    TEXT (get_buf ())
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
  | "%}" as str {
    match ctx.terminator with
      | None ->
	add_str str; main lexbuf
      | Some "%}" ->
	update_context `Html None;
	main lexbuf
      | _ -> failwith @@ spf "syntax error '%s'" str
  }
  | "-%}" as str {
    match ctx.terminator with
      | None ->
	add_str str; main lexbuf
      | Some "%}" ->
	update_context `Html None;
	add_str "{%>%}";
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
  | "if" as s { token_or_str (s, IF) (fun () -> main lexbuf) }
  | "else" as s { token_or_str (s, ELSE) (fun () -> main lexbuf) }
  | "elseif" as s { token_or_str (s, ELSEIF) (fun () -> main lexbuf) }
  | "endif" as s { token_or_str (s, ENDIF) (fun () -> main lexbuf) }
  | "for" as s { token_or_str (s, FOR) (fun () -> main lexbuf) }
  | "endfor" as s { token_or_str (s, ENDFOR) (fun () -> main lexbuf) }
  | "include" as s { token_or_str (s, INCLUDE) (fun () -> main lexbuf) }
  | "extends" as s { token_or_str (s, EXTENDS) (fun () -> main lexbuf) }
  | "block" as s { token_or_str (s, BLOCK) (fun () -> main lexbuf) }
  | "endblock" as s { token_or_str (s, ENDBLOCK) (fun () -> main lexbuf) }
  | "filter" as s { token_or_str (s, FILTER) (fun () -> main lexbuf) }
  | "endfilter" as s { token_or_str (s, ENDFILTER) (fun () -> main lexbuf) }
  | "macro" as s { token_or_str (s, MACRO) (fun () -> main lexbuf) }
  | "endmacro" as s { token_or_str (s, ENDMACRO) (fun () -> main lexbuf) }
  | "call" as s { token_or_str (s, CALL) (fun () -> main lexbuf) }
  | "endcall" as s { token_or_str (s, ENDCALL) (fun () -> main lexbuf) }
  | "import" as s { token_or_str (s, IMPORT) (fun () -> main lexbuf) }
  | "as" as s { token_or_str (s, AS) (fun () -> main lexbuf) }
  | "from" as s { token_or_str (s, FROM) (fun () -> main lexbuf) }
  | "in" as s { token_or_str (s, IN) (fun () -> main lexbuf) }
  | "set" as s { token_or_str (s, SET) (fun () -> main lexbuf) }
  | "not" as s { token_or_str (s, NOT) (fun () -> main lexbuf) }
  | "is" as s { token_or_str (s, IS) (fun () -> main lexbuf) }
  | "with" as s { token_or_str (s, WITH) (fun () -> main lexbuf) }
  | "endwith" as s { token_or_str (s, ENDWITH) (fun () -> main lexbuf) }
  | "without" as s { token_or_str (s, WITHOUT) (fun () -> main lexbuf) }
  | "context" as s { token_or_str (s, CONTEXT) (fun () -> main lexbuf) }
  | "autoescape" as s { token_or_str (s, AUTOESCAPE) (fun () -> main lexbuf) }
  | "endautoescape" as s { token_or_str (s, ENDAUTOESCAPE) (fun () -> main lexbuf) }
  | "rawinclude" as s { token_or_str (s, RAWINCLUDE) (fun () -> main lexbuf) }
  | "true" as s { token_or_str (s, TRUE) (fun () -> main lexbuf) }
  | "false" as s { token_or_str (s, FALSE) (fun () -> main lexbuf) }
  | "null" as s { token_or_str (s, NULL) (fun () -> main lexbuf) }
  | "==" as s { token_or_str (s, EQ_EQ) (fun () -> main lexbuf) }
  | "!=" as s { token_or_str (s, NEQ) (fun () -> main lexbuf) }
  | "<=" as s { token_or_str (s, LT_EQ) (fun () -> main lexbuf) }
  | ">=" as s { token_or_str (s, GT_EQ) (fun () -> main lexbuf) }
  | "&&" as s { token_or_str (s, AND) (fun () -> main lexbuf) }
  | "||" as s { token_or_str (s, OR) (fun () -> main lexbuf) }
  | "**" as s { token_or_str (s, POWER) (fun () -> main lexbuf) }
  | ","  as c { token_or_char (c, COMMA) (fun () -> main lexbuf) }
  | "=" as c { token_or_char (c, EQ) (fun () -> main lexbuf) }
  | "<" as c { token_or_char (c, LT) (fun () -> main lexbuf) }
  | ">" as c { token_or_char (c, GT) (fun () -> main lexbuf) }
  | "!" as c { token_or_char (c, NOT) (fun () -> main lexbuf) }
  | "." as c { token_or_char (c, DOT) (fun () -> main lexbuf) }
  | "+" as c { token_or_char (c, PLUS) (fun () -> main lexbuf) }
  | "-" as c { token_or_char (c, MINUS) (fun () -> main lexbuf) }
  | "*" as c { token_or_char (c, TIMES) (fun () -> main lexbuf) }
  | "/" as c { token_or_char (c, DIV) (fun () -> main lexbuf) }
  | "%" as c { token_or_char (c, MOD) (fun () -> main lexbuf) }
  | "(" as c { token_or_char (c, LPAREN) (fun () -> main lexbuf) }
  | ")" as c { token_or_char (c, RPAREN) (fun () -> main lexbuf) }
  | "[" as c { token_or_char (c, LBRACKET) (fun () -> main lexbuf) }
  | "]" as c { token_or_char (c, RBRACKET) (fun () -> main lexbuf) }
  | "{" as c { token_or_char (c, LBRACE) (fun () -> main lexbuf) }
  | "}" as c { token_or_char (c, RBRACE) (fun () -> main lexbuf) }
  | ":" as c { token_or_char (c, COLON) (fun () -> main lexbuf) }
  | "|" as c { token_or_char (c, VLINE) (fun () -> main lexbuf) }
  | ident_first_char ident_char* as str {
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

