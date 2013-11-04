{
  (*
    jg_lexer.mll

    Copyright (c) 2011- by Masaki WATANABE

    Licence: see LICENCE
  *)
  open Jg_types
  open Jg_parser

  type lexer_mode =
      Plain_mode
    | Expand_mode
    | Logic_mode

  let (@@) f g = f g

  let spf = Printf.sprintf

  let debug = false

  let cur_mode = ref Plain_mode

  let text_buf = Buffer.create 256

  let string_buf = Buffer.create 256

  let cached_tokens = ref []

  let cache_token token =
    cached_tokens := token :: !cached_tokens

  let gen_cached_lexer normal_lexer =
    fun lexbuf ->
      match !cached_tokens with
	| head :: rest ->
	  cached_tokens := rest;
	  head
	| _ -> normal_lexer lexbuf

  let init_lexer_pos fname lexbuf =
    let curr = lexbuf.Lexing.lex_curr_p in
    let pos : Lexing.position = {
      Lexing.pos_fname = (match fname with Some fname -> fname | _ -> "");
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = curr.Lexing.pos_lnum;
      Lexing.pos_cnum = 0
    } in
    lexbuf.Lexing.lex_curr_p <- pos

  let get_string_token () = 
    let string = Buffer.contents string_buf in
    Buffer.reset string_buf;
    STRING(string)

  let get_text () =
    let text = Buffer.contents text_buf in
    Buffer.reset text_buf;
    text

  (** if text_buf holds something, output it first, and 'token' is cached and yielded at next parsing. *)
  let get_text_token_with_cache token =
    let text = get_text () in
    if text = "" then
      token
    else
      begin
	cache_token token;
	TEXT(text)
      end

  let get_text_token_or_parse on_empty =
    let text = get_text () in
    if text <> "" then
      TEXT(text)
    else
      on_empty ()

  let update_line_no lexbuf =
    let curr = lexbuf.Lexing.lex_curr_p in
    let cur_lnum = curr.Lexing.pos_lnum in
    lexbuf.Lexing.lex_curr_p <- {curr with Lexing.pos_lnum = cur_lnum + 1}

  let reset_lexer () =
    cur_mode := Plain_mode;
    cached_tokens := [];
    Buffer.reset text_buf;
    Buffer.reset string_buf

  let keywords = [
    ("true", TRUE);
    ("false", FALSE);
    ("null", NULL);
    ("if", IF);
    ("else", ELSE);
    ("elseif", ELSEIF);
    ("endif", ENDIF);
    ("for", FOR);
    ("endfor", ENDFOR);
    ("include", INCLUDE);
    ("extends", EXTENDS);
    ("block", BLOCK);
    ("endblock", ENDBLOCK);
    ("filter", FILTER);
    ("endfilter", ENDFILTER);
    ("macro", MACRO);
    ("endmacro", ENDMACRO);
    ("call", CALL);
    ("endcall", ENDCALL);
    ("import", IMPORT);
    ("as", AS);
    ("from", FROM);
    ("in", IN);
    ("set", SET);
    ("not", NOT);
    ("is", IS);
    ("with", WITH);
    ("endwith", ENDWITH);
    ("without", WITHOUT);
    ("context", CONTEXT);
    ("autoescape", AUTOESCAPE);
    ("endautoescape", ENDAUTOESCAPE);
    ("rawinclude", RAWINCLUDE);
  ]

  let keyword_or_ident word =
    try
      List.assoc word keywords
    with
	Not_found -> IDENT word

  let token_or_str token str on_process =
    match !cur_mode with
      | Plain_mode ->
	Buffer.add_string text_buf str;
	on_process ()
      | _ ->
	token
}

let blank = [ ' ' '\t' ]
let ident_first_char = [ 'A'-'Z' 'a'-'z' ]
let ident_char =  [ 'A'-'Z' 'a'-'z' '_' '0'-'9' ]
let int_lit = ['0'-'9'] ['0'-'9']*
let float_lit = ['0'-'9']+('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

  rule main = parse
    | '\\' '{' {
      Buffer.add_char text_buf '{';
      main lexbuf
    }

    | '\\' '}' {
      Buffer.add_char text_buf '}';
      main lexbuf
    }
(*
    | '\\' '#' {
      Buffer.add_char text_buf '#';
      main lexbuf
    }
*)
    | '\"' {
      match !cur_mode with
	| Plain_mode ->
	  Buffer.add_char text_buf '\"';
	  main lexbuf
	| _ ->
	  string_token '\"' lexbuf;
	  get_string_token ()
    }

    | '\'' {
      match !cur_mode with
	| Plain_mode ->
	  Buffer.add_char text_buf '\'';
	  main lexbuf
	| _ ->
	  string_token '\'' lexbuf;
	  get_string_token ()
    }
(*
    | '#' {
      match !cur_mode with
	| Plain_mode ->
	  line_comment lexbuf;
	  main lexbuf
	| _ ->
	  raise @@ SyntaxError "lexer error"
    }
*)
    | "{#" {
      match !cur_mode with
	| Plain_mode ->
	  comment lexbuf;
	  main lexbuf
	| _ ->
	  raise @@ SyntaxError "lexer error"
    }

    | "{{" {
      match !cur_mode with
	| Plain_mode ->
	  cur_mode := Expand_mode;
	  get_text_token_with_cache EXPAND
	| _ ->
	  raise @@ SyntaxError "lexer error"
    }

    | "}}" {
      match !cur_mode with
	| Expand_mode ->
	  cur_mode := Plain_mode;
	  ENDEXPAND
	| _ ->
	  raise @@ SyntaxError "lexer error"
    }

    | "{%" blank+ "raw" blank+ "%}" {
      match !cur_mode with
	| Plain_mode ->
	  raw lexbuf
	| _ ->
	  raise @@ SyntaxError "lexer error"
    }

    | "{%-" {
      match !cur_mode with
	| Plain_mode->
	  cur_mode := Logic_mode;
	  get_text_token_with_cache @@ TEXT "{%<%}" (** special mark to remove left white-space *)
	| _ ->
	  raise @@ SyntaxError "lexer error"
    }

    | "{%" {
      match !cur_mode with
	| Plain_mode->
	  cur_mode := Logic_mode;
	  get_text_token_or_parse (fun () -> main lexbuf)
	| _ ->
	  raise @@ SyntaxError "lexer error"
    }
	
    | "-%}" {
      match !cur_mode with
	| Logic_mode ->
	  cur_mode := Plain_mode;
	  get_text_token_with_cache @@ TEXT "{%>%}" (** special mark to remove right white-space *)
	| _ ->
	  raise @@ SyntaxError "lexer error"
    }

    | "%}" {
      match !cur_mode with
	| Logic_mode ->
	  cur_mode := Plain_mode;
	  main lexbuf
	| _ ->
	  raise @@ SyntaxError "lexer error"
    }

    | "\n" {
      update_line_no lexbuf;
      Buffer.add_char text_buf '\n';
      main lexbuf
    }

    | int_lit {
      let str = Lexing.lexeme lexbuf in
      match !cur_mode with
	| Plain_mode ->
	  Buffer.add_string text_buf str;
	  main lexbuf
	| _ ->
	  INT (int_of_string str)
    }
	
    | float_lit {
      let str = Lexing.lexeme lexbuf in
      match !cur_mode with
	| Plain_mode ->
	  Buffer.add_string text_buf str;
	  main lexbuf
	| _ ->
	  FLOAT (float_of_string str)
    }

    | ident_first_char ident_char* as word {
      match !cur_mode with
	| Plain_mode ->
	  Buffer.add_string text_buf word;
	  main lexbuf
	| _ ->
	  keyword_or_ident word
    }
	
    | ","  { token_or_str COMMA     ","  (fun () -> main lexbuf) }
    | "==" { token_or_str EQ_EQ     "==" (fun () -> main lexbuf) }
    | "!=" { token_or_str NEQ       "!=" (fun () -> main lexbuf) }
    | "<=" { token_or_str LT_EQ     "<=" (fun () -> main lexbuf) }
    | ">=" { token_or_str GT_EQ     ">=" (fun () -> main lexbuf) }
    | "&&" { token_or_str AND       "&&" (fun () -> main lexbuf) }
    | "||" { token_or_str OR        "||" (fun () -> main lexbuf) }
    | "="  { token_or_str EQ        "="  (fun () -> main lexbuf) }
    | "<"  { token_or_str LT        "<"  (fun () -> main lexbuf) }
    | ">"  { token_or_str GT        ">"  (fun () -> main lexbuf) }
    | "!"  { token_or_str NOT       "!"  (fun () -> main lexbuf) }
    | "."  { token_or_str DOT       "."  (fun () -> main lexbuf) }
    | "+"  { token_or_str PLUS      "+"  (fun () -> main lexbuf) }
    | "-"  { token_or_str MINUS     "-"  (fun () -> main lexbuf) }
    | "**" { token_or_str POWER     "**" (fun () -> main lexbuf) }
    | "*"  { token_or_str TIMES     "*"  (fun () -> main lexbuf) }
    | "/"  { token_or_str DIV       "/"  (fun () -> main lexbuf) }
    | "%"  { token_or_str MOD       "%"  (fun () -> main lexbuf) }
    | "("  { token_or_str LPAREN    "("  (fun () -> main lexbuf) }
    | ")"  { token_or_str RPAREN    ")"  (fun () -> main lexbuf) }
    | "["  { token_or_str LBRACKET  "["  (fun () -> main lexbuf) }
    | "]"  { token_or_str RBRACKET  "]"  (fun () -> main lexbuf) }
    | "{"  { token_or_str LBRACE    "{"  (fun () -> main lexbuf) }
    | "}"  { token_or_str RBRACE    "}"  (fun () -> main lexbuf) }
    | ":"  { token_or_str COLON     ":"  (fun () -> main lexbuf) }
    | "|"  { token_or_str VLINE     "|"  (fun () -> main lexbuf) }

    | eof {
      match !cur_mode with
	| Plain_mode ->
	  get_text_token_or_parse (fun () -> EOF)
	| _ ->
	  raise @@ SyntaxError "lexer error"
    }
	
    | _ {
      match !cur_mode with
	| Plain_mode ->
	  Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
	  main lexbuf
	| _ ->
	  main lexbuf
    }
	
  and string_token trailer = parse
    | '\\' ['\\' '\"' 'n' 't' 'r'] {
      let char_to_add =
        match Lexing.lexeme_char lexbuf 1 with
            'n' -> '\n'
          | 't' -> '\t'
          | 'r' -> '\r'
          | c -> c in
      Buffer.add_char string_buf char_to_add;
      string_token trailer lexbuf
    }

    | "\n" {
      update_line_no lexbuf;
      string_token trailer lexbuf
    }

    | eof {
      raise @@ SyntaxError "string_token lexer error"
    }

    | _ {
      match Lexing.lexeme_char lexbuf 0 with
	| c when c = trailer -> ()
	| c ->
	  Buffer.add_char string_buf (Lexing.lexeme_char lexbuf 0);
	  string_token trailer lexbuf
    }

  and comment = parse
    | "#}" { () }

    | "\n" {
      update_line_no lexbuf;
      comment lexbuf
    }

    | eof {
      raise @@ SyntaxError "comment lexer error"
    }

    | _ {
      comment lexbuf
    }
(*
  and line_comment = parse
    | "\n" {
      update_line_no lexbuf;
      ()
    }

    | _ {
      line_comment lexbuf
    }
*)
  and raw = parse
    | "{%" blank+ "endraw" blank+ "%}" {
      let text = get_text () in
      TEXT(text)
    }

    | "\n" {
      update_line_no lexbuf;
      Buffer.add_char text_buf '\n';
      raw lexbuf
    }

    | eof {
      raise @@ SyntaxError "raw lexer error"
    }

    | _ {
      Buffer.add_char text_buf (Lexing.lexeme_char lexbuf 0);
      raw lexbuf
    }
