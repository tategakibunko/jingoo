{

open Jingoo

let keyword_class = ref "jghl-kw"
let comment_class = ref "jghl-cmt"
let stmt_class = ref "jghl-stmt"
let expr_class = ref "jghl-expr"
let string_class = ref "jghl-str"

let print_string = ref Stdlib.print_string
let print_char = ref Stdlib.print_char

let logic = ref false

let print_class c s =
  !print_string (Printf.sprintf "<span class=\"%s\">%s</span>" c s)


let fail ({ Lexing.lex_curr_p = { Lexing.pos_fname ; pos_lnum ; pos_bol ; pos_cnum } ; _ } as l) =
  failwith @@
  Printf.sprintf "File '%s', line %d, char %d: %s"
    pos_fname pos_lnum (pos_cnum - pos_bol) (Lexing.lexeme l)

}

let ident_first_char = [ 'A'-'Z' 'a'-'z' '_' ]
let ident_char =  [ 'A'-'Z' 'a'-'z' '_' '0'-'9' ]

rule main = parse

  | ( "and"
    | "as"
    | "autoescape"
    | "block"
    | "call"
    | "case"
    | "context"
    | "default"
    | "elif"
    | "else"
    | "elseif"
    | "endautoescape"
    | "endblock"
    | "endcall"
    | "endfilter"
    | "endfor"
    | "endfunction"
    | "endif"
    | "endmacro"
    | "endraw"
    | "endswitch"
    | "endwith"
    | "extends"
    | "filter"
    | "for"
    | "from"
    | "function"
    | "if"
    | "import"
    | "in"
    | "include"
    | "is"
    | "macro"
    | "not"
    | "null"
    | "or"
    | "raw"
    | "rawinclude"
    | "set"
    | "switch"
    | "with"
    | "without"
    | "||"
    | "|"
    | "&#38;&#38;"
    | "&&"
    ) as s {
      if !logic
      then print_class !keyword_class s
      else !print_string s ;
      main lexbuf
    }

  | ident_first_char ident_char* as s {
      !print_string s ;
      main lexbuf
    }

  | "{{" as s {
      if !logic then fail lexbuf ;
      logic := true ;
      print_class !expr_class s ;
      main lexbuf
    }
  | "}}" as s {
      if not !logic then fail lexbuf ;
      logic := false ;
      print_class !expr_class s ;
      main lexbuf
    }
  | ("{%"|"{%-") as s {
      if !logic then fail lexbuf ;
      logic := true ;
      print_class !stmt_class s ;
      main lexbuf
    }
  | ("%}"|"-%}") as s {
      if not !logic then fail lexbuf ;
      logic := false ;
      print_class !stmt_class s ;
      main lexbuf
    }
  | "{#" { comment (Buffer.create 42) lexbuf }
  | ("\"" | "'" | "&#39;" | "&#34;") as s {
      if !logic then string (Buffer.create 42) s lexbuf
      else begin
        !print_string s ;
        main lexbuf
      end
    }
  | eof { () }
  | _ as c {
    if c = '\n' then Lexing.new_line lexbuf ;
    !print_char c ;
    main lexbuf
  }

and comment buffer = parse
  | "#}" {
      print_class !stmt_class ("{#" ^ Buffer.contents buffer ^ "#}") ;
      main lexbuf
    }
  | _ as c {
    if c = '\n' then Lexing.new_line lexbuf ;
    Buffer.add_char buffer c ;
    comment buffer lexbuf
  }

and string buffer term = parse

  | ('\\' _) as s {
    Buffer.add_string buffer s ;
    string buffer term lexbuf
  }
  | ("&#34;"|"&#39;"|_) as s {
      if s = term
      then begin
        print_class !string_class (Printf.sprintf "%s%s%s" s (Buffer.contents buffer) s) ;
        main lexbuf
      end
      else begin
        Buffer.add_string buffer s ;
        string buffer term lexbuf
      end
    }

{

open Jg_types

let highlight = function
  | Tstr s ->
    begin try
        let buffer = Buffer.create (String.length s) in
        let lexbuf = Lexing.from_string s in
        logic := false ;
        print_string := Buffer.add_string buffer ;
        print_char := Buffer.add_char buffer ;
        main lexbuf ;
        Tstr (Buffer.contents buffer)
      with _ -> failwith s
    end
  | x -> Jg_types.failwith_type_error_1 "highlight" x

let () =
  Jg_stub.add_func ~namespace:"jg_highlight" ~func_name:"highlight" (func_arg1_no_kw highlight)

}
