(*
  jg_utils.ml

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)
open CamomileLibrary

let spf = Printf.sprintf

let (@@) f g = f g

let ($) f g x = f (g x)

let (+>) f g = g f

let (>>=) x f = f x

module Sub = SubText.Make(UTF8)

let strlen = UTF8.length
let strcmp = UTF8.compare

let rec substring base count str =
  let len = UTF8.length str in
  if base >= len || count = 0 then
    ""
  else if base <= 0 && count >= len then
    str
  else if base < 0 then
    substring (len + base) count str
  else if base + count >= len then
    let lp = UTF8.nth str base in
    let rp = UTF8.next str (UTF8.last str) in
    Sub.excerpt (Sub.refer str lp rp)
  else
    let lp = UTF8.nth str base in
    let rp = UTF8.nth str (base + count) in
    Sub.excerpt (Sub.refer str lp rp)
;;

let escape_html str =
  let str = Pcre.qreplace ~rex:(Pcre.regexp "&") ~templ:"&amp;" str in
  let str = Pcre.qreplace ~rex:(Pcre.regexp "\"") ~templ:"&quot;" str in
  let str = Pcre.qreplace ~rex:(Pcre.regexp "<") ~templ:"&lt;" str in
  let str = Pcre.qreplace ~rex:(Pcre.regexp ">") ~templ:"&gt;" str in
    str
;;

let chomp str =
  Pcre.qreplace ~rex:(Pcre.regexp "\\n+$") ~templ:"" str
;;

let is_lower str =
  try
    ignore @@ Pcre.exec ~rex:(Pcre.regexp "[A-Z]+") str;
    false
  with
      Not_found -> true
;;

let is_upper str =
  try
    ignore @@ Pcre.exec ~rex:(Pcre.regexp "[a-z]+") str;
    false
  with
      Not_found -> true
;;

let rec take ?pad n lst =
  match n, lst, pad with
    | n, _, _ when n <= 0 -> []
    | n, [], None -> []
    | n, [], Some value -> value :: (take (n-1) [] ?pad)
    | n, h :: rest, _ -> h :: (take (n-1) rest ?pad)
;;

let after n lst =
  if n >= List.length lst then
    []
  else
    let rec iter count rest =
      if count >= n then
	rest
      else
	(match rest with
	  | h :: tl -> iter (count + 1) tl
	  | [] -> []) in
    iter 0 lst
;;

let get_parser_error exn lexbuf =
  let curr = lexbuf.Lexing.lex_curr_p in
  let fname = curr.Lexing.pos_fname in
  let line = curr.Lexing.pos_lnum in
  let tok = Lexing.lexeme lexbuf in
  let msg = match exn with Jg_types.SyntaxError msg -> msg | _ -> Printexc.to_string exn in
  Printf.sprintf "%s: '%s' at line %d in file %s" msg tok line fname
;;

let read_file_as_string filename =
  let file = open_in_bin filename in
  let size = in_channel_length file in
    try
      let buf = String.create size in
        really_input file buf 0 size;
        close_in file;
        buf
    with e ->
      (try close_in file with _ -> ());
      raise e
;;

let rec get_file_path ?(template_dirs=[]) file_name =
  if file_name = "" then
    raise Not_found
  ;
  if not @@ Filename.is_implicit file_name then
    file_name
  else
    match template_dirs with
      | [] ->
	let file_path = Filename.concat (Sys.getcwd ()) file_name in
	if Sys.file_exists file_path then
	  file_path
	else
	  failwith @@ spf "file %s not found" file_path
      | dir :: rest ->
	let file_path = Filename.concat dir file_name in
	if Sys.file_exists file_path then
	  file_path
	else
	  get_file_path file_name ~template_dirs:rest
;;

type mutex_pair = {
  mutable lock: unit -> unit;
  mutable unlock: unit -> unit 
}

let lock_unlock = {
  lock = (fun () -> ());
  unlock = (fun () -> ())
}

module Maybe = struct
  let return x = Some x
  let bind x f =
    match x with
	Some x -> f x
      | None -> None
end

