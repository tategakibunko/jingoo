(*
  jg_utils.ml

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)
let spf = Printf.sprintf

let (@@) f g = f g

let ($) f g x = f (g x)

let (+>) f g = g f

let (>>=) x f = f x

let strlen = BatUTF8.length
let strcmp = BatUTF8.compare

(** application friendly substring *)
let rec substring base count str =
  let len = BatUTF8.length str in
  if base >= len || count = 0 then
    ""
  else if base = 0 && count >= len then
    str
  else if base < 0 then
    substring (len + base) count str
  else if base + count >= len then
    let lp = BatUTF8.nth str base in
    let rp = BatUTF8.next str (BatUTF8.last str) in
    String.sub str lp (rp - lp)
  else
    let lp = BatUTF8.nth str base in
    let rp = BatUTF8.nth str (base + count) in
    String.sub str lp (rp - lp)

let escape_html str =
  let str = Pcre.qreplace ~rex:(Pcre.regexp "&") ~templ:"&amp;" str in
  let str = Pcre.qreplace ~rex:(Pcre.regexp "\"") ~templ:"&quot;" str in
  let str = Pcre.qreplace ~rex:(Pcre.regexp "<") ~templ:"&lt;" str in
  let str = Pcre.qreplace ~rex:(Pcre.regexp ">") ~templ:"&gt;" str in
    str

let chomp str =
  Pcre.qreplace ~rex:(Pcre.regexp "\\n+$") ~templ:"" str

let is_lower str =
  try
    ignore @@ Pcre.exec ~rex:(Pcre.regexp "[A-Z]+") str;
    false
  with
      Not_found -> true

let is_upper str =
  try
    ignore @@ Pcre.exec ~rex:(Pcre.regexp "[a-z]+") str;
    false
  with
      Not_found -> true

let rec take ?pad n lst =
  match n, lst, pad with
    | n, _, _ when n <= 0 -> []
    | n, [], None -> []
    | n, [], Some value -> value :: (take (n-1) [] ?pad)
    | n, h :: rest, _ -> h :: (take (n-1) rest ?pad)

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

let get_parser_error exn lexbuf =
  let curr = lexbuf.Lexing.lex_curr_p in
  let fname = curr.Lexing.pos_fname in
  let line = curr.Lexing.pos_lnum in
  let tok = Lexing.lexeme lexbuf in
  let msg = match exn with Jg_types.SyntaxError msg -> msg | _ -> Printexc.to_string exn in
  Printf.sprintf "%s: '%s' at line %d in file %s" msg tok line fname

let read_file_as_string filename =
  let file = open_in_bin filename in
  let size = in_channel_length file in
  try
    let buf = String.create size in (* for compatibility of OCaml <= 3 *)
    (* let buf = Bytes.create size in *)
    really_input file buf 0 size;
    close_in file;
    buf
  with e ->
    (try close_in file with _ -> ());
    raise e

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

