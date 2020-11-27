(*
  jg_utils.ml

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)
let spf = Printf.sprintf

let ($) f g x = f (g x)

module UTF8 = struct
  let string_to_list s =
    Uutf.String.fold_utf_8 (fun acc _ c -> c :: acc) [] s
    |> List.rev

  let length s = Uutf.String.fold_utf_8 (fun acc _ _ -> acc + 1) 0 s

  let compare = String.compare

  let sub s off len =
    let buf = Buffer.create 0 in
    let encoder = Uutf.encoder `UTF_8 (`Buffer buf) in
    let uchar_array = string_to_list s |> Array.of_list in
    let sub_array = Array.sub uchar_array off len in
    Array.iter (function
        | `Malformed s -> Buffer.add_string buf s
        | `Uchar _ as u -> ignore @@ Uutf.encode encoder u
      ) sub_array;
    ignore @@ Uutf.encode encoder `End;
    Buffer.contents buf

  let is_space = Uucp.White.is_white_space

  (* cmap_utf_8 code code comes from
     http://erratique.ch/software/uucp/doc/Uucp.Case.html *)
  let cmap_utf_8 cmap s =
    let b = Buffer.create (String.length s * 2) in
    let add_map _ _ u =
      let u = match u with `Malformed _ -> Uutf.u_rep | `Uchar u -> u in
      match cmap u with
      | `Self -> Uutf.Buffer.add_utf_8 b u
      | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us
    in
    Uutf.String.fold_utf_8 add_map () s;
    Buffer.contents b

  let lowercase s = cmap_utf_8 Uucp.Case.Map.to_lower s

  let uppercase s = cmap_utf_8 Uucp.Case.Map.to_upper s

  let capitalize s =
    let first = ref true in
    let cmap u =
      if is_space u then `Self
      else if !first then (first := false ; Uucp.Case.Map.to_upper u)
      else Uucp.Case.Map.to_lower u
    in
    cmap_utf_8 cmap s

  let titlecase s =
    let up = ref true in
    let cmap u =
      if is_space u then (up := true ; `Self)
      else if !up then (up := false ; Uucp.Case.Map.to_upper u)
      else Uucp.Case.Map.to_lower u
    in
    cmap_utf_8 cmap s

  let trim s =
    let b = Buffer.create (String.length s) in
    let start = ref true in
    let ws = ref [] in
    Uutf.String.fold_utf_8
      (fun _ _ -> function
        | `Malformed s ->
          Buffer.add_string b s
        | `Uchar u when is_space u && !start ->
          ()
        | `Uchar u when !start ->
          start := false ;
          Uutf.Buffer.add_utf_8 b u
        | `Uchar u when is_space u ->
          ws := u :: !ws
        | `Uchar u ->
          List.iter (Uutf.Buffer.add_utf_8 b) (List.rev !ws) ;
          ws := [] ;
          Uutf.Buffer.add_utf_8 b u)
      () s ;
    Buffer.contents b

  let is_case_aux fn s =
    try
      Uutf.String.fold_utf_8
        (fun _ _ -> function
           | `Uchar u when not (fn u) -> raise Not_found
           | _ -> () )
        () s ;
      true
    with
      Not_found -> false

  let is_lower =
    is_case_aux Uucp.Case.is_lower

  let is_upper =
    is_case_aux Uucp.Case.is_upper

  let split ?(is_delim = is_space) str =
    let start = ref (-1) in
    let acc =
      Uutf.String.fold_utf_8
        (fun acc i -> function
           | `Uchar u when is_delim u && !start = -1 ->
             acc
           | `Uchar u when is_delim u ->
             let acc = (!start, i - !start) :: acc in
             start := -1 ;
             acc
           | _ ->
             if !start = -1 then start := i ;
             acc )
        [] str
    in
    let acc = if !start = -1 then acc else (!start, String.length str - !start) :: acc in
    List.rev_map (fun (a, b) -> String.sub str a b) acc

end

let strlen = UTF8.length
let strcmp = UTF8.compare

(** application friendly substring *)
let rec substring base count str =
  let len = UTF8.length str in
  if base >= len || count = 0 then
    ""
  else if base = 0 && count >= len then
    str
  else if base < 0 then
    substring (len + base) count str
  else if base + count >= len then
    UTF8.sub str base (len - base)
  else
    UTF8.sub str base count

(** [escape_html_char char] returns escaped string option *)
let escape_html_char = function
  | '&' -> Some "&amp;"
  | '"' -> Some "&quot;"
  | '\'' -> Some "&apos;"
  | '>' -> Some "&gt;"
  | '<' -> Some "&lt;"
  | _ -> None

(** [escape_html str] replaces '&', '"', '\'', '<' and '>'
    with their corresponding character reference *)
let escape_html str =
  let strlen = String.length str in
  let es_strlen =
    let rec loop acc i =
      if i >= strlen then acc else
        match escape_html_char (String.unsafe_get str i) with
        | Some es_str -> loop (acc + String.length es_str) (i + 1)
        | None -> loop (acc + 1) (i + 1) in
    loop 0 0 in
  if strlen = es_strlen then
    str
  else
    let buf = Bytes.create es_strlen in
    let rec loop istr ibuf =
      if ibuf >= es_strlen then Bytes.unsafe_to_string buf else
        let chr = String.unsafe_get str istr in
        match escape_html_char chr with
        | Some es_str ->
          let es_len = String.length es_str in
          Bytes.blit_string es_str 0 buf ibuf es_len;
          loop (istr + 1) (ibuf + es_len)
        | None ->
          Bytes.set buf ibuf chr;
          loop (istr + 1) (ibuf + 1) in
    loop 0 0

let chomp str =
  Re.replace_string (Re.compile @@ Re.seq [ Re.rep1 (Re.compl [ Re.notnl ]) ; Re.eos ] ) ~by:"" str

let rec take ?pad n lst =
  match n, lst, pad with
    | n, _, _ when n <= 0 -> []
    | _, [], None -> []
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
          | _ :: tl -> iter (count + 1) tl
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
    let buf = really_input_string file size in
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

module Maybe = struct
  let return x = Some x
  let bind x f =
    match x with
        Some x -> f x
      | None -> None
end

let array_find p a =
  let len = Array.length a in
  let rec loop i =
    if i = len then raise Not_found
    else
      let x = Array.unsafe_get a i in
      if p x then x else loop (i + 1)
  in
  loop 0
