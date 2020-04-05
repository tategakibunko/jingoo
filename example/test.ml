open Jingoo
open Jg_types

(* Copy of Filename.remove_extension which is available in 4.04 *)
let is_dir_sep s i = match Sys.os_type with
  | "Win32" | "Cygwin" ->
    let c = s.[i] in c = '/' || c = '\\' || c = ':'
  | _ -> (* normally "Unix" *)
    s.[i] = '/'
let extension_len name =
  let rec check i0 i =
    if i < 0 || is_dir_sep name i then 0
    else if name.[i] = '.' then check i0 (i - 1)
    else String.length name - i0
  in
  let rec search_dot i =
    if i < 0 || is_dir_sep name i then 0
    else if name.[i] = '.' then check i (i - 1)
    else search_dot (i - 1)
  in
  search_dot (String.length name - 1)
let filename_extension name =
  let l = extension_len name in
  if l = 0 then "" else String.sub name (String.length name - l) l
let filename_remove_extension name =
  let l = extension_len name in
  if l = 0 then name else String.sub name 0 (String.length name - l)

let file_contents file =
  let ic = open_in file in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n ;
  close_in ic ;
  Bytes.unsafe_to_string s

let ls dir filter =
  List.filter filter @@
    let rec loop result = function
      | f :: fs when Sys.is_directory f ->
         Sys.readdir f
         |> Array.to_list
         |> List.rev_map (Filename.concat f)
         |> List.rev_append fs
         |> loop result
      | f::fs -> loop (f :: result) fs
      | []    -> result
    in
    loop [] [dir]

let status = ref 0

let test jingoo_f =
  let html_f = filename_remove_extension jingoo_f ^ ".expected" in
  let models_f = filename_remove_extension jingoo_f ^ ".models" in
  let models =
    if Sys.file_exists models_f then
      let value = ref Tnull in
      let () =
        Jg_interp.from_string
          ~env:{ Jg_types.std_env with autoescape = false }
          ~output:((:=) value)
          (String.trim @@ file_contents models_f) in
      Jg_types.unbox_obj !value
    else []
  in
  let expected = file_contents html_f in
  try
    match Jg_template.from_file ~models jingoo_f with
    | res when res = expected -> prerr_endline @@ "--- OK: " ^ jingoo_f
    | res ->
      prerr_endline @@ "--- Error: " ^ jingoo_f ;
      prerr_endline @@ "--- Expected: " ;
      prerr_endline @@ expected ;
      prerr_endline @@ "--- Got: " ;
      prerr_endline @@ res ;
      status := 1
  with e ->
    prerr_endline @@ "--- Failure: " ^ jingoo_f ;
    prerr_endline @@ Printexc.to_string e ;
    status := 1

let () =
  let jingoo = ls (Sys.getenv "DOC_SAMPLE_DIR") (fun f -> filename_extension f = ".jingoo") in
  List.iter test (List.sort compare jingoo) ;
  exit !status
