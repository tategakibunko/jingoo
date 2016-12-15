open Jg_types

let to_md5 ?(defaults=[
  ("seed", Tstr "");
]) value kwargs =
  let seed = Jg_runtime.jg_get_kvalue "seed" kwargs ~defaults in
  match value, seed with
    | Tstr str, Tstr seed ->
      Tstr (Digest.to_hex (Digest.string (str ^ seed)))
    | _ -> Tnull

let () =
  Jg_stub.add_func "my_ext" "to_md5" (Jg_runtime.func_arg1 to_md5)
