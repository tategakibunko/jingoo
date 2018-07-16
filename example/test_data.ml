open Jg_types

let build_ht () =
  let ht = Hashtbl.create 10 in
  Hashtbl.add ht "a" (Tint 1);
  Hashtbl.add ht "b" (Tint 2);
  Thash ht

let rec lazy_model n =
  Tlazy (lazy (Tpat (function "cur" -> Tint n
                            | "prev" -> lazy_model (n - 1)
                            | "next" -> lazy_model (n + 1)
                            | _ -> raise Not_found ) ) )

let volatile = ref false

let models = [
  ("msg", Tstr "hello world");
  ("list1", Tlist [Tint 1]);
  ("list", Tlist [Tint 10; Tint 20; Tint 30]);
  ("long_list", Tlist [Tint 10; Tint 20; Tint 30; Tint 40; Tint 50; Tint 60; Tint 70; Tint 80; Tint 90; Tint 100]);
  ("obj", Tobj [("name", Tstr "hoge"); ("age", Tint 10)]);
  ("obj2", Tobj [
    ("child", Tobj [
      ("name", Tstr "aaa")
     ]);
   ]);
  ("rows", Tlist [
    Tobj [("name", Tstr "bob"); ("age", Tint 20)];
    Tobj [("name", Tstr "ken"); ("age", Tint 25)];
  ]);
  ("hash1", build_ht ());
  ("array1", Tarray [| Tstr "this"; Tstr "is"; Tstr "from"; Tstr "array" |]);
  ("lazy", lazy_model 0);
  ("volatile", Tvolatile (fun () -> Tbool !volatile));
]

let _ = volatile := true

