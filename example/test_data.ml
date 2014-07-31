open Jg_types

let models = [
  ("msg", Tstr "hello world");
  ("list1", Tlist [Tint 1]);
  ("list", Tlist [Tint 10; Tint 20; Tint 30]);
  ("long_list", Tlist [Tint 10; Tint 20; Tint 30; Tint 40; Tint 50;	Tint 60; Tint 70; Tint 80; Tint 90; Tint 100]);
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
] 
;;
