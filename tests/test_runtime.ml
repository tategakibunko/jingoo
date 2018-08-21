open OUnit2
open Jg_utils
open Jg_types
open Jg_runtime

let kwargs = []
;;

let tval_equal t1 t2 =
  match jg_eq_eq t1 t2 with
    | Tbool ret -> ret
    | _ -> failwith "tval_equal:invalid op"

let test_escape ctx =
  assert_equal (Tstr "&#60;script&#62;") (jg_escape_html (Tstr "<script>") kwargs);
  assert_equal (Tstr "&#34;&#34;") (jg_escape_html (Tstr "\"\"") kwargs);
  assert_equal
    (Tstr "Lo&#38;rem&#62;\n I&#60;ps&#34;um")
    (jg_escape_html (Tstr "Lo&rem>\n I<ps\"um") kwargs);
;;

let test_string_of_tvalue ctx =
  assert_equal "a" (string_of_tvalue (Tstr "a"));
  assert_equal "1" (string_of_tvalue (Tint 1));
  assert_equal "1." (string_of_tvalue (Tfloat 1.0));
  assert_equal "1.2" (string_of_tvalue (Tfloat 1.2));
  assert_equal "<obj>" (string_of_tvalue (Tobj [("name", Tstr "value")]));
  assert_equal "<list>" (string_of_tvalue (Tlist [Tint 0; Tint 1]));
;;

let test_plus ctx =
  assert_equal (Tint 2) (jg_plus (Tint 1) (Tint 1));
  assert_equal (Tint 0) (jg_plus (Tint 1) (Tint (-1)));
  assert_equal (Tfloat 1.0) (jg_plus (Tint 0) (Tfloat 1.0));
  assert_equal (Tfloat 2.0) (jg_plus (Tfloat 1.0) (Tfloat 1.0));
;;

let test_minus ctx =
  assert_equal (Tint 0) (jg_minus (Tint 1) (Tint 1));
  assert_equal (Tint 1) (jg_minus (Tint 2) (Tint 1));
  assert_equal (Tint 2) (jg_minus (Tint 1) (Tint (-1)));
  assert_equal (Tfloat (-1.0)) (jg_minus (Tint 0) (Tfloat 1.0));
  assert_equal (Tfloat 1.0) (jg_minus (Tint 1) (Tfloat 0.0));
  assert_equal (Tfloat 0.0) (jg_minus (Tfloat 1.0) (Tfloat 1.0));
;;

let test_list_eq_eq ctx =
  let lst1 = [Tint 0; Tint 1; Tint 2] in
  let lst2 = [Tint 0; Tint 1; Tint 2] in
  let lst3 = [Tint 0; Tint 1; Tint 3] in
  let lst4 = [Tint 0; Tint 1] in
  assert_equal (jg_list_eq_eq lst1 lst2) true;
  assert_equal (jg_list_eq_eq lst1 lst3) false;
  assert_equal (jg_list_eq_eq lst1 lst4) false;
;;

let test_obj_eq_eq ctx =
  let obj1 = Tobj [("name", Tstr "john"); ("age", Tint 20)] in
  let obj2 = Tobj [("name", Tstr "john"); ("age", Tint 20)] in
  let obj3 = Tobj [("name", Tstr "mary"); ("age", Tint 22)] in
  let obj4 = Tobj [("age", Tint 20); ("name", Tstr "john")] in
  assert_equal (jg_obj_eq_eq obj1 obj2) true;
  assert_equal (jg_obj_eq_eq obj1 obj3) false;
  assert_equal (jg_obj_eq_eq obj1 obj4) true;
;;

let test_batch ctx =
  let lst = jg_range (Tint 0) (Tint 9) [] in 
  let batched_list = jg_batch (Tint 4) lst [("fill_with", Tstr "x")] in
  let expect_list = Tlist [
    Tlist [(Tint 0); (Tint 1); (Tint 2); (Tint 3)];
    Tlist [(Tint 4); (Tint 5); (Tint 6); (Tint 7)];
    Tlist [(Tint 8); (Tint 9); (Tstr "x"); (Tstr "x")];
  ] in
  assert_equal (jg_eq_eq batched_list expect_list) (Tbool true)
;;

let test_capitalize ctx =
  let orig = Tstr "car" in
  let caps = Tstr "Car" in
  assert_equal (jg_capitalize orig kwargs) caps
;;  

let test_default ctx =
  assert_equal (jg_default (Tstr "hello") Tnull kwargs) (Tstr "hello");
  assert_equal (jg_default (Tstr "hello") (Tstr "hoge") kwargs) (Tstr "hoge")
;;

let test_length ctx =
  assert_equal (jg_length (Tstr "hoge") kwargs) (Tint 4);
  assert_equal (jg_length (Tstr "日本語") kwargs) (Tint 3);
  assert_equal (jg_length (Tlist [Tint 0; Tint 1]) kwargs) (Tint 2);
;;

let test_strlen ctx =
  assert_equal (jg_strlen (Tstr "hoge") kwargs) (Tint 4);
  assert_equal (jg_strlen (Tstr "日本語") kwargs) (Tint 3);
;;

let test_abs ctx =
  assert_equal (jg_abs (Tint (-1)) kwargs) (Tint 1);
  assert_equal (jg_abs (Tint 1) kwargs) (Tint 1);
;;

let test_upper ctx =
  assert_equal (jg_upper (Tstr "aaa") kwargs) (Tstr "AAA")
;;

let test_lower ctx =
  assert_equal (jg_lower (Tstr "AAA") kwargs) (Tstr "aaa") 
;;

let test_join ctx =
  assert_equal (jg_join (Tstr ",") (Tlist [Tstr "a"; Tstr "b"]) kwargs) (Tstr "a,b")
;;

let test_substring ctx =
  assert_equal (jg_substring (Tint 0) (Tint 1) (Tstr "hoge") kwargs) (Tstr "h");
  assert_equal (jg_substring (Tint 1) (Tint 1) (Tstr "hoge") kwargs) (Tstr "o");
  assert_equal (jg_substring (Tint 2) (Tint 1) (Tstr "hoge") kwargs) (Tstr "g");
  assert_equal (jg_substring (Tint 3) (Tint 1) (Tstr "hoge") kwargs) (Tstr "e");
  assert_equal (jg_substring (Tint 4) (Tint 1) (Tstr "hoge") kwargs) (Tstr "");
  assert_equal (jg_substring (Tint 5) (Tint 1) (Tstr "hoge") kwargs) (Tstr "");
  assert_equal (jg_substring (Tint 5) (Tint 0) (Tstr "hoge") kwargs) (Tstr "");
  assert_equal (jg_substring (Tint 0) (Tint 0) (Tstr "hoge") kwargs) (Tstr "");
  assert_equal (jg_substring (Tint 0) (Tint 2) (Tstr "hoge") kwargs) (Tstr "ho");
  assert_equal (jg_substring (Tint 0) (Tint 4) (Tstr "hoge") kwargs) (Tstr "hoge");
  assert_equal (jg_substring (Tint 0) (Tint 5) (Tstr "hoge") kwargs) (Tstr "hoge");

  (** negative base *)
  assert_equal (jg_substring (Tint (-1)) (Tint 1) (Tstr "hoge") kwargs) (Tstr "e");
  assert_equal (jg_substring (Tint (-2)) (Tint 1) (Tstr "hoge") kwargs) (Tstr "g");
  assert_equal (jg_substring (Tint (-3)) (Tint 1) (Tstr "hoge") kwargs) (Tstr "o");
  assert_equal (jg_substring (Tint (-4)) (Tint 1) (Tstr "hoge") kwargs) (Tstr "h");
  assert_equal (jg_substring (Tint (-4)) (Tint 2) (Tstr "hoge") kwargs) (Tstr "ho");
  assert_equal (jg_substring (Tint (-4)) (Tint 3) (Tstr "hoge") kwargs) (Tstr "hog");
  assert_equal (jg_substring (Tint (-4)) (Tint 4) (Tstr "hoge") kwargs) (Tstr "hoge");
  assert_equal (jg_substring (Tint (-4)) (Tint 5) (Tstr "hoge") kwargs) (Tstr "hoge");
  assert_equal (jg_substring (Tint (-5)) (Tint 1) (Tstr "hoge") kwargs) (Tstr "e");

  assert_equal (jg_substring (Tint 0) (Tint 1) (Tstr "日本語") kwargs) (Tstr "日");
  assert_equal (jg_substring (Tint 0) (Tint 2) (Tstr "日本語") kwargs) (Tstr "日本");
  assert_equal (jg_substring (Tint 0) (Tint 3) (Tstr "日本語") kwargs) (Tstr "日本語");
  assert_equal (jg_substring (Tint 0) (Tint 4) (Tstr "日本語") kwargs) (Tstr "日本語");
  assert_equal (jg_substring (Tint 1) (Tint 4) (Tstr "日本語") kwargs) (Tstr "本語");
  assert_equal (jg_substring (Tint 0) (Tint 10) Tnull kwargs) (Tstr "")
;;

let test_truncate ctx =
  assert_equal (jg_truncate (Tint 3) (Tstr "123456789") kwargs) (Tstr "123")
;;

let test_md5 ctx =
  let src = "hoge" in
  let md5 = String.lowercase src |> Digest.string |> Digest.to_hex in
  assert_equal (jg_md5 (Tstr src) kwargs) (Tstr md5)
;;

let test_reverse ctx =
  let lst = [Tint 0; Tint 1; Tint 2] in
  let rev = List.rev lst in
  let rev' = jg_reverse (Tlist lst)  kwargs in
  List.iter2 assert_equal rev (unbox_list rev')
;;

let test_last ctx =
  let lst = Tlist [Tint 0; Tint 1] in
  assert_equal (jg_last lst kwargs) (Tint 1)
;;

let test_replace ctx =
  let str = Tstr "hoge" in
  let src = Tstr "ho" in
  let dst = Tstr "hi" in
  let str'= Tstr "hige" in
  assert_equal (jg_replace src dst str kwargs) str'
;;

let test_replace_uni ctx =
  let src = Tstr "日本" in
  let dst = Tstr "英" in
  let str = Tstr "日本語" in
  let str'= Tstr "英語" in
  assert_equal (jg_replace src dst str kwargs) str'
;;

let test_random ctx =
  let rec iter ret i =
    if i < 100 then iter ((Tint i) :: ret) (i+1) else ret in
  let lst = iter [] 1 in
  let lst'= unbox_list @@ jg_random (Tlist lst) kwargs in
  let is_eq_eq = List.for_all2 (=) lst lst' in
  assert_equal is_eq_eq false
;;

let test_slice ctx =
  let lst = Tlist [Tint 1; Tint 2; Tint 3; Tint 4; Tint 5] in
  let expect = Tlist [
    Tlist [Tint 1; Tint 2];
    Tlist [Tint 3; Tint 4];
    Tlist [Tint 5];
  ] in
  let result = jg_slice (Tint 2) lst kwargs in
  assert_equal expect result
;;

let test_wordcount ctx =
  assert_equal (jg_wordcount (Tstr "hoge hige hage") kwargs) (Tint 3);
  assert_equal (jg_wordcount (Tstr "hoge") kwargs) (Tint 1);
  assert_equal (jg_wordcount (Tstr "") kwargs) (Tint 0);
  assert_equal (jg_wordcount (Tstr "日　本　語") kwargs) (Tint 3)
;;

let test_trim ctx =
  assert_equal (jg_trim (Tstr " a \n b c ") kwargs) (Tstr "a \n b c");
  assert_equal (jg_trim (Tstr "　日　本\n　　語　　　") kwargs) (Tstr "日　本\n　　語")
;;

let test_round ctx =
  assert_equal (jg_round (Tstr "floor") (Tfloat 1.5) kwargs) (Tfloat 1.0);
  assert_equal (jg_round (Tstr "ceil") (Tfloat 1.5) kwargs) (Tfloat 2.0)
;;

let test_range ctx =
  assert_equal (jg_range (Tint 0) (Tint 2) kwargs) (Tlist [Tint 0; Tint 1; Tint 2]);
  assert_equal (jg_range (Tint 2) (Tint 0) kwargs) (Tlist [Tint 2; Tint 1; Tint 0]);
  assert_equal (jg_range (Tint 2012) (Tint 2006) kwargs) (Tlist [Tint 2012; Tint 2011; Tint 2010; Tint 2009; Tint 2008; Tint 2007; Tint 2006]);
;;

let test_sum ctx =
  assert_equal (jg_sum (Tlist [Tint 0; Tint 1; Tint 2]) kwargs) (Tint 3);
  assert_equal (jg_sum (Tlist [Tint 0; Tint 1; Tfloat 2.1]) kwargs) (Tfloat 3.1)
;;

let test_int ctx =
  assert_equal (jg_int (Tint 1) kwargs) (Tint 1);
  assert_equal (jg_int (Tfloat 1.0) kwargs) (Tint 1)
;;

let test_float ctx =
  assert_equal (jg_float (Tfloat 1.0) kwargs) (Tfloat 1.0);
  assert_equal (jg_float (Tint 1) kwargs) (Tfloat 1.0)
;;


let test_times ctx =
  assert_equal (jg_times (Tint 0) (Tint 1)) (Tint 0);
  assert_equal (jg_times (Tint 1) (Tint 1)) (Tint 1);
  assert_equal (jg_times (Tint 2) (Tint 2)) (Tint 4);
  assert_equal (jg_times (Tfloat 1.0) (Tint 2)) (Tfloat 2.0);
  assert_equal (jg_times (Tfloat 2.0) (Tfloat 2.0)) (Tfloat 4.0);
  assert_equal (jg_times (Tfloat 0.0) (Tfloat 2.0)) (Tfloat 0.0);
  assert_equal (jg_times (Tfloat 0.0) (Tint 1)) (Tfloat 0.0)
;;

let test_power ctx =
  assert_equal (jg_power (Tint 2) (Tint (-1))) (Tfloat 1.0);
  assert_equal (jg_power (Tint 2) (Tint 0)) (Tfloat 1.0);
  assert_equal (jg_power (Tint 2) (Tint 1)) (Tfloat 2.0);
  assert_equal (jg_power (Tint 2) (Tint 10)) (Tfloat 1024.0);
;;

let test_div ctx =
  assert_raises (Failure "jg_div:zero division error") (fun () -> jg_div (Tint 4) (Tint 0));
  assert_raises (Failure "jg_div:zero division error") (fun () -> jg_div (Tint 4) (Tfloat 0.0));
  assert_equal (jg_div (Tint 4) (Tint 2)) (Tint 2);
  assert_equal (jg_div (Tfloat 4.0) (Tint 2)) (Tfloat 2.0)
;;

let test_mod ctx =
  assert_raises (Failure "jg_mod:zero division error") (fun () -> jg_mod (Tint 4) (Tint 0));
  assert_equal (jg_mod (Tint 4) (Tint 3)) (Tint 1);
  assert_equal (jg_mod (Tint 4) (Tint 1)) (Tint 0)
;;

let test_and ctx =
  assert_equal (jg_and (Tbool true) (Tbool true)) (Tbool true);
  assert_equal (jg_and (Tbool true) (Tbool false)) (Tbool false);
  assert_equal (jg_and (Tbool false) (Tbool true)) (Tbool false);
  assert_equal (jg_and (Tbool false) (Tbool false)) (Tbool false)
;;

let test_or ctx =
  assert_equal (jg_or (Tbool true) (Tbool true)) (Tbool true);
  assert_equal (jg_or (Tbool true) (Tbool false)) (Tbool true);
  assert_equal (jg_or (Tbool false) (Tbool true)) (Tbool true);
  assert_equal (jg_or (Tbool false) (Tbool false)) (Tbool false)
;;

let test_eq_eq ctx =
  assert_equal (jg_eq_eq (Tint 1) (Tint 1)) (Tbool true);
  assert_equal (jg_eq_eq (Tint 1) (Tfloat 1.0)) (Tbool false);
  assert_equal (jg_eq_eq (Tfloat 1.0) (Tfloat 1.0)) (Tbool true);
  assert_equal (jg_eq_eq (Tstr "hoge") (Tstr "hoge")) (Tbool true);
  assert_equal (jg_eq_eq (Tstr "hoge") (Tstr "hige")) (Tbool false);
  assert_equal (jg_eq_eq (Tstr "日本語") (Tstr "日本語")) (Tbool true);
  assert_equal (jg_eq_eq (Tstr "日本語") (Tstr "英語")) (Tbool false);
  assert_equal (jg_eq_eq (Tint 0) (Tstr "hoge")) (Tbool false)
;;

let test_urlize ctx =
  let text = Tstr "go to http://yahoo.co.jp" in
  match jg_urlize text  kwargs with
    | Tstr text' ->
      assert_equal text' "go to <a href='http://yahoo.co.jp'>http://yahoo.co.jp</a>"
    | _ -> failwith "ouch"
;;

let test_title ctx =
  let text = Tstr "this is it!" in
  match jg_title text kwargs with
    | Tstr text' ->
      assert_equal text' "This Is It!"
    | _ -> failwith "ouch"
;;

let test_striptags ctx =
  let text = Tstr "<p class='indent'>hogehoge</p> higehige <b>hagehage</b>" in
  match jg_striptags text kwargs with
    | Tstr text' ->
      assert_equal text' "hogehoge higehige hagehage"
    | _ -> failwith "ouch"
;;

let test_sort_int_list ctx =
  let lst = Tlist [Tint 3; Tint 1; Tint 2] in
  match jg_sort lst kwargs with
    | Tlist lst' ->
      assert_equal lst' [Tint 1; Tint 2; Tint 3]
    | _ -> failwith "ouch"
;;

let test_sort_int_array ctx =
  let ary = Tarray [| Tint 3; Tint 1; Tint 2 |] in
  match jg_sort ary kwargs with
  | Tarray ary' ->
     assert_equal ary' [| Tint 1; Tint 2; Tint 3 |]
  | _ -> failwith "ouch"
;;

let test_sort_float_list ctx =
  let lst = Tlist [Tfloat 3.0; Tfloat 1.1; Tfloat 2.2] in
  match jg_sort lst kwargs with
    | Tlist lst' ->
      assert_equal lst' [Tfloat 1.1; Tfloat 2.2; Tfloat 3.0]
    | _ -> failwith "ouch"
;;

let test_sort_float_array ctx =
  let ary = Tarray [| Tfloat 3.0; Tfloat 1.1; Tfloat 2.2 |] in
  match jg_sort ary kwargs with
    | Tarray ary' ->
      assert_equal ary' [| Tfloat 1.1; Tfloat 2.2; Tfloat 3.0 |]
    | _ -> failwith "ouch"
;;

let test_sort_string_list ctx =
  let lst = Tlist [Tstr "baba"; Tstr "aa"; Tstr "caca"] in
  match jg_sort lst kwargs with
    | Tlist lst' ->
      assert_equal lst' [Tstr "aa"; Tstr "baba"; Tstr "caca"]
    | _ -> failwith "ouch"
;;

let test_sort_rev ctx =
  let lst = Tlist [Tint 3; Tint 1; Tint 2] in
  match jg_sort lst [("reverse", Tbool true)] with
  | Tlist lst' ->
     assert_equal lst' [Tint 3; Tint 2; Tint 1]
  | _ -> failwith "ouch"
;;

let test_sort_attr ctx =
  let persons = Tlist [
    Tobj [("name", Tstr "bob"); ("info", Tobj [
      ("age", Tint 20)
    ])];
    Tobj [("name", Tstr "ken"); ("info", Tobj [
      ("age", Tint 25);
    ])];
  ] in
  let name_is name = function
    | Tobj alist -> unbox_string (List.assoc "name" alist) = name
    | _ -> failwith "invalid obj" in
  let forward_sorted = jg_sort persons [("attribute", Tstr "info.age")] |> unbox_list in
  let forward_expected = [name_is "bob"; name_is "ken"] in
  let reverse_sorted = jg_sort persons [("attribute", Tstr "info.age"); ("reverse", Tbool true)] |> unbox_list in
  let reverse_expected = [name_is "ken"; name_is "bob"] in
  let check_person checker person = checker person in
  assert_equal (List.for_all2 check_person forward_expected forward_sorted) true;
  assert_equal (List.for_all2 check_person reverse_expected reverse_sorted) true;
;;

let test_sort_string_array ctx =
  let ary = Tarray [| Tstr "baba"; Tstr "aa"; Tstr "caca" |] in
  match jg_sort ary kwargs with
  | Tarray ary' ->
     assert_equal ary' [| Tstr "aa"; Tstr "baba"; Tstr "caca" |]
  | _ -> failwith "ouch"
;;

let test_list ctx =
  match jg_list (Tstr "hoge") kwargs with
    | Tlist lst ->
      assert_equal lst [Tstr "h"; Tstr "o"; Tstr "g"; Tstr "e"]
    | _ -> failwith "ouch"
;;

let test_xmlattr ctx =
  let obj = Tobj [
    ("class", Tstr "profile");
    ("id", Tstr "taro");
    ("width", Tint 300);
  ] in
  match jg_xmlattr obj kwargs with
    | Tstr str ->
      assert_equal str "class='profile' id='taro' width='300'"
    | _ -> failwith "ouch"
;;

let test_wordwrap ctx =
  let text = String.concat " " [
    "this is it!!";
    "hoge hogehogehoge";
  ] in
  (match jg_wordwrap (Tint 12) (Tbool true) (Tstr text) kwargs with
    | Tstr text ->
      assert_equal text "this is it!!\nhoge hogehog\nehoge"
    | _ -> failwith "ouch");
  (match jg_wordwrap (Tint 12) (Tbool false) (Tstr text) kwargs with
    | Tstr text ->
      assert_equal text "this is it!!\nhoge hogehogehoge"
    | _ -> failwith "ouch")
;;

let test_sublist ctx =
  let lst = Tlist [Tint 0; Tint 1; Tint 2; Tint 3] in
  (match jg_sublist (Tint 0) (Tint 4) lst kwargs with
    | Tlist lst -> assert_equal lst [Tint 0; Tint 1; Tint 2; Tint 3]
    | _ -> failwith "ouch");
  (match jg_sublist (Tint 0) (Tint 3) lst kwargs with
    | Tlist lst -> assert_equal lst [Tint 0; Tint 1; Tint 2]
    | _ -> failwith "ouch");
  (match jg_sublist (Tint 0) (Tint 2) lst kwargs with
    | Tlist lst -> assert_equal lst [Tint 0; Tint 1]
    | _ -> failwith "ouch");
  (match jg_sublist (Tint 0) (Tint 1) lst kwargs with
    | Tlist lst -> assert_equal lst [Tint 0]
    | _ -> failwith "ouch");
  (match jg_sublist (Tint 0) (Tint 0) lst kwargs with
    | Tlist lst -> assert_equal lst []
    | _ -> failwith "ouch");
  (match jg_sublist (Tint 1) (Tint 0) lst kwargs with
    | Tlist lst -> assert_equal lst []
    | _ -> failwith "ouch");
  (match jg_sublist (Tint 1) (Tint 1) lst kwargs with
    | Tlist lst -> assert_equal lst [Tint 1]
    | _ -> failwith "ouch");
  (match jg_sublist (Tint 1) (Tint 2) lst kwargs with
    | Tlist lst -> assert_equal lst [Tint 1; Tint 2]
    | _ -> failwith "ouch");
  (match jg_sublist (Tint 1) (Tint 3) lst kwargs with
    | Tlist lst -> assert_equal lst [Tint 1; Tint 2; Tint 3]
    | _ -> failwith "ouch");
  (match jg_sublist (Tint 1) (Tint 4) lst kwargs with
    | Tlist lst -> assert_equal lst [Tint 1; Tint 2; Tint 3]
    | _ -> failwith "ouch");
;;

let test_fmt_float ctx =
  let value = Tfloat 3.141592 in
  assert_equal (jg_fmt_float (Tint 1) value kwargs) (Tfloat 3.1);
  assert_equal (jg_fmt_float (Tint 2) value kwargs) (Tfloat 3.14);
  assert_equal (jg_fmt_float (Tint 3) value kwargs) (Tfloat 3.142);
  assert_equal (jg_fmt_float (Tint 4) value kwargs) (Tfloat 3.1416);
;;

let test_divisibleby ctx =
  assert_equal (jg_test_divisibleby (Tint 2) (Tint 6) kwargs) (Tbool true);
  assert_equal (jg_test_divisibleby (Tint 5) (Tint 6) kwargs) (Tbool false);
  assert_equal (jg_test_divisibleby (Tint 0) (Tint 6) kwargs) (Tbool false);
;;

let test_even ctx =
  assert_equal (jg_test_even (Tint 0) kwargs) (Tbool true);
  assert_equal (jg_test_even (Tint 1) kwargs) (Tbool false);
  assert_equal (jg_test_even (Tint 2) kwargs) (Tbool true);
  assert_equal (jg_test_even (Tint 3) kwargs) (Tbool false);
;;

let test_odd ctx =
  assert_equal (jg_test_odd (Tint 0) kwargs) (Tbool false);
  assert_equal (jg_test_odd (Tint 1) kwargs) (Tbool true);
  assert_equal (jg_test_odd (Tint 2) kwargs) (Tbool false);
  assert_equal (jg_test_odd (Tint 3) kwargs) (Tbool true);
;;

let test_iterable ctx =
  assert_equal (jg_test_iterable (Tint 0) kwargs) (Tbool false);
  assert_equal (jg_test_iterable (Tfloat 1.0) kwargs) (Tbool false);
  assert_equal (jg_test_iterable (Tstr "hoge") kwargs) (Tbool true);
  assert_equal (jg_test_iterable (Tobj []) kwargs) (Tbool true);
  assert_equal (jg_test_iterable (Tlist []) kwargs) (Tbool true);
  assert_equal (jg_test_iterable (Tset []) kwargs) (Tbool true);
  assert_equal (jg_test_iterable Tnull kwargs) (Tbool true);
;;

let test_lower ctx =
  assert_equal (jg_test_lower (Tstr "aaa") kwargs) (Tbool true);
  assert_equal (jg_test_lower (Tstr "aaA") kwargs) (Tbool false)
;;

let test_upper ctx =
  assert_equal (jg_test_upper (Tstr "aaa") kwargs) (Tbool false);
  assert_equal (jg_test_upper (Tstr "AAA") kwargs) (Tbool true);
;;

let test_number ctx =
  assert_equal (jg_test_number (Tint 1) kwargs) (Tbool true);
  assert_equal (jg_test_number (Tfloat 1.0) kwargs) (Tbool true);
  assert_equal (jg_test_number (Tstr "1") kwargs) (Tbool false);
;;

let test_string ctx =
  assert_equal (jg_test_string (Tstr "aaa") kwargs) (Tbool true);
  assert_equal (jg_test_string (Tint 1) kwargs) (Tbool false);
;;

let test_groupby ctx =
  let person ~gender ~first_name ~last_name ~native_lang ~second_lang =
    Tpat (function
    | "gender" -> Tstr gender
    | "lang" -> Tpat (function
      | "native" -> Tstr native_lang
      | "second" -> Tstr second_lang
      | _ -> raise Not_found)
    | "first_name" -> Tstr first_name
    | "last_name" -> Tstr last_name
    | _ -> raise Not_found) in
  let persons = Tlist [
    person ~gender:"F" ~first_name:"Tobi" ~last_name:"Legault" ~native_lang:"French" ~second_lang:"English";
    person ~gender:"M" ~first_name:"Kip" ~last_name:"Schon" ~native_lang:"French" ~second_lang:"German";
    person ~gender:"F" ~first_name:"Lorriane" ~last_name:"Olive" ~native_lang:"English" ~second_lang:"Spanish";
    person ~gender:"F" ~first_name:"Hana" ~last_name:"Breton" ~native_lang:"French" ~second_lang:"German";
    person ~gender:"M" ~first_name:"Arlen" ~last_name:"Aubrey" ~native_lang:"English" ~second_lang:"French";
  ] in
  let groups_by_gender = unbox_list @@ jg_groupby (Tstr "gender") persons kwargs in
  let groups_by_lang_native = unbox_list @@ jg_groupby (Tstr "lang.native") persons kwargs in
  let get_group grouper groups =
    List.find (function
    | Tpat fn -> unbox_string (fn "grouper") = grouper
    | _ -> failwith "invalid item") groups in
  let get_group_list = function
    | Tpat fn -> unbox_list (fn "list")
    | _ -> failwith "invalid item" in
  let females = get_group "F" groups_by_gender |> get_group_list in
  let males = get_group "M" groups_by_gender |> get_group_list in
  let english_speakers = get_group "English" groups_by_lang_native |> get_group_list in
  let french_speakers = get_group "French" groups_by_lang_native |> get_group_list in
  (* checks only full name for ease *)
  let full_name_is ~first_name ~last_name item =
    unbox_string (jg_obj_lookup item "first_name") = first_name &&
    unbox_string (jg_obj_lookup item "last_name") = last_name in
  let females_expected = [
    full_name_is ~first_name:"Tobi" ~last_name:"Legault";
    full_name_is ~first_name:"Lorriane" ~last_name:"Olive";
    full_name_is ~first_name:"Hana" ~last_name:"Breton";
  ] in
  let males_expected = [
    full_name_is ~first_name:"Kip" ~last_name:"Schon";
    full_name_is ~first_name:"Arlen" ~last_name:"Aubrey";
  ] in
  let english_speakers_expected = [
    full_name_is ~first_name:"Lorriane" ~last_name:"Olive";
    full_name_is ~first_name:"Arlen" ~last_name:"Aubrey";
  ] in
  let french_speakers_expected = [
    full_name_is ~first_name:"Tobi" ~last_name:"Legault";
    full_name_is ~first_name:"Kip" ~last_name:"Schon";
    full_name_is ~first_name:"Hana" ~last_name:"Breton";
  ] in
  let check_person checker person = checker person in
  assert_equal (List.for_all2 check_person females_expected females) true;
  assert_equal (List.for_all2 check_person males_expected males) true;
  assert_equal (List.for_all2 check_person english_speakers_expected english_speakers) true;
  assert_equal (List.for_all2 check_person french_speakers_expected french_speakers) true
;;
  
let test_min_max ctx =
  let numbers = Tlist [Tint 3; Tint 1; Tint 2] in
  let persons = Tlist [
    Tobj [("name", Tstr "taro"); ("age", Tint 12)];
    Tobj [("name", Tstr "jiro"); ("age", Tint 10)];
    Tobj [("name", Tstr "hana"); ("age", Tint 13)];
  ] in
  let min_number = jg_min numbers [] in
  let max_number = jg_max numbers [] in
  let min_person = jg_min persons [("attribute", Tstr "age")] in
  let max_person = jg_max persons [("attribute", Tstr "age")] in
  assert_equal min_number (Tint 1);
  assert_equal max_number (Tint 3);
  assert_equal (jg_obj_lookup min_person "name") (Tstr "jiro");
  assert_equal (jg_obj_lookup max_person "name") (Tstr "hana")
;;

let test_nth ctx =
  let list = Tlist [Tint 3; Tint 0; Tint 2] in
  let ary = Tarray [| Tint 1; Tint 10; Tint 12 |] in
  assert_equal (jg_nth list 0) (Tint 3);
  assert_equal (jg_nth list 1) (Tint 0);
  assert_equal (jg_nth list 2) (Tint 2);
  assert_equal (jg_nth ary 0) (Tint 1);
  assert_equal (jg_nth ary 1) (Tint 10);
  assert_equal (jg_nth ary 2) (Tint 12);
;;

let suite = "runtime test" >::: [
  "test_escape" >:: test_escape;
  "test_string_of_tvalue" >:: test_string_of_tvalue;
  "test_plus" >:: test_plus;
  "test_minus" >:: test_minus;
  "test_times" >:: test_times;
  "test_power" >:: test_power;
  "test_div" >:: test_div;
  "test_mod" >:: test_mod;
  "test_and" >:: test_and;
  "test_or" >:: test_or;
  "test_eq_eq" >:: test_eq_eq;
  "test_batch" >:: test_batch;
  "test_list_eq_eq" >:: test_list_eq_eq;
  "test_obj_eq_eq" >:: test_obj_eq_eq;
  "test_capitalize" >:: test_capitalize;
  "test_default" >:: test_default;
  "test_length" >:: test_length;
  "test_strlen" >:: test_strlen;
  "test_abs" >:: test_abs;
  "test_upper" >:: test_upper;
  "test_lower" >:: test_lower;
  "test_join" >:: test_join;
  "test_substring" >:: test_substring;
  "test_truncate" >:: test_truncate;
  "test_md5" >:: test_md5;
  "test_reverse" >:: test_reverse;
  "test_last" >:: test_last;
  "test_replace" >:: test_replace;
  "test_replace_uni" >:: test_replace_uni;
  "test_random" >:: test_random;
  "test_slice" >:: test_slice;
  "test_wordcount" >:: test_wordcount;
  "test_trim" >:: test_trim;
  "test_round" >:: test_round;
  "test_range" >:: test_range;
  "test_sum" >:: test_sum;
  "test_float" >:: test_float;
  "test_urlize" >:: test_urlize;
  "test_title" >:: test_title;
  "test_striptags" >:: test_striptags;
  "test_list" >:: test_list;
  "test_sort_int_list" >:: test_sort_int_list;
  "test_sort_int_array" >:: test_sort_int_array;
  "test_sort_float_list" >:: test_sort_float_list;
  "test_sort_float_array" >:: test_sort_float_array;
  "test_sort_string_list" >:: test_sort_string_list;
  "test_sort_string_array" >:: test_sort_string_array;
  "test_sort_rev" >:: test_sort_rev;
  "test_sort_attr" >:: test_sort_attr;
  "test_xmlattr" >:: test_xmlattr;
  "test_wordwrap" >:: test_wordwrap;
  "test_sublist" >:: test_sublist;
  "test_fmt_float" >:: test_fmt_float;
  "test_divisibleby" >:: test_divisibleby;
  "test_even" >:: test_even;
  "test_odd" >:: test_odd;
  "test_iterable" >:: test_iterable;
  "test_lower" >:: test_lower;
  "test_upper" >:: test_upper;
  "test_number" >:: test_number;
  "test_string" >:: test_string;
  "test_groupby" >:: test_groupby;
  "test_min_max" >:: test_min_max;
  "test_nth" >:: test_nth;
]
;;
