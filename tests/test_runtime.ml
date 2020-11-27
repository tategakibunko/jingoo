open OUnit2
open Jingoo
open Jg_types
open Jg_runtime

let assert_equal_tvalue =
  let cmp a b = jg_eq_eq a b = Tbool true in
  assert_equal ~cmp ~printer:Jg_types.show_tvalue

let test_persons = Tlist [
  Tobj [("name", Tstr "taro"); ("age", Tint 12); ("extra", Tobj [
    ("rank", Tint 3);
  ])];
  Tobj [("name", Tstr "jiro"); ("age", Tint 10); ("extra", Tobj [
    ("rank", Tint 12);
  ])];
  Tobj [("name", Tstr "hana"); ("age", Tint 13); ("extra", Tobj [
    ("rank", Tint 5);
  ])];
]

let tval_equal t1 t2 =
  match jg_eq_eq t1 t2 with
    | Tbool ret -> ret
    | _ -> failwith "tval_equal:invalid op"

let test_escape _ctx =
  assert_equal_tvalue (Tstr "&lt;script&gt;") (jg_escape_html (Tstr "<script>"));
  assert_equal_tvalue (Tstr "&quot;&quot;") (jg_escape_html (Tstr "\"\""));
  assert_equal_tvalue (Tstr "&apos;&apos;") (jg_escape_html (Tstr "''"));
  assert_equal_tvalue
    (Tstr "Lo&amp;rem&gt;\n I&lt;ps&quot;um")
    (jg_escape_html (Tstr "Lo&rem>\n I<ps\"um"))

let test_string_of_tvalue _ctx =
  assert_equal "a" (string_of_tvalue (Tstr "a"));
  assert_equal "1" (string_of_tvalue (Tint 1));
  assert_equal "1." (string_of_tvalue (Tfloat 1.0));
  assert_equal "1.2" (string_of_tvalue (Tfloat 1.2));
  assert_equal "<obj>" (string_of_tvalue (Tobj [("name", Tstr "value")]));
  assert_equal "<list>" (string_of_tvalue (Tlist [Tint 0; Tint 1]))

let test_plus _ctx =
  assert_equal_tvalue (Tint 2) (jg_plus (Tint 1) (Tint 1));
  assert_equal_tvalue (Tint 0) (jg_plus (Tint 1) (Tint (-1)));
  assert_equal_tvalue (Tfloat 1.0) (jg_plus (Tint 0) (Tfloat 1.0));
  assert_equal_tvalue (Tfloat 2.0) (jg_plus (Tfloat 1.0) (Tfloat 1.0))

let test_minus _ctx =
  assert_equal_tvalue (Tint 0) (jg_minus (Tint 1) (Tint 1));
  assert_equal_tvalue (Tint 1) (jg_minus (Tint 2) (Tint 1));
  assert_equal_tvalue (Tint 2) (jg_minus (Tint 1) (Tint (-1)));
  assert_equal_tvalue (Tfloat (-1.0)) (jg_minus (Tint 0) (Tfloat 1.0));
  assert_equal_tvalue (Tfloat 1.0) (jg_minus (Tint 1) (Tfloat 0.0));
  assert_equal_tvalue (Tfloat 0.0) (jg_minus (Tfloat 1.0) (Tfloat 1.0))

let test_list_eq_eq _ctx =
  let lst1 = [Tint 0; Tint 1; Tint 2] in
  let lst2 = [Tint 0; Tint 1; Tint 2] in
  let lst3 = [Tint 0; Tint 1; Tint 3] in
  let lst4 = [Tint 0; Tint 1] in
  assert_equal ~cmp:jg_list_eq_eq lst1 lst2;
  assert_equal (jg_list_eq_eq lst1 lst3) false;
  assert_equal (jg_list_eq_eq lst1 lst4) false

let test_obj_eq_eq _ctx =
  let obj1 = Tobj [("name", Tstr "john"); ("age", Tint 20)] in
  let obj2 = Tobj [("name", Tstr "john"); ("age", Tint 20)] in
  let obj3 = Tobj [("name", Tstr "mary"); ("age", Tint 22)] in
  let obj4 = Tobj [("age", Tint 20); ("name", Tstr "john")] in
  assert_equal ~cmp:jg_obj_eq_eq obj1 obj2 ;
  assert_equal (jg_obj_eq_eq obj1 obj3) false;
  assert_equal ~cmp:jg_obj_eq_eq obj1 obj4

let test_batch_list _ctx =
  let ary = jg_range (Tint 0) (Tint 9) in
  let lst = Tlist (Array.to_list (unbox_array ary)) in
  let batched_list = jg_batch (Tint 4) lst ~kwargs:[("fill_with", Tstr "x")] in
  let expect_list = Tlist [
    Tlist [(Tint 0); (Tint 1); (Tint 2); (Tint 3)];
    Tlist [(Tint 4); (Tint 5); (Tint 6); (Tint 7)];
    Tlist [(Tint 8); (Tint 9); (Tstr "x"); (Tstr "x")];
  ] in
  assert_equal_tvalue expect_list batched_list

(* if fill_with keyword is not given, final row is shrinked by list_length mod slice_count *)
let test_batch_list2 _ctx =
  let ary = jg_range (Tint 0) (Tint 9) in
  let lst = Tlist (Array.to_list (unbox_array ary)) in
  let batched_list = jg_batch (Tint 4) lst in
  let expect_list = Tlist [
    Tlist [(Tint 0); (Tint 1); (Tint 2); (Tint 3)];
    Tlist [(Tint 4); (Tint 5); (Tint 6); (Tint 7)];
    Tlist [(Tint 8); (Tint 9)]
  ] in
  assert_equal_tvalue expect_list batched_list

let test_batch_array _ctx =
  let ary = jg_range (Tint 0) (Tint 9) in
  let batched_ary = jg_batch (Tint 4) ary ~kwargs:[("fill_with", Tstr "x")] in
  let expect_ary = Tarray [|
    Tarray [| (Tint 0); (Tint 1); (Tint 2); (Tint 3) |];
    Tarray [| (Tint 4); (Tint 5); (Tint 6); (Tint 7) |];
    Tarray [| (Tint 8); (Tint 9); (Tstr "x"); (Tstr "x") |];
  |] in
  assert_equal_tvalue expect_ary batched_ary

(* if fill_with keyword is not given, final row is shrinked by array_length mod slice_count *)
let test_batch_array2 _ctx =
  let ary = jg_range (Tint 0) (Tint 9) in
  let batched_ary = jg_batch (Tint 4) ary in
  let expect_ary = Tarray [|
    Tarray [| (Tint 0); (Tint 1); (Tint 2); (Tint 3) |];
    Tarray [| (Tint 4); (Tint 5); (Tint 6); (Tint 7) |];
    Tarray [| (Tint 8); (Tint 9) |];
  |] in
  assert_equal_tvalue expect_ary batched_ary

let test_capitalize _ctx =
  assert_equal_tvalue (jg_capitalize (Tstr "peter")) (Tstr "Peter");
  assert_equal_tvalue (jg_capitalize (Tstr "Paul")) (Tstr "Paul");
  assert_equal_tvalue (jg_capitalize (Tstr "MARY")) (Tstr "Mary");
  assert_equal_tvalue (jg_capitalize (Tstr "gUIDO")) (Tstr "Guido");
  assert_equal_tvalue (jg_capitalize (Tstr "my First cAR")) (Tstr "My first car")

let test_default _ctx =
  assert_equal_tvalue (jg_default (Tstr "hello") Tnull) (Tstr "hello");
  assert_equal_tvalue (jg_default (Tstr "hello") (Tstr "hoge")) (Tstr "hoge")

let test_length _ctx =
  assert_equal_tvalue (jg_length (Tstr "hoge")) (Tint 4);
  assert_equal_tvalue (jg_length (Tstr "日本語")) (Tint 3);
  assert_equal_tvalue (jg_length (Tstr "🐟🐠")) (Tint 2);
  assert_equal_tvalue (jg_length (Tlist [Tint 0; Tint 1])) (Tint 2)

let test_strlen _ctx =
  assert_equal_tvalue (jg_strlen (Tstr "hoge")) (Tint 4);
  assert_equal_tvalue (jg_strlen (Tstr "日本語")) (Tint 3);
  assert_equal_tvalue (jg_strlen (Tstr "🐟🐠")) (Tint 2)

let test_abs _ctx =
  assert_equal_tvalue (jg_abs (Tint (-1))) (Tint 1);
  assert_equal_tvalue (jg_abs (Tint 1)) (Tint 1)

let test_upper _ctx =
  assert_equal_tvalue (jg_upper (Tstr "aaa")) (Tstr "AAA")

let test_lower _ctx =
  assert_equal_tvalue (jg_lower (Tstr "AAA")) (Tstr "aaa")

let test_join _ctx =
  assert_equal_tvalue (jg_join (Tstr ",") (Tlist [Tstr "a"; Tstr "b"])) (Tstr "a,b")

let test_substring _ctx =
  assert_equal_tvalue (jg_substring (Tint 0) (Tint 1) (Tstr "hoge")) (Tstr "h");
  assert_equal_tvalue (jg_substring (Tint 1) (Tint 1) (Tstr "hoge")) (Tstr "o");
  assert_equal_tvalue (jg_substring (Tint 2) (Tint 1) (Tstr "hoge")) (Tstr "g");
  assert_equal_tvalue (jg_substring (Tint 3) (Tint 1) (Tstr "hoge")) (Tstr "e");
  assert_equal_tvalue (jg_substring (Tint 4) (Tint 1) (Tstr "hoge")) (Tstr "");
  assert_equal_tvalue (jg_substring (Tint 5) (Tint 1) (Tstr "hoge")) (Tstr "");
  assert_equal_tvalue (jg_substring (Tint 5) (Tint 0) (Tstr "hoge")) (Tstr "");
  assert_equal_tvalue (jg_substring (Tint 0) (Tint 0) (Tstr "hoge")) (Tstr "");
  assert_equal_tvalue (jg_substring (Tint 0) (Tint 2) (Tstr "hoge")) (Tstr "ho");
  assert_equal_tvalue (jg_substring (Tint 0) (Tint 4) (Tstr "hoge")) (Tstr "hoge");
  assert_equal_tvalue (jg_substring (Tint 0) (Tint 5) (Tstr "hoge")) (Tstr "hoge");

  (* negative base *)
  assert_equal_tvalue (jg_substring (Tint (-1)) (Tint 1) (Tstr "hoge")) (Tstr "e");
  assert_equal_tvalue (jg_substring (Tint (-2)) (Tint 1) (Tstr "hoge")) (Tstr "g");
  assert_equal_tvalue (jg_substring (Tint (-3)) (Tint 1) (Tstr "hoge")) (Tstr "o");
  assert_equal_tvalue (jg_substring (Tint (-4)) (Tint 1) (Tstr "hoge")) (Tstr "h");
  assert_equal_tvalue (jg_substring (Tint (-4)) (Tint 2) (Tstr "hoge")) (Tstr "ho");
  assert_equal_tvalue (jg_substring (Tint (-4)) (Tint 3) (Tstr "hoge")) (Tstr "hog");
  assert_equal_tvalue (jg_substring (Tint (-4)) (Tint 4) (Tstr "hoge")) (Tstr "hoge");
  assert_equal_tvalue (jg_substring (Tint (-4)) (Tint 5) (Tstr "hoge")) (Tstr "hoge");
  assert_equal_tvalue (jg_substring (Tint (-5)) (Tint 1) (Tstr "hoge")) (Tstr "e");

  assert_equal_tvalue (jg_substring (Tint 0) (Tint 1) (Tstr "日本語")) (Tstr "日");
  assert_equal_tvalue (jg_substring (Tint 0) (Tint 2) (Tstr "日本語")) (Tstr "日本");
  assert_equal_tvalue (jg_substring (Tint 0) (Tint 3) (Tstr "日本語")) (Tstr "日本語");
  assert_equal_tvalue (jg_substring (Tint 0) (Tint 4) (Tstr "日本語")) (Tstr "日本語");
  assert_equal_tvalue (jg_substring (Tint 1) (Tint 4) (Tstr "日本語")) (Tstr "本語");
  assert_equal_tvalue (jg_substring (Tint 0) (Tint 10) Tnull) (Tstr "")

let test_truncate _ctx =
  assert_equal_tvalue (jg_truncate (Tint 3) (Tstr "123456789")) (Tstr "123")

[@@@warning "-3"]
let test_md5 _ctx =
  let src = "hoge" in
  let md5 = String.lowercase src |> Digest.string |> Digest.to_hex in
  assert_equal_tvalue (jg_md5 (Tstr src)) (Tstr md5)
[@@@warning "+3"]

let test_reverse _ctx =
  let lst = [Tint 0; Tint 1; Tint 2] in
  let rev = List.rev lst in
  let rev' = jg_reverse (Tlist lst) in
  List.iter2 assert_equal_tvalue rev (unbox_list rev')

let test_last _ctx =
  let lst = Tlist [Tint 0; Tint 1] in
  assert_equal_tvalue (jg_last lst) (Tint 1)

let test_replace _ctx =
  let str = Tstr "hoge" in
  let src = Tstr "ho" in
  let dst = Tstr "hi" in
  let exp = Tstr "hige" in
  assert_equal_tvalue exp (jg_replace src dst str)

let test_replace_uni _ctx =
  let src = Tstr "日本" in
  let dst = Tstr "英" in
  let str = Tstr "日本語" in
  let exp= Tstr "英語" in
  assert_equal_tvalue exp (jg_replace src dst str)

let test_replace_regex _ctx =
  let src = Tstr {|\("[^"]*"\)|} in
  let dst = Tstr ">>>\\1<<<" in
  let str = Tstr {|"FOO"|} in
  let exp = Tstr {|>>>"FOO"<<<|} in
  assert_equal_tvalue exp (jg_replace src dst str)

let test_random _ctx =
  let rec iter ret i =
    if i < 100 then iter ((Tint i) :: ret) (i+1) else ret in
  let lst = iter [] 1 in
  let lst'= unbox_list @@ jg_random (Tlist lst) in
  let is_eq_eq = List.for_all2 (=) lst lst' in
  assert_equal is_eq_eq false

let test_slice _ctx =
  let lst = Tlist [Tint 1; Tint 2; Tint 3; Tint 4; Tint 5] in
  let expect = Tlist [
    Tlist [ Tint 1; Tint 2; Tint 3];
    Tlist [ Tint 4; Tint 5 ]
  ] in
  let result = jg_slice (Tint 2) lst in
  assert_equal_tvalue expect result ;
  assert_equal_tvalue (Tlist [ Tlist [] ; Tlist [] ]) (jg_slice (Tint 2) (Tlist []))

let test_wordcount _ctx =
  assert_equal_tvalue (jg_wordcount (Tstr "hoge hige hage")) (Tint 3);
  assert_equal_tvalue (jg_wordcount (Tstr "hoge")) (Tint 1);
  assert_equal_tvalue (jg_wordcount (Tstr "")) (Tint 0);
  assert_equal_tvalue (jg_wordcount (Tstr "日　本　語")) (Tint 3)

let test_trim _ctx =
  assert_equal_tvalue (jg_trim (Tstr " a \n b c ")) (Tstr "a \n b c");
  assert_equal_tvalue (jg_trim (Tstr "　日　本\n　　語　　　")) (Tstr "日　本\n　　語")

let test_round _ctx =
  assert_equal_tvalue (jg_round (Tstr "floor") (Tfloat 1.5)) (Tfloat 1.0);
  assert_equal_tvalue (jg_round (Tstr "ceil") (Tfloat 1.5)) (Tfloat 2.0)

let test_range _ctx =
  assert_equal_tvalue (Tarray [|Tint 0; Tint 1; Tint 2|]) (jg_range (Tint 0) (Tint 2));
  assert_equal_tvalue (Tarray [|Tint 2; Tint 1; Tint 0|]) (jg_range (Tint 2) (Tint 0));
  assert_equal_tvalue (Tarray [|Tint 2012; Tint 2011; Tint 2010; Tint 2009; Tint 2008; Tint 2007; Tint 2006|]) (jg_range (Tint 2012) (Tint 2006));
  assert_equal_tvalue (Tarray [|Tstr "a"; Tstr "b"; Tstr "c"; Tstr "d"|]) (jg_range (Tstr "a") (Tstr "d"));
  assert_equal_tvalue (Tarray [|Tstr "Z"; Tstr "Y"; Tstr "X"|]) (jg_range (Tstr "Z") (Tstr "X"))

let test_sum _ctx =
  assert_equal_tvalue (jg_sum (Tlist [Tint 0; Tint 1; Tint 2])) (Tint 3);
  assert_equal_tvalue (jg_sum (Tlist [Tint 0; Tint 1; Tfloat 2.1])) (Tfloat 3.1)

let test_int _ctx =
  assert_equal_tvalue (jg_int (Tint 1)) (Tint 1);
  assert_equal_tvalue (jg_int (Tfloat 1.0)) (Tint 1)

let test_float _ctx =
  assert_equal_tvalue (jg_float (Tfloat 1.0)) (Tfloat 1.0);
  assert_equal_tvalue (jg_float (Tint 1)) (Tfloat 1.0)


let test_times _ctx =
  assert_equal_tvalue (jg_times (Tint 0) (Tint 1)) (Tint 0);
  assert_equal_tvalue (jg_times (Tint 1) (Tint 1)) (Tint 1);
  assert_equal_tvalue (jg_times (Tint 2) (Tint 2)) (Tint 4);
  assert_equal_tvalue (jg_times (Tfloat 1.0) (Tint 2)) (Tfloat 2.0);
  assert_equal_tvalue (jg_times (Tfloat 2.0) (Tfloat 2.0)) (Tfloat 4.0);
  assert_equal_tvalue (jg_times (Tfloat 0.0) (Tfloat 2.0)) (Tfloat 0.0);
  assert_equal_tvalue (jg_times (Tfloat 0.0) (Tint 1)) (Tfloat 0.0)

let test_power _ctx =
  assert_equal_tvalue (jg_power (Tint 2) (Tint (-1))) (Tfloat 1.0);
  assert_equal_tvalue (jg_power (Tint 2) (Tint 0)) (Tfloat 1.0);
  assert_equal_tvalue (jg_power (Tint 2) (Tint 1)) (Tfloat 2.0);
  assert_equal_tvalue (jg_power (Tint 2) (Tint 10)) (Tfloat 1024.0)

let test_div _ctx =
  assert_raises (Failure "jg_div:zero division error") (fun () -> jg_div (Tint 4) (Tint 0));
  assert_raises (Failure "jg_div:zero division error") (fun () -> jg_div (Tint 4) (Tfloat 0.0));
  assert_equal_tvalue (jg_div (Tint 4) (Tint 2)) (Tint 2);
  assert_equal_tvalue (jg_div (Tfloat 4.0) (Tint 2)) (Tfloat 2.0)

let test_mod _ctx =
  assert_raises (Failure "jg_mod:zero division error") (fun () -> jg_mod (Tint 4) (Tint 0));
  assert_equal_tvalue (jg_mod (Tint 4) (Tint 3)) (Tint 1);
  assert_equal_tvalue (jg_mod (Tint 4) (Tint 1)) (Tint 0)

let test_and _ctx =
  assert_equal_tvalue (jg_and (Tbool true) (Tbool true)) (Tbool true);
  assert_equal_tvalue (jg_and (Tbool true) (Tbool false)) (Tbool false);
  assert_equal_tvalue (jg_and (Tbool false) (Tbool true)) (Tbool false);
  assert_equal_tvalue (jg_and (Tbool false) (Tbool false)) (Tbool false)

let test_or _ctx =
  assert_equal_tvalue (jg_or (Tbool true) (Tbool true)) (Tbool true);
  assert_equal_tvalue (jg_or (Tbool true) (Tbool false)) (Tbool true);
  assert_equal_tvalue (jg_or (Tbool false) (Tbool true)) (Tbool true);
  assert_equal_tvalue (jg_or (Tbool false) (Tbool false)) (Tbool false)

let test_eq_eq _ctx =
  assert_equal_tvalue (jg_eq_eq (Tint 1) (Tint 1)) (Tbool true);
  assert_equal_tvalue (jg_eq_eq (Tint 1) (Tfloat 1.0)) (Tbool false);
  assert_equal_tvalue (jg_eq_eq (Tfloat 1.0) (Tfloat 1.0)) (Tbool true);
  assert_equal_tvalue (jg_eq_eq (Tstr "hoge") (Tstr "hoge")) (Tbool true);
  assert_equal_tvalue (jg_eq_eq (Tstr "hoge") (Tstr "hige")) (Tbool false);
  assert_equal_tvalue (jg_eq_eq (Tstr "日本語") (Tstr "日本語")) (Tbool true);
  assert_equal_tvalue (jg_eq_eq (Tstr "日本語") (Tstr "英語")) (Tbool false);
  assert_equal_tvalue (jg_eq_eq (Tint 0) (Tstr "hoge")) (Tbool false);
  assert_equal_tvalue (jg_eq_eq Tnull Tnull) (Tbool true)

let test_urlize _ctx =
  assert_equal_tvalue
    (Tstr "go to <a href=\"http://yahoo.co.jp\">http://yahoo.co.jp</a>.")
    (jg_urlize @@ Tstr "go to http://yahoo.co.jp.") ;
  assert_equal_tvalue
    (Tstr "want to go to <a href=\"http://user@foo:8080/bar/?baz=0\">http://user@foo:8080/bar/?baz=0</a>?")
    (jg_urlize @@ Tstr "want to go to http://user@foo:8080/bar/?baz=0?") ;
  assert_equal_tvalue
    (Tstr "(<a href=\"http://foo.foo\">http://foo.foo</a>)")
    (jg_urlize @@ Tstr "(http://foo.foo)") ;
  assert_equal_tvalue
    (Tstr "<a href=\"http://gallica.bnf.fr/ark:/12148/bpt6k1249555/f41.image\">\
           http://gallica.bnf.fr/ark:/12148/bpt6k1249555/f41.image</a><br>Le 20 juin 1794")
    (jg_urlize @@ Tstr "http://gallica.bnf.fr/ark:/12148/bpt6k1249555/f41.image<br>Le 20 juin 1794") ;
  assert_equal_tvalue
    (Tstr "(<a href=\"http://foo.foo/foo(foo)(foo)\">http://foo.foo/foo(foo)(foo)</a>)")
    (jg_urlize @@ Tstr "(http://foo.foo/foo(foo)(foo))") ;
  assert_equal_tvalue
    (Tstr "<img src=\"http://foo.foo/foo.jpg\">")
    (jg_urlize @@ Tstr "<img src=\"http://foo.foo/foo.jpg\">") ;
  assert_equal_tvalue
    (Tstr "<a href='http://foo.foo'>foo</a>")
    (jg_urlize @@ Tstr "<a href='http://foo.foo'>foo</a>")

let test_title _ctx =
  assert_equal_tvalue (jg_title @@ Tstr "this is it!") (Tstr "This Is It!");
  assert_equal_tvalue (jg_title @@ Tstr "my First cAR") (Tstr "My First Car")

let test_striptags _ctx =
  assert_equal_tvalue
    (Tstr "hogehoge higehige hagehage")
    (jg_striptags @@ Tstr "<p class='indent'>hogehoge</p> higehige <b>hagehage</b>")

let test_sort_int_list _ctx =
  assert_equal_tvalue
    (Tlist [Tint 1; Tint 2; Tint 3])
    (jg_sort @@ Tlist [Tint 3; Tint 1; Tint 2])

let test_sort_int_array _ctx =
  assert_equal_tvalue
    (Tarray [| Tint 1; Tint 2; Tint 3 |])
    (jg_sort @@ Tarray [| Tint 3; Tint 1; Tint 2 |])

let test_sort_float_list _ctx =
  assert_equal_tvalue
    (Tlist [Tfloat 1.1; Tfloat 2.2; Tfloat 3.0])
    (jg_sort @@ Tlist [Tfloat 3.0; Tfloat 1.1; Tfloat 2.2])

let test_sort_float_array _ctx =
  assert_equal_tvalue
    (Tarray [| Tfloat 1.1; Tfloat 2.2; Tfloat 3.0 |])
    (jg_sort @@ Tarray [| Tfloat 3.0; Tfloat 1.1; Tfloat 2.2 |])

let test_sort_string_list _ctx =
  assert_equal_tvalue
    (Tlist [Tstr "aa"; Tstr "baba"; Tstr "caca"])
    (jg_sort @@ Tlist [Tstr "baba"; Tstr "aa"; Tstr "caca"] )

let test_sort_rev _ctx =
  let init = Tlist [Tint 3; Tint 1; Tint 2] in
  let exp = [ Tint 1 ; Tint 2 ; Tint 3 ] in
  assert_equal_tvalue
    (Tlist (List.rev exp))
    (jg_sort init ~kwargs:[("reverse", Tbool true)]) ;
  assert_equal_tvalue
    (Tlist exp)
    (jg_sort init ~kwargs:[("reverse", Tbool false)])

let test_sort_attr _ctx =
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
  let forward_sorted = jg_sort persons ~kwargs:[("attribute", Tstr "info.age")] |> unbox_list in
  let forward_expected = [name_is "bob"; name_is "ken"] in
  let reverse_sorted = jg_sort persons ~kwargs:[("attribute", Tstr "info.age"); ("reverse", Tbool true)] |> unbox_list in
  let reverse_expected = [name_is "ken"; name_is "bob"] in
  let check_person checker person = checker person in
  assert_equal (List.for_all2 check_person forward_expected forward_sorted) true;
  assert_equal (List.for_all2 check_person reverse_expected reverse_sorted) true

let test_sort_compare _ctx =
  let data = Tlist [
    Tset [Tstr "A"; Tint 0];
    Tset [Tstr "Z"; Tint 2];
    Tset [Tstr "b"; Tint 1]
  ] in
  let lower_fst_cmp = function
    | [a; b] -> jg_compare (jg_lower (jg_nth_aux a 0)) (jg_lower (jg_nth_aux b 0))
    | _ -> failwith "invalid args" in
  let jg_lower_fst_cmp = func_no_kw lower_fst_cmp 2 in
  assert_equal_tvalue (jg_sort data) data;
  assert_equal_tvalue
    (Tlist [Tset [Tstr "A"; Tint 0]; Tset [Tstr "b"; Tint 1]; Tset [Tstr "Z"; Tint 2]])
    (jg_sort ~kwargs:[("compare", jg_lower_fst_cmp)] data)

let test_sort_string_array _ctx =
  assert_equal_tvalue
    (Tarray [| Tstr "aa"; Tstr "baba"; Tstr "caca" |])
    (jg_sort @@ Tarray [| Tstr "baba"; Tstr "aa"; Tstr "caca" |])

let test_list _ctx =
  assert_equal_tvalue
    (Tlist [Tstr "h"; Tstr "o"; Tstr "g"; Tstr "e"])
    (jg_list @@ Tstr "hoge")

let test_xmlattr _ctx =
  let obj = Tobj [
    ("class", Tstr "profile");
    ("id", Tstr "taro");
    ("width", Tint 300);
  ] in
  assert_equal_tvalue
    (Tstr "class=\"profile\" id=\"taro\" width=\"300\"")
    (jg_xmlattr obj)

let test_wordwrap _ctx =
  let text = String.concat " " [
    "this is it!!";
    "hoge hogehogehoge";
  ] in
  assert_equal_tvalue
    (Tstr "this is it!!\nhoge hogehog\nehoge")
    (jg_wordwrap (Tint 12) (Tbool true) (Tstr text));
  assert_equal_tvalue
    (Tstr "this is it!!\nhoge hogehogehoge")
    (jg_wordwrap (Tint 12) (Tbool false) (Tstr text))

let test_sublist _ctx =
  let lst = Tlist [Tint 0; Tint 1; Tint 2; Tint 3] in
  assert_equal_tvalue
    (Tlist [Tint 0; Tint 1; Tint 2; Tint 3])
    (jg_sublist (Tint 0) (Tint 4) lst);
  assert_equal_tvalue
    (Tlist [Tint 0; Tint 1; Tint 2])
    (jg_sublist (Tint 0) (Tint 3) lst);
  assert_equal_tvalue
    (Tlist [Tint 0; Tint 1])
    (jg_sublist (Tint 0) (Tint 2) lst);
  assert_equal_tvalue
    (Tlist [Tint 0])
    (jg_sublist (Tint 0) (Tint 1) lst);
  assert_equal_tvalue
    (Tlist [])
    (jg_sublist (Tint 0) (Tint 0) lst);
  assert_equal_tvalue
    (Tlist [])
    (jg_sublist (Tint 1) (Tint 0) lst);
  assert_equal_tvalue
    (Tlist [Tint 1])
    (jg_sublist (Tint 1) (Tint 1) lst);
  assert_equal_tvalue
    (Tlist [Tint 1; Tint 2])
    (jg_sublist (Tint 1) (Tint 2) lst);
  assert_equal_tvalue
    (Tlist [Tint 1; Tint 2; Tint 3])
    (jg_sublist (Tint 1) (Tint 3) lst);
  assert_equal_tvalue
    (Tlist [Tint 1; Tint 2; Tint 3])
    (jg_sublist (Tint 1) (Tint 4) lst)

let test_fmt_float _ctx =
  let value = Tfloat 3.141592 in
  assert_equal_tvalue (jg_fmt_float (Tint 1) value) (Tfloat 3.1);
  assert_equal_tvalue (jg_fmt_float (Tint 2) value) (Tfloat 3.14);
  assert_equal_tvalue (jg_fmt_float (Tint 3) value) (Tfloat 3.142);
  assert_equal_tvalue (jg_fmt_float (Tint 4) value) (Tfloat 3.1416)

let test_divisibleby _ctx =
  assert_equal_tvalue (jg_test_divisibleby (Tint 2) (Tint 6)) (Tbool true);
  assert_equal_tvalue (jg_test_divisibleby (Tint 5) (Tint 6)) (Tbool false);
  assert_equal_tvalue (jg_test_divisibleby (Tint 0) (Tint 6)) (Tbool false)

let test_even _ctx =
  assert_equal_tvalue (jg_test_even (Tint 0)) (Tbool true);
  assert_equal_tvalue (jg_test_even (Tint 1)) (Tbool false);
  assert_equal_tvalue (jg_test_even (Tint 2)) (Tbool true);
  assert_equal_tvalue (jg_test_even (Tint 3)) (Tbool false)

let test_odd _ctx =
  assert_equal_tvalue (jg_test_odd (Tint 0)) (Tbool false);
  assert_equal_tvalue (jg_test_odd (Tint 1)) (Tbool true);
  assert_equal_tvalue (jg_test_odd (Tint 2)) (Tbool false);
  assert_equal_tvalue (jg_test_odd (Tint 3)) (Tbool true)

let test_iterable _ctx =
  assert_equal_tvalue (jg_test_iterable (Tint 0)) (Tbool false);
  assert_equal_tvalue (jg_test_iterable (Tfloat 1.0)) (Tbool false);
  assert_equal_tvalue (jg_test_iterable (Tstr "hoge")) (Tbool true);
  assert_equal_tvalue (jg_test_iterable (Tobj [])) (Tbool true);
  assert_equal_tvalue (jg_test_iterable (Tlist [])) (Tbool true);
  assert_equal_tvalue (jg_test_iterable (Tset [])) (Tbool true);
  assert_equal_tvalue (jg_test_iterable Tnull) (Tbool true)

let test_is_lower _ctx =
  assert_equal_tvalue (jg_test_lower (Tstr "aaa")) (Tbool true);
  assert_equal_tvalue (jg_test_lower (Tstr "aaA")) (Tbool false)

let test_is_upper _ctx =
  assert_equal_tvalue (jg_test_upper (Tstr "aaa")) (Tbool false);
  assert_equal_tvalue (jg_test_upper (Tstr "AAA")) (Tbool true)

let test_number _ctx =
  assert_equal_tvalue (jg_test_number (Tint 1)) (Tbool true);
  assert_equal_tvalue (jg_test_number (Tfloat 1.0)) (Tbool true);
  assert_equal_tvalue (jg_test_number (Tstr "1")) (Tbool false)

let test_string _ctx =
  assert_equal_tvalue (jg_test_string (Tstr "aaa")) (Tbool true);
  assert_equal_tvalue (jg_test_string (Tint 1)) (Tbool false)

let test_groupby _ctx =
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
  let groups_by_gender = unbox_list @@ jg_groupby Tnull persons ~kwargs:[("attribute", Tstr "gender")] in
  let groups_by_lang_native = unbox_list @@ jg_groupby Tnull persons ~kwargs:[("attribute", Tstr "lang.native")] in
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

let test_min_max _ctx =
  let numbers = Tlist [Tint 3; Tint 1; Tint 2] in
  let min_number = jg_min numbers in
  let max_number = jg_max numbers in
  let min_person = jg_min test_persons ~kwargs:[("attribute", Tstr "age")] in
  let max_person = jg_max test_persons ~kwargs:[("attribute", Tstr "age")] in
  assert_equal_tvalue min_number (Tint 1);
  assert_equal_tvalue max_number (Tint 3);
  assert_equal_tvalue (jg_obj_lookup min_person "name") (Tstr "jiro");
  assert_equal_tvalue (jg_obj_lookup max_person "name") (Tstr "hana")

let test_nth _ctx =
  let list = Tlist [Tint 3; Tint 0; Tint 2] in
  let ary = Tarray [| Tint 1; Tint 10; Tint 12 |] in
  let str1 = Tstr "日本語" in
  let str2 = Tstr "hoge" in
  assert_equal_tvalue (Tint 3) (jg_nth (Tint 0) list);
  assert_equal_tvalue (Tint 0) (jg_nth (Tint 1) list);
  assert_equal_tvalue (Tint 2) (jg_nth (Tint 2) list);
  assert_equal_tvalue (Tint 1) (jg_nth (Tint 0) ary);
  assert_equal_tvalue (Tint 10) (jg_nth (Tint 1) ary);
  assert_equal_tvalue (Tint 12) (jg_nth (Tint 2) ary) ;
  assert_equal_tvalue (Tstr "日") (jg_nth (Tint 0) str1);
  assert_equal_tvalue (Tstr "本") (jg_nth (Tint 1) str1);
  assert_equal_tvalue (Tstr "語") (jg_nth (Tint 2) str1);
  assert_equal_tvalue (Tstr "h") (jg_nth (Tint 0) str2);
  assert_equal_tvalue (Tstr "e") (jg_nth (Tint 3) str2)

let test_map _ctx =
  let names = unbox_list @@ jg_map Tnull test_persons ~kwargs:[("attribute", Tstr "name")] in
  let names_expected = [Tstr "taro"; Tstr "jiro"; Tstr "hana"] in
  let ranks = unbox_list @@ jg_map Tnull test_persons ~kwargs:[("attribute", Tstr "extra.rank")] in
  let ranks_expected = [Tint 3; Tint 12; Tint 5] in
  assert_equal (List.for_all2 (=) names names_expected) true;
  assert_equal (List.for_all2 (=) ranks ranks_expected) true

let alice = Tobj [ "name", Tstr "alice" ; "age", Tint 36 ]
let bob = Tobj [ "name", Tstr "bob" ; "age", Tint 42 ]
let carol = Tobj [ "name", Tstr "carol" ; "age", Tint 20]

let test_select_aux jg_select expected =
  let persons = Tlist [ alice ; bob ; carol ] in
  let select = func_arg1_no_kw @@ fun x ->
    Tbool (unbox_int (List.assoc "age" (unbox_obj x)) > 30)
  in
  assert_equal_tvalue expected (jg_select select persons)

let test_select _ctx =
  test_select_aux jg_select (Tlist [ alice ; bob ])

let test_reject _ctx =
  test_select_aux jg_reject (Tlist [ carol ])

let test_fold _ctx =
  let test seq =
    assert_equal_tvalue
      (Tint 45)
      (jg_fold (func_arg2_no_kw jg_add) (Tint 0) seq)
  in
  test (Tlist [Tint 0;Tint 1;Tint 2;Tint 3;Tint 4;Tint 5;Tint 6;Tint 7;Tint 8;Tint 9]) ;
  test (Tarray [|Tint 0;Tint 1;Tint 2;Tint 3;Tint 4;Tint 5;Tint 6;Tint 7;Tint 8;Tint 9|])

let test_fold_str _ctx =
  let str = "abc" in
  assert_equal_tvalue
    (Tstr str)
    (jg_fold (func_arg2_no_kw jg_plus) (Tstr "") (Tstr str))

let test_fold_mbstr _ctx =
  let mbstr = "日本語" in
  assert_equal_tvalue
    (Tstr mbstr)
    (jg_fold (func_arg2_no_kw jg_plus) (Tstr "") (Tstr mbstr))

let test_forall _ctx =
  let test res seq =
    assert_equal_tvalue
      (Tbool res)
      (jg_forall (func_arg1_no_kw @@ fun x -> Tbool (unbox_int x < 10)) seq)
  in
  test true (Tlist [Tint 0;Tint 1;Tint 2;Tint 3;Tint 4;Tint 5;Tint 6;Tint 7;Tint 8;Tint 9]) ;
  test false (Tarray [|Tint 0;Tint 10;Tint 2|])

let test_exists _ctx =
  let test res seq =
    assert_equal_tvalue
      (Tbool res)
      (jg_exists (func_arg1_no_kw @@ fun x -> Tbool (unbox_int x < 10)) seq)
  in
  test true (Tlist [Tint 0;Tint 1;Tint 2;Tint 3;Tint 4;Tint 5;Tint 6;Tint 7;Tint 8;Tint 9]) ;
  test true (Tarray [|Tint 0;Tint 10;Tint 2|]);
  test false (Tarray [|Tint 100;Tint 10;Tint 12|])

let test_type_volatile _ctx =
  let x =
    Tvolatile (let i = ref 0 in fun () -> incr i ; Tint !i)
  in
  let i1 = (jg_force x) in
  let i2 = (jg_force x) in
  let i3 = (jg_force x) in
  assert_equal_tvalue
    (Tlist [ Tint 1 ; Tint 2 ; Tint 3])
    (Tlist [ i1 ; i2 ; i3 ])

let test_type_lazy _ctx =
  let x = Tlazy (let i = ref 0 in lazy (incr i ; Tint !i)) in
  let i1 = (jg_force x) in
  let i2 = (jg_force x) in
  let i3 = (jg_force x) in
  assert_equal_tvalue
    (Tlist [ Tint 1 ; Tint 1 ; Tint 1])
    (Tlist [ i1 ; i2 ; i3 ])

(*
  https://github.com/tategakibunko/jingoo/pull/69
  https://github.com/tategakibunko/jingoo/pull/72
*)
let test_func_arg1 _ctx =
  let to_mail ?(kwargs=[]) ?(defaults=[]) value =
    let id = string_of_tvalue value in
    let domain = string_of_tvalue (jg_get_kvalue "domain" kwargs ~defaults) in
    Tstr (id ^ "@" ^ domain) in
  let jg_to_mail = func_arg1 ~name:"to_mail" (to_mail ~defaults:[("domain", Tstr "gmail.com")]) in
  let jg_to_mail_bad = func_arg1_no_kw ~name:"to_mail_bad" (to_mail ~defaults:[("domain", Tstr "gmail.com")]) in

  assert_equal_tvalue
    (jg_apply
       ~kwargs:[("domain", Tstr "hotmail.com")]
       jg_to_mail [Tstr "foo"])
    (Tstr "foo@hotmail.com");

  assert_equal_tvalue
    (jg_apply
       ~kwargs:[]
       jg_to_mail_bad [Tstr "foo"])
    (Tstr "foo@gmail.com");

  assert_raises (Failure "type error: to_mail_bad(domain=string,string)")
    (fun _ -> ignore (jg_apply
                        ~kwargs:[("domain", Tstr "hotmail.com")]
                        jg_to_mail_bad [Tstr "foo"]))

let test_printf _ctx =
  assert_equal_tvalue
    (Tstr "8 apples, 4 people, that's 0.5 apple per person.")
    (jg_apply (Tfun (fun ?kwargs:_ -> jg_printf))
       [ Tstr "%d %s, %d people, that's %.1f apple per person."
       ; Tint 8 ; Tstr "apples" ; Tint 4 ; Tfloat 0.5 ])

let test_compose _ctx =
  let fn =
    jg_compose
      (Tfun (fun ?kwargs:_ x -> Tstr (string_of_tvalue x)))
      (jg_apply (func_arg2_no_kw jg_attr) [ Tstr "foo" ] )
  in
  assert_equal_tvalue
    (Tstr "42")
    (jg_apply fn [ Tobj [("bar", Tnull) ; ("foo", Tint 42)] ]) ;
  assert_equal_tvalue
    (Tstr "")
    (jg_apply fn [ Tobj [] ])

let test_find _ctx =
  let l = Tarray [| Tint 0 ; Tint 2 ; Tint 4 ; Tint 5 |] in
  assert_equal_tvalue (Tint 5)
    (jg_find (func_arg1_no_kw (fun x -> box_bool ((unbox_int x) mod 2 = 1))) l) ;
  assert_equal_tvalue (Tint 0)
    (jg_find (func_arg1_no_kw (fun _ -> Tbool true)) l)

let test_unique _ctx =
  let lst1 = Tlist [Tint 1; Tint 2; Tint 3; Tint 2; Tint 1] in
  let lst2 = Tlist [Tset [Tstr "taro"; Tint 20]; Tset [Tstr "jiro"; Tint 10]; Tset [Tstr "taro"; Tint 20]] in
  let lst3 = Tlist [Tstr "foo"; Tstr "bar"; Tstr "日本語"; Tstr "漢字"; Tstr "hoge"] in
  let is_same_str_len = func_arg2_no_kw @@ fun s1 s2 ->
    let len1 = unbox_int (jg_length s1) in
    let len2 = unbox_int (jg_length s2) in
    Tbool (len1 = len2) in
  assert_equal_tvalue (jg_unique lst1) (Tlist [Tint 1; Tint 2; Tint 3]);
  assert_equal_tvalue (jg_unique lst2) (Tlist [Tset [Tstr "taro"; Tint 20]; Tset [Tstr "jiro"; Tint 10]]);
  assert_equal_tvalue (jg_unique lst3 ~kwargs:[("eq", is_same_str_len)]) (Tlist [Tstr "foo"; Tstr "漢字"; Tstr "hoge"])

let test_flatten _ctx =
  let test input =
    assert_equal_tvalue
      (Tlist [Tint 1; Tint 2; Tint 3])
      (jg_flatten input)
  in
  test (Tlist [Tint 1; Tint 2; Tint 3]);
  test (Tlist [Tlist [Tint 1]; Tarray [|Tint 2|]; Tint 3])

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
  "test_batch_list" >:: test_batch_list;
  "test_batch_list2" >:: test_batch_list2;
  "test_batch_array" >:: test_batch_array;
  "test_batch_array2" >:: test_batch_array2;
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
  "test_replace_regex" >:: test_replace_regex;
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
  "test_sort_compare" >:: test_sort_compare;
  "test_xmlattr" >:: test_xmlattr;
  "test_wordwrap" >:: test_wordwrap;
  "test_sublist" >:: test_sublist;
  "test_fmt_float" >:: test_fmt_float;
  "test_divisibleby" >:: test_divisibleby;
  "test_even" >:: test_even;
  "test_odd" >:: test_odd;
  "test_iterable" >:: test_iterable;
  "test_is_lower" >:: test_is_lower;
  "test_is_upper" >:: test_is_upper;
  "test_number" >:: test_number;
  "test_string" >:: test_string;
  "test_groupby" >:: test_groupby;
  "test_min_max" >:: test_min_max;
  "test_nth" >:: test_nth;
  "test_map" >:: test_map;
  "test_select" >:: test_select;
  "test_reject" >:: test_reject;
  "test_fold" >:: test_fold;
  "test_fold_str" >:: test_fold_str;
  "test_fold_mbstr" >:: test_fold_mbstr;
  "test_forall" >:: test_forall;
  "test_exists" >:: test_exists;
  "test_type_volatile" >:: test_type_volatile;
  "test_type_lazy" >:: test_type_lazy;
  "test_func_arg1" >:: test_func_arg1;
  "test_printf" >:: test_printf;
  "test_compose" >:: test_compose;
  "test_unique" >:: test_unique;
  "test_flatten" >:: test_flatten
]
