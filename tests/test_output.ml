(*
  test_output.ml

  Copyright (c) 2012 - by Masaki WATANABE <lambda.watanabe@gmail.com>

  Licence: GPL
*)
open OUnit
open Jg_utils
open Jg_types
open Jg_runtime

let test_expand_escape () =
  let script = "<script>alert(1)</script>" in
  let output = Jg_interp.from_string "{{danger}}" ~models:[
    ("danger", Tstr "<script>alert(1)</script>");
  ] in
  assert_equal output @@ Jg_utils.escape_html script
;;

let test_expand_safe () =
  let script = "<script>alert(1)</script>" in
  let output = Jg_interp.from_string "{{danger|safe}}" ~models:[
    ("danger", Tstr script);
  ] in
  assert_equal output script
;;

let test_expand_filter () =
  let output = Jg_interp.from_string "{{pi|int}}" ~models:[
    ("pi", Tfloat 3.14);
  ] in
  assert_equal output "3"
;;

let test_if () =
  let source = "{% if x == 1 %}one{% elseif x == 2 %}two{% else %}three{% endif %}" in
  let output = Jg_interp.from_string source ~models:[
    ("x", Tint 1);
  ] in
  assert_equal output "one";
  let output = Jg_interp.from_string source ~models:[
    ("x", Tint 2);
  ] in
  assert_equal output "two";
  let output = Jg_interp.from_string source ~models:[
    ("x", Tint 3);
  ] in
  assert_equal output "three"
;;
  
let test_for () =
  let output = Jg_interp.from_string "{% for i in range(1,3) %}{{i}}{% endfor %}" in
  assert_equal output "123";
  let output = Jg_interp.from_string "{% for (i,j) in [(1,'one'), (2,'two'), (3,'three')] %}({{i}},{{j}}){% endfor %}" in
  assert_equal output "(1,one)(2,two)(3,three)";
  let output = Jg_interp.from_string "{% for i,j in [(1,'one'), (2,'two'), (3,'three')] %}({{i}},{{j}}){% endfor %}" in
  assert_equal output "(1,one)(2,two)(3,three)"
;;

let test_loop_index () =
  let output = Jg_interp.from_string "{% for i in range(1,3) %}{{loop.index}}{% endfor %}" in
  assert_equal output "123"
;;

let test_loop_index0 () =
  let output = Jg_interp.from_string "{% for i in range(1,3) %}{{loop.index0}}{% endfor %}" in
  assert_equal output "012"
;;

let test_loop_revindex () =
  let output = Jg_interp.from_string "{% for i in range(1,3) %}{{loop.revindex}}{% endfor %}" in
  assert_equal output "321"
;;

let test_loop_revindex0 () =
  let output = Jg_interp.from_string "{% for i in range(1,3) %}{{loop.revindex0}}{% endfor %}" in
  assert_equal output "210"
;;

let test_loop_cycle () = 
  let output = Jg_interp.from_string "{% for i in range(1,3) %}{{loop.cycle(\"hoge\",\"hige\",\"hage\")}}{% endfor %}" in
  assert_equal output "hogehigehage"
;;

let test_loop_first () = 
  let output = Jg_interp.from_string "{% for i in range(1,3) %}{{loop.first}}{% endfor %}" in
  assert_equal output "truefalsefalse"
;;

let test_loop_last () = 
  let output = Jg_interp.from_string "{% for i in range(1,3) %}{{loop.last}}{% endfor %}" in
  assert_equal output "falsefalsetrue"
;;

let test_loop_length () =
  let output = Jg_interp.from_string "{% for i in range(1,3) %}{{loop.length}}{% endfor %}" in
  assert_equal output "333"
;;

let test_in () =
  let output = Jg_interp.from_string "{{ 'hoge' in ['hoge', 'hige', 'hage'] }}" in
  assert_equal output "true"
;;

let test_is () =
  let output = Jg_interp.from_string "{{ 6 is divisibleby 3 }}" in
  assert_equal output "true";
  let output = Jg_interp.from_string "{{ 6 is divisibleby(3) }}" in
  assert_equal output "true"
;;

let test_with () =
  let output = Jg_interp.from_string @@ String.concat "" [
    "{% set hoge = 'hoge' %}";
    "{% with hoge = 'tmp', hige = 'tmp2' %}";
    "{{ hoge }}{{ hige }}";
    "{% endwith %}";
    "{{ hoge }}{{ hige }}";
  ] in
  assert_equal output "tmptmp2hoge"
;;

let test_defined () =
  let output = Jg_interp.from_string @@ String.concat "" [
    "{% set obj = {age:10, name:'taro'} %}";
    "{{ obj.age is defined }}";
  ] in
  assert_equal output "true";

  let output = Jg_interp.from_string @@ String.concat "" [
    "{% set obj = {age:10, name:'taro'} %}";
    "{{ obj['name'] is defined }}";
  ] in
  assert_equal output "true";
;;

let from_file file_name =
  let env = {std_env with template_dirs = ["tmpl"]} in
  Jg_interp.from_file ~env file_name
;;

let test_extends () =
  let output = from_file "extends.tmpl" in
  assert_equal (Jg_utils.chomp output) "extended"
;;

let test_include () =
  let output = from_file "included.tmpl" in
  assert_equal (Jg_utils.chomp output) "this is included"
;;

let macro_three_words = String.concat "" [
  "{% macro three_words(one,two,three) %}";
  "{{one}} {{two}} {{three}}{{caller (' by','michael','jackson')}}";
  "{% endmacro %}";
]
;;

let test_macro () =
  let output = Jg_interp.from_string @@ String.concat "" [
    macro_three_words;
    "{{ three_words(\"this\", \"is\", \"it!\") }}";
  ] in
  assert_equal (Jg_utils.chomp output) "this is it!"
;;

let test_caller () =
  let output = Jg_interp.from_string @@ String.concat "" [
    macro_three_words;
    "{% call(a,b,c) three_words('this', 'is', 'it!') %}";
    "{{a}} {{b}} {{c}}";
    "{% endcall %}";
  ] in
  assert_equal (Jg_utils.chomp output) "this is it! by michael jackson"
;;

let test_filter () =
  let output = Jg_interp.from_string "{% filter upper %}must be upper{% endfilter %}" in
  assert_equal output "MUST BE UPPER"
;;

let test_set () =
  let output = Jg_interp.from_string "{% set x = \"hoge\" %}{{x}}" in
  assert_equal output "hoge";
  let output = Jg_interp.from_string "{% set x = 'hoge' %}{{x}}" in
  assert_equal output "hoge"
;;

let suite = "runtime test" >::: [
  "test_expand_escape" >:: test_expand_escape;
  "test_expand_safe" >:: test_expand_safe;
  "test_expand_filter" >:: test_expand_filter;
  "test_if" >:: test_if;
  "test_for" >:: test_for;
  "test_loop_index" >:: test_loop_index;
  "test_loop_index0" >:: test_loop_index0;
  "test_loop_revindex" >:: test_loop_revindex;
  "test_loop_revindex0" >:: test_loop_revindex0;
  "test_loop_cycle" >:: test_loop_cycle;
  "test_loop_first" >:: test_loop_first;
  "test_loop_last" >:: test_loop_last;
  "test_loop_length" >:: test_loop_length;
  "test_extends" >:: test_extends;
  "test_include" >:: test_include;
  "test_macro" >:: test_macro;
  "test_caller" >:: test_caller;
  "test_filter" >:: test_filter;
  "test_set" >:: test_set;
  "test_in" >:: test_in;
  "test_defined" >:: test_defined;
  "test_is" >:: test_is;
  "test_with" >:: test_with;
]
;;
