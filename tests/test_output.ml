(*
  test_output.ml

  Copyright (c) 2012 - by Masaki WATANABE <lambda.watanabe@gmail.com>

  Licence: GPL
*)
open OUnit2
open Jg_utils
open Jg_types
open Jg_runtime

let assert_interp ~test_ctxt ?(models=[]) source exp_output =
  let output = Jg_interp.from_string source ~models in
  logf test_ctxt `Info "Source: %S" source;
  logf test_ctxt `Info "Output: %S" output;
  assert_equal output exp_output
;;

let test_expand_escape test_ctxt =
  let script = "<script>alert(1)</script>" in
  assert_interp ~test_ctxt
    "{{danger}}" ~models:["danger", Tstr script]
    (Jg_utils.escape_html script)
;;

let test_expand_safe test_ctxt =
  let script = "<script>alert(1)</script>" in
  assert_interp ~test_ctxt
    "{{danger|safe}}" ~models:["danger", Tstr script]
    script
;;

let test_expand_filter test_ctxt =
  assert_interp ~test_ctxt
    "{{pi|int}}" ~models:["pi", Tfloat 3.14]
    "3"
;;

let test_if test_ctxt =
  let source =
    "{% if x == 1 %}one{% elseif x == 2 %}two{% else %}three{% endif %}"
  in
  assert_interp ~test_ctxt source ~models:["x", Tint 1] "one";
  assert_interp ~test_ctxt source ~models:["x", Tint 2] "two";
  assert_interp ~test_ctxt source ~models:["x", Tint 4] "three";
;;
  
let test_for test_ctxt =
  assert_interp ~test_ctxt
    "{% for i in range(1,3) %}{{i}}{% endfor %}"
    "123";
  assert_interp ~test_ctxt
    "{% for (i,j) in [(1,'one'), (2,'two'), (3,'three')] %}({{i}},{{j}})\
     {% endfor %}"
    "(1,one)(2,two)(3,three)";
  assert_interp ~test_ctxt
    "{% for i,j in [(1,'one'), (2,'two'), (3,'three')] %}({{i}},{{j}})\
     {% endfor %}"
    "(1,one)(2,two)(3,three)"
;;

let test_loop_index test_ctxt =
  assert_interp ~test_ctxt
    "{% for i in range(1,3) %}{{loop.index}}{% endfor %}"
    "123"
;;

let test_loop_index0 test_ctxt =
  assert_interp ~test_ctxt
    "{% for i in range(1,3) %}{{loop.index0}}{% endfor %}"
    "012"
;;

let test_loop_revindex test_ctxt =
  assert_interp ~test_ctxt
    "{% for i in range(1,3) %}{{loop.revindex}}{% endfor %}"
    "321"
;;

let test_loop_revindex0 test_ctxt =
  assert_interp ~test_ctxt
    "{% for i in range(1,3) %}{{loop.revindex0}}{% endfor %}"
    "210"
;;

let test_loop_cycle test_ctxt =
  assert_interp ~test_ctxt
    "{% for i in range(1,3) %}{{loop.cycle(\"hoge\",\"hige\",\"hage\")}}\
    {% endfor %}"
    "hogehigehage"
;;

let test_loop_first test_ctxt =
  assert_interp ~test_ctxt
    "{% for i in range(1,3) %}{{loop.first}}{% endfor %}"
    "truefalsefalse"
;;

let test_loop_last test_ctxt =
  assert_interp ~test_ctxt
    "{% for i in range(1,3) %}{{loop.last}}{% endfor %}"
    "falsefalsetrue"
;;

let test_loop_length test_ctxt =
  assert_interp ~test_ctxt
    "{% for i in range(1,3) %}{{loop.length}}{% endfor %}"
    "333"
;;

let test_in test_ctxt =
  assert_interp ~test_ctxt "{{ 'hoge' in ['hoge', 'hige', 'hage'] }}" "true"
;;

let test_is test_ctxt =
  assert_interp ~test_ctxt "{{ 6 is divisibleby 3 }}" "true";
  assert_interp ~test_ctxt "{{ 6 is divisibleby(3) }}" "true"
;;

let test_with test_ctxt =
  assert_interp ~test_ctxt
    "{% set hoge = 'hoge' %}\
     {% with hoge = 'tmp', hige = 'tmp2' %}\
     {{ hoge }}{{ hige }}\
     {% endwith %}\
     {{ hoge }}{{ hige }}"
    "tmptmp2hoge"
;;

let test_defined test_ctxt =
  assert_interp ~test_ctxt
    "{% set obj = {age:10, name:'taro'} %}\
     {{ obj.age is defined }}"
    "true";

  assert_interp ~test_ctxt
    "{% set obj = {age:10, name:'taro'} %}\
     {{ obj['name'] is defined }}"
    "true"
;;

let from_file test_ctxt file_name =
  let env = {std_env with template_dirs = [in_testdata_dir test_ctxt []]} in
  Jg_interp.from_file ~env file_name
;;

let test_extends test_ctxt =
  let output = from_file test_ctxt "extends.tmpl" in
  assert_equal (Jg_utils.chomp output) "extended"
;;

let test_include test_ctxt =
  let output = from_file test_ctxt "included.tmpl" in
  assert_equal (Jg_utils.chomp output) "this is included"
;;

let macro_three_words =
  "{% macro three_words(one,two,three) %}\
   {{one}} {{two}} {{three}}{{caller (' by','michael','jackson')}}\
   {% endmacro %}"
;;

let test_macro test_ctxt =
  let output = Jg_interp.from_string
    (macro_three_words^"{{ three_words(\"this\", \"is\", \"it!\") }}")
  in
  assert_equal (Jg_utils.chomp output) "this is it!"
;;

let test_caller test_ctxt =
  let output = Jg_interp.from_string
    (macro_three_words^
    "{% call(a,b,c) three_words('this', 'is', 'it!') %}\
     {{a}} {{b}} {{c}}\
     {% endcall %}")
  in
  assert_equal (Jg_utils.chomp output) "this is it! by michael jackson"
;;

let test_filter test_ctxt =
  assert_interp ~test_ctxt
    "{% filter upper %}must be upper{% endfilter %}"
    "MUST BE UPPER"
;;

let test_set test_ctxt =
  assert_interp ~test_ctxt "{% set x = \"hoge\" %}{{x}}" "hoge";
  assert_interp ~test_ctxt "{% set x = 'hoge' %}{{x}}" "hoge"
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
