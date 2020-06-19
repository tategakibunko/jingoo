# jingoo

## About jingoo

Jingoo is OCaml template engine almost compatible with [Jinja2](https://github.com/pallets/jinja/)(python template engine).

## Install

### manual

```bash
make
sudo make install
```
### opam

```bash
opam install jingoo
```

## Difference between Jinja2 and Jingoo

1. i18n features are not supported yet.
2. Cause of language difference between ocaml and python,
   some of built-in filters are different from original one,
   especially orders of arguments and supported optional arguments etc.
3. Single line comment is not supported. Because single '#' is used very much especially in html.

## Usage

### Simple usage

```ocaml
open Jingoo

(* output from direct string template *)
let result = Jg_template.from_string "{{ msg }}" ~models:[("msg", Jg_types.Tstr "hello, world!")]

(* or output from file template *)
let result2 = Jg_template.from_file "hello.jingoo" ~models:[("msg", Jg_types.Tstr "hello, world!")]

(* or you can use functional model like this by using Jg_template2 module(ver >= 1.4.0) *)
let result3 = Jg_template2.from_string "{{ msg }}(random value = {{ randam_int }})" ~models:(function
 | "msg" -> Jg_types.Tstr "hello, world!"
 | "randam_int" -> Jg_types.Tint (Random.int 100)
 | _ -> Jg_types.Tnull
)

(* or you can use closure for models by using Jg_template2 module(ver >= 1.4.0) *)
let hash = Hashtbl.create 10 in
let () = Hashtbl.add hash "msg" (Jg_types.Tstr "hello, world!") in
let result4 = Jg_template2.from_string "{{ msg }}" ~models:(fun key ->
  try Hashtbl.find hash key with Not_found -> Tnull
)
```

### More detailed example

### template code

```jinja2
{# sample.jingoo #}
<h1>{{ title }}</h1>

<ul>
{% for user in users %}
  <li><a href="{{ user.url }}">{{ user.name }}(age = {{ user.age }})</a></li>
{% endfor %}
</ul>

{% if is_debug %}
<p>this is debug mode!</p>
{% endif %}
```

### ocaml code

```ocaml
open Jingoo

let result = Jg_template.from_file "sample.jingoo" ~models:[
  ("title", Jg_types.Tstr "more detailed example");
  ("is_debug", Jg_types.Tbool true);
  ("users", Jg_types.Tlist [
    Jg_types.Tobj [
      ("name", Jg_types.Tstr "taro");
      ("url", Jg_types.Tstr "https://example.com");
      ("age", Jg_types.Tint 20);
    ];
    Jg_types.Tobj [
      ("name", Jg_types.Tstr "jiro");
      ("url", Jg_types.Tstr "https://example2.com");
      ("age", Jg_types.Tint 10);
    ];
  ])
]
```

### Custom filter example

Set your custom filter to `filters` field of environment.

```ocaml
open Jingoo

let to_mail ?(kwargs=[]) ?(defaults=[]) value =
  let id = Jg_runtime.string_of_tvalue value in
  let domain = Jg_runtime.string_of_tvalue (Jg_runtime.jg_get_kvalue "domain" kwargs ~defaults) in
  Jg_types.Tstr (id ^ "@" ^ domain)

let () =
  let result = Jg_template.from_string "{{id | to_mail(domain='gmail.com')}}"
    (* set your extension to 'filters' field of environment *)
    ~env:{Jg_types.std_env with
      filters = [
        (* CAUTION!: if jingoo <= 1.2.21, use 'Jg_runtime.func_arg1' instead of 'Jg_types.func_arg1_kw' *)
        ("to_mail", Jg_types.func_arg1_kw (to_mail ~defaults:[
          ("domain", Jg_types.Tstr "gmail.com");
        ]));
      ]
    }
    ~models:[
      ("id", Jg_types.Tstr "foo")
    ] in
  (* should output 'foo@gmail.com' *)
  print_endline result
```

### Dynlink filter example

1. Write your own filter(`my_ext.ml` for example) and add it by `Jg_stub.add`(namespace as `my_ext` and func_name as `to_md5` for example).

```ocaml
open Jingoo

let to_md5 ?(kwargs=[]) ?(defaults=[]) value =
  let seed = Jg_runtime.jg_get_kvalue "seed" kwargs ~defaults in
  match value, seed with
  | Jg_types.Tstr str, Jg_types.Tstr seed ->
     Jg_types.Tstr (Digest.to_hex (Digest.string (str ^ seed)))
  | _ -> Jg_types.Tnull

let () =
  (* CAUTION!: if jingoo <= 1.2.21, use 'Jg_runtime.func_arg1' instead of 'Jg_types.func_arg1_kw' *)
  Jg_stub.add_func ~namespace:"my_ext" ~func_name:"to_md5" (Jg_types.func_arg1_kw (to_md5 ~defaults:[
    ("seed", Jg_types.Tstr "");
  ]))
```

2. Compile it to `my_ext.cmxs` by `-shared` option.

```bash
ocamlfind ocamlopt -shared -o my_ext.cmxs my_ext.ml
```

3. Set `my_ext.cmxs` to `extensions` field of environment, and you can use your custom filter `my_ext.to_md5`.

```ocaml
open Jingoo

let result = Jg_template.from_string "{{msg | my_ext.to_md5(seed='aaa')}}"
  (* set your extension to 'extensions' field *)
  ~env:{Jg_types.std_env with
    extensions = [
      "my_ext.cmxs";
    ]
  }
  ~models:[
    ("msg", Jg_types.Tstr "foo");
  ] in
(* should output '3cb988a734183289506ab7738261c827' *)
print_endline result
```

## Cheatsheet

See [samples](https://github.com/tategakibunko/jingoo/tree/master/example/samples) directory.

`*.jingoo` is template example and `*.expected` is expected string.

## Documentation

[http://tategakibunko.github.io/jingoo/](http://tategakibunko.github.io/jingoo/)

## Playground

[https://sagotch.github.io/try-jingoo/](https://sagotch.github.io/try-jingoo/)

## License

MIT License
