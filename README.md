# jingoo

## about jingoo

Jingoo is OCaml template engine almost compatible with Jinja2(python template engine), and

1. that can interprets template Jinja2 syntax.
2. that can compile template to OCaml code.


## why same syntax?

1. because Jinja2 is widely used and well defined.
2. bacause Jinja2 is already well documented.
3. using same syntax, we can reduce learning cost.


## difference between Jinja2 and Jingoo

1. trim_blocks(environment value) is not supported yet. (but syntax {%- and -%} is supported since version 1.2.3)
2. i18n features are not supported yet.
3. cause of language difference between ocaml and python,
   some of built-in filters are different from original one,
   especially orders of arguments and supported optional arguments etc.
   see document of jingoo(but under construction) for detail.
4. line comment not supported. because single '#' is used very much especially in html.
   however, if you still want it, comment out the relevant code in src/jg_lexer.mll.
5. implicit conversion for keyword argument is not supported.
   that is, keyword label must be written if you want to use it. 


## simple usage 1 (via file)

```ocaml
Jg_template.from_file "hello.tmpl" ~models:[("msg", Tstr "hello, world!")]
```


## simple usage 2 (via compiled template)

* convert .tmpl to .ml by jingoo compiler(by default, it's installed in /usr/local/bin/jingoo)

```bash
jingoo -input my_template.tmpl > my_template.ml
```

* build the template source to shared library

```bash
ocamlfind ocamlopt -o my_template.cmxs -shared -package dynlink,jingoo -linkpkg my_template.ml
```

* then following code will call the renderer in shared library (not from file).

```ocaml
Jg_template.from_file "my_template.tmpl" ~use_compiled:true ~models:[("msg", Tstr "hey, shared library")]
```

## advanced usage

1. custom filter definition.
2. extension by dynlink module.

see example directory for detail.


## cheatsheet

see "example/cheatsheet.tmpl".


## license

MIT License
