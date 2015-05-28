# jingoo

## about jingoo

Jingoo is OCaml template engine almost compatible with Jinja2(python template engine).

## difference between Jinja2 and Jingoo

1. i18n features are not supported yet.
2. Cause of language difference between ocaml and python,
   some of built-in filters are different from original one,
   especially orders of arguments and supported optional arguments etc.
3. Single line comment is not supported. Because single '#' is used very much especially in html.
4. Implicit conversion for keyword argument is not supported, so keyword label is always required.

## simple usage

```ocaml
Jg_template.from_file "hello.tmpl" ~models:[("msg", Tstr "hello, world!")]
```

## advanced usage

1. custom filter definition.
2. extension by dynlink module.

see example directory for detail.


## cheatsheet

see "example/cheatsheet.tmpl".


## license

MIT License
