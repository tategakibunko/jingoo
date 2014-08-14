# jingoo

## about jingoo

Jingoo is OCaml template engine almost compatible with Jinja2(python template engine).

## difference between Jinja2 and Jingoo

1. i18n features are not supported yet.
2. cause of language difference between ocaml and python,
   some of built-in filters are different from original one,
   especially orders of arguments and supported optional arguments etc.
   see document of jingoo(but under construction) for detail.
3. line comment not supported. because single '#' is used very much especially in html.
   however, if you still want it, comment out the relevant code in src/jg_lexer.mll.
4. implicit conversion for keyword argument is not supported.
   that is, keyword label must be written if you want to use it. 


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
