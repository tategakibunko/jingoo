# jingoo

## about jingoo

Jingoo is OCaml template engine almost compatible with Jinja2(python template engine).

## install

### manual

```bash
make
sudo make install
```
### opam

```bash
opam install jingoo
```

## difference between Jinja2 and Jingoo

1. i18n features are not supported yet.
2. Cause of language difference between ocaml and python,
   some of built-in filters are different from original one,
   especially orders of arguments and supported optional arguments etc.
3. Single line comment is not supported. Because single '#' is used very much especially in html.
4. Implicit conversion for keyword argument is not supported, so keyword label is always required.

## simple usage

```ocaml
Jg_template.from_file "hello.jingoo" ~models:[("msg", Tstr "hello, world!")]
```

## advanced usage

1. custom filter definition.
2. extension by dynlink module.

see [examples](https://github.com/tategakibunko/jingoo/tree/master/example) for detail.


## cheatsheet

see [cheatsheet.jingoo](https://github.com/tategakibunko/jingoo/blob/master/example/cheatsheet.jingoo).


## license

MIT License
