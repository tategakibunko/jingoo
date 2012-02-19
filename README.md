
# jingoo 

## about jingoo

Jingoo is OCaml template engine almost compatible with Jinja2(python template engine), and

1. that can interprets template Jinja2 syntax.
2. that can compile template to Ocaml code.


## why same syntax?

1. because Jinja2 is widely used and well defined.
2. bacause Jinja2 is already well documented.
3. using same syntax, we can reduce learning cost.

## difference between Jinja2 and Jingoo

1. syntax {%- %} and trim_blocks(environment value) is not supported yet.
2. i18n features are not supported yet.
3. cause of language difference between ocaml and python,
   some of built-in filters are different from original one,
   especially orders of arguments and supported optional arguments etc.
   see document of jingoo(but under construction) for detail.
4. line comment not supported. because single '#' is used very much especially in html.
   however, if you still want it, comment out the relevant code in src/jg_lexer.mll.
5. implicit conversion for keyword argument is not supported.
   that is, keyword label must be written if you want to use it. see following example.
   keyword param 'fill_with' will work only when it declared with the name of label.

> OK: slice(2, [1,2,3,4,5], fill_with = "X") -> [[1,2], [3,4], [5,X]]
> NG: slice(2, [1,2,3,4,5], "X") -> [[1,2], [3,4], [5]]



## simple usage 1 (via file)

> Jg_template.from_file "hello.tmpl" ~models:[("msg", Tstr "hello, world!")]


## simple usage 2 (via compiled template)

1. convert .tmpl to .ml by jingoo compiler(by default, it's installed in /usr/local/bin/jingoo)

> jingoo -i my_template.tmpl > my_template.ml

2. build the template source to shared library

> ocamlfind ocamlopt -o my_template.cmxs -shared -package dynlink,jingoo -linkpkg my_template.ml

3. then following code will call the renderer in shared library (not from file).

> Jg_template.from_file "my_template.tmpl" ~use_compiled:true ~models:[("msg", Tstr "hey, shared library")]


## advanced usage
==============

1. custom filter definition.
2. extension by dynlink module.

see example directory for detail.


## cheatsheet

followings are something like a cheatsheet.
but best way to learn is to read Jinja2 official(http://jinja.pocoo.org/docs/templates/).
also example/cheatsheet.tmpl may help you.

### comment

> {# this is comment #}


### expand value

> {{ msg }}
> {{ 10 + 20 * 30 }}


### arithmetic operation

> {{ 1 + 2 }} -> 3
> {{ 2 - 1 }} -> 1
> {{ 2 * 2 }} -> 4
> {{ 3 % 2 }} -> 1
> {{ 2 ** 10 }} -> 1024



### expand value with filter

> {{ 3.14|int }}
> {{ "text"|upper }}
> {{ some_list|join(",") }}
> {{ ["one", "two", "three"]|join(",") }}

### in operation

> {% if "hoge" in ["hoge", "hige", "hage"] %}
> yes, "hoge" is member of list.
> {% endif %}


### expand value with safe filter(no escape)

> {% set js_alert = "<script>alert('hello')</script>" %}
> {{ js_alert|safe }}

### funcall

> {{ join(",", ["this", "is", "it!"]) }}

> {{ upper("must be upper" }}


### include template

> {% include "hello.tmpl" %}

> {% include "hello.tmpl" without context %}

> {% for item in items %}
> {% include "item-info.tmpl" with context %}
> {% endfor %}


### extends template

> {% extends "base.tmpl" %}
> {% block menu %} some menu {% endblock %}
> {% block main %} some main {% endblock %}


### set variable

> {% set greeting = "hello, jingoo!" %}


### if statement

> {% if some_value > 20 %}
>   over 20!
> {% elseif some_value > 30 %}
>   over 30!
> {% else %}
>   other!
> {% endif %}


### test statement

> {% set hoge = "hoge" %}

> {% if hoge is defined %}
> yes, hoge is defined.
> {% endif %}


### for statement

> {% for row in data_rows %}
>   &lt;p&gt;my name is {{ row.name }}&lt;/p&gt;
>   &lt;p&gt;age = {{ row.age }}&lt;/p&gt;
>   &lt;p&gt;{{loop.index0}}&lt;/p&gt;
>   &lt;p&gt;{{loop.index}}&lt;/p&gt;
>   &lt;p&gt;{{loop.revindex0}}&lt;/p&gt;
>   &lt;p&gt;{{loop.revindex}}&lt;/p&gt;
>   &lt;p&gt;{{loop.first}}&lt;/p&gt;
>   &lt;p&gt;{{loop.last}}&lt;/p&gt;
>   &lt;p&gt;{{loop.length}}&lt;/p&gt;
>   &lt;p&gt;{{loop.cycle(1,2,3)}}&lt;/p&gt;
> {% endfor %}

> {% for href, title in [("http://yahoo.co.jp", "yahoo"), ("http://google.co.jp", "google)] %}
> &lt;a href="{{href}}"&gt;{{title}}&lt;/a&gt;
> {% endfor %}


### macro statement

> {% macro login_box(user_name, password) %}
>   &lt;form&gt;
>     &lt;input type="text" name="user_name" value="{{user_name}}" /&gt;
>     &lt;input type="password" name="password" value="{{password}}" /&gt;
>     &lt;input type="submit" value="login" /&gt;
>   &lt;/form&gt;
> {% endmacro %}

> {{ login_box("some_user", "some_password") }}


### call statement

> {% macro subject_with_back(anchor, subject) %}
> &lt;a name="\#{{anchor}}">&lt;/a>
> &lt;h2&gt;{{subject}}&lt;/h2&gt;
> {{ caller("hello", "macro body!") }}
> &lt;a href="\#top">back to top&lt;/a&gt;
> {% endmacro %}

> {% call(a,b) subject_with_back("about", "about me") %}
> Hi, I'm a programmer living in Japan.
> {{a}} {{b}}
> {% endcall %}


### import statement

1. macro.tmpl

> {% macro a_macro(a,b) %}
>  this is a_macro, {{a}}, {{b}}
> {% endmacro %}

> {% macro b_macro(x,y) %}
>  this is b_macro, {{x}}, {{y}}
> {% endmacro %}

2. import macro

> {% import "macro.tmpl" %}
> {% import "macro.tmpl" as my_macroset %}
> {% from "macro.tmpl" import a_macro, b_macro %}
> {% from "macro.tmpl" import a_macro as a, b_macro as b %}


### raw statement

> {% raw %}
> this is not expanded -> {{foo}}
> {% endraw %}


### filter statement

> {% filter upper %}
> must be upper
> {% endfilter %}


### autoescape statement

> {% set script = "&lt;script&gt;&lt;/script&gt;" %}

> {% autoescape false %}
> this is not escaped -> {{script}}
> {% endautoescape %}


### with statement

> {% with hoge = "aaa", hige = "bbb" %}
> hoge is -> {{hoge}}
> hige is -> {{hige}}
> {% endwith %}
> after with, hoge is {{hoge}}, hige is {{hige}}

### eval (jingoo original?)

> {{ eval("{% set x = 'from eval' %}{{ x }}") }}


