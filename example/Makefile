PPRINT:=camlp4 pr_o.cmo pa_o.cmo
PKG:=pcre,camomile,dynlink
CMPL:=ocamlfind ocamlopt -g -thread -package $(PKG) -linkpkg -I ../src ../src/jingoo.cmxa
JG_CMPL:=../src/compiler
TARGET:= my_ext.cmxs cheatsheet.cmxs simple advanced 

.PHONY:$(TARGET)
all: $(TARGET)

my_ext.cmxs: my_ext.ml
	$(CMPL) -shared -o $@ $<

simple: simple.ml
	$(CMPL) -o $@ $<

advanced: advanced.ml
	$(CMPL) -o $@ $<

# make compiled template
cheatsheet.cmxs: cheatsheet.tmpl
	rm -f tmp.ml
	$(JG_CMPL) -input $< > tmp.ml
	$(PPRINT) tmp.ml > cheatsheet.ml
	rm -f tmp.ml
	$(CMPL) -shared -o $@ cheatsheet.ml

clean:
	rm -f *.cmi *.cmo *.cmx *.o *.cmxs
	rm -f $(TARGET)

rebuild:
	make clean
	make all
