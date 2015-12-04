SRCS:=\
	test_runtime.ml \
	test_output.ml \
	test.ml \

PKGS:=dynlink,oUnit,pcre,batteries
CMPL:=ocamlfind ocamlopt -g

all: test

test: $(SRCS)
	$(CMPL) -o $@ -package $(PKGS) -linkpkg -I ../src ../src/jingoo.cmxa $(SRCS)

clean:
	rm -f *.o *.cmi *.cmx test *.log *.cache

rebuild: clean all

run: test
	./test

.PHONY: run rebuild clean all
