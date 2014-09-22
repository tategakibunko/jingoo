SRCS:=\
	test_runtime.ml \
	test_output.ml \
	test.ml \

PKGS:=dynlink,oUnit,pcre,batteries
CMPL:=ocamlfind ocamlopt -g

all: test

.PHONY:test
test: $(SRCS)
	$(CMPL) -o $@ -package $(PKGS) -linkpkg -I ../src ../src/jingoo.cmxa $(SRCS)

clean:
	rm -f *.o *.cmi *.cmx test *.log *.cache

rebuild:
	make clean
	make all

run: test
	./test

