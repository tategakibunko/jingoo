EXE:=jingoo
COMPILER:=compiler
BINDIR:=/usr/local/bin
LEX:=ocamllex
YAC:=ocamlyacc
#YAC:=menhir
MLY:=jg_parser.mly
MLL:=jg_lexer.mll
PKG:=unix,threads,dynlink,pcre,camomile
#OCAMLC:=ocamlfind ocamlc -g -thread -annot
OCAMLC:=ocamlfind ocamlc -g -thread
OCAMLOPT:=ocamlfind ocamlopt -g -thread
LIB_SRCS:=jg_types.mli jg_types.ml jg_utils.ml\
	  jg_stub.mli jg_stub.ml \
	  jg_parser.mli jg_parser.ml jg_lexer.ml \
	  jg_runtime.ml jg_compile.ml jg_interp.ml \
	  jg_template.mli jg_template.ml \

LIB_ML:=$(filter %.ml,$(LIB_SRCS))
LIB_MLI:=$(filter %.mli,$(LIB_SRCS))
LIB_CMO:=$(foreach s,$(LIB_ML), $(basename $(notdir $(s))).cmo)
LIB_CMX:=$(foreach s,$(LIB_ML), $(basename $(notdir $(s))).cmx)
LIB_CMI:=$(foreach s,$(LIB_ML), $(basename $(notdir $(s))).cmi)

all: jg_parser.ml jg_lexer.ml jingoo.cma jingoo.cmxa jingoo_mt.cmx jingoo_mt.cmo compiler

jg_parser.ml: $(MLY)
	$(YAC) $(MLY)

jg_lexer.ml: $(MLL)
	$(LEX) $(MLL)

jingoo.cma: $(LIB_SRCS)
	$(OCAMLC) -package $(PKG) -c $(LIB_SRCS)
	$(OCAMLC) -package $(PKG) -o $@ -a $(LIB_CMO)

jingoo.cmxa: $(LIB_SRCS)
	$(OCAMLOPT) -package $(PKG) -c $(LIB_SRCS)
	$(OCAMLOPT) -package $(PKG) -o $@ -a $(LIB_CMX)

jingoo_mt.cmo: jingoo_mt.ml
	$(OCAMLC) -c $<

jingoo_mt.cmx: jingoo_mt.ml
	$(OCAMLOPT) -c $<

compiler: jingoo.cmxa jg_cmdline.ml
	$(OCAMLOPT) -o $@ -linkpkg -package $(PKG) jingoo.cmxa jg_cmdline.ml

install:
	ocamlfind install jingoo jingoo.a jingoo.cma jingoo.cmxa jingoo_mt.cm[ox] jingoo_mt.o *.cmi jg_types.mli jg_template.mli jg_stub.mli META
	cp -f $(COMPILER) $(BINDIR)/$(EXE)

uninstall:
	ocamlfind remove jingoo
	rm -f $(BINDIR)/$(EXE)

reinstall:
	make uninstall
	make install

clean:
	rm -f *.cmi *.cmo *.cmx *.cma *.a *.cmxa *.o *.annot *.byte
	rm -f $(COMPILER)

rebuild:
	make clean
	make all
