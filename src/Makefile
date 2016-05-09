EXE:=jingoo
COMPILER:=compiler
LEX:=ocamllex
YAC:=ocamlyacc
#YAC:=menhir
MLY:=jg_parser.mly
MLL:=jg_lexer.mll
PKG:=unix,dynlink,pcre,uutf
OCAMLC:=ocamlfind ocamlc -g
OCAMLOPT:=ocamlfind ocamlopt -g
LIB_SRCS:=jg_types.mli jg_types.ml jg_utils.ml\
	  jg_stub.mli jg_stub.ml \
	  jg_parser.mli jg_parser.ml jg_lexer.ml \
	  jg_runtime.ml jg_interp.ml \
	  jg_template.mli jg_template.ml \

LIB_ML:=$(filter %.ml,$(LIB_SRCS))
LIB_MLI:=$(filter %.mli,$(LIB_SRCS))
LIB_CMO:=$(foreach s,$(LIB_ML), $(basename $(notdir $(s))).cmo)
LIB_CMX:=$(foreach s,$(LIB_ML), $(basename $(notdir $(s))).cmx)
LIB_CMI:=$(foreach s,$(LIB_ML), $(basename $(notdir $(s))).cmi)

all: byte opt

byte: jg_parser.ml jg_lexer.ml jingoo.cma jingoo_mt.cmo

opt: jingoo.cmxa jingoo_mt.cmx compiler

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
	$(OCAMLC) -thread -c $<

jingoo_mt.cmx: jingoo_mt.ml
	$(OCAMLOPT) -thread -c $<

compiler: jingoo.cmxa jg_cmdline.ml
	$(OCAMLOPT) -o $@ -linkpkg -package $(PKG) jingoo.cmxa jg_cmdline.ml

install: all
	ocamlfind install jingoo *.a *.cma *.cmi jingoo_mt.cmo jingoo_mt.o jg_types.mli jg_template.mli jg_stub.mli META -optional *.cmxa jingoo_mt.cmx

uninstall:
	ocamlfind remove jingoo

reinstall: uninstall install

clean:
	rm -f *.cmi *.cmo *.cmx *.cma *.a *.cmxa *.o *.annot *.byte
	rm -f jg_lexer.ml jg_parser.ml jg_parser.mli
	rm -f $(COMPILER)

rebuild: clean all

.PHONY: rebuild clean reinstall uninstall install compiler
