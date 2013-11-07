################################################################################
#  jingoo: Template engine inspired by Jinja2.                                 #
#                                                                              #
#  Copyright (C) 2011-2013 by Masaki WATANABE                                  #
#                                                                              #
#  All rights reserved.                                                        #
#                                                                              #
#  Permission is hereby granted, free of charge, to any person obtaining a     #
#  copy of this software and associated documentation files (the "Software"),  #
#  to deal in the Software without restriction, including without limitation   #
#  the rights to use, copy, modify, merge, publish, distribute, sublicense,    #
#  and/or sell copies of the Software, and to permit persons to whom the       #
#  Software is furnished to do so, subject to the following conditions:        #
#                                                                              #
#  The above copyright notice and this permission notice shall be included in  #
#  all copies or substantial portions of the Software.                         #
#                                                                              #
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR  #
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,    #
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL    #
#  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER  #
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING     #
#  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER         #
#  DEALINGS IN THE SOFTWARE.                                                   #
################################################################################

EXE:=jingoo
COMPILER:=compiler
BINDIR:=/usr/local/bin
LEX:=ocamllex
YAC:=ocamlyacc
#YAC:=menhir
MLY:=jg_parser.mly
MLL:=jg_lexer.mll
PKG:=unix,dynlink,pcre,camomile
OCAMLC:=ocamlfind ocamlc -g
OCAMLOPT:=ocamlfind ocamlopt -g
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
	$(OCAMLC) -thread -c $<

jingoo_mt.cmx: jingoo_mt.ml
	$(OCAMLOPT) -thread -c $<

compiler: jingoo.cmxa jg_cmdline.ml
	$(OCAMLOPT) -o $@ -linkpkg -package $(PKG) jingoo.cmxa jg_cmdline.ml

install: all
	ocamlfind install jingoo *.a *.cma *.cmxa *.cmi jingoo_mt.cm[ox] jingoo_mt.o jg_types.mli jg_template.mli jg_stub.mli META
	cp -f $(COMPILER) $(BINDIR)/$(EXE)

uninstall:
	ocamlfind remove jingoo
	rm -f $(BINDIR)/$(EXE)

reinstall:
	make uninstall
	make install

clean:
	rm -f *.cmi *.cmo *.cmx *.cma *.a *.cmxa *.o *.annot *.byte
	rm -f jg_lexer.ml jg_parser.ml jg_parser.mli
	rm -f $(COMPILER)

rebuild:
	make clean
	make all
