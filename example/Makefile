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

PPRINT:=camlp4 pr_o.cmo pa_o.cmo
PKG:=pcre,camomile,dynlink
CMPL:=ocamlfind ocamlopt -g -package $(PKG) -linkpkg -I ../src ../src/jingoo.cmxa
JG_CMPL:=../src/compiler
TARGET:= my_ext.cmxs cheatsheet.cmxs simple advanced bench

#.PHONY:$(TARGET)
all: $(TARGET)

my_ext.cmxs: my_ext.ml
	$(CMPL) -shared -o $@ $<

simple: test_data.ml simple.ml
	$(CMPL) -o $@ test_data.ml simple.ml

advanced: test_data.ml advanced.ml
	$(CMPL) -o $@ test_data.ml advanced.ml

# benchmark for interp mode: ./bench
# benchmark for compiler mode: ./bench -cmpl
bench: bench.ml
	$(CMPL) -o $@ test_data.ml bench.ml

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
