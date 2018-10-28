ROOT_DIR:="$(dir $(realpath $(lastword $(MAKEFILE_LIST))))"
DUNE=dune

all:
	$(DUNE) build

%dune: %dune.in
	sed -e "s|%%%OUNIT_TESTDATA_DIR%%%|$(ROOT_DIR)/tests/data|g" $< > $@

test: tests/dune
	$(DUNE) build @tests/runtest

jingoo.install:
	$(DUNE) build @install

install: jingoo.install
	$(DUNE) install jingoo

uninstall: jingoo.install
	$(DUNE) uninstall jingoo

clean:
	$(DUNE) clean
