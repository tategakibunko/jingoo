ROOT_DIR:="$(dir $(realpath $(lastword $(MAKEFILE_LIST))))"
DUNE=dune
ifdef PROFILE
    DUNE_PROFILE=--profile $(PROFILE)
endif

.PHONY: all test doc install uninstall clean

all:
	$(DUNE) build $(DUNE_PROFILE)

%dune: %dune.in
	sed -e "s|%%%ROOT_DIR%%%|$(ROOT_DIR)|g" $< > $@

test: tests/dune example/dune
test: $(wildcard example/samples/*.expected)
test: $(wildcard example/samples/*.json)
test:
	$(DUNE) build $(DUNE_PROFILE) @runtest

doc: example/dune example/templates/dune
doc: $(wildcard example/templates/*.jingoo)
doc: $(wildcard example/samples/*.jingoo)
doc: $(wildcard example/samples/*.expected)
doc: $(wildcard example/samples/*.json)
doc:
	$(DUNE) build $(DUNE_PROFILE) @doc

OCAML_DOC_DIR=_build/default/_doc/_html/
TEMPLATES_DOC_DIR=_build/default/example/

gh-pages: doc
	gitstatus=`git status --untracked-files=no --porcelain` \
	&& [ -z "$$gitstatus" ] \
	&& branch=`git symbolic-ref -q HEAD | cut -d "/" -f 3` \
	&& [ ! -z "$$branch" ] \
	&& commit=`git rev-parse HEAD` \
	&& tmp=`mktemp -d`/ \
	&& mkdir "$$tmp"ocaml \
	&& mkdir "$$tmp"templates \
	&& mv $(OCAML_DOC_DIR)* "$$tmp"ocaml \
	&& mv $(TEMPLATES_DOC_DIR)templates/templates.css "$$tmp"templates \
	&& mv $(TEMPLATES_DOC_DIR)templates/templates.*.html "$$tmp"templates \
	&& mv $(TEMPLATES_DOC_DIR)index.html "$$tmp"\
	&& (! git show-ref --verify --quiet refs/heads/gh-pages \
	    || git branch -D -f gh-pages) \
	&& git checkout --orphan gh-pages \
	&& git rm -rf --ignore-unmatch . \
	&& git clean -df \
	&& mv "$$tmp"/* . \
	&& git add . \
	&& git commit -a -m "Build GitHub pages from commit $$commit" \
	&& git checkout "$$branch"

jingoo.install:
	$(DUNE) build $(DUNE_PROFILE) @install

install: jingoo.install
	$(DUNE) install jingoo

uninstall: jingoo.install
	$(DUNE) uninstall jingoo

clean:
	$(DUNE) clean
	rm -f tests/dune doc/dune

rebuild:
	make clean
	make
