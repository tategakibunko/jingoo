ROOT_DIR:="$(dir $(realpath $(lastword $(MAKEFILE_LIST))))"
DUNE=dune

.PHONY: all test doc install uninstall clean

all:
	$(DUNE) build

%dune: %dune.in
	sed -e "s|%%%OUNIT_TESTDATA_DIR%%%|$(ROOT_DIR)tests/data|g" $< > $@

test: tests/dune
	$(DUNE) build @tests/runtest

doc:
	$(DUNE) build @doc

DOCDIR=_build/default/_doc/_html/

gh-pages: doc
	gitstatus=`git status --untracked-files=no --porcelain` \
	&& [ -z "$$gitstatus" ] \
	&& branch=`git symbolic-ref -q HEAD | cut -d"/" -f 3` \
	&& [ ! -z "$$branch" ] \
	&& commit=`git rev-parse HEAD` \
	&& tmp=`mktemp -d` \
	&& mv $(DOCDIR)* "$$tmp" \
	&& (! git show-ref --verify --quiet refs/heads/gh-pages \
	    || git branch -D -f gh-pages) \
	&& git checkout --orphan gh-pages \
	&& git rm -rf --ignore-unmatch . \
	&& git clean -df \
	&& mv "$$tmp"/* . \
	&& git add . \
	&& echo '<html><head><meta http-equiv="refresh" content="0; URL=./jingoo/index.html" /></head><body>Redirecting to <a href="./jingoo/index.html">Jingoo</a> main module.</body></html>' > index.html \
	&& git commit -a -m "Build GitHub pages from commit $$commit" \
	&& git checkout "$$branch"

jingoo.install:
	$(DUNE) build @install

install: jingoo.install
	$(DUNE) install jingoo

uninstall: jingoo.install
	$(DUNE) uninstall jingoo

clean:
	$(DUNE) clean
