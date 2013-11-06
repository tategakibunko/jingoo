all:
	cd src && make

test:
	cd tests && make run

reinstall:
	cd src && make uninstall && make install

install:
	cd src && make install

uninstall:
	cd src && make uninstall

reinstall: uninstall install

clean:
	cd src && make clean
	cd tests && make clean

rebuild:
	cd src && make rebuild

.PHONY: all test reinstall install uninstall reinstall clean rebuild

# Precommit target
#  Check style of code.
PRECOMMIT_ARGS= \
	    --exclude myocamlbuild.ml \
	    --exclude setup.ml \
	    --exclude README.txt \
	    --exclude INSTALL.txt \
	    --exclude Makefile \
	    --exclude configure \
	    --exclude _tags

precommit:
	@if command -v OCamlPrecommit > /dev/null; then \
	  OCamlPrecommit $(PRECOMMIT_ARGS); \
	else \
	  echo "Skipping precommit checks.";\
	fi

precommit-full:
	OCamlPrecommit --full $(PRECOMMIT_ARGS)

test: precommit

.PHONY: precommit
