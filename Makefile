default: test

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

# Headache target
#  Fix license header of file.

headache:
	find ./ \
	  -name _darcs -prune -false \
	  -name .git -prune -false \
	  -name .svn -prune -false \
	  -o -name _build -prune -false \
	  -o -name dist -prune -false \
	  -o -name '*[^~]' -type f \
	  | xargs headache -h _header -c _headache.config

.PHONY: headache

# Deploy target
#  Deploy/release the software.

deploy: headache
	# TODO: create a plugin to create documentation.
	# oasis doc-dist
	admin-gallu-deploy --verbose \
	  --forge_upload --forge_group jingoo --forge_user gildor-admin
	# TODO: when oasis doc-dist will work, re-add.
	#  --forge_extra_file "dist/jingoo-doc-$(shell oasis query version).tar.gz"
	# TODO: create a plugin to send announcement.
	# oasis announce
	admin-gallu-oasis-increment \
	  --setup_run --setup_args "-setup-update dynamic" --use_vcs

.PHONY: deploy
