###############################################################################
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

# Always test when building.
default: test

all:
	cd src && make

test: all
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

# Headache target
#  Fix license header of file.

headache:
	find ./ \
	  -name _darcs -prune -false \
	  -o -name .git -prune -false \
	  -o -name .svn -prune -false \
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
