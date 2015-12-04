all:
	$(MAKE) -C src

byte:
	$(MAKE) -C byte

test:
	$(MAKE) -C tests run

reinstall: uninstall install

install:
	$(MAKE) -C src install

uninstall:
	$(MAKE) -C src uninstall

clean:
	$(MAKE) -C src clean
	$(MAKE) -C tests clean

rebuild:
	$(MAKE) -C src rebuild

.PHONY: all test reinstall install uninstall reinstall clean rebuild
