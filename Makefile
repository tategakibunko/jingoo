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
