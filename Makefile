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

clean:
	cd src && make clean
	cd tests && make clean

rebuild:
	cd src && make rebuild

