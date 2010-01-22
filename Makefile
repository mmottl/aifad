.PHONY: all clean install uninstall

all:
	omake

clean:
	omake clean

install:
	cd src && omake install

uninstall:
	cd src && omake uninstall
