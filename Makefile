.PHONY: all
all:	aifad
	@cd src && $(MAKE) byte-code

.PHONY: opt
opt:	aifad
	@cd src && $(MAKE) native-code

aifad:
	ln -sf src/aifad

.PHONY:	clean
clean:
	@cd src && $(MAKE) clean
	rm -f aifad
