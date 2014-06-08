all: build

build:
	obuild build

install:
	obuild install

uninstall:
	ocamlfind remove mtl

configure:
	obuild configure

test:
	@echo TODO
