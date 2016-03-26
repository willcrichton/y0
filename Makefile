SUBDIRS=src/parser,src/codegen
FLAGS=-use-menhir -use-ocamlfind -tag thread -r -Is $(SUBDIRS)
BINARY=y0c
ENTRYPOINT=main

all: build run

build:
	ocamlbuild $(FLAGS) src/$(ENTRYPOINT).native
	mv $(ENTRYPOINT).native $(BINARY)

debug:
	ocamlbuild $(FLAGS) src/$(ENTRYPOINT).d.byte
	mv $(ENTRYPOINT).d.byte $(BINARY)

run:
	./$(BINARY) tests/test.y0 --dump-llvm

clean:
	rm -f $(BINARY)
	rm -rf _build
