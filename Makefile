PACKAGES=core,llvm,llvm.all_backends,llvm.target,llvm.executionengine,ollvm,ollvm_llvmgateway
SUBDIRS=src/parser,src/codegen
FLAGS=-use-menhir -use-ocamlfind -tag thread -pkg $(PACKAGES) -r -Is $(SUBDIRS)
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
	./$(BINARY) tests/test.y0

clean:
	rm -f $(BINARY)
	rm -rf _build