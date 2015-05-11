# To make LLVM work: cp /usr/local/lib/libLLVMSupport.a /Users/will/.opam/system/lib/llvm

PACKAGES=core,ollvm,ollvm_llvmgateway,llvm
SUBDIRS=parser,codegen
FLAGS=-use-menhir -use-ocamlfind -tag thread -pkg $(PACKAGES) -r -Is $(SUBDIRS)
BINARY=y0c
ENTRYPOINT=main

all: build run

build:
	ocamlbuild $(FLAGS) $(ENTRYPOINT).native
	mv $(ENTRYPOINT).native $(BINARY)

debug:
	ocamlbuild $(FLAGS) $(ENTRYPOINT).d.byte
	mv $(ENTRYPOINT).d.byte $(BINARY)

run:
	./$(BINARY) test.y0

clean:
	rm -f $(BINARY)
	rm -rf _build