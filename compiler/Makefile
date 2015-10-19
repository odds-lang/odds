OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

default: all

all: clean scanner.ml

scanner.ml: scanner.mll
	$(OCAMLLEX) scanner.mll

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

.PHONY: clean
clean:
	rm -rf scanner.mll