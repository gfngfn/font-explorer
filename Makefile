OCBFLAGS= -use-ocamlfind -I src/ -I external/otfm/src/

all:
	ocamlbuild $(OCBFLAGS) main.native

