OCBFLAGS= -use-ocamlfind -I src/ -I external/otfm/src/ -pkgs "curses"

all:
	ocamlbuild $(OCBFLAGS) main.native

