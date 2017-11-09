OCBFLAGS= -use-ocamlfind -I src/ -I external/otfm/src/ -pkgs "curses,uutf,result"

all:
	ocamlbuild $(OCBFLAGS) main.native

