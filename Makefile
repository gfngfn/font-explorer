OCBFLAGS= -use-ocamlfind -I src/ -I external/otfm/src/ -pkgs "curses,uutf,result"
#-cflags "-i"

all:
	ocamlbuild $(OCBFLAGS) main.native

