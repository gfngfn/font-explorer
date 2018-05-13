PREFIX=/usr/local/bin
OCBFLAGS= -use-ocamlfind -I src/ -I external/otfm/src/ -pkgs "curses,uutf,result"
#-cflags "-i"

all:
	ocamlbuild $(OCBFLAGS) main.native
	mv main.native fontex

install:
	sudo install fontex $(PREFIX)

clean:
	ocamlbuild $(OCBFLAGS) -clean
