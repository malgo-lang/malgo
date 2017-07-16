.PHONY: all clean byte native profile debug sanity test

OCB_FLAGS = -use-ocamlfind -tag bin_annot -I src
OCB = ocamlbuild $(OCB_FLAGS)

PKGS =
PRED = 'byte' # or 'native'
OCF = ocamlfind query -r -predicates $(PRED) -format "-I %d %a" $(PKG)

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

profile: sanity
	$(OCB) -tag profile main.native

debug: sanity
	$(OCB) -tag debug main.byte

sanity:
# $(OCF)

top:
	utop -I _build/src _build/src/types.cmo _build/src/syntax.cmo

test: native
	./main.native
