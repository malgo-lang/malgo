.PHONY: all clean byte native profile debug test

OCB_FLAGS = -tag bin_annot
OCB = ocamlbuild $(OCB_FLANGS)

all: native byte

clean: $(OCB) -clean

native: $(OCB) main.native

byte: $(OCB) main.byte

profile: $(OCB) -tag profile main.native

debug: $(OCB) -tag debug main.byte

test: native
      ./main.native
