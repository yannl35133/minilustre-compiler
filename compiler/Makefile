DUNE_BUILD= /home/common/yann/.opam/dune-dev/bin/dune build

TARGET=exe
MAIN=minilustre

all: $(MAIN)

native: TARGET := exe
native: all
opt: native
$(MAIN).exe: native
$(MAIN).opt: native
$(MAIN).native: native


byte: TARGET := bc
byte: all
$(MAIN).bc: byte
$(MAIN).byte: byte

$(MAIN):
	$(DUNE_BUILD) $(MAIN).$(TARGET)


clean: 
	dune clean

realclean: clean
	rm -f *~

cleanall: realclean
