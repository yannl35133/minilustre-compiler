#MINILUSTRE=/Users/pouzet/cours/ens/synchrone/15-16/tps/tp2/correction/minilustre
MINILUSTRE=../compiler/minilustre.exe

FILE=pendulum.mls
NODE=play
FILE_ML=`basename $(FILE) .mls`.ml

compile:
	$(MINILUSTRE) -main $(NODE) $(FILE)
	ocamlfind ocamlc -o $(NODE) -thread unix.cma threads.cma graphics.cma $(FILE_ML) -package graphics

clean:
	rm -f *.cm* $(FILE_ML) $(NODE) *~

cleanall: clean
	rm -f *~

