CC   = ocamlfind ocamlc
EXE  = test
PPX  = preprocess
LIBS = compiler-libs.common

all: exe

test: exe
	./$(EXE)

exe: ppx runtime
	$(CC) runtime.cma test.ml -ppx "./$(PPX) -as-ppx" -o $(EXE) -linkpkg -package zarith

runtime:
	ocamlc -a runtime/replace.ml -o runtime.cma

ppx:
	$(CC) -package ppx_tools.metaquot ppx_wide_open.ml -linkpkg -package $(LIBS) -o $(PPX)

clean:
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -f `find . -name "\#*"`
	rm -f $(PPX) $(EXE)
