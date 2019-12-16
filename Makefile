# CC   = ocamlfind ocamlc
# EXE  = test
# PPX  = preprocess
# LIBS = compiler-libs.common

# all: exe

# test: exe
# 	./$(EXE)

# exe: ppx
# 	$(CC) test.ml -ppx "./$(PPX) -as-ppx" -o $(EXE) -linkpkg -package zarith

# ppx:
# 	$(CC) -package ppx_tools.metaquot ppx_wideopen.ml -linkpkg -package $(LIBS) -o $(PPX)

# clean:
# 	rm -f `find . -name "*.o"`
# 	rm -f `find . -name "*.a"`
# 	rm -f `find . -name "*.cm*"`
# 	rm -f `find . -name "*~"`
# 	rm -f `find . -name "\#*"`
# 	rm -f $(PPX) $(EXE)

build:
	dune build

test:
	@dune runtest
	@echo "----------------"
	@echo "Original Program"
	@echo "----------------"
	@cat tests/test.ml
	@./_build/default/.ppx/ppx_wideopen/ppx.exe tests/test.ml -o rewr
	@echo "-----------------"
	@echo "Rewritten Program"
	@echo "-----------------"
	@cat rewr; rm rewr

clean:
	dune clean

.PHONY: build test clean
