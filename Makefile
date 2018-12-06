SRC=lexer.ml
SRC_TEST=$(SRC) test.ml
SRC_BUILD=$(SRC) toplevel.ml

build: $(SRC_BUILD)
	ocamlc $(SRC) -o build
	./build
test: $(SRC_TEST)
	ocamlc $(SRC_TEST) -o test
	./test
