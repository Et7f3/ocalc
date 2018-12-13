SRC=type.ml utils.ml lexer.ml parser.ml
SRC_TEST=$(SRC) test.ml
SRC_BUILD=$(SRC) toplevel.ml
SRC_INTERPRETE=$(SRC) interprete.ml

build: $(SRC_BUILD)
	ocamlc $(SRC) -o build
	./build
	rm build

test: $(SRC_TEST)
	ocamlc $(SRC_TEST) -o test
	./test
	rm test

#-u -s option may speed up a little

interprete:
	(sed -r 's/open (.*)/;;\n#use "\L\1.ml";;/g' $(SRC_INTERPRETE); echo \;\;; cat -) | ocaml

eval:
	(sed -r 's/open (.*)/;;\n#use "\L\1.ml";;/g' $(SRC_INTERPRETE); echo \;\;) | ocaml

clean:
	rm *.cm*
