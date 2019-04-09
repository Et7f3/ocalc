SRC=type.ml utils.ml lexer.ml parser.ml grandEntier.mli grandEntier.ml
SRC_TEST=$(SRC) test.ml
SRC_BUILD=$(SRC) toplevel.ml
SRC_INTERPRETE=$(SRC) interprete.ml

build: $(SRC_BUILD)
	ocamlc $^ -o $@
	./$@
	rm $@

test: $(SRC_TEST)
	ocamlc $^ -o $@
	./$@
	rm $@

#-u -s option may speed up a little

interprete:
	(sed -r 's/open (.*)/;;\n#use "\L\1.ml";;/g' $(SRC_INTERPRETE); echo \;\;; cat -) | ocaml

eval:
	(sed -r 's/open (.*)/;;\n#use "\L\1.ml";;/g' $(SRC_INTERPRETE); echo \;\;) | ocaml

clean:
	rm -f *.cm* *.html
#*.css we will have our special stylesheet so don't remove it

doc_build: build
	ocamldoc -html -all-params -colorize-code -charset utf-8 $(SRC_BUILD)
