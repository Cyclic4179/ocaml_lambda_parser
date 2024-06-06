.PHONY: main clear

main:
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o parser lexer.cmo parser.cmo main.cmo
	rm parser.mli lexer.ml parser.ml
	rm lexer.cmi parser.cmi main.cmi
	rm lexer.cmo parser.cmo main.cmo

clear:
	rm parser
