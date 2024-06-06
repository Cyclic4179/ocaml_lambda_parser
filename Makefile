.PHONY: main clear

main:
	ocamlyacc parser.mly
	ocamlc -c expr.ml
	ocamlc -c parser.mli
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o parser expr.cmo lexer.cmo parser.cmo main.cmo
	rm parser.mli lexer.ml parser.ml
	rm lexer.cmi parser.cmi main.cmi expr.cmi
	rm lexer.cmo parser.cmo main.cmo expr.cmo

clear:
	rm parser
