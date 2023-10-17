
NAME = compila

SOURCES = \
  ast.ml \
  parser.ml \
  lexer.ml \
  type.ml \
  j.mli\
  j.ml\
  compilatore.ml\
  main.ml

OBJS = $(SOURCES:%.ml=%.cmo)

$(NAME): $(OBJS)
	ocamlc -o $@ unix.cma $^

opt: $(NAME).opt

$(NAME).opt: $(OBJS:%.cmo=%.cmx)
	ocamlopt -o $@ unix.cmxa $^

lexer.cmo : parser.cmi
parser.cmo parser.cmx : parser.cmi
j.cmo: j.cmi

%.cmi : %.mli
	ocamlc -c $<

%.cmo %.cmi : %.ml
	ocamlc -c $<

%.cmx %.cmi : %.ml
	ocamlopt -c $<

%.ml : %.mll
	ocamllex $<

%.ml %.mli : %.mly
	ocamlyacc -v $<

clean:
	rm -f parser.mli parser.output *.cmo *.cmi *.cmx $(NAME){,.opt}

