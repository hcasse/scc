export SHELL:=/bin/bash
OCAMLC = ocamlc -g
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex

SOURCES = \
	common.ml \
	ast.ml \
	sem.ml \
	parser.mly \
	lexer.mll \
	main.ml
OBJECTS = $(patsubst %.mll,%.cmo,$(patsubst %.mly,%.cmo,$(patsubst %.ml,%.cmo,$(SOURCES))))
CLEAN = $(OBJECTS)

all: scc

clean:
	rm -rf $(CLEAN)

scc: $(OBJECTS)
	$(OCAMLC) -o $@ $(OBJECTS) $(OBJECT:.cmo=.cmi)

ast.cmo: common.cmo
lexer.cmo: parser.cmi common.cmo
main.cmo: parser.cmi common.cmo ast.cmo
parser.cmo: parser.cmi common.cmo ast.cmo sem.cmo
sem.cmo: common.cmo ast.cmo
parser.cmi: common.cmo

%.cmo: %.ml
	$(OCAMLC) -c $< -o $@

%.cmi: %.mli
	$(OCAMLC) -c $< -o $@

%.ml %.mli: %.mly
	$(OCAMLYACC) -v $<

%.ml: %.mll
	$(OCAMLLEX) $< -o $@

DIST = \
	$(SOURCES) \
	*.mly \
	*.mll \
	samples \
	Makefile \
	karel.txt
ARC=scc

dist:
	-rm -rf $(ARC)
	mkdir $(ARC)
	cp -R $(DIST) $(ARC)
	tar cvfz $(ARC).tgz $(ARC)
	cd $(ARC); make

