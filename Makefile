export SHELL:=/bin/bash
OCAMLC = ocamlc -g
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex

SOURCES = \
	common.ml \
	ast.ml \
	sem.ml \
	cst.ml \
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


# packaging
DIST = \
	$(SOURCES) \
	Makefile \
	tests/
ARC=scc


dist:
	-rm -rf $(ARC)
	mkdir $(ARC)
	cp -R $(DIST) $(ARC)
	tar cvfz $(ARC).tgz $(ARC)
	cd $(ARC); make

DELIVER_NAME = deliver-$(shell date "+%Y.%m.%d")
DELIVER_FILES = $(sort $(DIST))
deliver:
	tar cvfz $(DELIVER_NAME).tgz $(DELIVER_FILES)


# testing
POS_TESTS = \
	global
NEG_TESTS = \
	init-exist-err \
	init-ptr-err

test: scc

	@for f in $(POS_TESTS); do \
		echo -n "$$f ... "; \
		if ./scc tests/$$f.c >tests/$$f.out 2>&1; then \
			if diff tests/$$f.ref tests/$$f.out; then \
				echo "OK"; \
			else \
				echo "failed (output)"; \
			fi; \
		else \
			echo "failed (error)"; \
		fi; \
	done

	@for f in $(NEG_TESTS); do \
		echo -n "$$f ... "; \
		if ./scc tests/$$f.c > tests/$$f.out 2>&1; then \
			echo "failed (no error)"; \
		else \
			if diff tests/$$f.ref tests/$$f.out; then \
				echo "OK"; \
			else \
				echo "failed (output)"; \
			fi; \
		fi; \
	done

tests/%.ref: tests/%.c scc
	-./scc $< >$@ 2>&1

	
