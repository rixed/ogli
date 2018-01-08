TESTABLE_SOURCES = \
	lr44.ml ogli_difftree.ml

# For the actual command line building all_tests.opt:
LINKED_FOR_TESTS = \
	lr44.ml ogli_difftree.ml

OGLILIB_SOURCES = \
	lr44.ml ogli.ml ogli_difftree.ml ogli_shape.ml \
	ogli_view.ml ogli_render.ml

NAME = ogli

TESTS_SOURCES = \
	tests/test.ml

SOURCES = \
	$(OGLILIB_SOURCES) $(TESTS_SOURCES)

PACKAGES = geom glop

INSTALLED = \
	META ogli.a ogli.cma ogli.cmxa $(OGLILIB_SOURCES:.ml=.cmi) $(OGLILIB_SOURCES:.ml=.cmx)

all: $(INSTALLED) tests/test.opt

doc:

ogli.a: ogli.cmxa

ogli.cmxa: $(OGLILIB_SOURCES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a $(filter %.cmx, $^) -o $@

ogli.cma: $(OGLILIB_SOURCES:.ml=.cmo)
	$(OCAMLC) $(OCAMLFLAGS) -a $(filter %.cmo, $^) -o $@

tests/test.opt: tests/test.cmx ogli.cmxa
	$(OCAMLOPT) -package "$(PACKAGES) unix" -I "." ogli.cmxa -linkpkg $(OCAMLOPTFLAGS) $< -o $@

tests/test.byte: tests/test.cmo ogli.cma
	$(OCAMLC) -package "$(PACKAGES)" -I "." ogli.cma -linkpkg $(OCAMLFLAGS) $< -o $@

all_tests.opt: $(LINKED_FOR_TESTS:.ml=.cmx) all_tests.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -linkpkg -package batteries,qcheck $(filter %.cmx, $^) $(filter %.ml, $^) -o $@

clean-spec:
	$(RM) tests/test.cmx tests/test.{opt,byte} tests/*.a tests/*.cmx tests/*.cmo tests/*.cmi tests/*.annot tests/*.o

distclean-spec:
	$(RM) tests/test.{opt,byte}

check-spec:
#	tests/test.opt

include make.common

# Dependencies

include .depend
