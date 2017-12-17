top_srcdir = .

TESTABLE_SOURCES = \
	ogli_difftree.ml

# For the actual command line building all_tests.opt:
LINKED_FOR_TESTS = \
	ogli_difftree.ml

OGLILIB_SOURCES = \
	ogli_difftree.ml ogli_geom.ml ogli_sbbox.ml ogli_compose.ml ogli_view.ml

TESTS_SOURCES = \
	tests/test.ml

SOURCES = \
	$(OGLILIB_SOURCES) $(TESTS_SOURCES)

PACKAGES = \
	geom oaah

INSTALLED = \
	ogli.cma ogli.cmxa $(OGLILIB_SOURCES:.ml=.cmi) $(OGLILIB_SOURCES:.ml=.cmx)

all: $(INSTALLED) tests/test.opt

doc:

ogli.cmxa: $(OGLILIB_SOURCES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a $(filter %.cmx, $^) -o $@

ogli.cma: $(OGLILIB_SOURCES:.ml=.cmo)
	$(OCAMLC) $(OCAMLFLAGS) -a $(filter %.cmo, $^) -o $@

tests/test.opt: tests/test.cmx $(top_srcdir)/ogli.cmxa
	$(OCAMLOPT) -package "$(PACKAGES)" -I "$(top_srcdir)" ogli.cmxa -linkpkg $(OCAMLOPTFLAGS) $< -o $@

all_tests.opt: $(LINKED_FOR_TESTS:.ml=.cmx) all_tests.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -linkpkg -package batteries,qcheck $(filter %.cmx, $^) $(filter %.ml, $^) -o $@

clean-spec:
	$(RM) tests/test.cmx tests/test.opt tests/*.a tests/*.cmx tests/*.cmo tests/*.cmi tests/*.annot tests/*.o

distclean-spec:
	$(RM) $(INSTALLED)

check-spec:
	$(RM) tests/test.opt

include $(top_srcdir)/make.common

# Dependencies

include .depend
