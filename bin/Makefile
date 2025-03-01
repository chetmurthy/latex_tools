WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),latex_tools,ounit2,pa_ppx.utils,latex_tools.link,pa_ppx.testutils,pp-binary-ints

ML= \
	extract_environments.ml count_references.ml resolve_imports.ml \

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

EXE=extract_environments.exe count_references.exe resolve_imports.exe

all: $(EXE)

%.exe: %.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf _build *.exe
