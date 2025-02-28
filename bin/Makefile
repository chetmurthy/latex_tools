WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),latex_tools,ounit2,pa_ppx.utils,latex_tools.link,pa_ppx.testutils,pp-binary-ints

ML= \
	extract_environments.ml \

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

EXE=extract_environments.exe

all: $(EXE)

extract_environments.exe: extract_environments.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<


clean::
	rm -rf _build *.exe
