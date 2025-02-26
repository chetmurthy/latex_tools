WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),latex_tools,ounit2,pa_ppx.utils,latex_tools.link,pa_ppx.testutils,pp-binary-ints

ML= \
	latex_tools_test.ml \

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=latex_tools_test.exe

all: $(TESTS)

test:: all
	mkdir -p _build && ./latex_tools_test.exe

latex_tools_test.exe: latex_tools_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe

