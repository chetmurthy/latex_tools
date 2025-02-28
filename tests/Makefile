WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),latex_tools,ounit2,pa_ppx.utils,latex_tools.link,pa_ppx.testutils,pp-binary-ints

ML= \
	utils_test.ml \
	latex_tools_test.ml \
	latex_token_tool.ml \

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=utils_test.exe latex_tools_test.exe
TESTEXE=latex_token_tool.exe

all: $(TESTS) $(TESTEXE)

test:: all
	mkdir -p _build && ./utils_test.exe
	mkdir -p _build && ./latex_tools_test.exe
	$(MAKE) tokens.TEST

latex_tools_test.exe: latex_tools_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

utils_test.exe: utils_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

latex_token_tool.exe: latex_token_tool.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES),latex_tools -linkpkg -linkall -o $@ $<

TOKENTEXFILES=$(wildcard token-fixtures/*.tex)
TOKENTESTS=$(patsubst %.tex,%.TOKEN-TEST,$(TOKENTEXFILES))
ROUNDTRIPTESTS=$(patsubst %.tex,%.ROUNDTRIP-TEST,$(TOKENTEXFILES))

.tex.TOKEN-TEST:: latex_token_tool.exe ../python-tools/tokens.py
	mkdir -p tmp/$(shell dirname $*)
	python3 ../python-tools/tokens.py --dump-tokens $*.tex | grep -q -v MergedSpacer > tmp/$*.python-tokens-out || /bin/true
	./latex_token_tool.exe $*.tex | grep -q -v MergedSpacer > tmp/$*.ocaml-tokens.out || /bin/true
	$(DIFF) -Bbwiu tmp/$*.python-tokens-out tmp/$*.ocaml-tokens.out

.tex.ROUNDTRIP-TEST::
	mkdir -p tmp/$(shell dirname $*)
	./latex_token_tool.exe -roundtrip $*.tex > tmp/$*.tex
	$(DIFF) -Bbwiu $*.tex tmp/$*.tex

.SUFFIXES: .TOKEN-TEST .ROUNDTRIP-TEST

tokens.TEST: $(TOKENTESTS) $(ROUNDTRIPTESTS)

clean::
	rm -rf *.cm* _build *.exe token-fixtures.*-out  tmp
