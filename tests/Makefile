WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),latex_tools,ounit2,pa_ppx.utils,latex_tools.link,pa_ppx.testutils,pp-binary-ints

ML= \
	latex_tools_test.ml \
	latex_token_test.ml \

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=latex_tools_test.exe
TESTEXE=latex_token_test.exe

all: $(TESTS) $(TESTEXE)

test:: all
	mkdir -p _build && ./latex_tools_test.exe
	$(MAKE) tokens.TEST

latex_tools_test.exe: latex_tools_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

tokens.TEST: token-fixtures.ocaml-out token-fixtures.python-out token-fixtures.ocaml-roundtrip-out
	for f in token-fixtures/*.tex; do $(DIFF) -Bbwiu token-fixtures.python-out/`basename $$f` token-fixtures.ocaml-out/`basename $$f` ; done
	for f in token-fixtures/*.tex; do $(DIFF) -Bbwiu token-fixtures/`basename $$f` token-fixtures.ocaml-roundtrip-out/`basename $$f` ; done

token-fixtures.python-out::
	rm -rf token-fixtures.python-out && mkdir token-fixtures.python-out
	for f in token-fixtures/*.tex; do echo $$f ; python3 ../python-tools/tokens.py --dump-tokens $$f | grep -v MergedSpacer > token-fixtures.python-out/`basename $$f` || /bin/true ; done

token-fixtures.ocaml-out:: latex_token_test.exe
	rm -rf token-fixtures.ocaml-out && mkdir token-fixtures.ocaml-out
	for f in token-fixtures/*.tex; do echo $$f ; ./latex_token_test.exe $$f | grep -v MergedSpacer > token-fixtures.ocaml-out/`basename $$f` ; done

token-fixtures.ocaml-roundtrip-out:: latex_token_test.exe
	rm -rf token-fixtures.ocaml-roundtrip-out && mkdir token-fixtures.ocaml-roundtrip-out
	for f in token-fixtures/*.tex; do echo $$f ; ./latex_token_test.exe -roundtrip $$f > token-fixtures.ocaml-roundtrip-out/`basename $$f` ; done

clean::
	rm -rf *.cm* _build *.exe token-fixtures.*-out

latex_token_test.exe: latex_token_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES),latex_tools -linkpkg -linkall -o $@ $<

clean::
	rm -rf *.cm* _build *.exe token-fixtures.*-out
