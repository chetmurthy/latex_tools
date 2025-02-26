# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile.top

WD=$(shell pwd)
DESTDIR=

SYSDIRS= latex_tools runtime

TESTDIRS= tests

all: sys
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

sys:
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) all; cd ..; done

## NOTE WELL: not testing with MDX (b/c MDX is backlevel)
test: all mdx-test
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

mdx-test:: README.asciidoc.TEST

META: sys
	$(JOINMETA) -rewrite latex_tools_runtime:latex_tools.runtime \
			-direct-include latex_tools \
			-wrap-subdir runtime:runtime > META

install: META
	$(OCAMLFIND) remove latex_tools || true
	$(OCAMLFIND) install latex_tools META local-install/lib/*/*.*

uninstall:
	$(OCAMLFIND) remove latex_tools || true

clean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ..; done
	rm -rf docs local-install $(BATCHTOP) META *.corrected

depend:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
