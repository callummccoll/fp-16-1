#
# Makefile
# Callum McColl, 2016-05-06 18:49
#

HC = ghc

BIN = main 
SRCDIR = .
BUILDDIR = $(SRCDIR)/build
HSRC = Emulation.hs Environment.hs Helpers.hs Main.hs Presentation.hs Ram.hs
HCFLAGS = -fno-warn-tabs -i../machine/parser -XPackageImports

all:	clean run

build:
	mkdir $(BUILDDIR)
	$(HC) --make $(HSRC) $(HCFLAGS) -hidir $(BUILDDIR) -odir $(BUILDDIR) -outputdir $(BUILDDIR) -tmpdir $(BUILDDIR) -o $(BUILDDIR)/$(BIN) 

run:	build
	$(BUILDDIR)/$(BIN)

clean:
ifeq ($(OS),Windows_NT)
	rmdir $(BUILDDIR) /s /q
else
	rm -rf $(BUILDDIR)
endif

# vim:ft=make
#
# DO NOT DELETE: Beginning of Haskell dependencies
Main.oo : Main.hs
# DO NOT DELETE: End of Haskell dependencies
