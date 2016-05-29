#
# Makefile
# Callum McColl, 2016-05-06 18:49
#

HC=ghc

BIN=main 
ifeq ($(OS),Windows_NT)
SRCDIR=.
HSRC=Emulation.hs  Environment.hs  Helpers.hs  Main.hs  Ram.hs
BUILDDIR=${SRCDIR}\build
else
SRCDIR!=pwd
HSRC!=ls *.hs
BUILDDIR=${SRCDIR}/build
endif
HCFLAGS=-fno-warn-tabs -i../machine/parser

all:	clean run

build:
	mkdir ${BUILDDIR}
	${HC} --make ${HSRC} ${HCFLAGS} -hidir ${BUILDDIR} -odir ${BUILDDIR} -outputdir ${BUILDDIR} -tmpdir ${BUILDDIR} -o ${BUILDDIR}/${BIN}

run:	build
	${BUILDDIR}/${BIN}

clean:
ifeq ($(OS),Windows_NT)
	rmdir ${BUILDDIR} /s /q
else
	rm -rf ${BUILDDIR}
endif

# vim:ft=make
#
# DO NOT DELETE: Beginning of Haskell dependencies
Main.oo : Main.hs
# DO NOT DELETE: End of Haskell dependencies
