#
# Makefile
# Callum McColl, 2016-05-06 18:49
#

HC=ghc

BIN=main 
SRCDIR=./
BUILDDIR=${SRCDIR}/build
HSRC=Emulation.hs  Environment.hs  Helpers.hs  Main.hs  Ram.hs
HCFLAGS=-fno-warn-tabs -i../machine/parser

all:	clean run

build:
	mkdir ${BUILDDIR}
	${HC} --make ${HSRC} ${HCFLAGS} -hidir ${BUILDDIR} -odir ${BUILDDIR} -outputdir ${BUILDDIR} -tmpdir ${BUILDDIR} -o ${BUILDDIR}/${BIN}

run:	build
	${BUILDDIR}/${BIN}

clean:
	rm -rf ${BUILDDIR}

# vim:ft=make
#
# DO NOT DELETE: Beginning of Haskell dependencies
Main.oo : Main.hs
# DO NOT DELETE: End of Haskell dependencies
