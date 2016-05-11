#
# Makefile
# Callum McColl, 2016-05-06 18:49
#

HC=ghc

BIN = tut
SRCDIR != pwd
BUILDDIR = ${SRCDIR}/build
HSRC != ls *.hs

LDFLAGS=


all:	clean run

build:
	mkdir ${BUILDDIR}
	${HC} --make ${HSRC} -hidir ${BUILDDIR} -odir ${BUILDDIR} -outputdir ${BUILDDIR} -tmpdir ${BUILDDIR} -o ${BUILDDIR}/${BIN}

run:	build
	${BUILDDIR}/${BIN}

clean:
	rm -rf ${BUILDDIR}

# vim:ft=make
#
# DO NOT DELETE: Beginning of Haskell dependencies
Main.oo : Main.hs
# DO NOT DELETE: End of Haskell dependencies
