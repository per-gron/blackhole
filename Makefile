VPATH = src
BUILDDIR = build
GAMBCLIBDIR = `gsc -e "(display (path-expand \\"~~lib\\"))"`

CFLAGS = -O2

.PHONY: clean all

all: bh blackhole.o1

%.o: %.c
	gsc -o $@ -obj -cc-options "${CFLAGS}" $^

bh: ${BUILDDIR}/bh_.o ${BUILDDIR}/bh.o
	gsc -o $@ -exe -ld-options "-L${GAMBCLIBDIR} -lgambcgsc" $^

${BUILDDIR}/bh_.c: ${BUILDDIR}/bh.c ${BUILDDIR}
	gsc -o ${BUILDDIR} -link -l "${GAMBCLIBDIR}/_gambcgsc" $<

${BUILDDIR}/bh.c: bh.scm ${BUILDDIR}
	gsc -o $@ -c $<

${BUILDDIR}:
	mkdir $@

blackhole.o1: deventry.scm
	gsc -o $@ -cc-options "${CFLAGS}" $< 

clean:
	rm -rf bh
	rm -rf blackhole.o1
	rm -rf ${BUILDDIR}
