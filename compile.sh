#!/bin/sh
GAMBCLIBDIR="`gsc -e \"(display (path-expand \\\"~~lib\\\"))\"`"
if [ -f "$GAMBCLIBDIR/_gambitgsc.c" ]; then
    GSC_STUB=gambitgsc
else
    GSC_STUB=gambcgsc
fi
echo "bh.scm:" && gsc -link -l "$GAMBCLIBDIR/_$GSC_STUB" bh.scm && gsc -obj bh_.c bh.c && gsc -exe -ld-options "$GAMBCLIBDIR/lib$GSC_STUB.a" bh_.o bh.o
rm -f bh_.o bh_.c bh.c bh.o

