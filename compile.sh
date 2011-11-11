#!/bin/sh
GAMBCLIBDIR="`gsc -e \"(display (path-expand \\\"~~lib\\\"))\"`"
gsc -link -l "$GAMBCLIBDIR/_gambcgsc.c" bh.scm && gsc -obj bh_.c bh.c && gsc -exe -ld-options "$GAMBCLIBDIR/libgambcgsc.a" bh_.o bh.o
rm -f bh_.o bh_.c bh.c bh.o

