@echo off

for /f "delims=" %%i in ('gsc -e "(display (path-expand \"~~lib\"))"') do set GAMBCLIBDIR=%%i
echo bh.scm:
gsc -link -l "%GAMBCLIBDIR%_gambcgsc" bh.scm
gsc -obj bh_.c bh.c
gsc -exe -ld-options "%GAMBCLIBDIR%libgambcgsc.lib" bh_.obj bh.obj
del /f bh_.obj bh_.c bh.c bh.obj
