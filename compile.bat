@echo off
REM You must change for correct path 
set VCVAR=D:\VS\VC\vcvarsall.bat amd64
setlocal enableextensions enabledelayedexpansion
set TMPFILENAME=tmp.GAMBCLIBDIR.tmp
gsc -e "(display (path-expand \"~~lib\"))" > %TMPFILENAME%
set /p GAMBCLIBDIR= <%TMPFILENAME%
call %VCVAR%

echo Compiling bh.scm:
gsc -link -l "%GAMBCLIBDIR%_gambcgsc" bh.scm 
gsc -obj bh_.c bh.c 
gsc -exe -ld-options "%GAMBCLIBDIR%libgambcgsc.lib" bh_.obj bh.obj
del /Q bh_.obj bh_.c bh.c bh.obj %TMPFILENAME%

echo Done

bh --help