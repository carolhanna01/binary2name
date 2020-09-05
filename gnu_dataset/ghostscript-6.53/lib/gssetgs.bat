@echo off
@rem $RCSfile: gssetgs.bat,v $ $Revision: 1.2.2.1 $

rem Set default values for GS (gs with graphics window) and GSC
rem (console mode gs) if the user hasn't set them.

if "%GS%"=="" set GS=gswin32
if "%GSC%"=="" set GSC=gswin32c
