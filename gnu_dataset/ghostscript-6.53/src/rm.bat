@rem $RCSfile: rm.bat,v $ $Revision: 1.1.2.1 $
@echo off
:next
if '%1'=='' goto exit
if '%1'=='-f' goto sh
if exist %1 erase %1
:sh
shift
goto next
:exit
