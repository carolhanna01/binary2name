@echo off
@rem $RCSfile: rm.cmd,v $ $Revision: 1.1.2.1 $
:next
if '%1'=='' goto exit
if '%1'=='-f' goto sh
erase %1
:sh
shift
goto next
:exit
