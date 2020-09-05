@echo off
@rem $RCSfile: gsnd.bat,v $ $Revision: 1.2.2.1 $

call gssetgs.bat
%GSC% -DNODISPLAY %1 %2 %3 %4 %5 %6 %7 %8 %9
