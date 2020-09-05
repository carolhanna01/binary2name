@echo off
@rem $RCSfile: lpr2.bat,v $ $Revision: 1.2.2.1 $

call gssetgs.bat
%GSC% -sDEVICE#djet500 -dNOPAUSE -sPROGNAME=lpr2 -- gslp.ps -2r %1 %2 %3 %4 %5 %6 %7 %8 %9
