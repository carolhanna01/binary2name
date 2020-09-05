@echo off
@rem $RCSfile: lpgs.bat,v $ $Revision: 1.2.2.1 $

call gssetgs.bat
%GSC% -sDEVICE#djet500 -dNOPAUSE -sPROGNAME=lpgs -- gslp.ps -fCourier9 %1 %2 %3 %4 %5 %6 %7 %8 %9
