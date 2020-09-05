@echo off
@rem $RCSfile: gsbj.bat,v $ $Revision: 1.2.2.1 $

call gssetgs.bat
%GSC% -q -sDEVICE=bj10e -r180 -dNOPAUSE -sPROGNAME=gsbj -- gslp.ps %1 %2 %3 %4 %5 %6 %7 %8 %9
