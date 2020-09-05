@echo off
@rem $RCSfile: gsdj.bat,v $ $Revision: 1.2.2.1 $

call gssetgs.bat
%GSC% -q -sDEVICE=deskjet -r300 -dNOPAUSE -sPROGNAME=gsdj -- gslp.ps %1 %2 %3 %4 %5 %6 %7 %8 %9
