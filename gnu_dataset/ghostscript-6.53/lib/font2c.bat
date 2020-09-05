@echo off
@rem $RCSfile: font2c.bat,v $ $Revision: 1.2.2.1 $

call gssetgs.bat
%GSC% -q -dNODISPLAY -dWRITESYSTEMDICT -- font2c.ps %1 %2 %3 %4 %5 %6 %7 %8 %9
