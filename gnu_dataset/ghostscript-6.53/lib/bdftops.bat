@echo off
@rem $RCSfile: bdftops.bat,v $ $Revision: 1.2.2.2 $

call gssetgs.bat
%GSC% -q -dBATCH -dNODISPLAY -- bdftops.ps %1 %2 %3 %4 %5 %6 %7 %8 %9
