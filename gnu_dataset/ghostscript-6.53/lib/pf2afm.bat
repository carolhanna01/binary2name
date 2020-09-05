@echo off 
@rem $RCSfile: pf2afm.bat,v $ $Revision: 1.1.2.2 $
@rem Make an AFM file from PFB / PFA and (optionally) PFM files.

if "%1"=="" goto usage
if not "%2"=="" goto usage
call gssetgs.bat

%GSC% -q -dNODISPLAY -dSAFER -dDELAYSAFER -- pf2afm.ps %1
goto end

:usage
echo "Usage: pf2afm disk_font_name"

:end


