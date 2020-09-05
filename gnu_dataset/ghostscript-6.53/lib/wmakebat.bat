@rem Execute this script with echo on, so we can see what's happening.
@rem $RCSfile: wmakebat.bat,v $ $Revision: 1.1.2.1 $
wmakel -u -n -h %1 %2 %3 %4 %5 %6 %7 %8 %9 >_wm_temp.bat
_wm_temp.bat
