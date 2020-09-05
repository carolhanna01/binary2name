set PATH=c:\GTK\lib;c:\GTK\bin;c:\Programfiler\GnuWin32\bin;$PATH
copy soundcard\winmidi.pyd .
c:\Python23\python.exe setup.py py2exe --packages cairo
c:\windows\system32\xcopy /s c:\python23\lib\site-packages\gtk-2.0\*.* dist
c:\windows\system32\xcopy /s c:\GTK\*.* dist
rem c:\Python23\python.exe -c "import shutil; shutil.rmtree(r'dist\share\themes\Emacs')
rem c:\Python23\python.exe -c "import shutil; shutil.rmtree(r'dist\share\themes\Metal')
rem c:\Python23\python.exe -c "import shutil; shutil.rmtree(r'dist\share\themes\Redmond95')
rem c:\Python23\python.exe -c "import shutil; shutil.rmtree(r'dist\share\themes\MS-Windows')
rem del dist\lib\gtk-2.0\2.4.0\engines\libwimp.dll
rem ove dist\soundcard\winmidi.pyd dist
del dist\uninst.exe
pause

