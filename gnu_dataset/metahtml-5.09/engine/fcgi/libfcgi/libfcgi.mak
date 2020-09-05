# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

# Another service of Ace Wrecking and Software
#
# xmb Generated NMAKE File for NT and VC++ 

!IF "$(CFG)" == ""
CFG=libfcgi - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to libfcgi - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "libfcgi - Win32 Release" && "$(CFG)" != "libfcgi - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "libfcgi.mak" CFG="libfcgi - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libfcgi - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libfcgi - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 
!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
#
#
#  Makefile for @SUBSYSTEM@ version @VERSION@(@PLATFORM@)
#  Auto-generated, do not edit this Makefile
#

#
# Default top-level directories in which to install architecture-
# specific files (exec_prefix) and machine-independent files such
# as scripts (prefix).  The values specified here may be overridden
# at configure-time with the --exec-prefix and --prefix options
# to the "configure" script.

#-----------------------------------------------------------------------------
# Normally do not edit this section. It is setup by configure
#
# the shell MUST BE /bin/sh
#
SHELL	= @SHELL@
PLATFORM_CLASS = @PLATFORM_CLASS@
O = o
L = a
X = @X@
#
exec_prefix =   ${prefix}
prefix =        /usr/local
common_prefix = @common_prefix@
CVS_TAG =       @CVS_TAG@
SRC_DIR =	.
BIN_DIR =       $(exec_prefix)\bin
LIB_DIR =       $(exec_prefix)\lib
ETC_DIR =       $(exec_prefix)\etc
BINCLUDE_DIR =	$(exec_prefix)\include
INCLUDE_DIR =	$(common_prefix)\include
CBIN_DIR =      $(common_prefix)\bin
CLIB_DIR =      $(common_prefix)\lib
CETC_DIR =      $(common_prefix)\etc
CONTRIB_DIR =   $(common_prefix)\contrib
MAN_DIR =       $(common_prefix)\man
MAN1_DIR =      $(MAN_DIR)\man1
MAN2_DIR =      $(MAN_DIR)\man2
MAN3_DIR =      $(MAN_DIR)\man3
MAN5_DIR =      $(MAN_DIR)\man5
MAN8_DIR =      $(MAN_DIR)\man8
INFO_DIR =      $(common_prefix)\info
INSTALL		= /usr/bin/install -c
INSTALL_PROGRAM	= /usr/bin/install -c
INSTALL_DATA	= ${INSTALL} -m 644
CC              = gcc @CCDEFS@
CFLAGS          = -g -O2 @INCLUDE_PATH@ -I. -DHAVE_CONFIG_H
RANLIB		= @RANLIB@
AR		= @AR@
GENMSGC         = @GENMSGC@
GENMSGH         = @GENMSGH@
#
#---------------------------------------------------------------------------
#
#
# All OMI makefiles will have the following make targets:
#
#	all - first rule, builds everything
#	export - installs everything
#	test - runs unit tests. This can be a null rule.
#	clean - cleans up after a make
#	realclean - cleans up after a configure and make
#


################################################################################
# Begin Project
# PROP Target_Last_Scanned "libfcgi - Win32 Debug"
CPP=cl.exe
MTL=mktyplib.exe
RSC=rc.exe
MC=mc.exe

!IF  "$(CFG)" == "libfcgi - Win32 Release"
# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release


ALL : "$(OUTDIR)\libfcgi.dll"


CLEAN : 
	-@erase "$(INTDIR)\fcgi_stdio.obj"
	-@erase "$(INTDIR)\fcgiapp.obj"
	-@erase "$(INTDIR)\strerror.obj"
	-@erase "$(INTDIR)\os_win32.obj"
	-@erase "$(OUTDIR)\libfcgi.dll"
	-@erase "$(OUTDIR)\libfcgi.exp"
	-@erase "$(OUTDIR)\libfcgi.lib"



"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"


!IF "$(INTDIR)" != "$(OUTDIR)"
"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"
!ENDIF


EXPORT : 
	if not exist "$(LIB_DIR)" mkdir "$(LIB_DIR)"
	$(INSTALL_DATA) $(OUTDIR)\libfcgi.dll $(LIB_DIR)
	$(INSTALL_DATA) $(OUTDIR)\libfcgi.lib $(LIB_DIR)


TEST : "$(OUTDIR)\libfcgi.dll"




# ADD BASE CPP /nologo /MT /W3 /GX /O /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O  /I ../include /I "..\include" /I "\omi\exports\Tcl\V7.4\common\include"  /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MD /W3 /GX /O  /I ../include /I "..\include" /I "\omi\exports\Tcl\V7.4\common\include"  /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/libfcgi.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/libfcgi.bsc" 
BSC32_SBRS= \

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib  /nologo /subsystem:windows /dll /machine:I386 /nodefaultlib:"LIBCD" /out:"$(OUTDIR)\libfcgi.dll"

LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib wsock32.lib  /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)/libfcgi.pdb" /machine:I386 /nodefaultlib:"LIBCD"\
  /out:"$(OUTDIR)\libfcgi.dll"\
 /implib:"$(OUTDIR)/libfcgi.lib" 

LINK32_OBJS= \
	"$(INTDIR)\fcgi_stdio.obj" \
	"$(INTDIR)\fcgiapp.obj" \
	"$(INTDIR)\strerror.obj" \
	"$(INTDIR)\os_win32.obj" \


"$(OUTDIR)\libfcgi.dll" : "$(OUTDIR)" "$(INTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libfcgi - Win32 Debug"
# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=\local\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\libfcgi.dll"


CLEAN : 
	-@erase "$(INTDIR)\fcgi_stdio.obj"
	-@erase "$(INTDIR)\fcgiapp.obj"
	-@erase "$(INTDIR)\strerror.obj"
	-@erase "$(INTDIR)\os_win32.obj"
	-@erase "$(OUTDIR)\libfcgi.dll"
	-@erase "$(OUTDIR)\libfcgi.exp"
	-@erase "$(OUTDIR)\libfcgi.lib"


"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"


!IF "$(INTDIR)" != "$(OUTDIR)"
"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"
!ENDIF


EXPORT : 
	if not exist "$(LIB_DIR)" mkdir "$(LIB_DIR)"
	$(INSTALL_DATA) $(OUTDIR)\libfcgi.dll $(LIB_DIR)
	$(INSTALL_DATA) $(OUTDIR)\libfcgi.lib $(LIB_DIR)


TEST : "$(OUTDIR)\libfcgi.dll"



# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /I "..\include" /I "\omi\exports\Tcl\V7.4\common\include"  /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MD /W3 /GX /Z7 /Od /I "..\include" /I "\omi\exports\Tcl\V7.4\common\include"  /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/libfcgi.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/libfcgi.bsc" 
BSC32_SBRS= \

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib  /nologo /subsystem:windows /dll /machine:I386 /nodefaultlib:"LIBCD" /out:"$(OUTDIR)\libfcgi.dll"

LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib wsock32.lib  /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)/libfcgi.pdb" /machine:I386 /nodefaultlib:"LIBCD"\
  /out:"$(OUTDIR)\libfcgi.dll"\
 /implib:"$(OUTDIR)/libfcgi.lib" 

LINK32_OBJS= \
	"$(INTDIR)\fcgi_stdio.obj" \
	"$(INTDIR)\fcgiapp.obj" \
	"$(INTDIR)\strerror.obj" \
	"$(INTDIR)\os_win32.obj" \


"$(OUTDIR)\libfcgi.dll" : "$(OUTDIR)" "$(INTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "libfcgi - Win32 Release"
# Name "libfcgi - Win32 Debug"

!IF  "$(CFG)" == "libfcgi - Win32 Release"

!ELSEIF  "$(CFG)" == "libfcgi - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File
SOURCE=fcgi_stdio.c
DEF_CPP_FCGI_=\
	"..\include\fcgi_stdio.h"\
	"..\include\fcgiapp.h"\
	"..\include\fcgios.h"


"$(INTDIR)\fcgi_stdio.obj" : $(SOURCE) $(DEF_CPP_FCGI_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)

# End Source File
################################################################################
# Begin Source File
SOURCE=fcgiapp.c
DEF_CPP_FCGIA=\
	"..\include\fcgi_config.h"\
	"..\include\fcgimisc.h"\
	"..\include\fcgiapp.h"\
	"..\include\fcgiappmisc.h"\
	"..\include\fastcgi.h"\
	"..\include\fcgios.h"


"$(INTDIR)\fcgiapp.obj" : $(SOURCE) $(DEF_CPP_FCGIA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)

# End Source File
################################################################################
# Begin Source File
SOURCE=strerror.c
DEF_CPP_STRER=\
	"..\include\fcgi_config.h"


"$(INTDIR)\strerror.obj" : $(SOURCE) $(DEF_CPP_STRER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)

# End Source File
################################################################################
# Begin Source File
SOURCE=os_win32.c
DEF_CPP_OS_WI=\
	"..\include\fcgios.h"


"$(INTDIR)\os_win32.obj" : $(SOURCE) $(DEF_CPP_OS_WI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)

# End Source File

# End Target
# End Project
################################################################################

