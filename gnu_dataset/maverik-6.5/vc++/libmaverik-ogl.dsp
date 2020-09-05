# Microsoft Developer Studio Project File - Name="libmaverik" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=libmaverik - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libmaverik.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libmaverik.mak" CFG="libmaverik - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libmaverik - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe
# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "libmaverik___Win32_Debug"
# PROP BASE Intermediate_Dir "libmaverik___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "../lib"
# PROP Intermediate_Dir "libmaverik___Win32_Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMAVERIK_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "../incl" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMAVERIK_EXPORTS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib glu32.lib opengl32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# Begin Target

# Name "libmaverik - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\src\objects\mav_ac3d.c
# End Source File
# Begin Source File

SOURCE=..\src\callbacks\mav_bb.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_box.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_callback.c
# End Source File
# Begin Source File

SOURCE=..\src\callbacks\mav_callbacks.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_class.c
# End Source File
# Begin Source File

SOURCE=..\src\callbacks\mav_clip.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_composite.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_cone.c
# End Source File
# Begin Source File

SOURCE=..\src\windows\mav_crossing.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_ctorus.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_cylinder.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_device.c
# End Source File
# Begin Source File

SOURCE=..\src\callbacks\mav_draw.c
# End Source File
# Begin Source File

SOURCE=..\src\callbacks\mav_dump.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_ellipse.c
# End Source File
# Begin Source File

SOURCE=..\src\windows\mav_expose.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_facet.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_frame.c
# End Source File
# Begin Source File

SOURCE=..\src\callbacks\mav_get.c
# End Source File
# Begin Source File

SOURCE=..\src\gfx\mav_gfxOpenGL.c
# End Source File
# Begin Source File

SOURCE=..\src\gfx\mav_gfxWMOpenGLWin32.c
# End Source File
# Begin Source File

SOURCE=..\src\SMS\mav_hbb.c
# End Source File
# Begin Source File

SOURCE=..\src\SMS\mav_hbbConstruct.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_hellipse.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_hsphere.c
# End Source File
# Begin Source File

SOURCE=..\src\callbacks\mav_id.c
# End Source File
# Begin Source File

SOURCE=..\src\callbacks\mav_intersect.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_jif.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_kernel.c
# End Source File
# Begin Source File

SOURCE=..\src\windows\mav_keyboard.c
# End Source File
# Begin Source File

SOURCE=..\src\navigation\mav_keyboardnav.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_lists.c
# End Source File
# Begin Source File

SOURCE=..\src\windows\mav_map.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_maths.c
# End Source File
# Begin Source File

SOURCE=..\src\windows\mav_mouse.c
# End Source File
# Begin Source File

SOURCE=..\src\navigation\mav_mousenav.c
# End Source File
# Begin Source File

SOURCE=..\src\navigation\mav_navigation.c
# End Source File
# Begin Source File

SOURCE=..\src\navigation\mav_navigators.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_object.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_objects.c
# End Source File
# Begin Source File

SOURCE=..\src\SMS\mav_objList.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_palette.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_polygon.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_polygonGrp.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_polyline.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_pyramid.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_rectangle.c
# End Source File
# Begin Source File

SOURCE=..\src\windows\mav_resize.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_rtorus.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_sms.c
# End Source File
# Begin Source File

SOURCE=..\src\SMS\mav_smscallbacks.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_smsobj.c
# End Source File
# Begin Source File

SOURCE=..\src\SMS\mav_smss.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_sphere.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_splashDef.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_teapot.c
# End Source File
# Begin Source File

SOURCE=..\src\objects\mav_text.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_textures.c
# End Source File
# Begin Source File

SOURCE=..\src\kernel\mav_window.c
# End Source File
# Begin Source File

SOURCE=..\src\windows\mav_windows.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
