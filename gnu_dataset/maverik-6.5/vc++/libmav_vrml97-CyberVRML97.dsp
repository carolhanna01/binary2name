# Microsoft Developer Studio Project File - Name="libmav_vrml97" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=libmav_vrml97 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libmav_vrml97.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libmav_vrml97.mak" CFG="libmav_vrml97 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libmav_vrml97 - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
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
# PROP BASE Output_Dir "libmav_vrml97___Win32_Debug"
# PROP BASE Intermediate_Dir "libmav_vrml97___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "../lib"
# PROP Intermediate_Dir "libmav_vrml97___Win32_Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMAV_VRML97_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "../incl" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMAV_VRML97_EXPORTS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# Begin Target

# Name "libmav_vrml97 - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\AudioClipNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\BillboardNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\BoundingBox.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\BoxNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\ConeNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\CylinderNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\DefNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\ElevationGridNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\Event.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\ExtrusionNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\Field.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\FileImage.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\FileJPEG.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\FilePNG.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\FileTarga.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\FontStyleNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\GeometryNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\GroupingNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\ImageTextureNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\IndexedFaceSetNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\IndexedLineSetNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\InlineNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\JavaVM.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\JNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\JScript.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\LodNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\mav_vrml97.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\MFColor.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\MFFloat.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\MField.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\MFInt32.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\MFRotation.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\MFString.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\MFTime.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\MFVec2f.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\MFVec3f.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\Node.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\Parser.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\PointSetNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\Proto.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\ProximitySensorNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\Route.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SceneGraph.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\ScriptNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFBool.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFColor.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFFloat.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFImage.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFInt32.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFMatrix.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFRotation.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFString.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFTime.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFVec2f.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SFVec3f.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SphereNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\SwitchNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\TextNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\TextureTransformNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\TimeSensorNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\TransformNode.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\UrlFile.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\vrmlparser.cxx
# End Source File
# Begin Source File

SOURCE=..\src\extras\VRML97\CyberVRML97\vrmlsetinfo.cxx
# End Source File
# Begin Source File

SOURCE="..\src\extras\VRML97\CyberVRML97\win32-vrml.tab.cxx"
# End Source File
# Begin Source File

SOURCE="..\src\extras\VRML97\CyberVRML97\win32-vrml.yy.cxx"
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
