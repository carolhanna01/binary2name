/* Copyright (C) 1999, Ghostgum Software Pty Ltd.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
  to anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU General Public License for full details.
  
  Everyone is granted permission to copy, modify and redistribute GNU
  Ghostscript, but only under the conditions described in the GNU General
  Public License.  A copy of this license is supposed to have been given
  to you along with GNU Ghostscript so you can know your rights and
  responsibilities.  It should be in a file named COPYING.  Among other
  things, the copyright notice and this notice must be preserved on all
  copies.
*/

// $RCSfile: dwinst.h,v $ $Revision: 1.2.2.1 $

// Definitions for Ghostscript installer

#ifndef MAXSTR
#ifdef MAX_PATH
#define MAXSTR MAX_PATH
#else
#define MAXSTR 256
#endif
#endif

class CInstall  
{
public:
	CInstall();
	virtual ~CInstall();
	void SetMessageFunction(void(*fn)(const char *));
	void AddMessage(const char *message);
	const char *GetMainDir();
	const char *GetUninstallName();
	BOOL GetPrograms(BOOL bUseCommon, char *buf, int buflen);
	BOOL Init(const char *szSourceDir, const char *szFileList);
	BOOL InstallFiles(BOOL bNoCopy, BOOL *pbQuit);
	BOOL InstallFile(char *filename, BOOL bNoCopy);
	BOOL MakeDir(const char *dirname);
	FILE * MakeTemp(char *name);

	BOOL SetAllUsers(BOOL bUseCommon);
	void SetTargetDir(const char *szTargetDir);
	void SetTargetGroup(const char *szTargetGroup);

	BOOL StartMenuBegin();
	BOOL StartMenuEnd();
	BOOL StartMenuAdd(const char *szDescription, const char *szProgram, const char *szArguments);

	BOOL UpdateRegistryBegin();
	BOOL UpdateRegistryKey(const char *product, const char *version);
	BOOL UpdateRegistryValue(const char *product, const char *version, const char *name, const char *value);
	BOOL UpdateRegistryEnd();

	BOOL WriteUninstall(const char *prog, BOOL bNoCopy);
	BOOL MakeLog(void);

	void CleanUp(void);

private:
	BOOL m_bNoCopy;
	BOOL m_bUseCommon;
	BOOL m_bQuit;

	// Source directory
	char m_szSourceDir[MAXSTR];

	// File containing list of files to install
	char m_szFileList[MAXSTR];

	// Target directory for program and fonts.
	char m_szTargetDir[MAXSTR];

	// Target Group for shortcut
	char m_szTargetGroup[MAXSTR];

	// Directory where the Start Menu is located.
	char m_szPrograms[MAXSTR];

	// Name used for uninstall
	char m_szUninstallName[MAXSTR];

	// Main directory prefix, where log files should be written
	char m_szMainDir[MAXSTR];

	// Full directory where log files should be written
	char m_szLogDir[MAXSTR];

	// Temporary log files for uninstall
	char m_szFileNew[MAXSTR];
	char m_szRegistryNew[MAXSTR];
	char m_szRegistryOld[MAXSTR];
	char m_szShellNew[MAXSTR];
	char m_szShellOld[MAXSTR];

	// Log files
	FILE * m_fLogNew;
	FILE * m_fLogOld;


	BOOL SetRegistryValue(HKEY hkey, const char *value_name, const char *value);
	BOOL CreateShellLink(LPCSTR description, LPCSTR program, LPCSTR arguments, LPCSTR icon = NULL, int nIconIndex = 0);
	void CopyFileContents(FILE *df, FILE *sf);

	void(*AddMessageFn)(const char *);

};

