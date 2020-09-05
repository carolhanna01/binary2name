/* Copyright (C) 1996-2000, Russell Lang.  All rights reserved.
  
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


// $RCSfile: dwmainc.cpp,v $ $Revision: 1.3.2.1 $
// Ghostscript DLL loader for Windows 95/NT
// For WINDOWCOMPAT (console mode) application

#define STRICT
#include <windows.h>
#include <shellapi.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dos.h>
#include <fcntl.h>
#include <io.h>
extern "C" {
#include "gscdefs.h"
#define GSREVISION gs_revision
#include "gsdll.h"
}
#include "dwmain.h"
#include "dwdll.h"

#if defined(_MSC_VER) && defined(__WIN32__)
#define _export
#endif

/* public handles */
HINSTANCE ghInstance;

const char *szDllName = "GSDLL32.DLL";


int FAR _export gsdll_callback(int message, char FAR *str, unsigned long count);

// the Ghostscript DLL class
gsdll_class gsdll;

char start_string[] = "systemdict /start get exec\n";

// program really starts at WinMain
int
new_main(int argc, char *argv[])
{
typedef char FAR * FARARGV_PTR;
int rc;

    setmode(fileno(stdin), O_BINARY);
    setmode(fileno(stdout), O_BINARY);

    // load DLL
    if (gsdll.load(ghInstance, szDllName, GSREVISION)) {
	char buf[256];
	gsdll.get_last_error(buf, sizeof(buf));
	fputs(buf, stdout);
	return 1;
    }

    // initialize the interpreter
    rc = gsdll.init(gsdll_callback, (HWND)NULL, argc, argv);
    if (rc == GSDLL_INIT_QUIT) {
        gsdll.unload();
	return 0;
    }
    if (rc) {
	char buf[256];
	gsdll.get_last_error(buf, sizeof(buf));
	fputs(buf, stdout);
        gsdll.unload();
	return rc;
    }

    // if (!batch)
    gsdll.execute(start_string, strlen(start_string));
    
    gsdll.unload();

    return 0;
}


#if defined(_MSC_VER)
/* MSVC Console EXE needs main() */
int
main(int argc, char *argv[])
{
    /* need to get instance handle */
    ghInstance = GetModuleHandle(NULL);

    return new_main(argc, argv);
}
#else
/* Borland Console EXE needs WinMain() */
#pragma argsused  // ignore warning about unused arguments in next function

int PASCAL 
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmdLine, int cmdShow)
{
#define MAXCMDTOKENS 128
    // BC++ 4.5 will give us _argc and _argv[], but they will be 
    // incorrect if there is a space in the program name.
    // Provide our own parsing code to create argc and argv[]. 
    int argc;
    LPSTR argv[MAXCMDTOKENS];
    LPSTR p;
    char command[256];
    char *args;
    char *d, *e;
 
    if (hPrevInstance) {
	fputs("Can't run twice", stdout);
	return FALSE;
    }

    /* copy the hInstance into a variable so it can be used */
    ghInstance = hInstance;

    // If called with "gswin32c.exe arg1 arg2"
    // lpszCmdLine returns:
    //    "arg1 arg2" if called with CreateProcess(NULL, command, ...)
    //    "arg2"      if called with CreateProcess(command, args, ...)
    // GetCommandLine() returns
    //    ""gswin32c.exe" arg1 arg2" 
    //          if called with CreateProcess(NULL, command, ...)
    //    "  arg1 arg2"      
    //          if called with CreateProcess(command, args, ...)
    // Consequently we must use GetCommandLine() 
    p = GetCommandLine();

    argc = 0;
    args = (char *)malloc(lstrlen(p)+1);
    if (args == (char *)NULL) {
	fprintf(stdout, "Insufficient memory in WinMain()\n");
	return 1;
    }

   
    // Parse command line handling quotes.
    d = args;
    while (*p) {
	// for each argument

	if (argc >= MAXCMDTOKENS - 1)
	    break;

        e = d;
        while ((*p) && (*p != ' ')) {
	    if (*p == '\042') {
		// Remove quotes, skipping over embedded spaces.
		// Doesn't handle embedded quotes.
		p++;
		while ((*p) && (*p != '\042'))
		    *d++ =*p++;
	    }
	    else 
		*d++ = *p;
	    if (*p)
		p++;
        }
	*d++ = '\0';
	argv[argc++] = e;

	while ((*p) && (*p == ' '))
	    p++;	// Skip over trailing spaces
    }
    argv[argc] = NULL;

    if (strlen(argv[0]) == 0) {
	GetModuleFileName(hInstance, command, sizeof(command)-1);
	argv[0] = command;
    }

    return new_main(argc, argv);
}
#endif


int
read_stdin(char FAR *str, int len)
{
int ch;
int count = 0;
    while (count < len) {
	ch = fgetc(stdin);
	if (ch == EOF)
	    return count;
	*str++ = ch;
	count++;
	if (ch == '\n')
	    return count;
    }
    return count;
}


int FAR _export
gsdll_callback(int message, char FAR *str, unsigned long count)
{
char buf[256];
    switch (message) {
	case GSDLL_POLL:
	    // Don't check message queue because we don't
	    // create any windows.
	    // May want to return error code if abort wanted
	    break;
	case GSDLL_STDIN:
	    return read_stdin(str, count);
	case GSDLL_STDOUT:
	    fwrite(str, 1, count, stdout);
	    fflush(stdout);
	    return count;
	case GSDLL_DEVICE:
	    if (count) {
		fputs("\n\
The mswindll device is not supported by the command line version of\n\
Ghostscript.  Select a different device using -sDEVICE= as described\n\
in Use.htm.\n", stdout);
	    }
	    break;
	case GSDLL_SYNC:
	    break;
	case GSDLL_PAGE:
	    break;
	case GSDLL_SIZE:
	    break;
	default:
	    sprintf(buf,"Callback: Unknown message=%d\n",message);
	    fputs(buf, stdout);
	    break;
    }
    return 0;
}


