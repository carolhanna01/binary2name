/* Copyright (C) 1996, Russell Lang.  All rights reserved.
  Portions Copyright (C) 1999 Aladdin Enterprises.  All rights reserved.
  
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


// $RCSfile: dwdll.h,v $ $Revision: 1.2.2.1 $

// gsdll_class for MS-Windows

#ifndef dwdll_INCLUDED
#  define dwdll_INCLUDED

extern "C" {
#include "gsdll.h"
#include "gsdllwin.h"
}

class gsdll_class {
    // instance of caller
    HINSTANCE hinstance;
    // handle to DLL.  Non-zero of loaded.
    HINSTANCE hmodule;
    // handle to parent window.  Can be NULL.
    HWND hwnd;
    // text description of last error
    char last_error[128];
    // true if init and execute_begin have been called
    BOOL initialized;
    // return code from last c_execute_end
    int execute_code;

    // pointer to callback from DLL
    GSDLL_CALLBACK callback;

    // pointers to DLL functions
    PFN_gsdll_revision c_revision;
    PFN_gsdll_init c_init;
    PFN_gsdll_execute_begin c_execute_begin;
    PFN_gsdll_execute_cont c_execute_cont;
    PFN_gsdll_execute_end c_execute_end;
    PFN_gsdll_exit c_exit;
    PFN_gsdll_lock_device c_lock_device;
    PFN_gsdll_copy_dib c_copy_dib;
    PFN_gsdll_copy_palette c_copy_palette;
    PFN_gsdll_draw c_draw;

    // pointer to os2dll or mswindll device
    // this needs to be extended to support multiple devices
    // also need to have one window per device
    char FAR *device;


        public:
    // Load DLL
    // Arguments:
    //   instance of calling EXE
    //   name of DLL, may include path
    //   expected version number of DLL
    // Returns:
    //   zero on success
    //   non-zero on error.  Error message available from get_last_error()
    // do nothing if DLL already loaded
    int load(const HINSTANCE hinstance, const char *name, const long version);

    // Get revision number of DLL
    int revision(char FAR * FAR *, char FAR * FAR *, long FAR *, long FAR *);

    // Unload DLL
    int unload(void);

    // Initialise DLL
    // Arguments:
    //   pointer to C callback function
    //   window handle of parent
    //   argc  (normal C command line)
    //   argv  (normal C command line)
    int init(GSDLL_CALLBACK callback, HWND hwnd, int argc, char FAR * FAR * argv);

    // Restart DLL
    int restart(int argc, char FAR * FAR * argv);

    // Execute string
    int execute(const char FAR *, int len);

    // Get last error string
    int get_last_error(char *str, int len);

    // lock device
    int gsdll_class::lock_device(const char FAR * device, int lock);

    // draw bitmap
    int gsdll_class::draw(const char FAR * device, HDC hdc, int dx, int dy, int wx, int wy, int sx, int sy);

    // copy bitmap
    HGLOBAL gsdll_class::copy_dib(const char FAR * device);

    // copy palette
    HPALETTE gsdll_class::copy_palette(const char FAR * device);
};

#endif /* dwdll_INCLUDED */
