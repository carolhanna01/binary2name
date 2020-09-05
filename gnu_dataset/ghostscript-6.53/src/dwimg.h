/* Copyright (C) 1996, Russell Lang.  All rights reserved.
  
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


// $RCSfile: dwimg.h,v $ $Revision: 1.2.2.1 $

// Image Window class

class ImageWindow {
    static ImageWindow *first;
    ImageWindow *next;

    HWND hwnd;
    char FAR *device;		// handle to Ghostscript device

    int width, height;

    // Window scrolling stuff
    int cxClient, cyClient;
    int cxAdjust, cyAdjust;
    int nVscrollPos, nVscrollMax;
    int nHscrollPos, nHscrollMax;

    void register_class(void);

         public:
    static HINSTANCE hInstance;	// instance of EXE

    static HWND hwndtext;	// handle to text window

    friend ImageWindow *FindImageWindow(char FAR * dev);
    void open(char FAR * dev);
    void close(void);
    void sync(void);
    void page(void);
    void size(int x, int y);
    void create_window(void);
    LRESULT WndProc(HWND, UINT, WPARAM, LPARAM);
};
