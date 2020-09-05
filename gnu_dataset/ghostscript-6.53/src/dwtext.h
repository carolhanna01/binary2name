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


// $RCSfile: dwtext.h,v $ $Revision: 1.2.2.1 $
// Text Window class


#ifdef _WINDOWS
#define _Windows
#endif


/* ================================== */
/* text window class */


class TextWindow {
    HINSTANCE hInstance;	/* required */
    LPSTR Title;		/* required */
    HICON hIcon;		/* optional */

    BYTE FAR *ScreenBuffer;
    POINT ScreenSize;		/* optional */
    char *DragPre;		/* optional */
    char *DragPost;		/* optional */
    int nCmdShow;		/* optional */
    HWND hwnd;

    BYTE FAR *KeyBuf;
    BYTE FAR *KeyBufIn;
    BYTE FAR *KeyBufOut;
    unsigned int KeyBufSize;
    BOOL quitnow;

    BOOL bFocus;
    BOOL bGetCh;
    char *fontname;		// font name

    int fontsize;		// font size in pts

    HFONT hfont;
    int CharAscent;

    int CaretHeight;
    int CursorFlag;
    POINT CursorPos;
    POINT ClientSize;
    POINT CharSize;
    POINT ScrollPos;
    POINT ScrollMax;

    void error(char *message);
    void new_line(void);
    void update_text(int count);
    void drag_drop(HDROP hdrop);
    void copy_to_clipboard(void);

         public:
    // constructor
         TextWindow(void);

    // destructor
        ~TextWindow(void);

    // register window class
    int register_class(HINSTANCE hinst, HICON hicon);

    // test if a key has been pressed
    // return TRUE if key hit
    // return FALSE if no key
    int kbhit(void);

    // Get a character from the keyboard, waiting if necessary
    int getch(void);

    // Get a line from the keyboard
    // store line in buf, with no more than len characters
    // including null character
    // return number of characters read
    int gets(LPSTR buf, int len);

    // Get a line from the keyboard
    // store line in buf, with no more than len characters
    // line is not null terminated
    // return number of characters read
    int read_line(LPSTR buf, int len);

    // Put a character to the window
    int putch(int ch);

    // Write cnt character from buf to the window
    void write_buf(LPSTR buf, int cnt);

    // Put a string to the window
    void puts(LPSTR str);

    // Scroll window to make cursor visible
    void to_cursor(void);

    // Create and show window with given name and min/max/normal state
    // return 0 on success, non-zero on error
    int create(LPSTR title, int cmdShow);

    // Destroy window
    int destroy(void);

    // Set window font and size
    // a must choose monospaced 
    void font(const char *fontname, int fontsize);

    // Set screen size in characters
    void size(int width, int height);

    // Set pre drag and post drag strings
    // If a file is dropped on the window, the following will
    // be poked into the keyboard buffer: 
    //   the pre_drag string
    //   the file name
    //   the post_drag string
    void drag(const char *pre_drag, const char *post_drag);

    // member window procedure
    LRESULT WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam);

    // provide access to window handle
    HWND get_handle(void) {
	return hwnd;
    };
};

/* ================================== */
