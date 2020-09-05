/*
   GNU Maverik - a system for managing display and interaction in 
              Virtual Environment applications.
   Copyright (C) 2008  Advanced Interfaces Group

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   The authors can be contacted via:
   www   - http://aig.cs.man.ac.uk
   email - maverik@aig.cs.man.ac.uk
   mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
        University of Manchester, Manchester, M13 9PL, UK
*/


/* 21/11/00 - added left/right shift/alt/ctrl          */
/* 08/11/00 - added font sizing and multiple font support */
/* font sizing needs update to src/kernel/mav_frame.c  */
/* minimum window resize                               */
/* tidy up loose handles on exit                       */
/* 29/10/00 - added enter/leave event handling         */
/*  (so mav_win_mouse gets correctly set)              */
/* changed from 5.4 - 27/1/2000, 18/10/00, 26/10/00    */
/* changes:                                            */
/* map/unmap event handled                             */
/* minor change to WM_PAINT event handling             */
/* single buffer and accum buffer flags work           */
/* windows share contexts                              */
/* expose event handled                                */
/* (most) punctuation chars reported correctly         */
/* mouse buttons polling works                         */
/* window size adjusted for title/border               */
/* uses mouse capture to avoid missing button ups      */
/* minor changes to window creation/event handling     */
/* events now return mouse position at time of event   */
/* fullscreen mode supported                           */

/* note for fullscreen:                                        */
/*   if mav_opt_fullscreen is set then ${HOME}/.mav_fullscreen */
/*   is read in for the width, height and bitdepth of the      */
/*   display mode, or if not found, the value MAVLIB_WIDTH,    */
/*   MAVLIB_HEIGHT and MAVLIB_BITS are used. If the display    */
/*   mode cannot be changed the application is run windowed.   */
/*   I tried to implement <ctrl>-F1 switching between windowed */
/*   and fullscreen modes but this requires changing the pixel */
/*   format and SetPixelFormat really does *not* like being    */
/*   called multiple times.                                    */


/* These are the only bits of Maverik we need. Keep interface to window
   manager as light as possible to make its easy for other implementation */

#ifdef MAVAPI
#undef MAVAPI
#endif
#if defined(WIN32) && !defined(__CYGWIN__) 
#ifdef LIBMAVERIK_EXPORTS
#define MAVAPI __declspec(dllexport) 
#else
#define MAVAPI __declspec(dllimport) 
#endif
#else
#define MAVAPI
#endif

#ifdef __cplusplus
extern "C" {
#endif
MAVAPI void mav_gfxWindowOpen(int id, int x, int y, int w, int h, char *name, char *disp, int wmp, int sb, int qb, int ms, int ab, int stenb, int desta, int *wret, int *hret);
MAVAPI void mav_gfxWindowClose(int id);
MAVAPI void mav_gfxWindowSet(int id);
MAVAPI void mav_gfxWindowBuffersSwap(void);
MAVAPI void mav_gfxWindowResGet(int *x, int *y);
MAVAPI int  mav_gfxWindowEventGet(int *info);
MAVAPI int  mav_gfxWindowEventPeek(void);
MAVAPI int  mav_gfxWindowPointerGet(int id, int *x, int *y, int *rx, int *ry, int *buts);
MAVAPI void mav_gfxWindowPointerSet(int win, int x, int y);
MAVAPI int  mav_gfxWindowKeyGet(int key);
MAVAPI int  mav_gfxWindowFontSet(char *s, int font, int *width);
MAVAPI void mav_gfxWindowStringDisplay(char *s, int font);
MAVAPI void mav_moduleNew(char *fn(void));
MAVAPI int  mav_gfxModuleInit(void);
MAVAPI char *mav_gfxModuleID(void);
MAVAPI void mav_gfx3DfxModeSet(int fullscreen);
MAVAPI int  mav_gfx3DfxBoardSet(int bd);
#ifdef __cplusplus
}
#endif



#define MAX_WINS 10 /* Make sure the same or greater than equivalent in mav_kernel.h */
#define MAX_FONTS 10 /* Same as above */
#define MAX_DPYS 8

extern int mav_opt_bindTextures;
extern int mav_opt_shareContexts;
extern int mav_opt_maxTextures;
extern int mavlib_voodoo;
extern void *mavlib_dlh;
extern char *mav_gfx_vendor;
extern char *mav_gfx_renderer;
extern char *mav_gfx_version;
extern int mav_opt_fullscreen;



#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <windows.h>
#include <GL/gl.h>

#if defined(GL_VERSION_1_1) || defined(GL_EXT_texture_object)
extern GLuint *mavlib_bindTextureIndex;
#endif



/* Certain features only available to Cygwin users */
#ifdef __CYGWIN__
#define TME 1
#endif



/* Defines for default fullscreen mode */
#define MAVLIB_WIDTH 320
#define MAVLIB_HEIGHT 240
#define MAVLIB_BITS 16

/* Font handles */
HFONT mavlib_fonts[MAX_FONTS];

/* Fullscreen mode variables */
int mavlib_gfx_width= -1;
int mavlib_gfx_height= -1;
int mavlib_gfx_bits= -1;
int mavlib_gfx_fullscreen= -1;

/* mav_win_mouse HWND */
HWND mavlib_hwnd= NULL;



/* Data structure to store window and context of each MAV_window */

typedef struct {
  HDC hdc;
  HWND win;
  HGLRC ctx;
  PIXELFORMATDESCRIPTOR pfd;
  int width;
  int height;
  int resized;
  int exposed;
  int mapped;
#ifdef TME 
  TRACKMOUSEEVENT tme;
#endif
  int enter;
  int leave;
} MAVLIB_winhand;

HINSTANCE mavlib_dpy;
MAVLIB_winhand mavlib_winhand[MAX_WINS];
int mavlib_currwin;

LONG WINAPI mavlib_winEventHandler(HWND hwnd, UINT umsg, WPARAM wparam, LPARAM lparam);



/* Routine to convert from Win32 window into MAV_window id */ 

int mavlib_winlookup(HWND w)
{
  int i;
  
  for (i=0; i<MAX_WINS; i++) {
    if (mavlib_winhand[i].win==w) return i;
  }

  return -1;
}



/* On exit command - shut down windows nicely */

void mavlib_gfxExit(void)
{
  int i;

  /* delete font handles */
  for (i=0; i<MAX_FONTS; i++) {
    if (mavlib_fonts[i]) DeleteObject (mavlib_fonts[i]);
  }

   /* shut down windows nicely */
  for (i=0; i<MAX_WINS; i++) {
    if (mavlib_winhand[i].win) {
      ReleaseDC (mavlib_winhand[i].win, mavlib_winhand[i].hdc);
      wglDeleteContext(mavlib_winhand[i].ctx);
      DestroyWindow(mavlib_winhand[i].win);
    }
  }
}



/* Routines to initialise the graphics module */

char *mav_gfxModuleID(void)
{
  return "Graphics (OpenGL and Windows)";
}

int mav_gfxModuleInit()
{
  WNDCLASS wc;
  int i;

  /* Initialise data structure */
  for (i=0; i<MAX_WINS; i++) mavlib_winhand[i].win= (HWND) NULL;

  /* reset font handles */
  for (i=0; i<MAX_FONTS; i++) mavlib_fonts[i]= NULL;

  /* add the new module */
  mav_moduleNew(mav_gfxModuleID);  

  /* Open connection to display */  
  mavlib_dpy = GetModuleHandle(NULL);
  if (!mavlib_dpy) {
    fprintf(stderr, "Error: cannot connect to screen\n");
    exit(1);
  }

  /* Register class */
  wc.style= CS_OWNDC; /* don't need CS_HREDRAW | CS_VREDRAW */
		/* CS_OWNDC is necessary for some cards */
  wc.lpfnWndProc= (WNDPROC) mavlib_winEventHandler;
  wc.cbClsExtra= 0;
  wc.cbWndExtra= 0;
  wc.hInstance= mavlib_dpy;
  wc.hIcon= 0;
  wc.hCursor= LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground= NULL;
  wc.lpszMenuName= NULL;
  wc.lpszClassName= "Maverik";
    
  if (!RegisterClass(&wc)) {
    fprintf(stderr, "Error: failed to register class\n");
    exit(1);
  }

  /* clean up on application exit */
  atexit(mavlib_gfxExit);

  return 1;
}



/* Routine to get the resolution of the display */

void mav_gfxWindowResGet(int *xres, int *yres)
{
  *xres= GetSystemMetrics(SM_CXSCREEN);
  *yres= GetSystemMetrics(SM_CYSCREEN);
}



/* Routine to set the current window */

void mav_gfxWindowSet(int i)
{
  wglMakeCurrent(mavlib_winhand[i].hdc, mavlib_winhand[i].ctx);

  mavlib_currwin= i;
}



/* Routine to swap the buffers of the current window */

void mav_gfxWindowBuffersSwap(void)
{
  SwapBuffers(mavlib_winhand[mavlib_currwin].hdc);
}



/* Routine to read a 2D raster font and store as an OpenGL display list */

GLuint mavlib_fontBase[MAX_FONTS];

int mav_gfxWindowFontSet(char *s, int i, int *width)
{
  unsigned int first, last;
  int pos= 1;
  int weight= FW_NORMAL;
  BOOL italic= FALSE;
  int family= FF_SWISS;
  int height= -1;
  char typeface[100];
  int tf= 0;

/* remove previous font */
  if (mavlib_fonts[i]) DeleteObject (mavlib_fonts[i]);

/* make a vague stab at a compatible font */

/* company */
  while (s[pos] != '-') pos ++;
  pos ++;

/* typeface - you never know, it might be on the system... */
  while (s[pos] != '-') {
    typeface[tf]= s[pos];
    tf ++;
    pos ++;
  }
  typeface[tf]= (char) NULL;
  pos ++;

/* weight */
  if (s[pos]=='b') weight= FW_BOLD;
  else if (s[pos]=='m') weight= FW_MEDIUM;
  while (s[pos] != '-') pos ++;
  pos ++;

/* slant */
  if (s[pos]=='i') italic= TRUE;
  if (s[pos]=='o' && s[pos+1] != 't')
    family= FF_MODERN;
  if (s[pos]=='r' && s[pos+1]=='-')
    family= FF_ROMAN;
  while (s[pos] != '-') pos ++;
  pos ++;

/* ignored */
  while (s[pos] != '-') pos ++;
  pos ++;

/* ignored */
  while (s[pos] != '-') pos ++;
  pos ++;

/* pixel height */
  if (s[pos] != '-' && s[pos] != '*') {
    sscanf (&s[pos], "%d", &height);
  }
  while (s[pos] != '-') pos ++;
  pos ++;

/* point height - only use if no pixel height */
  if (height==-1) {
    if (s[pos] != '-' && s[pos] != '*') {
      sscanf (&s[pos], "%d", &height);
      height /= 10;
    }
  }

/* no height defined then use default */
  if (height==-1) height= 0;

/* get the font (or a near match) */
  mavlib_fonts[i]= CreateFont (height, 0, 0, 0, weight, italic, FALSE, FALSE,
	ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
	DEFAULT_PITCH|family, typeface);
 
/* make it the current font */ 
  SelectObject (mavlib_winhand[mavlib_currwin].hdc, mavlib_fonts[i]);

/* generate display lists */
  first=0;
  last=255;

  mavlib_fontBase[i] = glGenLists((GLuint) last+1);
  if (mavlib_fontBase[i] == 0) return -2;

  wglUseFontBitmaps(mavlib_winhand[mavlib_currwin].hdc, first, last, mavlib_fontBase[i]+first);
  GetCharWidth32(mavlib_winhand[mavlib_currwin].hdc, first, last, width);

/* note - we don't release the font handle because it will be needed */
/* for text width calculations */

  return 0;
}



/* Routine to display a string in a 2D raster font */ 

void mav_gfxWindowStringDisplay(char *s, int font)
{
  glPushAttrib(GL_LIST_BIT);
  glListBase(mavlib_fontBase[font]);
  glCallLists(strlen(s), GL_UNSIGNED_BYTE, (GLubyte *)s);
  glPopAttrib();
}



/* Returns the width of the text in the given font - summing individual chars does not work */

int mav_gfxWindowStringLength(int win, char *s, int font)
{
  SIZE size;

  SelectObject (mavlib_winhand[win].hdc, mavlib_fonts[font]);
  GetTextExtentPoint32 (mavlib_winhand[win].hdc, s,
		strlen (s), &size);

  return size.cx;
}



/* Routine to create a OpenGL rendering context under Windows */

/* flags passed to mav_gfxWindowOpen */
int mavlib_sb= 0; /* single buffer */
int mavlib_ab= 0; /* accum buffer */

HGLRC mavlib_winSetUpGL(int win_id)
{
  int pixelFormat= 0;
  HGLRC rv = 0;

/* set the pfd for this window */
  mavlib_winhand[win_id].pfd.nSize= sizeof(PIXELFORMATDESCRIPTOR);
  mavlib_winhand[win_id].pfd.nVersion= 1;
  mavlib_winhand[win_id].pfd.dwFlags= PFD_SUPPORT_OPENGL|PFD_DRAW_TO_WINDOW;

/* double/single buffer */
  if (!mavlib_sb) mavlib_winhand[win_id].pfd.dwFlags |= PFD_DOUBLEBUFFER;

/* rgba type */
  mavlib_winhand[win_id].pfd.iPixelType= PFD_TYPE_RGBA;

/* colour depth - may need to change to get acceration from graphics card */
  mavlib_winhand[win_id].pfd.cColorBits= 16;

/* alpha buffer (not supported by generic implementation) */
  mavlib_winhand[win_id].pfd.cAlphaBits= 0;

/* accum buffer */
  if (mavlib_ab)
    mavlib_winhand[win_id].pfd.cAccumBits= 16;
  else
    mavlib_winhand[win_id].pfd.cAccumBits= 0;

/* z-buffer bits - may need to change to get
   acceleration from graphics card */
  mavlib_winhand[win_id].pfd.cDepthBits= 16; 

/* no stencil buffer */
  mavlib_winhand[win_id].pfd.cStencilBits= 0;

/* aux buffers (not supported by generic implementation) */
  mavlib_winhand[win_id].pfd.cAuxBuffers= 0;

/* layer plane (ignored?) */
  mavlib_winhand[win_id].pfd.dwLayerMask= PFD_MAIN_PLANE;

  pixelFormat= ChoosePixelFormat(mavlib_winhand[win_id].hdc, &mavlib_winhand[win_id].pfd);

  if (pixelFormat) {
    if (SetPixelFormat(mavlib_winhand[win_id].hdc, pixelFormat, &mavlib_winhand[win_id].pfd)) {
#if 0
      PIXELFORMATDESCRIPTOR pfdnew;

      DescribePixelFormat (mavlib_winhand[win_id].hdc, pixelFormat,
		sizeof (PIXELFORMATDESCRIPTOR), &pfdnew);
      if (pfdnew.dwFlags & PFD_GENERIC_FORMAT) fprintf (stderr, "Generic ICD\n");
      else fprintf (stderr, "Not generic ICD\n");
      fprintf (stderr, "%d colour bits\n", pfdnew.cColorBits);
      fprintf (stderr, "%d depth bits\n", pfdnew.cDepthBits);
#endif

/* create context */
      rv= wglCreateContext(mavlib_winhand[win_id].hdc);

/* share contexts */
      if (mav_opt_shareContexts && win_id!=1) {
	if (!wglShareLists(mavlib_winhand[1].ctx, rv)) {
	  wglDeleteContext(rv);
	  rv=0;
	}
      }

      if (rv) {
	if (!wglMakeCurrent(mavlib_winhand[win_id].hdc, rv)) {
	  wglDeleteContext(rv);
	  rv=0;
	}
      }
    }
  }

  return rv;
}



/* Function to get the mouse position associated with an event */

void mavlib_msgPointerGet(int win, int *x, int *y, int *rx, int *ry)
{
  DWORD mouse_pos;
  POINT point;

/* get the mouse position at the time of the last message */
  mouse_pos= GetMessagePos ();
  point.x= LOWORD(mouse_pos);
  point.y= HIWORD(mouse_pos);

  *rx= point.x;
  *ry= point.y;

  /* Get in coords of specified window */
  if (ScreenToClient(mavlib_winhand[win].win, &point)==0) {
    fprintf(stderr, "Error: failed to convert cursor pos\n");
    exit(1);
  }

  /* Store window cords */
  *x= point.x;
  *y= point.y;
}



/* This callback executed by DispatchMessage in mav_gfxWindowEventGet */

int mavlib_win32EventInfo[50]; /* Event info stored here */
int mavlib_win32EventGood=0;   /* Indicates type of event */
int mavlib_win32Create=-1;     /* Indicates id of window for create event */
int mavlib_win32MouseCaught= 0; /* count of capture requests */
/* button press windows */
HWND mavlib_win32MouseWin[4]= {NULL, NULL, NULL, NULL};
int mavlib_win32MouseEventsPending= 0; /* for flushing button up events */

LONG WINAPI mavlib_winEventHandler(HWND hwnd, UINT umsg, WPARAM wparam, LPARAM lparam) 
{
  LONG rv= 1;
  int v1=-1, v2=-1;
  LPMINMAXINFO mmi;
  char ch;

  /* Set event indicator to zero - not a managed event */
  mavlib_win32EventGood=0;

  switch (umsg) {
  case WM_GETMINMAXINFO:
/* this message is sent when the window is being resized */
    mmi= (LPMINMAXINFO) lparam;
/* make the (client) min track height 1 */
    mmi->ptMinTrackSize.y ++;
    return 0;
  case WM_CREATE: /* Create Window event */
    if (mavlib_win32Create!=-1) 
    {
      mavlib_winhand[mavlib_win32Create].hdc= GetDC(hwnd);
      mavlib_winhand[mavlib_win32Create].ctx= mavlib_winSetUpGL(mavlib_win32Create);

      if (!mavlib_winhand[mavlib_win32Create].ctx) {
	fprintf(stderr, "Error: failed to create context\n");
	exit(1);
      }

#ifdef TME 
      mavlib_winhand[mavlib_win32Create].tme.cbSize= sizeof (TRACKMOUSEEVENT);
      mavlib_winhand[mavlib_win32Create].tme.dwFlags= TME_LEAVE;
      mavlib_winhand[mavlib_win32Create].tme.hwndTrack= hwnd;
#endif
    }
    else
    {
      fprintf(stderr, "Error: unexpected create message\n");
      exit(1);
    }
    break;

  case WM_DESTROY:
/* restore display mode on exit */
    if (mavlib_gfx_fullscreen) {
      mavlib_gfx_fullscreen= 0;
      ChangeDisplaySettings (NULL, 0);
    }
    break;

  case WM_CLOSE: /* Pressed on close icon */
    if (mavlib_gfx_fullscreen) {
      mavlib_gfx_fullscreen= 0;
      ChangeDisplaySettings (NULL, 0);
    }
    exit(1);
    break;


  case WM_SYSKEYDOWN: /* Keyboard event - fill in info */
  case WM_KEYDOWN:
    if (v1==-1) v1=0;
  case WM_SYSKEYUP:
  case WM_KEYUP:
    if (v1==-1) v1=1;

    /* Bit 30 of lparam is set if key already held down */
    if (v1==0 && lparam & (1 << 30)) break;

    /* The return value of this event */
    mavlib_win32EventGood=1; 

    /* Get the id of the window in which the event occurred */
    mavlib_win32EventInfo[0]= mavlib_winlookup(hwnd);

    /* Get pointer position at time of message */
    mavlib_msgPointerGet(mavlib_win32EventInfo[0], &mavlib_win32EventInfo[1], &mavlib_win32EventInfo[2], &mavlib_win32EventInfo[3], &mavlib_win32EventInfo[4]);
 

    /* Pressed or released */
    mavlib_win32EventInfo[5]=v1;

    /* Get modifier status */
    if (GetKeyState(VK_SHIFT)<0) /* Shift key up/down */ 
    {
      mavlib_win32EventInfo[7]= 1; /* Pressed (which is represented by 0 elsewhere!) */
    }
    else
    {
      mavlib_win32EventInfo[7]= 0; /* Released */
    }

    if (GetKeyState(VK_CONTROL)<0) /* Ctrl key up/down */ 
    {
      mavlib_win32EventInfo[8]= 1;
    }
    else
    {
      mavlib_win32EventInfo[8]= 0;
    }
    
    if (GetKeyState(VK_MENU)<0) /* Alt key up/down */ 
    {
      mavlib_win32EventInfo[9]= 1;
    }
    else
    {
      mavlib_win32EventInfo[9]= 0;
    }

    /* Translate keycode into ASCII value or #defines */
    mavlib_win32EventInfo[6]=0;
    switch (wparam) {
    case VK_DELETE: mavlib_win32EventInfo[6]= 127; break;
    case VK_F1: mavlib_win32EventInfo[6]= 300; break;
    case VK_F2: mavlib_win32EventInfo[6]= 301; break;
    case VK_F3: mavlib_win32EventInfo[6]= 302; break;
    case VK_F4: mavlib_win32EventInfo[6]= 303; break;
    case VK_F5: mavlib_win32EventInfo[6]= 304; break;
    case VK_F6: mavlib_win32EventInfo[6]= 305; break;
    case VK_F7: mavlib_win32EventInfo[6]= 306; break;
    case VK_F8: mavlib_win32EventInfo[6]= 307; break;
    case VK_F9: mavlib_win32EventInfo[6]= 308; break;
    case VK_F10: mavlib_win32EventInfo[6]= 309; break;
    case VK_F11: mavlib_win32EventInfo[6]= 310; break;
    case VK_F12: mavlib_win32EventInfo[6]= 311; break;
    case VK_UP: mavlib_win32EventInfo[6]= 312; break;
    case VK_DOWN: mavlib_win32EventInfo[6]= 313; break;
    case VK_LEFT: mavlib_win32EventInfo[6]= 314; break;
    case VK_RIGHT: mavlib_win32EventInfo[6]= 315; break;
    case VK_PRIOR: mavlib_win32EventInfo[6]= 316; break;
    case VK_NEXT: mavlib_win32EventInfo[6]= 317; break;
    case VK_SHIFT:
	ch= (char) (lparam >> 16);
	if (ch==(char) MapVirtualKey (VK_SHIFT, 0))
	  mavlib_win32EventInfo[6]= 318; /* left shift */
	else
	  mavlib_win32EventInfo[6]= 319; /* right shift */
	break;
    case VK_MENU:
	if (lparam & (1<<24))
	  mavlib_win32EventInfo[6]= 321; /* right alt */
	else
	  mavlib_win32EventInfo[6]= 320; /* left alt */
	break;
    case VK_HOME: mavlib_win32EventInfo[6]= 324; break;
    case VK_END: mavlib_win32EventInfo[6]= 325; break;
    case VK_INSERT: mavlib_win32EventInfo[6]= 326; break;
    case VK_CONTROL:
	if (lparam & (1<<24)) {
	  mavlib_win32EventInfo[6]= 328; /* right ctrl */
	} else {
/* a left control is sent before some right alt events - check next */
/* message to see if we should ignore this one */
	  MSG key_msg;

	  if (PeekMessage (&key_msg, hwnd, WM_KEYFIRST, WM_KEYLAST,
			PM_NOREMOVE)) {
	    if ((key_msg.lParam & (1<<24)) && key_msg.wParam==VK_MENU) {
/* remove from event queue if it's a right alt event */
	      PeekMessage (&key_msg, hwnd, WM_KEYFIRST, WM_KEYLAST,
			PM_REMOVE);
	      mavlib_win32EventInfo[6]= 321; /* right alt */
	    } else {
	      mavlib_win32EventInfo[6]= 327; /* left ctrl */
	    }
	  } else {
	    mavlib_win32EventInfo[6]= 327; /* left ctrl */
	  }
	}
	break;
    case VK_CAPITAL: mavlib_win32EventInfo[6]= 329; break;
    default: mavlib_win32EventInfo[6]= MapVirtualKey(wparam, 2); break;
    }

    /* Windows reports everything as uppercase - compensate for this */
    if (mavlib_win32EventInfo[6]>='A' && mavlib_win32EventInfo[6]<='Z' && !mavlib_win32EventInfo[7]) mavlib_win32EventInfo[6]+=32;

    /* check for shift key and change punctuation characters */
    if (mavlib_win32EventInfo[7]) {
      switch (mavlib_win32EventInfo[6]) {
	case '`': mavlib_win32EventInfo[6]= '~'; break;
        case '1': mavlib_win32EventInfo[6]= '!'; break;
        case '2': mavlib_win32EventInfo[6]= '\"'; break;
        case '3': mavlib_win32EventInfo[6]= '#'; break;
        case '4': mavlib_win32EventInfo[6]= '$'; break;
        case '5': mavlib_win32EventInfo[6]= '%'; break;
        case '6': mavlib_win32EventInfo[6]= '^'; break;
        case '7': mavlib_win32EventInfo[6]= '&'; break;
        case '8': mavlib_win32EventInfo[6]= '*'; break;
        case '9': mavlib_win32EventInfo[6]= '('; break;
        case '0': mavlib_win32EventInfo[6]= ')'; break;
        case '-': mavlib_win32EventInfo[6]= '_'; break;
        case '=': mavlib_win32EventInfo[6]= '+'; break;
        case '\\': mavlib_win32EventInfo[6]= '|'; break;
        case '[': mavlib_win32EventInfo[6]= '{'; break;
        case ']': mavlib_win32EventInfo[6]= '}'; break;
        case ';': mavlib_win32EventInfo[6]= ':'; break;
        case '\'': mavlib_win32EventInfo[6]= '@'; break;
        case '#': mavlib_win32EventInfo[6]= '~'; break;
        case ',': mavlib_win32EventInfo[6]= '<'; break;
        case '.': mavlib_win32EventInfo[6]= '>'; break;
        case '/': mavlib_win32EventInfo[6]= '?'; break;
	default: /* do nothing */;
      }
    }

    /* No event if we cant translate keycode */
    if (mavlib_win32EventInfo[6]==0) mavlib_win32EventGood=0;

    /* End of event */
    mavlib_win32EventInfo[10]= -999;
    break;

  case WM_LBUTTONDOWN: /* Mouse button event - fill in info */
    if (v1==-1) v1=1;
    if (v2==-1) v2=0;
  case WM_MBUTTONDOWN:
    if (v1==-1) v1=2;
    if (v2==-1) v2=0;
  case WM_RBUTTONDOWN:
    if (v1==-1) v1=3;
    if (v2==-1) v2=0;
  case WM_LBUTTONUP:
    if (v1==-1) v1=1;
    if (v2==-1) v2=1;
  case WM_MBUTTONUP:
    if (v1==-1) v1=2;
    if (v2==-1) v2=1;
  case WM_RBUTTONUP:
    if (v1==-1) v1=3;
    if (v2==-1) v2=1;

/* catch the mouse to trap button up events */
    if (v2==0) { /* button pressed */
/* catch the mouse */
      if (!mavlib_win32MouseCaught) SetCapture (hwnd); 
      mavlib_win32MouseWin[v1]= hwnd; /* store the window for this event */
      mavlib_win32MouseCaught ++; /* keep count of number of captures */
    } else {
/* release the mouse */
      mavlib_win32MouseCaught --;
      mavlib_win32MouseWin[v1]= NULL;
      if (!mavlib_win32MouseCaught) {
        ReleaseCapture ();
      }
    }

  case 522: /* For some reason WM_MOUSEWHEEL is not defined for me */
    if (v1==-1) {
      short del= HIWORD(wparam);
      if (del>0)
      {
	v1=4; /* wheel up */
      }
      else
      {
	v1=5; /* wheel down */
      }
    }
    if (v2==-1) v2=0; /* consider it as a button press event although a release is never generated */

    /* The return value of this event */
    mavlib_win32EventGood=2;

    /* Get the id of the window in which the event occurred */
    mavlib_win32EventInfo[0]= mavlib_winlookup(hwnd);

    /* Get pointer position */
    mavlib_msgPointerGet(mavlib_win32EventInfo[0], &mavlib_win32EventInfo[1], &mavlib_win32EventInfo[2], &mavlib_win32EventInfo[3], &mavlib_win32EventInfo[4]);

    /* Pressed or released */
    mavlib_win32EventInfo[5]=v2;

    /* Which button */
    mavlib_win32EventInfo[6]=v1;

    /* Get modifier status */
    if (GetKeyState(VK_SHIFT)<0) /* Shift key up/down */ 
    {
      mavlib_win32EventInfo[7]= 1; /* Pressed (which is represented by 0 elsewhere!) */
    }
    else
    {
      mavlib_win32EventInfo[7]= 0; /* Released */
    }

    if (GetKeyState(VK_CONTROL)<0) /* Ctrl key up/down */ 
    {
      mavlib_win32EventInfo[8]= 1;
    }
    else
    {
      mavlib_win32EventInfo[8]= 0;
    }
    
    if (GetKeyState(VK_MENU)<0) /* Alt key up/down */ 
    {
      mavlib_win32EventInfo[9]= 1;
    }
    else
    {
      mavlib_win32EventInfo[9]= 0;
    }
    
    /* End of event */
    mavlib_win32EventInfo[10]=-999;
    break;
  case WM_MOUSEMOVE:
    if (hwnd != mavlib_hwnd) {
      v1= mavlib_winlookup (hwnd);
      mavlib_winhand[v1].enter= 1;
      mavlib_hwnd= hwnd;
    }
/* TrackMouseEvent anyway to catch crossing mess ups */
#ifdef TME
    TrackMouseEvent (&mavlib_winhand[v1].tme);
    break;
  case WM_MOUSELEAVE:
    mavlib_winhand[mavlib_winlookup(hwnd)].leave= 1;
/* reset mavlib_hwnd unless already changed */
/* (enter events of the next window may be processed first) */
    if (mavlib_hwnd==hwnd) mavlib_hwnd= NULL;
#endif
    break;
  case WM_SIZE:  /* Resize event - store in winhand for now */
    v1= mavlib_winlookup(hwnd);
    if (wparam==SIZE_MINIMIZED) {
      mavlib_winhand[v1].mapped= 2; /* unmap (+1) */
    } else {
      mavlib_winhand[v1].width= LOWORD(lparam);
      mavlib_winhand[v1].height= HIWORD(lparam);
      mavlib_winhand[v1].resized= 1;
    }
    break;
  case WM_QUERYOPEN: /* sent before opening an iconic window */
    v1= mavlib_winlookup(hwnd);
    mavlib_winhand[v1].mapped= 1; /* map (+1) */
    break;
  case WM_PAINT: /* expose event */
/* WM not happy unless you validate the update region */
/* NULL validates the whole window */
    ValidateRect (hwnd, NULL);
    v1= mavlib_winlookup(hwnd);
/* store for checking in mav_gfxWindowEventGet */
    mavlib_winhand[v1].exposed= 1;
    break;
  default: /* Let the system deal with all other events */
    rv= DefWindowProc(hwnd, umsg, wparam, lparam);
  }

  return rv;
}



/* Routine to open a window and an OpenGL context */

void mav_gfxWindowOpen(int id, int x, int y, int width, int height, char *nm, char *disp, int wmplacement, int sb, int qb, int ms, int ab, int stenb, int desta, int *wret, int *hret)
{
  RECT wr;

  if (mav_opt_fullscreen==1)
    mavlib_gfx_fullscreen= 1;
  else
    mavlib_gfx_fullscreen= 0;

  if (qb) {
    fprintf(stderr, "Quad buffer visuals not supported on this platform\n");
    exit(1);
  }

  if (ms) {
    fprintf(stderr, "Multisampled visuals not supported on this platform\n");
    exit(1);
  }

  if (stenb) {
    fprintf(stderr, "Stencil buffer visuals not supported on this platform\n");
    exit(1);
  }

  if (desta) {
    fprintf(stderr, "Destination alpha buffer visuals not supported on this platform\n");
    exit(1);
  }

  if (mavlib_gfx_fullscreen) {
/* change display mode */
    DEVMODE dmSS;
    FILE *fp;
    char fname[1000];

/* get mode parameters */
    if (mavlib_gfx_width==-1||mavlib_gfx_height==-1||
		mavlib_gfx_bits==-1) {
/* try from file first */
      sprintf (fname, "%s/.mav_fullscreen", getenv("HOME"));
      fp= fopen (fname,"r");
      if (fp) {
	fscanf (fp, "%d %d %d", &mavlib_gfx_width, &mavlib_gfx_height,
		&mavlib_gfx_bits);
        fclose (fp);
      }
    }

    if (mavlib_gfx_width==-1||mavlib_gfx_height==-1||
		mavlib_gfx_bits==-1) {
/* usr defaults */
      mavlib_gfx_width= MAVLIB_WIDTH;
      mavlib_gfx_height= MAVLIB_HEIGHT;
      mavlib_gfx_bits= MAVLIB_BITS;
    }

    memset (&dmSS,0,sizeof(DEVMODE));
    dmSS.dmSize= sizeof(DEVMODE);
    dmSS.dmPelsWidth= mavlib_gfx_width;
    dmSS.dmPelsHeight= mavlib_gfx_height;
    dmSS.dmBitsPerPel= mavlib_gfx_bits;
    dmSS.dmFields= DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;

    if (ChangeDisplaySettings (&dmSS, CDS_FULLSCREEN)!=DISP_CHANGE_SUCCESSFUL) {
      fprintf (stderr, "Couldn't change to fullscreen\n");
      mavlib_gfx_fullscreen= 0;
    } else {
      x= 0;
      y= 0;
      width= mavlib_gfx_width;
      height= mavlib_gfx_height;
    }
  }

/* assume we'll get the correct window size */
/* probably ought to check it (after the initial resize event) */
  *wret= width;
  *hret= height;

/* get window size with borders so we get a client area of the desired size */
  if (!mavlib_gfx_fullscreen) {
    wr.left= x;
    wr.right= width+x;
    wr.top= y;
    wr.bottom= height+y;

    if (AdjustWindowRect (&wr, WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_OVERLAPPEDWINDOW, FALSE)) {
      x= wr.left;
      y= wr.top;
      width= wr.right-x;
      height= wr.bottom-y;
    }
  }

/* set values for WM_CREATE event */
  mavlib_win32Create=id;
  mavlib_sb= sb;
  mavlib_ab= ab;

  if (mavlib_gfx_fullscreen) {
    mavlib_winhand[id].win= CreateWindowEx(WS_EX_TOPMOST, "Maverik", nm,
		 WS_POPUP,
                 0,0, width, height,
                 NULL, NULL, mavlib_dpy, NULL);
  } else {
    mavlib_winhand[id].win= CreateWindow ("Maverik", nm,
		WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_OVERLAPPEDWINDOW,
		x, y, width, height,
		NULL, NULL, mavlib_dpy, NULL);
  }

  mavlib_win32Create=-1;

/* set event values */
  mavlib_winhand[id].width=-1;
  mavlib_winhand[id].height=-1;
  mavlib_winhand[id].resized=0;
  mavlib_winhand[id].exposed= 0;
  mavlib_winhand[id].mapped= 0;
  mavlib_winhand[id].enter= 0;
  mavlib_winhand[id].leave= 0;

  if (!mavlib_winhand[id].win) {
    fprintf(stderr, "Error: Couldn't open window!\n");
    exit(1);
  }

  ShowWindow(mavlib_winhand[id].win, SW_SHOWDEFAULT);
  UpdateWindow(mavlib_winhand[id].win);
  SetFocus(mavlib_winhand[id].win);

/* at this point a resize event will have occurred but (hopefully) */
/* to the size that was originally chosen */

  /* Set to active window */
  mav_gfxWindowSet(id);

  /* Get graphics vendor info */
  if (!mav_gfx_vendor) mav_gfx_vendor= (char *) glGetString(GL_VENDOR);
  if (!mav_gfx_renderer) mav_gfx_renderer= (char *) glGetString(GL_RENDERER);
  if (!mav_gfx_version) mav_gfx_version= (char *) glGetString(GL_VERSION);  

  if (id==1 && mav_opt_bindTextures) {
#if defined(GL_VERSION_1_1) || defined(GL_EXT_texture_object)
    mavlib_bindTextureIndex= (GLuint *) malloc(mav_opt_maxTextures*3*sizeof(GLuint));
    if (!mavlib_bindTextureIndex) fprintf(stderr, "Warning: bind texture malloc failed, ignoring.\n");
#ifdef GL_VERSION_1_1
    glGenTextures(mav_opt_maxTextures*3, mavlib_bindTextureIndex);
#else
    glGenTexturesEXT(mav_opt_maxTextures*3, mavlib_bindTextureIndex);
#endif
#else
    fprintf(stderr, "Warning: no bind texture extension, ignoring.\n");
#endif
  }
}



/* Routine to close a window */

void mav_gfxWindowClose(int id)
{
  wglDeleteContext(mavlib_winhand[id].ctx);
  DeleteDC (mavlib_winhand[id].hdc);
  DestroyWindow(mavlib_winhand[id].win);
  mavlib_winhand[id].win= (HWND) NULL;
}



/* 
   Check if any events are outstanding (do not block if there are not) 
   Return value gives the type of event.
*/

int mav_gfxWindowEventPeek(void)
{
  int rv=0;
  int winid=0;

  /* TODO - peek event, is this needed? */

  return (rv + (winid<<8));
}



/* 
   Get next event returning data in info array (again, do not block if there are no event) 
   Return value gives the type of event.
*/

int mav_gfxWindowEventGet(int *info)
{
  int rv=0;
  MSG msg;
  int i=0;

  /* Look for mapping, resize, expose, enter or leave events */
  for (i=0; i<MAX_WINS; i++) {
    if (mavlib_winhand[i].win && mavlib_winhand[i].mapped) {
      info[0]= i;
      info[1]= mavlib_winhand[i].mapped - 1;
      mavlib_winhand[i].mapped= 0;
      return (4 + (info[0]<<8)); /* map/unmap event */
    }
    if (mavlib_winhand[i].win && mavlib_winhand[i].resized) {
      info[0]= i;
      info[1]= mavlib_winhand[i].width;
      info[2]= mavlib_winhand[i].height;
      mavlib_winhand[i].resized=0;
      return (3 + (info[0]<<8)); /* Indicates a resize event */
    }
    if (mavlib_winhand[i].win && mavlib_winhand[i].exposed) {
      info[0]= i;
      mavlib_winhand[i].exposed= 0;
      return (6 + (info[0]<<8)); /* Indicates an expose event */
    }
    if (mavlib_winhand[i].win && mavlib_winhand[i].enter) {
      info[0]= i;
      mavlib_winhand[i].enter= 0;
      info[1]= 0;
      return (5 + (info[0]<<8)); /* Indicates an enter event */
    }
    if (mavlib_winhand[i].win && mavlib_winhand[i].leave) {
      info[0]= i;
      mavlib_winhand[i].leave= 0;
      info[1]= 1;
      return (5 + (info[0]<<8)); /* Indicates a leave event */
    }
  }

/* check for released mouse capture */
/* this section isn't really necessary unless there are other processes */
/* running which capture the mouse (or you press the windows key) */
  if (mavlib_win32MouseEventsPending || mavlib_win32MouseCaught) {
    if (!GetCapture ()) {
/* mouse no longer caught - simulate button up event for all pressed buttons */
      int i;
      int not_got_one= 1;

      mavlib_win32MouseCaught= 0;

      for (i=1; not_got_one && i<4; i++) {
	if (mavlib_win32MouseWin[i]) {
	  not_got_one= 0;
	  info[0]= mavlib_winlookup (mavlib_win32MouseWin[i]);
	  mav_gfxWindowPointerGet (info[0], &info[1], &info[2], &info[3],
			&info[4], NULL);
	  info[5]= 1; /* released */
	  info[6]= i; /* button */
/* get modifier status */
	  if (GetKeyState (VK_SHIFT)<0)
	    info[7]= 1;
	  else
	    info[7]= 0;

	  if (GetKeyState (VK_CONTROL)<0)
	    info[8]= 1;
	  else
	    info[8]= 0;

	  if (GetKeyState (VK_MENU)<0)
	    info[9]= 1;
	  else
	    info[9]= 0;

	  info[10]= -999;
/* reset mouse win */
	  mavlib_win32MouseWin[i]= NULL;
	}
      }

      if (not_got_one) mavlib_win32MouseEventsPending= 0;
      else {
	mavlib_win32MouseEventsPending= 1;
	return (2 + (info[0]<<8)); /* button event */
      }
    }
  }
/* end of mouse-capture-maybe-not-necessary section */


  /* Get next event - non blocking */
  if (PeekMessage(&msg, 0, 0, 0, PM_REMOVE)) {	

    /* Deal with event - this calls the mavlib_winEventHandler callback fn */
/* no point in calling TranslateMessage as Maverik interprets virtual keys */
/* and it just sends an extra (ignored) message to the queue */
    DispatchMessage(&msg);
      
    /* Is this an event dealt with my Maverik - value set by callback fn */
    if (mavlib_win32EventGood) {

      /* TODO - msg has a better pointer position info */
/* this is now handled properly in mavlib_winEventHandler */

      /* Copy values filled in by callback fn into info */
      i=0;
      while (mavlib_win32EventInfo[i]!=-999) {
	info[i]=mavlib_win32EventInfo[i];
	i++;
	rv=mavlib_win32EventGood;
      }
    }
  }

  return (rv + (info[0]<<8));
}



/* Routine to return the position of the mouse in a window and root coords */

int mav_gfxWindowPointerGet(int win, int *x, int *y, int *rx, int *ry, int *buts)
{
  int rv=1;
  POINT point;

  if (win>0 && win<MAX_WINS && mavlib_winhand[win].win) 
  {
    if (GetCursorPos(&point)==0) {
      fprintf(stderr, "Error: failed to get cursor pos\n");
      exit(1);
    }

    /* Store root cords */
    *rx= point.x;
    *ry= point.y;
  
    /* Get in coords of specified window */
    if (ScreenToClient(mavlib_winhand[win].win, &point)==0) {
      fprintf(stderr, "Error: failed to convert cursor pos\n");
      exit(1);
    }

    /* Store window cords */
    *x= point.x;
    *y= point.y;

/* get mouse button status */
    if (buts) {
      if (GetKeyState (VK_LBUTTON)<0) buts[0]= 0;
      else buts[0]= 1;
      if (GetKeyState (VK_MBUTTON)<0) buts[1]= 0;
      else buts[1]= 1;
      if (GetKeyState (VK_RBUTTON)<0) buts[2]= 0;
      else buts[2]= 1;
     }
  }
  else
  {
    rv=0;
  }

  return rv;
}



/* Routine to set the mouses position */

void mav_gfxWindowPointerSet(int win, int x, int y)
{
  POINT point;
  
  /* Point in coords of specified window */
  point.x= x;
  point.y= y;

  /* Get in coords of root window */
  if (ClientToScreen(mavlib_winhand[win].win, &point)==0) {
    fprintf(stderr, "Error: failed to convert cursor pos\n");
    exit(1);
  }

  /* Set cursor pos */
  SetCursorPos(point.x, point.y);
}



/* Routine to return the state of a key */

int mav_gfxWindowKeyGet(int key)
{
  int vk;

  /* Convert to keycode */
  switch (key) {
  case 300: vk= VK_F1; break;
  case 301: vk= VK_F2; break;
  case 302: vk= VK_F3; break;
  case 303: vk= VK_F4; break;
  case 304: vk= VK_F5; break;
  case 305: vk= VK_F6; break;
  case 306: vk= VK_F7; break;
  case 307: vk= VK_F8; break;
  case 308: vk= VK_F9; break;
  case 309: vk= VK_F10; break;
  case 310: vk= VK_F11; break;
  case 311: vk= VK_F12; break;
  case 312: vk= VK_UP; break;
  case 313: vk= VK_DOWN; break;
  case 314: vk= VK_LEFT; break;
  case 315: vk= VK_RIGHT; break;
  case 316: vk= VK_PRIOR; break;
  case 317: vk= VK_NEXT; break;
  case 318: vk= VK_SHIFT; break;
  case 319: vk= VK_SHIFT; break;
  case 320: vk= VK_MENU; break;
  case 321: vk= VK_MENU; break;
  case 324: vk= VK_HOME; break;
  case 325: vk= VK_END; break;
  case 326: vk= VK_INSERT; break;
  case 327: vk= VK_CONTROL; break;
  case 328: vk= VK_CONTROL; break;
  case 329: vk= VK_CAPITAL; break;
  default: vk= key; break;
  }

  /* Get key state */
  if (GetKeyState(vk)<0)
  {
    return 0; /* Pressed */
  }
  else
  {
    return 1; /* Released */
  }
}



/* Routines specific to Voodoo */

void mav_gfx3DfxModeSet(int fullscreen)
{
}

int mav_gfx3DfxBoardSet(int bd)
{
  int rv=0;

  return rv;
}
