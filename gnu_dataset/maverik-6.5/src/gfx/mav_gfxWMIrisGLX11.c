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


/* These are the only bits of Maverik we need. Keep interface to window
   manager as light as possible to make its easy for other implementation */

#ifdef __cplusplus
extern "C" {
#endif
void mav_gfxWindowOpen(int id, int x, int y, int w, int h, char *name, char *disp, int wmp, int sb, int qb, int ms, int ab, int stenb, int desta, int *wret, int *hret);
void mav_gfxWindowClose(int id);
void mav_gfxWindowSet(int id);
void mav_gfxWindowBuffersSwap(void);
void mav_gfxWindowResGet(int *x, int *y);
int  mav_gfxWindowEventGet(int *info);
int  mav_gfxWindowEventPeek(void);
int  mav_gfxWindowPointerGet(int id, int *x, int *y, int *rx, int *ry, int *buts);
void mav_gfxWindowPointerSet(int win, int x, int y);
int  mav_gfxWindowKeyGet(int key);
int  mav_gfxWindowFontSet(char *s, int font, int *width);
void mav_gfxWindowStringDisplay(char *s, int font);
void mav_moduleNew(char *fn(void));
int  mav_gfxModuleInit(void);
char *mav_gfxModuleID(void);
void mav_gfx3DfxModeSet(int fullscreen);
int  mav_gfx3DfxBoardSet(int bd);
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



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include <X11/keysym.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <gl/glws.h>



/* Data structures to store window and context of each MAV_window */

typedef struct {
  char *name;
  Display *dpy;
  Window rootwin;
  int scrnum;
} MAVLIB_dpyhand;

typedef struct {
  int dpy;
  Window win;
  int quad;
  int quad_separate;
} MAVLIB_winhand;

MAVLIB_dpyhand mavlib_dpy[MAX_DPYS];
MAVLIB_winhand mavlib_winhand[MAX_WINS];
int mavlib_currwin;
int mavlib_quadId=-1;



/* Routine to convert from X Window into MAV_window id */ 

int mavlib_winlookup(Window w)
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
}



/* Routines to open a X display */

void mavlib_XOpen(int id, char *nm)
{
  mavlib_dpy[id].dpy= XOpenDisplay(nm);

  if (!mavlib_dpy[id].dpy) {
    fprintf(stderr, "Error: cannot connect to X server %s\n", XDisplayName(nm));
    exit(1);
  }

  mavlib_dpy[id].scrnum= DefaultScreen(mavlib_dpy[id].dpy);
  mavlib_dpy[id].rootwin= RootWindow(mavlib_dpy[id].dpy, mavlib_dpy[id].scrnum);

  if (nm) {
    mavlib_dpy[id].name= (char *) malloc(sizeof(char)*(strlen(nm)+1));
    if (!mavlib_dpy[id].name) {
      fprintf(stderr, "Error: failed to malloc X server name %s\n", nm);
      exit(1);
    }
    strcpy(mavlib_dpy[id].name, nm);
  }
}

int mavlib_XLookup(char *nm)
{
  if (nm)
  {
    int i;

    /* Look for name */
    for (i=1; i<MAX_DPYS; i++) {
      if (mavlib_dpy[i].dpy && !strcmp(mavlib_dpy[i].name, nm)) return i;
    }    

    /* Not found, open it */
    for (i=1; i<MAX_DPYS; i++) {
      if (!mavlib_dpy[i].dpy) {
	mavlib_XOpen(i, nm);
	return i;	  
      }
    }

    fprintf(stderr, "Error: maximum number of displays exceeded\n");
    exit(1);

    return 0;
  }
  else
  {
    return 0;
  }
}



/* Routines to initialise the graphics module */

char *mav_gfxModuleID(void)
{
  return "Graphics (IrisGL and X11)";
}

int mav_gfxModuleInit()
{
  char *voodoo;
  int i;

  /* Initialise data structure */
  for (i=0; i<MAX_WINS; i++) mavlib_winhand[i].win= (Window) NULL;
  for (i=0; i<MAX_DPYS; i++) mavlib_dpy[i].dpy= (Display *) NULL;

  /* add the new module */
  mav_moduleNew(mav_gfxModuleID);  

  /* Open connection to display */  
  mavlib_XOpen(0, NULL);

  /* use environment variable to look for a Voodoo card */
  voodoo= getenv("MESA_GLX_FX");
  if (voodoo) {
    if (!strcmp(voodoo, "f")) mavlib_voodoo= 1;
    if (!strcmp(voodoo, "fullscreen")) mavlib_voodoo= 1;
  }

  return 1;
}



/* Routine to get the resolution of the display */

void mav_gfxWindowResGet(int *xres, int *yres)
{
  *xres= DisplayWidth(mavlib_dpy[0].dpy, mavlib_dpy[0].scrnum);
  *yres= DisplayHeight(mavlib_dpy[0].dpy, mavlib_dpy[0].scrnum);
}



/* Routine to set the current window */

void mav_gfxWindowSet(int i)
{
  GLXwinset(mavlib_dpy[mavlib_winhand[i].dpy].dpy, mavlib_winhand[i].win);
  mavlib_currwin= i;
}



/* Routine to swap the buffers of the current window */

void mav_gfxWindowBuffersSwap(void)
{
  swapbuffers();
}



/* Routine to read a 2D raster font and store as a display list (OpenGL only) */

int mav_gfxWindowFontSet(char *s, int i, int *width)
{
  return 0;
}



/* Routine to display a string in a 2D raster font */ 

void mav_gfxWindowStringDisplay(char *s, int font)
{
  charstr(s);
}



/* Routine to open a window and an IrisGL context */

void mav_gfxWindowOpen(int id, int x, int y, int width, int height, char *nm, char *disp, int wmplacement, int sb, int qb, int ms, int ab, int stenb, int desta, int *wret, int *hret)
{
  XVisualInfo *visinfo=NULL;
  Colormap cmap;
  XSetWindowAttributes attr;
  XWindowAttributes winattr;
  XTextProperty tp;
  XSizeHints sh;
  XClassHint ch;
  XEvent e;
  int attr_flags;
  GLXconfig params[3] = {{GLX_NORMAL, GLX_RGB, TRUE}, {GLX_NORMAL, GLX_DOUBLE, TRUE}, {0, 0, 0}};
  GLXconfig *next, *retconfig;
  XVisualInfo template;
  int nret;

  fprintf(stderr, "\n*** IrisGL is no longer actively supported\n");
  fprintf(stderr, "*** Graphics functionality may differ from the OpenGL version\n\n");

  mavlib_winhand[id].dpy= mavlib_XLookup(disp);

  if (sb) params[1].arg=FALSE;

  if ((retconfig = GLXgetconfig(mavlib_dpy[mavlib_winhand[id].dpy].dpy, mavlib_dpy[mavlib_winhand[id].dpy].scrnum, params)) == NULL) {
    fprintf(stderr, "Error: couldn't get an RGBA, Double-buffered visual\n");
    exit(1);
  }

  /*
   * Scan through config info, pulling info needed to create a window
   * that supports the rendering mode.
   */

  for (next = retconfig; next->buffer; next++) {
    if (next->buffer == GLX_NORMAL) {
      if (next->mode == GLX_COLORMAP) {
        cmap = next->arg;
      }
      else if (next->mode == GLX_VISUAL) {
        template.visualid = next->arg;
        template.screen = DefaultScreen(mavlib_dpy[mavlib_winhand[id].dpy].dpy);
        visinfo = XGetVisualInfo(mavlib_dpy[mavlib_winhand[id].dpy].dpy, VisualScreenMask | VisualIDMask, &template, &nret);
      }
    }
  }

  cmap=XCreateColormap(mavlib_dpy[mavlib_winhand[id].dpy].dpy, mavlib_dpy[mavlib_winhand[id].dpy].rootwin, visinfo->visual, AllocNone);

  /* set window attributes */
  attr.colormap=cmap;
  attr.event_mask= ExposureMask | StructureNotifyMask | KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask | EnterWindowMask | LeaveWindowMask;
  attr.border_pixel=BlackPixel(mavlib_dpy[mavlib_winhand[id].dpy].dpy, mavlib_dpy[mavlib_winhand[id].dpy].scrnum);
  attr.background_pixel=BlackPixel(mavlib_dpy[mavlib_winhand[id].dpy].dpy, mavlib_dpy[mavlib_winhand[id].dpy].scrnum);
  attr_flags = CWColormap | CWEventMask | CWBorderPixel | CWBackPixel;

   /* Create the window */
  mavlib_winhand[id].win = XCreateWindow(mavlib_dpy[mavlib_winhand[id].dpy].dpy, mavlib_dpy[mavlib_winhand[id].dpy].rootwin, x,y, width, height, 0, visinfo->depth, InputOutput, visinfo->visual, attr_flags, &attr);

  if (!mavlib_winhand[id].win) {
    fprintf(stderr, "Error: Couldn't open window!\n");
    exit(1);
  }
  
  /* Set its name and resource class */
  ch.res_name= NULL;
  ch.res_class= "MaverikApp";

  XStringListToTextProperty(&nm, 1, &tp);
  XSetWMProperties(mavlib_dpy[mavlib_winhand[id].dpy].dpy, mavlib_winhand[id].win, &tp, &tp, 0, 0, 0, 0, &ch);
  XFree(tp.value);

  /* Reposition window to requested position (window manager may not comply) */
  if (!wmplacement) {
    sh.flags = USPosition | USSize;
    XSetWMProperties(mavlib_dpy[mavlib_winhand[id].dpy].dpy, mavlib_winhand[id].win, 0, 0, 0, 0, &sh, 0, 0);
  }
  
  /* map window and wait notify event - i.e. its now visible  */
  XMapWindow(mavlib_dpy[mavlib_winhand[id].dpy].dpy, mavlib_winhand[id].win);

  do {
    XNextEvent(mavlib_dpy[mavlib_winhand[id].dpy].dpy, &e);
  } while (e.type!=MapNotify || e.xmap.window!=mavlib_winhand[id].win);
  
  /* get actual width and height of window (again pesky window managers) */
  XGetWindowAttributes(mavlib_dpy[mavlib_winhand[id].dpy].dpy, mavlib_winhand[id].win, &winattr);
  *wret= winattr.width;
  *hret= winattr.height;
  
  /* create graphics context */

  /*
   * Rescan configuration info and find window slot that getconfig
   * provided.  Fill it in with the window we just created.
   */
  for (next = retconfig; next->buffer; next++) {
    if ((next->buffer == GLX_NORMAL) && (next->mode == GLX_WINDOW)) {
      next->arg = mavlib_winhand[id].win;
      break;
    }
  }

  if (GLXlink(mavlib_dpy[mavlib_winhand[id].dpy].dpy, retconfig) < 0) {
    fprintf(stderr, "Error: could not link with the GL\n");
    exit(1);
  }  
  
  /* Set to active window */
  mav_gfxWindowSet(id);

  {
    /* define texturing  environment for IrisGL */
    float tevprops1[]= {TV_DECAL, TV_NULL};
    float tevprops2[]= {TV_MODULATE, TV_NULL};
    tevdef(1, 0, tevprops1);
    tevdef(2, 0, tevprops2);

    /* we'll have a bit of sub pixel accuracy */
    subpixel(TRUE);
  }
}



/* Routine to close a window */

void mav_gfxWindowClose(int id)
{
  XDestroyWindow(mavlib_dpy[mavlib_winhand[id].dpy].dpy, mavlib_winhand[id].win);
  mavlib_winhand[id].win= (Window) NULL;
}



/* 
   Check if any events are outstanding (do not block if there are not) 
   Return value gives the type of event.
*/

int mav_gfxWindowEventPeek(void)
{
  int rv=0;
  int winid=0;
  XEvent event;

  if (XEventsQueued(mavlib_dpy[0].dpy, QueuedAfterReading)) {

    /* Look at, but dont remove, the next event */
    XPeekEvent(mavlib_dpy[0].dpy, &event);

    /* get the id of the window in which the event occurred */
    winid= mavlib_winlookup(event.xany.window);

    switch(event.type) {
    case KeyPress:
    case KeyRelease:
      rv=1;
      break;
    case ButtonPress:
    case ButtonRelease:
      rv=2;
      break;
    case ConfigureNotify:
      rv=3;
      break;
    case MapNotify:
    case UnmapNotify:
      rv=4;
      break;
    case EnterNotify:
    case LeaveNotify:
      rv=5;
      break;
    case Expose:
      rv=6;
      break;
    default:
      printf("unknown event %i\n", event.type);
      rv=-1;
      break;
    }
  }

  return (rv + (winid<<8));
}



/* 
   Get next event returning data in info array (again, do not block if there are no event) 
   Return value gives the type of event.
*/

int (*mavlib_extraXEventHandler)(XEvent)= NULL;

int mav_gfxWindowEventGet(int *info)
{
  XEvent event;
  KeySym ks;
  char tmp;
  int rv=0;

  if (XEventsQueued(mavlib_dpy[0].dpy, QueuedAfterReading)) {

    /* Get next event - N.B. this is a blocking call */      
    XNextEvent(mavlib_dpy[0].dpy, &event);

    /* get the id of the window in which the event occurred */
    info[0]= mavlib_winlookup(event.xany.window);

    /* only consider events for active Maverik windows */
    if (info[0]!=-1) {

      switch(event.type) {

      case KeyRelease:
        /* Filter out key repeats */
        if(XPending(mavlib_dpy[0].dpy)) {
          XEvent nextevent;

          XPeekEvent(mavlib_dpy[0].dpy, &nextevent);
          if(nextevent.type == KeyPress &&
             nextevent.xkey.time == event.xkey.time &&
             nextevent.xkey.state == event.xkey.state &&
             nextevent.xkey.keycode == event.xkey.keycode) {
            /* Ignore KeyPress/KeyRelease pair */
            XNextEvent(mavlib_dpy[0].dpy, &nextevent);
            break;
          }
        }
	/* Fall through */

      case KeyPress:
	rv=1;
	
	info[1]= event.xkey.x;
	info[2]= event.xkey.y;
	info[3]= event.xkey.x_root;
	info[4]= event.xkey.y_root;
	
	if (event.type==KeyPress) info[5]=0;
	if (event.type==KeyRelease) info[5]=1;
	
	/* Convert key to ASCII or symbolic value */
	
	info[6]=0;
	
	if (XLookupString(&event.xkey, &tmp, sizeof(tmp), &ks, NULL)) 
	{
	  info[6]= (int) tmp;
	}
	else
	{
	  switch (ks) {
	  case XK_F1: info[6]= 300; break;
	  case XK_F2: info[6]= 301; break;
	  case XK_F3: info[6]= 302; break;
	  case XK_F4: info[6]= 303; break;
	  case XK_F5: info[6]= 304; break;
	  case XK_F6: info[6]= 305; break;
	  case XK_F7: info[6]= 306; break;
	  case XK_F8: info[6]= 307; break;
	  case XK_F9: info[6]= 308; break;
	  case XK_F10: info[6]= 309; break;
	  case XK_F11: info[6]= 310; break;
	  case XK_F12: info[6]= 311; break;
	  case XK_Up: info[6]= 312; break;
	  case XK_Down: info[6]= 313; break;
	  case XK_Left: info[6]= 314; break;
	  case XK_Right: info[6]= 315; break;
#ifndef MAV_SUNOS4
	  case XK_Page_Up: info[6]= 316; break;
	  case XK_Page_Down: info[6]= 317; break;
#endif
	  case XK_Shift_L: info[6]= 318; break;
	  case XK_Shift_R: info[6]= 319; break;
	  case XK_Alt_L: info[6]= 320; break;
	  case XK_Alt_R: info[6]= 321; break;
	  case XK_Meta_L: info[6]= 322; break;
	  case XK_Meta_R: info[6]= 323; break;
	  case XK_Home: info[6]= 324; break;
	  case XK_End: info[6]= 325; break;
	  case XK_Insert: info[6]= 326; break;
	  case XK_Control_L: info[6]= 327; break;
	  case XK_Control_R: info[6]= 328; break;
	  case XK_Caps_Lock: info[6]= 329; break;
	  }
	}

	if (info[6]==0) rv=0;

	if (event.xkey.state&0x1)  /* Shift key up/down */ 
	{
	  info[7]= 1; /* Pressed (which is represented by 0 elsewhere!) */
	}
	else
	{
	  info[7]= 0; /* Released */
	}

	if ((event.xkey.state>>2)&0x1)  /* Ctrl key up/down */ 
	{
	  info[8]= 1;
	}
	else
	{
	  info[8]= 0;
	}

	if ((event.xkey.state>>3)&0x1)  /* Alt key up/down */ 
	{
	  info[9]= 1;
	}
	else
	{
	  info[9]= 0;
	}
	break;

      case ButtonPress:
      case ButtonRelease:
	rv=2;

	info[1]= event.xbutton.x;
	info[2]= event.xbutton.y;
	info[3]= event.xbutton.x_root;
	info[4]= event.xbutton.y_root;
	
	if (event.type==ButtonPress) info[5]=0;
	if (event.type==ButtonRelease) info[5]=1;
	info[6]= event.xbutton.button;

	if (event.xkey.state&0x1)  /* Shift key up/down */ 
	{
	  info[7]= 1; /* Pressed (which is represented by 0 elsewhere!) */
	}
	else
	{
	  info[7]= 0; /* Released */
	}
	
	if ((event.xkey.state>>2)&0x1)  /* Ctrl key up/down */ 
	{
	  info[8]= 1;
	}
	else
	{
	  info[8]= 0;
	}

	if ((event.xkey.state>>3)&0x1)  /* Alt key up/down */ 
	{
	  info[9]= 1;
	}
	else
	{
	  info[9]= 0;
	}
	break;

      case ConfigureNotify:
	rv=3;
	info[1]=event.xconfigure.width;
	info[2]=event.xconfigure.height;
	break;

      case MapNotify:
      case UnmapNotify:
	rv=4;
	if (event.type==MapNotify) info[1]=0;
	if (event.type==UnmapNotify) info[1]=1;
	break;

      case EnterNotify: 
	rv=5;
	info[1]=0;
	break;

      case LeaveNotify:
	rv=5;
	info[1]=1;
	break;

      case Expose:
	rv=6;
	break;

      default:
	if (mavlib_extraXEventHandler) 
	{
	  rv= (*mavlib_extraXEventHandler)(event);
	}
	else
	{
	  rv=-1;
	  fprintf(stderr, "unknown event %i\n", event.type);
	}
      }
    }
  }

  return (rv + (info[0]<<8));
}



/* Routine to return the position of the mouse in a window and root coords */

int mav_gfxWindowPointerGet(int win, int *x, int *y, int *rx, int *ry, int *buts)
{
  Window dumroot, dumchild;
  unsigned int btnmask;
  int rv=1;

  if (win>0 && win<MAX_WINS && mavlib_winhand[win].win) 
  {
    XQueryPointer(mavlib_dpy[mavlib_winhand[win].dpy].dpy, mavlib_winhand[win].win, &dumroot, &dumchild, rx, ry, x, y, &btnmask);

    if (buts) {
      buts[0]= !((btnmask>>8) & 0x1); /* left */
      buts[1]= !((btnmask>>9) & 0x1); /* middle */
      buts[2]= !((btnmask>>10) & 0x1); /* right */
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
  XWarpPointer(mavlib_dpy[mavlib_winhand[win].dpy].dpy, None, mavlib_winhand[win].win, 0,0,0,0, x, y);
}



/* Routine to return the state of a key */

int mav_gfxWindowKeyGet(int key)
{
  char rv[32], ip[2];
  int keysym=-1, keycode, c, r;

  /* Convert ASCII key or symbolic value to keysym */ 
  if (key>=33 && key<255) 
  {
    ip[0]=key;
    ip[1]=0;
    keysym=XStringToKeysym(ip);
  }
  else
  {
    switch (key) {
    case 8: keysym=XK_BackSpace; break;
    case 9: keysym=XK_Tab; break;
    case 10: keysym=XK_Return; break;
    case 27: keysym=XK_Escape; break;
    case 32: keysym=XK_space; break;
    case 300: keysym=XK_F1; break;
    case 301: keysym=XK_F2; break;
    case 302: keysym=XK_F3; break;
    case 303: keysym=XK_F4; break;
    case 304: keysym=XK_F5; break;
    case 305: keysym=XK_F6; break;
    case 306: keysym=XK_F7; break;
    case 307: keysym=XK_F8; break;
    case 308: keysym=XK_F9; break;
    case 309: keysym=XK_F10; break;
    case 310: keysym=XK_F11; break;
    case 311: keysym=XK_F12; break;
    case 312: keysym=XK_Up; break;
    case 313: keysym=XK_Down; break;
    case 314: keysym=XK_Left; break;
    case 315: keysym=XK_Right; break;
#ifndef MAV_SUNOS4
    case 316: keysym=XK_Page_Up; break;
    case 317: keysym=XK_Page_Down; break;
#endif
    case 318: keysym=XK_Shift_L; break;
    case 319: keysym=XK_Shift_R; break;
    case 320: keysym=XK_Alt_L; break;
    case 321: keysym=XK_Alt_R; break;
    case 322: keysym=XK_Meta_L; break;
    case 323: keysym=XK_Meta_R; break;
    case 324: keysym=XK_Home; break;
    case 325: keysym=XK_End; break;
    case 326: keysym=XK_Insert; break;
    case 327: keysym=XK_Control_L; break;
    case 328: keysym=XK_Control_R; break;
    case 329: keysym=XK_Caps_Lock; break;
    default: fprintf(stderr, "Warning: unknown key symbol %i\n", key);
    }
  }
    
  keycode= XKeysymToKeycode(mavlib_dpy[0].dpy, keysym);
     
  XQueryKeymap(mavlib_dpy[0].dpy, rv);

  c= keycode/8;
  r= keycode-c*8;

  if (1&(rv[c]>>r)) 
    return 0;
  else
    return 1;
}



/* Routines specific to Voodoo */

void mav_gfx3DfxModeSet(int fullscreen)
{
  typedef unsigned char (*FN)(int);
  FN fn;
  
  /* Look for XMesaSetFXmode function */
  fn= (FN) dlsym(mavlib_dlh, "XMesaSetFXmode");

  if (fn)
  {
    /* Execute function with correct parameters - may change in future */
    if (fullscreen)
    {
      fn(2);
    }
    else
    {
      fn(1);
    }
  }
  else
  {
    fprintf(stderr, "Warning: cound not find XMesaSetFXmode function, ignoring\n");
  }
}

int mav_gfx3DfxBoardSet(int bd)
{
  int rv=0;
  typedef unsigned char (*FN)(int);
  FN fn;
  
  /* Look for fxMesaSelectCurrentBoard function */
  fn= (FN) dlsym(mavlib_dlh, "fxMesaSelectCurrentBoard");

  if (fn) 
  {
    /* Execute function with correct parameters - may change in future */
    rv= fn(bd);
  }
  else
  {
    fprintf(stderr, "Warning: cound not find fxMesaSelectCurrentBoard function, ignoring\n");
  }

  return rv;
}
