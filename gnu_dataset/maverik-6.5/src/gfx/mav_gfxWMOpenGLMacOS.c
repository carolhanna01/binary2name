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



#define MAX_WINS  10 /* Make sure the same or greater than equivalent in mav_kernel.h */
#define MAX_FONTS 10 /* Same as above */
#define MAX_DPYS   8

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

#include <agl.h>
#include <Quickdraw.h>
#include <Fonts.h>
#include <Windows.h>
#include <Events.h>
#include <MacWindows.h>

#if defined(GL_VERSION_1_1) || defined(GL_EXT_texture_object)
extern GLuint *mavlib_bindTextureIndex;
#endif



/* Data structure to store window and context of each MAV_window */

typedef struct {
  WindowPtr win;
  AGLContext ctx;
} MAVLIB_winhand;

MAVLIB_winhand mavlib_winhand[MAX_WINS];
int mavlib_currwin;
WindowPtr mavlib_rootwin;



/* Routine to convert from MacOS window into MAV_window id */ 

int mavlib_winlookup(WindowPtr w)
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
  /* shut down windows nicely */
  for (i=0; i<MAX_WINS; i++) {
    if (mavlib_winhand[i].win) {
       aglSetCurrentContext(NULL);
       aglSetDrawable(mavlib_winhand[i].ctx, NULL);
       aglDestroyContext(mavlib_winhand[i].ctx);
       DisposeWindow(mavlib_winhand[i].win);
    }
  } /* for */
}



/* Routines to initialise the graphics module */

char *mav_gfxModuleID(void)
{
  return "Graphics (OpenGL and MacOS)";
}

int mav_gfxModuleInit()
{
  for (i=0; i<MAX_WINS; i++) mavlib_winhand[i].win= (WindowPtr) NULL;

  /* add the new module */
  mav_moduleNew(mav_gfxModuleID);  
  
  /* Initialize Macintosh system */
  InitGraf(&qd.thePort);
  InitFonts();
  InitWindows();
  InitMenus();
  TEInit();
  InitDialogs(NULL);
  FlushEvents (everyEvent, 0);
  InitCursor();

  /* clean up on application exit */
  atexit(mavlib_gfxExit);

  return 1;
}



/* Routine to get the resolution of the display */

void mav_gfxWindowResGet(int *xres, int *yres)
{
  GDHandle gdev = GetMainDevice ();
  Rect bounds = (*gdev)->gdRect;
  *yres = bounds.bottom;
  *xres = bounds.right;
}



/* Routine to set the current window */

void mav_gfxWindowSet(int i)
{
  aglSetDrawable (mavlib_winhand[i].ctx, (CGrafPort *)mavlib_winhand[i].win);
  aglSetCurrentContext (mavlib_winhand[i].ctx);

  mavlib_currwin= i;
}



/* Routine to swap the buffers of the current window */

void mav_gfxWindowBuffersSwap(void)
{
  aglSwapBuffers (mavlib_winhand[mavlib_currwin].ctx);
}



/* Routine to read a 2D raster font and store as an OpenGL display list */

GLuint mavlib_fontBase[MAX_FONTS];

int mav_gfxWindowFontSet(char *s, int i, int *width)
{
  int j;
  mavlib_fontBase[i] = glGenLists((GLuint) 256);
  if (mavlib_fontBase[i] == 0) return -2;
  if (!aglUseFont (mavlib_winhand[mavlib_currwin].ctx, 4 /*monaco*/, 1 /*bold*/, 10, 0, 255, mavlib_fontBase[i])) {
    return -1;
  }
  for (j=0; j<=255; j++) width[j]= 10;

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



/* Routine to open a window and an OpenGL context */

void mav_gfxWindowOpen(int id, int x, int y, int width, int height, char *nm, char *disp, int wmplacement, int sb, int qb, int ms, int ab, int stenb, int desta, int *wret, int *hret)
{
  AGLDrawable win;
  AGLPixelFormat fmt;
  AGLContext ctx;
  Rect rect;
  Str255 title;
  GLint attrib[20];
  int i=0;

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

  for (i=19; i>=0; i--) {
  	attrib[i]=AGL_NONE;
  }
  i=0;
  attrib[i]=AGL_RGBA; i++;
  attrib[i]=AGL_MAXIMUM_POLICY; i++;
  if (!sb) {
  	attrib[i] = AGL_DOUBLEBUFFER; i++;
  }
  if (ab) {
  	attrib[i] = AGL_ACCUM_RED_SIZE; i++;
  	attrib[i] = 4; i++;
  	attrib[i] = AGL_ACCUM_GREEN_SIZE; i++;
  	attrib[i] = 4; i++;
  	attrib[i] = AGL_ACCUM_BLUE_SIZE; i++;
  	attrib[i] = 4; i++;
  	/*attrib[i] = AGL_ACCUM_ALPHA_SIZE; i++;
  	attrib[i] = 4; i++;*/
  }
  attrib[i] = AGL_DEPTH_SIZE; i++;
  attrib[i] = 16; i++;
#ifndef MAV_MACNOACC
  attrib[i] = AGL_ALL_RENDERERS; i++;
  attrib[i] = AGL_ACCELERATED; i++;
#endif  
  attrib[i] = AGL_NONE;

  strncpy ((char*)title+1, nm, 255);
  title[0] = strlen (nm);
  
  SetRect (&rect, x, y, width+x, height+y);
  win = (AGLDrawable) NewCWindow (0L, &rect, title, false, documentProc, (WindowPtr) -1L, false, 0L);
  if (!win) {
  	fprintf (stderr, "could not open window\n");
  	return;
  }
  ShowWindow ((GrafPort*) win);
  HiliteWindow ((GrafPort*) win, true);
  SetPort ((GrafPort*) win);
  fmt = aglChoosePixelFormat (NULL, 0, attrib);
  ctx = aglCreateContext (fmt, NULL);
  aglSetDrawable (ctx, win);
  aglSetCurrentContext (ctx);
  mavlib_winhand[id].win = (GrafPort*)win;
  mavlib_winhand[id].ctx = ctx;

  *wret = width; *hret = height;
  
  /* Set to active window */
  mav_gfxWindowSet(id);

  /* Get graphics vendor info */
  if (!mav_gfx_vendor) mav_gfx_vendor= (char *) glGetString(GL_VENDOR);
  if (!mav_gfx_renderer) mav_gfx_renderer= (char *) glGetString(GL_RENDERER);
  if (!mav_gfx_version) mav_gfx_version= (char *) glGetString(GL_VERSION);  

  if (id==1 && mav_opt_bindTextures) {
#if defined(GL_VERSION_1_1) || defined(GL_EXT_texture_object)
    mavlib_bindTextureIndex= malloc(mav_opt_maxTextures*3*sizeof(GLuint));
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
  if (mavlib_winhand[id].win) {
     aglSetCurrentContext(NULL);
     aglSetDrawable(mavlib_winhand[id].ctx, NULL);
     aglDestroyContext(mavlib_winhand[id].ctx);
     DisposeWindow(mavlib_winhand[id].win);
  }
}



/* 
   Check if any events are outstanding (do not block if there are not) 
   Return value gives the type of event.
*/

int mav_gfxWindowEventPeek(void)
{
  int rv=0;
  int winid=0;

  EventRecord event;
       
  if (EventAvail (everyEvent, &event)) {
    winid= mavlib_winlookup(FrontWindow ());
    switch (event.what) {
      case mouseDown:
      case mouseUp:
        rv=2;
        break;
      case autoKey:
      case keyDown:
      case keyUp:
        rv=1;
        break;
      case updateEvt:
        rv=6;
        break;
      case activateEvt:
        rv=5;
        break;
      case kHighLevelEvent:
      default:
        printf("unknown event %d\n", event.what);
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

int mavlib_macLastEvent[20];

int mav_gfxWindowEventGet(int *info)
{
  int rv=0;

  EventRecord event;
  
  if (GetOSEvent(everyEvent, &event)) {
    /* first check if the last event was a key press. if so, resend a key release event first. */
    if (mavlib_macLastEvent[11] == 1 && event.what != autoKey) {
       mavlib_macLastEvent[5]=1; /* set to key release */
       mavlib_macLastEvent[11] = 0;
       mavlib_dealWithKeyboardEvent(mavlib_macLastEvent);
       mavlib_macLastEvent[11]=0; /* reset */
    }
    switch (event.what) {
      case mouseDown:
      case mouseUp: {
 	    int onwhat;
		WindowPtr win;
		  
        info[0]= mavlib_winlookup(FrontWindow ());
		onwhat = FindWindow (event.where, &win);
		/*if (!win) return (rv + (info[0]<<8)) ;*/
		switch (onwhat) { /* test for window handling */
		    case inSysWindow:
			   SystemClick(&event, win);/* pass clicks to Desk Accessories */
			   return return (rv + (info[0]<<8));
		    case inMenuBar: {
		       short menuID, menuEntry;
		       long choosen = MenuSelect (event.where);
		       menuID = (choosen>>16)&0xffff;
		       menuEntry = choosen&0xffff;
		       if (menuID > 0) {
		         Str255	DAName;
		         GetMenuItemText (GetMenuHandle (menuID), menuEntry, DAName);
		         OpenDeskAcc (DAName);
		       }
		       HiliteMenu (0);
		       return (rv + (info[0]<<8));
		    }
		    case inContent:
		       if (info[0] != mavlib_winlookup (win)) {
		          SelectWindow (win);
		          return (rv + (info[0]<<8));
		       }
 	           break;
		    case inDrag:
		       if (info[0] != mavlib_winlookup (win)) {
		          SelectWindow (win);
		          return (rv + (info[0]<<8));
		       }
		       if (info[0]>0) DragWindow (win, event.where, &(*GetGrayRgn())->rgnBBox); return (rv + (info[0]<<8));
		       break;
		    case inGrow:
			   if (info[0]>0) {
			      long newsize;
			      RgnHandle region;
			      newsize = GrowWindow (win, event.where, &(*GetGrayRgn())->rgnBBox);
			      info[1] = newsize&0xffff;
			      info[2] = (newsize>>16) & 0xffff;
			      SizeWindow (win, info[1], info[2], true);
			      aglUpdateContext (mavlib_winhand[info[0]].ctx);
			      mavlib_dealWithResizeEvent (info);
			      region = ((WindowPeek)win)->strucRgn;
			      InvalRgn (region);
			      return 0; /* must update */
			   }
			case inGoAway:
			   if (info[0]>0) mav_windowDelete (info[0]); return (rv + (info[0]<<8));
		} /* switch onwhat */

	    info[1]= event.where.h - ((info[0]<=0)?0:
	    		(*(((WindowPeek)mavlib_winhand[info[0]].win)->contRgn))->rgnBBox.left);
	    info[2]= event.where.v - ((info[0]<=0)?0:
	    		(*(((WindowPeek)mavlib_winhand[info[0]].win)->contRgn))->rgnBBox.top);
	    info[3]= event.where.h;
	    info[4]= event.where.v;
	
	    if (event.what==mouseDown) info[5]=0;
	    if (event.what==mouseUp) info[5]=1;
	
		/*printf ("button pos x:%i y:%i rootx:%i rooty:%i\n", info[1],info[2],info[3],info[4]);*/
	/* set mouse button number */
	    if (event.modifiers & controlKey) {
	      info[6]=2; /* middle button when control held */
	    } else if (event.modifiers & optionKey) {
	      info[6]=3; /* right button when alt held */
	    } else {
	      info[6]=1; /* left button */
	    }  
        info[7]=event.modifiers & shiftKey; /* shift modifier */
        info[8]=event.modifiers & controlKey; /* control modifier */
        info[9]=event.modifiers & optionKey; /* alt modifier */
        rv=2;
        break;
      }
      case autoKey:
      /*case keyUp :*/ /* keyUp events are not always sent to the queue !!! */
      case keyDown: {
        char theKey = (event.message & keyCodeMask) >> 8;
        int theChar = event.message & charCodeMask;
        info[0]= mavlib_winlookup(FrontWindow ());
	    info[1]= event.where.h - ((info[0]<=0)?0:
	    		(*(((WindowPeek)mavlib_winhand[info[0]].win)->contRgn))->rgnBBox.left); /* should be root win */
	    info[2]= event.where.v - ((info[0]<=0)?0:
	    		(*(((WindowPeek)mavlib_winhand[info[0]].win)->contRgn))->rgnBBox.top); /* should be root win */
	    info[3]= event.where.h;
	    info[4]= event.where.v;
	
	    if (event.what==keyDown || event.what==autoKey) {
	        info[5]=0;
	    }
	    if (event.what==keyUp) {
	    	info[5]=1;
	    }
	    /*printf ("key code is %hi  the char is %i or %c\n", theKey, theChar, theChar);*/
	    
		switch (theKey) {
			case 0x7a: theChar = 300; break; /* XK_F1 */
			case 0x78: theChar = 301; break; /* XK_F2 */
			case 0x63: theChar = 302; break; /* XK_F4 */
			case 0x76: theChar = 303; break; /* XK_F5 */
			case 0x60: theChar = 304; break; /* XK_F5 */
			case 0x61: theChar = 305; break; /* XK_F6 */
			case 0x62: theChar = 306; break; /* XK_F7 */
			case 0x64: theChar = 307; break; /* XK_F8 */
			case 0x65: theChar = 308; break; /* XK_F9 */
			case 0x6d: theChar = 309; break; /* XK_F10 */
			case 0x67: theChar = 310; break; /* XK_F11 */
			case 0x6f: theChar = 311; break; /* XK_F12 */
			case 0x7e: theChar = 312; break; /* XK_Up */
			case 0x7d: theChar = 313; break; /* XK_Down */
			case 0x7b: theChar = 314; break; /* XK_Left */
			case 0x7c: theChar = 315; break; /* XK_Right */
			case 0x74: theChar = 316; break; /* XK_Page_Up */
			case 0x79: theChar = 317; break; /* XK_Page_Down */
			case 0x38: theChar = 318; break; /* XK_Shift_L */
			case 0x3c: theChar = 319; break; /* XK_Shift_R */
			case 0x3a: theChar = 320; break; /* XK_Alt_L */
			case 0x3d: theChar = 320; break; /* XK_Alt_R */
			case 0x37: theChar = 322; break; /* XK_Meta_L  == Apple Key */
			/* cannot distinguish the meta right from left on the Mac !! */
			case 0x73: theChar = 324; break; /* XK_Home */
			case 0x77: theChar = 325; break; /* XK_End */
			case 0x72: theChar = 326; break; /* XK_Insert */
			case 0x3b: theChar = 327; break; /* XK_Control_L */
			case 0x3e: theChar = 328; break; /* XK_Control_R */
			case 0x39: theChar = 329; break; /* XK_Caps_Lock */
			default: theKey = 0; break;
        }
        info[6]=theChar;
        info[7]=event.modifiers & shiftKey; /* shift modifier */
        info[8]=event.modifiers & controlKey; /* control modifier */
        info[9]=event.modifiers & optionKey; /* alt modifier */
        { int j;
          for (j=0; j<20; j++) { /* keep a copy */
            mavlib_macLastEvent[j] = info[j];
          }
          mavlib_macLastEvent[11] = 1; /* save this event identifier */
        }

        rv=1;
        break;
      }
      case updateEvt:
        info[0]= mavlib_winlookup((GrafPort*)event.message);
		info[1]=0;
        rv=6;
        break;
      case activateEvt:
        info[0]= mavlib_winlookup((GrafPort*)event.message);
        if (event.modifiers & activeFlag) {
          info[1]=1;
        } else {
          info[1]=0;
        }
        rv=5;
        break;
       case diskEvt:
	    if (event.message < 0) {
	       Point dialogpt = {100, 100};
		   DIBadMount(dialogpt, event.message);
	    }
	    break;
      case kHighLevelEvent:
      default:
        printf("unknown event %i\n", event.what);
        rv=-1;
        break;
    }
  	/*printf ("event is %i on %i\n", rv, info[0]);*/
    if (info[0]==-1) rv=-1; /* suppress events for others */
  }

  return (rv + (info[0]<<8));
}



/* Routine to return the position of the mouse in a window and root coords */

int mav_gfxWindowPointerGet(int win, int *x, int *y, int *rx, int *ry, int *buts)
{
  int rv=1;

  Point pt;
  RgnPtr rgn;
  GetMouse (&pt); /* get it in local coords */
  if (win > 0) {
	  rgn = *((WindowPeek)(mavlib_winhand[win].win))->strucRgn;
	  *x = pt.h; *y = pt.v;
	  *rx = pt.h+rgn->rgnBBox.left;
	  *ry = pt.v+rgn->rgnBBox.top;
      rv = 1;
  } else {
      rv = 0;
  }

  return rv;
}



/* Routine to set the mouses position */

void mav_gfxWindowPointerSet(int win, int x, int y)
{
  printf ("cannot set mouse pointer \n");
}



/* Routine to return the state of a key */

int mav_gfxWindowKeyGet(int key)
{
  unsigned int whatbyte;
  unsigned char bitmask;
  unsigned char theChar=0;
  unsigned char keys[16];
  GetKeys ((long*)keys);
  switch (key) {
		case 300: theChar =  0x7a; break; /* XK_F1 */
		case 301: theChar = 0x78; break; /* XK_F2 */
		case 302: theChar = 0x63; break; /* XK_F4 */
		case 303: theChar = 0x76; break; /* XK_F5 */
		case 304: theChar = 0x60; break; /* XK_F5 */
		case 305: theChar = 0x61; break; /* XK_F6 */
		case 306: theChar = 0x62; break; /* XK_F7 */
		case 307: theChar = 0x64; break; /* XK_F8 */
		case 308: theChar = 0x65; break; /* XK_F9 */
		case 309: theChar = 0x6d; break; /* XK_F10 */
		case 310: theChar = 0x67; break; /* XK_F11 */
		case 311: theChar = 0x6f; break; /* XK_F12 */
		case 312: theChar = 0x7e; break; /* XK_Up */
		case 313: theChar = 0x7d; break; /* XK_Down */
		case 314: theChar = 0x7b; break; /* XK_Left */
		case 315: theChar = 0x7c; break; /* XK_Right */
		case 316: theChar = 0x74; break; /* XK_Page_Up */
		case 317: theChar = 0x79; break; /* XK_Page_Down */
		case 318: theChar = 0x38; break; /* XK_Shift_L */
		case 319: theChar = 0x3c; break; /* XK_Shift_R */
		case 320: theChar = 0x3a; break; /* XK_Alt_L */
		case 321: theChar = 0x3d; break; /* XK_Alt_R */
		case 322:
		case 323: theChar = 0x37; break; /* XK_Meta_L  == Apple Key */
		case 324: theChar = 0x73; break; /* XK_Home */
		case 325: theChar = 0x77; break; /* XK_End */
		case 326: theChar = 0x72; break; /* XK_Insert */
		case 327: theChar = 0x3b; break; /* XK_Control_L */
		case 328: theChar = 0x3e; break; /* XK_Control_R */
		case 329: theChar = 0x39; break; /* XK_Caps_Lock */
		default: theChar = 0; break;
  }
  if (theChar>0) {
     whatbyte = (int)(theChar/8.0);
     bitmask = 1<<(theChar%8);
  /* a special key */
     return (keys[whatbyte] & bitmask)!=bitmask;
  } else {
     whatbyte = (int)(key/8.0);
     bitmask = 1<<(key%8);
  /* 7 bit ascii char */
     return (keys[whatbyte] & bitmask)!=bitmask;
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
