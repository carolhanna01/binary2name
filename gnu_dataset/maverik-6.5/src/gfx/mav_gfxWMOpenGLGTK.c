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

typedef void (*MAV_frameFn)(void *);
void mav_frameFn4Add(MAV_frameFn fn, void *);
void *mav_malloc(int amount);
void mav_free(void *d);
#ifdef __cplusplus
}
#endif



#define MAX_WINS  10 /* Make sure the same or greater than equivalent in mav_kernel.h */
#define MAX_FONTS 10 /* Same as above */

extern int mav_opt_bindTextures;
extern int mav_opt_shareContexts;
extern int mav_opt_maxTextures;
extern char *mav_gfx_vendor;
extern char *mav_gfx_renderer;
extern char *mav_gfx_version;



#include <stdio.h>
#include <stdlib.h>

#include <GL/gl.h>
#include <gtk/gtk.h>
#include <gtkgl/gtkglarea.h>
#include <gdk/gdkkeysyms.h>

#if defined(GL_VERSION_1_1) || defined(GL_EXT_texture_object)
extern GLuint *mavlib_bindTextureIndex;
#endif



/* Data structures to store the GTK widget of each MAV_window */

typedef struct {
  GtkWidget *win;
  int quad;
  int quad_separate;
} MAVLIB_winhand;

MAVLIB_winhand mavlib_winhand[MAX_WINS];
int mavlib_currwin;
int mavlib_quadId=-1;

/* Data structures for event handling */

typedef struct MAVLIB_GTKEVENT {
  int rv;
  int info[10];
  struct MAVLIB_GTKEVENT *next;
} MAVLIB_gtkEvent;

MAVLIB_gtkEvent *mavlib_gtkEventStart=NULL;
MAVLIB_gtkEvent *mavlib_gtkEventHead=NULL;
MAVLIB_gtkEvent *mavlib_gtkEventTail=NULL;
int mavlib_gtkWinInit;



/* Routines to initialise the graphics module */

char *mav_gfxModuleID(void)
{
  return "Graphics (OpenGL and GTK)";
}

int mav_gfxModuleInit(void)
{
  int i, argc=0;

  /* add the new module */
  mav_moduleNew(mav_gfxModuleID);  

  /* Initialise data structure */
  for (i=0; i<MAX_WINS; i++) mavlib_winhand[i].win= (GtkWidget *) NULL;

  /* Init GTK */
  gtk_init(&argc, NULL);

  /* Make sure that OpenGL is supported */
  if (gdk_gl_query() == FALSE) {
    fprintf(stderr, "Error: GTK+ OpenGL area not supported!\n");
    exit(1);
  }

  /* Initialise the linked list to store GTK events */
  mavlib_gtkEventStart= (MAVLIB_gtkEvent *) mav_malloc(sizeof(MAVLIB_gtkEvent));
  mavlib_gtkEventHead= mavlib_gtkEventStart;
  mavlib_gtkEventTail= mavlib_gtkEventStart;

  return 1;
}



/* Callback functions for event handling */

void mavlib_gtkEventAdd(GtkWidget *widget, int rv, int *info)
{
  MAVLIB_gtkEvent *ev= (MAVLIB_gtkEvent *) mav_malloc(sizeof(MAVLIB_gtkEvent));
  int i;
  
  /* Copy event info */
  ev->rv= rv;
  if (info) for (i=0; i<10; i++) ev->info[i]= info[i];

  /* Find which window in occured in */
  ev->info[0]= -1;
  for (i=0; i<MAX_WINS; i++) {
    if (mavlib_winhand[i].win && mavlib_winhand[i].win==widget) ev->info[0]=i;
  }

  if (ev->info[0]==-1) {
    fprintf(stderr, "Error: failed to lookup widget %p\n", widget);
    exit(1);
  }

  /* Keep linked list of events */
  mavlib_gtkEventHead->next= ev;
  mavlib_gtkEventHead= ev;
}

gint mavlib_gtkExpose(GtkWidget *widget, GdkEventExpose *event) 
{
  if (mavlib_gtkWinInit)
  {
    mavlib_gtkEventAdd(widget, 6, NULL);
  }
  else
  {
    mavlib_gtkWinInit=1;
  }

  return TRUE;
}

gint mavlib_gtkConfig(GtkWidget *widget, GdkEventConfigure *event) 
{
  int info[10];

  info[1]= event->width;
  info[2]= event->height;

  mavlib_gtkEventAdd(widget, 3, info);

  return TRUE;
}

gint mavlib_gtkCrossing(GtkWidget *widget, GdkEventCrossing *event) 
{
  int info[10];

  if (event->type==GDK_ENTER_NOTIFY)
  {
    gtk_widget_grab_focus(widget);
    info[1]= 0;
  }
  else
  {
    info[1]= 1;
  }

  mavlib_gtkEventAdd(widget, 5, info);

  return TRUE;
}

void mavlib_gtkDealWithKey(GtkWidget *widget, GdkEventKey *event, int pressrel) 
{
  int info[10];

  /* TODO - get mouse pos at time of event */
  {
    int i, win=-1;

    for (i=0; i<MAX_WINS; i++) {
      if (mavlib_winhand[i].win && mavlib_winhand[i].win==widget) win=i;
    }

    if (win==-1) {
      fprintf(stderr, "Error: failed to lookup widget %p\n", widget);
      exit(1);
    }

    mav_gfxWindowPointerGet(win, &info[1], &info[2], &info[3], &info[4], NULL); 
  }

  info[5]= pressrel;
  info[6]= 0;

  if (event->length)
  {
    info[6]= (int) event->string[0];
  }
  else
  {
    switch (event->keyval) {
    case GDK_F1: info[6]= 300; break;
    case GDK_F2: info[6]= 301; break;
    case GDK_F3: info[6]= 302; break;
    case GDK_F4: info[6]= 303; break;
    case GDK_F5: info[6]= 304; break;
    case GDK_F6: info[6]= 305; break;
    case GDK_F7: info[6]= 306; break;
    case GDK_F8: info[6]= 307; break;
    case GDK_F9: info[6]= 308; break;
    case GDK_F10: info[6]= 309; break;
    case GDK_F11: info[6]= 310; break;
    case GDK_F12: info[6]= 311; break;
    case GDK_Up: info[6]= 312; break;
    case GDK_Down: info[6]= 313; break;
    case GDK_Left: info[6]= 314; break;
    case GDK_Right: info[6]= 315; break;
    case GDK_Page_Up: info[6]= 316; break;
    case GDK_Page_Down: info[6]= 317; break;
    case GDK_Shift_L: info[6]= 318; break;
    case GDK_Shift_R: info[6]= 319; break;
    case GDK_Alt_L: info[6]= 320; break;
    case GDK_Alt_R: info[6]= 321; break;
    case GDK_Meta_L: info[6]= 322; break;
    case GDK_Meta_R: info[6]= 323; break;
    case GDK_Home: info[6]= 324; break;
    case GDK_End: info[6]= 325; break;
    case GDK_Insert: info[6]= 326; break;
    case GDK_Control_L: info[6]= 327; break;
    case GDK_Control_R: info[6]= 328; break;
    case GDK_Caps_Lock: info[6]= 329; break;
    default: info[6]= event->keyval; break;
    }
  }

  if (event->state&0x1) 
  {
    info[7]=1;
  }
  else
  {
    info[7]=0;
  }

  if ((event->state>>2)&0x1)
  {
    info[8]=1;
  }
  else
  {
    info[8]=0;
  }

  if ((event->state>>3)&0x1)
  {
    info[9]=1;
  }
  else
  {
    info[9]=0;
  }

  mavlib_gtkEventAdd(widget, 1, info);
}

gint mavlib_gtkKP(GtkWidget *widget, GdkEventKey *event) 
{
  mavlib_gtkDealWithKey(widget, event, 0);
  return TRUE;
}

gint mavlib_gtkKR(GtkWidget *widget, GdkEventKey *event) 
{
  /* Filter out key repeats */
  GdkEvent *nextevent= gdk_event_get();

  if (nextevent) {
    if (nextevent->type==GDK_KEY_PRESS && nextevent->key.time==event->time &&
	nextevent->key.state==event->state && nextevent->key.keyval==event->keyval) 
    {
      gdk_event_free(nextevent);
      return TRUE;
    }
    else
    {
      gdk_event_put(nextevent);
      gdk_event_free(nextevent);
    }
  }

  mavlib_gtkDealWithKey(widget, event, 1);

  return TRUE;
}

void mavlib_gtkDealWithButton(GtkWidget *widget, GdkEventButton *event, int pressrel) 
{
  int info[10];

  info[1]= event->x;
  info[2]= event->y;
  info[3]= event->x_root;
  info[4]= event->y_root;
  info[5]= pressrel;
  info[6]= event->button;

  if (event->state&0x1) 
  {
    info[7]=1;
  }
  else
  {
    info[7]=0;
  }

  if ((event->state>>2)&0x1)
  {
    info[8]=1;
  }
  else
  {
    info[8]=0;
  }

  if ((event->state>>3)&0x1)
  {
    info[9]=1;
  }
  else
  {
    info[9]=0;
  }

  mavlib_gtkEventAdd(widget, 2, info);
}

gint mavlib_gtkBP(GtkWidget *widget, GdkEventButton *event) 
{
  mavlib_gtkDealWithButton(widget, event, 0);
  return TRUE;
}

gint mavlib_gtkBR(GtkWidget *widget, GdkEventButton *event) 
{
  mavlib_gtkDealWithButton(widget, event, 1);
  return TRUE;
}

gint mavlib_gtkDelete(GtkWidget *widget, GdkEventAny *event)
{
  exit(0);
  return TRUE;
}



/* The default GUI to display OpenGL canvas in */

void mavlib_defaultGUI(GtkWidget *mavarea, char *nm)
{
  GtkWidget *window;
   
  /* Main window */
  window= gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(window), nm);
  gtk_signal_connect(GTK_OBJECT(window), "delete_event", GTK_SIGNAL_FUNC(mavlib_gtkDelete), NULL);
  
  gtk_container_add(GTK_CONTAINER(window), mavarea);
  
  gtk_widget_show(mavarea);
  gtk_widget_show(window);
}



/* Function called once per frame to do GTK processing */

void mavlib_gtkUpdate(void *ignored)
{
  gtk_main_iteration_do(FALSE);
}



/* Routine to open a window and an OpenGL context */

void mav_gfxWindowOpen(int id, int x, int y, int width, int height, char *nm, char *disp, int wmplacement, int sb, int qb, int ms, int ab, int stenb, int desta, int *wret, int *hret)
{
  int attrib[50];
  int ac=7;

  attrib[0]= GDK_GL_RGBA;
  attrib[1]= GDK_GL_RED_SIZE;
  attrib[2]= 1;
  attrib[3]= GDK_GL_GREEN_SIZE;
  attrib[4]= 1;
  attrib[5]= GDK_GL_BLUE_SIZE;
  attrib[6]= 1;
  
  if (desta) {
    attrib[ac]= GDK_GL_ALPHA_SIZE; ac++;
    attrib[ac]= 1; ac++;
  }

  attrib[ac]= GDK_GL_DEPTH_SIZE; ac++;
  attrib[ac]= 1; ac++;

  if (!sb) {
    attrib[ac]= GDK_GL_DOUBLEBUFFER; ac++;
  }

  if (ab) {
    attrib[ac]= GDK_GL_ACCUM_RED_SIZE; ac++;
    attrib[ac]= 1; ac++;
    attrib[ac]= GDK_GL_ACCUM_GREEN_SIZE; ac++;
    attrib[ac]= 1; ac++;
    attrib[ac]= GDK_GL_ACCUM_BLUE_SIZE; ac++;
    attrib[ac]= 1; ac++;
    if (desta) {
      attrib[ac]= GDK_GL_ACCUM_ALPHA_SIZE; ac++;
      attrib[ac]= 1; ac++;
    }
  }

  if (stenb) {
    attrib[ac]= GDK_GL_STENCIL_SIZE; ac++;
    attrib[ac]= 1; ac++;
  }

  if (qb) 
  {
    if (qb==3) /* Make sure this is equal to MAV_STEREO_QUAD_BUFFERS_SEPARATE_Z defined in mav_windows.h */
    {
      mavlib_winhand[id].quad_separate= 1;
    }
    else
    {
      mavlib_winhand[id].quad_separate= 0;
    }

    if (mavlib_quadId==-1)
    {
      attrib[ac]= GDK_GL_STEREO; ac++;
      mavlib_quadId= id;
      mavlib_winhand[id].quad= -1;
    }
    else
    {
      mavlib_winhand[id].win= mavlib_winhand[mavlib_quadId].win;
      mavlib_winhand[id].quad= mavlib_quadId;
      mavlib_quadId= -1;

      *wret= width;
      *hret= height;

      mav_gfxWindowSet(id);

      return;
    }
  }
  else
  {
    mavlib_winhand[id].quad= 0;
  }

  attrib[ac]= GDK_GL_NONE;

  /* Create window and context */
  if (id==1 || !mav_opt_shareContexts)
  {
    mavlib_winhand[id].win= gtk_gl_area_new(attrib);
  }
  else
  {
    mavlib_winhand[id].win= gtk_gl_area_share_new(attrib, GTK_GL_AREA(mavlib_winhand[1].win));
  }

  if (!mavlib_winhand[id].win) {
    fprintf(stderr, "Error: couldn't get an RGB");
    if (desta) fprintf(stderr, "A");
    if (!sb) fprintf(stderr, ", double-buffered");
    if (ms) fprintf(stderr, ", multi-sampled");
    if (ab) fprintf(stderr, ", acculmation-buffered");
    if (stenb) fprintf(stderr, ", stencil-buffered");
    if (qb) fprintf(stderr, ", quad-buffered");
    fprintf (stderr, " visual\n");
    exit(1);
  }

  /* Indicate which events we are interested in receiving in  */
  gtk_widget_set_events(mavlib_winhand[id].win, GDK_EXPOSURE_MASK | GDK_STRUCTURE_MASK | GDK_ENTER_NOTIFY_MASK| GDK_LEAVE_NOTIFY_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK | GDK_KEY_PRESS_MASK | GDK_KEY_RELEASE_MASK);

  /* Set event callbacks */
  gtk_signal_connect(GTK_OBJECT(mavlib_winhand[id].win), "expose_event", GTK_SIGNAL_FUNC(mavlib_gtkExpose), NULL);
  gtk_signal_connect(GTK_OBJECT(mavlib_winhand[id].win), "configure_event", GTK_SIGNAL_FUNC(mavlib_gtkConfig), NULL);
  gtk_signal_connect(GTK_OBJECT(mavlib_winhand[id].win), "enter_notify_event", GTK_SIGNAL_FUNC(mavlib_gtkCrossing), NULL);
  gtk_signal_connect(GTK_OBJECT(mavlib_winhand[id].win), "leave_notify_event", GTK_SIGNAL_FUNC(mavlib_gtkCrossing), NULL);
  gtk_signal_connect(GTK_OBJECT(mavlib_winhand[id].win), "button_press_event", GTK_SIGNAL_FUNC(mavlib_gtkBP), NULL);
  gtk_signal_connect(GTK_OBJECT(mavlib_winhand[id].win), "button_release_event", GTK_SIGNAL_FUNC(mavlib_gtkBR), NULL);
  gtk_signal_connect(GTK_OBJECT(mavlib_winhand[id].win), "key_press_event", GTK_SIGNAL_FUNC(mavlib_gtkKP), NULL);
  gtk_signal_connect(GTK_OBJECT(mavlib_winhand[id].win), "key_release_event", GTK_SIGNAL_FUNC(mavlib_gtkKR), NULL);
  
  /* Size window */
  gtk_widget_set_usize(mavlib_winhand[id].win, width, height);
  *wret= width;
  *hret= height;
  
  /* Add created window to a GUI */
  if (disp)
  { 
    typedef void (*MAVLIB_GTKGUI)(GtkWidget *, char *nm);
    MAVLIB_GTKGUI dispfn= (MAVLIB_GTKGUI) disp;
    (*dispfn)(mavlib_winhand[id].win, nm);
  }
  else
  {
    mavlib_defaultGUI(mavlib_winhand[id].win, nm);
  }

  /* Wait for expose event */
  mavlib_gtkWinInit=0;
  while (!mavlib_gtkWinInit) gtk_main_iteration_do(FALSE);

  /* Set focus */
  GTK_WIDGET_SET_FLAGS(mavlib_winhand[id].win, GTK_CAN_FOCUS);
  gtk_widget_grab_focus(mavlib_winhand[id].win);

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

  /* GDK update poll */
  if (id==1) mav_frameFn4Add(mavlib_gtkUpdate, NULL);
}



/* Routine to set the current window */

void mav_gfxWindowSet(int i)
{
  gtk_gl_area_make_current(GTK_GL_AREA(mavlib_winhand[i].win));

  if (mavlib_winhand[i].quad) {
    if (mavlib_winhand[i].quad==-1) 
    {
      glDrawBuffer(GL_BACK_LEFT);
      if (!mavlib_winhand[i].quad_separate) glClear(GL_DEPTH_BUFFER_BIT);
    }
    else
    {
      glDrawBuffer(GL_BACK_RIGHT);
      if (!mavlib_winhand[i].quad_separate) glClear(GL_DEPTH_BUFFER_BIT);
    }
  }

  mavlib_currwin= i;
}



/* Routine to swap the buffers of the current window */

void mav_gfxWindowBuffersSwap(void)
{
  if (mavlib_winhand[mavlib_currwin].quad<=0) gtk_gl_area_swapbuffers(GTK_GL_AREA(mavlib_winhand[mavlib_currwin].win));
}



/* Routine to return the position of the mouse in a window and root coords */

int mav_gfxWindowPointerGet(int win, int *x, int *y, int *rx, int *ry, int *buts) 
{
  GdkWindow *w;
  GdkModifierType btnmask;
  int posx, posy, wx, wy;
  int rv=1;

  if (win>0 && win<MAX_WINS && mavlib_winhand[win].win)
  {
    w= gtk_widget_get_parent_window(mavlib_winhand[win].win);
    gdk_window_get_position(w, &posx, &posy);
    gdk_window_get_pointer(w, &wx, &wy, &btnmask);

    *rx= posx+wx;
    *ry= posy+wy;
    *x= wx-mavlib_winhand[win].win->allocation.x;
    *y= wy-mavlib_winhand[win].win->allocation.y;

    if (buts) {
      if (btnmask & GDK_BUTTON1_MASK) /* Left button */
      {
	buts[0]= 0; /* Pressed */
      }
      else
      {
	buts[0]= 1; /* Released */
      }

      if (btnmask & GDK_BUTTON2_MASK) /* Middle button */
      {
	buts[1]= 0; /* Pressed */
      }
      else
      {
	buts[1]= 1; /* Released */
      }

      if (btnmask & GDK_BUTTON3_MASK) /* Right button */
      {
	buts[2]= 0; /* Pressed */
      }
      else
      {
	buts[2]= 1; /* Released */
      }
    }
  }
  else
  {
    rv=0;
  }

  return rv;
}



/* Routine to close a window */

void mav_gfxWindowClose(int id)
{
  gdk_window_destroy(gtk_widget_get_parent_window(mavlib_winhand[id].win));
  mavlib_winhand[id].win= (GtkWidget *) NULL;
}



/* Routine to get the resolution of the display */

void mav_gfxWindowResGet(int *xres, int *yres)
{
  *xres= gdk_screen_width();
  *yres= gdk_screen_height();
}



/* Routine to read a 2D raster font and store as an OpenGL display list */

GLuint mavlib_fontBase[MAX_FONTS];

int mav_gfxWindowFontSet(char *s, int i, int *width)
{
  GdkFont *font;
  unsigned int j, first, last;

  first=0;
  last=255;

  font= gdk_font_load(s);
  if (!font) return -1;

  mavlib_fontBase[i] = glGenLists((GLuint) last+1);
  if (mavlib_fontBase[i] == 0) {
    /* gdk_font_unref(font); Causing problem with NVIDIA cards */
    return -2;
  }

  gdk_gl_use_gdk_font(font, first, last, mavlib_fontBase[i]);
  for (j=first; j<=last; j++) width[j]= gdk_char_width(font, j);

  /* gdk_font_unref(font); Causing problem with NVIDIA cards */

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



/* Routine to set the mouses position */

void mav_gfxWindowPointerSet(int win, int x, int y)
{
  /* TODO */
  fprintf(stderr, "Warning: cant set pointer under GTK\n");
}



/* Routine to return the state of a key */

int mav_gfxWindowKeyGet(int key)
{
  /* TODO */
  fprintf(stderr, "Warning: cant get a key under GTK\n");
  return 1;
}



/* Get next event returning data in info array */

int mav_gfxWindowEventGet(int *info)
{
  int i, rv;

  if (mavlib_gtkEventHead==mavlib_gtkEventTail)
  {
    return 0;
  }
  else
  {
    /* Note old event */
    MAVLIB_gtkEvent *oldev= mavlib_gtkEventTail;

    /* Move linked list on */
    mavlib_gtkEventTail= mavlib_gtkEventTail->next;

    /* Copy data */
    for (i=0; i<10; i++) info[i]= mavlib_gtkEventTail->info[i];
    rv= mavlib_gtkEventTail->rv + (mavlib_gtkEventTail->info[0]<<8);

    /* Delete old event */
    mav_free(oldev);

    return rv;
  }
}



/* Check if any events are outstanding */

int mav_gfxWindowEventPeek(void)
{
  if (mavlib_gtkEventHead==mavlib_gtkEventTail)
  {
    return 0;
  }
  else
  {
    return mavlib_gtkEventTail->next->rv + (mavlib_gtkEventTail->next->info[0]<<8);
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
