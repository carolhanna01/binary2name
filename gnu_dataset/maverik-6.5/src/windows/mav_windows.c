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


#include "mavlib_windows.h"
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#if !defined(WIN32) && !defined(macintosh)
#include <dlfcn.h>
#endif
#include <string.h>
#if (!defined(WIN32) || defined(__CYGWIN__)) && !defined(macintosh)
#include <sys/types.h>
#include <unistd.h>
#endif

#if !defined(WIN32) || defined(__CYGWIN__)
char *strdup(const char *);
#endif

extern int mavlib_voodoo;

int mav_opt_noWins=MAV_FALSE;
int mav_opt_stereo=MAV_UNDEFINED;
int mav_opt_fullscreen=MAV_UNDEFINED;
int mav_opt_x=MAV_UNDEFINED;
int mav_opt_y=MAV_UNDEFINED;
int mav_opt_width=MAV_UNDEFINED;
int mav_opt_height=MAV_UNDEFINED;
char *mav_opt_name=NULL;
char *mav_opt_disp=NULL;
int mav_opt_right_x=MAV_UNDEFINED;
int mav_opt_right_y=MAV_UNDEFINED;
int mav_opt_right_width=MAV_UNDEFINED;
int mav_opt_right_height=MAV_UNDEFINED;
char *mav_opt_right_name=NULL;
char *mav_opt_right_disp=NULL;
int mav_opt_restrictMouse=MAV_UNDEFINED;
MAV_class *mav_class_world;
MAV_class *mav_class_any;
MAV_class *mav_class_none;
MAV_object *mav_object_world;
MAV_object *mav_object_any;
MAV_object *mav_object_none;
MAV_ctrlF mav_ctrlF[15];
char *mav_ctrlF_desc[15];

MAV_window *mav_win_left=NULL;
MAV_window *mav_win_mono=NULL;
MAV_window *mav_win_right=NULL;

int mav_xres;
int mav_yres;
int mav_mouse_x;
int mav_mouse_y;
int mav_mouse_root_x;
int mav_mouse_root_y;
int mav_mouse_button[10];
MAV_window *mav_win_mouse= NULL;
MAV_vector mav_mouse_pos;
MAV_vector mav_mouse_dir;

MAV_viewModifierParams mav_stp_default;



/* Routine to poll the windows device, i.e. get the mouse's position */

void mavlib_pollWindow(void)
{  
#if defined(macintosh) || defined(WIN32)
  if (mav_win_mouse) mav_mouseGet(mav_win_mouse, &mav_mouse_x, &mav_mouse_y, &mav_mouse_root_x, &mav_mouse_root_y, NULL);
#else
  if (mav_win_mouse) mav_mouseGet(mav_win_mouse, &mav_mouse_x, &mav_mouse_y, &mav_mouse_root_x, &mav_mouse_root_y, mav_mouse_button);
#endif
}



/* Routine to calculate the world position of the mouse */

void mavlib_calcWindow(void)
{
  MAV_line ln;
  
  if (mav_win_mouse) {
    ln= mav_lineFrom2DPoint(mav_win_mouse, mav_mouse_x, mav_mouse_y);

    /* place at twice near clip plane distance so we can render at this position */
    mav_mouse_dir= ln.dir;
    mav_mouse_pos= mav_vectorAdd(ln.pt, mav_vectorScalar(ln.dir, mav_win_mouse->ncp*2));
  }
}



/* Routine to check the windows device for events, i.e. mouse, keyboard, resize etc... */

int mavlib_checkWindowEvents(void)
{
  int rv, rv2=0, info[20];

  /* get events from window manager */
  rv= mav_gfxWindowEventGet(info);

  switch (rv&127)  {

  case 1: /* keyboard event */
    rv2= mavlib_dealWithKeyboardEvent(info);
    break;
  case 2: /* mouse event */
    rv2= mavlib_dealWithMouseEvent(info);
    break;
  case 3: /* resize event (only deal with the last one) */
#ifndef macintosh
    while (mav_gfxWindowEventPeek()==rv) mav_gfxWindowEventGet(info);
#endif
    rv2= mavlib_dealWithResizeEvent(info);    
    break;
  case 4: /* map/unmap event (only deal with the last one) */
#ifndef macintosh
    while (mav_gfxWindowEventPeek()==rv) mav_gfxWindowEventGet(info);
#endif
    rv2= mavlib_dealWithMapEvent(info);
    break;
  case 5: /* enter/leave event (only deal with the last one) */
#ifndef macintosh
    while (mav_gfxWindowEventPeek()==rv) mav_gfxWindowEventGet(info);
#endif
    rv2= mavlib_dealWithCrossingEvent(info);
    break;
  case 6: /* expose event (only deal with the last one) */
#ifndef macintosh
    while (mav_gfxWindowEventPeek()==rv) mav_gfxWindowEventGet(info);
#endif
    rv2= mavlib_dealWithExposeEvent(info);
    break;
  }

  return rv2;
}



/* Routine to restrict mouse to window */

int mavlib_restrictLastX;
int mavlib_restrictLastY;

void mavlib_restrictMouse(void *ignored)
{
  if (mav_opt_restrictMouse) {
    /* Is pointer outside of window? */
    if (mav_mouse_x>mav_win_current->width  || mav_mouse_x<0 ||
	mav_mouse_y>mav_win_current->height || mav_mouse_y<0) 
    {
      /* If so move it to last safe position and resample mouse */
      mav_gfxWindowPointerSet(1, mavlib_restrictLastX, mavlib_restrictLastY);
      mavlib_pollWindow();
    }
    else
    {
      /* If not then update last safe position to be current position */
      mavlib_restrictLastX= mav_mouse_x;
      mavlib_restrictLastY= mav_mouse_y;
    }
  }
}



/* Reserved Ctrl F1 keypress */

int mavlib_fullscreen=1;
int mavlib_restrictMouseOpt=0;

void mavlib_cf1(MAV_window *win)
{
  MAV_window *o, *w;
  
  mavlib_fullscreen=!mavlib_fullscreen;

  /* turn off/restore restrict mouse option */
  if (mavlib_fullscreen)
  {
    mav_opt_restrictMouse= mavlib_restrictMouseOpt;
  }
  else
  {
    mavlib_restrictMouseOpt= mav_opt_restrictMouse;
    mav_opt_restrictMouse= MAV_FALSE;
  }

  o= mav_win_current;
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) {
    mav_windowSet(w);
    mav_gfx3DfxModeSet(mavlib_fullscreen);
  }
  
  mav_windowSet(o);
}



void mavlib_cf5(MAV_window *w)
{
  if (w->orthogonal)
  {
    mav_windowOrthogonalSet(w, w->ncp*0.9, w->fcp, w->ortho_size, w->aspect);
  }
  else
  {
    mav_windowPerspectiveSet(w, w->ncp*0.9, w->fcp, w->fov, w->aspect);
  }
  printf("Near clip plane now at %f\n", w->ncp);
}

void mavlib_cf6(MAV_window *w)
{
  if (w->orthogonal)
  {
    mav_windowOrthogonalSet(w, w->ncp*1.1, w->fcp, w->ortho_size, w->aspect);
  }
  else
  {
    mav_windowPerspectiveSet(w, w->ncp*1.1, w->fcp, w->fov, w->aspect);
  }
  printf("Near clip plane now at %f\n", w->ncp);
}

void mavlib_cf7(MAV_window *w)
{
  if (w->orthogonal)
  {
    mav_windowOrthogonalSet(w, w->ncp, w->fcp*0.9, w->ortho_size, w->aspect);
  }
  else
  {
    mav_windowPerspectiveSet(w, w->ncp, w->fcp*0.9, w->fov, w->aspect);
  }
  printf("Far clip plane now at %f\n", w->fcp);
}

void mavlib_cf8(MAV_window *w)
{
  if (w->orthogonal)
  {
    mav_windowOrthogonalSet(w, w->ncp, w->fcp*1.1, w->ortho_size, w->aspect);
  }
  else
  {
    mav_windowPerspectiveSet(w, w->ncp, w->fcp*1.1, w->fov, w->aspect);
  }
  printf("Far clip plane now at %f\n", w->fcp);
}

void mavlib_cf9(MAV_window *w)
{
  if (w->orthogonal)
  {
    mav_windowOrthogonalSet(w, w->ncp, w->fcp, w->ortho_size*0.9, w->aspect);
    printf("Orthogonal size now at %f\n", w->ortho_size);
  }
  else
  {
    mav_windowPerspectiveSet(w, w->ncp, w->fcp, w->fov*0.9, w->aspect);
    printf("Field of view now at %f\n", w->fov);
  }
}

void mavlib_cf10(MAV_window *w)
{
  if (w->orthogonal)
  {
    mav_windowOrthogonalSet(w, w->ncp, w->fcp, w->ortho_size*1.1, w->aspect);
    printf("Orthogonal size now at %f\n", w->ortho_size);
  }
  else
  {
    mav_windowPerspectiveSet(w, w->ncp, w->fcp, w->fov*1.1, w->aspect);
    printf("Field of view now at %f\n", w->fov);
  }
}



/* Reserved Ctrl F12 keypress */

void mavlib_cf12(MAV_window *w)
{
#if defined(WIN32) || defined(macintosh)
  fprintf(stderr, "Warning: dynamic loading of modules is not supported under this OS, ignoring\n"); /* WIN32 */
#else
  char buf[500], filename[500], initName[100];
  void *dlh;
  MAV_moduleInitFn fn;
  int i;

  if (!getenv("MAV_HOME")) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: MAV_HOME variable not set, can't load module\n");
    return;
  }

  /* Prompt user for module */
  fprintf(stderr, "Enter name of module to load:\n");
  fgets(buf, 500, stdin);

  /* Remove new line */
  buf[strlen(buf)-1]=0;
  
  /* Try to load that module */
  sprintf(filename, "%s/lib/libmav_%s.so", getenv("MAV_HOME"), buf);
  fprintf(stderr, "Looking for library file %s... ", filename);

#ifdef MAV_SUNOS4
  dlh= dlopen(filename, 1);
#else
  dlh= dlopen(filename, RTLD_NOW);
#endif  

  if (dlh) 
  {
    fprintf(stderr, "found it\n");
  }
  else
  {
    fprintf(stderr, "\n%s\n", dlerror());
    return;
  }

  /* Find the initialise function */
  sprintf(initName, "mav_%sModuleInit", buf);
  fprintf(stderr, "Looking for function %s... ", initName);

  fn= (MAV_moduleInitFn) dlsym(dlh, initName);

  if (fn)
  {
    fprintf(stderr, "got it, executing\n");
    fn();
  }
  else
  {
    fprintf(stderr, "failed\n");

    /* Capitalise and re-try */
    for (i=0; i<strlen(buf); i++) if (buf[i]>=97 && buf[i]<=122) buf[i]-=32;
    sprintf(initName, "mav_%sModuleInit", buf);
    fprintf(stderr, "Looking for function %s... ", initName);

    fn= (MAV_moduleInitFn) dlsym(dlh, initName);
    
    if (fn)
    {
      fprintf(stderr, "got it, executing\n");
      fn();
    }
    else
    {
      fprintf(stderr, "failed\n");
      return;
    }
  }
#endif
}



/* Routines to initialise the windows module */

void mavlib_geomDecode(char *geom, int check_undefined, 
		       int *width, int *height, int *xoff, int *yoff)
{

  /* decode an X-style geometry specification into the 4 given ints */

  int n= 0, n2;
  char off_sign;
  int xres, yres, w, h, x, y;

  /* if string begins with + or -, it contains offsets only */
  if (!(geom[0] == '-' || geom[0] == '+'))
  {
    /* decode width + height */

    sscanf(geom, "%dx%d%n", &w, &h, &n);

    if (!check_undefined || (check_undefined && *width == MAV_UNDEFINED))
    {
      *width= w;
    }

    if (!check_undefined || (check_undefined && *height == MAV_UNDEFINED))
    {
      *height= h;
    }
  }

  if (geom[n])
  {
    /* decode xoff + yoff */
    
    mav_gfxWindowResGet(&xres, &yres);

    off_sign= geom[n++];
    sscanf(&geom[n], "%d%n", &x, &n2);
    n += n2;

    if (off_sign == '-')
    {
      /* number is an offset from right-side of screen */
      if (*width != MAV_UNDEFINED)
	x= xres - x - *width;
      else
      {
	if (mavlib_voodoo)
	  x= xres - x - 640;
	else
	  x= xres/2.0 - x;
      }	
    }

    if (!check_undefined || (check_undefined && *xoff == MAV_UNDEFINED))
    {
      *xoff= x;
    }

    off_sign= geom[n++];
    sscanf(&geom[n], "%d", &y);

    if (off_sign == '-')
    {
      if (*height != MAV_UNDEFINED)
	y= yres - y - *height;
      else
      {
	if (mavlib_voodoo)
	  y= yres - y - 480;
	else
	  y= yres/2.0 - y;
      }	
    }

    if (!check_undefined || (check_undefined && *yoff == MAV_UNDEFINED))
    {
      *yoff= y;
    }
  }
}


void mavlib_windowsCmdLineParse(int argc, char *argv[])
{
  int cur_arg;
  int cur_char;
  char *arg_name;

  /* start at 1 to skip prog name */
  for (cur_arg=1; cur_arg < argc; cur_arg++)
  {

    /* skip args already parsed by another module */
    if (argv[cur_arg])
    {

      arg_name= mav_malloc(sizeof(char)*strlen(argv[cur_arg])+1);
      strcpy(arg_name, argv[cur_arg]);

      /* lowercase arg_name */
      for (cur_char=0; arg_name[cur_char]; cur_char++)
      {  
	arg_name[cur_char]= tolower(arg_name[cur_char]);
      }

      /* X display (main and right windows) */
      if (strcmp(arg_name, "-display") == 0)
      {
	mav_opt_disp= argv[cur_arg+1];
	argv[cur_arg]= NULL;
	argv[++cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-rdisplay") == 0 ||
	       strcmp(arg_name, "-right_display") == 0)
      {
	mav_opt_right_disp= argv[cur_arg+1];
	mav_opt_stereo= MAV_STEREO_TWO_WINS;	/* assume stereo required */
	argv[cur_arg]= NULL;
	argv[++cur_arg]= NULL;
      }

      /* window geometry (main and right windows) */
      else if (strcmp(arg_name, "-geom") == 0 ||
	       strcmp(arg_name, "-geometry") == 0)
      {
	mavlib_geomDecode(argv[cur_arg+1], MAV_FALSE,
			  &mav_opt_width, &mav_opt_height,
			  &mav_opt_x, &mav_opt_y);
	/* set right window to same geometry, unless already set */
	mavlib_geomDecode(argv[cur_arg+1], MAV_TRUE,
			  &mav_opt_right_width, &mav_opt_right_height,
			  &mav_opt_right_x, &mav_opt_right_y);

	argv[cur_arg]= NULL;
	argv[++cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-rgeom") == 0 ||
	       strcmp(arg_name, "-right_geometry") == 0)
      {
	mavlib_geomDecode(argv[cur_arg+1], MAV_FALSE,
			  &mav_opt_right_width, &mav_opt_right_height,
			  &mav_opt_right_x, &mav_opt_right_y);

	mav_opt_stereo= MAV_STEREO_TWO_WINS;	/* assume stereo required */
	
	argv[cur_arg]= NULL;
	argv[++cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-name") == 0)
      {
	mav_opt_name= argv[cur_arg+1];
	argv[cur_arg]= NULL;
	argv[++cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-rname") == 0 ||
	       strcmp(arg_name, "-right_name") == 0)
      {
	mav_opt_right_name= argv[cur_arg+1];
	mav_opt_stereo= MAV_STEREO_TWO_WINS;	/* assume stereo required */
	argv[cur_arg]= NULL;
	argv[++cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-fullscreen") == 0)
      {
	mav_opt_fullscreen= MAV_TRUE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-nofullscreen") == 0)
      {
	mav_opt_fullscreen= MAV_FALSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-stereo") == 0)
      {
	if (strstr(argv[cur_arg+1], "quad"))
	{
	  if (strstr(argv[cur_arg+1], "separate"))
	  {
	    mav_opt_stereo= MAV_STEREO_QUAD_BUFFERS_SEPARATE_Z;
	  }
	  else
	  {
	    mav_opt_stereo= MAV_STEREO_QUAD_BUFFERS;
	  }
	}
	else
	{
	  mav_opt_stereo= MAV_STEREO_TWO_WINS;
	}
	argv[cur_arg]= NULL;
	argv[++cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-nostereo") == 0)
      {
	mav_opt_stereo= MAV_FALSE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-restrictmouse") == 0 ||
	       strcmp(arg_name, "-lockmouse") == 0)
      {
	mav_opt_restrictMouse= MAV_TRUE;
	argv[cur_arg]= NULL;
      }

      else if (strcmp(arg_name, "-norestrictmouse") == 0 ||
	       strcmp(arg_name, "-nolockmouse") == 0)
      {
	mav_opt_restrictMouse= MAV_FALSE;
	argv[cur_arg]= NULL;
      }
      else if (strcmp(arg_name, "-mavhelp") == 0)
      {
	printf("  -[no]fullscreen\t\t\tWindow fills the screen\n"
	       "  -stereo <type>\t\t\tOpen two windows for stereo viewing (type=twowins, quad, quadseparate)\n"
	       "  -[no](restrictMouse|lockMouse)\tRestrict mouse pointer to the Maverik window\n"
	       "  -display <X display string>\t\tDisplay to open window on\n"
	       "  -(geometry|geom) <X geometry string>\tSize and position of window\n"
	       "  -name <string>\t\t\tWindow title\n"
	       "  -(right_display|rdisplay) <X display string>\tDisplay to open right window (in stereo) on\n"
	       "  -(right_geometry|rgeom) <X geometry string>\tGeometry of right window\n"
	       "  -(right_name|rname) <string>\t\tTitle for right window\n");

	/* Don't open a window, as program will exit after all options are printed */
	mav_opt_noWins= MAV_TRUE;
      }

      mav_free(arg_name);
    }
  }
}


void mavlib_windowsEnvVarsParse(void)
{
  char *ev;
  
  if ((ev= getenv("MAV_FULLSCREEN")))
  {
    mav_opt_fullscreen= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }

  if ((ev= getenv("MAV_STEREO")))
  {
    if (strcmp(ev, "0") == 0) 
    {
      mav_opt_stereo= MAV_FALSE;
    }
    else
    {
      if (strstr(ev, "quad")) 
      {
	if (strstr(ev, "separate"))
	{
	  mav_opt_stereo= MAV_STEREO_QUAD_BUFFERS_SEPARATE_Z;
	}
	else
	{
	  mav_opt_stereo= MAV_STEREO_QUAD_BUFFERS;
	}
      }
      else
      {
	mav_opt_stereo= MAV_STEREO_TWO_WINS;
      }
    }
  }

  if ((ev= getenv("MAV_RESTRICTMOUSE")))
  {
    mav_opt_restrictMouse= (strcmp(ev, "0") == 0) ? MAV_FALSE : MAV_TRUE;
  }

  if ((ev= getenv("MAV_DISPLAY")))
  {
    mav_opt_disp= ev;
  }

  if ((ev= getenv("MAV_RIGHT_DISPLAY")))
  {
    mav_opt_right_disp= ev;
  }

  if ((ev= getenv("MAV_NAME")))
  {
    mav_opt_name= ev;
  }

  if ((ev= getenv("MAV_RIGHT_NAME")))
  {
    mav_opt_right_name= ev;
  }

  if ((ev= getenv("MAV_GEOMETRY")))
  {
    mavlib_geomDecode(ev, MAV_FALSE, &mav_opt_width, &mav_opt_height,
		      &mav_opt_x, &mav_opt_y);
  }

  if ((ev= getenv("MAV_RIGHT_GEOMETRY")))
  {
    mavlib_geomDecode(ev, MAV_FALSE,
		      &mav_opt_right_width, &mav_opt_right_height,
		      &mav_opt_right_x, &mav_opt_right_y);
  }
}


void mavlib_windowsConfigFileParse(FILE *cf_file)
{
  char line[200], opt[100], val[100];
  int cur_char;

  fseek(cf_file, 0, SEEK_SET);

  while (fgets(line, 200, cf_file))
  {
    if (sscanf(line, "%s %s", opt, val) == 2)
    {
      /* lowercase opt name */
      for (cur_char=0; opt[cur_char]; cur_char++)
      {  
	opt[cur_char]= tolower(opt[cur_char]);
      }

      /* config file options mustn't override application settings */

      if (strcmp(opt, "fullscreen") == 0 && mav_opt_fullscreen == MAV_UNDEFINED)
      {
	mav_opt_fullscreen= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }

      else if (strcmp(opt, "stereo") == 0 && mav_opt_stereo == MAV_UNDEFINED)
      {
	if (strcmp(val, "0") == 0)
	{
	  mav_opt_stereo= MAV_FALSE;
	}
	else
	{
	  if (strstr(val, "quad")) 
	  {
	    if (strstr(val, "separate"))
	    {
	      mav_opt_stereo= MAV_STEREO_QUAD_BUFFERS_SEPARATE_Z;
	    }
	    else
	    {
	      mav_opt_stereo= MAV_STEREO_QUAD_BUFFERS;
	    }
	  }
	  else
	  {
	    mav_opt_stereo= MAV_STEREO_TWO_WINS;
	  }
	}
      }

      else if (strcmp(opt, "restrictmouse") == 0 && mav_opt_restrictMouse == MAV_UNDEFINED)
      {
	mav_opt_restrictMouse= (strcmp(val, "0") == 0) ? MAV_FALSE : MAV_TRUE;
      }

      else if (strcmp(opt, "display") == 0 && mav_opt_disp == NULL)
      {
	mav_opt_disp= strdup(val);
      }

      else if (strcmp(opt, "right_display") == 0 && mav_opt_right_disp == NULL)
      {
	mav_opt_right_disp= strdup(val);
      }

      else if (strcmp(opt, "name") == 0 && mav_opt_name == NULL)
      {
	/* need to read the rest of the line as window name */
	sscanf(line, "%*s %n", &cur_char);
	mav_opt_name= strdup(&line[cur_char]);
      }

      else if (strcmp(opt, "right_name") == 0 && mav_opt_right_name == NULL)
      {
	sscanf(line, "%*s %n", &cur_char);
	mav_opt_right_name= strdup(&line[cur_char]);
      }

      else if (strcmp(opt, "geometry") == 0)
      {
	mavlib_geomDecode(val, MAV_TRUE, &mav_opt_width, &mav_opt_height,
			  &mav_opt_x, &mav_opt_y);
      }

      else if (strcmp(opt, "right_geometry") == 0)
      {
	mavlib_geomDecode(val, MAV_TRUE, &mav_opt_right_width, &mav_opt_right_height,
			  &mav_opt_right_x, &mav_opt_right_y);
      }
    }
  }
}

void mavlib_windowsOptionsDefaultSet(void)
{
  /* set any undefined options */

  if (mav_opt_fullscreen == MAV_UNDEFINED) mav_opt_fullscreen= MAV_FALSE;
  if (mav_opt_stereo == MAV_UNDEFINED) mav_opt_stereo= MAV_FALSE;
  /* restrictMouse is set depending on availability of a Voodoo card */
}


char *mav_windowsModuleID(void)
{
  return "Windows";
}

int mav_windowsModuleInit(void)
{
#if !defined(WIN32) || defined(__CYGWIN__)
  FILE *f;
  char filename[500];
#endif
  char buf[500], sbuf[510];
  int lx, ly, lw=0, lh=0, rx, ry, rw, rh, i;

  /* set options */
  if (mav_userconf) mavlib_windowsConfigFileParse(mav_userconf);
  mavlib_windowsEnvVarsParse();
  if (mav_argc) mavlib_windowsCmdLineParse(mav_argc, mav_argv);
  mavlib_windowsOptionsDefaultSet();
  
  /* add the new module */  
  mav_moduleNew(mav_windowsModuleID);

  /* add mouse/keyboard/window events as new devices */
  mav_deviceNew(mavlib_pollWindow, mavlib_calcWindow, mavlib_checkWindowEvents);

  /* define new classes for mouse/keyboard interaction */
  mav_class_world= mav_classNew();
  mav_class_any= mav_classNew();
  mav_class_none= mav_classNew();
  
  /* and their corresponding objects */
  mav_object_world= mav_objectNew(mav_class_world, NULL);
  mav_object_any= mav_objectNew(mav_class_any, NULL);
  mav_object_none= mav_objectNew(mav_class_none, NULL);

  /* define new callbacks for mouse/keyboard interaction and window events */
  mav_callback_keyboard= mav_callbackNew();
  mav_callback_sysKeyboard= mav_callbackNew();
  mav_callback_leftButton= mav_callbackNew();
  mav_callback_middleButton= mav_callbackNew();
  mav_callback_rightButton= mav_callbackNew();
  mav_callback_wheelUpButton= mav_callbackNew();
  mav_callback_wheelDownButton= mav_callbackNew();
  mav_callback_anyButton= mav_callbackNew();
  mav_callback_sysMouse= mav_callbackNew();
  mav_callback_resize= mav_callbackNew();
  mav_callback_map= mav_callbackNew();
  mav_callback_crossing= mav_callbackNew();
  mav_callback_expose= mav_callbackNew();
  
  /* define default handling routines for some event */
  mav_callbackResizeSet(mav_win_all, mav_resizeDefault);
  mav_callbackMapSet(mav_win_all, mav_mapDefault);
  mav_callbackExposeSet(mav_win_all, mav_exposeDefault);

  /* initialise ctrl-f key presses */
  for (i=0; i<15; i++) {
    mav_ctrlF[i]=NULL;
    mav_ctrlF_desc[i]=NULL;
  }

  if (mavlib_voodoo) {
    mav_ctrlF[1]= mavlib_cf1;
    mav_ctrlF_desc[1]= "Ctrl-F1 toggle between full screen and in-window rendering";
  }

  mav_ctrlF[5]= mavlib_cf5;
  mav_ctrlF_desc[5]= "Ctrl-F5 decrease near clipping plane by 10%";

  mav_ctrlF[6]= mavlib_cf6;
  mav_ctrlF_desc[6]= "Ctrl-F6 increase near clipping plane by 10%";

  mav_ctrlF[7]= mavlib_cf7;
  mav_ctrlF_desc[7]= "Ctrl-F7 decrease far clipping plane by 10%";

  mav_ctrlF[8]= mavlib_cf8;
  mav_ctrlF_desc[8]= "Ctrl-F8 increase far clipping plane by 10%";

  mav_ctrlF[9]= mavlib_cf9;
  mav_ctrlF_desc[9]= "Ctrl-F9 decrease field of view/orthogonal size by 10%";

  mav_ctrlF[10]= mavlib_cf10;
  mav_ctrlF_desc[10]= "Ctrl-F10 increase field of view/orthogonal size by 10%";

  mav_ctrlF[12]= mavlib_cf12;
  mav_ctrlF_desc[12]= "Ctrl-F12 load a module on the fly";

  /* store screen resolution */
  mav_gfxWindowResGet(&mav_xres, &mav_yres);

  /* open the requested number of windows */
  if (!mav_opt_noWins) {

    /* calculate windows size and position */
    if (mav_opt_x != MAV_UNDEFINED)
    {
      lx= mav_opt_x;
    }
    else
    {
      lx= 0;
    }

    if (mav_opt_y != MAV_UNDEFINED)
    {
      ly= mav_opt_y;
    }
    else
    {
      if (mavlib_voodoo) 
      {
	ly= 0;
      }
      else
      {
#ifdef MAV_LINUX
	ly= mav_yres/2-35;
#else
	ly= mav_yres/2;
#endif      
      }
    }

    if (mav_opt_width != MAV_UNDEFINED)
    {
      lw= mav_opt_width;
    }
    else
    {
      if (mavlib_voodoo)
      {
	lw= 640;
      }
      else
      {
	lw= mav_xres/2.0;
      }
    }

    if (mav_opt_height != MAV_UNDEFINED)
    {
      lh= mav_opt_height;
    }
    else
    {
      if (mavlib_voodoo) 
      {
	lh= 480;
      }
      else
      {
	lh= mav_yres/2.0;
      }
    }
  
    if (mav_opt_fullscreen==MAV_TRUE) {
      lx= 0;
      ly= 0;
      lw= mav_xres;
      lh= mav_yres;
    }

    if (mav_opt_right_x != MAV_UNDEFINED)
    {
      rx= mav_opt_right_x;
    }
    else
    {
      rx= mav_xres/2;
    }

    if (mav_opt_right_y != MAV_UNDEFINED)
    {
      ry= mav_opt_right_y;
    }
    else
    {
      if (mavlib_voodoo)
      {
	ry=0;
      }
      else
      {
#ifdef MAV_LINUX
	ry= mav_yres/2-35;
#else
	ry= mav_yres/2;
#endif
      }
    }

    if (mav_opt_right_width != MAV_UNDEFINED)
    {
      rw= mav_opt_right_width;
    }
    else
    {
      if (mavlib_voodoo) 
      {
	rw= 640;
      }
      else
      {
	rw= mav_xres/2.0;
      }
    }

    if (mav_opt_right_height != MAV_UNDEFINED)
    {
      rh= mav_opt_right_height;
    }
    else
    {
      if (mavlib_voodoo) 
      {
	rh= 480;
      }
      else
      {
	rh= mav_yres/2.0;
      }
    }
  
    if (mav_opt_fullscreen==MAV_TRUE) {
      rx= 0;
      ry= 0;
      rw= mav_xres;
      rh= mav_yres;
    }

    /* find name of executable */

    if (mav_argc)
    {
      strcpy(buf, mav_argv[0]);
    }
    else
    {
#if (defined(WIN32) && !defined(__CYGWIN__)) || defined(macintosh)
      strcpy(buf, "Maverik"); /* WIN32 */
#else
      sprintf(filename, "/tmp/mavname%i", getpid());
#ifdef __CYGWIN__
      sprintf (buf, "ps a | awk '{if ($1==%i) print $8}' 2>&1 >%s", getpid(), filename);
#else
#ifdef MAV_LINUX
      sprintf(buf, "ps a | awk '{if ($1==%i) print $5}' 2>&1 >%s", getpid(), filename);
#else
      sprintf(buf, "ps -f | awk '{if ($2==%i) print $8}' 2>&1 >%s", getpid(), filename);
#endif
#endif

      system(buf);
      f= fopen(filename, "r");
      if (f) 
      {
	if (fscanf(f, "%s", buf)!=1) strcpy(buf, "Maverik");
#ifdef __CYGWIN__
        else {
	  int last_slash= -1;
	  int i= 0;

/* code to strip path from executable */
	  while (buf[i] != 0) {
	    if (buf[i] == '/') last_slash= i;
	    i ++;
	  }

	  if (last_slash != -1) {
	    i= 0;
	    last_slash ++;
	    while (buf[i+last_slash] != 0) {
	      buf[i]= buf[i+last_slash];
	      i ++;
	    }
	    buf[i]= 0;
	  }
        }
#endif

	fclose(f);
      }
      else
      {
	strcpy(buf, "Maverik");
      }
      sprintf(sbuf, "rm -f %s", filename);
      system(sbuf);
#endif
    }

    /* open the windows with the correct name */
    if (mav_opt_stereo) 
    {
      if (mav_opt_stereo==MAV_STEREO_QUAD_BUFFERS) mav_opt_quadBuf= MAV_STEREO_QUAD_BUFFERS;
      if (mav_opt_stereo==MAV_STEREO_QUAD_BUFFERS_SEPARATE_Z) mav_opt_quadBuf= MAV_STEREO_QUAD_BUFFERS_SEPARATE_Z;
      
      if (mav_opt_name) 
      {
	mav_win_left= mav_windowNew(lx, ly, lw, lh, mav_opt_name, mav_opt_disp);
      }
      else
      {
	sprintf(sbuf, "%s (left)", buf);
	mav_win_left= mav_windowNew(lx, ly, lw, lh, sbuf, mav_opt_disp);
      }

      if (mavlib_voodoo) mav_gfx3DfxBoardSet(1);

      if (mav_opt_right_name) 
      {
	mav_win_right= mav_windowNew(rx, ry, rw, rh, mav_opt_right_name, mav_opt_right_disp);
      }
      else
      {
	sprintf(sbuf, "%s (right)", buf);
	mav_win_right= mav_windowNew(rx, ry, rw, rh, sbuf, mav_opt_right_disp);  
      }

      /* Set correct view */
      mav_stp_default.offset=1.0;
      mav_windowViewModifierSet(mav_win_left, &mav_stp_default, mav_eyeLeft);
      mav_windowViewModifierSet(mav_win_right, &mav_stp_default, mav_eyeRight);
    }
    else
    {
      if (mav_opt_name) 
      {
	mav_win_left= mav_windowNew(lx, ly, lw, lh, mav_opt_name, mav_opt_disp);
      }
      else
      {
	mav_win_left= mav_windowNew(lx, ly, lw, lh, buf, mav_opt_disp);
      }
    }

    mav_win_mono= mav_win_left;
    mav_win_mouse= mav_win_left;
  }

  /* Restrict mouse to current window if requested */
  if (mav_opt_restrictMouse == MAV_UNDEFINED) {
    if (mavlib_voodoo)
    {
      mav_opt_restrictMouse= MAV_TRUE;
    }
    else
    {
      mav_opt_restrictMouse= MAV_FALSE;
    }
  }
  mavlib_restrictLastX= lw/2;
  mavlib_restrictLastY= lh/2;
  mav_frameFn1Add(mavlib_restrictMouse, NULL);

/* Define the mouse's surface params */

  mav_mouseSurfaceParamsSet(mav_surfaceParamsNew(MAV_COLOUR, MAV_COLOUR_RED, 0, 0));

  return 1;
}



/* Routine to find a MAV_window pointer from its id number */

MAV_window *mavlib_getWindow(int id)
{
  MAV_window *win;

  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &win)) if (win->id==id) return win;
  
  return NULL;
}



/* Routine to calculate a line through a the mouse's position */

MAV_line mav_lineFrom2DPoint(MAV_window *w, int x, int y)
{
  MAV_line ln;
  MAV_vector up, right;
  float width, height, rx, ry;

  /* get x and y in range 0.0 to 1.0 */
  rx= ((float) x)/w->width;
  ry= ((float) y)/w->height;

  if (w->orthogonal) 
  {
    /* get size of window */
    height= w->ortho_size;
    width= height * w->aspect;

    right= mav_vectorScalar(w->right, width*(rx-0.5));
    up= mav_vectorScalar(w->up, -height*(ry-0.5));

    ln.pt= mav_vectorAdd(w->eye, mav_vectorAdd(right, up));
    ln.dir= w->view;
  } 
  else 
  {
    /* get size of window */
    height= tan(w->fov*0.5*MAV_PI_OVER_180)*2.0;
    width= height * w->aspect;
  
    right= mav_vectorScalar(w->right, width*(rx-0.5));
    up= mav_vectorScalar(w->up, -height*(ry-0.5));

    ln.pt= w->eye;
    ln.dir= mav_vectorAdd(w->view, mav_vectorAdd(right, up));
    ln.dir= mav_vectorNormalize(ln.dir);
  }

  return ln;
}



/* Routines to perform stereo offset calculations */

void mav_eyeLeft(MAV_window *w)
{
  w->view= w->vp->trans_view;
  w->up= w->vp->trans_up;
  w->right= w->vp->trans_right;
  
  w->eye= mav_vectorSub(w->vp->trans_eye, mav_vectorScalar(w->right, w->vmp->offset*0.5));
}

void mav_eyeRight(MAV_window *w)
{
  w->view= w->vp->trans_view;
  w->up= w->vp->trans_up;
  w->right= w->vp->trans_right;

  w->eye= mav_vectorAdd(w->vp->trans_eye, mav_vectorScalar(w->right, w->vmp->offset*0.5));
}
