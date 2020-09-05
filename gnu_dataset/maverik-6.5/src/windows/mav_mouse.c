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
#include <stdio.h>

MAV_callback *mav_callback_leftButton;
MAV_callback *mav_callback_middleButton;
MAV_callback *mav_callback_rightButton;
MAV_callback *mav_callback_wheelUpButton;
MAV_callback *mav_callback_wheelDownButton;
MAV_callback *mav_callback_anyButton;
MAV_callback *mav_callback_sysMouse;



/* Wrapper routines to set and execute the mouse callbacks */

void mav_callbackMouseSet(int button, MAV_window *w, MAV_class *c, MAV_callbackMouseFn fn)
{
  switch (button) {
  case MAV_LEFT_BUTTON:
    mav_callbackSet(mav_callback_leftButton, w, c, (MAV_callbackFn) fn);
    break;
  case MAV_MIDDLE_BUTTON:
    mav_callbackSet(mav_callback_middleButton, w, c, (MAV_callbackFn) fn);
    break;
  case MAV_RIGHT_BUTTON:
    mav_callbackSet(mav_callback_rightButton, w, c, (MAV_callbackFn) fn);
    break;
  case MAV_WHEELUP_BUTTON:
    mav_callbackSet(mav_callback_wheelUpButton, w, c, (MAV_callbackFn) fn);
    break;
  case MAV_WHEELDOWN_BUTTON:
    mav_callbackSet(mav_callback_wheelDownButton, w, c, (MAV_callbackFn) fn);
    break;
  case MAV_ANY_BUTTON:
    mav_callbackSet(mav_callback_anyButton, w, c, (MAV_callbackFn) fn);
    break;
  default:
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "button not valid\n");
    break;
  }
}

int mav_callbackMouseExec(int button, MAV_window *w, MAV_object *o, MAV_mouseEvent *me)
{
  int rv=0;

  switch (button) {
  case MAV_LEFT_BUTTON:
    rv= mav_callbackExec(mav_callback_leftButton, w, o, (void *) me, NULL);
    break;
  case MAV_MIDDLE_BUTTON:
    rv= mav_callbackExec(mav_callback_middleButton, w, o, (void *) me, NULL);
    break;
  case MAV_RIGHT_BUTTON:
    rv= mav_callbackExec(mav_callback_rightButton, w, o, (void *) me, NULL);
    break;
  case MAV_WHEELUP_BUTTON:
    rv= mav_callbackExec(mav_callback_wheelUpButton, w, o, (void *) me, NULL);
    break;
  case MAV_WHEELDOWN_BUTTON:
    rv= mav_callbackExec(mav_callback_wheelDownButton, w, o, (void *) me, NULL);
    break;
  case MAV_ANY_BUTTON:
    rv= mav_callbackExec(mav_callback_anyButton, w, o, (void *) me, NULL);
    break;
  default:
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "button not valid\n");
    break;
  }

  return rv;
}

void mav_callbackSysMouseSet(MAV_window *w, MAV_class *c, MAV_callbackMouseFn fn)
{
  mav_callbackSet(mav_callback_sysMouse, w, c, (MAV_callbackFn) fn);
}

int mav_callbackSysMouseExec(MAV_window *w, MAV_object *o, MAV_mouseEvent *me)
{
  return (mav_callbackExec(mav_callback_sysMouse, w, o, (void *) me, NULL));
}



/* Routine to deal with mouse events */

int mavlib_dealWithMouseEvent(int *info)
{
  MAV_mouseEvent me;
  MAV_callback *bc=NULL;
  int rv= MAV_FALSE;  
  int rv2= MAV_FALSE;
  int i;

  /* Make up event data structure */

  me.win= mavlib_getWindow(info[0]);
  me.x= info[1];
  me.y= info[2];
  me.root_x= info[3];
  me.root_y= info[4];

  switch (info[5]) {
  case 0:
    me.movement= MAV_PRESSED;
    break;
  case 1:
    me.movement= MAV_RELEASED;
    break;
  }

  switch (info[6]) {
  case 1:
    me.button= MAV_LEFT_BUTTON;
    bc= mav_callback_leftButton;
    break;
  case 2:
    me.button= MAV_MIDDLE_BUTTON;
    bc= mav_callback_middleButton;
    break;
  case 3:
    me.button= MAV_RIGHT_BUTTON;
    bc= mav_callback_rightButton;
    break;
  case 4:
    me.button= MAV_WHEELUP_BUTTON;
    bc= mav_callback_wheelUpButton;
    break;
  case 5:
    me.button= MAV_WHEELDOWN_BUTTON;
    bc= mav_callback_wheelDownButton;
    break;
  default:
    return rv;
  }

  for (i=0; i<MAV_MODIFIER_MAX; i++) {
    if (info[i+7])  /* This is inconsistent with 0=pressed, 1=released ! */
    {
      me.modifiers[i]= MAV_PRESSED;
    }
    else
    {
      me.modifiers[i]= MAV_RELEASED;
    }
  }

  /* Check if system callback is defined - used for mouse navigation */

  if (mav_callbackQuery(mav_callback_sysMouse, me.win, mav_object_world))
  {
    rv= mav_callbackSysMouseExec(me.win, mav_object_world, &me);
    if (rv) return rv;
  }

  /* See what we hit */

  me.line= mav_lineFrom2DPoint(me.win, me.x, me.y);
  me.intersects= mav_SMSIntersectLineAll(me.win, me.line, &me.objint, &me.obj);

  /* First check for anybutton events - precedence of classes is the same as for keyboard */

  if (mav_callbackQuery(mav_callback_anyButton, me.win, mav_object_world))
  {
    rv= mav_callbackMouseExec(MAV_ANY_BUTTON, me.win, mav_object_world, &me);
    rv2= MAV_TRUE;
  }
  else
  {
    if (me.intersects) 
    {
      if (mav_callbackQuery(mav_callback_anyButton, me.win, mav_object_any))
      {
	rv= mav_callbackMouseExec(MAV_ANY_BUTTON, me.win, mav_object_any, &me);
	rv2= MAV_TRUE;
      }
      else
      {
	if (mav_callbackQuery(mav_callback_anyButton, me.win, me.obj))
	{
	  rv= mav_callbackMouseExec(MAV_ANY_BUTTON, me.win, me.obj, &me);
	  rv2= MAV_TRUE;
	}
      }
    }
    else
    {
      if (mav_callbackQuery(mav_callback_anyButton, me.win, mav_object_none))
      {
	rv= mav_callbackMouseExec(MAV_ANY_BUTTON, me.win, mav_object_none, &me);
	rv2= MAV_TRUE;
      }
    }
  }

  /* If nothing for anybutton callback, try the actual button callback */

  if (!rv2) {
    if (mav_callbackQuery(bc, me.win, mav_object_world))
    {
      rv= mav_callbackMouseExec(me.button, me.win, mav_object_world, &me);
      rv2= MAV_TRUE;
    }
    else
    {
      if (me.intersects) 
      {
	if (mav_callbackQuery(bc, me.win, mav_object_any))
	{
	  rv= mav_callbackMouseExec(me.button, me.win, mav_object_any, &me);
	  rv2= MAV_TRUE;
	}
	else
	{
	  if (mav_callbackQuery(bc, me.win, me.obj))
	  {
	    rv= mav_callbackMouseExec(me.button, me.win, me.obj, &me);
	    rv2= MAV_TRUE;
	  }
	}
      }
      else
      {
	if (mav_callbackQuery(bc, me.win, mav_object_none))
	{
	  rv= mav_callbackMouseExec(me.button, me.win, mav_object_none, &me);
	  rv2= MAV_TRUE;
	}
      }
    }
  }

  return rv;
}



/* Routine to query and set the mouse's position */

void mav_mouseGet(MAV_window *win, int *x, int *y, int *rx, int *ry, int *buts)
{
#if defined(macintosh) || (defined(WIN32) && !defined(__CYGWIN__))
  if (buts && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: polling of mouse buttons not supported\n");
#endif

  if (!mav_gfxWindowPointerGet(win->id, x, y, rx, ry, buts)) {
    mav_win_mouse= mav_win_orig;
    mav_gfxWindowPointerGet(mav_win_mouse->id, x, y, rx, ry, buts);
  }
}

void mav_mouseSet(MAV_window *win, int x, int y)
{
  mav_gfxWindowPointerSet(win->id, x, y);
}



/* Routine to draw the mouse as a 2D cross */

int mav_drawingMouse=MAV_FALSE;
MAV_surfaceParams *mavlib_mouseSp=NULL;

void mav_mouseDraw(void *ignored)
{
  MAV_window *orig_win= mav_win_current;

  mav_drawingMouse=MAV_TRUE;

  if (mav_win_mouse) {
    if (mav_win_current!=mav_win_mouse) mav_windowSet(mav_win_mouse);
    
    mav_surfaceParamsUse(mavlib_mouseSp);

    mav_gfxLineBegin();
#ifdef WIN32
    mav_gfxVertex(mav_vectorAdd(mav_mouse_pos, mav_vectorScalar(mav_win_current->right, -100.0)));
    mav_gfxVertex(mav_vectorAdd(mav_mouse_pos, mav_vectorScalar(mav_win_current->right, +100.0)));
#else
    mav_gfxVertex(mav_vectorAdd(mav_mouse_pos, mav_vectorScalar(mav_win_current->right, -10000.0)));
    mav_gfxVertex(mav_vectorAdd(mav_mouse_pos, mav_vectorScalar(mav_win_current->right, +10000.0)));
#endif
    mav_gfxLineEnd();
    
    mav_gfxLineBegin();
#ifdef WIN32
    mav_gfxVertex(mav_vectorAdd(mav_mouse_pos, mav_vectorScalar(mav_win_current->up, -100.0)));
    mav_gfxVertex(mav_vectorAdd(mav_mouse_pos, mav_vectorScalar(mav_win_current->up, +100.0)));
#else
    mav_gfxVertex(mav_vectorAdd(mav_mouse_pos, mav_vectorScalar(mav_win_current->up, -10000.0)));
    mav_gfxVertex(mav_vectorAdd(mav_mouse_pos, mav_vectorScalar(mav_win_current->up, +10000.0)));
#endif
    mav_gfxLineEnd();
    
    if (mav_win_current!=orig_win) mav_windowSet(orig_win);
  }
}



/* Routine to set the mouse's surface params */

void mav_mouseSurfaceParamsSet(MAV_surfaceParams *sp)
{
  mavlib_mouseSp= sp;
}
