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

MAV_callback *mav_callback_resize;



/* Wrapper routines to set and execute the resize callback */

void mav_callbackResizeSet(MAV_window *w, MAV_callbackResizeFn fn)
{
  mav_callbackSet(mav_callback_resize, w, mav_class_world, (MAV_callbackFn) fn);
}

int mav_callbackResizeExec(MAV_window *w, MAV_resizeEvent *re)
{
  return mav_callbackExec(mav_callback_resize, w, mav_object_world, (void *) re, NULL);
}



/* Routine to deal with this event */

int mavlib_dealWithResizeEvent(int *info)
{
  MAV_resizeEvent re;
  
  /* Make up event data structure */
  re.win= mavlib_getWindow(info[0]);
  re.width= info[1];
  re.height= info[2];

  return mav_callbackResizeExec(re.win, &re);
}



/* Default event handling routine */

int mav_resizeDefault(MAV_object *obj, MAV_resizeEvent *ev)
{
  MAV_window *orig_win= mav_win_current;
  
  /* resize window keeping the same field of view or orthogonal size */

  if (ev->win!=mav_win_current) mav_windowSet(ev->win);

  ev->win->width= ev->width;
  ev->win->height= ev->height;
  mav_gfxViewPortSet(0,0,ev->width,ev->height);
  if (ev->win->orthogonal)
  {
    mav_windowOrthogonalSet(ev->win, ev->win->ncp, ev->win->fcp, ev->win->ortho_size, ((float) ev->width)/ev->height);
  }
  else
  {
    mav_windowPerspectiveSet(ev->win, ev->win->ncp, ev->win->fcp, ev->win->fov, ((float) ev->width)/ev->height);
  }

  if (mav_win_current!=orig_win) mav_windowSet(orig_win);

  return -100;
}

