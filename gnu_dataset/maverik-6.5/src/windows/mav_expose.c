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

MAV_callback *mav_callback_expose;



/* Wrapper routines to set and execute the expose callback */

void mav_callbackExposeSet(MAV_window *w, MAV_callbackExposeFn fn)
{
  mav_callbackSet(mav_callback_expose, w, mav_class_world, (MAV_callbackFn) fn);
}

int mav_callbackExposeExec(MAV_window *w, MAV_exposeEvent *ee)
{
  return mav_callbackExec(mav_callback_expose, w, mav_object_world, (void *) ee, NULL);
}



/* Routine to deal with this event */

int mavlib_dealWithExposeEvent(int *info)
{
  MAV_exposeEvent ee;
  
  /* Make up event data structure */
  ee.win= mavlib_getWindow(info[0]);

  return mav_callbackExposeExec(ee.win, &ee);
}



/* Default event handling routine */

int mav_exposeDefault(MAV_object *o, MAV_exposeEvent *ee)
{
  return -100;
}
