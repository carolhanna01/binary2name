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

MAV_callback *mav_callback_map;



/* Wrapper routines to set and execute the mapping callback */

void mav_callbackMapSet(MAV_window *w, MAV_callbackMapFn fn)
{
  mav_callbackSet(mav_callback_map, w, mav_class_world, (MAV_callbackFn) fn);
}

int mav_callbackMapExec(MAV_window *w, MAV_mapEvent *me)
{
  return mav_callbackExec(mav_callback_map, w, mav_object_world, (void *) me, NULL);
}



/* Routine to deal with this event */

int mavlib_dealWithMapEvent(int *info)
{
  MAV_mapEvent me;
  
  /* Make up event data structure */
  me.win= mavlib_getWindow(info[0]);

  switch (info[1]) {
  case 0:
    me.map= MAV_MAP;
    break;
  case 1:
    me.map= MAV_UNMAP;
    break;
  }

  return mav_callbackMapExec(me.win, &me);
}



/* Default event handling routine */

int mav_mapDefault(MAV_object *obj, MAV_mapEvent *me)
{
  return -100;
}
