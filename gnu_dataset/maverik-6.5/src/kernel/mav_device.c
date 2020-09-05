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


#include "mavlib_kernel.h"
#include <stdlib.h>

MAV_list *mavlib_devicePoll_list;
MAV_list *mavlib_deviceCalc_list;
MAV_list *mavlib_deviceEvent_list;
int mav_needFrameDraw=0;



/* Routine to add a new device */

void mav_deviceNew(MAV_devicePollFn p, MAV_deviceCalcFn c, MAV_deviceEventFn e)
{
  /* Add the device to the list */
  if (p) mav_listItemAdd(mavlib_devicePoll_list, (void *) p);
  if (c) mav_listItemAdd(mavlib_deviceCalc_list, (void *) c);
  if (e) mav_listItemAdd(mavlib_deviceEvent_list, (void *) e);
}



/* Routine to poll devices */

void mav_devicePoll(void)
{
  MAV_devicePollFn fn;

  /* go through list of devices calling poll function */
  mav_listPointerReset(mavlib_devicePoll_list);
  while (mav_listItemNext(mavlib_devicePoll_list, (void **) &fn)) (*fn)();
}



/* As above but to calculate their world coordinates */

void mav_deviceCalc(void)
{
  MAV_deviceCalcFn fn;

  mav_listPointerReset(mavlib_deviceCalc_list);
  while (mav_listItemNext(mavlib_deviceCalc_list, (void **) &fn)) (*fn)();
}



/* As above but checking for events */

int mav_eventsCheck(void)
{
  MAV_deviceEventFn fn;
  int rv= MAV_FALSE;

  /* Go through list of devices. N.B. we deal with one event at a time */
  mav_listPointerReset(mavlib_deviceEvent_list);
  while (mav_listItemNext(mavlib_deviceEvent_list, (void **) &fn)) {
    rv=(*fn)();
    if (rv) return rv;
  }

  if (mav_firstFrame) rv=-100;

  return rv;
}
