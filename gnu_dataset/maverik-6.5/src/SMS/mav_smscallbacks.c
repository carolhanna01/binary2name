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


#include "mavlib_sms.h"
#include <stdlib.h>
#include <stdio.h>



/* Wrapper routines to the SMS callbacks */

MAV_SMSCallback *mav_SMSCallback_objectAdd;
MAV_SMSCallback *mav_SMSCallback_intersect;
MAV_SMSCallback *mav_SMSCallback_pointerReset;
MAV_SMSCallback *mav_SMSCallback_pointerPush;
MAV_SMSCallback *mav_SMSCallback_pointerPop;
MAV_SMSCallback *mav_SMSCallback_objectNext;
MAV_SMSCallback *mav_SMSCallback_execFn;
MAV_SMSCallback *mav_SMSCallback_empty;
MAV_SMSCallback *mav_SMSCallback_size;
MAV_SMSCallback *mav_SMSCallback_objectContains;



/* Add object */

void mav_SMSCallbackObjectAddSet(MAV_SMSClass *c, MAV_SMSCallbackObjectAddFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_objectAdd, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackObjectAddExec(MAV_SMS *s, MAV_object *obj)
{
  return (mav_SMSCallbackExec(mav_SMSCallback_objectAdd, s, (void *) obj, NULL, NULL, NULL));
}

int mav_SMSObjectAdd(MAV_SMS *s, MAV_object *o)
{
  return mav_SMSCallbackObjectAddExec(s,o);
}


/* Intersect objects */

void mav_SMSCallbackIntersectSet(MAV_SMSClass *c, MAV_SMSCallbackIntersectFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_intersect, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackIntersectExec(MAV_SMS *s, MAV_window *w, MAV_line ln, MAV_objectIntersection *oi, MAV_object **obj)
{
  return (mav_SMSCallbackExec(mav_SMSCallback_intersect, s, (void *) w, (void *) &ln, (void *) oi, (void *) obj));
}



/* Reset Pointer */

void mav_SMSCallbackPointerResetSet(MAV_SMSClass *c, MAV_SMSCallbackPointerResetFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_pointerReset, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackPointerResetExec(MAV_SMS *s)
{
  return (mav_SMSCallbackExec(mav_SMSCallback_pointerReset, s, NULL, NULL, NULL, NULL));
}



/* Push Pointer */

void mav_SMSCallbackPointerPushSet(MAV_SMSClass *c, MAV_SMSCallbackPointerPushFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_pointerPush, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackPointerPushExec(MAV_SMS *s)
{
  return (mav_SMSCallbackExec(mav_SMSCallback_pointerPush, s, NULL, NULL, NULL, NULL));
}



/* Pop Pointer */

void mav_SMSCallbackPointerPopSet(MAV_SMSClass *c, MAV_SMSCallbackPointerPopFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_pointerPop, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackPointerPopExec(MAV_SMS *s)
{
  return (mav_SMSCallbackExec(mav_SMSCallback_pointerPop, s, NULL, NULL, NULL, NULL));
}



/* Next Object */

void mav_SMSCallbackObjectNextSet(MAV_SMSClass *c, MAV_SMSCallbackObjectNextFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_objectNext, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackObjectNextExec(MAV_SMS *s, MAV_object **obj)
{
  return (mav_SMSCallbackExec(mav_SMSCallback_objectNext, s, (void *) obj, NULL, NULL, NULL));
}



/* ExecuteFn */

void mav_SMSCallbackExecFnSet(MAV_SMSClass *c, MAV_SMSCallbackExecFnFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_execFn, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackExecFnExec(MAV_SMS *s, MAV_drawInfo *di, MAV_SMSExecFn *fn)
{
  return (mav_SMSCallbackExec(mav_SMSCallback_execFn, s, (void *) di, (void *) fn, NULL, NULL));
}



/* Empty SMS */

void mav_SMSCallbackEmptySet(MAV_SMSClass *c, MAV_SMSCallbackEmptyFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_empty, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackEmptyExec(MAV_SMS *s, int o)
{
  return (mav_SMSCallbackExec(mav_SMSCallback_empty, s, (void *) &o, NULL, NULL, NULL));
}



/* Size of SMS */

void mav_SMSCallbackSizeSet(MAV_SMSClass *c, MAV_SMSCallbackSizeFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_size, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackSizeExec(MAV_SMS *s, int *sz)
{
  int rv=1;

  if (mav_SMSCallbackQuery(mav_SMSCallback_size, s))
  {
    rv= mav_SMSCallbackExec(mav_SMSCallback_size, s, (void *) sz, NULL, NULL, NULL);
  }
  else
  {
    /* Count by hand with reset and step through if no size callback defined */
    
    MAV_object *o;
    *sz=0;
    
    mav_SMSCallbackPointerResetExec(s);
    while (mav_SMSCallbackObjectNextExec(s, &o)) (*sz)++;
  }

  return rv;
}




/* SMS contains object */

void mav_SMSCallbackObjectContainsSet(MAV_SMSClass *c, MAV_SMSCallbackObjectContainsFn fn)
{
  mav_SMSCallbackSet(mav_SMSCallback_objectContains, c, (MAV_SMSCallbackFn) fn);
}

int mav_SMSCallbackObjectContainsExec(MAV_SMS *s, MAV_object *obj, int *found)
{
  int rv=1;

  if (mav_SMSCallbackQuery(mav_SMSCallback_objectContains, s))
  {
    rv= mav_SMSCallbackExec(mav_SMSCallback_objectContains, s, (void *) obj, (void *) found, NULL, NULL);
  }
  else
  {
    /* Check by hand with reset and step through if no contains callback defined */

    MAV_object *o;
    *found=0;

    mav_SMSCallbackPointerResetExec(s);
    while (mav_SMSCallbackObjectNextExec(s, &o)) if (o==obj) *found=1;
  }

  return rv;
}
