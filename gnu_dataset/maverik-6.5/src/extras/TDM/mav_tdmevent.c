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


#include "mavlib_tdm.h"
#include <stdlib.h>

MAV_callback *mav_callback_TDM;
MAV_callback *mav_callback_sysTDM;



/* Wrapper routines to set and execute the TDM callbacks */

void mav_callbackTDMSet(MAV_window *w, MAV_class *c , MAV_callbackTDMFn fn)
{
  mav_callbackSet(mav_callback_TDM, w, c, (MAV_callbackFn) fn);
}

int mav_callbackTDMExec(MAV_window *w, MAV_object *o , MAV_TDMEvent *ev)
{
  return (mav_callbackExec(mav_callback_TDM, w, o, (void *) ev, NULL));
}

void mav_callbackSysTDMSet(MAV_window *w, MAV_class *c , MAV_callbackTDMFn fn)
{
  mav_callbackSet(mav_callback_sysTDM, w, c, (MAV_callbackFn) fn);
}

int mav_callbackSysTDMExec(MAV_window *w, MAV_object *o , MAV_TDMEvent *ev)
{
  return (mav_callbackExec(mav_callback_sysTDM, w, o, (void *) ev, NULL));
}



/* Routine to deal with this event */

#ifdef MAV_TDM
int mavlib_dealWithTDMEvent(TDM_buttonEvent *tev)
{
  MAV_TDMEvent ev;
  MAV_matrix iv, pos;
  int rv=0;

  /* Store info on the event */
  ev.tracker= tev->tracker;
  ev.button= tev->button;
  ev.movement= !tev->movement;
  mavlib_tdm2mav(&ev.pos, tev->pos[tev->tracker]);

  /* Check if system callback is defined - used for navigation */
  if (mav_callbackQuery(mav_callback_sysTDM, mav_win_current, mav_object_world))
  {
    rv= mav_callbackSysTDMExec(mav_win_current, mav_object_world, &ev);
    if (rv) return rv;
  }

  /* Calc trackers position at time of the event */
  iv= mavlib_TDM_iv();
  pos= mavlib_TDM_calcPos(ev.tracker, ev.pos, iv);

  /* Make the line */
  ev.line.pt= mav_win_current->eye;
  ev.line.dir= mav_vectorNormalize(mav_vectorSub(mav_matrixXYZGet(pos), mav_win_current->eye));
  
  /* See what we hit */
  ev.intersects= mav_SMSIntersectLineAll(mav_win_current, ev.line, &ev.objint, &ev.obj);

  /* Check if any callbacks are defined for the world object */
  if (mav_callbackQuery(mav_callback_TDM, mav_win_current, mav_object_world))
  {
    rv= mav_callbackTDMExec(mav_win_current, mav_object_world, &ev);
  }
  else
  {
    /* If we intersected, check for any class callbacks before specific class ones */
    if (ev.intersects) 
    {
      if (mav_callbackQuery(mav_callback_TDM, mav_win_current, mav_object_any))
      {
	rv= mav_callbackTDMExec(mav_win_current, mav_object_any, &ev);
      }
      else
      {
	if (mav_callbackQuery(mav_callback_TDM, mav_win_current, ev.obj))
	{
	  rv= mav_callbackTDMExec(mav_win_current, ev.obj, &ev);
	}
      }
    }
    else
    {
      /*  If no intersection check for none class */
      if (mav_callbackQuery(mav_callback_TDM, mav_win_current, mav_object_none))
      {
	rv= mav_callbackTDMExec(mav_win_current, mav_object_none, &ev);
      }
    }
  }

  return rv;
}
#endif
