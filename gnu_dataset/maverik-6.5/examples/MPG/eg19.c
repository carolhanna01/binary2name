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


#include "maverik.h"
#include <stdio.h>

/* Wrapper functions to set and execute the calcVol callback */

MAV_callback *mav_callback_calcVol;
typedef int (*MAV_callbackCalcVolFn)(MAV_object *o, float *vol);

void mav_callbackCalcVolSet(MAV_window *w, MAV_class *c, MAV_callbackCalcVolFn fn)
{
  mav_callbackSet(mav_callback_calcVol, w, c, (MAV_callbackFn) fn);
}

int mav_callbackCalcVolExec(MAV_window *w, MAV_object *o, float *vol)
{
  return (mav_callbackExec(mav_callback_calcVol, w, o, vol, NULL));
}



/* Routine to calculate the volume of a box */

int mav_boxCalcVol(MAV_object *o, float *vol)
{
  /* Convert from generic Maverik object to a box object */
  MAV_box *box= (MAV_box *) mav_objectDataGet(o);

  /* Calculate volume */
  *vol= (box->size.x*box->size.y*box->size.z);
  
  return MAV_TRUE;
}



/* Define a box */
void defBox(MAV_box *b)
{
  b->size.x= 1.0; /* Specify its size */
  b->size.y= 2.0;
  b->size.z= 3.0;
  b->matrix= MAV_ID_MATRIX; /* Position and orientation */
  b->sp= mav_sp_default;    /* Surface parameters, i.e. colour */
}

/* Render a frame */
void drawFrame(MAV_SMS *sms, MAV_object *obj)
{
  float vol;

  /* Check for and act on any events */
  mav_eventsCheck();
    
  /* Request start of a new frame */
  mav_frameBegin();
    
  /* Display the SMS in all windows */
  mav_SMSDisplay(mav_win_all, sms);

  /* Calculate and display the volume of the object */
  mav_callbackCalcVolExec(mav_win_current, obj, &vol);
  printf("vol is %f\n", vol);

  /* Request end of the frame */
  mav_frameEnd();
}

/* Mouse event callback */
int mouseEvent(MAV_object *o, MAV_mouseEvent *me)
{
  MAV_box *box;

  /* Convert from generic Maverik object to the box object */
  box= (MAV_box *) mav_objectDataGet(o);

  if (me->movement==MAV_PRESSED) { /* Only consider button presses */
    box->size.x+=1.0; /* Make box a bit bigger */
  }   

  return 1;
}

/* Keyboard event callback */
int keyEvent(MAV_object *o, MAV_keyboardEvent *ke)
{
  if (ke->movement==MAV_PRESSED) { /* Only consider button presses */
    if (ke->key<255) { /* Only consider printable ASCII characters */
      printf("Pressed %c\n",  ke->key);
    }
  }   

  return 1;
}

int main(int argc, char *argv[])
{
  MAV_box box;
  MAV_object *obj;
  MAV_SMS *sms;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* Create a new callback, and this for the box class */
  mav_callback_calcVol= mav_callbackNew();
  mav_callbackCalcVolSet(mav_win_all, mav_class_box, mav_boxCalcVol);

  /* Define a box object */
  defBox(&box);

  /* Register the box as a Maverik object */
  obj= mav_objectNew(mav_class_box, &box);

  /* Create a SMS */
  sms= mav_SMSObjListNew();

  /* Add object to SMS */
  mav_SMSObjectAdd(sms, obj);

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Define mouse callback */
  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_box, mouseEvent);

  /* Define keyboard event callback */
  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyEvent);

  /* Rendering loop */
  while (1) drawFrame(sms, obj);
}
