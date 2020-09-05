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
#include "mav_vrml97.h"
#include "mav_lwo.h"
#include <stdio.h>
#include <stdlib.h>

MAV_object *obj;
int drawbb=0;
int yaw=0;
int yawv=0;

int keyb(MAV_object *o, MAV_keyboardEvent *ke)
{
  if (ke->movement==MAV_PRESSED) {
    switch (ke->key) {
    case 'd': /* dump object */
      mav_callbackDumpExec(ke->win, obj);
      break;
    case 'b': /* draw BB */
      drawbb=!drawbb;
      break;
    case 'y': /* yaw velocity */
      if (yawv) 
      {
	yawv=0;
      }
      else
      {
	yawv=5;
      }
      break;
    }
  }
  
  return 1;

}



int main(int argc, char *argv[])
{
  MAV_SMS *sms;
  MAV_composite c;
  MAV_BB bb;

/* Initialise the Maverik system */

  mav_opt_trans= MAV_TRUE;
  mav_initialise(&argc, argv);

/* Initialise the VRML97 module */

  mav_VRML97ModuleInit();

/* Initialise the Lightwave module */

  mav_LWOModuleInit();

  if (argc!=2) {
    printf("Usage: %s <filename>\n", argv[0]);
    exit(0);
  }

/* Read the composite */

  if (mav_compositeRead(argv[1], &c, MAV_ID_MATRIX)==MAV_FALSE) {
    printf("failed to read %s.\n", argv[1]);
    exit(1);
  }

/* Register as a Maverik object and put it into an SMS */

  sms= mav_SMSObjListNew();
  obj= mav_objectNew(mav_class_composite, &c);
  mav_SMSObjectAdd(sms, obj);

/* Use default mouse navigation */

  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

/* Define keyboard interaction */

  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyb);

/* Main loop */

  while (1) {

/* Yaw the composite */

    yaw+=yawv;
    c.matrix= mav_matrixSet(0,0,yaw,0,0,0);

/* Check for, and act on, any events */

    mav_eventsCheck();

/* Do what needs to be done at the start of a frame */

    mav_frameBegin();

/* Display the SMS to all windows */

    mav_SMSDisplay(mav_win_all, sms);

/* Draw BB of object if applicable */

    if (drawbb) {
      mav_callbackBBExec(mav_win_current, obj, &bb);
      mav_BBDisplay(mav_win_all, bb);
    }

/* Do what needs to be done at the end of a frame */

    mav_frameEnd();
  }
}


