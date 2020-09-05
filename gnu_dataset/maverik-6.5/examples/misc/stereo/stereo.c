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

/* stereo.c */
#include "maverik.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
  MAV_box box;
  MAV_object *obj;
  MAV_SMS *sms;

/* Request stereo windows */
  mav_opt_stereo= MAV_STEREO_TWO_WINS;

  /* Uncomment if you have LCD shutter glasses
  mav_opt_stereo= MAV_STEREO_QUAD_BUFFERS;
  */

  /* Uncomment if you have more than one pipe. Set mav_opt_right_disp to be the DISPLAY
     of the pipe on which to open the window. This will be something like :0.1 or :1.0 
     depending on the configuration of the system.
  mav_opt_shareContexts= MAV_FALSE;
  mav_opt_right_disp= ":0.1";
  */

/* Initialise the Maverik system */
  mav_initialise(&argc, argv);

/* Define the box */
  box.size.x=1.0; /* Size */
  box.size.y=2.0;
  box.size.z=3.0;
  box.matrix= MAV_ID_MATRIX; /* Position and orientation */
  box.sp= mav_sp_default; /* Surface parameters, i.e. colour */

/* Register as a Maverik object */
  obj= mav_objectNew(mav_class_box, &box);

/* Create an SMS and add object to it */
  sms= mav_SMSObjListNew();
  mav_SMSObjectAdd(sms, obj);

/* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

/* Define stereo parameters, i.e. the stereo offset */
  mav_stp_default.offset= 0.5;
  
/* Main loop */
  while (1) {

/* Check for, and act on, any events */
    mav_eventsCheck();

/* Do what needs to be done at the start of a frame */
    mav_frameBegin();

/* Display the SMS to all windows */
    mav_SMSDisplay(mav_win_all, sms);

/* Do what needs to be done at the end of a frame */
    mav_frameEnd();
  }
}
