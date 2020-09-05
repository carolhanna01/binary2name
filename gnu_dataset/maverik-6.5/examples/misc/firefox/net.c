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
#include <stdlib.h>

MAV_object *objs[4];

int 
but(MAV_object *obj, MAV_mouseEvent *me)
{
  	if (me->movement==MAV_PRESSED) {
    	/* open appropriate page */
		if (obj==objs[0]) 
			system("firefox -remote \"openURL(http://www.ac3d.org)\"");
    	if (obj==objs[1]) 
			system("firefox -remote \"openURL(http://aig.cs.man.ac.uk/research/deva/deva.php)\"");
    	if (obj==objs[2]) 
			system("firefox -remote \"openURL(http://aig.cs.man.ac.uk)\"");
    	if (obj==objs[3]) 
			system("firefox -remote \"openURL(http://aig.cs.man.ac.uk/maverik/maverik.php)\"");
	}
  
  	return 1;
}



int 
main(int argc, char *argv[]) 
{
  	MAV_composite comp[4];
  	MAV_SMS *sms;
  	int i;

/* Initialise the Maverik system */

  	mav_initialise(&argc, argv);

/* Read in composites and put them in an SMS */

  	mav_compositeReadAC3D("../composites/ac3dlogo.ac", &comp[0], MAV_ID_MATRIX);
  	mav_compositeReadAC3D("../composites/devalogo.ac", &comp[1], MAV_ID_MATRIX);
  	mav_compositeReadAC3D("../composites/aiglogo.ac", &comp[2], MAV_ID_MATRIX);
  	mav_compositeReadAC3D("../../MPG/mavlogo.ac", &comp[3], MAV_ID_MATRIX);

  	comp[0].matrix= MAV_ID_MATRIX;
  	comp[1].matrix= mav_matrixSet(0,0,0, -3,0,0);
  	comp[2].matrix= mav_matrixSet(0,0,0, 3,0,0);
  	comp[3].matrix= mav_matrixSet(0,0,0, 0,3,0);

  	sms= mav_SMSObjListNew();
  	for (i=0; i<4; i++) {
    	objs[i]= mav_objectNew(mav_class_composite, &comp[i]);
    	mav_SMSObjectAdd(sms, objs[i]);
  	}

/* Use default mouse navigation */

  	mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

/* Define mouse interaction callback */

  	mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_composite, but);

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

