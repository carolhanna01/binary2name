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

int main(int argc, char *argv[]) 
{
  MAV_sphere sph;
  MAV_SMS *sms;
  MAV_vector out;
  float tran=1;
  int idx;
  
/* Initialise the Maverik system */

  mav_opt_trans= MAV_TRUE;
  mav_initialise(&argc, argv);

/* Set background colour */
  
  mav_windowBackgroundColourSet(mav_win_all, 0.0, 0.0, 0.0);

/* Set up mouse navigators */

  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

/* Define an object - a sphere */

  sph.radius=10.0;
  sph.nchips=15;
  sph.nverts=30;
  sph.sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 0);
  sph.matrix= MAV_ID_MATRIX;

/* Define an SMS and put the sphere in it */
  
  sms = mav_SMSObjListNew();
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_sphere, &sph));

/* Define initial eye point */

  mav_vp_default.eye= mav_vectorSet(0,0,100);

/* Get an unused colour index */

  idx= mav_paletteColourIndexEmptyGet(mav_palette_default);
  if (idx<0) {
    fprintf(stderr, "Could not find an unused colour index\n");
    exit(1);
  }

/* Dont warn when this colour is redefined */

  mav_paletteColourIndexWarnSet(mav_palette_default, idx, MAV_FALSE);

/* Main loop */

  while (1) {

/* Check for, and act on, any events */

    mav_eventsCheck();

/* Do what needs to be done at the start of a frame */

    mav_frameBegin();

/* Draw the objects */

    mav_SMSDisplay(mav_win_all, sms);

/* Define a white colour with a varying transparency */
  
    tran-=0.02;
    if (tran<0) tran=1;
    mav_paletteColourSet(mav_palette_default, idx, 1, 1, 1, tran);

/* Display some text at a fixed position (0.25, 0.25) with this colour */

    mav_stringDisplay(mav_win_all, "* fixed text *", idx, 0, 0.25, 0.25);

/* Calculate the screen position of the sphere centre (0,0,0) and display some text */

    out= mav_vectorScrnPos(MAV_NULL_VECTOR);  
    mav_stringDisplay(mav_win_all, "Some text !", MAV_COLOUR_GREEN, 0, out.x, out.y);

/* Do what needs to be done at the end of a frame */

    mav_frameEnd();
  }
}


