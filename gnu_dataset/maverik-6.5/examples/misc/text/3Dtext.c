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
#include <string.h>
#if !defined(WIN32) || defined(__CYGWIN__)
char *strdup (const char *s1);
#endif

MAV_text text;
float mscale=1.0;

int keyb(MAV_object *obj, MAV_keyboardEvent *ev)
{
  if (ev->movement==MAV_PRESSED) {
    switch (ev->key) {
    case 's':
      text.style=MAV_STROKE_FONT;
      break;
    case 'f':
      text.style=MAV_FILLED_FONT;
      break;
    case 'o':
      text.style=MAV_OUTLINE_FONT;
      break;
    case 'c':
      text.justify=MAV_CENTER_JUSTIFY;
      break;
    case 'l':
      text.justify=MAV_LEFT_JUSTIFY;
      break;
    case 'r':
      text.justify=MAV_RIGHT_JUSTIFY;
      break;
    case 'w':
      mscale*=1.1;
      break;
    case 'e':
      mscale*=0.9;
      break;
    case 'm':
      if (text.sp->mode==MAV_MATERIAL) 
      {
	text.sp->mode=MAV_COLOUR;
      }
      else
      {
	text.sp->mode=MAV_MATERIAL;
      }
      break;
    }
  }
   
  return 1;
}


int main(int argc, char *argv[]) 
{
  MAV_sphere sph;
  MAV_SMS *sms;
  MAV_BB bb;
  int i;

/* Initialise the Maverik system */

  mav_initialise(&argc, argv);
        
/* Define mouse navigation */

  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

/* Define an object - a sphere */

  sph.radius=0.5;
  sph.nchips=5;
  sph.nverts=15;
  sph.sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 0);
  sph.matrix= MAV_ID_MATRIX;

/* Define a text object */

  text.text=strdup("--- Some text");
  text.sp= mav_surfaceParamsNew(MAV_COLOUR, MAV_COLOUR_WHITE, 1, 0);
  text.style=MAV_FILLED_FONT;
  text.justify=MAV_LEFT_JUSTIFY;
  text.matrix=MAV_ID_MATRIX;

/* Define an SMS and put the objects in it */
  
  sms = mav_SMSObjListNew();
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_sphere, &sph));
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_text, &text));

/* Define the initial eye point */

  mav_vp_default.eye= mav_vectorSet(0,0,5); /* eye position */

/* Define keyboard callback */

  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyb);

/* Main loop */

  i=0;

  while (1) {

/* Rotate and scale text */

    i+=5;
    text.matrix= mav_matrixScaleSet(mav_matrixSet(0, i, 0, 0, 0, 0), mscale);

/* Check for, and act on, any events */

    mav_eventsCheck();
    
/* Do what needs to be done at the start of a frame */

    mav_frameBegin();

/* Draw the objects */

    mav_SMSDisplay(mav_win_all, sms);

/* Draw the BB of the text */

    mav_callbackBBExec(mav_win_all, mav_objectDataWith(&text), &bb);
    mav_BBDisplay(mav_win_all, bb);

/* Do what needs to be done at the end of a frame */

    mav_frameEnd();
  }
}

