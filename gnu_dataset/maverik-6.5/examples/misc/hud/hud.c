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
#include "mav_hud.h"
#include <stdio.h>
#include <stdlib.h>

/* Define a box */
void defBox(MAV_box *b, int col)
{
  b->size.x= 1.0; /* Specify its size */
  b->size.y= 2.0;
  b->size.z= 3.0;
  b->matrix= MAV_ID_MATRIX; /* Position and orientation */

  /* Define its "surface parameters", i.e. the colour with which it's rendered */
  /* Use the sign of col to indicate a material or texture, and the value */
  /* of col gives the material or texture index to use */

  if (col>=0)
  {
    b->sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, col, 0); /* Use material index col */
  }
  else
  {
    b->sp= mav_surfaceParamsNew(MAV_TEXTURE, 0, 0, -col); /* Use texture index col */
  }
}

/* Render a frame */
void drawFrame(MAV_SMS *sms)
{
  /* Check for and act on any events */
  mav_eventsCheck();
    
  /* Request start of a new frame */
  mav_frameBegin();
    
  /* Display the SMS in all windows */
  mav_SMSDisplay(mav_win_all, sms);

  /* Request end of the frame */
  mav_frameEnd();
}

int hit(MAV_object *o, MAV_mouseEvent *me)
{
  if (me->movement==MAV_PRESSED) printf("Ouch\n");
  return 1;
}

int main(int argc, char *argv[])
{
  MAV_box box;
  MAV_object *obj;
  MAV_SMS *sms;
  MAV_hud *hud1, *hud2;

  /* Initialise the Maverik system */
  mav_opt_trans= MAV_TRUE;
  mav_initialise(&argc, argv);

  /* Initialise the HUD module */
  mav_hudModuleInit();

  /* Define a box object */
  defBox(&box, 2);

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Register the box as a Maverik object */
  obj= mav_objectNew(mav_class_box, &box);

  /* Create a SMS */
  sms= mav_SMSObjListNew();

  /* Add object to SMS */
  mav_SMSObjectAdd(sms, obj);

  /* Make a simple HUD with 3 lines of text */
  hud1= mav_hudNew(3, 100, 300, 200, 50, mav_sp_default);
  hud1->line[0]= "Line 0 text";
  hud1->line[1]= "Line 1 text";
  hud1->line[2]= "Line 2 text";
  
  /* Make this HUD un-selectable */
  hud1->selectable=0;

  /* Add it to the SMS */
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_hud, hud1));

  /* Make a HUD with no text that has a transparent colour */
  mav_paletteColourSet(mav_palette_default, 21, 1.0, 1.0, 1.0, 0.3);
  hud2= mav_hudNew(0, 350, 250, 100, 100, mav_surfaceParamsNew(MAV_COLOUR, 21, 0, 0));

  /* Add it to the SMS */
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_hud, hud2));

  /* Set a mouse callback for the hud class */
  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_hud, hit);

  /* Rendering loop */
  while (1) drawFrame(sms);
}
