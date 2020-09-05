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


#include "allobjs.h"
#include <stdio.h>

MAV_SMS *sms;
MAV_surfaceParams *selectedSp;
MAV_surfaceParams *unselectedSp;



int main(int argc, char *argv[]) 
{
  MAV_viewParams vp;
    
/* Initialise the Maverik system */

  mav_initialise(&argc, argv);

/* set perspective, background colour and viewing frustum for window */
  
  mav_windowBackgroundColourSet(mav_win_all, 0, 0.5, 1.0);
  mav_windowPerspectiveSet(mav_win_all, 0.1, 10000, 90.0, 1.25);
  mav_windowBackfaceCullSet(mav_win_all, MAV_FALSE);
  mav_windowViewParamsSet(mav_win_all, &vp);

/* define callbacks for mouse and keyboard interaction */

  /* callbacks to select and unselect objects - independent of class */

  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_any, select_obj); 
  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_none, unselect_obj); 

  /* callbacks for changing object dimensions - class specific */

  mav_callbackMouseSet(MAV_LEFT_BUTTON, mav_win_all, mav_class_box, dec_box);
  mav_callbackMouseSet(MAV_RIGHT_BUTTON, mav_win_all, mav_class_box, inc_box);

  mav_callbackMouseSet(MAV_LEFT_BUTTON, mav_win_all, mav_class_pyramid, dec_pyr);
  mav_callbackMouseSet(MAV_RIGHT_BUTTON, mav_win_all, mav_class_pyramid, inc_pyr);

  mav_callbackMouseSet(MAV_LEFT_BUTTON, mav_win_all, mav_class_cylinder, dec_cyl);
  mav_callbackMouseSet(MAV_RIGHT_BUTTON, mav_win_all, mav_class_cylinder, inc_cyl);

  mav_callbackMouseSet(MAV_LEFT_BUTTON, mav_win_all, mav_class_cone, dec_cone);
  mav_callbackMouseSet(MAV_RIGHT_BUTTON, mav_win_all, mav_class_cone, inc_cone);

  mav_callbackMouseSet(MAV_LEFT_BUTTON, mav_win_all, mav_class_hsphere, dec_hsph);
  mav_callbackMouseSet(MAV_RIGHT_BUTTON, mav_win_all, mav_class_hsphere, inc_hsph);

  mav_callbackMouseSet(MAV_LEFT_BUTTON, mav_win_all, mav_class_ctorus, dec_ct);
  mav_callbackMouseSet(MAV_RIGHT_BUTTON, mav_win_all, mav_class_ctorus, inc_ct);

  mav_callbackMouseSet(MAV_LEFT_BUTTON, mav_win_all, mav_class_rtorus, dec_rt);
  mav_callbackMouseSet(MAV_RIGHT_BUTTON, mav_win_all, mav_class_rtorus, inc_rt);

  /* keyboard callback */ 

  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyb); 

/* create an SMS for the objects */

  sms = mav_SMSObjListNew();

/* define 2 colours */

  mav_paletteColourSet(mav_palette_default, 1, 0.5, 0.4, 0.3, 1.0);
  mav_paletteColourSet(mav_palette_default, 2, 0.3, 0.8, 0.2, 1.0);

/* define a lighting model and 2 material */

  mav_paletteLightingModelSet(mav_palette_default, 0.4, 0.4, 0.4, 1.0, 1);

  mav_paletteMaterialSet(mav_palette_default, 1, 0.5, 0.5, 0.4, 1.0,  0.4, 0.2, 0.2, 1.0,  
		  0.4, 0.4, 0.4, 1.0,  0.0, 0.0, 0.0, 0.0,  30.0);

  mav_paletteMaterialSet(mav_palette_default, 2, 0.1, 0.1, 0.1, 1.0,  0.0, 0.8, 0.2, 1.0,
		  0.1, 0.1, 0.1, 1.0,  0.0, 0.0, 0.0, 0.0,  30.0);

/* define 2 textures */

  mav_paletteTextureSet(mav_palette_default, 1, "../textures/stainedglass.ppm");
  mav_paletteTextureSet(mav_palette_default, 2, "../textures/marble_floor.ppm");

/* define the selected and unselected surface params */

  unselectedSp= mav_surfaceParamsNew(MAV_MATERIAL, 1, 1, 1);
  selectedSp= mav_surfaceParamsNew(MAV_MATERIAL, 2, 2, 2);

/* define a single light source */

  mav_paletteLightSet(mav_palette_default, 1,  0.0, 0.0, 0.0, 0.0,  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0);
  mav_paletteLightPos(mav_palette_default, 1,  mav_vectorSet(150,50,0));

/* define objects and insert them into the SMS */

  defprims();

/* use default keyboard navigation */

  mav_navigationKeyboard(mav_win_all, mav_navigationKeyboardDefault);
  mav_navigationKeyboardDefaultParams(mav_win_all, 100, 0.002, 0.0001);

/* define the viewing parameters */

  vp.eye.x= 50; /* eye position */
  vp.eye.y= 50;
  vp.eye.z= 100;

  vp.view.x= 0;  /* view direction */
  vp.view.y= 0;
  vp.view.z= -1;

  vp.up.x= 0;  /* view up and World up */
  vp.up.y= 1;
  vp.up.z= 0;
  vp.fixed_up=vp.up;
  vp.mod= NULL;
  
/* main loop */

  while (1) {

/* check for, and act on, any events */

    mav_eventsCheck();

/* do what needs to be done at the start of a frame */

    mav_frameBegin();

/* draw the object to all windows */

    mav_SMSDisplay(mav_win_all, sms);

/* do what needs to be done at the end of a frame */

    mav_frameEnd();
  }
}
