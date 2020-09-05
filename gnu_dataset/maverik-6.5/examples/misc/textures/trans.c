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

/* Define a box */
MAV_rectangle *defBoard(MAV_matrix mat, int tex) {

  MAV_rectangle *rect= (MAV_rectangle *)mav_malloc(sizeof(MAV_rectangle));
  MAV_vector pos;

  pos= mav_matrixXYZGet(mat);
  rect->matrix= mav_matrixXYZSet(mat, mav_vectorSet(pos.x, pos.y+2.5, pos.z));
  rect->width= 2.5;
  rect->height= 5;
  rect->xtile= 1;
  rect->ytile= 1;
  rect->sp= mav_surfaceParamsNew(MAV_TEXTURE, 0, 0, tex);

  return rect;
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

int main(int argc, char *argv[])
{
  MAV_rectangle r;
  MAV_SMS *sms;

  /* Initialise the Maverik system */
  mav_opt_trans= MAV_TRUE;
  mav_initialise(&argc, argv);

  mav_paletteMaterialSet(mav_palette_default, 1,
			 0, 0, 0, 1,
			 0.8, 0.8, 0.8, 1,
			 0.2, 0.2, 0.2, 1,
			 0, 0, 0, 1,
			 30);

  mav_paletteTextureSet(mav_palette_default, 1, "marble_floor.ppm");
  mav_paletteTextureSet(mav_palette_default, 2, "lamp.png");
  mav_paletteTextureSet(mav_palette_default, 3, "tree.png");
  mav_paletteTextureSet(mav_palette_default, 4, "stainedwindow.png");

  mav_windowBackgroundColourSet(mav_win_all, 0.9, 0.9, 1.0);

  /* Clamp transparent textures to prevent edge artifacts */
  mav_paletteTextureEnvSet(mav_palette_default, 2, mav_texEnvClamp);
  mav_paletteTextureEnvSet(mav_palette_default, 3, mav_texEnvClamp);
  mav_paletteTextureEnvSet(mav_palette_default, 4, mav_texEnvClamp);

  /* Create a SMS */
  sms= mav_SMSObjListNew();

  /* floor */
  r.width= 50.0; /* Size */
  r.height= 50.0;
  r.xtile= 3; /* Texture repeat tiling */
  r.ytile= 3;
  /* Orientation (RPY 0,-90,0) and position (XYZ 0,0,0) */
  r.matrix= mav_matrixSet(0,-90,0, 0,0,0); 
  r.sp= mav_surfaceParamsNew(MAV_TEXTURE, 0, 0, 1); 

  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rectangle, &r));

  /* Define boards */
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rectangle,
		    defBoard(mav_matrixSet(0,0,0, 0,0,0), 2)));
  
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rectangle,
		    defBoard(mav_matrixSet(0,0,-30, 5,0,1), 3)));
  
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rectangle,
		    defBoard(mav_matrixSet(0,0,-45, 5,0,-22), 3)));
  
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rectangle,
		    defBoard(mav_matrixSet(0,0,35, -1,0,10), 3)));
  
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rectangle,
		    defBoard(mav_matrixSet(0,0,50, -4,0,1.5), 4)));
  
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rectangle,
		    defBoard(mav_matrixSet(0,0,0, 3,0,-10), 4)));
  
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rectangle,
		    defBoard(mav_matrixSet(0,0,7, -2.4,0,15), 4)));
  
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rectangle,
		    defBoard(mav_matrixSet(0,0,20, -1,0,3), 2)));
  
  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* initial eye posn */
  mav_win_current->vp->eye= mav_vectorSet(0, 3, 22);

  /* Rendering loop */
  while (1) drawFrame(sms);
}
