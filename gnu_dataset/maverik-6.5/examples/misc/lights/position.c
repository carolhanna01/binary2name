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
#include <math.h>
#include <stdio.h>



MAV_matrix *mat=NULL;
float hitDist;
MAV_vector hitOffset;

void mover(void *ignored)
{
  MAV_vector v;

  /* calculate new position of object */
  v= mav_vectorAdd(mav_win_current->vp->eye, mav_vectorScalar(mav_mouse_dir, hitDist));

  *mat= mav_matrixXYZSet(*mat, mav_vectorSub(v, hitOffset));
}

int selected(MAV_object *obj, MAV_mouseEvent *me)
{
  if (me->movement==MAV_PRESSED)
  {
    mav_callbackGetMatrixExec(mav_win_all, me->obj, &mat);

    /* store the distance to the intersection and the distance from that to the centre of the object */
    hitDist= me->objint.pt1;
    hitOffset= mav_vectorAdd(me->line.pt, mav_vectorScalar(me->line.dir, hitDist));
    hitOffset= mav_vectorSub(hitOffset, mav_matrixXYZGet(*mat));

    mav_frameFn2Add(mover, NULL);
    mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_world, selected);
  }
  else
  {
    mav_frameFn2Rmv(mover, NULL);
    mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_world, NULL);
  }

  return 1;
}



int main(int argc, char *argv[]) 
{
  MAV_sphere sph;
  MAV_SMS *sms;
  MAV_box box;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);
  
  /* Define a sphere */
  sph.radius=4.0;
  sph.nchips=15;
  sph.nverts=30;
  sph.sp= mav_sp_default;
  sph.matrix= MAV_ID_MATRIX;

  /* Define a box - represents the light source */
  box.size= mav_vectorSet(1,1,1);
  box.sp= mav_surfaceParamsNew(MAV_COLOUR, MAV_COLOUR_RED, 0, 0);
  box.matrix= mav_matrixSet(0,0,0,7,0,0);

  /* Define an SMS and put the objects in it */
  sms= mav_SMSObjListNew();
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_sphere, &sph));
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_box, &box));

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Define mouse selection of objects */
  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_any, selected);

  /* Set the positioning of light 0 (the default initialized light) to */
  /* be absolute, i.e. its position is specified in world coordinates */
  mav_paletteLightPositioning(mav_palette_default, 0, MAV_LIGHT_ABSOLUTE);

  /* Main loop */
  while (1) {

    /* Check for, and act on, any events */
    mav_eventsCheck();

    /* Do what needs to be done at the start of a frame */
    mav_frameBegin();

    /* Light 0 follows box position */
    mav_paletteLightPos(mav_palette_default, 0, mav_matrixXYZGet(box.matrix));
    
    /* Display the objects to all windows */
    mav_SMSDisplay(mav_win_all, sms);

    /* Do what needs to be done at the end of a frame */
    mav_frameEnd();
  }
}


