/*
 GNGNU Maverik - a system for managing display and interaction in 
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

MAV_surfaceParams **sp, *sp1, *sp2;
MAV_matrix *mat=NULL;
float hitDist;
MAV_vector hitOffset;

int keyb(MAV_object *obj, MAV_keyboardEvent *ke)
{
  if (ke->movement==MAV_PRESSED) {
    if (ke->key=='q') hitDist*=1.1;
    if (ke->key=='a') hitDist*=0.9; 
    if (ke->key=='m' && sp) *sp= sp1;
    if (ke->key=='t' && sp) *sp= sp2;
 }
  
  return 1;
}

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
    obj= me->obj;
    mav_callbackGetMatrixExec(mav_win_all, obj, &mat);
    mav_callbackGetSurfaceParamsExec(mav_win_all, obj, &sp);

/* store the distance to the intersection and the distance from that
   to the centre of the object */

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
    sp= NULL;
  }

  return 1;
}


int main(int argc, char *argv[]) 
{
  MAV_composite gp;
  MAV_object *gpo;
  MAV_sphere sph;
  MAV_cone cone;
  MAV_SMS *sms;

/* Initialise the Maverik system */

  mav_opt_trans=MAV_TRUE; /* Allow for transparent objects */
  mav_initialise(&argc, argv);
        
/* Enable backface cull */
  
  mav_windowBackfaceCullSet(mav_win_all, MAV_TRUE);

/* Use default mouse navigation */

  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

/* Define mouse selection of objects */

  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_any, selected);
  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyb);

/* Create some materials one of which is semi transparent */

  mav_paletteMaterialSet(mav_palette_default, 1,  0.2, 0.2, 0.4, 1.0,  0.4, 0.2, 0.2, 1.0,  
		  0.4, 0.4, 0.4, 1.0,  0.0, 0.0, 0.0, 1.0, 30.0);

  mav_paletteMaterialSet(mav_palette_default, 2,  0.4, 0.2, 0.4, 0.4,  0.1, 0.5, 0.6, 0.4,  
		  0.3, 0.4, 0.4, 0.4,  0.0, 0.0, 0.0, 0.4, 30.0);

  sp1= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 0);
  sp2= mav_surfaceParamsNew(MAV_MATERIAL, 0, 2, 0);

/* Define an object - a cone */

  cone.rt=0.0;
  cone.rb=1.0;
  cone.height=2.0;
  cone.nverts=30;
  cone.sp= sp1;
  cone.endcap= 1;
  cone.matrix= mav_matrixSet(0, -90, 0, 0, 2.5, 0);

/* Define an object - a sphere */

  sph.radius=1.0;
  sph.nchips=15;
  sph.nverts=30;
  sph.sp= sp1;
  sph.matrix= MAV_ID_MATRIX;

/* Define a ground plane */

  if (mav_compositeReadAC3D("ground.ac", &gp, MAV_ID_MATRIX)==MAV_FALSE) {
    printf("can not read ground plane composite\n");
    exit(1);
  }

  gpo= mav_objectNew(mav_class_composite, &gp);

/* Define an SMS and put the sphere and cone in it */
  
  sms= mav_SMSObjListNew();
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_sphere, &sph));
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_cone, &cone));

/* Define initial view */

  mav_vp_default.eye= mav_vectorSet(0,1.3,5);

/* Main loop */

  while (1) {

/* Check for, and act on, any events */

    mav_eventsCheck();

/* Do what needs to be done at the start of a frame */

    mav_frameBegin();

/* Display the ground plane then SMS to all windows */

    mav_compositeDraw(gpo, NULL);
    mav_SMSDisplay(mav_win_all, sms);

/* Do what needs to be done at the end of a frame */

    mav_frameEnd();
  }
}

