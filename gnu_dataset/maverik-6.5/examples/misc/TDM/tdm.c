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
#include "mav_tdm.h"
#include <stdio.h>

MAV_TDMCursor cur;
float tdmscale=1.0;
int drawbb=0;
int polltdm=0;



/* hit object callback */

int tdmhit(MAV_object *o, MAV_TDMEvent *ev)
{
  if (ev->intersects) printf("hit\n");

  if (ev->button==1) {
    printf("setting origin as %f %f %f\n", ev->pos.pos.x, ev->pos.pos.y, ev->pos.pos.z);
    mav_TDMXYZOriginSet(ev->pos.pos.x, ev->pos.pos.y, ev->pos.pos.z);
  }

  return 0;
}



/* lookabout view parameter mode */

void lookabout(MAV_window *w)
{
  float ax, ay;

/* calcaulated translated view parameters */

  w->vp->trans_eye= w->vp->eye;
  w->vp->trans_view= w->vp->view;
  w->vp->trans_up= w->vp->up;
  w->vp->trans_right= w->vp->right;

/* rotate view parameters by an amount governed by the mouse pos */

  ax= (320-mav_mouse_x)*0.01;
  ay= (256-mav_mouse_y)*0.01;

/* pitch view by amount ay */

  w->vp->trans_view= mav_vectorRotate(w->vp->trans_view, w->vp->trans_right, ay);
  w->vp->trans_up= mav_vectorRotate(w->vp->trans_up, w->vp->trans_right, ay);

/* yaw view by amount ax */

  w->vp->trans_view= mav_vectorRotate(w->vp->trans_view, w->vp->trans_up, ax);
  w->vp->trans_right= mav_vectorRotate(w->vp->trans_right, w->vp->trans_up, ax);
}



/* keyboard event callback */

int act_trk=MAV_TDM_RED;

int key(MAV_object *obj, MAV_keyboardEvent *ke)
{
  if (ke->movement==MAV_PRESSED) {

    switch (ke->key) {
    case 's': /* change style of cursor */
      cur.style++;
      if (cur.style==4) cur.style=0;
      break;
    case 'l': /* various view parameter modifiers */
      mav_win_current->vp->mod= lookabout;
      break;
    case 'r':
      act_trk= MAV_TDM_RED;	
      break;
    case 'b':
      act_trk= MAV_TDM_BLUE;	
      break;
    case 'v':
      act_trk= MAV_TDM_VEL;
      break;
    case 'h':
      act_trk= MAV_TDM_HMD;
      break;
    case '0':
      mav_win_current->vp->mod= mav_TDM_vp[0];
      break;
    case '1':
      mav_win_current->vp->mod= mav_TDM_vp[1];
      break;
    case '2':
      mav_win_current->vp->mod= mav_TDM_vp[2];
      break;
    case '3':
      mav_win_current->vp->mod= mav_TDM_vp[3];
      break;
    case 'f':
      mav_win_current->vp->mod= NULL;
      break;
    case '=': /* increase TDM scale */
      tdmscale*=1.1;
      printf("TDM scale at %f\n", tdmscale);
      mav_TDMScaleSet(tdmscale);
      break;
    case '-': /* decrease TDM scale */
      tdmscale*=0.9;
      printf("TDM scale at %f\n", tdmscale);
      mav_TDMScaleSet(tdmscale);
      break;
    case 'd': /* toggle drawing BB */
      drawbb=!drawbb;
      break;
    case 'p': /* toggle poll information on trackers */
      polltdm=!polltdm;
      break;
    }
  }

  return 1;
}



int main(int argc, char *argv[])
{
  MAV_viewParams vp;
  MAV_cylinder cyl;
  MAV_cone cone;
  MAV_SMS *sms, *cursms;

/* Initialise the Maverik system */

  mav_initialise(&argc, argv);

/* Initialise the TDM module */

  /* Uncomment to specify the dynamically loaded TDM library */
  /* mav_opt_TDMLib= "/usr/local/TDM-2.1/lib/libtdm-keys.so"; */

  mav_TDMModuleInit();

/* Set perspective and background colour */
  
  mav_windowPerspectiveSet(mav_win_all, 0.1, 1000.0, 70.0, 1.25); 
  mav_windowBackgroundColourSet(mav_win_all, 0.0, 0.5, 1.0);

/* Use default mouse navigation */

  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

/*
  Set up mouse navigation so that left button yaws the view parameters 
  and moves forwards/backwards, right button moves left/right and up/down
  and middle button rolls and pitches.
*/

  mav_navigationMouseDefaultParams(mav_win_all, MAV_LEFT_BUTTON, mav_navigateYaw, 0.002, -0.0001,
                              mav_navigateForwards, 0.002, 0.0001);
  mav_navigationMouseDefaultParams(mav_win_all, MAV_MIDDLE_BUTTON, mav_navigateRoll, 0.002, 0.0001,
                              mav_navigatePitch, 0.002, 0.0001);
  mav_navigationMouseDefaultParams(mav_win_all, MAV_RIGHT_BUTTON, mav_navigateRight, 0.002, 0.0001,
                              mav_navigateUp, 0.002, 0.0001);

/* Create a lighting model, light (and position it) and two materials */

  mav_paletteLightingModelSet(mav_palette_default, 0.4, 0.4, 0.4, 1.0, MAV_TRUE);
  mav_paletteLightSet(mav_palette_default, 0,  0.0, 0.0, 0.0, 1.0,  1.0, 1.0, 1.0, 1.0,
               1.0, 1.0, 1.0, 1.0);
  mav_paletteLightPos(mav_palette_default, 0, mav_vectorSet(100,150,150));
  mav_paletteMaterialSet(mav_palette_default, 1,  0.5, 0.5, 0.4, 1.0,  0.4, 0.2, 0.2, 1.0,  
                  0.4, 0.4, 0.4, 1.0,  0.0, 0.0, 0.0, 1.0, 30.0);
  mav_paletteMaterialSet(mav_palette_default, 2,  0.5, 0.2, 0.2, 1.0,  0.5, 0.2, 0.2, 1.0,  
                  0.4, 0.4, 0.4, 1.0,  0.0, 0.0, 0.0, 1.0, 30.0);

/* Define an object - a cylinder */

  cyl.radius=1.0;
  cyl.height=2.0;
  cyl.nverts=30;
  cyl.sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 0);
  cyl.matrix= MAV_ID_MATRIX;
  cyl.endcap= 1;

/* Define another object - a cone */

  cone.rt=0.0;
  cone.rb=1.0;
  cone.height=2.0;
  cone.nverts=30;
  cone.sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 0);
  cone.matrix= MAV_ID_MATRIX;
  cone.endcap= 1;

/* Define a TDM cursor */

  cur.tracker= 0;
  cur.style= 0;
  cur.sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, 2, 0);

/* Define an SMS to contain the objects and put the cylinder and cone in it */
  
  sms= mav_SMSNew(mav_SMSClass_objList, mav_objListNew());
  mav_SMSCallbackObjectAddExec(sms, mav_objectNew(mav_class_cylinder, &cyl));
  mav_SMSCallbackObjectAddExec(sms, mav_objectNew(mav_class_cone, &cone));

/* Define another SMS to contain the cursor */

  cursms= mav_SMSNew(mav_SMSClass_objList, mav_objListNew());
  mav_SMSCallbackObjectAddExec(cursms, mav_objectNew(mav_class_TDMCursor, &cur));  

/* Make this SMS unselectable otherwise we will always intersect the cursor */

  mav_SMSSelectabilitySet(cursms, mav_win_all, MAV_FALSE);

/* Set keyboard and TDM event callback */

  mav_callbackKeyboardSet(mav_win_all, mav_class_world, key);
  mav_callbackTDMSet(mav_win_all, mav_class_world, tdmhit);

/* Define a set of viewing parameters */

  vp.eye.x= 0; /* eye position */
  vp.eye.y= 0;
  vp.eye.z= 20;

  vp.view.x= 0;  /* view direction */
  vp.view.y= 0;
  vp.view.z= -1;

  vp.up.x= 0;  /* view up and World up */
  vp.up.y= 1;
  vp.up.z= 0;
  vp.fixed_up=vp.up;

/* No view modification function required */

  vp.mod = NULL;
  
/* Bind the viewing parameters to the window */

  mav_windowViewParamsSet(mav_win_all, &vp);

/* Main loop */

  while (1) {

/* Check for, and act on, any events */

    mav_eventsCheck();

/* Do what needs to be done at the start of a frame */

    mav_frameBegin();

/* Set the cones matrix so that it follows the active tracker */

    cone.matrix= mav_TDM_matrix[act_trk];

/* Display the SMSs to all windows */

    mav_SMSDisplay(mav_win_all, sms);
    mav_SMSDisplay(mav_win_all, cursms);

/* Draw BB of cursor if applicable */

    if (drawbb)
    {
      MAV_BB bb;
      mav_callbackBBExec(mav_win_all, mav_objectDataWith(&cur), &bb);
      mav_BBDisplay(mav_win_all, bb);
    }

/* Display information on cursor if applicable */

    if (polltdm)
    {
      mav_vectorPrint("red pos ", mav_TDM_pos[0].pos);
      if (mav_TDM_pos[0].status & TDM_CALIBRATED) printf(" Position is calibrated\n");
      if (mav_TDM_pos[0].status & TDM_OUT_OF_RANGE) printf(" Position is out-of-range\n");
      mav_vectorPrint("red  u  ", mav_TDM_pos[0].u);
      mav_vectorPrint("red  v  ", mav_TDM_pos[0].v);
      mav_vectorPrint("red  n  ", mav_TDM_pos[0].n);
      printf("%i%i%i%i%i %i%i%i%i%i\n\n", mav_TDMButtonQuery(0,0), mav_TDMButtonQuery(0,1), mav_TDMButtonQuery(0,2), mav_TDMButtonQuery(0,3), mav_TDMButtonQuery(0,4), mav_TDMButtonQuery(1,0), mav_TDMButtonQuery(1,1), mav_TDMButtonQuery(1,2), mav_TDMButtonQuery(1,3), mav_TDMButtonQuery(1,4));
    }      

/* Do what needs to be done at the end of a frame */

    mav_frameEnd();
  }
}

