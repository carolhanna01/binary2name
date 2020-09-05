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


#include "mavlib_tdm.h"
#include <stdio.h>

MAV_class *mav_class_TDMCursor;
MAV_object *mavlib_TDM_cursorObj[13];
MAV_BB mavlib_TDM_cursorBB[4];



/* Routine to render a TDM cursor */

int mav_TDMCursorDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_TDMCursor *cur= (MAV_TDMCursor *) mav_objectDataGet(obj);

/* Set the correct colouring */

  mav_surfaceParamsUse(cur->sp);

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(mav_TDM_matrix[cur->tracker]);

  mav_gfxMatrixScale(mavlib_TDM_drawScale[cur->tracker], mavlib_TDM_drawScale[cur->tracker], mavlib_TDM_drawScale[cur->tracker]);

/* Draw the appropriate objects for the style */  

  switch (cur->style) {
  case 0:
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[0], di);
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[1], di);
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[2], di);
    break;
  case 1:
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[3], di);
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[4], di);
    break;
  case 2:
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[5], di);
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[6], di);
    break;
  case 3:
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[7], di);
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[8], di);
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[9], di);
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[10], di);
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[11], di);
    mav_callbackDrawExec(mav_win_current, mavlib_TDM_cursorObj[12], di);
    break;
  default:
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: TDM cursor style not recognised, ignoring\n");
    break;
  }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;
}



/* Routine to calculate the bounding box of a TDM cursor */

int mav_TDMCursorBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_TDMCursor *cur= (MAV_TDMCursor *) mav_objectDataGet(obj);
  MAV_BB bb2;

  /* Account for draw scale */
  bb2.min= mav_vectorScalar(mavlib_TDM_cursorBB[cur->style].min, mavlib_TDM_drawScale[cur->tracker]);
  bb2.max= mav_vectorScalar(mavlib_TDM_cursorBB[cur->style].max, mavlib_TDM_drawScale[cur->tracker]);
  
  /* Align BB */
  mav_BBAlign(bb2, mav_TDM_matrix[cur->tracker], bb);

  return 1;
}



/* Routine to identify a TDM cursor */

int mav_TDMCursorID(MAV_object *obj, char **id)
{
  *id= "TDM cursor";
  return 1;
}



/* Routine to return the userdef field of a TDM cursor */

int mav_TDMCursorGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_TDMCursor *cur = (MAV_TDMCursor *) mav_objectDataGet(obj);

  *ud= &cur->userdef;

  return 1;
}



/* Routine to return the surface params field of a TDM cursor */

int mav_TDMCursorGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_TDMCursor *cur = (MAV_TDMCursor *) mav_objectDataGet(obj);

  *sp= &cur->sp;

  return 1;
}



/* Routine to dump a TDM cursor */

int mav_TDMCursorDump(MAV_object *obj)
{
  MAV_TDMCursor *cur = (MAV_TDMCursor *) mav_objectDataGet(obj);

  printf("*** Dumping object %p - a MAV_TDMCursor with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("tracker %i\n", cur->tracker);
  printf("style %i\n", cur->style);
  mav_surfaceParamsPrint("surface params ", *cur->sp);
  printf("userdef %p\n", cur->userdef);

  return 1;
}



/* Routine to initialise cursor style 0 - a cross */

void mavlib_TDM_style0Init(void)
{
  MAV_cylinder *cyl1=(MAV_cylinder *) mav_malloc(sizeof(MAV_cylinder));
  MAV_cylinder *cyl2=(MAV_cylinder *) mav_malloc(sizeof(MAV_cylinder));
  MAV_cylinder *cyl3=(MAV_cylinder *) mav_malloc(sizeof(MAV_cylinder));
  MAV_BB bb;
  int i;

  /* Define the objects making up the shape */
  cyl1->nverts=6;
  cyl1->radius=0.25;
  cyl1->height=5.0;
  cyl1->endcap=1;
  cyl1->sp=mav_sp_current;
  cyl1->matrix=mav_matrixSet(0.0, 0.0, 0.0, 0.0, 0.0, 0.0);

  cyl2->nverts=6;
  cyl2->radius=0.25;
  cyl2->height=5.0;
  cyl2->endcap=1;
  cyl2->sp=mav_sp_current;
  cyl2->matrix=mav_matrixSet(0.0, 90.0, 0.0, 0.0, 0.0, 0.0);

  cyl3->nverts=6;
  cyl3->radius=0.25;
  cyl3->height=5.0;
  cyl3->endcap=1;
  cyl3->sp=mav_sp_current;
  cyl3->matrix=mav_matrixSet(0.0, 0.0, 90.0, 0.0, 0.0, 0.0);

  /* Register them */
  mavlib_TDM_cursorObj[0]= mav_objectNew(mav_class_cylinder, cyl1);
  mavlib_TDM_cursorObj[1]= mav_objectNew(mav_class_cylinder, cyl2);
  mavlib_TDM_cursorObj[2]= mav_objectNew(mav_class_cylinder, cyl3);

  /* Calculate cursor's BB */
  mav_BBCompInit(&mavlib_TDM_cursorBB[0]);
  for (i=0; i<3; i++) {
    mav_callbackBBExec(mav_win_all, mavlib_TDM_cursorObj[i], &bb);
    mav_BBCompBB(bb, &mavlib_TDM_cursorBB[0]);
  }
}



/* Routine to initialise cursor style 1 - a pointer */

void mavlib_TDM_style1Init(void)
{
  MAV_cylinder *cyl= (MAV_cylinder *) mav_malloc(sizeof(MAV_cylinder));
  MAV_cone *cone= (MAV_cone *) mav_malloc(sizeof(MAV_cone));
  MAV_BB bb;
  int i;

  /* Define the objects making up the shape */
  cyl->nverts=6;
  cyl->radius=0.25;
  cyl->height=3.0;
  cyl->endcap=1;
  cyl->sp=mav_sp_current;
  cyl->matrix=mav_matrixSet(0.0, 0.0, 0.0, 0.0, 0.0, 3.5);
    
  cone->nverts=6;
  cone->rt=0.5;
  cone->rb=0.0;
  cone->height=2.0;
  cone->endcap=1;
  cone->sp=mav_sp_current;
  cone->matrix=mav_matrixSet(0.0, 0.0, 0.0, 0.0, 0.0, 1.0);

  /* Register them */
  mavlib_TDM_cursorObj[3]= mav_objectNew(mav_class_cylinder, cyl);
  mavlib_TDM_cursorObj[4]= mav_objectNew(mav_class_cone, cone);

  /* Calculate cursor's BB */
  mav_BBCompInit(&mavlib_TDM_cursorBB[1]);
  for (i=3; i<5; i++) {
    mav_callbackBBExec(mav_win_all, mavlib_TDM_cursorObj[i], &bb);
    mav_BBCompBB(bb, &mavlib_TDM_cursorBB[1]);
  }
}



/* Routine to initialise cursor style 2 - a thick arrow */

void mavlib_TDM_style2Init(void)
{
  MAV_pyramid *pyr1= (MAV_pyramid *) mav_malloc(sizeof(MAV_pyramid));
  MAV_pyramid *pyr2= (MAV_pyramid *) mav_malloc(sizeof(MAV_pyramid));
  MAV_BB bb;
  int i;

  /* Define the objects making up the shape */
  pyr1->bot_size_x=1.0;
  pyr1->bot_size_y=1.0;
  pyr1->top_size_x=2.0;
  pyr1->top_size_y=1.0;
  pyr1->offset_x=0;
  pyr1->offset_y=0;
  pyr1->height=1.0;
  pyr1->sp=mav_sp_current;
  pyr1->matrix=mav_matrixSet(0.0, 0.0, 0.0, 0.0, 0.0, 0.5);

  pyr2->bot_size_x=0.0;
  pyr2->bot_size_y=1.0;
  pyr2->top_size_x=2.0;
  pyr2->top_size_y=1.0;
  pyr2->offset_x=0;
  pyr2->offset_y=0;
  pyr2->height=1.0;
  pyr2->sp=mav_sp_current;
  pyr2->matrix=mav_matrixSet(0.0, 0.0, 0.0, 0.0, 0.0, -0.5);

  /* Register them */
  mavlib_TDM_cursorObj[5]= mav_objectNew(mav_class_pyramid, pyr1);
  mavlib_TDM_cursorObj[6]= mav_objectNew(mav_class_pyramid, pyr2);

  /* Calculate cursor's BB */
  mav_BBCompInit(&mavlib_TDM_cursorBB[2]);
  for (i=5; i<7; i++) {
    mav_callbackBBExec(mav_win_all, mavlib_TDM_cursorObj[i], &bb);
    mav_BBCompBB(bb, &mavlib_TDM_cursorBB[2]);
  }
}



/* Routine to initialise cursor style 3 - an HMD */

void mavlib_TDM_style3Init(void)
{
  MAV_rtorus *rt1= (MAV_rtorus *) mav_malloc(sizeof(MAV_rtorus));
  MAV_rtorus *rt2= (MAV_rtorus *) mav_malloc(sizeof(MAV_rtorus));
  MAV_rtorus *rt3= (MAV_rtorus *) mav_malloc(sizeof(MAV_rtorus));
  MAV_box *box= (MAV_box *) mav_malloc(sizeof(MAV_box));
  MAV_cylinder *cyl1= (MAV_cylinder *) mav_malloc(sizeof(MAV_cylinder));
  MAV_cylinder *cyl2= (MAV_cylinder *) mav_malloc(sizeof(MAV_cylinder));
  MAV_BB bb;
  int i;

  /* Define the objects making up the shape */
  rt1->nchips=10;
  rt1->radius=2.0;
  rt1->height=0.25;
  rt1->width=0.025;
  rt1->angle=MAV_PI;
  rt1->sp=mav_sp_current;
  rt1->matrix=mav_matrixSet(0.0, 0.0, 0.0, 0.0, 0.0, 0.0);

  rt2->nchips=10;
  rt2->radius=2.0;
  rt2->height=0.25;
  rt2->width=0.025;
  rt2->angle=MAV_PI;
  rt2->sp=mav_sp_current;
  rt2->matrix=mav_matrixSet(0.0, 0.0, 90.0, 0.0, 0.0, 0.0);
  
  rt3->nchips=10;
  rt3->radius=2.0;
  rt3->height=0.25;
  rt3->width=0.025;
  rt3->angle=MAV_2_PI;
  rt3->sp=mav_sp_current;
  rt3->matrix=mav_matrixSet(0.0, 90.0, 0.0, 0.0, 0.0, 0.0);
  
  box->size.x=2.0;
  box->size.y=0.525;
  box->size.z=0.75;
  box->sp=mav_sp_current;
  box->matrix=mav_matrixSet(0.0, 0.0, 0.0, 0.0, -0.1375, -2.375);
    
  cyl1->nverts=6;
  cyl1->height=1.0;
  cyl1->radius=0.4;
  cyl1->sp=mav_sp_current;
  cyl1->endcap=1;    
  cyl1->matrix=mav_matrixSet(0.0, 0.0, 0.0, 0.5, -0.8, -2.4);
  
  cyl2->nverts=6;
  cyl2->height=1.0;
  cyl2->radius=0.4;
  cyl2->sp=mav_sp_current;
  cyl2->endcap=1;    
  cyl2->matrix=mav_matrixSet(0.0, 0.0, 0.0, -0.5, -0.8, -2.4);

  /* Register them */
  mavlib_TDM_cursorObj[7]= mav_objectNew(mav_class_rtorus, rt1);
  mavlib_TDM_cursorObj[8]= mav_objectNew(mav_class_rtorus, rt2);
  mavlib_TDM_cursorObj[9]= mav_objectNew(mav_class_rtorus, rt3);
  mavlib_TDM_cursorObj[10]= mav_objectNew(mav_class_box, box);
  mavlib_TDM_cursorObj[11]= mav_objectNew(mav_class_cylinder, cyl1);
  mavlib_TDM_cursorObj[12]= mav_objectNew(mav_class_cylinder, cyl2);

  /* Calculate cursor's BB */
  mav_BBCompInit(&mavlib_TDM_cursorBB[3]);
  for (i=7; i<13; i++) {
    mav_callbackBBExec(mav_win_all, mavlib_TDM_cursorObj[i], &bb);
    mav_BBCompBB(bb, &mavlib_TDM_cursorBB[3]);
  }
}



/* Routine to initalise the different cursor styles */

void mavlib_TDM_cursorInit(void)
{
  mavlib_TDM_style0Init();
  mavlib_TDM_style1Init();
  mavlib_TDM_style2Init();
  mavlib_TDM_style3Init();
}
