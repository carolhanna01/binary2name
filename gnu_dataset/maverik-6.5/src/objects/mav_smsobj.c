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


#include "mavlib_objects.h"
#include <stdio.h>

MAV_class *mav_class_SMSObj;



/* Routine to render an SMS object */

int mav_SMSObjDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_SMSObj *smsobj = (MAV_SMSObj *) mav_objectDataGet(obj);
  MAV_drawInfo di2;

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(smsobj->matrix);

  if (di)
  {
    /* Transform the drawinfo */

    di2= mav_drawInfoTransFrame(*di, smsobj->matrix);

    /* Display the SMS */

    mav_SMSDisplayUsingDrawInfo(mav_win_current, smsobj->sms, di2);
  }
  else
  {
    /* Display the SMS */

    mav_SMSDisplayUnCulled(mav_win_current, smsobj->sms);
  }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;
}



/* Routine to calculate the bounding box of an SMS object */

int mav_SMSObjBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_SMSObj *smsobj = (MAV_SMSObj *) mav_objectDataGet(obj);
  MAV_object *o;
  MAV_BB tmpbb;
  int rv=0;

/* Initialise composite BB */
 
  mav_BBCompInit(bb);

/* Step through SMS calculating BB's of the objects */

  mav_SMSCallbackPointerResetExec(smsobj->sms);
  while (mav_SMSCallbackObjectNextExec(smsobj->sms, &o)) {
    if (mav_callbackBBExec(mav_win_current, o, &tmpbb)) {
      mav_BBCompBB(tmpbb, bb);
      rv=1;
    }
  }

/* Account for transformation */

  if (rv) mav_BBAlign(*bb, smsobj->matrix, bb);

  return rv;
}



/* Routine to intersect an SMS object */

int mav_SMSObjIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_SMSObj *smsobj=(MAV_SMSObj *) mav_objectDataGet(obj);
  MAV_line ln2; 
  MAV_object *hobj;
  MAV_objectIntersection o2, smallest;
  int flag;

  o->pt1=-100.0;
  o->pt2=-100.0;
  smallest.pt1=MAV_INFINITY;
  smsobj->selobj=NULL;
  flag=0;

/* 
   Use inverse of object transformation to transform the start position
   and direction of the intersection ray (originally in global coords) into 
   the objects local coords.
*/

  ln2= mav_lineTransFrame(*ln, smsobj->matrix);

/* Find closest intersection */

  if (mav_SMSCallbackIntersectExec(smsobj->sms, mav_win_current, ln2, &o2, &hobj)) {
    flag=1;
    if (o2.pt1<smallest.pt1) {
      smallest=o2;
      smsobj->selobj=hobj;
    }
  }

  if (!flag) return (MAV_FALSE);

/* Account for scale */ 

  *o=smallest;
  o->pt1*=mav_matrixScaleGet(smsobj->matrix);
  o->pt2*=mav_matrixScaleGet(smsobj->matrix);

  return(1);
}



/* Routine to identify an SMS object */

int mav_SMSObjID(MAV_object *obj, char **id)
{
  *id= "SMS object";
  return 1;
}



/* Routine to return the userdef field of an SMS object */

int mav_SMSObjGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_SMSObj *smsobj = (MAV_SMSObj *) mav_objectDataGet(obj);

  *ud= &smsobj->userdef;

  return 1;
}



/* Routine to return the matrix field of an SMS object */

int mav_SMSObjGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_SMSObj *smsobj = (MAV_SMSObj *) mav_objectDataGet(obj);

  *mat= &smsobj->matrix;

  return 1;
}



/* Routine to return the surface params field of an SMS object */

int mav_SMSObjGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_SMSObj *smsobj = (MAV_SMSObj *) mav_objectDataGet(obj);
  MAV_object *o;

  mav_SMSCallbackPointerResetExec(smsobj->sms);
  
  if (mav_SMSCallbackObjectNextExec(smsobj->sms, &o))
  {
    return mav_callbackGetSurfaceParamsExec(mav_win_current, o, sp);
  }
  else
  {
    return 0;
  }
}



/* Routine to dump an SMS object */

int mav_SMSObjDump(MAV_object *obj)
{
  MAV_SMSObj *smsobj = (MAV_SMSObj *) mav_objectDataGet(obj);

  printf("*** Dumping object %p - a MAV_SMSObj with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("dumping an SMS object\n");
  mav_matrixPrint("matrix\n", smsobj->matrix);
  printf("userdef %p\n", smsobj->userdef);

  return 1;
}
