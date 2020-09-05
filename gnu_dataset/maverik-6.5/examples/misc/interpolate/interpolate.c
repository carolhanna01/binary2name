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



int main(int argc, char *argv[]) 
{
  MAV_box start, end, interp;
  MAV_SMS *sms1, *sms2;
  MAV_vector vec, pos;
  MAV_quaternion orient;
  float fac=0;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);
  
  /* Define two boxes with a random position and orientation to act as start at end points */
  start.size= mav_vectorSet(2, 2, 2);
  start.sp= mav_sp_default;
  start.matrix= mav_matrixSet(mav_random()*360, mav_random()*360, mav_random()*360,
		    mav_random()*20-10, mav_random()*20-10, mav_random()*20-10);

  end.size= mav_vectorSet(2, 2, 2);
  end.sp= mav_sp_default;
  end.matrix= mav_matrixSet(mav_random()*360, mav_random()*360, mav_random()*360,
		    mav_random()*20-10, mav_random()*20-10, mav_random()*20-10);

  /* Define another box which we will interpolate between start and end points */
  interp.size= mav_vectorSet(2, 2, 2);
  interp.sp= mav_sp_default;
  interp.matrix= MAV_ID_MATRIX;

  /* Define two SMSs and put the objects in them */
  sms1= mav_SMSObjListNew();
  mav_SMSObjectAdd(sms1, mav_objectNew(mav_class_box, &start));
  mav_SMSObjectAdd(sms1, mav_objectNew(mav_class_box, &end));

  sms2= mav_SMSObjListNew();
  mav_SMSObjectAdd(sms2, mav_objectNew(mav_class_box, &interp));

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Main loop */
  while (1) {

    /* Interpolation factor */
    fac+=0.01;
    if (fac>1.0) fac=0.0;

    /* Calculate vector between start and end point */
    vec= mav_vectorSub(mav_matrixXYZGet(end.matrix), mav_matrixXYZGet(start.matrix));

    /* Calculate interpolated position along this vector */
    pos= mav_vectorAdd(mav_matrixXYZGet(start.matrix), mav_vectorScalar(vec, fac));

    /* Calculate interpolated orientation between start and end point - quaternions are best for this */
    orient= mav_quaternionInterpolate(mav_quaternionMatrixConvert(start.matrix), mav_quaternionMatrixConvert(end.matrix), fac);

    /* Set transformation of interpolated box */
    interp.matrix= mav_matrixQuaternionConvert(orient);
    interp.matrix= mav_matrixXYZSet(interp.matrix, pos);

    /* Check for, and act on, any events */
    mav_eventsCheck();

    /* Do what needs to be done at the start of a frame */
    mav_frameBegin();

    /* Display the objects to all windows */
    mav_windowPolygonModeSet(mav_win_all, MAV_POLYGON_LINE);
    mav_SMSDisplay(mav_win_all, sms1);
    mav_windowPolygonModeSet(mav_win_all, MAV_POLYGON_FILL);

    mav_SMSDisplay(mav_win_all, sms2);

    /* Do what needs to be done at the end of a frame */
    mav_frameEnd();
  }
}


