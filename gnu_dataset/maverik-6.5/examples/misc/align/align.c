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

int main(int argc, char *argv[])
{
  MAV_box box;
  MAV_object *obj, *spo;
  MAV_SMS *sms;
  MAV_vector pt1, pt2;
  float ang=0;
  MAV_sphere sph;
  MAV_vector v, vn1, vn2;
  MAV_quaternion q;

/* Initialise the Maverik system */
  mav_initialise(&argc, argv);

/* Define the box */
  box.size.x=3.5; /* Size */
  box.size.y=0.5;
  box.size.z=0.5;
  box.matrix= MAV_ID_MATRIX; /* Position and orientation */
  box.sp= mav_sp_default; /* Surface parameters, i.e. colour */

/* Register as a Maverik object */
  obj= mav_objectNew(mav_class_box, &box);

/* Create an SMS and add object to it */
  sms= mav_SMSObjListNew();
  mav_SMSObjectAdd(sms, obj);

/* Define a sphere */
  sph.radius=0.1;
  sph.nverts=5;
  sph.nchips=10;
  sph.sp= mav_sp_default;
  spo= mav_objectNew(mav_class_sphere, &sph);

/* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

/* Main loop */
  while (1) {

/* Calculate two "random" moving points */
    ang+=0.01;

    pt1.x= 2.0*sin(ang/2.0);
    pt1.y= 2.0*cos(ang);
    pt1.z= 3.0*sin(ang);

    pt2.x= 5.0*cos(ang);
    pt2.y= 5.0*sin(ang*3.0);
    pt2.z= 7.0*cos(ang);

/* Align box's X axis with the vector joining the two points */
    vn1= mav_vectorSet(1,0,0);
    v= mav_vectorSub(pt2, pt1);
    vn2= mav_vectorNormalize(v);
    v= mav_vectorAdd(pt1, mav_vectorScalar(v, 0.5));
    
    q= mav_quaternionSet(mav_vectorNormalize(mav_vectorCrossProduct(vn1, vn2)), acos(mav_vectorDotProduct(vn1, vn2))/MAV_PI_OVER_180);

    box.matrix= mav_matrixXYZSet(mav_matrixQuaternionConvert(q), v);

/* Check for, and act on, any events */
    mav_eventsCheck();

/* Do what needs to be done at the start of a frame */
    mav_frameBegin();

/* Display the SMS to all windows */
    mav_SMSDisplay(mav_win_all, sms);

/* Display the two points as spheres */
    sph.matrix= mav_matrixXYZSet(MAV_ID_MATRIX, pt1);
    mav_callbackDrawExec(mav_win_all, spo, NULL);

    sph.matrix= mav_matrixXYZSet(MAV_ID_MATRIX, pt2);
    mav_callbackDrawExec(mav_win_all, spo, NULL);

/* Do what needs to be done at the end of a frame */
    mav_frameEnd();
  }
}
