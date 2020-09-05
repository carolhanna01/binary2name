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
#include "mavlib_avatar.h"
#include <math.h>
#include <stdio.h>

#define MAX_WALK_SPD 2.0


/* Routine to animate an avatar */

void mav_avatarAnimate(MAV_avatar *avatar) {

  MAVLIB_avatarCurves *curves= mavlib_avatarCurve1;
  float t, value, v, val, angle, scl, abs_spd;
  int i, j;
  static int walking= MAV_TRUE, toggle_walk= MAV_FALSE;
  static float old_speed= 0, old_value= 0;

  abs_spd= fabs(avatar->speed);
  /* set flag to toggle walk/run when speed crosses MAX_WALK_SPD
boundary */
  if ((abs_spd > MAX_WALK_SPD && old_speed <= MAX_WALK_SPD) ||
      (abs_spd <= MAX_WALK_SPD && old_speed > MAX_WALK_SPD))
    toggle_walk= MAV_TRUE;

  old_speed= abs_spd;

  /* calculate angle scale for walk speed */
  scl= fabs(0.85*avatar->speed);

  value= fmod(avatar->time+avatar->last_time,1.0);

  /* only toggle walk/run when left foot hits ground */
  if (toggle_walk && old_value < 0.4 && value > 0.4) {
    walking= walking ? MAV_FALSE : MAV_TRUE;
    toggle_walk= MAV_FALSE;
  }
  old_value= value;

  value-= avatar->offset;

  for (i=0; i<curves->num_joints; i++) {
    /* account for joint cycle offset */
    v= value-curves->joints[i].offset;
    while (v < 0.0) v += 1.0;
    while (v > 1.0) v -= 1.0;

    /* move t to [start..end] */
    if (avatar->speed < 0) t=
curves->joints[i].end-v*(curves->joints[i].end-curves->joints[i].start);

    else t=
curves->joints[i].start+v*(curves->joints[i].end-curves->joints[i].start);

    for (j=0; j<curves->joints[i].num_beziers; j++) {

      if (t>=curves->joints[i].beziers[j].value[0] &&
t<=curves->joints[i].beziers[j].value[3]) {

 /* move to [0..1] */
 val=
(t-curves->joints[i].beziers[j].value[0])/(curves->joints[i].beziers[j].value[3]-curves->joints[i].beziers[j].value[0]);

 /* evaluate bezier */
 angle=
scl*90.0*((1.0-val)*(1.0-val)*(1.0-val)*curves->joints[i].beziers[j].angle[0]+

    3.0*val*(1.0-val)*(1.0-val)*curves->joints[i].beziers[j].angle[1]+
    3.0*val*val*(1.0-val)*curves->joints[i].beziers[j].angle[2]+
    val*val*val*curves->joints[i].beziers[j].angle[3]);

 if (curves->joints[i].part_num == -1)
 {
   /* update vertical height */
   if (walking) avatar->vertical= mav_matrixSet (0.0,0.0,0.0,
0.0,-0.055*angle/105.0-1.7,0.0);
   else avatar->vertical= mav_matrixSet (0.0,0.0,0.0,
0.0,0.055*angle/105.0-1.77,0.0);
 }
 else
 {
   /* update rotation angle */
   switch (curves->joints[i].axis)
   {
   case ROLL :
     avatar->part[curves->joints[i].part_num]->rotation= mav_matrixSet
(-angle,0.0,0.0, 0.0,0.0,0.0);
     break;
   case PITCH :
     avatar->part[curves->joints[i].part_num]->rotation= mav_matrixSet
(0.0,-angle,0.0, 0.0,0.0,0.0);
     break;
   case YAW :
     avatar->part[curves->joints[i].part_num]->rotation= mav_matrixSet
(0.0,0.0,-angle, 0.0,0.0,0.0);
     break;
   }
 }

 j= curves->joints[i].num_beziers;
      }
    }
  }
}



/* Routine to move an avatar */

void mav_avatarMove(MAV_avatar *avatar)
{
  MAV_vector pos, dir;
  float vel;

  /* get last position from matrix */
  pos= mav_matrixXYZGet(avatar->matrix);

  /* get direction of travel (-z axis) */
  dir.x= -avatar->matrix.mat[0][2];
  dir.y= -avatar->matrix.mat[1][2];
  dir.z= -avatar->matrix.mat[2][2];
  dir= mav_vectorNormalize(dir);

  /* work out the new position */
  vel= avatar->time*avatar->speed*mav_matrixScaleGet(avatar->matrix);
  pos= mav_vectorAdd(pos, mav_vectorScalar(dir, vel));

  /* and update his matrix accordingly */
  avatar->matrix= mav_matrixXYZSet (avatar->matrix, pos);
}
