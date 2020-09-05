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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "maverik.h"
#include "city_macros.h"
#include "city_types.h"

/* warp the 2d grid to generate a more realistic city structure */
/* uses parametric bezier patch representation of grid */

typedef float BMatrix[4][4];

typedef struct {
  BMatrix Gx,Gy,Gz;
} BPatch;

extern int size;
BPatch bpatch;

static float
EvaluateGeometricConstraints
(float u, float v, BMatrix G)
{
  float utmp[1][4], gtmp[1][4], mtmp[1][4];
  float uu= u*u;
  float uuu= uu*u;
  float vv= v*v;
  float vvv= vv*v;

  utmp[0][0]= -uuu+3.0*uu-3.0*u+1.0;
  utmp[0][1]= 3.0*uuu-6.0*uu+3.0*u;
  utmp[0][2]= -3.0*uuu+3.0*uu;
  utmp[0][3]= uuu;
  gtmp[0][0]= utmp[0][0]*G[0][0] + utmp[0][1]*G[1][0] + utmp[0][2]*G[2][0] + utmp[0][3]*G[3][0];
  gtmp[0][1]= utmp[0][0]*G[0][1] + utmp[0][1]*G[1][1] + utmp[0][2]*G[2][1] + utmp[0][3]*G[3][1];
  gtmp[0][2]= utmp[0][0]*G[0][2] + utmp[0][1]*G[1][2] + utmp[0][2]*G[2][2] + utmp[0][3]*G[3][2];
  gtmp[0][3]= utmp[0][0]*G[0][3] + utmp[0][1]*G[1][3] + utmp[0][2]*G[2][3] + utmp[0][3]*G[3][3];
  mtmp[0][0]= -gtmp[0][0] + 3.0*gtmp[0][1] - 3.0*gtmp[0][2] + gtmp[0][3];
  mtmp[0][1]= 3.0*gtmp[0][0] - 6.0*gtmp[0][1] + 3.0*gtmp[0][2];
  mtmp[0][2]= -3.0*gtmp[0][0] + 3.0*gtmp[0][1];
  mtmp[0][3]= gtmp[0][0];
  return (mtmp[0][0]*vvv + mtmp[0][1]*vv + mtmp[0][2]*v + mtmp[0][3]);
}

MAV_vector
Calc_Grid_Position
(float u, float v)
{
  MAV_vector vec;

  vec.x= EvaluateGeometricConstraints (u,v, bpatch.Gx);
  vec.y= EvaluateGeometricConstraints (u,v, bpatch.Gy);
  vec.z= EvaluateGeometricConstraints (u,v, bpatch.Gz);
  return vec;
}

void
Init_Warp_Grid (void)
{
  int j,i;
  float radius;

  /* place vertices */
  for (j= 0; j< 4; j++)
    for (i= 0; i< 4; i++)
      {
	if ((i == 0 && j == 0) || (i == 3 && j == 3) || (i == 3 && j == 0) || (i == 0 && j == 3))
	  radius= 0.0;
	else
	  radius= size*BLOCK_WIDTH*0.5*mav_random();

	bpatch.Gx[j][i]= size*BLOCK_WIDTH*j/3.0+radius*(mav_random()-0.5);
	bpatch.Gy[j][i]= 0.0;
	bpatch.Gz[j][i]= size*BLOCK_WIDTH*i/3.0+radius*(mav_random()-0.5);
      }
}
