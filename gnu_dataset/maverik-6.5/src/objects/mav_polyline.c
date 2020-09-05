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

MAV_class *mav_class_polyline;



/* Routine to render a polyline */

int mav_polylineDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_polyline *line= (MAV_polyline *) mav_objectDataGet(obj);
  int i,j;

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(line->matrix);

  for (i=0; i<line->nlines; i++) {

    /* Set the correct colouring */
    mav_surfaceParamsUse(line->sp[i]); 

    /* Open or closed lines */
    if (line->closed[i]) 
    {
      mav_gfxLineClosedBegin();
    }
    else
    {
      mav_gfxLineBegin();
    }

    /* draw vertices */
    for (j=0; j<line->np[i]; j++) mav_gfxVertex(line->vert[i][j]);

    if (line->closed[i]) 
    {
      mav_gfxLineClosedEnd();
    }
    else
    {
      mav_gfxLineEnd();
    }
  }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;
}



/* Routine to calculate the bounding box of a polyline (quick but overestimates) */

int mav_polylineBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_polyline *line = (MAV_polyline *) mav_objectDataGet(obj);
  int i,j;

/* Local coordinate frame BB of points */

  if (line->nlines>0)
  {
    mav_BBCompInit(bb);
    for (i=0; i<line->nlines; i++) {
      for (j=0; j<line->np[i]; j++) mav_BBCompPt(line->vert[i][j], bb);
    }

/* Global axis align it */ 

    mav_BBAlign(*bb, line->matrix, bb);
    
    return 1;
  }
  else
  {
    return 0;
  }
}



/* Another routine to calculate the bounding box of a polyline (slow but accurate) */

int mav_polylineBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_polyline *line = (MAV_polyline *) mav_objectDataGet(obj);
  int i,j;

/* BB of points after matrix transformation  */

  if (line->nlines>0)
  {
    mav_BBCompInit(bb);
    for (i=0; i<line->nlines; i++) {
      for (j=0; j<line->np[i]; j++) mav_BBCompPt(mav_vectorMult(line->vert[i][j], line->matrix), bb);
    }

    return 1;
  }
  else
  {
    return 0;
  }
}



/* Routine to identify a polyline */

int mav_polylineID(MAV_object *o, char **id)
{
  *id= "polyline";

  return 1;
}



/* Routine to return the userdef field of a polyline */

int mav_polylineGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_polyline *line = (MAV_polyline *) mav_objectDataGet(obj);

  *ud= &line->userdef;

  return 1;
}



/* Routine to return the matrix field of a polyline */

int mav_polylineGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_polyline *line = (MAV_polyline *) mav_objectDataGet(obj);

  *mat= &line->matrix;

  return 1;
}



/* Routine to return the surface params field of a polyline */

int mav_polylineGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_polyline *line = (MAV_polyline *) mav_objectDataGet(obj);

  *sp= &line->sp[0];

  return 1;
}



/* Routine to dump a polyline */

int mav_polylineDump(MAV_object *obj)
{
  MAV_polyline *line = (MAV_polyline *) mav_objectDataGet(obj);
  int i,j;
  
  printf("*** Dumping object %p - a MAV_polyline with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("nlines %i\n", line->nlines);

  for (i=0; i<line->nlines; i++) {
    printf("np[%i] %i\n", i, line->np[i]);
    for (j=0; j<line->np[i]; j++) {
      printf("vert[%i][%i] ", i,j);
      mav_vectorPrint("", line->vert[i][j]);
    }
    printf("surface params[%i] ", i);
    mav_surfaceParamsPrint("", *line->sp[i]);
  }
  mav_matrixPrint("matrix\n", line->matrix);
  printf("userdef %p\n", line->userdef);

  return 1;
}
