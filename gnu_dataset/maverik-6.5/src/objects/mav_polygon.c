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

MAV_class *mav_class_polygon;



/* Routine to render a polygon */

int mav_polygonDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_polygon *apoly = (MAV_polygon *) mav_objectDataGet(obj);
  int i;

/* Set the correct colouring */

  mav_surfaceParamsUse(apoly->sp);

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(apoly->matrix);

  if (apoly->sp->mode==MAV_MATERIAL || apoly->sp->mode>=MAV_LIT_TEXTURE) mav_gfxNormal(apoly->norm);

  mav_gfxPolygonBegin();
  for (i=0; i<apoly->np; i++) {
    if (apoly->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(apoly->tex[i]);
    mav_gfxVertex(apoly->vert[i]);
  }
  mav_gfxPolygonEnd();

  /* Display normal if applicable */
  if (mav_opt_drawNormals<1000000) {

    MAV_vector cent= mav_vectorSet(0,0,0);
    MAV_surfaceParams sp;
    sp.mode= MAV_COLOUR;
    sp.colour= MAV_COLOUR_RED;
    sp.material= 0;
    sp.texture= 0;
    mav_surfaceParamsUse(&sp);

    /* Find center of polygon */
    for (i=0; i<apoly->np; i++) cent= mav_vectorAdd(cent, apoly->vert[i]);
    cent= mav_vectorScalar(cent, 1.0/apoly->np);

    /* Draw normal */
    mav_gfxLineBegin();
    mav_gfxVertex(cent);
    mav_gfxVertex(mav_vectorAdd(cent, mav_vectorScalar(apoly->norm, mav_opt_drawNormals)));
    mav_gfxLineEnd();
  }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;
}



/* Routine to calculate the bounding box of a polygon (quick but overestimates) */

int mav_polygonBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_polygon *apoly = (MAV_polygon *) mav_objectDataGet(obj);
  int i;

/* Local coordinate frame BB of points */

  if (apoly->np >0 )
  {
    mav_BBCompInit(bb);
    for (i=0; i<apoly->np; i++) mav_BBCompPt(apoly->vert[i], bb);

/* Global axis align it */ 

    mav_BBAlign(*bb, apoly->matrix, bb);
    
    return 1;
  }
  else
  {
    return 0;
  }
}



/* Another routine to calculate the bounding box of a polygon (slow but accurate) */

int mav_polygonBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_polygon *apoly = (MAV_polygon *) mav_objectDataGet(obj);
  int i;

/* BB of points after matrix transformation  */

  if (apoly->np >0 )
  {
    mav_BBCompInit(bb);
    for (i=0; i<apoly->np; i++) mav_BBCompPt(mav_vectorMult(apoly->vert[i], apoly->matrix), bb);

    return 1;
  }
  else
  {
    return 0;
  }
}



/* Routine to intersect a polygon */

int mav_polygonIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_polygon *apoly = (MAV_polygon *) mav_objectDataGet(obj);
  MAV_line ln2;
  int rv= MAV_FALSE;

/* Rotate and translate line so that the polygon is centered and axis aligned */

  ln2= mav_lineTransFrame(*ln, apoly->matrix);

  rv= mav_linePolygonIntersection(apoly, ln2, o);

  if (rv) {

/* Compensate for scale and set out distance to equal in distance */

    o->pt1*= mav_matrixScaleGet(apoly->matrix);
    o->pt2= o->pt1;
  }

  return(rv);
}



/* Routine to identify a polygon */

int mav_polygonID(MAV_object *o, char **id)
{
  *id= "polygon";

  return 1;
}



/* Routine to return the userdef field of a polygon */

int mav_polygonGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_polygon *apoly = (MAV_polygon *) mav_objectDataGet(obj);

  *ud= &apoly->userdef;

  return 1;
}



/* Routine to return the matrix field of a polygon */

int mav_polygonGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_polygon *apoly = (MAV_polygon *) mav_objectDataGet(obj);

  *mat= &apoly->matrix;

  return 1;
}



/* Routine to return the surface params field of a polygon */

int mav_polygonGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_polygon *apoly = (MAV_polygon *) mav_objectDataGet(obj);

  *sp= &apoly->sp;

  return 1;
}



/* Routine to dump a polygon */

int mav_polygonDump(MAV_object *obj)
{
  MAV_polygon *apoly = (MAV_polygon *) mav_objectDataGet(obj);
  int i;

  printf("*** Dumping object %p - a MAV_polygon with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("np %i\n", apoly->np);
  mav_vectorPrint("norm ", apoly->norm);

  for (i=0; i<apoly->np; i++) {
    if (apoly->sp->mode>=MAV_TEXTURE) {
      printf("tex[%i] ", i);
      mav_texCoordPrint("", apoly->tex[i]);
    }
    printf("vert[%i] ", i);
    mav_vectorPrint("", apoly->vert[i]);
  }

  mav_surfaceParamsPrint("surface params ", *apoly->sp);
  mav_matrixPrint("matrix\n", apoly->matrix);
  printf("userdef %p\n", apoly->userdef);

  return 1;
}
