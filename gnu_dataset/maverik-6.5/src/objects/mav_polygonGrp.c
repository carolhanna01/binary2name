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

MAV_class *mav_class_polygonGrp;



/* Routine to render a polygon group */

int mav_polygonGrpDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_polygonGrp *polyGrp = (MAV_polygonGrp *) mav_objectDataGet(obj);
  MAV_vector curr_norm;
  int i,j;

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(polyGrp->matrix);

/* Set the current normal to be an impossible value so it gets set the first time round */

  curr_norm.x = -99.0;
  curr_norm.y = -99.0;
  curr_norm.z = -99.0;

  for (i=0; i<polyGrp->npolys; i++) {

/* Set the correct colouring */

    mav_surfaceParamsUse(polyGrp->sp[i]); 

    if (polyGrp->sp[i]->mode==MAV_MATERIAL || polyGrp->sp[i]->mode>=MAV_LIT_TEXTURE) {

/* Only need to set the normal if it was different from last time around */

      if (curr_norm.x!=polyGrp->norm[i].x || curr_norm.y!=polyGrp->norm[i].y || curr_norm.z!=polyGrp->norm[i].z) {
	curr_norm= polyGrp->norm[i];
	mav_gfxNormal(polyGrp->norm[i]);
      }
    }

    mav_gfxPolygonBegin();
    for (j=0; j<polyGrp->np[i]; j++) {  
      if (polyGrp->sp[i]->mode>=MAV_TEXTURE) mav_gfxTexCoord(polyGrp->tex[i][j]);
      mav_gfxVertex(polyGrp->vert[i][j]);
    }
    mav_gfxPolygonEnd();
  }

  /* Display normals if applicable */
  if (mav_opt_drawNormals<1000000) {

    MAV_surfaceParams sp;
    sp.mode= MAV_COLOUR;
    sp.colour= MAV_COLOUR_RED;
    sp.material= 0;
    sp.texture= 0;
    mav_surfaceParamsUse(&sp);

    for (i=0; i<polyGrp->npolys; i++) {
      
      /* Find center of polygon */
      MAV_vector cent= mav_vectorSet(0,0,0);
      for (j=0; j<polyGrp->np[i]; j++) cent= mav_vectorAdd(cent, polyGrp->vert[i][j]);
      cent= mav_vectorScalar(cent, 1.0/polyGrp->np[i]);

      /* Draw normal */
      mav_gfxLineBegin();
      mav_gfxVertex(cent);
      mav_gfxVertex(mav_vectorAdd(cent, mav_vectorScalar(polyGrp->norm[i], mav_opt_drawNormals)));
      mav_gfxLineEnd();
    }
  }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;
}



/* Routine to calculate the bounding box of a polygon group (quick but overestimates) */

int mav_polygonGrpBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_polygonGrp *polyGrp = (MAV_polygonGrp *) mav_objectDataGet(obj);  
  int i,j;

/* Local coordinate frame BB of points */

  if (polyGrp->npolys >0)
  {
    mav_BBCompInit(bb);

    for (i=0; i<polyGrp->npolys; i++) {
      for (j=0; j<polyGrp->np[i]; j++) mav_BBCompPt(polyGrp->vert[i][j], bb);
    }

/* Global axis align it */ 

    mav_BBAlign(*bb, polyGrp->matrix, bb);
    
    return 1;
  }
  else
  {
    return 0;
  }
}



/* Another routine to calculate the bounding box of a polygon group (slow but accurate) */

int mav_polygonGrpBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_polygonGrp *polyGrp = (MAV_polygonGrp *) mav_objectDataGet(obj);  
  int i,j;

/* BB of points after matrix transformation  */

  if (polyGrp->npolys >0)
  {
    mav_BBCompInit(bb);

    for (i=0; i<polyGrp->npolys; i++) {
      for (j=0; j<polyGrp->np[i]; j++) mav_BBCompPt(mav_vectorMult(polyGrp->vert[i][j], polyGrp->matrix), bb);
    }
    
    return 1;
  }
  else
  {
    return 0;
  }
}



/* Routine to intersect a polygon group */

int mav_polygonGrpIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_polygonGrp *polyGrp = (MAV_polygonGrp *) mav_objectDataGet(obj);
  MAV_line ln2;
  MAV_polygon apoly;
  MAV_objectIntersection polyInt[100];
  int i, j, nhits;

  o->pt1=-100.0;
  o->pt2=-100.0;

/* Rotate and translate pt and dir so that the polygrp is centered and axis aligned */

  ln2= mav_lineTransFrame(*ln, polyGrp->matrix);

  nhits=0;

  for (i=0; i<polyGrp->npolys; i++) {

    apoly.np = polyGrp->np[i];
    apoly.vert = (MAV_vector *) mav_malloc(apoly.np*sizeof(MAV_vector));
    apoly.norm = polyGrp->norm[i];
    for (j=0; j<polyGrp->np[i]; j++) apoly.vert[j] = polyGrp->vert[i][j];

    if (mav_linePolygonIntersection(&apoly, ln2, &polyInt[nhits])) nhits++;

    mav_free(apoly.vert);
    
    if (nhits >= 100) {
      fprintf(stderr, "Error: more than 100 intersection in mav_polyGrpIntersect\n");
      return(MAV_FALSE);
    }
  }
  
/* Sort intersection and return the cloest */

  if (nhits == 0) return (MAV_FALSE);

  j=0;
  for (i=1; i<nhits; i++) {
    if (polyInt[i].pt1<polyInt[j].pt1) j=i;
  }

/* Compensate for scale */

  o->pt1=polyInt[j].pt1*mav_matrixScaleGet(polyGrp->matrix);
  o->pt2=o->pt1;

  return(1);
}



/* Routine to identify a polygon group */

int mav_polygonGrpID(MAV_object *o, char **id)
{
  *id= "polygon group";

  return 1;
}



/* Routine to return the userdef field of a polygon group */

int mav_polygonGrpGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_polygonGrp *polyGrp = (MAV_polygonGrp *) mav_objectDataGet(obj);

  *ud= &polyGrp->userdef;

  return 1;
}



/* Routine to return the matrix field of a polygon group */

int mav_polygonGrpGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_polygonGrp *polyGrp = (MAV_polygonGrp *) mav_objectDataGet(obj);

  *mat= &polyGrp->matrix;

  return 1;
}



/* Routine to return the surface params field of a polygon group */

int mav_polygonGrpGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_polygonGrp *polyGrp = (MAV_polygonGrp *) mav_objectDataGet(obj);

  *sp= &polyGrp->sp[0];

  return 1;
}



/* Routine to dump a polygon group */

int mav_polygonGrpDump(MAV_object *obj)
{
  MAV_polygonGrp *polyGrp = (MAV_polygonGrp *) mav_objectDataGet(obj);
  int i,j;

  printf("*** Dumping object %p - a MAV_polygonGrp with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("npolys %i\n", polyGrp->npolys);

  for (i=0; i<polyGrp->npolys; i++) {
    printf("np[%i] %i\n", i, polyGrp->np[i]);
    printf("norm[%i] ", i);
    mav_vectorPrint("", polyGrp->norm[i]);

    for (j=0; j<polyGrp->np[i]; j++) {
      if (polyGrp->sp[i]->mode>=MAV_TEXTURE) {
	printf("tex[%i][%i] ", i, j);
	mav_texCoordPrint("", polyGrp->tex[i][j]);
      }      
      printf("vert[%i][%i] ", i, j);
      mav_vectorPrint("", polyGrp->vert[i][j]);
    }
    
    printf("surface params[%i] ", i);
    mav_surfaceParamsPrint("", *polyGrp->sp[i]);
  }

  mav_matrixPrint("matrix\n", polyGrp->matrix);
  printf("userdef %p\n", polyGrp->userdef);

  return 1;
}
