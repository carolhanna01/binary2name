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

MAV_class *mav_class_facet;



/* Routine to render a facet */

int mav_facetDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_facet *facet = (MAV_facet *) mav_objectDataGet(obj);
  MAV_vector curr_norm;
  int i,j;

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(facet->matrix);

/* Set the current normal to be an impossible value so it gets set the first time round */

  curr_norm.x = -99.0;
  curr_norm.y = -99.0;
  curr_norm.z = -99.0;

  for (i=0; i<facet->npolys; i++) {

/* Set the correct colouring */

    mav_surfaceParamsUse(facet->sp[i]); 

    mav_gfxPolygonBegin();
    for (j=0; j<facet->np[i]; j++) {  
      if (facet->sp[i]->mode==MAV_MATERIAL || facet->sp[i]->mode>=MAV_LIT_TEXTURE) {

/* Only need to set the normal if it was different from last time around */

	if (curr_norm.x!=facet->norm[i][j].x || curr_norm.y!=facet->norm[i][j].y || curr_norm.z!=facet->norm[i][j].z) {
	  curr_norm= facet->norm[i][j];
	  mav_gfxNormal(facet->norm[i][j]);
	}
      }

      if (facet->sp[i]->mode>=MAV_TEXTURE) mav_gfxTexCoord(facet->tex[i][j]);

      mav_gfxVertex(facet->vert[i][j]);
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

    for (i=0; i<facet->npolys; i++) {
      for (j=0; j<facet->np[i]; j++) {  
	mav_gfxLineBegin();
	mav_gfxVertex(facet->vert[i][j]);
	mav_gfxVertex(mav_vectorAdd(facet->vert[i][j], mav_vectorScalar(facet->norm[i][j], mav_opt_drawNormals)));
	mav_gfxLineEnd();
      }
    }
  }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;
}



/* Routine to calculate the bounding box of a facet (quick but overestimates) */

int mav_facetBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_facet *facet = (MAV_facet *) mav_objectDataGet(obj);  
  int i,j;

/* Local coordinate frame BB of points */

  if (facet->npolys >0)
  {
    mav_BBCompInit(bb);

    for (i=0; i<facet->npolys; i++) {
      for (j=0; j<facet->np[i]; j++) mav_BBCompPt(facet->vert[i][j], bb);
    }

/* Global axis align it */ 

    mav_BBAlign(*bb, facet->matrix, bb);
    
    return 1;
  }
  else
  {
    return 0;
  }
}



/* Another routine to calculate the bounding box of a facet (slow but accurate) */

int mav_facetBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_facet *facet = (MAV_facet *) mav_objectDataGet(obj);  
  int i,j;

/* BB of points after matrix transformation */

  if (facet->npolys >0)
  {
    mav_BBCompInit(bb);

    for (i=0; i<facet->npolys; i++) {
      for (j=0; j<facet->np[i]; j++) mav_BBCompPt(mav_vectorMult(facet->vert[i][j], facet->matrix), bb);
    }
    
    return 1;
  }
  else
  {
    return 0;
  }
}



/* Routine to intersect a facet */

int mav_facetIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_facet *facet = (MAV_facet *) mav_objectDataGet(obj);
  MAV_line ln2;  
  MAV_polygon apoly;
  MAV_objectIntersection polyInt[100];
  MAV_vector vec1, vec2;
  int i, j, nhits;

  o->pt1=-100.0;
  o->pt2=-100.0;

/* Rotate and translate pt and dir so that the facet is centered and axis aligned */

  ln2= mav_lineTransFrame(*ln, facet->matrix);

  nhits=0;

/* Make a polygon for each part of the facet */

  for (i=0; i<facet->npolys; i++) {

    if (facet->np[i]>0) {
      apoly.np = facet->np[i];

      apoly.vert = (MAV_vector *) mav_malloc(apoly.np*sizeof(MAV_vector));
      for (j=0; j<facet->np[i]; j++) apoly.vert[j] = facet->vert[i][j];

/* Cross product vertices to get the normals */
    
      vec1= mav_vectorNormalize(mav_vectorSub(apoly.vert[0], apoly.vert[1]));
      vec2= mav_vectorNormalize(mav_vectorSub(apoly.vert[2], apoly.vert[1]));
      apoly.norm= mav_vectorNormalize(mav_vectorCrossProduct(vec1, vec2));

/* Intersect the polygon */

      if (mav_linePolygonIntersection(&apoly, ln2, &polyInt[nhits])) nhits++;

      mav_free(apoly.vert);
    
      if (nhits >= 100) {
	fprintf(stderr, "Error: more than 100 intersection in mav_facetIntersect\n");
	return(MAV_FALSE);
      }
    }
  }
  
/* Sort intersection and return the closest */

  if (nhits == 0) return (MAV_FALSE);

  j=0;
  for (i=1; i<nhits; i++) {
    if (polyInt[i].pt1<polyInt[j].pt1) j=i;
  }

/* Compensate for scale */

  o->pt1=polyInt[j].pt1*mav_matrixScaleGet(facet->matrix);
  o->pt2=o->pt1;

  return(1);
}



/* Routine to identify a facet */

int mav_facetID(MAV_object *o, char **id)
{
  *id= "facet";

  return 1;
}



/* Routine to return the userdef field of a facet */

int mav_facetGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_facet *facet = (MAV_facet *) mav_objectDataGet(obj);

  *ud= &facet->userdef;

  return 1;
}



/* Routine to return the matrix field of a facet */

int mav_facetGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_facet *facet = (MAV_facet *) mav_objectDataGet(obj);

  *mat= &facet->matrix;

  return 1;
}



/* Routine to return the surface params field of a facet */

int mav_facetGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_facet *facet = (MAV_facet *) mav_objectDataGet(obj);

  *sp= &facet->sp[0];

  return 1;
}



/* Routine to dump a facet */

int mav_facetDump(MAV_object *obj)
{
  MAV_facet *facet = (MAV_facet *) mav_objectDataGet(obj);
  int i,j;

  printf("*** Dumping object %p - a MAV_facet with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("npolys %i\n", facet->npolys);

  for (i=0; i<facet->npolys; i++) {
    printf("np[%i] %i\n", i, facet->np[i]);

    for (j=0; j<facet->np[i]; j++) {
      printf("norm[%i][%i] ", i, j);
      mav_vectorPrint("", facet->norm[i][j]);
      if (facet->sp[i]->mode>=MAV_TEXTURE) {
	printf("tex[%i][%i] ", i, j);
	mav_texCoordPrint("", facet->tex[i][j]);
      }      
      printf("vert[%i][%i] ", i, j);
      mav_vectorPrint("", facet->vert[i][j]);
    }
    
    printf("surface params[%i] ", i);
    mav_surfaceParamsPrint("", *facet->sp[i]);
  }

  mav_matrixPrint("matrix\n", facet->matrix);
  printf("userdef %p\n", facet->userdef);

  return 1;
}
