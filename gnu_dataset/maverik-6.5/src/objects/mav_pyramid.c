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
#include <math.h>

MAV_class *mav_class_pyramid;



/* Routine to calculate a normal from a set of vertices */

MAV_vector mavlib_normFromVerts(MAV_vector v1, MAV_vector v2, MAV_vector v3, MAV_vector v4)
{

#define NORM_EPS 0.01

  MAV_vector vec1, vec2;

/*
  Define vectors from vert2 to vert1 and vert2 to vert3. If vert1 or vert3 equals 
  vert2 (e.g. in the case of a pyramid going to a point where there are only 3
  discrete vertices) vert4 is used instead.
*/

  vec1 = mav_vectorSub(v1, v2);
  if (mav_vectorDotProduct(vec1, vec1) < NORM_EPS) vec1 = mav_vectorSub(v4, v2);

  vec2 = mav_vectorSub(v3, v2);
  if (mav_vectorDotProduct(vec2, vec2) < NORM_EPS) vec2 = mav_vectorSub(v4, v2);

/* Calculate cross product of vectors to give the normal */

  return (mav_vectorNormalize(mav_vectorCrossProduct(vec2, vec1)));
}



/* Routine to render a pyramid */

int mav_pyramidDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_pyramid *pyr=(MAV_pyramid *) mav_objectDataGet(obj);
  MAV_vector vert[8], norm;
  MAV_texCoord tex[4];
  float w,h,d,bsx,bsy,tsx,tsy;

/* Set the correct colouring */

  mav_surfaceParamsUse(pyr->sp); 

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(pyr->matrix);

  w= pyr->offset_x*0.5;
  h= pyr->offset_y*0.5;
  d= pyr->height*0.5;
  bsx= pyr->bot_size_x*0.5;
  bsy= pyr->bot_size_y*0.5;
  tsx= pyr->top_size_x*0.5;
  tsy= pyr->top_size_y*0.5;

/* Store the 8 vertices of the pyramid */

  vert[0].x = -w-bsx;
  vert[0].y = -h-bsy;
  vert[0].z = -d;

  vert[1].x = vert[0].x;
  vert[1].y = -h+bsy;
  vert[1].z = -d;

  vert[2].x = -w+bsx;
  vert[2].y = vert[0].y;
  vert[2].z = -d;

  vert[3].x = vert[2].x;
  vert[3].y = vert[1].y;
  vert[3].z = -d;

  vert[4].x = w-tsx;
  vert[4].y = h-tsy;
  vert[4].z = d;

  vert[5].x = vert[4].x;
  vert[5].y = h+tsy;
  vert[5].z = d;

  vert[6].x = w+tsx;
  vert[6].y = vert[4].y;
  vert[6].z = d;

  vert[7].x = vert[6].x;
  vert[7].y = vert[5].y;
  vert[7].z = d;
  
  if (pyr->sp->mode>=MAV_TEXTURE) {
    tex[0].s = 0.0;
    tex[0].t = 0.0;
    
    tex[1].s = 0.0;
    tex[1].t = 1.0;
  
    tex[2].s = 1.0;
    tex[2].t = 0.0;

    tex[3].s = 1.0;
    tex[3].t = 1.0;
  }

/* Face 0,  Norm 0, 0, -1, Verts 0132 */ 

  if (pyr->sp->mode==MAV_MATERIAL || pyr->sp->mode>=MAV_LIT_TEXTURE) {
    norm.x=0.0;
    norm.y=0.0;
    norm.z=-1.0;  
    mav_gfxNormal(norm);
  }

  mav_gfxPolygonBegin();
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[0]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[1]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[3]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[2]);
  mav_gfxPolygonEnd();

/* Face 1,  Norm 0, 0, 1, Verts 4675 */ 

  if (pyr->sp->mode==MAV_MATERIAL || pyr->sp->mode>=MAV_LIT_TEXTURE) {
    norm.z=1.0;
    mav_gfxNormal(norm);
  }

  mav_gfxPolygonBegin();
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[4]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[6]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[7]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[5]);
  mav_gfxPolygonEnd();

/* Face 2, Verts 2376 */ 

  if (pyr->sp->mode==MAV_MATERIAL || pyr->sp->mode>=MAV_LIT_TEXTURE) {
    mav_gfxNormal(mavlib_normFromVerts(vert[2], vert[3], vert[7], vert[6]));
  }

  mav_gfxPolygonBegin();
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[2]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[3]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[7]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[6]);
  mav_gfxPolygonEnd();

/* Face 3, Verts 0451 */ 

  if (pyr->sp->mode==MAV_MATERIAL || pyr->sp->mode>=MAV_LIT_TEXTURE) {
    mav_gfxNormal(mavlib_normFromVerts(vert[0], vert[4], vert[5], vert[1]));
  }

  mav_gfxPolygonBegin();
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[0]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[4]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[5]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[1]);
  mav_gfxPolygonEnd();

/* Face 4, Verts 1573 */ 

  if (pyr->sp->mode==MAV_MATERIAL || pyr->sp->mode>=MAV_LIT_TEXTURE) {
    mav_gfxNormal(mavlib_normFromVerts(vert[1], vert[5], vert[7], vert[3]));
  }

  mav_gfxPolygonBegin();
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[1]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[5]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[7]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[3]);
  mav_gfxPolygonEnd();

/* Face 5, Verts 0264 */ 

  if (pyr->sp->mode==MAV_MATERIAL || pyr->sp->mode>=MAV_LIT_TEXTURE) {
    mav_gfxNormal(mavlib_normFromVerts(vert[0], vert[2], vert[6], vert[4]));  
  }

  mav_gfxPolygonBegin();
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[0]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[2]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[6]);
  if (pyr->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[4]);
  mav_gfxPolygonEnd();

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;
}



/* Routine to calculate the bounding box of a pyramid (quick but overestimates) */

int mav_pyramidBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_pyramid *pyr=(MAV_pyramid *) mav_objectDataGet(obj);
  MAV_BB tmp;
  MAV_vector vert[4];
  float w,h,bsx,bsy,tsx,tsy;

  w= pyr->offset_x*0.5;
  h= pyr->offset_y*0.5;
  bsx= pyr->bot_size_x*0.5;
  bsy= pyr->bot_size_y*0.5;
  tsx= pyr->top_size_x*0.5;
  tsy= pyr->top_size_y*0.5;

/* Store the xy extents of the pyramid */

  vert[0].x = -w-bsx;
  vert[0].y = -h-bsy;

  vert[1].x = -w+bsx;
  vert[1].y = -h+bsy;

  vert[2].x = w-tsx;
  vert[2].y = h-tsy;

  vert[3].x = w+tsx;
  vert[3].y = h+tsy;

/* Local coordinate frame BB */

  if (vert[0].x < vert[2].x)
  {
    tmp.min.x= vert[0].x;
  }
  else
  {
    tmp.min.x= vert[2].x;
  }

  if (vert[0].y < vert[2].y)
  {
    tmp.min.y= vert[0].y;
  }
  else
  {
    tmp.min.y= vert[2].y;
  }

  if (vert[1].x > vert[3].x)
  {
    tmp.max.x= vert[1].x;
  }
  else
  {
    tmp.max.x= vert[3].x;
  }

  if (vert[1].y > vert[3].y)
  {
    tmp.max.y= vert[1].y;
  }
  else
  {
    tmp.max.y= vert[3].y;
  }

  tmp.max.z= pyr->height*0.5;
  tmp.min.z= -tmp.max.z;

/* Global axis align it */ 

  mav_BBAlign(tmp, pyr->matrix, bb);

  return 1;
}



/* Another routine to calculate the bounding box of a pyramid (slow but accurate) */

int mav_pyramidBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_pyramid *pyr = (MAV_pyramid *) mav_objectDataGet(obj);
  MAV_vector vert[8];
  float w,h,d,bsx,bsy,tsx,tsy;
  int i;

  w= pyr->offset_x*0.5;
  h= pyr->offset_y*0.5;
  d= pyr->height*0.5;
  bsx= pyr->bot_size_x*0.5;
  bsy= pyr->bot_size_y*0.5;
  tsx= pyr->top_size_x*0.5;
  tsy= pyr->top_size_y*0.5;

/* Store the 8 vertices of the pyramid */

  vert[0].x = -w-bsx;
  vert[0].y = -h-bsy;
  vert[0].z = -d;

  vert[1].x = vert[0].x;
  vert[1].y = -h+bsy;
  vert[1].z = -d;

  vert[2].x = -w+bsx;
  vert[2].y = vert[0].y;
  vert[2].z = -d;

  vert[3].x = vert[2].x;
  vert[3].y = vert[1].y;
  vert[3].z = -d;

  vert[4].x = w-tsx;
  vert[4].y = h-tsy;
  vert[4].z = d;

  vert[5].x = vert[4].x;
  vert[5].y = h+tsy;
  vert[5].z = d;

  vert[6].x = w+tsx;
  vert[6].y = vert[4].y;
  vert[6].z = d;

  vert[7].x = vert[6].x;
  vert[7].y = vert[5].y;
  vert[7].z = d;

  /* Find BB of these points after the matrix transformation */

  mav_BBCompInit(bb);
  for (i=0; i<8; i++) mav_BBCompPt(mav_vectorMult(vert[i], pyr->matrix), bb);

  return 1;
}



/* Routine to intersect a pyramid */

int mav_pyramidIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_pyramid *pyr = (MAV_pyramid *) mav_objectDataGet(obj);
  MAV_polygon apoly;
  MAV_line ln2;
  MAV_vector vert[8];
  MAV_objectIntersection planeInt[6];
  float w,h,d,bsx,bsy,tsx,tsy;
  int nhits;

  o->pt1=-100.0;
  o->pt2=-100.0;

/* Rotate and translate line pt and dir so that the pyramid is centered and axis aligned */

  ln2= mav_lineTransFrame(*ln, pyr->matrix);

  w= pyr->offset_x*0.5;
  h= pyr->offset_y*0.5;
  d= pyr->height*0.5;
  bsx= pyr->bot_size_x*0.5;
  bsy= pyr->bot_size_y*0.5;
  tsx= pyr->top_size_x*0.5;
  tsy= pyr->top_size_y*0.5;

/* Store the 8 vertices of the pyramid */

  vert[0].x = -w-bsx;
  vert[0].y = -h-bsy;
  vert[0].z = -d;

  vert[1].x = vert[0].x;
  vert[1].y = -h+bsy;
  vert[1].z = -d;

  vert[2].x = -w+bsx;
  vert[2].y = vert[0].y;
  vert[2].z = -d;

  vert[3].x = vert[2].x;
  vert[3].y = vert[1].y;
  vert[3].z = -d;

  vert[4].x = w-tsx;
  vert[4].y = h-tsy;
  vert[4].z = d;

  vert[5].x = vert[4].x;
  vert[5].y = h+tsy;
  vert[5].z = d;

  vert[6].x = w+tsx;
  vert[6].y = vert[4].y;
  vert[6].z = d;

  vert[7].x = vert[6].x;
  vert[7].y = vert[5].y;
  vert[7].z = d;

/* Define the polygon to be used in the intersection test */

  apoly.np = 4;
  apoly.vert = (MAV_vector *) mav_malloc(4*sizeof(MAV_vector));

  nhits=0;

/* Polygon for face 0,  Norm 0, 0, -1, Verts 0132 */ 

  apoly.norm.x = 0.0;
  apoly.norm.y = 0.0;
  apoly.norm.z = -1.0;
  apoly.vert[0] = vert[0];
  apoly.vert[1] = vert[1];
  apoly.vert[2] = vert[3];
  apoly.vert[3] = vert[2];

  if (mav_linePolygonIntersection(&apoly, ln2, &planeInt[nhits])) nhits++;

/* Polygon for face 1,  Norm 0, 0, 1, Verts 4675 */ 

  apoly.norm.z = 1.0;
  apoly.vert[0] = vert[4];
  apoly.vert[1] = vert[6];
  apoly.vert[2] = vert[7];
  apoly.vert[3] = vert[5];

  if (mav_linePolygonIntersection(&apoly, ln2, &planeInt[nhits])) nhits++;

/* Polygon for face 2, Normal from verts, Verts 2376 */ 

  apoly.norm = mavlib_normFromVerts(vert[2], vert[3], vert[7], vert[6]);
  apoly.vert[0] = vert[2];
  apoly.vert[1] = vert[3];
  apoly.vert[2] = vert[7];
  apoly.vert[3] = vert[6];

  if (mav_linePolygonIntersection(&apoly, ln2, &planeInt[nhits])) nhits++;

/* Polygon for face 3, Normal from verts, Verts 0451 */ 

  apoly.norm = mavlib_normFromVerts(vert[0], vert[4], vert[5], vert[1]);  
  apoly.vert[0] = vert[0];
  apoly.vert[1] = vert[4];
  apoly.vert[2] = vert[5];
  apoly.vert[3] = vert[1];

  if (mav_linePolygonIntersection(&apoly, ln2, &planeInt[nhits])) nhits++;

/* Polygon for face 4, Normal from verts, Verts 1573 */ 

  apoly.norm = mavlib_normFromVerts(vert[1], vert[5], vert[7], vert[3]);
  apoly.vert[0] = vert[1];
  apoly.vert[1] = vert[5];
  apoly.vert[2] = vert[7];
  apoly.vert[3] = vert[3];

  if (mav_linePolygonIntersection(&apoly, ln2, &planeInt[nhits])) nhits++;

/* Polygon for face 5, Normal from verts, Verts 0264 */ 

  apoly.norm = mavlib_normFromVerts(vert[0], vert[2], vert[6], vert[4]);
  apoly.vert[0] = vert[0];
  apoly.vert[1] = vert[2];
  apoly.vert[2] = vert[6];
  apoly.vert[3] = vert[4];

  if (mav_linePolygonIntersection(&apoly, ln2, &planeInt[nhits])) nhits++;

  mav_free(apoly.vert);

/* Sort intersection and return appropriate value */

  return(mav_objectIntersectionsSort(nhits, planeInt, mav_matrixScaleGet(pyr->matrix), o));
}



/* Routine to identify a pyramid */

int mav_pyramidID(MAV_object *o, char **id)
{
  *id= "pyramid";

  return 1;
}



/* Routine to return the userdef field of a pyramid */

int mav_pyramidGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_pyramid *pyr = (MAV_pyramid *) mav_objectDataGet(obj);

  *ud= &pyr->userdef;

  return 1;
}



/* Routine to return the matrix field of a pyramid */

int mav_pyramidGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_pyramid *pyr = (MAV_pyramid *) mav_objectDataGet(obj);

  *mat= &pyr->matrix;

  return 1;
}



/* Routine to return the surface params field of a pyramid */

int mav_pyramidGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_pyramid *pyr = (MAV_pyramid *) mav_objectDataGet(obj);

  *sp= &pyr->sp;

  return 1;
}



/* Routine to dump a pyramid */

int mav_pyramidDump(MAV_object *obj)
{
  MAV_pyramid *pyr = (MAV_pyramid *) mav_objectDataGet(obj);
  
  printf("*** Dumping object %p - a MAV_pyramid with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("bot size x %f\n", pyr->bot_size_x);
  printf("bot size y %f\n", pyr->bot_size_y);
  printf("top size x %f\n", pyr->top_size_x);
  printf("top size y %f\n", pyr->top_size_y);
  printf("offset x %f\n", pyr->offset_x);
  printf("offset y %f\n", pyr->offset_y);
  printf("height %f\n", pyr->height);
  mav_surfaceParamsPrint("surface params ", *pyr->sp);
  mav_matrixPrint("matrix\n", pyr->matrix);
  printf("userdef %p\n", pyr->userdef);
  
  return 1;
}
