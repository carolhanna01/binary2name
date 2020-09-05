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

MAV_class *mav_class_box;



/* Routine to render a box */

int mav_boxDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_box *box=(MAV_box *) mav_objectDataGet(obj);
  MAV_vector vert[8], norm;
  MAV_texCoord tex[4];
  float w,h,d;

/* Set the correct colouring */

  mav_surfaceParamsUse(box->sp); 

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(box->matrix);

  w= box->size.x*0.5;
  h= box->size.y*0.5;
  d= box->size.z*0.5;

  vert[0].x = -w;
  vert[0].y = -h;
  vert[0].z = -d;

  vert[1].x = -w;
  vert[1].y = h;
  vert[1].z = -d;

  vert[2].x = w;
  vert[2].y = -h;
  vert[2].z = -d;

  vert[3].x = w;
  vert[3].y = h;
  vert[3].z = -d;

  vert[4].x = -w;
  vert[4].y = -h;
  vert[4].z = d;

  vert[5].x = -w;
  vert[5].y = h;
  vert[5].z = d;

  vert[6].x = w;
  vert[6].y = -h;
  vert[6].z = d;

  vert[7].x = w;
  vert[7].y = h;
  vert[7].z = d;

  if (box->sp->mode>=MAV_TEXTURE) {
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

  if (box->sp->mode==MAV_MATERIAL || box->sp->mode>=MAV_LIT_TEXTURE) {
    norm.x=0.0;
    norm.y=0.0;
    norm.z=-1.0;
    mav_gfxNormal(norm);
  }

  mav_gfxPolygonBegin();  
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[0]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[1]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[3]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[2]);
  mav_gfxPolygonEnd();

/* Face 1,  Norm 0, 0, 1, Verts 4675 */ 

  if (box->sp->mode==MAV_MATERIAL || box->sp->mode>=MAV_LIT_TEXTURE) {
    norm.z=1.0;
    mav_gfxNormal(norm);
  }

  mav_gfxPolygonBegin();
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[4]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[6]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[7]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[5]);
  mav_gfxPolygonEnd();

/* Face 2,  Norm 1, 0, 0, Verts 2376 */ 

  if (box->sp->mode==MAV_MATERIAL || box->sp->mode>=MAV_LIT_TEXTURE) {
    norm.x=1.0;
    norm.z=0.0;
    mav_gfxNormal(norm);
  }

  mav_gfxPolygonBegin();
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[2]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[3]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[7]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[6]);
  mav_gfxPolygonEnd();

/* Face 3,  Norm -1, 0, 0, Verts 0451 */ 

  if (box->sp->mode==MAV_MATERIAL || box->sp->mode>=MAV_LIT_TEXTURE) {
    norm.x=-1.0;
    mav_gfxNormal(norm);
  }

  mav_gfxPolygonBegin();
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[0]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[4]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[5]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[1]);
  mav_gfxPolygonEnd();

/* Face 4,  Norm 0, 1, 0, Verts 1573 */ 

  if (box->sp->mode==MAV_MATERIAL || box->sp->mode>=MAV_LIT_TEXTURE) {
    norm.x=0.0;
    norm.y=1.0;
    mav_gfxNormal(norm);
  }

  mav_gfxPolygonBegin();
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[1]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[5]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[7]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[3]);
  mav_gfxPolygonEnd();

/* Face 5,  Norm 0, -1, 0, Verts 0264 */ 

  if (box->sp->mode==MAV_MATERIAL || box->sp->mode>=MAV_LIT_TEXTURE) {
    norm.y=-1.0;  
    mav_gfxNormal(norm);
  }

  mav_gfxPolygonBegin();
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
  mav_gfxVertex(vert[0]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
  mav_gfxVertex(vert[2]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
  mav_gfxVertex(vert[6]);
  if (box->sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
  mav_gfxVertex(vert[4]);
  mav_gfxPolygonEnd();

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;
}



/* Routine to calculate the bounding box of a box (quick but overestimates) */

int mav_boxBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_box *box = (MAV_box *) mav_objectDataGet(obj);
  MAV_BB tmp;

/* Local coordinate frame BB */

  tmp.max.x = box->size.x*0.5;
  tmp.max.y = box->size.y*0.5;
  tmp.max.z = box->size.z*0.5;

  tmp.min.x = -tmp.max.x;
  tmp.min.y = -tmp.max.y;
  tmp.min.z = -tmp.max.z;

/* Global axis align it */ 

  mav_BBAlign(tmp, box->matrix, bb);

  return 1;
}



/* Another routine to calculate the bounding box of a box (slow but accurate) */

int mav_boxBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_box *box = (MAV_box *) mav_objectDataGet(obj);
  MAV_vector vert[8];
  float w,h,d;
  int i;

  /* Calculate vertices of box */

  w= box->size.x*0.5;
  h= box->size.y*0.5;
  d= box->size.z*0.5;

  vert[0].x = -w;
  vert[0].y = -h;
  vert[0].z = -d;

  vert[1].x = -w;
  vert[1].y = h;
  vert[1].z = -d;

  vert[2].x = w;
  vert[2].y = -h;
  vert[2].z = -d;

  vert[3].x = w;
  vert[3].y = h;
  vert[3].z = -d;

  vert[4].x = -w;
  vert[4].y = -h;
  vert[4].z = d;

  vert[5].x = -w;
  vert[5].y = h;
  vert[5].z = d;

  vert[6].x = w;
  vert[6].y = -h;
  vert[6].z = d;

  vert[7].x = w;
  vert[7].y = h;
  vert[7].z = d;

  /* Find BB of these points after the matrix transformation */

  mav_BBCompInit(bb);
  for (i=0; i<8; i++) mav_BBCompPt(mav_vectorMult(vert[i], box->matrix), bb);

  return 1;
}



/* Routine to intersect a box */

int mav_boxIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_box *box=(MAV_box *) mav_objectDataGet(obj);
  MAV_line ln2;
  MAV_BB bb;
  float w,h,d;
  int rv;

/* Rotate and translate line so that the box is centered and axis aligned */

  ln2= mav_lineTransFrame(*ln, box->matrix);

/* Construct a BB to perform the intersect with */

  w= box->size.x*0.5;
  h= box->size.y*0.5;
  d= box->size.z*0.5;

  bb.min.x= -w;
  bb.max.x=  w;
  bb.min.y= -h;
  bb.max.y=  h;
  bb.min.z= -d;
  bb.max.z=  d;

  rv= mav_BBIntersectsLine(bb, ln2, o);

/* Account for scale */

  if (rv) {
    o->pt1*= mav_matrixScaleGet(box->matrix);
    o->pt2*= mav_matrixScaleGet(box->matrix);
  }

  return rv;
}



/* Routine to identify a box */

int mav_boxID(MAV_object *o, char **id)
{
  *id= "box";

  return 1;
}



/* Routine to return the userdef field of a box */

int mav_boxGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_box *box = (MAV_box *) mav_objectDataGet(obj);

  *ud= &box->userdef;

  return 1;
}



/* Routine to return the matrix field of a box */

int mav_boxGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_box *box = (MAV_box *) mav_objectDataGet(obj);

  *mat= &box->matrix;

  return 1;
}



/* Routine to return the surface params field of a box */

int mav_boxGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_box *box = (MAV_box *) mav_objectDataGet(obj);

  *sp= &box->sp;

  return 1;
}



/* Routine to dump a box */

int mav_boxDump(MAV_object *obj)
{
  MAV_box *box = (MAV_box *) mav_objectDataGet(obj);

  printf("*** Dumping object %p - a MAV_box with data pointer %p\n", obj, mav_objectDataGet(obj));
  mav_vectorPrint("size ", box->size);
  mav_surfaceParamsPrint("surface params ", *box->sp);
  mav_matrixPrint("matrix\n", box->matrix);
  printf("userdef %p\n", box->userdef);

  return 1;
}
