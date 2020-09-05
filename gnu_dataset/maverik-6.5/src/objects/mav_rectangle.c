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

MAV_class *mav_class_rectangle;



/* Routine to render a rectangle */

int mav_rectangleDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_rectangle *rect = (MAV_rectangle *) mav_objectDataGet(obj);
  MAV_vector vert;
  MAV_texCoord tex;

/* Set the correct colouring */

  mav_surfaceParamsUse(rect->sp);

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(rect->matrix);

/* Set the normal */

  if (rect->sp->mode==MAV_MATERIAL || rect->sp->mode>=MAV_LIT_TEXTURE) {
    vert.x=0;
    vert.y=0;
    vert.z=1;
    mav_gfxNormal(vert);
  }

/* Draw the rectangle */

  mav_gfxPolygonBegin();

  if (rect->sp->mode>=MAV_TEXTURE) {
    tex.s= 0;
    tex.t= 0;
    mav_gfxTexCoord(tex);
  }

  vert.x= -rect->width*0.5;
  vert.y= -rect->height*0.5;
  vert.z= 0;
  mav_gfxVertex(vert);

  if (rect->sp->mode>=MAV_TEXTURE) {
    tex.s= rect->xtile;
    mav_gfxTexCoord(tex);
  }

  vert.x= -vert.x;
  mav_gfxVertex(vert);

  if (rect->sp->mode>=MAV_TEXTURE) {
    tex.t= rect->ytile;
    mav_gfxTexCoord(tex);
  }

  vert.y= -vert.y;
  mav_gfxVertex(vert);

  if (rect->sp->mode>=MAV_TEXTURE) {
    tex.s= 0;
    mav_gfxTexCoord(tex);
  }

  vert.x= -vert.x;
  mav_gfxVertex(vert);

  mav_gfxPolygonEnd();

  /* Display normal if applicable */
  if (mav_opt_drawNormals<1000000) {

    MAV_surfaceParams sp;
    sp.mode= MAV_COLOUR;
    sp.colour= MAV_COLOUR_RED;
    sp.material= 0;
    sp.texture= 0;
    mav_surfaceParamsUse(&sp);

    mav_gfxLineBegin();
    mav_gfxVertex(mav_vectorSet(0, 0, 0));
    mav_gfxVertex(mav_vectorSet(0, 0, mav_opt_drawNormals));
    mav_gfxLineEnd();
  }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;
}



/* Routine to calculate the bounding box of a rectangle */

int mav_rectangleBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_rectangle *rect = (MAV_rectangle *) mav_objectDataGet(obj);
  MAV_BB tmp;

/* Local coordinate frame BB of rectangle */

  tmp.max.x= rect->width*0.5;
  tmp.max.y= rect->height*0.5;
  tmp.max.z= 0;

  tmp.min.x= -tmp.max.x;
  tmp.min.y= -tmp.max.y;
  tmp.min.z= 0;

/* Global axis align it */ 

  mav_BBAlign(tmp, rect->matrix, bb);
    
  return 1;
}



/* Routine to intersect a rectangle */

int mav_rectangleIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_rectangle *rect = (MAV_rectangle *) mav_objectDataGet(obj);
  MAV_polygon apoly;
  MAV_line ln2;
  int rv= MAV_FALSE;

/* Rotate and translate line so that the rectangle is centered and axis aligned */

  ln2= mav_lineTransFrame(*ln, rect->matrix);

/* Make a polygon out of it */

  apoly.np=4;
  apoly.vert=mav_malloc(apoly.np*sizeof(MAV_vector));
  apoly.norm.x=0;
  apoly.norm.y=0;
  apoly.norm.z=1;

  apoly.vert[0].x= -rect->width*0.5;
  apoly.vert[0].y= -rect->height*0.5;
  apoly.vert[0].z= 0;

  apoly.vert[1].x= -apoly.vert[0].x;
  apoly.vert[1].y= apoly.vert[0].y;
  apoly.vert[1].z= 0;

  apoly.vert[2].x= apoly.vert[1].x;
  apoly.vert[2].y= -apoly.vert[1].y;
  apoly.vert[2].z= 0;

  apoly.vert[3].x= apoly.vert[0].x;
  apoly.vert[3].y= apoly.vert[2].y;
  apoly.vert[3].z= 0;

  rv= mav_linePolygonIntersection(&apoly, ln2, o);

  mav_free(apoly.vert);

  if (rv) {

/* Compensate for scale and set out distance to equal in distance */

    o->pt1*= mav_matrixScaleGet(rect->matrix);
    o->pt2= o->pt1;
  }

  return(rv);
}



/* Routine to identify a rectangle */

int mav_rectangleID(MAV_object *o, char **id)
{
  *id= "rectangle";

  return 1;
}



/* Routine to return the userdef field of a rectangle */

int mav_rectangleGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_rectangle *rect = (MAV_rectangle *) mav_objectDataGet(obj);

  *ud= &rect->userdef;

  return 1;
}



/* Routine to return the matrix field of a rectangle */

int mav_rectangleGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_rectangle *rect = (MAV_rectangle *) mav_objectDataGet(obj);

  *mat= &rect->matrix;

  return 1;
}



/* Routine to return the surface params field of a rectangle */

int mav_rectangleGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_rectangle *rect = (MAV_rectangle *) mav_objectDataGet(obj);

  *sp= &rect->sp;

  return 1;
}



/* Routine to dump a rectangle */

int mav_rectangleDump(MAV_object *obj)
{
  MAV_rectangle *rect= (MAV_rectangle *) mav_objectDataGet(obj);

  printf("*** Dumping object %p - a MAV_rectangle with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("width %f\n", rect->width);
  printf("height %f\n", rect->height);
  if (rect->sp->mode>=MAV_TEXTURE) {
    printf("xtile %f\n", rect->xtile);
    printf("ytile %f\n", rect->ytile);
  }
  mav_surfaceParamsPrint("surface params ", *rect->sp);
  mav_matrixPrint("matrix\n", rect->matrix);
  printf("userdef %p\n", rect->userdef);
  
  return 1;
}
