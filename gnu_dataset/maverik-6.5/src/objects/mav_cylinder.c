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

MAV_class *mav_class_cylinder;


/* Routine to render a cylinder */

int mav_cylinderDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_cylinder *cyl = (MAV_cylinder *) mav_objectDataGet(obj);
  MAV_vector *topcap, *botcap, norm;
  MAV_texCoord tex;
  float ang, dA, ca, sa, hb2;
  int i;
  int nverts;

/* set the number of vertices */
  if ((mav_opt_curveLOD || cyl->nverts<3) && di) {
    float dist;
    MAV_vector centre;

/* get world coords of centre of cylinder */
    centre.x= 0; centre.y= 0; centre.z= 0;
    centre= mav_vectorMult (centre, cyl->matrix);

/* compute least possible distance from nearest end to user */
    dist= sqrt (mav_vectorDotProduct (mav_vectorSub (centre, di->vp.eye),
		mav_vectorSub (centre, di->vp.eye))) - cyl->height*0.5;
    dist= dist*dist;
    if (dist<MAV_EPSILON) nverts= mav_opt_vertsMax;
    else {
      nverts= mav_opt_vertsMin + (int) (cyl->radius*cyl->radius/dist * mav_opt_curveFactor);

      if (nverts<mav_opt_vertsMin) nverts= mav_opt_vertsMin;
      else if (nverts>mav_opt_vertsMax) nverts= mav_opt_vertsMax;
    }
  } else {
/* no LOD - use fixed value */
    nverts= cyl->nverts;
  }

/* Set the correct colouring */

  mav_surfaceParamsUse(cyl->sp); 

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(cyl->matrix);

/* Malloc off memory to store the vertices */

  topcap = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));
  botcap = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));

  dA  = MAV_2_PI/nverts;
  ang = 0.0;
  hb2 = cyl->height*0.5;

/* Cylindrical surface */

  mav_gfxStripQBegin();

  for (i=0; i<=nverts; i++) {

    ca=cos(ang);
    sa=sin(ang);

/* 
   Store vertices in temp array as they can also be used to draw the endcaps 
   without the need for recalculating them.
*/

    topcap[i].x=ca*cyl->radius;
    topcap[i].y=sa*cyl->radius;
    topcap[i].z=hb2;

    botcap[i].x=topcap[i].x;
    botcap[i].y=topcap[i].y;
    botcap[i].z=-hb2;

    if (cyl->sp->mode==MAV_MATERIAL || cyl->sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=ca;
      norm.y=sa;
      norm.z=0.0;
      mav_gfxNormal(norm);
    }

    if (cyl->sp->mode>=MAV_TEXTURE) {
      tex.s=ang/MAV_2_PI;
      tex.t=1.0;
      mav_gfxTexCoord(tex);
    }

    mav_gfxVertex(topcap[i]);

    if (cyl->sp->mode>=MAV_TEXTURE) {
      tex.t=0.0;
      mav_gfxTexCoord(tex);
    }

    mav_gfxVertex(botcap[i]);

    ang+=dA;
  }

  mav_gfxStripQEnd();

/* Draw endcaps if requested */

  if (cyl->endcap) {

/* Top cap */

    if (cyl->sp->mode==MAV_MATERIAL || cyl->sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=0.0;
      norm.y=0.0;
      norm.z=1.0;
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();
    for (i=0; i<nverts; i++) {
      if (cyl->sp->mode>=MAV_TEXTURE) {
        tex.s=(topcap[i].x/cyl->radius)*0.5-0.5;
        tex.t=(topcap[i].y/cyl->radius)*0.5-0.5;
        mav_gfxTexCoord(tex);
      }
      mav_gfxVertex(topcap[i]);
    }
    mav_gfxPolygonEnd();

/* Bottom cap */

    if (cyl->sp->mode==MAV_MATERIAL || cyl->sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=0.0;
      norm.y=0.0;
      norm.z=-1.0;
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();
    for (i=nverts; i>0; i--) {
      if (cyl->sp->mode>=MAV_TEXTURE) {
        tex.s=(botcap[i].x/cyl->radius)*0.5-0.5;
        tex.t=(botcap[i].y/cyl->radius)*0.5-0.5;
        mav_gfxTexCoord(tex);
      }
      mav_gfxVertex(botcap[i]);
    }
    mav_gfxPolygonEnd();
  }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  mav_free(topcap);
  mav_free(botcap);

  return 1;
}



/* Routine to calculate the bounding box of a cylinder (quick but overestimates) */

int mav_cylinderBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_cylinder *cyl = (MAV_cylinder *) mav_objectDataGet(obj);
  MAV_BB tmp;

/* Local coordinate frame BB */

  tmp.min.x=-cyl->radius;
  tmp.max.x= cyl->radius;

  tmp.min.y=-cyl->radius;
  tmp.max.y= cyl->radius;

  tmp.max.z= cyl->height*0.5;
  tmp.min.z= -tmp.max.z;

/* Global axis align it */ 

  mav_BBAlign(tmp, cyl->matrix, bb);

  return 1;
}



/* Another routine to calculate the bounding box of a cylinder (slow but accurate) */

int mav_cylinderBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_cylinder *cyl = (MAV_cylinder *) mav_objectDataGet(obj);
  MAV_vector vert;
  float ang, dA, hb2;
  int i;

  /* Initialise BB */

  mav_BBCompInit(bb);

  /* Vertices calculated in same was as draw callback */

  dA  = MAV_2_PI/cyl->nverts;
  ang = 0.0;
  hb2 = cyl->height*0.5;

  for (i=0; i<cyl->nverts; i++) {

    vert.x=cos(ang)*cyl->radius;
    vert.y=sin(ang)*cyl->radius;
    vert.z=hb2;
    mav_BBCompPt(mav_vectorMult(vert, cyl->matrix), bb);

    vert.z=-hb2;
    mav_BBCompPt(mav_vectorMult(vert, cyl->matrix), bb);

    ang+=dA;
  }

  return 1;
}



/* Routine to intersect a cylinder */

int mav_cylinderIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_cylinder *cyl=(MAV_cylinder *) mav_objectDataGet(obj);
  MAV_line ln2;
  float hb2, a, b, c, sr, sr2, t1, t2, st1, st2;
  float zin, zout;

  o->pt1=-100.0;
  o->pt2=-100.0;
  hb2=cyl->height*0.5;

/* Rotate and translate pt and dir so that the cylinder is centered and axis aligned */

  ln2= mav_lineTransFrame(*ln, cyl->matrix);

/* Calculate the coefficients of the quadratic */

  a = ln2.dir.x*ln2.dir.x + ln2.dir.y*ln2.dir.y;
  b = 2.0*(ln2.pt.x*ln2.dir.x + ln2.pt.y*ln2.dir.y);
  c = ln2.pt.x*ln2.pt.x + ln2.pt.y*ln2.pt.y - cyl->radius*cyl->radius;

/* Check if line direction is along Z, i.e. a=0 - non-quadratic solution */
  
  if (a == 0.0) 
  {

/* Check intersection is within circular face */

    if (ln2.pt.x*ln2.pt.x + ln2.pt.y*ln2.pt.y > cyl->radius*cyl->radius) return (MAV_FALSE);

/* In and out intersection now just depends on wheather line is aligned along +Z or -Z */

    if (ln2.dir.z > 0.0) 
    {
      if (ln2.pt.z > hb2) return (MAV_FALSE);
          
      if (ln2.pt.z > -hb2)
      {
	st1=0.0;
      }
      else
      {
	st1=-hb2-ln2.pt.z;
      }
          
      st2=hb2-ln2.pt.z;
    }
    else
    {
      if (ln2.pt.z < -hb2) return (MAV_FALSE);

      if (ln2.pt.z < hb2)
      {
	st1=0.0;
      }
      else
      {
	st1=ln2.pt.z-hb2;
      }
 
      st2=hb2+ln2.pt.z;
    }
  }
  else
  {

/* Quadratic solution */

    sr = b*b - 4.0*a*c;

    if (sr < 0.0) return (MAV_FALSE);
    sr2=sqrt(sr);

    t1 = (-b + sr2) / (2.0*a);
    t2 = (-b - sr2) / (2.0*a);
  
/* Sort intersection distances so that t2>t1 */

    st1=t1;
    st2=t2;

    if (st1 > st2) {
      st1=t2;
      st2=t1;
    }

/* Check is cylinder is behind us */

    if (st1<0.0 && st2<0.0) return(MAV_FALSE);

/* Check if ray originates inside object - if so set dist to closed intersection to zero */

    if (st1 < 0.0 && st2 > 0.0) st1=0.0;
      
/* Calculate in and out z intersections */

    zin=ln2.pt.z + st1*ln2.dir.z;
    zout=ln2.pt.z + st2*ln2.dir.z;
      
/* Reject intersections if line goes totally above or below cylinder */

    if (zin >  hb2 && zout >  hb2) return (MAV_FALSE); 
    if (zin < -hb2 && zout < -hb2) return (MAV_FALSE); 

/* Check if in intersects with top of cylinder */

    if (zin > hb2) {

      if (ln2.dir.z == 0.0) return(MAV_FALSE);

/* Find the t which has hb2 as its z value */

      st1=(hb2-ln2.pt.z)/ln2.dir.z;
    }

/* Check if in intersects with bottom of cylinder */

    if (zin < -hb2) {

      if (ln2.dir.z == 0.0) return(MAV_FALSE);
        
/* Find the t which has -hb2 as its z value */

      st1=(-hb2-ln2.pt.z)/ln2.dir.z;
    }

/* Check if out intersects with top of cylinder */
      
    if (zout > hb2) {

      if (ln2.dir.z == 0.0) return(MAV_FALSE);

/* Find the t which has hb2 as its z value */

      st2=(hb2-ln2.pt.z)/ln2.dir.z;
    }

/* Check if out intersects with bottom of cylinder */

    if (zout < -hb2) {

      if (ln2.dir.z == 0.0) return(MAV_FALSE);
    
/* Find the t which has -hb2 as its z value */

      st2=(-hb2-ln2.pt.z)/ln2.dir.z;
    }
  }

/* Compensate for scale */

  o->pt1 = st1*mav_matrixScaleGet(cyl->matrix);
  o->pt2 = st2*mav_matrixScaleGet(cyl->matrix);

  return(MAV_TRUE);
}



/* Routine to identify a cylinder */

int mav_cylinderID(MAV_object *o, char **id)
{
  *id= "cylinder";

  return 1;
}



/* Routine to return the userdef field of a cylinder */

int mav_cylinderGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_cylinder *cyl = (MAV_cylinder *) mav_objectDataGet(obj);

  *ud= &cyl->userdef;

  return 1;
}



/* Routine to return the matrix field of a cylinder */

int mav_cylinderGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_cylinder *cyl = (MAV_cylinder *) mav_objectDataGet(obj);

  *mat= &cyl->matrix;

  return 1;
}



/* Routine to return the surface params field of a cylinder */

int mav_cylinderGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_cylinder *cyl = (MAV_cylinder *) mav_objectDataGet(obj);

  *sp= &cyl->sp;

  return 1;
}



/* Routine to dump a cylinder */

int mav_cylinderDump(MAV_object *obj)
{
  MAV_cylinder *cyl = (MAV_cylinder *) mav_objectDataGet(obj);

  printf("*** Dumping object %p - a MAV_cylinder with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("radius %f\n", cyl->radius);
  printf("height %f\n", cyl->height);
  printf("nverts %i\n", cyl->nverts);
  printf("endcap %i\n", cyl->endcap);
  mav_surfaceParamsPrint("surface params ", *cyl->sp);
  mav_matrixPrint("matrix\n", cyl->matrix);
  printf("userdef %p\n", cyl->userdef);

  return 1;
}
