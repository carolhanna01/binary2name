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

MAV_class *mav_class_cone;

/* Routine to render a cone */

int mav_coneDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_cone *cone = (MAV_cone *) mav_objectDataGet(obj);
  MAV_vector *topcap, *botcap, norm;
  MAV_texCoord tex;
  float ang, dA, ca, sa, zcomp, hb2;
  int i;
  int nverts;

/* set the number of vertices */
  if ((mav_opt_curveLOD || cone->nverts<3) && di) {
    float dist;
    MAV_vector centre;

/* get world coords of centre of cone */
    centre.x= 0; centre.y= 0; centre.z= 0;
    centre= mav_vectorMult (centre, cone->matrix);

/* compute least possible distance from nearest end to user */
    dist= sqrt (mav_vectorDotProduct (mav_vectorSub (centre, di->vp.eye),
                mav_vectorSub (centre, di->vp.eye))) - cone->height*0.5;
    dist= dist*dist;
    if (dist<MAV_EPSILON) nverts= mav_opt_vertsMax;
    else {
/* use the larger of the two radii */
      if (cone->rt>cone->rb) nverts= mav_opt_vertsMin +
	(int) (cone->rt*cone->rt/dist * mav_opt_curveFactor);
      else nverts= mav_opt_vertsMin + (int) (cone->rb*cone->rb/dist *
	mav_opt_curveFactor);

      if (nverts<mav_opt_vertsMin) nverts= mav_opt_vertsMin;
      else if (nverts>mav_opt_vertsMax) nverts= mav_opt_vertsMax;
    }
  } else {
/* no LOD - use fixed value */
    nverts= cone->nverts;
  }

/* Set the correct colouring */

  mav_surfaceParamsUse(cone->sp); 

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(cone->matrix);

/* Malloc off memory to store the vertices */

  topcap = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));
  botcap = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));

  dA  = MAV_2_PI/nverts;
  ang = 0.0;
  zcomp = (cone->rb-cone->rt)/cone->height;
  hb2 = cone->height*0.5;

/* Draw cone surface */

  mav_gfxStripQBegin();

  for (i=0; i<=nverts; i++) {

    ca=cos(ang);
    sa=sin(ang);

/* 
   Store vertices in temp array as they can also be used to draw the endcaps 
   without the need for recalculating them.
*/

    topcap[i].x=ca*cone->rt;
    topcap[i].y=sa*cone->rt;
    topcap[i].z=hb2;

    botcap[i].x=ca*cone->rb;
    botcap[i].y=sa*cone->rb;
    botcap[i].z=-topcap[i].z;

    if (cone->sp->mode==MAV_MATERIAL || cone->sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=ca;
      norm.y=sa;
      norm.z=zcomp;
      mav_gfxNormal(mav_vectorNormalize(norm));
    }

    if (cone->sp->mode>=MAV_TEXTURE) {
      tex.s=ang/MAV_2_PI;
      tex.t=1.0;
      mav_gfxTexCoord(tex);
    }

    mav_gfxVertex(topcap[i]);

    if (cone->sp->mode>=MAV_TEXTURE) {
      tex.t=0.0;
      mav_gfxTexCoord(tex);
    }

    mav_gfxVertex(botcap[i]);

    ang+=dA;
  }

  mav_gfxStripQEnd();

/* Draw endcaps if requested */

  if (cone->endcap) {

/* Top cap */

    if (cone->sp->mode==MAV_MATERIAL || cone->sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=0.0;
      norm.y=0.0;
      norm.z=1.0;
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();
    for (i=0; i<nverts; i++) {
      if (cone->sp->mode>=MAV_TEXTURE) {
        tex.s=(topcap[i].x/cone->rt)*0.5-0.5;
        tex.t=(topcap[i].y/cone->rt)*0.5-0.5;
        mav_gfxTexCoord(tex);
      } 
      mav_gfxVertex(topcap[i]);
    }
    mav_gfxPolygonEnd();

/* Bottom cap */

    if (cone->sp->mode==MAV_MATERIAL || cone->sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=0.0;
      norm.y=0.0;
      norm.z=-1.0;
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();
    for (i=nverts; i>0; i--) {
      if (cone->sp->mode>=MAV_TEXTURE) {
        tex.s=(botcap[i].x/cone->rb)*0.5-0.5;
        tex.t=(botcap[i].y/cone->rb)*0.5-0.5;
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



/* Routine to calculate the bounding box of a cone (quick but overestimates) */

int mav_coneBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_cone *cone = (MAV_cone *) mav_objectDataGet(obj);
  MAV_BB tmp;
  float br;

  if (cone->rt > cone->rb) 
  {
    br=cone->rt;
  }
  else
  {
    br=cone->rb;
  }

/* Local coordinate frame BB */

  tmp.max.x = br;
  tmp.max.y = br;
  tmp.max.z = cone->height*0.5;

  tmp.min.x = -tmp.max.x;
  tmp.min.y = -tmp.max.y;
  tmp.min.z = -tmp.max.z;


/* Global axis align it */ 

  mav_BBAlign(tmp, cone->matrix, bb);

  return 1;
}



/* Another routine to calculate the bounding box of a cone (slow but accurate) */

int mav_coneBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_cone *cone = (MAV_cone *) mav_objectDataGet(obj);
  MAV_vector vert;
  float ang, dA, ca, sa, hb2;
  int i;

  /* Initialise BB */

  mav_BBCompInit(bb);

  /* Vertices calculated in same was as draw callback */

  dA  = MAV_2_PI/cone->nverts;
  ang = 0.0;
  hb2 = cone->height*0.5;

  for (i=0; i<cone->nverts; i++) {

    ca= cos(ang);
    sa= sin(ang);

    vert.x=ca*cone->rt;
    vert.y=sa*cone->rt;
    vert.z=hb2;
    mav_BBCompPt(mav_vectorMult(vert, cone->matrix), bb);

    vert.x=ca*cone->rb;
    vert.y=sa*cone->rb;
    vert.z=-hb2;
    mav_BBCompPt(mav_vectorMult(vert, cone->matrix), bb);

    ang+=dA;
  }

  return 1;
}



/* Routine to intersect a cone */

int mav_coneIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_cone *cone= (MAV_cone *) mav_objectDataGet(obj);
  MAV_line ln2;
  MAV_vector hit1, hit2;
  MAV_objectIntersection oint[4];
  float check1, check2, a, b, c, root;
  int nhits= 0;

  o->pt1=-100.0;
  o->pt2=-100.0;

  ln2= mav_lineTransFrame(*ln, cone->matrix);

/* check endcaps */
  if (ln2.dir.z != 0) {
    check1= (-cone->height*0.5 - ln2.pt.z)/ln2.dir.z;
    check2= (cone->height*0.5 - ln2.pt.z)/ln2.dir.z;

    hit1.x= ln2.pt.x + check1*ln2.dir.x;
    hit1.y= ln2.pt.y + check1*ln2.dir.y;
    hit1.z= -cone->height*0.5;

    hit2.x= ln2.pt.x + check2*ln2.dir.x;
    hit2.y= ln2.pt.y + check2*ln2.dir.y;
    hit2.z= cone->height*0.5;

    if ((hit1.x*hit1.x+hit1.y*hit1.y) <= (cone->rb*cone->rb)) {
      if (check1<0.0) check1= 0.0;
      oint[nhits].pt1= check1;
      oint[nhits].pt2= check1;
      nhits ++;
    }

    if ((hit2.x*hit2.x+hit2.y*hit2.y) <= (cone->rt*cone->rt)) {
      if (check2<0.0) check2= 0.0;
      oint[nhits].pt1= check2;
      oint[nhits].pt2= check2;
      nhits ++;
    }

    if (nhits==2) {
      if ((check1<=0.0) && (check2<=0.0)) return MAV_FALSE;

/* ray intersects top and bottom faces - therefore doesn't hit curve */
      return(mav_objectIntersectionsSort(nhits, oint, mav_matrixScaleGet(cone->matrix), o));
    }
  }

  /* solve ray/cone quadratic */
  a= ln2.dir.x*ln2.dir.x + ln2.dir.y*ln2.dir.y - ln2.dir.z*ln2.dir.z*
	(cone->rt-cone->rb)*(cone->rt-cone->rb)/(cone->height*cone->height);

  b= 2.0*ln2.pt.x*ln2.dir.x+2.0*ln2.pt.y*ln2.dir.y - 2.0*ln2.pt.z*ln2.dir.z*
	(cone->rt-cone->rb)*(cone->rt-cone->rb)/(cone->height*cone->height) -
	2.0*ln2.dir.z * (cone->rb+cone->rt)*(cone->rt-cone->rb)/(2.0*cone->height);

  c= ln2.pt.x*ln2.pt.x+ln2.pt.y*ln2.pt.y- ln2.pt.z*ln2.pt.z*
	(cone->rt-cone->rb)*(cone->rt-cone->rb)/(cone->height*cone->height) -
	2.0*ln2.pt.z*(cone->rb+cone->rt)*(cone->rt-cone->rb)/(2.0*cone->height) -
	(cone->rb+cone->rt)*(cone->rb+cone->rt)*0.25;

  if (a!=0) {
    root= b*b-4.0*a*c;

    if (root>0) {
      root= sqrt (root);
      check1= (-b+root)/(2.0*a);
      check2= (-b-root)/(2.0*a);
    
      hit1.x= ln2.pt.x + check1*ln2.dir.x;
      hit1.y= ln2.pt.y + check1*ln2.dir.y;
      hit1.z= ln2.pt.z + check1*ln2.dir.z;
      
      hit2.x= ln2.pt.x + check2*ln2.dir.x;
      hit2.y= ln2.pt.y + check2*ln2.dir.y;
      hit2.z= ln2.pt.z + check2*ln2.dir.z;
      
      /* check hit1 is a valid hit */
      if ((hit1.z>= -cone->height*0.5)&&(hit1.z<= cone->height*0.5)) {
	if (check1<0.0) check1= 0.0;
	oint[nhits].pt1= check1;
	oint[nhits].pt2= check1;
	nhits ++;
      }
      
      /* check hit2 */
      if ((hit2.z>= -cone->height*0.5)&&(hit2.z<= cone->height*0.5)) {
	if (check2<0.0) check2= 0.0;
	oint[nhits].pt1= check2;
	oint[nhits].pt2= check2;
	nhits ++;
      }
      
      if (nhits==2 && oint[0].pt1<=0.0 && oint[1].pt1<=0.0) return MAV_FALSE;
    }
  }
  
  return(mav_objectIntersectionsSort(nhits, oint, mav_matrixScaleGet(cone->matrix), o));
}



/* Routine to identify a cone */

int mav_coneID(MAV_object *o, char **id)
{
  *id= "cone";

  return 1;
}



/* Routine to return the userdef field of a cone */

int mav_coneGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_cone *cone = (MAV_cone *) mav_objectDataGet(obj);

  *ud= &cone->userdef;

  return 1;
}



/* Routine to return the matrix field of a cone */

int mav_coneGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_cone *cone = (MAV_cone *) mav_objectDataGet(obj);

  *mat= &cone->matrix;

  return 1;
}



/* Routine to return the surface params field of a cone */

int mav_coneGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_cone *cone = (MAV_cone *) mav_objectDataGet(obj);

  *sp= &cone->sp;

  return 1;
}



/* Routine to dump a cone */

int mav_coneDump(MAV_object *obj)
{
  MAV_cone *cone = (MAV_cone *) mav_objectDataGet(obj);

  printf("*** Dumping object %p - a MAV_cone with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("rt %f\n", cone->rt);
  printf("rb %f\n", cone->rb);
  printf("height %f\n", cone->height);
  printf("nverts %i\n", cone->nverts);
  printf("endcap %i\n", cone->endcap);
  mav_surfaceParamsPrint("surface params ", *cone->sp);
  mav_matrixPrint("matrix\n", cone->matrix);
  printf("userdef %p\n", cone->userdef);

  return 1;
}
