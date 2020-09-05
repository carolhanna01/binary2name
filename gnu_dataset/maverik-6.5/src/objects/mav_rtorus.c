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

MAV_class *mav_class_rtorus;
int mavlib_signedLineInfPlaneIntersection(MAV_vector, MAV_vector, MAV_line, MAV_objectIntersection *);
int mavlib_signedLinePolygonIntersection(MAV_polygon *, MAV_line, MAV_objectIntersection *);



/* Routine to render a rectangular torus */

int mav_rtorusDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_rtorus *rt = (MAV_rtorus *) mav_objectDataGet(obj);
  MAV_vector norm, *vert0, *vert1, *vert2, *vert3, *norm0, *norm1;
  MAV_texCoord tex;
  float ang, dA, ca, sa, w;
  int i;
  int nchips;

/* set number of chips */
  if ((mav_opt_curveLOD || rt->nchips<3) && di) {
    float dist;
    MAV_vector centre;

/* get world coords of centre of rtorus */
    centre.x= 0;
    centre.y= 0;
    centre.z= 0;
    centre= mav_vectorMult (centre, rt->matrix);

/* get least possible distance from top/bottom face to user */
    dist= sqrt(mav_vectorDotProduct (mav_vectorSub (centre, di->vp.eye),
                mav_vectorSub (centre, di->vp.eye)))-rt->height*0.5;
    dist= dist*dist;
    if (dist<MAV_EPSILON) nchips= mav_opt_vertsMax;
    else {
      nchips= mav_opt_vertsMin + (int) (rt->radius*rt->radius/dist * mav_opt_curveFactor);
      if (nchips<mav_opt_vertsMin) nchips= mav_opt_vertsMin;
      else if (nchips>mav_opt_vertsMax) nchips= mav_opt_vertsMax;
    }
    nchips /= 4;
    if (nchips %2 ==1) nchips ++;
  } else {
/* no LOD - use fixed value */
    nchips= rt->nchips;
  }

/* Set the correct colouring */

  mav_surfaceParamsUse(rt->sp); 

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(rt->matrix);

/* Malloc off memory to store the vertices */

  vert0 = (MAV_vector *) mav_malloc((nchips+1)*sizeof(MAV_vector));
  vert1 = (MAV_vector *) mav_malloc((nchips+1)*sizeof(MAV_vector));
  vert2 = (MAV_vector *) mav_malloc((nchips+1)*sizeof(MAV_vector));
  vert3 = (MAV_vector *) mav_malloc((nchips+1)*sizeof(MAV_vector));
  norm0 = (MAV_vector *) mav_malloc((nchips+1)*sizeof(MAV_vector));
  norm1 = (MAV_vector *) mav_malloc((nchips+1)*sizeof(MAV_vector));
  
  dA=rt->angle/nchips;
  ang=0.0;
  w= rt->width*0.5;

/* Store the 4 vertices and 2 normals for each chip angle */ 

  for (i=0; i<=nchips; i++) {

    ca=cos(ang);
    sa=sin(ang);

    vert0[i].x=(rt->radius - w) * ca;
    vert0[i].y=(rt->radius - w) * sa;
    vert0[i].z=-rt->height*0.5;

    vert1[i].x=vert0[i].x;
    vert1[i].y=vert0[i].y;
    vert1[i].z=-vert0[i].z;

    vert2[i].x=(rt->radius + w) * ca;
    vert2[i].y=(rt->radius + w) * sa;
    vert2[i].z=vert0[i].z;

    vert3[i].x=vert2[i].x;
    vert3[i].y=vert2[i].y;
    vert3[i].z=vert1[i].z;

    if (rt->sp->mode==MAV_MATERIAL || rt->sp->mode>=MAV_LIT_TEXTURE) {
      norm0[i].x=-ca;
      norm0[i].y=-sa;
      norm0[i].z=0.0;

      norm1[i].x=ca;
      norm1[i].y=sa;
      norm1[i].z=0.0;
    }

    ang+=dA;
  }
  
/* Inner face */

  mav_gfxStripQBegin();
  for (i=0; i<=nchips; i++) {
    if (rt->sp->mode==MAV_MATERIAL || rt->sp->mode>=MAV_LIT_TEXTURE) mav_gfxNormal(norm0[i]);
    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.s=((float) i)/nchips;
      tex.t=0.0;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert0[i]);

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.t=1;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert1[i]);
  } 
  mav_gfxStripQEnd();

/* Outer face */

  mav_gfxStripQBegin();
  for (i=0; i<=nchips; i++) {
    if (rt->sp->mode==MAV_MATERIAL || rt->sp->mode>=MAV_LIT_TEXTURE) mav_gfxNormal(norm1[i]);
    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.s=((float) i)/nchips;
      tex.t=1.0;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert3[i]);

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.t=0.0;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert2[i]);
  }
  mav_gfxStripQEnd();

/* Upper face */

  if (rt->sp->mode==MAV_MATERIAL || rt->sp->mode>=MAV_LIT_TEXTURE) {
    norm.x=0.0;
    norm.y=0.0;
    norm.z=1.0;
    mav_gfxNormal(norm);
  }

  mav_gfxStripQBegin();
  for (i=0; i<=nchips; i++) {
    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.s=0.0;
      tex.t=((float) i)/nchips;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert1[i]);

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.s=1.0;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert3[i]);
  }
  mav_gfxStripQEnd();

/* Lower face */

  if (rt->sp->mode==MAV_MATERIAL || rt->sp->mode>=MAV_LIT_TEXTURE) {
    norm.z=-1.0;  
    mav_gfxNormal(norm);
  }

  mav_gfxStripQBegin();
  for (i=0; i<=nchips; i++) {
    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.s=1.0;
      tex.t=((float) i)/nchips;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert2[i]);

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.s=0.0;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert0[i]);
  }
  mav_gfxStripQEnd();

/* Draw endcaps if requested */

  if (rt->endcap) {

    if (rt->sp->mode==MAV_MATERIAL || rt->sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=0.0;
      norm.y=-1.0;
      norm.z=0.0;
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.s=0;
      tex.t=1;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert1[0]);

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.t=0;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert0[0]);

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.s=1;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert2[0]);

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.t=1;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert3[0]);

    mav_gfxPolygonEnd();

    if (rt->sp->mode==MAV_MATERIAL || rt->sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=-sin(rt->angle);
      norm.y=cos(rt->angle);
      norm.z=0.0;
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.s=0;
      tex.t=0;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert0[nchips]);

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.t=1;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert1[nchips]);

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.s=1;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert3[nchips]);

    if (rt->sp->mode>=MAV_TEXTURE) {
      tex.t=0;
      mav_gfxTexCoord(tex);
    }
    mav_gfxVertex(vert2[nchips]);

    mav_gfxPolygonEnd();
  }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  mav_free(vert0);
  mav_free(vert1);
  mav_free(vert2);
  mav_free(vert3);
  mav_free(norm0);
  mav_free(norm1);

  return 1;
}



/* Routine to calculate the bounding box of a rectangular torus (quick but overestimates) */

int mav_rtorusBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_rtorus *rt = (MAV_rtorus *) mav_objectDataGet(obj);
  MAV_BB tmp;
  float w= rt->width*0.5;

/* Local coordinate frame BB depends on quadrant of torus's angle */

  tmp.max.x=rt->radius + w;

  if (rt->angle < MAV_PI_OVER_2) 
  {
    tmp.min.x=(rt->radius-w) * cos(rt->angle);
    tmp.min.y=0.0;
    tmp.max.y=(rt->radius+w) * sin(rt->angle);
  } 
  else if (rt->angle < MAV_PI) 
  {
    tmp.min.x=(rt->radius+w) * cos(rt->angle);
    tmp.min.y=0.0;
    tmp.max.y=rt->radius+w;
  }
  else if (rt->angle < 3.0*MAV_PI_OVER_2) 
  {
    tmp.min.x=-(rt->radius+w);
    tmp.min.y=(rt->radius+w) * sin(rt->angle);
    tmp.max.y=rt->radius+w;
  } 
  else
  {
    tmp.min.x=-(rt->radius+w);
    tmp.min.y=-(rt->radius+w);
    tmp.max.y=rt->radius+w;
  }
  
  tmp.max.z=rt->height*0.5;
  tmp.min.z=-tmp.max.z;

/* Global axis align it */ 

  mav_BBAlign(tmp, rt->matrix, bb);

  return 1;
}



/* Another routine to calculate the bounding box of a rectangular torus (slow but accurate) */

int mav_rtorusBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_rtorus *rt = (MAV_rtorus *) mav_objectDataGet(obj);
  MAV_vector vert;
  float ang, dA, ca, sa, w, h;
  int i;

  /* Initialise BB */

  mav_BBCompInit(bb);

  /* Vertices calculated in same was as draw callback */

  dA=rt->angle/rt->nchips;
  ang=0.0;
  w= rt->width*0.5;
  h= rt->height*0.5;

  for (i=0; i<=rt->nchips; i++) {

    ca=cos(ang);
    sa=sin(ang);

    vert.x=(rt->radius - w) * ca;
    vert.y=(rt->radius - w) * sa;
    vert.z=h;
    mav_BBCompPt(mav_vectorMult(vert, rt->matrix), bb);

    vert.z=-vert.z;
    mav_BBCompPt(mav_vectorMult(vert, rt->matrix), bb);

    vert.x=(rt->radius + w) * ca;
    vert.y=(rt->radius + w) * sa;
    vert.z=h;
    mav_BBCompPt(mav_vectorMult(vert, rt->matrix), bb);

    vert.z=-vert.z;
    mav_BBCompPt(mav_vectorMult(vert, rt->matrix), bb);

    ang+=dA;
  }

  return 1;
}



/* Routine to intersect a rectangular torus */

int mav_rtorusIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_rtorus *rt= (MAV_rtorus *) mav_objectDataGet(obj);
  MAV_line ln2;
  MAV_polygon apoly;
  MAV_vector top, bot;
  MAV_objectIntersection dist[10];
  float topdist, botdist, angle, a, b, c, curve1, curve2, root;
  int i, num_hits;

  num_hits= 0;
  i=0;

  o->pt1=-100.0;
  o->pt2=-100.0;

  ln2= mav_lineTransFrame(*ln, rt->matrix);

/* check top and bottom */
  if (ln2.dir.z != 0) {
    topdist= (rt->height*0.5 - ln2.pt.z)/ln2.dir.z;
    botdist= (-rt->height*0.5 - ln2.pt.z)/ln2.dir.z;

    top.x= ln2.pt.x+topdist*ln2.dir.x;
    top.y= ln2.pt.y+topdist*ln2.dir.y;
    top.z= rt->height*0.5;

    bot.x= ln2.pt.x+botdist*ln2.dir.x;
    bot.y= ln2.pt.y+botdist*ln2.dir.y;
    bot.z= -rt->height*0.5;

    if (((top.x*top.x+top.y*top.y)<=((rt->radius+rt->width*0.5)*(rt->radius+rt->width*0.5)))&&
	((top.x*top.x+top.y*top.y)>=((rt->radius-rt->width*0.5)*(rt->radius-rt->width*0.5)))) {
      angle= atan2 (top.y, top.x);
      if (((angle>=0)&&(angle<=rt->angle))||((angle<0)&&
		((angle+2.0*MAV_PI)<=rt->angle))) {
	if (topdist<=0.0) 
	{
	  i=!i;
	}
	else
	{
	  dist[num_hits].pt1= topdist;
	  num_hits++;
	}
      }
    }

    if (((bot.x*bot.x+bot.y*bot.y)<=((rt->radius+rt->width*0.5)*(rt->radius+rt->width*0.5)))&&
        ((bot.x*bot.x+bot.y*bot.y)>=((rt->radius-rt->width*0.5)*(rt->radius-rt->width*0.5)))) {
      angle= atan2 (bot.y, bot.x);
      if (((angle>=0)&&(angle<=rt->angle))||((angle<0)&&
                ((angle+2.0*MAV_PI)<=rt->angle))) {
	if (botdist<=0.0) 
	{
	  i=!i;
	}
	else
	{
	  dist[num_hits].pt1= botdist;
	  num_hits ++;
	}
      }
    }

  }

/* inner and outer curves */
/* outer first (radius==rt->radius+rt->width*0.5) */
  a= ln2.dir.x*ln2.dir.x + ln2.dir.y*ln2.dir.y;
  b= 2.0*(ln2.pt.x*ln2.dir.x + ln2.pt.y*ln2.dir.y);
  c= ln2.pt.x*ln2.pt.x + ln2.pt.y*ln2.pt.y -
	(rt->radius+rt->width*0.5)*(rt->radius+rt->width*0.5);

  if (a != 0) {
    root= b*b-4*a*c;
    if (root>0) {
      root= sqrt (root);
      curve1= (-b+root)/(2.0*a);
      curve2= (-b-root)/(2.0*a);

      top.x= ln2.pt.x + curve1*ln2.dir.x;
      top.y= ln2.pt.y + curve1*ln2.dir.y;
      top.z= ln2.pt.z + curve1*ln2.dir.z;

      bot.x= ln2.pt.x + curve2*ln2.dir.x;
      bot.y= ln2.pt.y + curve2*ln2.dir.y;
      bot.z= ln2.pt.z + curve2*ln2.dir.z;

      if ((top.z>= -rt->height*0.5)&&(top.z<=rt->height*0.5)) {
      angle= atan2 (top.y, top.x);
      if (((angle>=0)&&(angle<=rt->angle))||((angle<0)&&
		((angle+2.0*MAV_PI)<=rt->angle))) {
	if (curve1<=0.0) 
	{
	  i=!i;
	}
	else
	{
	  dist[num_hits].pt1= curve1;
	  num_hits ++;
	}
      }
      }

      if ((bot.z>= -rt->height*0.5)&&(bot.z<=rt->height*0.5)) {
      angle= atan2 (bot.y, bot.x);
      if (((angle>=0)&&(angle<=rt->angle))||((angle<0)&&
                ((angle+2.0*MAV_PI)<=rt->angle))) {
	if (curve2<=0.0) 
	{
          i=!i;
        }
	else
	{
	  dist[num_hits].pt1= curve2;
	  num_hits ++;
	}
      }
      }
    }

/* inner curve (radius==rt->radius-rt->width*0.5)  a and b stay the same */
    c= ln2.pt.x*ln2.pt.x + ln2.pt.y*ln2.pt.y -
	(rt->radius-rt->width*0.5)*(rt->radius-rt->width*0.5);
    root= b*b-4*a*c;
    if (root>0) {
      root= sqrt (root);
      curve1= (-b+root)/(2.0*a);
      curve2= (-b-root)/(2.0*a);

      top.x= ln2.pt.x + curve1*ln2.dir.x;
      top.y= ln2.pt.y + curve1*ln2.dir.y;
      top.z= ln2.pt.z + curve1*ln2.dir.z;

      bot.x= ln2.pt.x + curve2*ln2.dir.x;
      bot.y= ln2.pt.y + curve2*ln2.dir.y;
      bot.z= ln2.pt.z + curve2*ln2.dir.z;

      if ((top.z>= -rt->height*0.5)&&(top.z<=rt->height*0.5)) {
      angle= atan2 (top.y, top.x);
      if (((angle>=0)&&(angle<=rt->angle))||((angle<0)&&
                ((angle+2.0*MAV_PI)<=rt->angle))) {
	if (curve1<=0.0) 
	{
	  i=!i;
	}
	else
	{
	  dist[num_hits].pt1= curve1;
	  num_hits ++;
	}
      }
      }

      if ((bot.z>= -rt->height*0.5)&&(bot.z<=rt->height*0.5)) {
      angle= atan2 (bot.y, bot.x);
      if (((angle>=0)&&(angle<=rt->angle))||((angle<0)&&
                ((angle+2.0*MAV_PI)<=rt->angle))) {
	if (curve2<=0.0) 
	{
	  i=!i;
	}
	else
	{
	  dist[num_hits].pt1= curve2;
	  num_hits ++;
	}
      }
      }
    }

  }

/* endcaps */
  apoly.vert= mav_malloc (4*sizeof (MAV_vector));

  apoly.np= 4;
  apoly.norm.x=0.0;
  apoly.norm.y=-1.0;
  apoly.norm.z=0.0;
  
  apoly.vert[0].x=rt->radius+rt->width*0.5;
  apoly.vert[0].y=0;
  apoly.vert[0].z=rt->height*0.5;

  apoly.vert[1].x=rt->radius-rt->width*0.5;
  apoly.vert[1].y=0;
  apoly.vert[1].z=rt->height*0.5;

  apoly.vert[2].x=rt->radius-rt->width*0.5;
  apoly.vert[2].y=0;
  apoly.vert[2].z=-rt->height*0.5;

  apoly.vert[3].x=rt->radius+rt->width*0.5;
  apoly.vert[3].y=0;
  apoly.vert[3].z=-rt->height*0.5;

  if (mavlib_signedLinePolygonIntersection(&apoly, ln2, &dist[num_hits])) {
    if (dist[num_hits].pt1<=0) 
    {
      i=!i;
    }
    else
    {
      num_hits ++;
    }
  }

  apoly.norm.x=-sin(rt->angle);
  apoly.norm.y=cos(rt->angle);
  apoly.norm.z=0.0;

  apoly.vert[0].x=(rt->radius+rt->width*0.5)*cos(rt->angle);
  apoly.vert[0].y=(rt->radius+rt->width*0.5)*sin(rt->angle);
  apoly.vert[0].z=rt->height*0.5;

  apoly.vert[1].x=(rt->radius-rt->width*0.5)*cos(rt->angle);
  apoly.vert[1].y=(rt->radius-rt->width*0.5)*sin(rt->angle);
  apoly.vert[1].z=rt->height*0.5;

  apoly.vert[2].x=(rt->radius-rt->width*0.5)*cos(rt->angle);
  apoly.vert[2].y=(rt->radius-rt->width*0.5)*sin(rt->angle);
  apoly.vert[2].z=-rt->height*0.5;

  apoly.vert[3].x=(rt->radius+rt->width*0.5)*cos(rt->angle);
  apoly.vert[3].y=(rt->radius+rt->width*0.5)*sin(rt->angle);
  apoly.vert[3].z=-rt->height*0.5;

  if (mavlib_signedLinePolygonIntersection(&apoly, ln2, &dist[num_hits])) {
    if (dist[num_hits].pt1<=0) 
    {
      i=!i;
    }
    else
    {
      num_hits ++;
    }
  }

  mav_free(apoly.vert);

  if (i) {
    dist[num_hits].pt1=0;
    num_hits++;
  }

  return (mav_objectIntersectionsSort(num_hits, dist, mav_matrixScaleGet(rt->matrix), o));
}



/* Routine to calculate the signed distance to an infinite plane */

int mavlib_signedLineInfPlaneIntersection(MAV_vector pt, MAV_vector norm, MAV_line ln, MAV_objectIntersection *o)
{
  float dn, t;

  o->pt1=-100;
  o->pt2=-100;

/*
  Calculate dot product of the line direction and plane normal. Check if this is 
  zero, i.e. line runs parrallel to the plane.
*/

  dn= mav_vectorDotProduct(ln.dir, norm);

  if (dn == 0.0) return (MAV_FALSE);

/* Calculate the distance along the line to the point of interscetion */

  t = mav_vectorDotProduct(mav_vectorSub(pt, ln.pt), norm) / dn;

/* Calculate the point of intersection */

  o->intpt = mav_vectorAdd(ln.pt, mav_vectorScalar(ln.dir, t));
  o->pt1= t;
  o->pt2= t;

  return(MAV_TRUE);
}



/* Routine to calculate the signed distance to a polygon */

int mavlib_signedLinePolygonIntersection(MAV_polygon *apoly, MAV_line ln, MAV_objectIntersection *o)
{
  MAV_vector *new_vert, axis, norm;
  MAV_matrix rot;
  MAV_line rotln;
  float ca, sa, ang;
  int i, j, c=MAV_FALSE;

/* Malloc off space to rotate vertices so norm is parrallel to Z axis */

  new_vert=mav_malloc(apoly->np*sizeof(MAV_vector));

/* Find rotation so polygon's normal is aligned with Z axis */

  ca= apoly->norm.z;
  if (ca>=1.0)
  {
    rot= MAV_ID_MATRIX;
  }
  else
  {
    axis.x= apoly->norm.y;
    axis.y= -apoly->norm.x;
    axis.z= 0;

    ang= MAV_RAD2DEG(acos(ca));
    sa= mav_vectorMag(axis);
    if (sa<0.0) ang= 360-ang;

    rot= mav_matrixQuaternionConvert(mav_quaternionSet(mav_vectorNormalise(axis), ang));
  }

/* Rotate all the polygon pts to be Z axis aligned */

  for (i=0; i<apoly->np; i++) new_vert[i]= mav_vectorMult(apoly->vert[i], rot);

/* Rotate line by the same amount */

  rotln.pt= mav_vectorMult(ln.pt, rot);
  rotln.dir= mav_vectorMult(ln.dir, rot);

/* Check if line intersects with infite plane of the polygon */

  norm.x=0.0;
  norm.y=0.0;
  norm.z=1.0;

  if (mavlib_signedLineInfPlaneIntersection(new_vert[0], norm, rotln, o))
  {
    /* Check if intersection is within polygon - This bit taken from cga FAQ */

    for (i = 0, j = apoly->np-1; i < apoly->np; j = i++) {
      if ((((new_vert[i].y<=o->intpt.y) && (o->intpt.y<new_vert[j].y)) ||
           ((new_vert[j].y<=o->intpt.y) && (o->intpt.y<new_vert[i].y))) &&
          (o->intpt.x < (new_vert[j].x - new_vert[i].x) * (o->intpt.y - new_vert[i].y) / (new_vert[j].y - new_vert[i].y) + new_vert[i].x))
        
        c = !c;
    }
  }
  
  mav_free(new_vert);
  return(c);
}



/* Routine to identify a rectangular torus */

int mav_rtorusID(MAV_object *obj, char **id)
{
  *id= "rectangular torus";
  return 1;
}



/* Routine to return the userdef field of a rectangular torus */

int mav_rtorusGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_rtorus *rt = (MAV_rtorus *) mav_objectDataGet(obj);

  *ud= &rt->userdef;

  return 1;
}



/* Routine to return the matrix field of a rectangular torus */

int mav_rtorusGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_rtorus *rt = (MAV_rtorus *) mav_objectDataGet(obj);

  *mat= &rt->matrix;

  return 1;
}



/* Routine to return the surface params field of a rectangular torus */

int mav_rtorusGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_rtorus *rt = (MAV_rtorus *) mav_objectDataGet(obj);

  *sp= &rt->sp;

  return 1;
}



/* Routine to dump a rectangular torus */

int mav_rtorusDump(MAV_object *obj)
{
  MAV_rtorus *rt= (MAV_rtorus *) mav_objectDataGet(obj);

  printf("*** Dumping object %p - a MAV_rtorus with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("radius %f\n", rt->radius);
  printf("width %f\n", rt->width);
  printf("height %f\n", rt->height);
  printf("angle %f\n", rt->angle);
  printf("nchips %i\n", rt->nchips);
  printf("endcap %i\n", rt->endcap);
  mav_surfaceParamsPrint("surface params ", *rt->sp);
  mav_matrixPrint("matrix\n", rt->matrix);
  printf("userdef %p\n", rt->userdef);

  return 1;
}
