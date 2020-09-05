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

MAV_class *mav_class_hellipse;



/* Routine to render a half ellipse */

int mav_hellipseDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_hellipse *helip = (MAV_hellipse *) mav_objectDataGet(obj);
  MAV_vector vert, norm, *tmpvert, *tmpnorm;
  MAV_texCoord tex, *tmptex;
  float vang, dV, cang, dC, ca, sa, cc, sc;
  int i, j;
  int nverts, nchips;

/* set the number of vertices and chips */
  if ((mav_opt_curveLOD || helip->nverts<3 || helip->nchips<3) && di) {
    float dist;
    MAV_vector centre;

/* get world coords of centre of hellipse */
    centre.x= 0;
    centre.y= 0;
    centre.z= 0;
    centre= mav_vectorMult (centre, helip->matrix);

/* get (squared) distance to user */
    dist= (mav_vectorDotProduct (mav_vectorSub (centre, di->vp.eye),
                mav_vectorSub (centre, di->vp.eye)));
    if (dist<MAV_EPSILON) nchips= mav_opt_vertsMax;
    else {
      nchips= mav_opt_vertsMin + (int) (helip->height*helip->height/dist * mav_opt_curveFactor);
      if (nchips<mav_opt_vertsMin) nchips= mav_opt_vertsMin;
      else if (nchips>mav_opt_vertsMax) nchips= mav_opt_vertsMax;
    }
    nchips /= 4;
    if (nchips %2==1) nchips ++;

    if (dist < MAV_EPSILON) nverts= mav_opt_vertsMax;
    else {
      nverts= mav_opt_vertsMin + (int) (helip->radius*helip->radius/dist * mav_opt_curveFactor);
      if (nverts<mav_opt_vertsMin) nverts= mav_opt_vertsMin;
      else if (nverts>mav_opt_vertsMax) nverts= mav_opt_vertsMax;
    }
  } else {
/* no LOD - use fixed values */
    nverts= helip->nverts;
    nchips= helip->nchips;
  }

/* Set the correct colouring */

  mav_surfaceParamsUse(helip->sp); 

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(helip->matrix);

/* Malloc off memory to store the vertices */

  tmpvert = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));
  tmpnorm = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));
  tmptex  = (MAV_texCoord *) mav_malloc((nverts+1)*sizeof(MAV_texCoord));

/* 
   Chip angle is angle from Z axis in the range 90 degrees to 0 
   degress in nchips steps.
*/

  cang=MAV_PI_OVER_2;
  dC=MAV_PI/(2*nchips);

  dV=MAV_2_PI/nverts;

/* 
   For the first chip angle (90 degrees) all vertices are at the north
   pole and all their normals are 0,0,1. Store the vertex and normal in
   a tmp array indexed by vertex angle.
*/
  
  for (i=0; i<=nverts; i++) {
    tmpvert[i].x=0.0;
    tmpvert[i].y=0.0;
    tmpvert[i].z=helip->height;

    if (helip->sp->mode==MAV_MATERIAL || helip->sp->mode>=MAV_LIT_TEXTURE) {
      tmpnorm[i].x=0.0;
      tmpnorm[i].y=0.0;
      tmpnorm[i].z=1.0;
    }

    if (helip->sp->mode>=MAV_TEXTURE) {
      tmptex[i].s=((float) i)/nverts;
      tmptex[i].t=1;
    }
  }

/* Loop over the rest of the chip angles */ 

  for (j=1; j<=nchips; j++) {

    cang-=dC;
    vang=0.0;
         
    cc=cos(cang);
    sc=sin(cang);
    vert.z=helip->height*sc;

    mav_gfxStripQBegin();

/* 
   For each chip angle, loop over vertex angle and calculate the vertex 
   position and normals.
*/

    for (i=0; i<=nverts; i++) {
      
      ca=cos(vang)*cc;
      sa=sin(vang)*cc;
      
/* 
   Make a quad strip from the vertex and normal stored in the tmp array
   (index by vertex angle) and the vertex and normal calculated for this 
   chip and vertex angle.  

   Store the vertex and normal for this chip and vertex angle in the tmp array 
*/

      if (helip->sp->mode==MAV_MATERIAL || helip->sp->mode>=MAV_LIT_TEXTURE) {
        norm.x = ca/helip->radius;
        norm.y = sa/helip->radius;
        norm.z = sc/helip->height;
        norm= mav_vectorNormalize(norm);
	mav_gfxNormal(tmpnorm[i]);
      }

      if (helip->sp->mode>=MAV_TEXTURE) {
        tex.s=vang/MAV_2_PI;
        tex.t=cang/MAV_PI+0.5;
	mav_gfxTexCoord(tmptex[i]);
      }

      vert.x=helip->radius*ca;
      vert.y=helip->radius*sa;

      mav_gfxVertex(tmpvert[i]);

      if (helip->sp->mode==MAV_MATERIAL || helip->sp->mode>=MAV_LIT_TEXTURE) {
	mav_gfxNormal(norm);
	tmpnorm[i]=norm;
      }

      if (helip->sp->mode>=MAV_TEXTURE) {
	mav_gfxTexCoord(tex);
	tmptex[i]=tex;
      }

      mav_gfxVertex(vert);
      tmpvert[i]=vert;
      
      vang+=dV;
    }

    mav_gfxStripQEnd();
  }  

/* Draw the endcap if desired - vertices are stored in tmp array */
  
  if (helip->endcap) {

    if (helip->sp->mode==MAV_MATERIAL || helip->sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=0.0;
      norm.y=0.0;
      norm.z=-1.0;
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();
    for (i=nverts; i>0; i--) {
      if (helip->sp->mode>=MAV_TEXTURE) {
        tex.s=(tmpvert[i].x/helip->radius)*0.5-0.5;
        tex.t=(tmpvert[i].y/helip->radius)*0.5-0.5;
        mav_gfxTexCoord(tex);
      }
      mav_gfxVertex(tmpvert[i]);
    }
    mav_gfxPolygonEnd();
  }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  mav_free(tmpvert);
  mav_free(tmpnorm);
  mav_free(tmptex);

  return 1;
}



/* Routine to calculate the bounding box of a half ellipse (quick but overestimates) */

int mav_hellipseBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_hellipse *helip = (MAV_hellipse *) mav_objectDataGet(obj);
  MAV_BB tmp;

/* Local coordinate frame BB */

  tmp.min.x = -helip->radius;
  tmp.min.y = -helip->radius;
  tmp.min.z = 0.0;

  tmp.max.x = helip->radius;
  tmp.max.y = helip->radius;
  tmp.max.z = helip->height;

/* Global axis align it */ 

  mav_BBAlign(tmp, helip->matrix, bb);

  return(1);
}



/* Another routine to calculate the bounding box of a half ellipse (slow but accurate) */

int mav_hellipseBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_hellipse *helip = (MAV_hellipse *) mav_objectDataGet(obj);
  MAV_vector vert;
  float vang, dV, cang, dC, ca, sa, cc, sc;
  int i, j;

  /* Initialise BB */

  mav_BBCompInit(bb);

  /* Vertices calculated in same was as draw callback */

  cang=MAV_PI_OVER_2;
  dC=MAV_PI/(2*helip->nchips);

  dV=MAV_2_PI/helip->nverts;

  /* pole */

  vert.x=0.0;
  vert.y=0.0;
  vert.z=helip->height;
  mav_BBCompPt(mav_vectorMult(vert, helip->matrix), bb);

  /* surface */

  for (j=1; j<=helip->nchips; j++) {

    cang-=dC;
    vang=0.0;
         
    cc=cos(cang);
    sc=sin(cang);
    vert.z=helip->height*sc;

    for (i=0; i<helip->nverts; i++) {
      
      ca=cos(vang)*cc;
      sa=sin(vang)*cc;

      vert.x=helip->radius*ca;
      vert.y=helip->radius*sa;
      mav_BBCompPt(mav_vectorMult(vert, helip->matrix), bb);

      vang+=dV;
    }
  }

  return 1;
}



/* Routine to intersect a half ellipse */

int mav_hellipseIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_hellipse *helip = (MAV_hellipse *) mav_objectDataGet(obj);
  MAV_line ln2;
  float a, b, c, sr, sr2, t1, t2, st1, st2, zin, zout, rmaj2, rmin2;

  o->pt1=-100.0;
  o->pt2=-100.0;

/* Rotate and translate line pt and dir so that the hellipse is centered and axis aligned */

  ln2= mav_lineTransFrame(*ln, helip->matrix);

/* Calculate the coefficients of the quadratic - see documentation */

  rmaj2=helip->radius * helip->radius;
  rmin2=helip->height * helip->height;

  a = (ln2.dir.x*ln2.dir.x)/rmaj2 + (ln2.dir.y*ln2.dir.y)/rmaj2 + (ln2.dir.z*ln2.dir.z)/rmin2;
  b = 2.0*((ln2.pt.x*ln2.dir.x)/rmaj2 + (ln2.pt.y*ln2.dir.y)/rmaj2 + (ln2.pt.z*ln2.dir.z)/rmin2);
  c = (ln2.pt.x*ln2.pt.x)/rmaj2 + (ln2.pt.y*ln2.pt.y)/rmaj2 + (ln2.pt.z*ln2.pt.z)/rmin2 - 1.0;

/* a can never be zero */

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

/* Check is hellipse is behind us */

  if (st1<0.0 && st2<0.0) return(MAV_FALSE);
  
/* Check if ray originates inside object - if so set dist to closed intersection to zero */

  if (st1 < 0.0 && st2 > 0.0) st1=0.0;

/* Calculate in and out z intersections */

  zin=ln2.pt.z + st1*ln2.dir.z;
  zout=ln2.pt.z + st2*ln2.dir.z;

/* Reject intersections if lines goes totally below half ellipse */

  if (zin < 0.0 && zout < 0.0) return (MAV_FALSE);

/* Check if in intersects with flat surface */

  if (zin < 0.0) {

    if (ln2.dir.z == 0.0) return(MAV_FALSE);
        
/* Find the t which has zero as its z value */

    st1=-ln2.pt.z/ln2.dir.z;
  }


/* Check if out intersects with flat surface */

  if (zout < 0.0) {

    if (ln2.dir.z == 0.0) return(MAV_FALSE);
        
/* Find the t which has zero as its z value */

    st2=-ln2.pt.z/ln2.dir.z;
        
  }

/* Compensate for scale */

  o->pt1 = st1*mav_matrixScaleGet(helip->matrix);
  o->pt2 = st2*mav_matrixScaleGet(helip->matrix);

  return (1);
}



/* Routine to identify a half ellipse */

int mav_hellipseID(MAV_object *obj, char **id)
{
  *id= "half ellipse";
  return 1;
}



/* Routine to return the userdef field of a half ellipse */

int mav_hellipseGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_hellipse *helip = (MAV_hellipse *) mav_objectDataGet(obj);

  *ud= &helip->userdef;

  return 1;
}



/* Routine to return the matrix field of a half ellipse */

int mav_hellipseGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_hellipse *helip = (MAV_hellipse *) mav_objectDataGet(obj);

  *mat= &helip->matrix;

  return 1;
}



/* Routine to return the surface params field of a half ellipse */

int mav_hellipseGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_hellipse *helip = (MAV_hellipse *) mav_objectDataGet(obj);

  *sp= &helip->sp;

  return 1;
}



/* Routine to dump a half ellipse */

int mav_hellipseDump(MAV_object *obj)
{
  MAV_hellipse *helip = (MAV_hellipse *) mav_objectDataGet(obj);

  printf("*** Dumping object %p - a MAV_hellipse with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("radius %f\n", helip->radius);
  printf("height %f\n", helip->height);
  printf("nverts %i\n", helip->nverts);
  printf("nchips %i\n", helip->nchips);
  printf("endcap %i\n", helip->endcap);
  mav_surfaceParamsPrint("surface params ", *helip->sp);
  mav_matrixPrint("matrix\n", helip->matrix);
  printf("userdef %p\n", helip->userdef);

  return 1;
}
