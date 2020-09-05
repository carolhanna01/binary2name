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

MAV_class *mav_class_ellipse;



/* Routine to render an ellipse */

int mav_ellipseDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_ellipse *elip = (MAV_ellipse *) mav_objectDataGet(obj);
  MAV_vector vert, norm, *tmpvert, *tmpnorm;
  MAV_texCoord tex, *tmptex;
  float vang, dV, cang, dC, ca, sa, cc, sc;
  int i, j;
  int nverts, nchips;

/* set the number of vertices and chips */
  if ((mav_opt_curveLOD || elip->nverts<3 || elip->nchips<3) && di) {
    float dist;
    MAV_vector centre;

/* get world coords of centre of ellipse */
    centre.x= 0; centre.y= 0; centre.z= 0;
    centre= mav_vectorMult (centre, elip->matrix);

/* get (squared) distance to user */
    dist= mav_vectorDotProduct (mav_vectorSub (centre, di->vp.eye),
		mav_vectorSub (centre, di->vp.eye));
    if (dist<MAV_EPSILON) nchips= mav_opt_vertsMax;
    else {
      nchips= mav_opt_vertsMin + (int) (elip->height*elip->height/dist *
		mav_opt_curveFactor);
      if (nchips<mav_opt_vertsMin) nchips= mav_opt_vertsMin;
      else if (nchips>mav_opt_vertsMax) nchips= mav_opt_vertsMax;
    }
    nchips /= 4;
    if (nchips %2==1) nchips ++;

    if (dist<MAV_EPSILON) nverts= mav_opt_vertsMax;
    else {
      nverts= mav_opt_vertsMin + (int) (elip->radius*elip->radius/dist *
		mav_opt_curveFactor);
      if (nverts<mav_opt_vertsMin) nverts= mav_opt_vertsMin;
      else if (nverts>mav_opt_vertsMax) nverts= mav_opt_vertsMax;
    }
  } else {
/* no LOD - use fixed values */
    nverts= elip->nverts;
    nchips= elip->nchips;
  }

/* Set the correct colouring */

  mav_surfaceParamsUse(elip->sp); 

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(elip->matrix);

/* Malloc off memory to store the vertices */

  tmpvert = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));
  tmpnorm = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));
  tmptex  = (MAV_texCoord *) mav_malloc((nverts+1)*sizeof(MAV_texCoord));

/* 
   Chip angle is angle from Z axis in the range 90 degrees to -90 
   degress in nchips steps.
*/

  cang=MAV_PI_OVER_2;
  dC=MAV_PI/nchips;

  dV=MAV_2_PI/nverts;

/* 
   For the first chip angle (90 degrees) all vertices are at the north
   pole and all their normals are 0,0,1. Store the vertex and normal in
   a tmp array indexed by vertex angle.
*/
  
  for (i=0; i<=nverts; i++) {
    tmpvert[i].x=0.0;
    tmpvert[i].y=0.0;
    tmpvert[i].z=elip->height;

    if (elip->sp->mode==MAV_MATERIAL || elip->sp->mode>=MAV_LIT_TEXTURE) {
      tmpnorm[i].x=0.0;
      tmpnorm[i].y=0.0;
      tmpnorm[i].z=1.0;
    }

    if (elip->sp->mode>=MAV_TEXTURE) {
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
    vert.z=elip->height*sc;

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

      if (elip->sp->mode==MAV_MATERIAL || elip->sp->mode>=MAV_LIT_TEXTURE) {
        norm.x = ca/elip->radius;
        norm.y = sa/elip->radius;
        norm.z = sc/elip->height;
        norm= mav_vectorNormalize(norm);
	mav_gfxNormal(tmpnorm[i]);
      }

      if (elip->sp->mode>=MAV_TEXTURE) {
        tex.s=vang/MAV_2_PI;
        tex.t=cang/MAV_PI+0.5;
	mav_gfxTexCoord(tmptex[i]);
      }

      vert.x=elip->radius*ca;
      vert.y=elip->radius*sa;

      mav_gfxVertex(tmpvert[i]);

      if (elip->sp->mode==MAV_MATERIAL || elip->sp->mode>=MAV_LIT_TEXTURE) {
	mav_gfxNormal(norm);
	tmpnorm[i]=norm;
      }

      if (elip->sp->mode>=MAV_TEXTURE) {
	mav_gfxTexCoord(tex);
	tmptex[i]=tex;
      }

      mav_gfxVertex(vert);
      tmpvert[i]=vert;
      
      vang+=dV;
    }

    mav_gfxStripQEnd();
  }  

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  mav_free(tmpvert);
  mav_free(tmpnorm);
  mav_free(tmptex);

  return 1;
}



/* Routine to calculate the bounding box of an ellipse (quick but overestimates) */

int mav_ellipseBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_ellipse *elip = (MAV_ellipse *) mav_objectDataGet(obj);
  MAV_BB tmp;

/* Local coordinate frame BB */

  tmp.min.x = -elip->radius;
  tmp.min.y = -elip->radius;
  tmp.min.z = -elip->height;

  tmp.max.x = elip->radius;
  tmp.max.y = elip->radius;
  tmp.max.z = elip->height;

/* Global axis align it */ 

  mav_BBAlign(tmp, elip->matrix, bb);

  return 1;
}



/* Another routine to calculate the bounding box of an ellipse (slow but accurate) */

int mav_ellipseBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_ellipse *elip = (MAV_ellipse *) mav_objectDataGet(obj);
  MAV_vector vert;
  float vang, dV, cang, dC, ca, sa, cc, sc;
  int i, j;

  /* Initialise BB */

  mav_BBCompInit(bb);

  /* Vertices calculated in same was as draw callback */
  
  cang=MAV_PI_OVER_2;
  dC=MAV_PI/elip->nchips;

  dV=MAV_2_PI/elip->nverts;

  /* poles */

  vert.x=0.0;
  vert.y=0.0;
  vert.z=elip->height;
  mav_BBCompPt(mav_vectorMult(vert, elip->matrix), bb);

  vert.z=-elip->height;
  mav_BBCompPt(mav_vectorMult(vert, elip->matrix), bb);

  /* surface */
  
  for (j=1; j<elip->nchips; j++) {

    cang-=dC;
    vang=0.0;
         
    cc=cos(cang);
    sc=sin(cang);
    vert.z=elip->height*sc;

    for (i=0; i<elip->nverts; i++) {
      
      ca=cos(vang)*cc;
      sa=sin(vang)*cc;

      vert.x=elip->radius*ca;
      vert.y=elip->radius*sa;
      mav_BBCompPt(mav_vectorMult(vert, elip->matrix), bb);

      vang+=dV;
    }
  }

  return 1;
}



/* Routine to intersect an ellipse */

int mav_ellipseIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_ellipse *elip = (MAV_ellipse *) mav_objectDataGet(obj);
  MAV_line ln2;
  float a, b, c, sr, sr2, t1, t2, st1, st2, rmaj2, rmin2;

  o->pt1=-100.0;
  o->pt2=-100.0;

/* Rotate and translate line pt and dir so that the ellipse is centered and axis aligned */
  
  ln2= mav_lineTransFrame(*ln, elip->matrix);

/* Calculate the coefficients of the quadratic */

  rmaj2=elip->radius * elip->radius;
  rmin2=elip->height * elip->height;

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

/* Check is ellipse is behind us */

  if (st1<0.0 && st2<0.0) return(MAV_FALSE);
  
/* Check if ray originates inside object - if so set dist to closed intersection to zero */

  if (st1 < 0.0 && st2 > 0.0) st1=0.0;
  
/* Compensate for scale */

  o->pt1 = st1*mav_matrixScaleGet(elip->matrix);
  o->pt2 = st2*mav_matrixScaleGet(elip->matrix);

  return (1);
}



/* Routine to identify an ellipse */

int mav_ellipseID(MAV_object *obj, char **id)
{
  *id= "ellipse";
  return 1;
}



/* Routine to return the userdef field of an ellipse */

int mav_ellipseGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_ellipse *elip = (MAV_ellipse *) mav_objectDataGet(obj);

  *ud= &elip->userdef;

  return 1;
}



/* Routine to return the matrix field of an ellipse */

int mav_ellipseGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_ellipse *elip = (MAV_ellipse *) mav_objectDataGet(obj);

  *mat= &elip->matrix;

  return 1;
}



/* Routine to return the surface params field of an ellipse */

int mav_ellipseGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_ellipse *elip = (MAV_ellipse *) mav_objectDataGet(obj);

  *sp= &elip->sp;

  return 1;
}



/* Routine to dump an ellipse */

int mav_ellipseDump(MAV_object *obj)
{
  MAV_ellipse *elip = (MAV_ellipse *) mav_objectDataGet(obj);

  printf("*** Dumping object %p - a MAV_ellipse with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("radius %f\n", elip->radius);
  printf("height %f\n", elip->height);
  printf("nverts %i\n", elip->nverts);
  printf("nchips %i\n", elip->nchips);
  mav_surfaceParamsPrint("surface params ", *elip->sp);
  mav_matrixPrint("matrix\n", elip->matrix);
  printf("userdef %p\n", elip->userdef);

  return 1;
}

