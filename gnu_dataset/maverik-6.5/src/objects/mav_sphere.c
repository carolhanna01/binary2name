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

MAV_class *mav_class_sphere;



/* Routine to render a sphere */

int mav_sphereDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_sphere *sph = (MAV_sphere *) mav_objectDataGet(obj);
  MAV_vector vert, norm, *tmpvert, *tmpnorm;
  MAV_texCoord tex, *tmptex;
  float vang, dV, cang, dC, ca, sa, cc, sc;
  int i, j;
  int nverts, nchips;

/* set the number of vertices and chips */
  if ((mav_opt_curveLOD || sph->nverts<3 || sph->nchips<3) && di) {
    float dist;
    MAV_vector centre;

/* get world coords of centre of sphere */
    centre.x= 0; centre.y= 0; centre.z= 0;
    centre= mav_vectorMult (centre, sph->matrix);

/* get (squared) distance to user */
    dist= (mav_vectorDotProduct (mav_vectorSub (centre, di->vp.eye),
                mav_vectorSub (centre, di->vp.eye)));
    if (dist<MAV_EPSILON) nchips= mav_opt_vertsMax;
    else {
      nchips= mav_opt_vertsMin + (int) (sph->radius*sph->radius/dist * mav_opt_curveFactor);
      if (nchips<mav_opt_vertsMin) nchips= mav_opt_vertsMin;
      else if (nchips>mav_opt_vertsMax) nchips= mav_opt_vertsMax;
    }
    nchips /= 4;
    if (nchips %2==1) nchips ++;

    if (dist < MAV_EPSILON) nverts= mav_opt_vertsMax;
    else {
      nverts= mav_opt_vertsMin + (int) (sph->radius*sph->radius/dist * mav_opt_curveFactor);
      if (nverts<mav_opt_vertsMin) nverts= mav_opt_vertsMin;
      else if (nverts>mav_opt_vertsMax) nverts= mav_opt_vertsMax;
    }
  } else {
/* no LOD - use fixed values */
    nverts= sph->nverts;
    nchips= sph->nchips;
  }

/* Set the correct colouring */

  mav_surfaceParamsUse(sph->sp); 

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(sph->matrix);

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
    tmpvert[i].z=sph->radius;

    if (sph->sp->mode==MAV_MATERIAL || sph->sp->mode>=MAV_LIT_TEXTURE) {
      tmpnorm[i].x=0.0;
      tmpnorm[i].y=0.0;
      tmpnorm[i].z=1.0;
    }

    if (sph->sp->mode>=MAV_TEXTURE) {
      tmptex[i].s=((float) i)/nverts;
      tmptex[i].t=1.0;
    }
  }

/* Loop over the rest of the chip angles */ 

  for (j=1; j<=nchips; j++) {

    cang-=dC;
    vang=0.0;
         
    cc=cos(cang);
    sc=sin(cang);
    vert.z=sph->radius*sc;

    mav_gfxStripQBegin();

/* 
   For each chip angle, loop over vertex angle and calculate the vertex 
   position and normals.
*/

    for (i=0; i<=nverts; i++) {
      
      ca=cos(vang)*cc;
      sa=sin(vang)*cc;
      
      vert.x=sph->radius*ca;
      vert.y=sph->radius*sa;

      if (sph->sp->mode==MAV_MATERIAL || sph->sp->mode>=MAV_LIT_TEXTURE) {
        norm.x=ca;
        norm.y=sa;
        norm.z=sc;
      }
      
      if (sph->sp->mode>=MAV_TEXTURE) {
        tex.s=vang/MAV_2_PI;
        tex.t=cang/MAV_PI+0.5;
      }
/* 
   Make a quad strip from the vertex and normal stored in the tmp array
   (index by vertex angle) and the vertex and normal calculated for this 
   chip and vertex angle.  
*/

      if (sph->sp->mode==MAV_MATERIAL || sph->sp->mode>=MAV_LIT_TEXTURE) mav_gfxNormal(tmpnorm[i]);
      if (sph->sp->mode>=MAV_TEXTURE)  mav_gfxTexCoord(tmptex[i]);
      mav_gfxVertex(tmpvert[i]);
      
      if (sph->sp->mode==MAV_MATERIAL || sph->sp->mode>=MAV_LIT_TEXTURE) mav_gfxNormal(norm);
      if (sph->sp->mode>=MAV_TEXTURE)  mav_gfxTexCoord(tex);
      mav_gfxVertex(vert);

/* Store the vertex and normal for this chip and vertex angle in the tmp array */ 

      tmpvert[i]=vert;

      if (sph->sp->mode==MAV_MATERIAL || sph->sp->mode>=MAV_LIT_TEXTURE) tmpnorm[i]=norm;
      if (sph->sp->mode>=MAV_TEXTURE) tmptex[i]=tex;

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



/* Routine to calculate the bounding box of a sphere (quick but overestimates) */

int mav_sphereBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_sphere *sph = (MAV_sphere *) mav_objectDataGet(obj);
  MAV_BB tmp;

/* Local coordinate frame BB */

  tmp.min.x = -sph->radius;
  tmp.min.y = -sph->radius;
  tmp.min.z = -sph->radius;

  tmp.max.x = sph->radius;
  tmp.max.y = sph->radius;
  tmp.max.z = sph->radius;

/* Global axis align it */ 

  mav_BBAlign(tmp, sph->matrix, bb);

  return 1;
}



/* Another routine to calculate the bounding box of a sphere (slow but accurate) */

int mav_sphereBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_sphere *sph = (MAV_sphere *) mav_objectDataGet(obj);
  MAV_vector vert;
  float vang, dV, cang, dC, ca, sa, cc, sc;
  int i, j;

  /* Initialise BB */

  mav_BBCompInit(bb);

  /* Vertices calculated in same was as draw callback */

  cang=MAV_PI_OVER_2;
  dC=MAV_PI/sph->nchips;

  dV=MAV_2_PI/sph->nverts;

  /* poles */

  vert.x=0.0;
  vert.y=0.0;
  vert.z=sph->radius;
  mav_BBCompPt(mav_vectorMult(vert, sph->matrix), bb);

  vert.z=-sph->radius;
  mav_BBCompPt(mav_vectorMult(vert, sph->matrix), bb);

  /* surface */

  for (j=1; j<sph->nchips; j++) {

    cang-=dC;
    vang=0.0;
         
    cc=cos(cang);
    sc=sin(cang);
    vert.z=sph->radius*sc;

    for (i=0; i<sph->nverts; i++) {
      
      ca=cos(vang)*cc;
      sa=sin(vang)*cc;
      
      vert.x=sph->radius*ca;
      vert.y=sph->radius*sa;
      mav_BBCompPt(mav_vectorMult(vert, sph->matrix), bb);

      vang+=dV;
    }
  }

  return 1;
}



/* Routine to intersect a sphere */

int mav_sphereIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_sphere *sph = (MAV_sphere *) mav_objectDataGet(obj);
  MAV_line ln2;
  float a, b, c, sr, sr2, t1, t2, st1, st2;

  o->pt1=-100.0;
  o->pt2=-100.0;

/* Rotate and translate line so that the sphere is centered and axis aligned */

  ln2= mav_lineTransFrame(*ln, sph->matrix);

/* Calculate the coefficients of the quadratic - see documentation */

  a = ln2.dir.x*ln2.dir.x + ln2.dir.y*ln2.dir.y + ln2.dir.z*ln2.dir.z;
  b = 2.0*(ln2.pt.x*ln2.dir.x + ln2.pt.y*ln2.dir.y + ln2.pt.z*ln2.dir.z);
  c = ln2.pt.x*ln2.pt.x + ln2.pt.y*ln2.pt.y + ln2.pt.z*ln2.pt.z - sph->radius*sph->radius;

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

/* Check is sphere is behind us */

  if (st1<0.0 && st2<0.0) return(MAV_FALSE);
  
/* Check if ray originates inside object - if so set dist to closed intersection to zero */

  if (st1 < 0.0 && st2 > 0.0) st1=0.0;

/* Compensate for scale */

  o->pt1= st1*mav_matrixScaleGet(sph->matrix);
  o->pt2= st2*mav_matrixScaleGet(sph->matrix);

  return (MAV_TRUE);
}



/* Routine to identify a sphere */

int mav_sphereID(MAV_object *obj, char **id)
{
  *id= "sphere";
  return 1;
}



/* Routine to return the userdef field of a sphere */

int mav_sphereGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_sphere *sph = (MAV_sphere *) mav_objectDataGet(obj);

  *ud= &sph->userdef;

  return 1;
}



/* Routine to return the matrix field of a sphere */

int mav_sphereGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_sphere *sph = (MAV_sphere *) mav_objectDataGet(obj);

  *mat= &sph->matrix;

  return 1;
}



/* Routine to return the surface params field of a sphere */

int mav_sphereGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_sphere *sph = (MAV_sphere *) mav_objectDataGet(obj);

  *sp= &sph->sp;

  return 1;
}



/* Routine to dump a sphere */

int mav_sphereDump(MAV_object *obj)
{
  MAV_sphere *sph = (MAV_sphere *) mav_objectDataGet(obj);

  printf("*** Dumping object %p - a MAV_sphere with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("radius %f\n", sph->radius);
  printf("nverts %i\n", sph->nverts);
  printf("nchips %i\n", sph->nchips);
  mav_surfaceParamsPrint("surface params ", *sph->sp);
  mav_matrixPrint("matrix\n", sph->matrix);
  printf("userdef %p\n", sph->userdef);

  return 1;
}

