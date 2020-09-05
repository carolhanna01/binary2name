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

MAV_class *mav_class_ctorus;


/* Routine to render a ctorus */

int mav_ctorusDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_ctorus *ct = (MAV_ctorus *) mav_objectDataGet(obj);
  MAV_vector vert, norm, *fixedvert, *tmpvert, *fixednorm, *tmpnorm;
  MAV_texCoord tex, *tmptex;
  float dC, cang, dV, vang, cv, sv, cc, sc;
  int i, j;
  int nverts, nchips;

/* set the number of vertices and chips */
  if ((mav_opt_curveLOD || ct->nverts<3 || ct->nchips<3) && di) {
    float dist;
    MAV_vector centre;

/* get world coords of centre of ctorus */
    centre.x= 0; centre.y= 0; centre.z= 0;
    centre= mav_vectorMult (centre, ct->matrix);

/* compute (squared) distance to user */
    dist= mav_vectorDotProduct (mav_vectorSub (centre, di->vp.eye),
                mav_vectorSub (centre, di->vp.eye));
    if (dist<MAV_EPSILON) nchips= mav_opt_vertsMax;
    else {
      nchips= mav_opt_vertsMin + (int) (ct->rmajor*ct->rmajor/dist * mav_opt_curveFactor);
      if (nchips<mav_opt_vertsMin) nchips= mav_opt_vertsMin;
      else if (nchips>mav_opt_vertsMax) nchips= mav_opt_vertsMax;
    }
    nchips /= 4;
    if (nchips %2==1) nchips ++;

/* now compute (least possible) distance from endcap to user */
    dist= sqrt (dist);
    dist -= ct->rmajor;
    dist= dist*dist;
    if (dist < MAV_EPSILON) nverts= mav_opt_vertsMax;
    else {
      nverts= mav_opt_vertsMin + (int) (ct->rminor*ct->rminor/dist * mav_opt_curveFactor);
      if (nverts<mav_opt_vertsMin) nverts= mav_opt_vertsMin;
      else if (nverts>mav_opt_vertsMax) nverts= mav_opt_vertsMax;
    }
  } else {
/* no LOD - use fixed values */
    nverts= ct->nverts;
    nchips= ct->nchips;
  }

/* Set the correct colouring */

  mav_surfaceParamsUse(ct->sp); 

/* Store the current transformation matrix - then multiply it by the local transformation */
  
  mav_gfxMatrixPush();
  mav_gfxMatrixMult(ct->matrix);

/* Malloc off memory to store the vertices */

  tmpvert = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));
  tmpnorm = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));
  tmptex  = (MAV_texCoord *) mav_malloc((nverts+1)*sizeof(MAV_texCoord));
  fixedvert = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));
  fixednorm = (MAV_vector *) mav_malloc((nverts+1)*sizeof(MAV_vector));
    
/* Chips angle is angle around Z axis in the range 0 to angle in nchips steps */

  dC=ct->angle/nchips;
  cang=dC;

  dV=MAV_2_PI/nverts;
  vang=0.0;

/* 
   For the first chip angle (0 degrees) calculate the vertices and normal. Store
   these values twice, once as a tmp array like in drawSphere, and once in a fixed
   array so they can be rotated by any chip angle to sweep out the torus.
*/

  for (i=0; i<=nverts; i++) {

    cv=cos(vang);
    sv=sin(vang);

    if (ct->sp->mode==MAV_MATERIAL || ct->sp->mode>=MAV_LIT_TEXTURE) {
      fixednorm[i].x=cv;
      fixednorm[i].y=0.0;
      fixednorm[i].z=sv;
      tmpnorm[i]=fixednorm[i];
    }

    if (ct->sp->mode>=MAV_TEXTURE) {
      tmptex[i].s=((float) i)/nverts;
      tmptex[i].t=0;
    }

    fixedvert[i].x=ct->rminor*cv + ct->rmajor;
    fixedvert[i].y=0.0;
    fixedvert[i].z=ct->rminor*sv;
    tmpvert[i]=fixedvert[i];

    vang+=dV;
  }

/* Draw one endcap if requested - the vertices are given by the tmp vertex array */

  if (ct->endcap) {

    if (ct->sp->mode==MAV_MATERIAL || ct->sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=0.0;
      norm.y=-1.0;
      norm.z=0.0;
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();
    for (i=0; i<nverts; i++) {
      if (ct->sp->mode>=MAV_TEXTURE) {
        tex.s=((fixedvert[i].x-ct->rmajor)/ct->rminor)*0.5-0.5;
        tex.t=(fixedvert[i].z/ct->rminor)*0.5-0.5;
        mav_gfxTexCoord(tex);
      }
      mav_gfxVertex(tmpvert[i]);
    }
    mav_gfxPolygonEnd();
  }

/* Loop over the rest of the chip angles */

  for (j=1; j<=nchips; j++) {

    cc=cos(cang);
    sc=sin(cang);

    mav_gfxStripQBegin();

/* 
   For each chip angle, loop over the vertex angle and rotate the fixed 
   vertex and normal array by the chips angle to give the vertex position 
   and normal.
*/

    for (i=0; i<=nverts; i++) {

/* 
   Make a quad strip from the vertex and normal stored in the tmp array
   (index by vertex angle) and the vertex and normal calculated for this 
   chip and vertex angle.  

   Store the vertex and normal for this chip and vertex angle in the tmp array 
*/

      if (ct->sp->mode==MAV_MATERIAL || ct->sp->mode>=MAV_LIT_TEXTURE) {
        norm.x = fixednorm[i].x * cc;
        norm.y = fixednorm[i].x * sc;
        norm.z = fixednorm[i].z;
	mav_gfxNormal(tmpnorm[i]);
      }

      if (ct->sp->mode>=MAV_TEXTURE) {
        tex.s = tmptex[i].s;
        tex.t = cang/ct->angle;
	mav_gfxTexCoord(tmptex[i]);
      }

      vert.x = fixedvert[i].x * cc;
      vert.y = fixedvert[i].x * sc;
      vert.z = fixedvert[i].z;
      mav_gfxVertex(tmpvert[i]);

      if (ct->sp->mode==MAV_MATERIAL || ct->sp->mode>=MAV_LIT_TEXTURE) {
	mav_gfxNormal(norm);
        tmpnorm[i]=norm;
      }

      if (ct->sp->mode>=MAV_TEXTURE) {
	mav_gfxTexCoord(tex);
        tmptex[i]=tex;
      }

      mav_gfxVertex(vert);
      tmpvert[i]=vert;
    }
    
    mav_gfxStripQEnd();

    cang+=dC;
  }

/* Draw the other endcap if requested - vertices are given by the tmp array */ 
    
  if (ct->endcap) {

    if (ct->sp->mode==MAV_MATERIAL || ct->sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=-sin(ct->angle);
      norm.y=cos(ct->angle);
      norm.z=0.0;
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();
    for (i=nverts; i>0; i--) {
      if (ct->sp->mode>=MAV_TEXTURE) {
        tex.s=((fixedvert[i].x-ct->rmajor)/ct->rminor)*0.5-0.5;
        tex.t=(fixedvert[i].z/ct->rminor)*0.5-0.5;
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
  mav_free(fixedvert);
  mav_free(fixednorm);

  return 1;
}



/* Routine to calculate the bounding box of a ctorus (quick but overestimates) */

int mav_ctorusBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_ctorus *ct = (MAV_ctorus *) mav_objectDataGet(obj);
  MAV_BB tmp;

/* Local coordinate frame BB depends on quadrant of torus's angle */

  tmp.max.x=ct->rmajor + ct->rminor;

  if (ct->angle < MAV_PI_OVER_2) {
    tmp.min.x=(ct->rmajor-ct->rminor) * cos(ct->angle);
    tmp.min.y=0.0;
    tmp.max.y=(ct->rmajor+ct->rminor) * sin(ct->angle);
  }

  if (ct->angle >= MAV_PI_OVER_2 && ct->angle < MAV_PI) {
    tmp.min.x=(ct->rmajor+ct->rminor) * cos(ct->angle);
    tmp.min.y=0.0;
    tmp.max.y=ct->rmajor+ct->rminor;
  }

  if (ct->angle >= MAV_PI && ct->angle <= 3.0*MAV_PI_OVER_2) {
    tmp.min.x=-(ct->rmajor+ct->rminor);
    tmp.min.y=(ct->rmajor+ct->rminor) * sin(ct->angle);
    tmp.max.y=ct->rmajor+ct->rminor;
  }

  if (ct->angle > 3.0*MAV_PI_OVER_2) {
    tmp.min.x=-(ct->rmajor+ct->rminor);
    tmp.min.y=-(ct->rmajor+ct->rminor);
    tmp.max.y=ct->rmajor+ct->rminor;
  }

  tmp.min.z=-ct->rminor;
  tmp.max.z=ct->rminor;

/* Global axis align it */ 

  mav_BBAlign(tmp, ct->matrix, bb);

  return 1;
}



/* Another routine to calculate the bounding box of a ctorus (slow but accurate) */

int mav_ctorusBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_ctorus *ct = (MAV_ctorus *) mav_objectDataGet(obj);
  MAV_vector vert, *fixedvert;
  float dC, cang, dV, vang, cv, sv, cc, sc;
  int i, j;

  /* Initialise BB */

  mav_BBCompInit(bb);

  /* Vertices calculated in same was as draw callback */

  fixedvert = (MAV_vector *) mav_malloc(ct->nverts*sizeof(MAV_vector));

  dC=ct->angle/ct->nchips;
  cang=dC;

  dV=MAV_2_PI/ct->nverts;
  vang=0.0;

  for (i=0; i<ct->nverts; i++) {

    cv=cos(vang);
    sv=sin(vang);

    fixedvert[i].x=ct->rminor*cv + ct->rmajor;
    fixedvert[i].y=0.0;
    fixedvert[i].z=ct->rminor*sv;
    mav_BBCompPt(mav_vectorMult(fixedvert[i], ct->matrix), bb);

    vang+=dV;
  }

  for (j=1; j<=ct->nchips; j++) {

    cc=cos(cang);
    sc=sin(cang);

    for (i=0; i<ct->nverts; i++) {
      vert.x = fixedvert[i].x * cc;
      vert.y = fixedvert[i].x * sc;
      vert.z = fixedvert[i].z;
      mav_BBCompPt(mav_vectorMult(vert, ct->matrix), bb);
    }

    cang+=dC;
  }

  mav_free(fixedvert);

  return 1;
}



/* Routine to solve a quartic taken from Graphics Gems I  */

/*
 *  Roots3And4.c
 *
 *  Utility functions to find cubic and quartic roots,
 *  coefficients are passed like this:
 *
 *      c[0] + c[1]*x + c[2]*x^2 + c[3]*x^3 + c[4]*x^4 = 0
 *
 *  The functions return the number of non-complex roots and
 *  put the values into the s array.
 *
 *  Author:         Jochen Schwarze (schwarze@isa.de)
 *
 *  Jan 26, 1990    Version for Graphics Gems
 *  Oct 11, 1990    Fixed sign problem for negative q's in SolveQuartic
 *                  (reported by Mark Podlipec),
 *                  Old-style function definitions,
 *                  IsZero() as a macro
 *  Nov 23, 1990    Some systems do not declare acos() and cbrt() in
 *                  <math.h>, though the functions exist in the library.
 *                  If large coefficients are used, EQN_EPS should be
 *                  reduced considerably (e.g. to 1E-30), results will be
 *                  correct but multiple roots might be reported more
 *                  than once.
 */

#include <math.h>
#if (defined(WIN32) && !defined(__CYGWIN__)) || defined(macintosh)
double cbrt(double x) {return pow(x, (double)(1.0/3.0));}
#else
double cbrt(double x);
#endif

/* epsilon surrounding for near zero values */

#define     EQN_EPS     1e-30
#define     IsZero(x)   ((x) > -EQN_EPS && (x) < EQN_EPS)

int mavlib_SolveQuadric(double c[3], double s[2])
{
    double p, q, D;

    /* normal form: x^2 + px + q = 0 */

    p = c[ 1 ] / (2 * c[ 2 ]);
    q = c[ 0 ] / c[ 2 ];

    D = p * p - q;

    if (IsZero(D))
    {
        s[ 0 ] = - p;
        return 1;
    }
    else if (D < 0)
    {
        return 0;
    }
    else if (D > 0)
    {
        double sqrt_D = sqrt(D);

        s[ 0 ] =   sqrt_D - p;
        s[ 1 ] = - sqrt_D - p;
        return 2;
    }

    return 0;
}


int mavlib_SolveCubic(double c[4], double s[3])
{
    int     i, num;
    double  sub;
    double  A, B, C;
    double  sq_A, p, q;
    double  cb_p, D;

    /* normal form: x^3 + Ax^2 + Bx + C = 0 */

    A = c[ 2 ] / c[ 3 ];
    B = c[ 1 ] / c[ 3 ];
    C = c[ 0 ] / c[ 3 ];

    /*  substitute x = y - A/3 to eliminate quadric term:
        x^3 +px + q = 0 */

    sq_A = A * A;
    p = 1.0/3 * (- 1.0/3 * sq_A + B);
    q = 1.0/2 * (2.0/27 * A * sq_A - 1.0/3 * A * B + C);

    /* use Cardano's formula */

    cb_p = p * p * p;
    D = q * q + cb_p;

    if (IsZero(D))
    {
        if (IsZero(q)) /* one triple solution */
        {
            s[ 0 ] = 0;
            num = 1;
        }
        else /* one single and one double solution */
        {
            double u = cbrt(-q);

            s[ 0 ] = 2 * u;
            s[ 1 ] = - u;
            num = 2;
        }
    }
    else if (D < 0) /* Casus irreducibilis: three real solutions */
    {
        double phi = 1.0/3 * acos(-q / sqrt(-cb_p));
        double t = 2 * sqrt(-p);

        s[ 0 ] =   t * cos(phi);
        s[ 1 ] = - t * cos(phi + MAV_PI / 3);
        s[ 2 ] = - t * cos(phi - MAV_PI / 3);
        num = 3;
    }
    else /* one real solution */
    {
        double sqrt_D = sqrt(D);
        double u = cbrt(sqrt_D - q);
        double v = - cbrt(sqrt_D + q);

        s[ 0 ] = u + v;
        num = 1;
    }

    /* resubstitute */

    sub = 1.0/3 * A;

    for (i = 0; i < num; ++i)
        s[ i ] -= sub;

    return num;
}


int mavlib_SolveQuartic(double c[5], double s[4])
{
    double  coeffs[ 4 ];
    double  z, u, v, sub;
    double  A, B, C, D;
    double  sq_A, p, q, r;
    int     i, num;

    /* normal form: x^4 + Ax^3 + Bx^2 + Cx + D = 0 */

    A = c[ 3 ] / c[ 4 ];
    B = c[ 2 ] / c[ 4 ];
    C = c[ 1 ] / c[ 4 ];
    D = c[ 0 ] / c[ 4 ];

    /*  substitute x = y - A/4 to eliminate cubic term:
        x^4 + px^2 + qx + r = 0 */

    sq_A = A * A;
    p = - 3.0/8 * sq_A + B;
    q = 1.0/8 * sq_A * A - 1.0/2 * A * B + C;
    r = - 3.0/256*sq_A*sq_A + 1.0/16*sq_A*B - 1.0/4*A*C + D;

    if (IsZero(r))
    {
        /* no absolute term: y(y^3 + py + q) = 0 */

        coeffs[ 0 ] = q;
        coeffs[ 1 ] = p;
        coeffs[ 2 ] = 0;
        coeffs[ 3 ] = 1;

        num = mavlib_SolveCubic(coeffs, s);

        s[ num++ ] = 0;
    }
    else
    {
        /* solve the resolvent cubic ... */

        coeffs[ 0 ] = 1.0/2 * r * p - 1.0/8 * q * q;
        coeffs[ 1 ] = - r;
        coeffs[ 2 ] = - 1.0/2 * p;
        coeffs[ 3 ] = 1;

        (void) mavlib_SolveCubic(coeffs, s);

        /* ... and take the one real solution ... */

        z = s[ 0 ];

        /* ... to build two quadric equations */

        u = z * z - r;
        v = 2 * z - p;

        if (IsZero(u))
            u = 0;
        else if (u > 0)
            u = sqrt(u);
        else
            return 0;

        if (IsZero(v))
            v = 0;
        else if (v > 0)
            v = sqrt(v);
        else
            return 0;

        coeffs[ 0 ] = z - u;
        coeffs[ 1 ] = q < 0 ? -v : v;
        coeffs[ 2 ] = 1;

        num = mavlib_SolveQuadric(coeffs, s);

        coeffs[ 0 ]= z + u;
        coeffs[ 1 ] = q < 0 ? v : -v;
        coeffs[ 2 ] = 1;

        num += mavlib_SolveQuadric(coeffs, s + num);
    }

    /* resubstitute */

    sub = 1.0/4 * A;

    for (i = 0; i < num; ++i)
        s[ i ] -= sub;

    return num;
}



/* Routine to intersect a ctorus */

int mav_ctorusIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_ctorus *ct = (MAV_ctorus *) mav_objectDataGet(obj);
  MAV_line ln2; 
  MAV_objectIntersection dist[8];
  MAV_vector hit_pt;
  double coeffs[5], eqn_roots[4];
  float face_dist, temp, angle;
  int nhits, nroots, i=0;

  o->pt1=-100.0;
  o->pt2=-100.0;

  nhits= 0;

/* Rotate and translate line pt and dir so that the ctorus is centered and axis aligned */

  ln2= mav_lineTransFrame(*ln, ct->matrix);

/* calculate the coefficients of the quartic equation */
  temp= ln2.pt.x*ln2.pt.x+ln2.pt.y*ln2.pt.y+ln2.pt.z*ln2.pt.z -
        (ct->rmajor*ct->rmajor+ct->rminor*ct->rminor);

  coeffs[0]= temp*temp + 4*ct->rmajor*ct->rmajor*
        (ln2.pt.z*ln2.pt.z - ct->rminor*ct->rminor);
  coeffs[1]= 4.0*(temp*mav_vectorDotProduct(ln2.pt, ln2.dir)+
        2.0*ct->rmajor*ct->rmajor*ln2.pt.z*ln2.dir.z);
  coeffs[2]= 2.0*(2.0*mav_vectorDotProduct(ln2.pt, ln2.dir)*
        mav_vectorDotProduct(ln2.pt, ln2.dir) + temp +
        2.0*ct->rmajor*ct->rmajor*ln2.dir.z*ln2.dir.z);
  coeffs[3]= 4.0*mav_vectorDotProduct (ln2.pt, ln2.dir);
  coeffs[4]= 1.0;

  nroots= mavlib_SolveQuartic (coeffs, eqn_roots);

  for (; nroots>0; nroots--) {
    hit_pt.x= ln2.pt.x+eqn_roots[nroots-1]*ln2.dir.x;
    hit_pt.y= ln2.pt.y+eqn_roots[nroots-1]*ln2.dir.y;

    angle= atan2 (hit_pt.y, hit_pt.x);
    if (((angle>=0)&&(angle<=ct->angle))||((angle<0)&&
        ((angle+2.0*MAV_PI)<=ct->angle))) {
      if (eqn_roots[nroots-1]<=0.0) {
	i=!i;
      }
      else {
	dist[nhits].pt1= eqn_roots[nroots-1];
	nhits ++;
      }
    }
  }

/* now intersect the 2 circular faces */
/* face at angle==0 */

  if (ln2.dir.y != 0.0) {
    face_dist= -ln2.pt.y/ln2.dir.y;
    hit_pt.x= ln2.pt.x + face_dist * ln2.dir.x;
    hit_pt.y= 0;
    hit_pt.z= ln2.pt.z + face_dist * ln2.dir.z;

    if (((hit_pt.x-ct->rmajor)*(hit_pt.x-ct->rmajor)+
        hit_pt.z*hit_pt.z)<=(ct->rminor*ct->rminor)) {
      if (face_dist<=0.0) {
	i=!i;
      } 
      else {
	dist[nhits].pt1= face_dist;
	nhits ++;
      }
    }
  }

/* face at angle==ct->angle */

  temp= sin (ct->angle)*ln2.dir.x - cos (ct->angle)*ln2.dir.y;
  if (temp != 0) {
    face_dist= (cos(ct->angle)*ln2.pt.y - sin(ct->angle)*ln2.pt.x)/
        temp;

    hit_pt.x= ln2.pt.x + face_dist * ln2.dir.x;
    hit_pt.y= ln2.pt.y + face_dist * ln2.dir.y;
    hit_pt.z= ln2.pt.z + face_dist * ln2.dir.z;

    if (((hit_pt.x-ct->rmajor*cos(ct->angle))*
        (hit_pt.x-ct->rmajor*cos(ct->angle)) +
        (hit_pt.y-ct->rmajor*sin(ct->angle))*
        (hit_pt.y-ct->rmajor*sin(ct->angle)) +
        hit_pt.z*hit_pt.z)<=(ct->rminor*ct->rminor)) {
      if (face_dist<=0.0) {
	i=!i;
      }
      else {
	dist[nhits].pt1= face_dist;
	nhits ++;
      }
    }
  }

  if (i) {
    dist[nhits].pt1=0;
    nhits++;
  }


/* Sort intersection and return appropriate value */

  return(mav_objectIntersectionsSort(nhits, dist, mav_matrixScaleGet(ct->matrix), o));
}



/* Routine to identify a ctorus */

int mav_ctorusID(MAV_object *obj, char **id)
{
  *id= "circular torus";
  return 1;
}



/* Routine to return the userdef field of a ctorus */

int mav_ctorusGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_ctorus *ct = (MAV_ctorus *) mav_objectDataGet(obj);

  *ud= &ct->userdef;

  return 1;
}



/* Routine to return the matrix field of a ctorus */

int mav_ctorusGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_ctorus *ct = (MAV_ctorus *) mav_objectDataGet(obj);

  *mat= &ct->matrix;

  return 1;
}



/* Routine to return the surface params field of a ctorus */

int mav_ctorusGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_ctorus *ct = (MAV_ctorus *) mav_objectDataGet(obj);

  *sp= &ct->sp;

  return 1;
}



/* Routine to dump a ctorus */

int mav_ctorusDump(MAV_object *obj)
{
  MAV_ctorus *ct= (MAV_ctorus *) mav_objectDataGet(obj);

  printf("*** Dumping object %p - a MAV_ctorus with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("rmajor %f\n", ct->rmajor);
  printf("rminor %f\n", ct->rminor);
  printf("angle %f\n", ct->angle);
  printf("nverts %i\n", ct->nverts);
  printf("nchips %i\n", ct->nchips);
  printf("endcap %i\n", ct->endcap);
  mav_surfaceParamsPrint("surface params ", *ct->sp);
  mav_matrixPrint("matrix\n", ct->matrix);
  printf("userdef %p\n", ct->userdef);

  return 1;
}
