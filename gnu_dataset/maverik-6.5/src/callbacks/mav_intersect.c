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


#include "mavlib_callbacks.h"
#include <stdlib.h>
#include <stdio.h>



/* Wrapper routines to set and execute intersect callback */

MAV_callback *mav_callback_intersect;

void mav_callbackIntersectSet(MAV_window *w, MAV_class *c, MAV_callbackIntersectFn fn)
{
  mav_callbackSet(mav_callback_intersect, w, c, (MAV_callbackFn) fn);
}

int mav_callbackIntersectExec(MAV_window *w, MAV_object *o, MAV_line ln, MAV_objectIntersection *oint)
{
  MAV_BB bb;
  int rv=0;
  
/* If no intersect method defined, back off to intersecting with BB */

  if (mav_callbackQuery(mav_callback_intersect, w, o))
  {
    rv= mav_callbackExec(mav_callback_intersect, w, o, (void *) &ln, (void *) oint);
  }
  else if (mav_callbackQuery(mav_callback_BB, w, o))
  {
    if (mav_callbackBBExec(w, o, &bb)) rv= mav_BBIntersectsLine(bb, ln, oint);
  }

  return rv;
}



/* Support routines for intersection */

/* Routine to translate a line defined in one coordinate frame into another */

MAV_line mav_lineTransFrame(MAV_line lin, MAV_matrix mat)
{
  MAV_line rv;
  MAV_matrix inv;
  MAV_vector pt2;
  float scale;

/* Calculate the unit scaled matrix of mat */
  scale= mav_matrixScaleGet(mat);
  mat= mav_matrixScaleSet(mat, 1.0/scale);

/* Invert the rotation part of the matrix - transpose rows and columns */
  inv.mat[0][0] = mat.mat[0][0];
  inv.mat[0][1] = mat.mat[1][0];
  inv.mat[0][2] = mat.mat[2][0];
  inv.mat[0][3] = 0.0;

  inv.mat[1][0] = mat.mat[0][1];
  inv.mat[1][1] = mat.mat[1][1];
  inv.mat[1][2] = mat.mat[2][1];
  inv.mat[1][3] = 0.0;

  inv.mat[2][0] = mat.mat[0][2];
  inv.mat[2][1] = mat.mat[1][2];
  inv.mat[2][2] = mat.mat[2][2];
  inv.mat[2][3] = 0.0;

  inv.mat[3][0] = 0.0;
  inv.mat[3][1] = 0.0;
  inv.mat[3][2] = 0.0;
  inv.mat[3][3] = 1.0;

/* Translate point then rotate by inverse matrix and account for scale */
  pt2.x = lin.pt.x - mat.mat[MAV_MATRIX_XCOMP];
  pt2.y = lin.pt.y - mat.mat[MAV_MATRIX_YCOMP];
  pt2.z = lin.pt.z - mat.mat[MAV_MATRIX_ZCOMP];

  rv.pt= mav_vectorScalar(mav_vectorMult(pt2, inv), 1.0/scale);
  
/* Rotate line direction by inverse matrix */
  rv.dir= mav_vectorMult(lin.dir, inv);

  return rv;
}



/* Routine to intersect a line with a finite axis aligned plane */

int mav_lineAxisPlaneIntersection(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax, MAV_vector pt, MAV_vector norm, MAV_line ln, MAV_objectIntersection *o)
{
  int rv=0;

/* Calculate intersection of line with infinite plane */ 
  rv = mav_lineInfPlaneIntersection(pt, norm, ln, o);

/* If return value is not true, intersection fails - return the return value */ 
  if (!rv) return (rv);

/* Check intersection point is within the finite plane */
  if (o->intpt.x >= xmin && o->intpt.x <=xmax && 
      o->intpt.y >= ymin && o->intpt.y <=ymax && 
      o->intpt.z >= zmin && o->intpt.z <=zmax)
  {
    return(rv);
  }
  else
  {
    return(MAV_FALSE);
  }
}



/* Routine to intersect a line with a infinte axis aligned plane */

int mav_lineInfPlaneIntersection(MAV_vector pt, MAV_vector norm, MAV_line ln, MAV_objectIntersection *o)
{
  float dn, t;

/* initialise return value */
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

/* Check the distance is not negative i.e. the intersection point is behind us */
  if (t < 0.0) return (MAV_FALSE);

/* Calculate the point of intersection */
  o->intpt = mav_vectorAdd(ln.pt, mav_vectorScalar(ln.dir, t));
  o->pt1= t;
  o->pt2= t;

  return(MAV_TRUE);
}



/* Routine to sort n intersections into the 2 unique closest */

int mav_objectIntersectionsSort(int nhits, MAV_objectIntersection *ints, float sc, MAV_objectIntersection *res)
{
  int i, flag;
  MAV_objectIntersection closestInt, secInt;

  if (nhits==0) return(MAV_FALSE);

/* Find the closest intersections */

  flag=0;
  closestInt.pt1= MAV_INFINITY;
  
  for (i=0; i<nhits; i++) {
    if (ints[i].pt1>MAV_EPSILON && ints[i].pt1<closestInt.pt1) {
      closestInt=ints[i];
      flag=1;
    }
  }

  if (!flag) return 0;

/* Find the next closest intersection */

  flag=0;
  secInt.pt1= MAV_INFINITY;

  for (i=0; i<nhits; i++) {
    if (ints[i].pt1>MAV_EPSILON && (ints[i].pt1-closestInt.pt1)>MAV_EPSILON && ints[i].pt1<secInt.pt1) {
      secInt=ints[i];
      flag=1;
    }
  }

  if (flag) 
  {

/* More than one unique intersection */

    res->pt1=closestInt.pt1;
    res->pt2=secInt.pt1;
  }
  else
  {

/* Only one unique intersection - inside body */

    res->pt1=0.0;
    res->pt2=closestInt.pt1;
  }

  res->pt1*=sc;
  res->pt2*=sc;

  return(MAV_TRUE);
}



/* Routine to dump an object intersection data structure */

void mav_objectIntersectionPrint(char *s, MAV_objectIntersection oi)
{
  printf("%s", s);
  printf("pt1 %f pt2 %f\n", oi.pt1, oi.pt2);
}



/* Routine to dump an line data structure */

void mav_linePrint(char *s, MAV_line ln)
{
  printf("%s", s);
  mav_vectorPrint("pt  ", ln.pt);
  mav_vectorPrint("dir ", ln.dir);
}
