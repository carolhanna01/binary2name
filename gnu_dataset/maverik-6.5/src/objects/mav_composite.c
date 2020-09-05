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
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>

MAV_class *mav_class_composite;



/* Routine to render a composite */

int mav_compositeDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_composite *comp= (MAV_composite *) mav_objectDataGet(obj);
  MAV_drawInfo di2, *di3= NULL;
  int i;

/* Store the current transformation matrix - then multiply it by the local transformation */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(comp->matrix);

/* Transform the drawinfo */

  if (di) {
    di2= mav_drawInfoTransFrame(*di, comp->matrix);
    di3= &di2;
  }

/* Draw each object */ 

  for (i=0; i<comp->numobj; i++) mav_callbackDrawExec(mav_win_current, comp->obj[i], di3);
  
/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;
}



/* Routine to calculate the bounding box of a composite */

int mav_compositeBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_composite *comp=(MAV_composite *) mav_objectDataGet(obj);
  
/* Align local BB with global coords */

  mav_BBAlign(comp->bb, comp->matrix, bb);

  return 1;
}



/* Routine to store the local BB of a composite */

int mav_compositeCalcBB(MAV_composite *comp)
{
  MAV_BB bb;
  int i;

  if (comp->numobj > 0) 
  {

/* Initialise the composite BB */

    mav_BBCompInit(&comp->bb);

/* For each object in composite calc its BB and find the composite of these */

    for (i=0; i<comp->numobj; i++) {
      mav_callbackBBExec(mav_win_current, comp->obj[i], &bb);
      mav_BBCompBB(bb, &comp->bb); /* Find the composite of all these BB's */
    }
    return 1;
  }
  else
  {
    return 0;
  }
}



/* Routine to intersect a composite */

int mav_compositeIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *o)
{
  MAV_composite *comp=(MAV_composite *) mav_objectDataGet(obj);
  MAV_line ln2; 
  MAV_objectIntersection o2, smallest;
  int i, flag;

  o->pt1=-100.0;
  o->pt2=-100.0;
  smallest.pt1=MAV_INFINITY;
  comp->selobj=-1;
  flag=0;

/* 
   Use inverse of composites transformation to transform the start position
   and direction of the intersection ray (originally in global coords) into 
   the composites local coords.
*/

  ln2= mav_lineTransFrame(*ln, comp->matrix);

/* Find closest intersection */

  for (i=0; i<comp->numobj; i++) {
    if (mav_callbackIntersectExec(mav_win_current, comp->obj[i], ln2, &o2)) {
      flag=1;
      if (o2.pt1<smallest.pt1) {
	smallest=o2;
        comp->selobj=i;
      }
    }
  }

  if (!flag) return (MAV_FALSE);

/* Account for scale */ 

  *o=smallest;
  o->pt1*=mav_matrixScaleGet(comp->matrix);
  o->pt2*=mav_matrixScaleGet(comp->matrix);

  return(1);
}



/* Routine to identify a composite */

int mav_compositeID(MAV_object *o, char **id)
{
  *id= "composite";

  return 1;
}



/* Routine to return the userdef field of a composite */

int mav_compositeGetUserdef(MAV_object *obj, void ***ud)
{
  MAV_composite *comp = (MAV_composite *) mav_objectDataGet(obj);

  *ud= &comp->userdef;

  return 1;
}



/* Routine to return the matrix field of a composite */

int mav_compositeGetMatrix(MAV_object *obj, MAV_matrix **mat)
{
  MAV_composite *comp = (MAV_composite *) mav_objectDataGet(obj);

  *mat= &comp->matrix;

  return 1;
}



/* Routine to return the surface params field of a composite */

int mav_compositeGetSurfaceParams(MAV_object *obj, MAV_surfaceParams ***sp)
{
  MAV_composite *comp = (MAV_composite *) mav_objectDataGet(obj);

  if (comp->numobj)
  {
    return mav_callbackGetSurfaceParamsExec(mav_win_current, comp->obj[0], sp);
  }
  else
  {
    return 0;
  }
}



/* Routine to dump a composite */

int mav_compositeDump(MAV_object *obj)
{
  MAV_composite *comp = (MAV_composite *) mav_objectDataGet(obj);
  char *s;
  int i;

  printf("*** Dumping object %p - a MAV_composite with data pointer %p\n", obj, mav_objectDataGet(obj));
  printf("numobj %i\n", comp->numobj);
  for (i=0; i<comp->numobj; i++) {
    mav_callbackIDExec(mav_win_current, comp->obj[i], &s);
    printf("obj[%i] is a %s\n", i, s);
    mav_callbackDumpExec(mav_win_current, comp->obj[i]);
  }
  
  mav_matrixPrint("matrix\n", comp->matrix);

  return 1;
}



/* Routine to empty a composite */

void mav_compositeEmpty(MAV_composite *c)
{
  int i,j;

  for (i=0; i<c->numobj; i++) {
    if (mav_objectClassGet(c->obj[i])==mav_class_facet) 
    {
      MAV_facet *f= (MAV_facet *) mav_objectDataGet(c->obj[i]);

      for (j=0; j<f->npolys; j++) {
	mav_free(f->norm[j]);
	mav_free(f->tex[j]);
	mav_free(f->vert[j]);
	mav_free(f->sp[j]);
      }

      mav_free(f->np);
      mav_free(f->norm);
      mav_free(f->tex);
      mav_free(f->vert);
      mav_free(f->sp);
      mav_free(f);
    }
    else if (mav_objectClassGet(c->obj[i])==mav_class_polyline) 
    {
      MAV_polyline *pl= (MAV_polyline *) mav_objectDataGet(c->obj[i]);

      for (j=0; j<pl->nlines; j++) {
	mav_free(pl->vert[j]);
	mav_free(pl->sp[j]);
      }
      mav_free(pl->np);
      mav_free(pl->closed);
      mav_free(pl->vert);
      mav_free(pl->sp);
      mav_free(pl);
    }
    else 
    {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: class not recognised in compositeEmpty, ignoring\n");
    }

    mav_free(c->obj[i]);
  }

  free(c->filename);
  mav_free(c->obj);
}



/* Not everyone has strcasecmp! */

int mavlib_strcasecmp(const char *s1, const char *s2)
{
  int i;

  if (strlen(s1)!=strlen(s2)) return 1;
  
  for (i=0; i<strlen(s1); i++) {
    if (tolower(s1[i])!=tolower(s2[i])) return 1;
  }

  return 0;
}



/* Routine to define a composite from file */

int mav_compositeRead(char *filename, MAV_composite *c, MAV_matrix m)
{
  int rv= MAV_FALSE, i, fd; 
  char *ext;

  /* Find filename extension */
  for (i=strlen(filename)-1; i>=0 && filename[i]!='.'; i--);
  
  if (i==-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: can not find file extension for %s, ignoring\n", filename);
  }
  else
  {
    ext= &filename[i];
    
    fd=0;
    for (i=0; i<MAV_MAX_COMPOSITE_FORMATS; i++) {
      if (mav_compositeFormat[i].defined && !mavlib_strcasecmp(mav_compositeFormat[i].ext, ext)) {
	fd=1;
	rv= (*mav_compositeFormat[i].fn)(filename, c, m);
      }
    }
    
    if (fd==0 && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: file format %s not supported, ignoring\n", ext);
  }

  return rv; 
}

