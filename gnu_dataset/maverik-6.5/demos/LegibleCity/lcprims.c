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


#include "lcity.h"
#include <math.h>



int mav_characterDraw(MAV_object *obj, MAV_drawInfo *di)
{
  Character *c= (Character *) mav_objectDataGet(obj);
  MAV_vector dr;
  float dist;
  int i,j;

  /* Calculate distance from center of character to eye point */
  dr= mav_vectorSub(c->centre, mav_win_current->vp->eye);
  dist= sqrt(mav_vectorDotProduct(dr, dr));

  /* Apply level of detail based on this */
  if (apply_lod) 
  {
    if (dist < fog_distance)
    {
      /* Use correct colouring and colour */
      mav_surfaceParamsUse(c->sp);

      /* Store view matrix then apply transformation */
      mav_gfxMatrixPush();
      mav_gfxMatrixMult(c->matrix);

      if (dist < box_distance)
      {
	/* Draw as fully blown character */
	for (i=0; i<c->character->num_polys; i++) {
	  mav_gfxPolygonBegin();
	  mav_gfxNormal(c->character->polys[i].normal);
	  for (j=0; j<c->character->polys[i].num_verts; j++) mav_gfxVertex(c->character->polys[i].verts[j]);
	  mav_gfxPolygonEnd();
	}
      }
      else
      {
	/* Draw its bounding box */
	mav_BBDisplayWithSurfaceParams(mav_win_current, c->character->bb, c->sp);
      }

      /* Restore the matrix */
      mav_gfxMatrixPop();
    }
  }
  else
  {
    /* Use correct colouring and colour */
    mav_surfaceParamsUse(c->sp);

    /* Store view matrix then apply transformation */
    mav_gfxMatrixPush();
    mav_gfxMatrixMult(c->matrix);

    /* Draw as fully blown character */
    for (i=0; i<c->character->num_polys; i++) {
      mav_gfxPolygonBegin();
      mav_gfxNormal(c->character->polys[i].normal);
      for (j=0; j<c->character->polys[i].num_verts; j++) mav_gfxVertex(c->character->polys[i].verts[j]);
      mav_gfxPolygonEnd();
    }

    /* Restore the matrix */
    mav_gfxMatrixPop();
  }

  return 1;
}

int mav_characterBBox(MAV_object *obj, MAV_BB *bb)
{
  Character *c= (Character *) mav_objectDataGet(obj);

  /* Align the stored BB to account for transformation */
  mav_BBAlign(c->character->bb, c->matrix, bb);

  return 1;
}



/* Features */

int mav_featureDraw(MAV_object *obj, MAV_drawInfo *di)
{
  Feature *f= (Feature *) mav_objectDataGet(obj);
  int i,j;

  /* Use the correct colouring and colour */
  mav_surfaceParamsUse(f->sp);

  /* Store view matrix then apply transformation */
  mav_gfxMatrixPush();
  mav_gfxMatrixMult(f->matrix);

  /* Draw the polygons */
  for (i=0; i<f->num_polys; i++) {
    mav_gfxPolygonBegin();
    mav_gfxNormal(f->polys[i].normal);
    for (j=0; j<f->polys[i].num_verts; j++) mav_gfxVertex(f->polys[i].verts[j]);
    mav_gfxPolygonEnd();
  }

  /* Restore the matrix */
  mav_gfxMatrixPop();

  return 1;
}

int mav_featureBBox(MAV_object *obj, MAV_BB *bb)
{
  Feature *f= (Feature *) mav_objectDataGet(obj);

  /* Align the stored BB to acount for transformation */
  mav_BBAlign(f->bb, f->matrix, bb);

  return 1;
}



/* Vectors */

int mav_vectorDraw(MAV_object *obj, MAV_drawInfo *di)
{
  Vector *v= (Vector *) mav_objectDataGet(obj);

  /* Use the correct colour */
  mav_surfaceParamsUse(v->sp);

  /* Store view matrix then apply transformation */
  mav_gfxMatrixPush();
  mav_gfxMatrixMult(v->matrix);

  /* Draw the vector */
  mav_gfxLineBegin();
  mav_gfxVertex(v->start);
  mav_gfxVertex(v->end);
  mav_gfxLineEnd();

  /* Restore the matrix */
  mav_gfxMatrixPop();
  
  return 1;
}

int mav_vectorBBox(MAV_object *obj, MAV_BB *bb)
{
  Vector *v= (Vector *) mav_objectDataGet(obj);

  /* Align the stored BB to account for transformation */
  mav_BBAlign(v->bb, v->matrix, bb);

  return 1;
}
