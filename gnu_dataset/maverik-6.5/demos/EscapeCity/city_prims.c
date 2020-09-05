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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
#include "maverik.h"
#include "city_macros.h"
#include "city_types.h"

extern MAV_class *mav_class_citycell;
extern MAV_list *list_of_objects;
extern MAV_SMS *sms;
extern unsigned int current_frame;
extern int size;
extern int bb_pos;
extern BB bb_array[MAX_BILLBOARDS];
extern int num_visible_objects;
extern Visible_Object *visible_objects;
extern int culling;
extern float object_dist;
extern MAV_vector object_pos;

#ifdef DISPLAY_LISTS
int num_display_lists= 1;
int background_display_list;
#endif
float max_composite_dist= 65.0;
int current_texture= -1;
int num_rendered_cells;
int num_rendered_polygons;
int wire= 0;

#ifdef NEVER
void
mav_getXYZInt (MAV_vector pt, MAV_vector *int_pt, MAV_vector norm, double soln)
{
  if (norm.x != 0.0)
    int_pt->x= (soln - pt.y*norm.y - pt.z*norm.z)/norm.x;
  if (norm.y != 0.0)
    int_pt->y= (soln - pt.x*norm.x - pt.z*norm.z)/norm.y;
  if (norm.z != 0.0)
    int_pt->z= (soln - pt.x*norm.x - pt.y*norm.y)/norm.z;
}

int
mav_BBIntersectsFrustum (MAV_BB bb, int *corner_list, float *soln_list, MAV_vector *norm_list)
{
  int result= 1;
  int n;
  float res_1=0, res_2=0;
  MAV_vector pos_corner, neg_corner, xyz_int;

  /* n is current plane */
  n= 0;

  while ((n < 6) && result)
    {
      /* check each plane in turn */
      /* only need to check 2 corners per plane */
      /* corner_list gives the corners that need checking */
      switch (corner_list[n])
	{
	case 0:
	  res_1= mav_vectorDotProduct (bb.min, norm_list[n]);
	  res_2= mav_vectorDotProduct (bb.max, norm_list[n]);
	  break;
	case 1:
	  res_1= bb.min.x * norm_list[n].x + bb.min.y * norm_list[n].y + bb.max.z * norm_list[n].z;
	  res_2= bb.max.x * norm_list[n].x + bb.max.y * norm_list[n].y + bb.min.z * norm_list[n].z;
	  break;
	case 2:
	  res_1= bb.min.x * norm_list[n].x + bb.max.y * norm_list[n].y + bb.max.z * norm_list[n].z;
	  res_2= bb.max.x * norm_list[n].x + bb.min.y * norm_list[n].y + bb.min.z * norm_list[n].z;
	  break;
	case 3:
	  res_1= bb.min.x * norm_list[n].x + bb.max.y * norm_list[n].y + bb.min.z * norm_list[n].z;
	  res_2= bb.max.x * norm_list[n].x + bb.min.y * norm_list[n].y + bb.max.z * norm_list[n].z;
	}

      /* check for total enclosure */
      if ((res_1 > soln_list[n]) && (res_2 > soln_list[n]))
	/* no intersection */
	result= 0;
      else if ((res_1 > soln_list[n]) || (res_2 > soln_list[n]))
	{
	  /* partial enclosure */
	  result= 2;
	  /* now we need to clip the bb before continuing */
	  if (res_1 <= soln_list[n])
	    {
	      /* corner 1 is on the +ve side of the plane */
	      switch (corner_list[n])
		{
		case 0:
		  /* bb.min */
		  pos_corner= bb.min;
		  neg_corner= bb.max;
		  xyz_int= neg_corner;
		  mav_getXYZInt (pos_corner, &xyz_int, norm_list[n], soln_list[n]);
		  if (xyz_int.x < neg_corner.x)
		    bb.max.x= xyz_int.x;
		  if (xyz_int.y < neg_corner.y)
		    bb.max.y= xyz_int.y;
		  if (xyz_int.z < neg_corner.z)
		    bb.max.z= xyz_int.z;
		  break;
		case 1:
		  /* bb.min.x, bb.min.y, bb.max.z */
		  pos_corner= bb.min;
		  pos_corner.z= bb.max.z;
		  neg_corner= bb.max;
		  neg_corner.z= bb.min.z;
		  xyz_int= neg_corner;
		  mav_getXYZInt (pos_corner, &xyz_int, norm_list[n], soln_list[n]);
		  if (xyz_int.x < neg_corner.x)
		    bb.max.x= xyz_int.x;
		  if (xyz_int.y < neg_corner.y)
		    bb.max.y= xyz_int.y;
		  if (xyz_int.z > neg_corner.z)
		    bb.min.z= xyz_int.z;
		  break;
		case 2:
		  /* bb.min.x, bb.max.y, bb.max.z */
		  pos_corner= bb.max;
		  pos_corner.x= bb.min.x;
		  neg_corner= bb.min;
		  neg_corner.x= bb.max.x;
		  xyz_int= neg_corner;
		  mav_getXYZInt (pos_corner, &xyz_int, norm_list[n], soln_list[n]);
		  if (xyz_int.x < neg_corner.x)
		    bb.max.x= xyz_int.x;
		  if (xyz_int.y > neg_corner.y)
		    bb.min.y= xyz_int.y;
		  if (xyz_int.z > neg_corner.z)
		    bb.min.z= xyz_int.z;
		  break;
		case 3:
		  /* bb.min.x, bb.max.y, bb.min.z */
		  pos_corner= bb.min;
		  pos_corner.y= bb.max.y;
		  neg_corner= bb.max;
		  neg_corner.y= bb.min.y;
		  xyz_int= neg_corner;
		  mav_getXYZInt (pos_corner, &xyz_int, norm_list[n], soln_list[n]);
		  if (xyz_int.x < neg_corner.x)
		    bb.max.x= xyz_int.x;
		  if (xyz_int.y > neg_corner.y)
		    bb.min.y= xyz_int.y;
		  if (xyz_int.z < neg_corner.z)
		    bb.max.z= xyz_int.z;
		}
	    }
	  else
	    {
	      /* corner 2 is on the +ve side */
	      switch (corner_list[n])
		{
		case 0:
		  /* bb.max */
		  pos_corner= bb.max;
		  neg_corner= bb.min;
		  xyz_int= neg_corner;
		  mav_getXYZInt (pos_corner, &xyz_int, norm_list[n], soln_list[n]);
		  if (xyz_int.x > neg_corner.x)
		    bb.min.x= xyz_int.x;
		  if (xyz_int.y > neg_corner.y)
		    bb.min.y= xyz_int.y;
		  if (xyz_int.z > neg_corner.z)
		    bb.min.z= xyz_int.z;
		  break;
		case 1:
		  /* bb.max.x, bb.max.y, bb.min.z */
		  pos_corner= bb.max;
		  pos_corner.z= bb.min.z;
		  neg_corner= bb.min;
		  neg_corner.z= bb.max.z;
		  xyz_int= neg_corner;
		  mav_getXYZInt (pos_corner, &xyz_int, norm_list[n], soln_list[n]);
		  if (xyz_int.x > neg_corner.x)
		    bb.min.x= xyz_int.x;
		  if (xyz_int.y > neg_corner.y)
		    bb.min.y= xyz_int.y;
		  if (xyz_int.z < neg_corner.z)
		    bb.max.z= xyz_int.z;
		  break;
		case 2:
		  /* bb.max.x, bb.min.y, bb.min.z */
		  pos_corner= bb.min;
		  pos_corner.x= bb.max.x;
		  neg_corner= bb.max;
		  neg_corner.x= bb.min.x;
		  xyz_int= neg_corner;
		  mav_getXYZInt (pos_corner, &xyz_int, norm_list[n], soln_list[n]);
		  if (xyz_int.x > neg_corner.x)
		    bb.min.x= xyz_int.x;
		  if (xyz_int.y < neg_corner.y)
		    bb.max.y= xyz_int.y;
		  if (xyz_int.z < neg_corner.z)
		    bb.max.z= xyz_int.z;
		  break;
		case 3:
		  /* bb.max.x, bb.min.y, bb.max.z */
		  pos_corner= bb.max;
		  pos_corner.y= bb.min.y;
		  neg_corner= bb.min;
		  neg_corner.y= bb.max.y;
		  xyz_int= neg_corner;
		  mav_getXYZInt (pos_corner, &xyz_int, norm_list[n], soln_list[n]);
		  if (xyz_int.x > neg_corner.x)
		    bb.min.x= xyz_int.x;
		  if (xyz_int.y < neg_corner.y)
		    bb.max.y= xyz_int.y;
		  if (xyz_int.z > neg_corner.z)
		    bb.min.z= xyz_int.z;
		}
	    }
	}
      
      n ++;
    }
  
  /* if result is still 1 at this point then the frustum competely */
  /* encloses the bb */
  return (result);
}
#endif

#ifdef NEVER
void
Render_Frustum (void)
{
  MAV_surfaceParams sp;
  MAV_vector from;
  MAV_vector left,right;
  MAV_vector vr;

  sp.mode= MAV_COLOUR;
  sp.colour= MAV_COLOUR_YELLOW;

  from= mav_win_left->vp->eye;
  vr= mav_vectorCrossProduct (mav_win_left->vp->trans_view,mav_win_left->vp->trans_up);
  vr= mav_vectorNormalize(vr);

  left.x= from.x+mav_win_left->vp->view.x*85.49118708+vr.x*51.87732582;
  left.y= from.y;
  left.z= from.z+mav_win_left->vp->view.z*85.49118708+vr.z*51.87732582;
  right.x= from.x+mav_win_left->vp->view.x*85.49118708-vr.x*51.87732582;
  right.y= from.y;
  right.z= from.z+mav_win_left->vp->view.z*85.49118708-vr.z*51.87732582;

  mav_surfaceParamsUse (sp);
  mav_gfxLineWidthSet (3);
  glBegin (GL_LINE_STRIP);
  mav_gfxVertex (left);
  mav_gfxVertex (from);
  mav_gfxVertex (right);
  glEnd ();
}
#endif

void
Render_Background
(float radius)
{
  float theta0, phi0;
  float theta1, phi1;
  float t0,t1;
  int divn= 3;
  int i,j;
  MAV_vector cent;
  float rphi0, rphi1;
  MAV_vector p;
  MAV_texCoord t;
  MAV_surfaceParams sp;

#ifdef DISPLAY_LISTS
#ifdef MAV_GL
  makeobj (num_display_lists);
#else
  glNewList (num_display_lists, GL_COMPILE);
#endif
  background_display_list= num_display_lists;
  num_display_lists++;
#endif

#ifdef TEXTURES
  sp.mode= MAV_TEXTURE;
  sp.texture= TEX_GRASS;
#else
  sp.mode= MAV_MATERIAL;
  sp.material= MAT_GRASS;
#endif
  mav_surfaceParamsUse (&sp);

  cent.x= 0.5*size*BLOCK_WIDTH;
  cent.y= -0.1;
  cent.z= 0.5*size*BLOCK_WIDTH;

#ifndef MAV_GL
  glBegin (GL_TRIANGLES);
#endif

  for (i= 0; i< 4*divn; i++)
    {
      theta0= 0.25*i/(float)divn;
      theta1= 0.25*(i+1)/(float)divn;

      p.x= 0.0; p.y= 1.0; p.z= 0.0; mav_gfxNormal (p);
#ifdef MAV_GL
      mav_gfxPolygonBegin ();
#endif

      mav_gfxVertex (cent);

      p.x= cent.x+radius*sin(2.0*MAV_PI*theta0);
      p.y= cent.y;
      p.z= cent.z+radius*cos(2.0*MAV_PI*theta0);
      mav_gfxVertex (p);

      p.x= cent.x+radius*sin(2.0*MAV_PI*theta1);
      p.z= cent.z+radius*cos(2.0*MAV_PI*theta1);
      mav_gfxVertex (p);

#ifdef MAV_GL
      mav_gfxPolygonEnd ();
#endif
    }

#ifndef MAV_GL
  glEnd ();
#endif

  glDisable (GL_FOG);
  sp.mode= MAV_TEXTURE;
  sp.texture= TEX_SKY;
  mav_surfaceParamsUse (&sp);

#ifndef MAV_GL
  glBegin (GL_TRIANGLES);
#endif

  /* draw sky hemisphere */
  for (j= 0; j< divn; j++)
    {
      phi0= j/(float)divn;
      phi1= (j+1)/(float)divn;

      rphi0= radius*sin(MAV_PI_OVER_2*phi0);
      rphi1= radius*sin(MAV_PI_OVER_2*phi1);

      for (i= 0; i< 4*divn; i++)
	{
	  theta0= 0.25*i/(float)divn;
	  theta1= 0.25*(i+1)/(float)divn;

	  t0= 0.01+0.99*fabs(0.99-1.98*theta0);
	  t1= 0.01+0.99*fabs(0.99-1.98*theta1);

	  if (j == 0)
	    {
#ifdef MAV_GL
	      mav_gfxPolygonBegin ();
#endif
	      t.s= 0.5;
	      t.t= 0.99-0.98*phi0;
	      mav_gfxTexCoord (t);
	      p.x= cent.x;
	      p.y= cent.y+radius;
	      p.z= cent.z;
	      mav_gfxVertex (p);

	      t.s= t1;
	      t.t= 0.99-0.98*phi1;
	      mav_gfxTexCoord (t);
	      p.x= cent.x+rphi1*sin(2.0*MAV_PI*theta1);
	      p.y= cent.y+radius*cos(MAV_PI_OVER_2*phi1);
	      p.z= cent.z+rphi1*cos(2.0*MAV_PI*theta1);
	      mav_gfxVertex (p);

	      t.s= t0;
	      mav_gfxTexCoord (t);
	      p.x= cent.x+rphi1*sin(2.0*MAV_PI*theta0);
	      p.z= cent.z+rphi1*cos(2.0*MAV_PI*theta0);
	      mav_gfxVertex (p);

#ifdef MAV_GL
	      mav_gfxPolygonEnd ();
#endif
	    }
	  else
	    {
#ifdef MAV_GL
	      mav_gfxPolygonBegin ();
#endif
	      t.s= t1;
	      t.t= 0.99-0.98*phi0;
	      mav_gfxTexCoord (t);
	      p.x= cent.x+rphi0*sin(2.0*MAV_PI*theta1);
	      p.y= cent.y+radius*cos(MAV_PI_OVER_2*phi0);
	      p.z= cent.z+rphi0*cos(2.0*MAV_PI*theta1);
	      mav_gfxVertex (p);

	      t.s= t1;
	      t.t= 0.99-0.98*phi1;
	      mav_gfxTexCoord (t);
	      p.x= cent.x+rphi1*sin(2.0*MAV_PI*theta1);
	      p.y= cent.y+radius*cos(MAV_PI_OVER_2*phi1);
	      p.z= cent.z+rphi1*cos(2.0*MAV_PI*theta1);
	      mav_gfxVertex (p);

	      t.s= t0;
	      mav_gfxTexCoord (t);
	      p.x= cent.x+rphi1*sin(2.0*MAV_PI*theta0);
	      p.z= cent.z+rphi1*cos(2.0*MAV_PI*theta0);
	      mav_gfxVertex (p);

#ifdef MAV_GL
	      mav_gfxPolygonEnd ();
#endif

#ifdef MAV_GL
	      mav_gfxPolygonBegin ();
#endif
	      t.s= t1;
	      t.t= 0.99-0.98*phi0;
	      mav_gfxTexCoord (t);
	      p.x= cent.x+rphi0*sin(2.0*MAV_PI*theta1);
	      p.y= cent.y+radius*cos(MAV_PI_OVER_2*phi0);
	      p.z= cent.z+rphi0*cos(2.0*MAV_PI*theta1);
	      mav_gfxVertex (p);

	      t.s= t0;
	      t.t= 0.99-0.98*phi1;
	      mav_gfxTexCoord (t);
	      p.x= cent.x+rphi1*sin(2.0*MAV_PI*theta0);
	      p.y= cent.y+radius*cos(MAV_PI_OVER_2*phi1);
	      p.z= cent.z+rphi1*cos(2.0*MAV_PI*theta0);
	      mav_gfxVertex (p);

	      t.t= 0.99-0.98*phi0;
	      mav_gfxTexCoord (t);
	      p.x= cent.x+rphi0*sin(2.0*MAV_PI*theta0);
	      p.y= cent.y+radius*cos(MAV_PI_OVER_2*phi0);
	      p.z= cent.z+rphi0*cos(2.0*MAV_PI*theta0);
	      mav_gfxVertex (p);
#ifdef MAV_GL
	      mav_gfxPolygonEnd ();
#endif
 	    }
	}
    }

#ifndef MAV_GL
  glEnd();
#endif
  glEnable (GL_FOG);

#ifdef DISPLAY_LISTS
#ifdef MAV_GL
  closeobj ();
#else
  glEndList();
#endif
#endif
}

int
mav_objectEnlist
(MAV_object *obj, void *v1, void *v2)
{
  /* add occluder to list of those inside frustum */
  visible_objects[num_visible_objects].obj= obj;
  num_visible_objects++;
  return 1;
}

#ifdef COMPOSITES
int
mav_compositeDrawWire
(MAV_object *obj, MAV_drawInfo *di)
{
  if (wire)
    {
      MAV_composite *comp= (MAV_composite *)obj->the_data;

      mav_gfxMatrixPush ();
      mav_gfxMatrixMult (comp->matrix);

      mav_BBDisplayWithColour (mav_win_current, comp->bb, MAV_COLOUR_YELLOW);

      mav_gfxMatrixPop ();
    }
  else
    mav_compositeDraw (obj, NULL);

  return 1;
}
#endif

int
mav_cityCellGetMatrix
(MAV_object *obj, MAV_matrix **mat)
{
  *mat= &(((MAV_cityCell *)obj->the_data)->matrix);
  return 1;
}

int
mav_cityCellGetUserdef
(MAV_object *obj, void ***ud)
{
  *ud= &(((MAV_cityCell *)obj->the_data)->userdef);
  return 1;
}

int
mav_cityCellBBox
(MAV_object *obj, MAV_BB *bb)
{
  MAV_cityCell *s= (MAV_cityCell *) obj->the_data;
  mav_BBAlign (s->bb, s->matrix, bb);
  return 1;
}

int
mav_cityCellDraw
(MAV_object *cobj, MAV_drawInfo *di)
{
  MAV_cityCell *c= (MAV_cityCell *)cobj->the_data;
  Poly *p;
  MAV_object *obj;
  int begin;

  mav_gfxMatrixPush ();
  mav_gfxMatrixMult (c->matrix);

  if (wire)
    {
      mav_gfxLineWidthSet (1);
      mav_BBDisplayWithColour (mav_win_current, c->bb, MAV_COLOUR_WHITE);
    }
  else
    {
#ifdef DISPLAY_LISTS
      if (c->lod_polys && object_dist > 2.0*max_composite_dist)
#ifdef MAV_GL
	callobj (c->lod_display_list);
#else
	glCallList (c->lod_display_list);
#endif
      else
#ifdef MAV_GL
	callobj (c->cell_display_list);
#else
	glCallList (c->cell_display_list);
#endif
#else
      if (c->lod_polys && object_dist > 2.0*max_composite_dist)
	p= c->lod_polys;
      else
	p= c->polys;

      begin= 1;
      while (p)
	{
	  if (begin)
	    {
	      mav_surfaceParamsUse (&p->sp);
	      begin= 0;
#ifndef MAV_GL
	      glBegin (GL_QUADS);
#endif
	    }

	  if (p->sp.texture > -1)
	    {
#ifdef MAV_GL
	      mav_gfxPolygonBegin ();
	      mav_gfxNormal (p->normal);
	      mav_gfxTexCoord (p->texcoords[0]);
	      mav_gfxVertex (p->verts[0]);
	      mav_gfxTexCoord (p->texcoords[1]);
	      mav_gfxVertex (p->verts[1]);
	      mav_gfxTexCoord (p->texcoords[2]);
	      mav_gfxVertex (p->verts[2]);
	      mav_gfxTexCoord (p->texcoords[3]);
	      mav_gfxVertex (p->verts[3]);
	      mav_gfxPolygonEnd ();
#else
	      glNormal3fv ((GLfloat*)&p->normal);
	      glTexCoord2fv ((GLfloat*)&p->texcoords[0]);
	      glVertex3fv ((GLfloat*)&p->verts[0]);
	      glTexCoord2fv ((GLfloat*)&p->texcoords[1]);
	      glVertex3fv ((GLfloat*)&p->verts[1]);
	      glTexCoord2fv ((GLfloat*)&p->texcoords[2]);
	      glVertex3fv ((GLfloat*)&p->verts[2]);
	      glTexCoord2fv ((GLfloat*)&p->texcoords[3]);
	      glVertex3fv ((GLfloat*)&p->verts[3]);
#endif
	    }
	  else
	    {
#ifdef MAV_GL
	      mav_gfxPolygonBegin ();
	      mav_gfxNormal (p->normal);
	      mav_gfxVertex (p->verts[0]);
	      mav_gfxVertex (p->verts[1]);
	      mav_gfxVertex (p->verts[2]);
	      mav_gfxVertex (p->verts[3]);
	      mav_gfxPolygonEnd ();
#else
	      glNormal3fv ((GLfloat*)&p->normal);
	      glVertex3fv ((GLfloat*)&p->verts[0]);
	      glVertex3fv ((GLfloat*)&p->verts[1]);
	      glVertex3fv ((GLfloat*)&p->verts[2]);
	      glVertex3fv ((GLfloat*)&p->verts[3]);
#endif
	    }
	  
#ifndef MAV_GL
	  if (p->drawn)
	    {
	      glEnd();
	      begin= 1;
	    }
#endif
	  p= p->next;
	}
#endif
    }
#ifndef MAV_GL
  glEnd();
#endif

  mav_gfxMatrixPop ();

#ifdef COMPOSITES
#ifdef DISPLAY_LISTS
  if (wire)
    {
      mav_listPointerReset (c->features);
      while (mav_listItemNext (c->features, (void **)&obj))
	mav_callbackDrawExec (mav_win_current, obj, NULL);
    }
  else
    {
      if (c->feature_display_list > 0)
	{
#ifdef MAV_GL
	  callobj (c->feature_display_list);
#else
	  glCallList (c->feature_display_list);
#endif
	}
    }
#else
  if (c->features)
    {
      mav_listPointerReset (c->features);
      while (mav_listItemNext (c->features, (void **)&obj))
	mav_callbackDrawExec (mav_win_current, obj, NULL);
    }
#endif

  if (object_dist < max_composite_dist)
    {
#ifdef DISPLAY_LISTS
      if (wire)
	{
	  mav_listPointerReset (c->composites);
	  while (mav_listItemNext (c->composites, (void **)&obj))
	    mav_callbackDrawExec (mav_win_current, obj, NULL);
	}
      else
	{
	  if (c->composite_display_list > 0)
	    {
#ifdef MAV_GL
	      callobj (c->composite_display_list);
#else
	      glCallList (c->composite_display_list);
#endif
	    }
	}
#else
      mav_listPointerReset (c->composites);
      while (mav_listItemNext (c->composites, (void **)&obj))
	mav_callbackDrawExec (mav_win_current, obj, NULL);
#endif
    }
#endif

  return 1;
}

int
mav_occluderGetMatrix
(MAV_object *obj, MAV_matrix **mat)
{
  *mat= &(((MAV_occluder *)obj->the_data)->matrix);
  return 1;
}

int
mav_occluderGetUserdef
(MAV_object *obj, void ***ud)
{
  *ud= &(((MAV_occluder *)obj->the_data)->userdef);
  return 1;
}

int
mav_occluderBBox
(MAV_object *obj, MAV_BB *bb)
{
  MAV_occluder *o= (MAV_occluder *) obj->the_data;
  mav_BBAlign (o->occluder, o->matrix, &o->global_bb);
  *bb= o->global_bb;
  return 1;
}

int
mav_occluderDraw
(MAV_object *obj, MAV_drawInfo *di)
{
  if (wire)
    {
      MAV_occluder *o= (MAV_occluder *)obj->the_data;
      MAV_BB bb;

      mav_BBAlign (o->occluder, o->matrix, &bb);
      bb.min.x += 0.1;
      bb.min.y += 0.1;
      bb.min.z += 0.1;
      bb.max.x -= 0.1;
      bb.max.y -= 0.1;
      bb.max.z -= 0.1;
      mav_BBDisplayWithColour (mav_win_current, bb, MAV_COLOUR_RED);
    }

  return 1;
}

int
mav_billboardGetMatrix
(MAV_object *obj, MAV_matrix **mat)
{
  *mat= &(((MAV_billboard *)obj->the_data)->matrix);
  return 1;
}

int
mav_billboardGetUserdef
(MAV_object *obj, void ***ud)
{
  *ud= &(((MAV_billboard *)obj->the_data)->userdef);
  return 1;
}

int
mav_billboardBBox
(MAV_object *obj, MAV_BB *bb)
{
  MAV_billboard *b= (MAV_billboard *) obj->the_data;
  mav_BBAlign (b->bb, b->matrix, bb);
  return 1;
}

int
mav_billboardDraw
(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_billboard *bb= (MAV_billboard *)obj->the_data;
  MAV_vector dr;
  float rota;
  MAV_vector p;
  MAV_texCoord t;
  MAV_surfaceParams sp;

  dr.x= mav_win_current->vp->eye.x-object_pos.x;
  dr.z= mav_win_current->vp->eye.z-object_pos.z;
  dr.x /= object_dist;
  dr.z /= object_dist;

  rota= 57.29577951*acos(dr.z);
  if (mav_win_current->vp->eye.x < object_pos.x) rota= -rota;

  if (wire)
    {
      sp.mode= MAV_COLOUR;
      sp.colour= MAV_COLOUR_GREEN;
      mav_surfaceParamsUse (&sp);

      mav_gfxMatrixPush ();
      mav_gfxMatrixMult (bb->matrix);
#ifdef MAV_GL
      rot (rota, 'y');
#else
      glRotatef (rota, 0.0,1.0,0.0);
#endif
      mav_gfxLineClosedBegin ();
      p.x= -bb->width/2.0;
      p.y= bb->height;
      p.z= 0.0;
      mav_gfxVertex (p);

      p.y= 0.0;
      mav_gfxVertex (p);

      p.x= bb->width/2.0;
      mav_gfxVertex (p);

      p.y= bb->height;
      mav_gfxVertex (p);
      mav_gfxLineClosedEnd ();

      mav_gfxLineBegin ();
      p.x= 0.0;
      p.y= bb->height/2.0;
      p.z= 0.0;
      mav_gfxVertex (p);

      p.z= 5.0;
      mav_gfxVertex (p);

      p.x= -0.5;
      p.z= 4.0;
      mav_gfxVertex (p);
      mav_gfxLineEnd ();

      mav_gfxLineBegin ();
      p.x= 0.0;
      p.z= 5.0;
      mav_gfxVertex (p);

      p.x= 0.5;
      p.z= 4.0;
      mav_gfxVertex (p);
      mav_gfxLineEnd ();

      mav_gfxMatrixPop ();
    }
  else
    {
      mav_surfaceParamsUse (&bb->sp);

      mav_gfxMatrixPush ();
      mav_gfxMatrixMult (bb->matrix);

#ifdef MAV_GL
      rot (rota, 'y');
      RGBcolor (255,255,255);
#else
      glRotatef (rota, 0.0,1.0,0.0);
#endif

#ifdef MAV_GL
      blendfunction (BF_SA, BF_MSA);
#else
      glEnable (GL_BLEND);
      glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
#endif

      mav_gfxPolygonBegin();
      t.s= 0.0;
      t.t= 1.0;
      mav_gfxTexCoord (t);
      p.x= -bb->width/2.0;
      p.y= bb->height;
      p.z= 0.0;
      mav_gfxVertex (p);

      t.t= 0.0;
      mav_gfxTexCoord (t);
      p.y= 0.0;
      mav_gfxVertex (p);

      t.s= 1.0;
      mav_gfxTexCoord (t);
      p.x= bb->width/2.0;
      mav_gfxVertex (p);

      t.t= 1.0;
      mav_gfxTexCoord (t);
      p.y= bb->height;
      mav_gfxVertex (p);
      mav_gfxPolygonEnd ();

#ifdef MAV_GL
      blendfunction (BF_ONE, BF_ZERO);
#else
      glDisable (GL_BLEND);
#endif

      mav_gfxMatrixPop ();
    }

  return 1;
}


#ifdef DISPLAY_LISTS
static void
Compile_Display_List
(MAV_cityCell *c, Poly *polys)
{
  Poly *p;
  int i;
  int current_texture, current_material;
  int num_polys_left;
  int num_textured_polys_left;
  int num_textured_lit_polys_left;

  p= polys;
  num_polys_left= 0;
  num_textured_polys_left= 0;
  num_textured_lit_polys_left= 0;
  while (p)
    {
      num_polys_left++;
      if (p->sp.texture > -1)
	{
	  if (p->sp.material > -1)
	    num_textured_lit_polys_left++;
	  else
	    num_textured_polys_left++;
	}
      p->drawn= 0;
      p= p->next;
    }

  while (num_polys_left > 0)
    {
      current_texture= -1;
      current_material= -1;
      p= polys;
      while (p)
	{
	  if (current_texture == -1 && current_material == -1 && p->drawn == 0)
	    {
	      if ((num_textured_polys_left > 0 && p->sp.texture > -1) ||
		  (num_textured_lit_polys_left > 0 && p->sp.texture > -1 && p->sp.material > -1) ||
		  num_polys_left > 0)
		{
		  current_texture= p->sp.texture;
		  current_material= p->sp.material;

		  mav_surfaceParamsUse (p->sp);
#ifndef MAV_GL
		  glBegin (GL_QUADS);
#endif
		}
	    }

	  if (p->drawn == 0 && p->sp.texture == current_texture && p->sp.material == current_material)
	    {
	      num_polys_left--;
	      if (p->sp.texture > -1)
		{
		  if (p->sp.material > -1) num_textured_lit_polys_left--;
		  else num_textured_polys_left--;
		}

	      p->drawn= 1;
#ifdef MAV_GL
	      mav_gfxPolygonBegin ();
#endif
	      mav_gfxNormal (p->normal);
	      for (i= 0; i< (int)p->num; i++)
		{
		  if (p->sp.texture > -1)
		    mav_gfxTexCoord (p->texcoords[i]);
		   mav_gfxVertex (p->verts[i]);
		}
#ifdef MAV_GL
	      mav_gfxPolygonEnd ();
#endif
	    }

	  p= p->next;
	}
#ifndef MAV_GL
      glEnd();
#endif
    }
}

void
Build_Display_Lists (void)
{
  MAV_object *obj, *cobj;
  MAV_cityCell *c;
  int i;
  int old_wire;

  old_wire= wire;
  wire= 0;

  mav_listPointerReset (list_of_objects);
  while (mav_listItemNext (list_of_objects, (void **)&obj))
    {
      if (obj->the_class == mav_class_citycell)
	{
	  mav_surfaceParamsUndefine ();

	  c= (MAV_cityCell *) obj->the_data;

	  c->cell_display_list= num_display_lists;
#ifdef MAV_GL
	  makeobj (num_display_lists);
#else
	  glNewList (num_display_lists, GL_COMPILE);
#endif
	  Compile_Display_List (c, c->polys);
#ifdef MAV_GL
	  closeobj ();
#else
	  glEndList();
#endif
	  num_display_lists++;

	  if (c->lod_polys)
	    {
	      mav_surfaceParamsUndefine ();

	      c->lod_display_list= num_display_lists;
#ifdef MAV_GL
	      makeobj (num_display_lists);
#else
	      glNewList (num_display_lists, GL_COMPILE);
#endif
	      Compile_Display_List (c, c->lod_polys);
#ifdef MAV_GL
	      closeobj ();
#else
	      glEndList();
#endif
	      num_display_lists++;
	    }

#ifdef COMPOSITES
	  if (c->composites)
	    {
	      mav_surfaceParamsUndefine ();

	      c->composite_display_list= num_display_lists;
#ifdef MAV_GL
	      makeobj (num_display_lists);
#else
	      glNewList (num_display_lists, GL_COMPILE);
#endif
	      mav_listPointerReset (c->composites);
	      while (mav_listItemNext (c->composites, (void **)&cobj))
		mav_callbackDrawExec (mav_win_current, cobj, NULL);
#ifdef MAV_GL
	      closeobj ();
#else
	      glEndList();
#endif
	      num_display_lists++;
	    }

	  if (c->features)
	    {
	      mav_surfaceParamsUndefine ();

	      c->feature_display_list= num_display_lists;
#ifdef MAV_GL
	      makeobj (num_display_lists);
#else
	      glNewList (num_display_lists, GL_COMPILE);
#endif
	      mav_listPointerReset (c->features);
	      while (mav_listItemNext (c->features, (void **)&cobj))
		mav_callbackDrawExec (mav_win_current, cobj, NULL);
#ifdef MAV_GL
	      closeobj ();
#else
	      glEndList();
#endif
	      num_display_lists++;
	    }
#endif
	}
    }

  wire= old_wire;
}
#endif
