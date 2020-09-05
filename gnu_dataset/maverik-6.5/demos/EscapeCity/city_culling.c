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
#else
#include <sys/time.h>
#endif
#include <GL/gl.h>
#include "maverik.h"
#include "city_macros.h"
#include "city_types.h"

static int Cull_Object (MAV_object *obj, int this_id);
int mav_cityCellDraw (MAV_object *obj, MAV_drawInfo *di);
int mav_compositeDrawWire (MAV_object *obj, MAV_drawInfo *di);
int mav_occluderDraw (MAV_object *obj, MAV_drawInfo *di);
int mav_billboardDraw (MAV_object *obj, MAV_drawInfo *di);

extern MAV_class *mav_class_avatar;
extern MAV_class *mav_class_occluder;
extern MAV_class *mav_class_citycell;
extern MAV_class *mav_class_billboard;
extern int num_visible_objects;
extern Visible_Object *visible_objects;
extern int num_rendered_cells;
extern int num_rendered_polygons;
extern int current_frame;
extern int wire;
extern int max_rendered_cells;
extern int culling;

BB bb_array[MAX_BILLBOARDS];
float object_dist;
MAV_vector object_pos;
int bb_pos;
int num_occluders= 0;
Candidate candidate;
#ifndef WIN32
struct timeval starttime,endtime;
#endif
double cull_time, render_time;

void
initTime (void)
{
#ifndef WIN32
  gettimeofday(&starttime, NULL);
#endif
}

double
calcTime (void)
{
#ifdef WIN32
  return 1.0;
#else
  int elapsedSec, elapsedUSec;

  gettimeofday(&endtime, NULL);
  elapsedSec= endtime.tv_sec-starttime.tv_sec;
  elapsedUSec= endtime.tv_usec-starttime.tv_usec;
  return 1000.0*(elapsedSec+elapsedUSec/1.0E+6);
#endif
}

static int
Compare_Object_Distances
(const void *a, const void *b)
{
  Visible_Object *s1, *s2;

  s1= (Visible_Object *)a;
  s2= (Visible_Object *)b;

  if (s1->dist < s2->dist) return -1;
  else if (s1->dist > s2->dist) return 1;
  else
    {
      if (s1->obj->the_class == mav_class_occluder && s2->obj->the_class != mav_class_occluder)
	return 1;
      else if (s1->obj->the_class != mav_class_occluder && s2->obj->the_class == mav_class_occluder)
	return -1;
      else return 0;
    }
}

static float
Find_Shortest_Distance
(MAV_object *obj)
{
  MAV_BB bb;
  MAV_vector dr;

  mav_callbackBBExec(mav_win_left, obj, &bb);

  if (mav_win_current->vp->eye.y > bb.max.y)
    {
      if (mav_win_current->vp->eye.x < bb.min.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* ac */
	      dr.x= mav_win_current->vp->eye.x-bb.min.x;
	      dr.y= mav_win_current->vp->eye.y-bb.max.y;
	      dr.z= mav_win_current->vp->eye.z-bb.min.z;
	      return sqrt(dr.x*dr.x+dr.y*dr.y+dr.z*dr.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* ad */
	      dr.x= mav_win_current->vp->eye.x-bb.min.x;
	      dr.y= mav_win_current->vp->eye.y-bb.max.y;
	      dr.z= mav_win_current->vp->eye.z-bb.max.z;
	      return sqrt(dr.x*dr.x+dr.y*dr.y+dr.z*dr.z);
	    }
	  else
	    {
	      /* a */
	      dr.x= mav_win_current->vp->eye.x-bb.min.x;
	      dr.y= mav_win_current->vp->eye.y-bb.max.y;
	      return sqrt(dr.x*dr.x+dr.y*dr.y);
	    }
	}
      else if (mav_win_current->vp->eye.x > bb.max.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* cb */
	      dr.x= mav_win_current->vp->eye.x-bb.max.x;
	      dr.y= mav_win_current->vp->eye.y-bb.max.y;
	      dr.z= mav_win_current->vp->eye.z-bb.min.z;
	      return sqrt(dr.x*dr.x+dr.y*dr.y+dr.z*dr.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* bd */
	      dr.x= mav_win_current->vp->eye.x-bb.max.x;
	      dr.y= mav_win_current->vp->eye.y-bb.max.y;
	      dr.z= mav_win_current->vp->eye.z-bb.max.z;
	      return sqrt(dr.x*dr.x+dr.y*dr.y+dr.z*dr.z);
	    }
	  else
	    {
	      /* b */
	      dr.x= mav_win_current->vp->eye.x-bb.max.x;
	      dr.y= mav_win_current->vp->eye.y-bb.max.y;
	      return sqrt(dr.x*dr.x+dr.y*dr.y);
	    }
	}
      else
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* c */
	      dr.y= mav_win_current->vp->eye.y-bb.max.y;
	      dr.z= mav_win_current->vp->eye.z-bb.min.z;
	      return sqrt(dr.y*dr.y+dr.z*dr.z);
	    }
	  else
	    {
	      /* d */
	      dr.y= mav_win_current->vp->eye.y-bb.max.y;
	      dr.z= mav_win_current->vp->eye.z-bb.max.z;
	      return sqrt(dr.y*dr.y+dr.z*dr.z);
	    }
	}
    }
  else if (mav_win_current->vp->eye.y < bb.min.y)
    {
      if (mav_win_current->vp->eye.x < bb.min.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* ac */
	      dr.x= mav_win_current->vp->eye.x-bb.min.x;
	      dr.y= mav_win_current->vp->eye.y-bb.min.y;
	      dr.z= mav_win_current->vp->eye.z-bb.min.z;
	      return sqrt(dr.x*dr.x+dr.y*dr.y+dr.z*dr.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* ad */
	      dr.x= mav_win_current->vp->eye.x-bb.min.x;
	      dr.y= mav_win_current->vp->eye.y-bb.min.y;
	      dr.z= mav_win_current->vp->eye.z-bb.max.z;
	      return sqrt(dr.x*dr.x+dr.y*dr.y+dr.z*dr.z);
	    }
	  else
	    {
	      /* a */
	      dr.x= mav_win_current->vp->eye.x-bb.min.x;
	      dr.y= mav_win_current->vp->eye.y-bb.min.y;
	      return sqrt(dr.x*dr.x+dr.y*dr.y);
	    }
	}
      else if (mav_win_current->vp->eye.x > bb.max.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* cb */
	      dr.x= mav_win_current->vp->eye.x-bb.max.x;
	      dr.y= mav_win_current->vp->eye.y-bb.min.y;
	      dr.z= mav_win_current->vp->eye.z-bb.min.z;
	      return sqrt(dr.x*dr.x+dr.y*dr.y+dr.z*dr.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* bd */
	      dr.x= mav_win_current->vp->eye.x-bb.max.x;
	      dr.y= mav_win_current->vp->eye.y-bb.min.y;
	      dr.z= mav_win_current->vp->eye.z-bb.max.z;
	      return sqrt(dr.x*dr.x+dr.y*dr.y+dr.z*dr.z);
	    }
	  else
	    {
	      /* b */
	      dr.x= mav_win_current->vp->eye.x-bb.max.x;
	      dr.y= mav_win_current->vp->eye.y-bb.min.y;
	      return sqrt(dr.x*dr.x+dr.y*dr.y);
	    }
	}
      else
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* c */
	      dr.y= mav_win_current->vp->eye.y-bb.min.y;
	      dr.z= mav_win_current->vp->eye.z-bb.min.z;
	      return sqrt(dr.y*dr.y+dr.z*dr.z);
	    }
	  else
	    {
	      /* d */
	      dr.y= mav_win_current->vp->eye.y-bb.min.y;
	      dr.z= mav_win_current->vp->eye.z-bb.max.z;
	      return sqrt(dr.y*dr.y+dr.z*dr.z);
	    }
	}
    }
  else
    {
      if (mav_win_current->vp->eye.x < bb.min.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* ac */
	      dr.x= mav_win_current->vp->eye.x-bb.min.x;
	      dr.z= mav_win_current->vp->eye.z-bb.min.z;
	      return sqrt(dr.x*dr.x+dr.z*dr.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* ad */
	      dr.x= mav_win_current->vp->eye.x-bb.min.x;
	      dr.z= mav_win_current->vp->eye.z-bb.max.z;
	      return sqrt(dr.x*dr.x+dr.z*dr.z);
	    }
	  else
	    {
	      /* a */
	      return fabs(mav_win_current->vp->eye.x-bb.min.x);
	    }
	}
      else if (mav_win_current->vp->eye.x > bb.max.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* cb */
	      dr.x= mav_win_current->vp->eye.x-bb.max.x;
	      dr.z= mav_win_current->vp->eye.z-bb.min.z;
	      return sqrt(dr.x*dr.x+dr.z*dr.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* bd */
	      dr.x= mav_win_current->vp->eye.x-bb.max.x;
	      dr.z= mav_win_current->vp->eye.z-bb.max.z;
	      return sqrt(dr.x*dr.x+dr.z*dr.z);
	    }
	  else
	    {
	      /* b */
	      return fabs(mav_win_current->vp->eye.x-bb.max.x);
	    }
	}
      else
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* c */
	      return fabs(mav_win_current->vp->eye.z-bb.min.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* d */
	      return fabs(mav_win_current->vp->eye.z-bb.max.z);
	    }
	  else
	    {
	      /* inside */
	      return 0.0;
	    }
	}
    }
}

static int
Compare_Billboard_Distances
(const void *a, const void *b)
{
  BB *b1, *b2;

  b1= (BB *)a;
  b2= (BB *)b;

  if (b1->dist > b2->dist) return (-1);
  else if (b1->dist < b2->dist) return (1);
  else return 0;
}

void
Redisplay_Objects (void)
{
  int i;

  for (i= 0; i< num_visible_objects; i++)
    {
      if (visible_objects[i].visible && visible_objects[i].obj->the_class != mav_class_billboard)
	{
	  object_dist= visible_objects[i].dist;
	  mav_callbackDrawExec (mav_win_current, visible_objects[i].obj, NULL);
	}
    }

  if (bb_pos > 0)
    {
      for (i= 0; i< bb_pos; i++)
	{
	  object_pos= bb_array[i].pos;
	  object_dist= bb_array[i].dist;
	  mav_billboardDraw (bb_array[i].obj, NULL);
	}
    }
}

void
Display_Objects (void)
{
  int i;
  MAV_vector dr;

  num_occluders= 0;
  bb_pos= 0;

  /* calculate minimum distance from vp eye point to each cell */
  for (i= 0; i< num_visible_objects; i++)
    visible_objects[i].dist= Find_Shortest_Distance (visible_objects[i].obj);

  /* sort them */
  qsort ((void *)visible_objects, num_visible_objects, sizeof(Visible_Object), Compare_Object_Distances);

  for (i= 0; i< num_visible_objects; i++)
    visible_objects[i].visible= culling ? Cull_Object (visible_objects[i].obj, i) : 1;

  cull_time= calcTime();

#ifdef MAV_GL
  if (num_visible_objects > max_rendered_cells)
    num_visible_objects= max_rendered_cells;
#endif

  for (i= 0; i< num_visible_objects; i++)
    {
      if (visible_objects[i].visible)
	{
	  if (visible_objects[i].obj->the_class != mav_class_billboard)
	    {
	      object_dist= visible_objects[i].dist;
	      mav_callbackDrawExec (mav_win_current, visible_objects[i].obj, NULL);
	    }
	  else
	    {
	      /* add to billboard list */
	      bb_array[bb_pos].obj= visible_objects[i].obj;
	      bb_array[bb_pos].pos.x= 0.0;
	      bb_array[bb_pos].pos.y= ((MAV_billboard *)visible_objects[i].obj->the_data)->height/2.0;
	      bb_array[bb_pos].pos.z= 0.0;
	      bb_array[bb_pos].pos= mav_vectorMult (bb_array[bb_pos].pos, ((MAV_billboard *)visible_objects[i].obj->the_data)->matrix);
	      dr.x= mav_win_current->vp->eye.x-bb_array[bb_pos].pos.x;
	      dr.z= mav_win_current->vp->eye.z-bb_array[bb_pos].pos.z;
	      bb_array[bb_pos].dist= sqrt(dr.x*dr.x+dr.z*dr.z);
	      bb_pos++;
	    }
	}
    }

  if (bb_pos > 0)
    {
      if (!wire && bb_pos > 1)
	qsort ((void *)bb_array, bb_pos, sizeof(BB), Compare_Billboard_Distances);

      for (i= 0; i< bb_pos; i++)
	{
	  object_pos= bb_array[i].pos;
	  object_dist= bb_array[i].dist;
	  mav_billboardDraw (bb_array[i].obj, NULL);
	}
    }

  render_time= calcTime()-cull_time;
}

/*
  Faster Line Segment Intersection:
   Franklin Antonio, GGemsIII
*/
static int
Lines_Intersect
(MAV_vector *p1, MAV_vector *p2, MAV_vector *p3, MAV_vector *p4, MAV_vector *p, float *dist) 
{
  register float Ax,Bx,Cx,Ay,By,Cy,d,e,f;
  register float x1lo,x1hi,y1lo,y1hi;

  Ax= p2->x-p1->x;
  Bx= p3->x-p4->x;

  if (Ax < 0.0)
    {
      x1lo= p2->x;
      x1hi= p1->x;
    }
  else
    {
      x1hi= p2->x;
      x1lo= p1->x;
    }

  if (Bx > 0.0)
    {
      if (x1hi <= p4->x || p3->x <= x1lo) return 0;
    }
  else
    {
      if (x1hi <= p3->x || p4->x <= x1lo) return 0;
    }

  Ay= p2->y-p1->y;
  By= p3->y-p4->y;

  if (Ay < 0.0)
    {
      y1lo= p2->y;
      y1hi= p1->y;
    }
  else
    {
      y1hi= p2->y;
      y1lo= p1->y;
    }

  if (By > 0.0)
    {
      if (y1hi <= p4->y || p3->y <= y1lo) return 0;
    }
  else
    {
      if (y1hi <= p3->y || p4->y <= y1lo) return 0;
    }

  Cx= p1->x-p3->x;
  Cy= p1->y-p3->y;
  d= By*Cx-Bx*Cy;
  f= Ay*Bx-Ax*By;
  if (f > 0.0)
    {
      if (d < 0.0 || d > f) return 0;
    }
  else
    {
      if (d > 0.0 || d < f) return 0;
    }

  e= Ax*Cy-Ay*Cx;
  if (f > 0.0)
    {
      if (e < 0.0 || e > f) return 0;
    }
  else
    {
      if (e > 0.0 || e < f) return 0;
    }

  if (fabs(f) < 1e-6) return 0; /* need to fix for colinear */

  d /= f;
  p->x= p1->x+d*Ax;
  p->y= p1->y+d*Ay;
  *dist= d;

  return 1;
}

/*
  Crossings point-in-polygon algorithm:
   Eric Haines, GGemsIV
*/
static int
Point_In_Polygon
(MAV_vector *pgon, int numverts, MAV_vector *point)
{
  register int j, yflag0, yflag1, inside_flag, xflag0 ;
  register float ty, tx;
  register MAV_vector *vtx0, *vtx1;
  register int line_flag;
  register int v= 0;

  tx= point->x;
  ty= point->y;

  vtx0= &pgon[numverts-1];
  yflag0= (vtx0->y >= ty);
  vtx1= &pgon[v];

  inside_flag= 0;
  line_flag= 0;
  for (j= numverts+1; --j;)
    {
      yflag1= (vtx1->y >= ty);
      if (yflag0 != yflag1)
	{
	  xflag0= (vtx0->x >= tx);
	  if (xflag0 == (vtx1->x >= tx))
	    {
	      if (xflag0) inside_flag = !inside_flag;
            }
	  else
	    {
	      if ((vtx1->x-(vtx1->y-ty)*(vtx0->x-vtx1->x)/(vtx0->y-vtx1->y)) >= tx)
		{
		  inside_flag= !inside_flag;
                }
            }

            if (line_flag)
	      return (inside_flag);

            line_flag= 1;
        }

      yflag0= yflag1;
      vtx0= vtx1;
      v++;
      vtx1= &pgon[v];
    }

  return (inside_flag);
}

static MAV_vector
Transform_Point
(float x, float y, float z)
{
  MAV_matrix *mat= &mav_win_left->pdvMat;
  MAV_vector out;
  register float w;

  out.x= x*mat->mat[0][0]+y*mat->mat[0][1]+z*mat->mat[0][2]+mat->mat[0][3];
  out.y= x*mat->mat[1][0]+y*mat->mat[1][1]+z*mat->mat[1][2]+mat->mat[1][3];
  out.z= x*mat->mat[2][0]+y*mat->mat[2][1]+z*mat->mat[2][2]+mat->mat[2][3];
  w=     x*mat->mat[3][0]+y*mat->mat[3][1]+z*mat->mat[3][2]+mat->mat[3][3];

  if (w < 0.0)
    w= -w;

  out.x /= w;
  out.y /= w;
  out.z /= w;

  return out;
}

/*
  Liang-Barsky parametric line-clipping algorithm:
   Foley and van Dam
*/
static int
CLIPt
(float denom, float num, float *tE, float *tL)
{
  register float t;

  if (denom > 0.0)
    {
      t= num/denom;
      if (t > *tL)
	return 0;
      else if (t > *tE)
	*tE= t;
    }
  else if (denom < 0.0)
    {
      t= num/denom;
      if (t < *tE)
	return 0;
      else if (t < *tL)
	*tL= t;
    }
  else
    {
      if (num > 0.0)
	return 0;
    }
  return 1;
}

#define PointInside(x,y) (x >= -1.0 && x <= 1.0 && y >= -1.0 && y <= 1.0)
static int
Liang_Barsky
(MAV_vector *p0, MAV_vector *p1)
{
  register float dx= p1->x-p0->x;
  register float dy= p1->y-p0->y;
  float tE= 0.0;
  float tL= 1.0;

  /* this line might not be necessary */
  if (fabs(dx) < 1e-6 && fabs(dy) < 1e-6 && PointInside(p0->x,p0->y))
    return 1;

  if (CLIPt(dx,-1.0-p0->x,&tE,&tL))
    if (CLIPt(-dx,p0->x-1.0,&tE,&tL))
      if (CLIPt(dy,-1.0-p0->y,&tE,&tL))
	if (CLIPt(-dy,p0->y-1.0,&tE,&tL))
	  {
	    if (tL < 1.0)
	      {
		p1->x= p0->x+tL*dx;
		p1->y= p0->y+tL*dy;
	      }
	    if (tE > 0.0)
	      {
		p0->x= p0->x+tE*dx;
		p0->y= p0->y+tE*dy;
	      }
	    return 1;
	  }
  return 0;
}

static void
Build_Occluder
(MAV_occluder *occluder)
{
  int i;
  MAV_BB bb;

  mav_BBAlign (occluder->occluder, occluder->matrix, &bb);

  if (mav_win_current->vp->eye.y > bb.max.y)
    {
      if (mav_win_current->vp->eye.x < bb.min.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* ac+top */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[4]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[5]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* ad+top */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      occluder->vertices[1]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[4]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[5]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	  else
	    {
	      /* a+top */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[4]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      occluder->vertices[5]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	}
      else if (mav_win_current->vp->eye.x > bb.max.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* cb+top */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[4]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* bd+top */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      occluder->vertices[1]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[4]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[5]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	    }
	  else
	    {
	      /* b+top */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[4]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	}
      else
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* c+top */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[4]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      occluder->vertices[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* d+top */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      occluder->vertices[1]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[4]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[5]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	  else /* top */
	    occluder->num_vertices= 0;
	}
    }
  else if (mav_win_current->vp->eye.y < bb.min.y)
    {
      if (mav_win_current->vp->eye.x < bb.min.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* ac+base */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[4]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[5]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* ad+base */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[4]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[5]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	  else
	    {
	      /* a+base */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[4]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[5]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	}
      else if (mav_win_current->vp->eye.x > bb.max.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* cb+base */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[4]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* bd+base */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[4]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[5]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	    }
	  else
	    {
	      /* b+base */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      occluder->vertices[1]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[4]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	}
      else
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* c+base */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[4]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[5]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* d+base */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[4]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[5]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	    }
	  else /* base */
	    occluder->num_vertices= 0;
	}
    }
  else
    {
      if (mav_win_current->vp->eye.x < bb.min.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* ac */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[4]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[5]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* ad */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[4]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[5]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	  else
	    {
	      /* a */
	      occluder->num_vertices= 4;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	}
      else if (mav_win_current->vp->eye.x > bb.max.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* cb */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[4]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* bd */
	      occluder->num_vertices= 6;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[4]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[5]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	    }
	  else
	    {
	      /* b */
	      occluder->num_vertices= 4;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      occluder->vertices[1]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	}
      else
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* c */
	      occluder->num_vertices= 4;
	      occluder->vertices[0]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      occluder->vertices[1]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      occluder->vertices[2]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      occluder->vertices[3]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* d */
	      occluder->num_vertices= 4;
	      occluder->vertices[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      occluder->vertices[1]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      occluder->vertices[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      occluder->vertices[3]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	    }
	  else /* inside */
	    {
	      occluder->num_vertices= 0;
	      return;
	    }
	}
    }

  /* find occluder bound */
  for (i= 0; i< occluder->num_vertices; i++)
    {
      if (i == 0)
	{
	  occluder->bound.min.x= occluder->bound.max.x= occluder->vertices[0].x;
	  occluder->bound.min.y= occluder->bound.max.y= occluder->vertices[0].y;
	}
      else
	{
	  if (occluder->vertices[i].x < occluder->bound.min.x)
	    occluder->bound.min.x= occluder->vertices[i].x;
	  if (occluder->vertices[i].y < occluder->bound.min.y)
	    occluder->bound.min.y= occluder->vertices[i].y;
	  if (occluder->vertices[i].x > occluder->bound.max.x)
	    occluder->bound.max.x= occluder->vertices[i].x;
	  if (occluder->vertices[i].y > occluder->bound.max.y)
	    occluder->bound.max.y= occluder->vertices[i].y;
	}
    }

  if (occluder->bound.max.x < -1.0 || occluder->bound.min.x > 1.0 ||
      occluder->bound.max.y < -1.0 || occluder->bound.min.y > 1.0)
    occluder->num_vertices= 0;

  if (occluder->num_vertices > 0)
    num_occluders++;
}

static int
Build_Candidate
(MAV_object *obj, Candidate *candidate)
{
  int i;
  MAV_vector p[6];
  int num_edges= 0;
  MAV_vector start,end;
  MAV_BB bb;

  /* get an axis aligned bounding box in world coordinates */
  mav_callbackBBExec (mav_win_left, obj, &bb);

  if (mav_win_current->vp->eye.y > bb.max.y)
    {
      if (mav_win_current->vp->eye.x < bb.min.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* ac+top */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[4]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[5]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* ad+top */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[1]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[4]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[5]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	  else
	    {
	      /* a+top */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[4]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[5]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	}
      else if (mav_win_current->vp->eye.x > bb.max.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* cb+top */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[4]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* bd+top */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[1]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[3]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[4]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[5]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	    }
	  else
	    {
	      /* b+top */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[2]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[4]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	}
      else
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* c+top */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[4]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* d+top */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[1]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[4]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[5]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	  else
	    {
	      /* top */
	      num_edges= 4;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[2]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[3]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	}
    }
  else if (mav_win_current->vp->eye.y < bb.min.y)
    {
      if (mav_win_current->vp->eye.x < bb.min.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* ac+base */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[1]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[4]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[5]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* ad+base */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[4]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[5]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	  else
	    {
	      /* a+base */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[4]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[5]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	}
      else if (mav_win_current->vp->eye.x > bb.max.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* cb+base */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[1]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[3]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[4]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* bd+base */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[3]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[4]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[5]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	    }
	  else
	    {
	      /* b+base */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[1]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[3]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[4]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	}
      else
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* c+base */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[1]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[4]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[5]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* d+base */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[3]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[4]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[5]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	    }
	  else
	    {
	      /* base */
	      num_edges= 4;
	      p[0]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[1]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	    }
	}
    }
  else
    {
      if (mav_win_current->vp->eye.x < bb.min.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* ac */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[1]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[4]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[5]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* ad */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[4]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[5]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	  else
	    {
	      /* a */
	      num_edges= 4;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[3]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	    }
	}
      else if (mav_win_current->vp->eye.x > bb.max.x)
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* cb */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[1]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[3]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[4]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[5]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* bd */
	      num_edges= 6;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[3]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[4]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[5]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	    }
	  else
	    {
	      /* b */
	      num_edges= 4;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	      p[1]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	    }
	}
      else
	{
	  if (mav_win_current->vp->eye.z < bb.min.z)
	    {
	      /* c */
	      num_edges= 4;
	      p[0]= Transform_Point(bb.min.x,bb.max.y,bb.min.z);
	      p[1]= Transform_Point(bb.min.x,bb.min.y,bb.min.z);
	      p[2]= Transform_Point(bb.max.x,bb.min.y,bb.min.z);
	      p[3]= Transform_Point(bb.max.x,bb.max.y,bb.min.z);
	    }
	  else if (mav_win_current->vp->eye.z > bb.max.z)
	    {
	      /* d */
	      num_edges= 4;
	      p[0]= Transform_Point(bb.max.x,bb.max.y,bb.max.z);
	      p[1]= Transform_Point(bb.max.x,bb.min.y,bb.max.z);
	      p[2]= Transform_Point(bb.min.x,bb.min.y,bb.max.z);
	      p[3]= Transform_Point(bb.min.x,bb.max.y,bb.max.z);
	    }
	  else /* inside */
	    return 0;
	}
    }

  /* clip edges to screen */
  candidate->num_edges= 0;
  for (i= 0; i< num_edges; i++)
    {
      start= p[i];
      end= (i == num_edges-1) ? p[0] : p[i+1];
      if (Liang_Barsky (&start,&end))
	{
	  candidate->edges[candidate->num_edges].start= start;
	  candidate->edges[candidate->num_edges].end= end;
	  candidate->edges[candidate->num_edges].visible= 1;

	  if (candidate->num_edges == 0)
	    {
	      candidate->bound.min.x= candidate->bound.max.x= start.x;
	      candidate->bound.min.y= candidate->bound.max.y= start.y;
	    }
	  else
	    {
	      if (start.x < candidate->bound.min.x) candidate->bound.min.x= start.x;
	      if (start.y < candidate->bound.min.y) candidate->bound.min.y= start.y;
	      if (start.x > candidate->bound.max.x) candidate->bound.max.x= start.x;
	      if (start.y > candidate->bound.max.y) candidate->bound.max.y= start.y;
	    }
	  if (end.x < candidate->bound.min.x) candidate->bound.min.x= end.x;
	  if (end.y < candidate->bound.min.y) candidate->bound.min.y= end.y;
	  if (end.x > candidate->bound.max.x) candidate->bound.max.x= end.x;
	  if (end.y > candidate->bound.max.y) candidate->bound.max.y= end.y;

	  candidate->num_edges++;
	}
    }

  candidate->num_visible_edges= candidate->num_edges;

  if (candidate->num_edges > 0)
    return 1;
  else
    return 0;
}

static int
Compare_Intersection_Distances
(const void *a, const void *b)
{
  Intersection *s1, *s2;

  s1= (Intersection *)a;
  s2= (Intersection *)b;

  if (s1->dist < s2->dist) return -1;
  else if (s1->dist > s2->dist) return 1;
  else return 0;
}

static void
Clip_Candidate
(Candidate *candidate, MAV_occluder *occluder)
{
  int i,j,n;
  int okay;
  int jp1;
  MAV_vector p;
  float dist;
  int num_intersections;
  Intersection intersections[8];
  Intersection tmp;
  int inside;
  int redo;

  /* test bboxes */
  if ((candidate->bound.max.x <= occluder->bound.min.x) ||
      (candidate->bound.min.x >= occluder->bound.max.x) ||
      (candidate->bound.max.y <= occluder->bound.min.y) ||
      (candidate->bound.min.y >= occluder->bound.max.y))
    return;

  /* test each candidate edge */
  for (i= 0; i< candidate->num_edges; i++)
    {
      if (candidate->edges[i].visible)
	{
	  okay= 1;
	  /* see if edge misses occluder bbox */
	  if (candidate->edges[i].start.x > candidate->edges[i].end.x)
	    {
	      if (candidate->edges[i].start.x <= occluder->bound.min.x)
		okay= 0;
	      if (candidate->edges[i].end.x >= occluder->bound.max.x)
		okay= 0;
	    }
	  else
	    {
	      if (candidate->edges[i].end.x <= occluder->bound.min.x)
		okay= 0;
	      if (candidate->edges[i].start.x >= occluder->bound.max.x)
		okay= 0;
	    }
	  if (candidate->edges[i].start.y > candidate->edges[i].end.y)
	    {
	      if (candidate->edges[i].start.y <= occluder->bound.min.y)
		okay= 0;
	      if (candidate->edges[i].end.y >= occluder->bound.max.y)
		okay= 0;
	    }
	  else
	    {
	      if (candidate->edges[i].end.y <= occluder->bound.min.y)
		okay= 0;
	      if (candidate->edges[i].start.y >= occluder->bound.max.y)
		okay= 0;
	    }

	  if (okay)
	    {
	      num_intersections= 2;
	      intersections[0].p= candidate->edges[i].start;
	      intersections[0].dist= 0.0;
	      intersections[1].p= candidate->edges[i].end;
	      intersections[1].dist= 1.0;

	      /* test for intersect with occluder edge */
	      for (j= 0; j< occluder->num_vertices; j++)
		{
		  jp1= (j == occluder->num_vertices-1) ? 0 : j+1;
		  if (Lines_Intersect (&candidate->edges[i].start,&candidate->edges[i].end,
				       &occluder->vertices[j],&occluder->vertices[jp1],&p,&dist))
		    {
		      intersections[num_intersections].p= p;
		      intersections[num_intersections].dist= dist;
		      num_intersections++;
		    }
		}

	      if (num_intersections == 2)
		{
		  /* either inside or outside */
		  if (Point_In_Polygon (occluder->vertices, occluder->num_vertices, &candidate->edges[i].start))
		    {
		      /* candidate edge is completely inside occluder */
		      candidate->edges[i].visible= 0;
		      candidate->num_visible_edges--;
		    }
		}
	      else
		{
		  if (num_intersections == 3)
		    {
		      tmp= intersections[1];
		      intersections[1]= intersections[2];
		      intersections[2]= tmp;
		    }
		  else
		    {
		      /* sort intersections */
		      qsort ((void *)intersections, num_intersections, sizeof(Intersection), Compare_Intersection_Distances);
		    }

		  /* create some new edges */
		  inside= Point_In_Polygon (occluder->vertices, occluder->num_vertices, &candidate->edges[i].start);
		  redo= 0;
		  for (n= 0; n< num_intersections-1; n++)
		    {
		      if (!inside)
			{
			  if (redo == 0)
			    {
			      candidate->edges[i].start= intersections[n].p;
			      candidate->edges[i].end= intersections[n+1].p;
			      redo= 1;
			    }
			  else
			    {
			      candidate->edges[candidate->num_edges].start= intersections[n].p;
			      candidate->edges[candidate->num_edges].end= intersections[n+1].p;
			      candidate->edges[candidate->num_edges].visible= 1;
			      candidate->num_edges++;
			      candidate->num_visible_edges++;
			    }
			}
		      inside= !inside;
		    }

		  if (!redo)
		    {
		      /* remove this edge */
		      candidate->edges[i].visible= 0;
		      candidate->num_visible_edges--;
		    }
		}
	    }
	}
    }
}

int
Cull_Object
(MAV_object *obj, int this_id)
{
  int i;
  MAV_occluder *occluder;

  if (num_occluders > 0)
    {
      if (Build_Candidate (obj, &candidate))
	{
	  for (i= 0; i< this_id; i++)
	    {
	      if (visible_objects[i].obj->the_class == mav_class_occluder)
		{
		  occluder= (MAV_occluder *) visible_objects[i].obj->the_data;

		  Clip_Candidate (&candidate, occluder);
		  if (candidate.num_visible_edges == 0)
		    return 0;
		}
	    }
	}
    }

  if (obj->the_class == mav_class_occluder)
    Build_Occluder ((MAV_occluder *)obj->the_data);

  return 1;
}
