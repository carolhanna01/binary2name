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
#include <string.h>
#if !defined(WIN32) || defined(__CYGWIN__)
char *strdup (const char *s1);
#endif
#include <math.h>
#include "maverik.h"
#include "city_macros.h"
#include "city_types.h"

#define PAR_TO_WOR(u,v,r1,r2,r3,r4,res) {\
res.x= (r1).x+(u)*(r2).x+(v)*(r3).x+(u)*(v)*((r4).x-(r3).x);\
res.y= (r1).y+(u)*(r2).y+(v)*(r3).y+(u)*(v)*((r4).y-(r3).y);\
res.z= (r1).z+(u)*(r2).z+(v)*(r3).z+(u)*(v)*((r4).z-(r3).z); }

int vcb_main (int num_seeds, int num_cells);
void Init_Warp_Grid (void);
MAV_vector Calc_Grid_Position (float u, float v);
void Add_Composite (MAV_cityCell *cell, int index, MAV_matrix *matrix);
void Add_Feature (MAV_cityCell *cell, int index, MAV_matrix *matrix);

extern int num_cells;
extern MAV_class *mav_class_citycell;
extern MAV_class *mav_class_occluder;
extern MAV_class *mav_class_billboard;
extern int cells[RUN_LENGTH][4];
extern int total_objects;
MAV_list *list_of_objects;

int num_polys= 0;
int size;
float road_width;
Block *city_scape;
float *building_heights;
int num_billboards= 0;
MAV_cityCell *bounce_cell= NULL;

/* street names */
char street_endings[][3]= {
  "St.",
  "Av.",
  "Rd.",
  "Cl.",
  "Bl.",
  "Pl."
};

char street_names[][13]= {
  "Adrian",
  "Alan",
  "Applet",
  "Blancmange",
  "Crocodile",
  "Condensation",
  "Dan",
  "Deva",
  "Dodecahedron",
  "Dogger",
  "eSCAPE",
  "Essential",
  "Fisher",
  "Fountain",
  "Frog",
  "Flange",
  "James",
  "Janet",
  "Jon",
  "Karlsruhe",
  "Kettleofish",
  "Martin",
  "Maverik",
  "Mel",
  "Money",
  "Nonetheless",
  "Plethora",
  "Plinth",
  "Plum",
  "Polygon",
  "Roger",
  "Scarborough",
  "Simon",
  "Steve",
  "Suitcase",
  "Telly",
  "Tubby",
  "Theodolite",
  "Tim",
  "Toby",
  "1st",
  "2nd",
  "3rd",
  "4th",
  "5th",
  "6th",
  "7th",
  "8th",
  "9th",
  "10th",
  "11th",
  "12th",
  "13th",
  "14th",
  "15th",
  "16th",
  "17th",
  "18th",
  "19th",
  "20th",
  "21st",
  "22nd",
  "23rd",
  "24th",
  "25th",
  "26th",
  "27th",
  "28th",
  "29th",
  "30th",
  "31st",
  "32nd",
  "33rd",
  "34th",
  "35th",
  "36th",
  "37th",
  "38th",
  "39th",
  "40th"
};

#ifdef NEVER
static float
Find_Distance
(float blx, float blz, float trx, float trz, float x, float z)
{
  MAV_vector dr;

  if (x < blx)
    {
      if (z < blz)
	{
	  /* ac */
	  dr.x= x-blx;
	  dr.z= z-blz;
	  return sqrt(dr.x*dr.x+dr.z*dr.z);
	}
      else if (z > trz)
	{
	  /* ad */
	  dr.x= x-blx;
	  dr.z= z-trz;
	  return sqrt(dr.x*dr.x+dr.z*dr.z);
	}
      else
	{
	  /* a */
	  return x-blx;
	}
    }
  else if (x > trx)
    {
      if (z < blz)
	{
	  /* cb */
	  dr.x= x-trx;
	  dr.z= z-blz;
	  return sqrt(dr.x*dr.x+dr.z*dr.z);
	}
      else if (z > trz)
	{
	  /* bd */
	  dr.x= x-trx;
	  dr.z= z-trz;
	  return sqrt(dr.x*dr.x+dr.z*dr.z);
	}
      else
	{
	  /* b */
	  return x-trx;
	}
    }
  else
    {
      if (z < blz)
	{
	  /* c */
	  return z-blz;
	}
      else
	{
	  /* d */
	  return z-trz;
	}
    }
}
#endif

static void
Add_Poly
(MAV_cityCell *c, int num,
 int material, int texture,
 float *vx, float *vy, float *vz,
 float *tu, float *tv)
{
  int i;
  Poly *p;
  MAV_vector r1,r2;

  if (num > 4)
    {
      fprintf(stdout, "Warning(): can't have polygon with > 4 vertices\n");
      return;
    }

  p= mav_malloc(sizeof(Poly));

  p->next= c->polys;
  c->polys= p;
  c->num_polys++;
  num_polys++;
  p->num= (unsigned char)4;
#ifdef DISPLAY_LISTS
  p->drawn= 0;
#endif
  if (texture == -1)
    p->sp.mode= MAV_MATERIAL;
  else if (material == -1)
    p->sp.mode= MAV_TEXTURE;
  else
    p->sp.mode= MAV_LIT_TEXTURE;
  p->sp.material= material;
  p->sp.texture= texture;

  r1.x= vx[2]-vx[1];
  r1.y= vy[2]-vy[1];
  r1.z= vz[2]-vz[1];
  r2.x= vx[0]-vx[1];
  r2.y= vy[0]-vy[1];
  r2.z= vz[0]-vz[1];
  r1= mav_vectorNormalize(r1);
  r2= mav_vectorNormalize(r2);
  p->normal= mav_vectorCrossProduct(r1,r2);
  p->normal= mav_vectorNormalize(p->normal);
  p->normal.x= -p->normal.x;
  p->normal.y= -p->normal.y;
  p->normal.z= -p->normal.z;

  for (i= 0; i< num; i++)
    {
      p->verts[i].x= vx[num-1-i];
      p->verts[i].y= vy[num-1-i];
      p->verts[i].z= vz[num-1-i];
      if (texture != -1)
	{
	  p->texcoords[i].s= tu[num-1-i];
	  p->texcoords[i].t= tv[num-1-i];
	}
    }
  
  if (num == 3)
    {
      p->verts[3].x= vx[0];
      p->verts[3].y= vy[0];
      p->verts[3].z= vz[0];
      if (texture != -1)
	{
	  p->texcoords[3].s= tu[0];
	  p->texcoords[3].t= tv[0];
	}
    }
}

static void
Add_Lod_Poly
(MAV_cityCell *c, int num,
 int material, int texture,
 float *vx, float *vy, float *vz,
 float *tu, float *tv)
{
  int i;
  Poly *p;
  MAV_vector r1,r2;

  if (num > 4)
    {
      fprintf(stdout, "Warning(): can't have polygon with > 4 vertices\n");
      return;
    }

  p= mav_malloc(sizeof(Poly));

  p->next= c->lod_polys;
  c->lod_polys= p;
  c->num_lod_polys++;
  p->num= (unsigned char)4;
#ifdef DISPLAY_LISTS
  p->drawn= 0;
#endif
  if (texture == -1)
    p->sp.mode= MAV_MATERIAL;
  else if (material == -1)
    p->sp.mode= MAV_TEXTURE;
  else
    p->sp.mode= MAV_LIT_TEXTURE;
  p->sp.material= material;
  p->sp.texture= texture;

  r1.x= vx[2]-vx[1];
  r1.y= vy[2]-vy[1];
  r1.z= vz[2]-vz[1];
  r2.x= vx[0]-vx[1];
  r2.y= vy[0]-vy[1];
  r2.z= vz[0]-vz[1];
  r1= mav_vectorNormalize(r1);
  r2= mav_vectorNormalize(r2);
  p->normal= mav_vectorCrossProduct(r1,r2);
  p->normal= mav_vectorNormalize(p->normal);
  p->normal.x= -p->normal.x;
  p->normal.y= -p->normal.y;
  p->normal.z= -p->normal.z;

  for (i= 0; i< num; i++)
    {
      p->verts[i].x= vx[num-1-i];
      p->verts[i].y= vy[num-1-i];
      p->verts[i].z= vz[num-1-i];
      if (texture != -1)
	{
	  p->texcoords[i].s= tu[num-1-i];
	  p->texcoords[i].t= tv[num-1-i];
	}
    }
  
  if (num == 3)
    {
      p->verts[3].x= vx[0];
      p->verts[3].y= vy[0];
      p->verts[3].z= vz[0];
      if (texture != -1)
	{
	  p->texcoords[3].s= tu[0];
	  p->texcoords[3].t= tv[0];
	}
    }
}

static void
Add_Relative_Poly
(float sx, float sy, float sz,
 MAV_cityCell *c, int num, int material, int texture,
 float *vx, float *vy, float *vz,
 float *tu, float *tv)
{
  int i;
  Poly *p;
  MAV_vector r1,r2;

  if (num > 4)
    {
      fprintf(stdout, "Warning(): can't have polygon with > 4 vertices\n");
      return;
    }

  p= mav_malloc(sizeof(Poly));

  p->next= c->polys;
  c->polys= p;
  c->num_polys++;
  num_polys++;
  p->num= (unsigned char)num;
#ifdef DISPLAY_LISTS
  p->drawn= 0;
#endif
  if (texture == -1)
    p->sp.mode= MAV_MATERIAL;
  else if (material == -1)
    p->sp.mode= MAV_TEXTURE;
  else
    p->sp.mode= MAV_LIT_TEXTURE;
  p->sp.material= material;
  p->sp.texture= texture;

  r1.x= vx[2]-vx[1];
  r1.y= vy[2]-vy[1];
  r1.z= vz[2]-vz[1];
  r2.x= vx[0]-vx[1];
  r2.y= vy[0]-vy[1];
  r2.z= vz[0]-vz[1];
  r1= mav_vectorNormalize(r1);
  r2= mav_vectorNormalize(r2);
  p->normal= mav_vectorCrossProduct(r1,r2);
  p->normal= mav_vectorNormalize(p->normal);

  for (i= 0; i< num; i++)
    {
      p->verts[i].x= sx+vx[num-1-i];
      p->verts[i].y= sy+vy[num-1-i];
      p->verts[i].z= sz+vz[num-1-i];
      if (texture != -1)
	{
	  p->texcoords[i].s= tu[num-1-i];
	  p->texcoords[i].t= tv[num-1-i];
	}
    }
  
  if (num == 3)
    {
      p->verts[3].x= sx+vx[0];
      p->verts[3].y= sx+vy[0];
      p->verts[3].z= sx+vz[0];
      if (texture != -1)
	{
	  p->texcoords[3].s= tu[0];
	  p->texcoords[3].t= tv[0];
	}
    }
}

static void
Add_Billboard
(int texture,
 float px, float py, float pz,
 float width, float height,
 float rotx, float roty, float rotz)
{
  MAV_billboard *bb;

  if (num_billboards >= MAX_BILLBOARDS)
    {
      fprintf(stdout, "Add_Billboard(): maximum number of billboards exceeded (%d)\n", num_billboards);
      return;
    }

  bb= mav_malloc(sizeof(MAV_billboard));

  num_billboards++;
  num_polys++;
  bb->sp.mode= MAV_TEXTURE;
  bb->sp.texture= texture;
  bb->width= width;
  bb->height= height;
  bb->rot.x= rotx;
  bb->rot.y= roty;
  bb->rot.z= rotz;
  bb->bb.min.x= -width/2.0;
  bb->bb.min.y= 0.0;
  bb->bb.min.z= -width/2.0;
  bb->bb.max.x= width/2.0;
  bb->bb.max.y= height;
  bb->bb.max.z= width/2.0;
  bb->matrix= mav_matrixSet (0.0,0.0,0.0, px,py,pz);
  mav_listItemAdd (list_of_objects, (void *)mav_objectNew (mav_class_billboard, bb));
  total_objects++;
}

#if 0
static void
Add_Box
(MAV_cityCell *c, int material,
 float blx, float bly, float blz,
 float trx, float try, float trz)
{
  float x[4],y[4],z[4];

  x[0]= blx; y[0]= bly; z[0]= blz;
  x[1]= trx; y[1]= bly; z[1]= blz;
  x[2]= trx; y[2]= try; z[2]= blz;
  x[3]= blx; y[3]= try; z[3]= blz;
  Add_Poly (c, 4, material,-1, x,y,z, NULL,NULL);

  x[0]= trx; y[0]= bly; z[0]= blz;
  x[1]= trx; y[1]= bly; z[1]= trz;
  x[2]= trx; y[2]= try; z[2]= trz;
  x[3]= trx; y[3]= try; z[3]= blz;
  Add_Poly (c, 4, material,-1, x,y,z, NULL,NULL);

  x[0]= blx; y[0]= bly; z[0]= trz;
  x[1]= blx; y[1]= try; z[1]= trz;
  x[2]= trx; y[2]= try; z[2]= trz;
  x[3]= trx; y[3]= bly; z[3]= trz;
  Add_Poly (c, 4, material,-1, x,y,z, NULL,NULL);

  x[0]= blx; y[0]= bly; z[0]= blz;
  x[1]= blx; y[1]= try; z[1]= blz;
  x[2]= blx; y[2]= try; z[2]= trz;
  x[3]= blx; y[3]= bly; z[3]= trz;
  Add_Poly (c, 4, material,-1, x,y,z, NULL,NULL);

  x[0]= blx; y[0]= try; z[0]= blz;
  x[1]= trx; y[1]= try; z[1]= blz;
  x[2]= trx; y[2]= try; z[2]= trz;
  x[3]= blx; y[3]= try; z[3]= trz;
  Add_Poly (c, 4, material,-1, x,y,z, NULL,NULL);
}
#endif

static void
Add_Pavement
(MAV_cityCell *c, MAV_vector *pos, int type)
{
  float x[4],y[4],z[4];
  float tu[4],tv[4];

  if (type & 1)
    {
      x[0]= pos[2].x; y[0]= PAVE_HEIGHT; z[0]= pos[2].z;
      x[1]= pos[1].x; y[1]= PAVE_HEIGHT; z[1]= pos[1].z;
      x[2]= pos[0].x; y[2]= PAVE_HEIGHT; z[2]= pos[0].z;
      x[3]= c->nw.x; y[3]= PAVE_HEIGHT; z[3]= c->nw.z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[2].x; y[0]= 0.0; z[0]= pos[2].z;
      x[1]= pos[1].x; y[1]= 0.0; z[1]= pos[1].z;
      x[2]= pos[1].x; y[2]= PAVE_HEIGHT; z[2]= pos[1].z;
      x[3]= pos[2].x; y[3]= PAVE_HEIGHT; z[3]= pos[2].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[1].x; y[0]= 0.0; z[0]= pos[1].z;
      x[1]= pos[0].x; y[1]= 0.0; z[1]= pos[0].z;
      x[2]= pos[0].x; y[2]= PAVE_HEIGHT; z[2]= pos[0].z;
      x[3]= pos[1].x; y[3]= PAVE_HEIGHT; z[3]= pos[1].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);
    }

  if (type & 2)
    {
      x[0]= pos[0].x; y[0]= PAVE_HEIGHT; z[0]= pos[0].z;
      x[1]= pos[1].x; y[1]= PAVE_HEIGHT; z[1]= pos[1].z;
      x[2]= pos[4].x; y[2]= PAVE_HEIGHT; z[2]= pos[4].z;
      x[3]= pos[3].x; y[3]= PAVE_HEIGHT; z[3]= pos[3].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[1].x; y[0]= 0.0; z[0]= pos[1].z;
      x[1]= pos[4].x; y[1]= 0.0; z[1]= pos[4].z;
      x[2]= pos[4].x; y[2]= PAVE_HEIGHT; z[2]= pos[4].z;
      x[3]= pos[1].x; y[3]= PAVE_HEIGHT; z[3]= pos[1].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);
    }

  if (type & 4)
    {
      x[0]= pos[3].x; y[0]= PAVE_HEIGHT; z[0]= pos[3].z;
      x[1]= pos[4].x; y[1]= PAVE_HEIGHT; z[1]= pos[4].z;
      x[2]= pos[5].x; y[2]= PAVE_HEIGHT; z[2]= pos[5].z;
      x[3]= c->ne.x; y[3]= PAVE_HEIGHT; z[3]= c->ne.z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[4].x; y[0]= 0.0; z[0]= pos[4].z;
      x[1]= pos[5].x; y[1]= 0.0; z[1]= pos[5].z;
      x[2]= pos[5].x; y[2]= PAVE_HEIGHT; z[2]= pos[5].z;
      x[3]= pos[4].x; y[3]= PAVE_HEIGHT; z[3]= pos[4].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[3].x; y[0]= 0.0; z[0]= pos[3].z;
      x[1]= pos[4].x; y[1]= 0.0; z[1]= pos[4].z;
      x[2]= pos[4].x; y[2]= PAVE_HEIGHT; z[2]= pos[4].z;
      x[3]= pos[3].x; y[3]= PAVE_HEIGHT; z[3]= pos[3].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);
     }

  if (type & 8)
    {
      x[0]= pos[4].x; y[0]= PAVE_HEIGHT; z[0]= pos[4].z;
      x[1]= pos[9].x; y[1]= PAVE_HEIGHT; z[1]= pos[9].z;
      x[2]= pos[10].x; y[2]= PAVE_HEIGHT; z[2]= pos[10].z;
      x[3]= pos[5].x; y[3]= PAVE_HEIGHT; z[3]= pos[5].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[4].x; y[0]= 0.0; z[0]= pos[4].z;
      x[1]= pos[9].x; y[1]= 0.0; z[1]= pos[9].z;
      x[2]= pos[9].x; y[2]= PAVE_HEIGHT; z[2]= pos[9].z;
      x[3]= pos[4].x; y[3]= PAVE_HEIGHT; z[3]= pos[4].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);
    }

  if (type & 16)
    {
      x[0]= pos[10].x; y[0]= PAVE_HEIGHT; z[0]= pos[10].z;
      x[1]= pos[9].x; y[1]= PAVE_HEIGHT; z[1]= pos[9].z;
      x[2]= pos[11].x; y[2]= PAVE_HEIGHT; z[2]= pos[11].z;
      x[3]= c->se.x; y[3]= PAVE_HEIGHT; z[3]= c->se.z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[10].x; y[0]= 0.0; z[0]= pos[10].z;
      x[1]= pos[9].x; y[1]= 0.0; z[1]= pos[9].z;
      x[2]= pos[9].x; y[2]= PAVE_HEIGHT; z[2]= pos[9].z;
      x[3]= pos[10].x; y[3]= PAVE_HEIGHT; z[3]= pos[10].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[9].x; y[0]= 0.0; z[0]= pos[9].z;
      x[1]= pos[11].x; y[1]= 0.0; z[1]= pos[11].z;
      x[2]= pos[11].x; y[2]= PAVE_HEIGHT; z[2]= pos[11].z;
      x[3]= pos[9].x; y[3]= PAVE_HEIGHT; z[3]= pos[9].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);
    }

  if (type & 32)
    {
      x[0]= pos[7].x; y[0]= PAVE_HEIGHT; z[0]= pos[7].z;
      x[1]= pos[8].x; y[1]= PAVE_HEIGHT; z[1]= pos[8].z;
      x[2]= pos[11].x; y[2]= PAVE_HEIGHT; z[2]= pos[11].z;
      x[3]= pos[9].x; y[3]= PAVE_HEIGHT; z[3]= pos[9].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[9].x; y[0]= 0.0; z[0]= pos[9].z;
      x[1]= pos[7].x; y[1]= 0.0; z[1]= pos[7].z;
      x[2]= pos[7].x; y[2]= PAVE_HEIGHT; z[2]= pos[7].z;
      x[3]= pos[9].x; y[3]= PAVE_HEIGHT; z[3]= pos[9].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);
    }

  if (type & 64)
    {
      x[0]= pos[8].x; y[0]= PAVE_HEIGHT; z[0]= pos[8].z;
      x[1]= pos[7].x; y[1]= PAVE_HEIGHT; z[1]= pos[7].z;
      x[2]= pos[6].x; y[2]= PAVE_HEIGHT; z[2]= pos[6].z;
      x[3]= c->sw.x; y[3]= PAVE_HEIGHT; z[3]= c->sw.z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[8].x; y[0]= 0.0; z[0]= pos[8].z;
      x[1]= pos[7].x; y[1]= 0.0; z[1]= pos[7].z;
      x[2]= pos[7].x; y[2]= PAVE_HEIGHT; z[2]= pos[7].z;
      x[3]= pos[8].x; y[3]= PAVE_HEIGHT; z[3]= pos[8].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[7].x; y[0]= 0.0; z[0]= pos[7].z;
      x[1]= pos[6].x; y[1]= 0.0; z[1]= pos[6].z;
      x[2]= pos[6].x; y[2]= PAVE_HEIGHT; z[2]= pos[6].z;
      x[3]= pos[7].x; y[3]= PAVE_HEIGHT; z[3]= pos[7].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);
    }

  if (type & 128)
    {
      x[0]= pos[2].x; y[0]= PAVE_HEIGHT; z[0]= pos[2].z;
      x[1]= pos[6].x; y[1]= PAVE_HEIGHT; z[1]= pos[6].z;
      x[2]= pos[7].x; y[2]= PAVE_HEIGHT; z[2]= pos[7].z;
      x[3]= pos[1].x; y[3]= PAVE_HEIGHT; z[3]= pos[1].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);

      x[0]= pos[7].x; y[0]= 0.0; z[0]= pos[7].z;
      x[1]= pos[1].x; y[1]= 0.0; z[1]= pos[1].z;
      x[2]= pos[1].x; y[2]= PAVE_HEIGHT; z[2]= pos[1].z;
      x[3]= pos[7].x; y[3]= PAVE_HEIGHT; z[3]= pos[7].z;
      tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
      tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
      tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
      tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
      Add_Poly (c, 4, MAT_WHITE,TEX_PAVEMENT, x,y,z, tu,tv);
    }
}

static void
Add_Relative_Box
(float sx, float sy, float sz,
 MAV_cityCell *c, int material, int texture,
 float blx, float bly, float blz,
 float trx, float try, float trz,
 float scale)
{
  float x[4],y[4],z[4];
  float tu[4],tv[4];

  x[0]= sx+blx; y[0]= sy+bly; z[0]= sz+blz;
  x[1]= sx+trx; y[1]= sy+bly; z[1]= sz+blz;
  x[2]= sx+trx; y[2]= sy+try; z[2]= sz+blz;
  x[3]= sx+blx; y[3]= sy+try; z[3]= sz+blz;
  if (texture > -1)
    {
      tu[0]= scale*x[0]; tv[0]= scale*y[0];
      tu[1]= scale*x[1]; tv[1]= scale*y[1];
      tu[2]= scale*x[2]; tv[2]= scale*y[2];
      tu[3]= scale*x[3]; tv[3]= scale*y[3];
    }
  Add_Poly (c, 4, material,texture, x,y,z, tu,tv);

  x[0]= sx+trx; y[0]= sy+bly; z[0]= sz+blz;
  x[1]= sx+trx; y[1]= sy+bly; z[1]= sz+trz;
  x[2]= sx+trx; y[2]= sy+try; z[2]= sz+trz;
  x[3]= sx+trx; y[3]= sy+try; z[3]= sz+blz;
  if (texture > -1)
    {
      tu[0]= scale*z[0]; tv[0]= scale*y[0];
      tu[1]= scale*z[1]; tv[1]= scale*y[1];
      tu[2]= scale*z[2]; tv[2]= scale*y[2];
      tu[3]= scale*z[3]; tv[3]= scale*y[3];
    }
  Add_Poly (c, 4, material,texture, x,y,z, tu,tv);

  x[0]= sx+blx; y[0]= sy+bly; z[0]= sz+trz;
  x[1]= sx+blx; y[1]= sy+try; z[1]= sz+trz;
  x[2]= sx+trx; y[2]= sy+try; z[2]= sz+trz;
  x[3]= sx+trx; y[3]= sy+bly; z[3]= sz+trz;
  if (texture > -1)
    {
      tu[0]= scale*(-x[0]); tv[0]= scale*y[0];
      tu[1]= scale*(-x[1]); tv[1]= scale*y[1];
      tu[2]= scale*(-x[2]); tv[2]= scale*y[2];
      tu[3]= scale*(-x[3]); tv[3]= scale*y[3];
    }
  Add_Poly (c, 4, material,texture, x,y,z, tu,tv);

  x[0]= sx+blx; y[0]= sy+bly; z[0]= sz+blz;
  x[1]= sx+blx; y[1]= sy+try; z[1]= sz+blz;
  x[2]= sx+blx; y[2]= sy+try; z[2]= sz+trz;
  x[3]= sx+blx; y[3]= sy+bly; z[3]= sz+trz;
  if (texture > -1)
    {
      tu[0]= scale*(-z[0]); tv[0]= scale*y[0];
      tu[1]= scale*(-z[1]); tv[1]= scale*y[1];
      tu[2]= scale*(-z[2]); tv[2]= scale*y[2];
      tu[3]= scale*(-z[3]); tv[3]= scale*y[3];
    }
  Add_Poly (c, 4, material,texture, x,y,z, tu,tv);

  x[0]= sx+blx; y[0]= sy+try; z[0]= sz+blz;
  x[1]= sx+trx; y[1]= sy+try; z[1]= sz+blz;
  x[2]= sx+trx; y[2]= sy+try; z[2]= sz+trz;
  x[3]= sx+blx; y[3]= sy+try; z[3]= sz+trz;
  if (texture > -1)
    {
      tu[0]= scale*x[0]; tv[0]= scale*z[0];
      tu[1]= scale*x[1]; tv[1]= scale*z[1];
      tu[2]= scale*x[2]; tv[2]= scale*z[2];
      tu[3]= scale*x[3]; tv[3]= scale*z[3];
    }
  Add_Poly (c, 4, material,texture, x,y,z, tu,tv);

  x[0]= sx+blx; y[0]= sy+bly; z[0]= sz+blz;
  x[1]= sx+blx; y[1]= sy+bly; z[1]= sz+trz;
  x[2]= sx+trx; y[2]= sy+bly; z[2]= sz+trz;
  x[3]= sx+trx; y[3]= sy+bly; z[3]= sz+blz;
  if (texture > -1)
    {
      tu[0]= scale*(-x[0]); tv[0]= scale*z[0];
      tu[1]= scale*(-x[1]); tv[1]= scale*z[1];
      tu[2]= scale*(-x[2]); tv[2]= scale*z[2];
      tu[3]= scale*(-x[3]); tv[3]= scale*z[3];
    }
  Add_Poly (c, 4, material,texture, x,y,z, tu,tv);
}

static void
Add_Relative_Lod_Box
(float sx, float sy, float sz,
 MAV_cityCell *c, int material, int texture,
 float blx, float bly, float blz,
 float trx, float try, float trz,
 float scale)
{
  float x[4],y[4],z[4];
  float tu[4],tv[4];

  x[0]= sx+blx; y[0]= sy+bly; z[0]= sz+blz;
  x[1]= sx+trx; y[1]= sy+bly; z[1]= sz+blz;
  x[2]= sx+trx; y[2]= sy+try; z[2]= sz+blz;
  x[3]= sx+blx; y[3]= sy+try; z[3]= sz+blz;
  if (texture > -1)
    {
      tu[0]= scale*x[0]; tv[0]= scale*y[0];
      tu[1]= scale*x[1]; tv[1]= scale*y[1];
      tu[2]= scale*x[2]; tv[2]= scale*y[2];
      tu[3]= scale*x[3]; tv[3]= scale*y[3];
    }
  Add_Lod_Poly (c, 4, material,texture, x,y,z, tu,tv);

  x[0]= sx+trx; y[0]= sy+bly; z[0]= sz+blz;
  x[1]= sx+trx; y[1]= sy+bly; z[1]= sz+trz;
  x[2]= sx+trx; y[2]= sy+try; z[2]= sz+trz;
  x[3]= sx+trx; y[3]= sy+try; z[3]= sz+blz;
  if (texture > -1)
    {
      tu[0]= scale*z[0]; tv[0]= scale*y[0];
      tu[1]= scale*z[1]; tv[1]= scale*y[1];
      tu[2]= scale*z[2]; tv[2]= scale*y[2];
      tu[3]= scale*z[3]; tv[3]= scale*y[3];
    }
  Add_Lod_Poly (c, 4, material,texture, x,y,z, tu,tv);

  x[0]= sx+blx; y[0]= sy+bly; z[0]= sz+trz;
  x[1]= sx+blx; y[1]= sy+try; z[1]= sz+trz;
  x[2]= sx+trx; y[2]= sy+try; z[2]= sz+trz;
  x[3]= sx+trx; y[3]= sy+bly; z[3]= sz+trz;
  if (texture > -1)
    {
      tu[0]= scale*(-x[0]); tv[0]= scale*y[0];
      tu[1]= scale*(-x[1]); tv[1]= scale*y[1];
      tu[2]= scale*(-x[2]); tv[2]= scale*y[2];
      tu[3]= scale*(-x[3]); tv[3]= scale*y[3];
    }
  Add_Lod_Poly (c, 4, material,texture, x,y,z, tu,tv);

  x[0]= sx+blx; y[0]= sy+bly; z[0]= sz+blz;
  x[1]= sx+blx; y[1]= sy+try; z[1]= sz+blz;
  x[2]= sx+blx; y[2]= sy+try; z[2]= sz+trz;
  x[3]= sx+blx; y[3]= sy+bly; z[3]= sz+trz;
  if (texture > -1)
    {
      tu[0]= scale*(-z[0]); tv[0]= scale*y[0];
      tu[1]= scale*(-z[1]); tv[1]= scale*y[1];
      tu[2]= scale*(-z[2]); tv[2]= scale*y[2];
      tu[3]= scale*(-z[3]); tv[3]= scale*y[3];
    }
  Add_Lod_Poly (c, 4, material,texture, x,y,z, tu,tv);

  x[0]= sx+blx; y[0]= sy+try; z[0]= sz+blz;
  x[1]= sx+trx; y[1]= sy+try; z[1]= sz+blz;
  x[2]= sx+trx; y[2]= sy+try; z[2]= sz+trz;
  x[3]= sx+blx; y[3]= sy+try; z[3]= sz+trz;
  if (texture > -1)
    {
      tu[0]= scale*x[0]; tv[0]= scale*z[0];
      tu[1]= scale*x[1]; tv[1]= scale*z[1];
      tu[2]= scale*x[2]; tv[2]= scale*z[2];
      tu[3]= scale*x[3]; tv[3]= scale*z[3];
    }
  Add_Lod_Poly (c, 4, material,texture, x,y,z, tu,tv);

  x[0]= sx+blx; y[0]= sy+bly; z[0]= sz+blz;
  x[1]= sx+blx; y[1]= sy+bly; z[1]= sz+trz;
  x[2]= sx+trx; y[2]= sy+bly; z[2]= sz+trz;
  x[3]= sx+trx; y[3]= sy+bly; z[3]= sz+blz;
  if (texture > -1)
    {
      tu[0]= scale*(-x[0]); tv[0]= scale*z[0];
      tu[1]= scale*(-x[1]); tv[1]= scale*z[1];
      tu[2]= scale*(-x[2]); tv[2]= scale*z[2];
      tu[3]= scale*(-x[3]); tv[3]= scale*z[3];
    }
  Add_Lod_Poly (c, 4, material,texture, x,y,z, tu,tv);
}

static void
Map_Coords
(MAV_vector cent, MAV_vector dru, MAV_vector drv, float u, float v, float *vx, float *vy, float *vz)
{
  *vx= cent.x+dru.x*u+drv.x*v;
  *vy= DELTA_HEIGHT;
  *vz= cent.z+dru.z*u+drv.z*v;
}

static void
Add_Horizontal_Crossing
(MAV_cityCell *c, MAV_vector *pos)
{
  float vx[4],vy[4],vz[4];
  int i;
  float len;
  MAV_vector c0,c1;
  MAV_vector dru,drv;
  MAV_vector cent;
  float u;

  c0.x= 0.5*(c->nw.x+c->sw.x);
  c0.y= 0.5*(c->nw.y+c->sw.y);
  c0.z= 0.5*(c->nw.z+c->sw.z);
  c1.x= 0.5*(c->se.x+c->ne.x);
  c1.y= 0.5*(c->se.y+c->ne.y);
  c1.z= 0.5*(c->se.z+c->ne.z);

  dru.x= c1.x-c0.x;
  dru.y= c1.y-c0.y;
  dru.z= c1.z-c0.z;
  dru= mav_vectorNormalize(dru);
  drv.x= pos[6].x-pos[2].x;
  drv.y= pos[6].y-pos[2].y;
  drv.z= pos[6].z-pos[2].z;
  drv= mav_vectorNormalize(drv);

  cent.x= 0.5*(pos[1].x+pos[7].x);
  cent.y= 0.5*(pos[1].y+pos[7].y);
  cent.z= 0.5*(pos[1].z+pos[7].z);

  len= 2.0*road_width/10.0;
  u= len/4.0;
  for (i= 0; i< 10; i++)
    {
      Map_Coords (cent, dru,drv, u, 0.5*PAVE_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u+len/2.0, 0.5*PAVE_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+len/2.0, -0.5*PAVE_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u, -0.5*PAVE_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      u += len;
    }
}

static void
Add_Vertical_Crossing
(MAV_cityCell *c, MAV_vector *pos)
{
  float vx[4],vy[4],vz[4];
  int i;
  float len;
  MAV_vector c0,c1;
  MAV_vector dru,drv;
  MAV_vector cent;
  float v;

  c0.x= 0.5*(c->nw.x+c->ne.x);
  c0.y= 0.5*(c->nw.y+c->ne.y);
  c0.z= 0.5*(c->nw.z+c->ne.z);
  c1.x= 0.5*(c->sw.x+c->se.x);
  c1.y= 0.5*(c->sw.y+c->se.y);
  c1.z= 0.5*(c->sw.z+c->se.z);

  dru.x= pos[5].x-pos[2].x;
  dru.y= pos[5].y-pos[2].y;
  dru.z= pos[5].z-pos[2].z;
  dru= mav_vectorNormalize(dru);
  drv.x= c1.x-c0.x;
  drv.y= c1.y-c0.y;
  drv.z= c1.z-c0.z;
  drv= mav_vectorNormalize(drv);

  cent.x= 0.5*(pos[1].x+pos[4].x);
  cent.y= 0.5*(pos[1].y+pos[4].y);
  cent.z= 0.5*(pos[1].z+pos[4].z);

  len= 2.0*road_width/10.0;
  v= len/4.0;
  for (i= 0; i< 10; i++)
    {
      Map_Coords (cent, dru,drv, -0.5*PAVE_WIDTH, v, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, -0.5*PAVE_WIDTH, v+len/2.0, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, 0.5*PAVE_WIDTH, v+len/2.0, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, 0.5*PAVE_WIDTH, v, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      v += len;
    }
}

static void
Add_Vertical_Marks
(MAV_cityCell *c, MAV_vector *pos)
{
  float vx[4],vy[4],vz[4];
  int i;
  float len;
  MAV_vector c0,c1;
  MAV_vector dru,drv;
  MAV_vector cent;
  float v;

  c0.x= 0.5*(c->nw.x+c->ne.x);
  c0.y= 0.5*(c->nw.y+c->ne.y);
  c0.z= 0.5*(c->nw.z+c->ne.z);
  c1.x= 0.5*(c->sw.x+c->se.x);
  c1.y= 0.5*(c->sw.y+c->se.y);
  c1.z= 0.5*(c->sw.z+c->se.z);

  dru.x= pos[5].x-pos[2].x;
  dru.y= pos[5].y-pos[2].y;
  dru.z= pos[5].z-pos[2].z;
  dru= mav_vectorNormalize(dru);
  drv.x= c1.x-c0.x;
  drv.y= c1.y-c0.y;
  drv.z= c1.z-c0.z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  v= len/4.0;
  for (i= 0; i< 4; i++)
    {
      Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v+len/2.0, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v+len/2.0, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      v += len;
    }
}

static void
Add_Horizontal_Marks
(MAV_cityCell *c, MAV_vector *pos)
{
  float vx[4],vy[4],vz[4];
  int i;
  float len;
  MAV_vector c0,c1;
  MAV_vector dru,drv;
  MAV_vector cent;
  float u;

  c0.x= 0.5*(c->nw.x+c->sw.x);
  c0.y= 0.5*(c->nw.y+c->sw.y);
  c0.z= 0.5*(c->nw.z+c->sw.z);
  c1.x= 0.5*(c->se.x+c->ne.x);
  c1.y= 0.5*(c->se.y+c->ne.y);
  c1.z= 0.5*(c->se.z+c->ne.z);

  dru.x= c1.x-c0.x;
  dru.y= c1.y-c0.y;
  dru.z= c1.z-c0.z;
  dru= mav_vectorNormalize(dru);
  drv.x= pos[6].x-pos[2].x;
  drv.y= pos[6].y-pos[2].y;
  drv.z= pos[6].z-pos[2].z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  u= len/4.0;
  for (i= 0; i< 4; i++)
    {
      Map_Coords (cent, dru,drv, u, 0.5*MARK_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u+len/2.0, 0.5*MARK_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+len/2.0, -0.5*MARK_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u, -0.5*MARK_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      u += len;
    }
}

static void
Add_Vertical_CMarks
(MAV_cityCell *c, MAV_vector *pos)
{
  float vx[4],vy[4],vz[4];
  int i;
  float len;
  MAV_vector c0,c1;
  MAV_vector dru,drv;
  MAV_vector cent;
  float v;

  c0.x= 0.5*(c->nw.x+c->ne.x);
  c0.y= 0.5*(c->nw.y+c->ne.y);
  c0.z= 0.5*(c->nw.z+c->ne.z);
  c1.x= 0.5*(c->sw.x+c->se.x);
  c1.y= 0.5*(c->sw.y+c->se.y);
  c1.z= 0.5*(c->sw.z+c->se.z);

  dru.x= pos[5].x-pos[2].x;
  dru.y= pos[5].y-pos[2].y;
  dru.z= pos[5].z-pos[2].z;
  dru= mav_vectorNormalize(dru);
  drv.x= c1.x-c0.x;
  drv.y= c1.y-c0.y;
  drv.z= c1.z-c0.z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  v= len/4.0;
  for (i= 0; i< 4; i++)
    {
      if (i == 0 || i == 3)
	{
	  Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v, &vx[0],&vy[0],&vz[0]);
	  Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v+len/2.0, &vx[1],&vy[1],&vz[1]);
	  Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v+len/2.0, &vx[2],&vy[2],&vz[2]);
	  Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v, &vx[3],&vy[3],&vz[3]);
	  Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);
	}

      v += len;
    }
}

static void
Add_Horizontal_CMarks
(MAV_cityCell *c, MAV_vector *pos)
{
  float vx[4],vy[4],vz[4];
  int i;
  float len;
  MAV_vector c0,c1;
  MAV_vector dru,drv;
  MAV_vector cent;
  float u;

  c0.x= 0.5*(c->nw.x+c->sw.x);
  c0.y= 0.5*(c->nw.y+c->sw.y);
  c0.z= 0.5*(c->nw.z+c->sw.z);
  c1.x= 0.5*(c->se.x+c->ne.x);
  c1.y= 0.5*(c->se.y+c->ne.y);
  c1.z= 0.5*(c->se.z+c->ne.z);

  dru.x= c1.x-c0.x;
  dru.y= c1.y-c0.y;
  dru.z= c1.z-c0.z;
  dru= mav_vectorNormalize(dru);
  drv.x= pos[6].x-pos[2].x;
  drv.y= pos[6].y-pos[2].y;
  drv.z= pos[6].z-pos[2].z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  u= len/4.0;
  for (i= 0; i< 4; i++)
    {
      if (i == 0 || i == 3)
	{
	  Map_Coords (cent, dru,drv, u, 0.5*MARK_WIDTH, &vx[0],&vy[0],&vz[0]);
	  Map_Coords (cent, dru,drv, u+len/2.0, 0.5*MARK_WIDTH, &vx[1],&vy[1],&vz[1]);
	  Map_Coords (cent, dru,drv, u+len/2.0, -0.5*MARK_WIDTH, &vx[2],&vy[2],&vz[2]);
	  Map_Coords (cent, dru,drv, u, -0.5*MARK_WIDTH, &vx[3],&vy[3],&vz[3]);
	  Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);
	}

      u += len;
    }
}

static void
Add_West_North_Marks
(MAV_cityCell *c, MAV_vector *pos)
{
  float vx[4],vy[4],vz[4];
  int i;
  float len;
  MAV_vector c0,c1;
  MAV_vector dru,drv;
  MAV_vector cent;
  float u,v;

  c0.x= 0.5*(c->nw.x+c->ne.x);
  c0.y= 0.5*(c->nw.y+c->ne.y);
  c0.z= 0.5*(c->nw.z+c->ne.z);
  c1.x= 0.5*(c->sw.x+c->se.x);
  c1.y= 0.5*(c->sw.y+c->se.y);
  c1.z= 0.5*(c->sw.z+c->se.z);

  dru.x= pos[5].x-pos[2].x;
  dru.y= pos[5].y-pos[2].y;
  dru.z= pos[5].z-pos[2].z;
  dru= mav_vectorNormalize(dru);
  drv.x= c1.x-c0.x;
  drv.y= c1.y-c0.y;
  drv.z= c1.z-c0.z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  v= len/4.0;
  for (i= 0; i< 2; i++)
    {
      Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v+len/2.0, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v+len/2.0, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      v += len;
    }

  c0.x= 0.5*(c->nw.x+c->sw.x);
  c0.y= 0.5*(c->nw.y+c->sw.y);
  c0.z= 0.5*(c->nw.z+c->sw.z);
  c1.x= 0.5*(c->se.x+c->ne.x);
  c1.y= 0.5*(c->se.y+c->ne.y);
  c1.z= 0.5*(c->se.z+c->ne.z);

  dru.x= c1.x-c0.x;
  dru.y= c1.y-c0.y;
  dru.z= c1.z-c0.z;
  dru= mav_vectorNormalize(dru);
  drv.x= pos[6].x-pos[2].x;
  drv.y= pos[6].y-pos[2].y;
  drv.z= pos[6].z-pos[2].z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  u= len/4.0;
  for (i= 0; i< 2; i++)
    {
      Map_Coords (cent, dru,drv, u, 0.5*MARK_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u+len/2.0, 0.5*MARK_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+len/2.0, -0.5*MARK_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u, -0.5*MARK_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      u += len;
    }
}

static void
Add_East_North_Marks
(MAV_cityCell *c, MAV_vector *pos)
{
  float vx[4],vy[4],vz[4];
  int i;
  float len;
  MAV_vector c0,c1;
  MAV_vector dru,drv;
  MAV_vector cent;
  float u,v;

  c0.x= 0.5*(c->nw.x+c->ne.x);
  c0.y= 0.5*(c->nw.y+c->ne.y);
  c0.z= 0.5*(c->nw.z+c->ne.z);
  c1.x= 0.5*(c->sw.x+c->se.x);
  c1.y= 0.5*(c->sw.y+c->se.y);
  c1.z= 0.5*(c->sw.z+c->se.z);

  dru.x= pos[5].x-pos[2].x;
  dru.y= pos[5].y-pos[2].y;
  dru.z= pos[5].z-pos[2].z;
  dru= mav_vectorNormalize(dru);
  drv.x= c1.x-c0.x;
  drv.y= c1.y-c0.y;
  drv.z= c1.z-c0.z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  v= len/4.0;
  for (i= 0; i< 2; i++)
    {
      Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v+len/2.0, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v+len/2.0, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      v += len;
    }

  c0.x= 0.5*(c->nw.x+c->sw.x);
  c0.y= 0.5*(c->nw.y+c->sw.y);
  c0.z= 0.5*(c->nw.z+c->sw.z);
  c1.x= 0.5*(c->se.x+c->ne.x);
  c1.y= 0.5*(c->se.y+c->ne.y);
  c1.z= 0.5*(c->se.z+c->ne.z);

  dru.x= c1.x-c0.x;
  dru.y= c1.y-c0.y;
  dru.z= c1.z-c0.z;
  dru= mav_vectorNormalize(dru);
  drv.x= pos[6].x-pos[2].x;
  drv.y= pos[6].y-pos[2].y;
  drv.z= pos[6].z-pos[2].z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  u= 2.0*len+len/4.0;
  for (i= 0; i< 2; i++)
    {
      Map_Coords (cent, dru,drv, u, 0.5*MARK_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u+len/2.0, 0.5*MARK_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+len/2.0, -0.5*MARK_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u, -0.5*MARK_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      u += len;
    }
}

static void
Add_West_South_Marks
(MAV_cityCell *c, MAV_vector *pos)
{
  float vx[4],vy[4],vz[4];
  int i;
  float len;
  MAV_vector c0,c1;
  MAV_vector dru,drv;
  MAV_vector cent;
  float u,v;

  c0.x= 0.5*(c->nw.x+c->ne.x);
  c0.y= 0.5*(c->nw.y+c->ne.y);
  c0.z= 0.5*(c->nw.z+c->ne.z);
  c1.x= 0.5*(c->sw.x+c->se.x);
  c1.y= 0.5*(c->sw.y+c->se.y);
  c1.z= 0.5*(c->sw.z+c->se.z);

  dru.x= pos[5].x-pos[2].x;
  dru.y= pos[5].y-pos[2].y;
  dru.z= pos[5].z-pos[2].z;
  dru= mav_vectorNormalize(dru);
  drv.x= c1.x-c0.x;
  drv.y= c1.y-c0.y;
  drv.z= c1.z-c0.z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  v= 2.0*len+len/4.0;
  for (i= 0; i< 2; i++)
    {
      Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v+len/2.0, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v+len/2.0, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      v += len;
    }

  c0.x= 0.5*(c->nw.x+c->sw.x);
  c0.y= 0.5*(c->nw.y+c->sw.y);
  c0.z= 0.5*(c->nw.z+c->sw.z);
  c1.x= 0.5*(c->se.x+c->ne.x);
  c1.y= 0.5*(c->se.y+c->ne.y);
  c1.z= 0.5*(c->se.z+c->ne.z);

  dru.x= c1.x-c0.x;
  dru.y= c1.y-c0.y;
  dru.z= c1.z-c0.z;
  dru= mav_vectorNormalize(dru);
  drv.x= pos[6].x-pos[2].x;
  drv.y= pos[6].y-pos[2].y;
  drv.z= pos[6].z-pos[2].z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  u= len/4.0;
  for (i= 0; i< 2; i++)
    {
      Map_Coords (cent, dru,drv, u, 0.5*MARK_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u+len/2.0, 0.5*MARK_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+len/2.0, -0.5*MARK_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u, -0.5*MARK_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      u += len;
    }
}

static void
Add_East_South_Marks
(MAV_cityCell *c, MAV_vector *pos)
{
  float vx[4],vy[4],vz[4];
  int i;
  float len;
  MAV_vector c0,c1;
  MAV_vector dru,drv;
  MAV_vector cent;
  float u,v;

  c0.x= 0.5*(c->nw.x+c->ne.x);
  c0.y= 0.5*(c->nw.y+c->ne.y);
  c0.z= 0.5*(c->nw.z+c->ne.z);
  c1.x= 0.5*(c->sw.x+c->se.x);
  c1.y= 0.5*(c->sw.y+c->se.y);
  c1.z= 0.5*(c->sw.z+c->se.z);

  dru.x= pos[5].x-pos[2].x;
  dru.y= pos[5].y-pos[2].y;
  dru.z= pos[5].z-pos[2].z;
  dru= mav_vectorNormalize(dru);
  drv.x= c1.x-c0.x;
  drv.y= c1.y-c0.y;
  drv.z= c1.z-c0.z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  v= 2.0*len+len/4.0;
  for (i= 0; i< 2; i++)
    {
      Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, v+len/2.0, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v+len/2.0, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, v, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      v += len;
    }

  c0.x= 0.5*(c->nw.x+c->sw.x);
  c0.y= 0.5*(c->nw.y+c->sw.y);
  c0.z= 0.5*(c->nw.z+c->sw.z);
  c1.x= 0.5*(c->se.x+c->ne.x);
  c1.y= 0.5*(c->se.y+c->ne.y);
  c1.z= 0.5*(c->se.z+c->ne.z);

  dru.x= c1.x-c0.x;
  dru.y= c1.y-c0.y;
  dru.z= c1.z-c0.z;
  dru= mav_vectorNormalize(dru);
  drv.x= pos[6].x-pos[2].x;
  drv.y= pos[6].y-pos[2].y;
  drv.z= pos[6].z-pos[2].z;
  drv= mav_vectorNormalize(drv);

  cent= c0;

  len= sqrt((c0.x-c1.x)*(c0.x-c1.x)+(c0.y-c1.y)*(c0.y-c1.y)+(c0.z-c1.z)*(c0.z-c1.z))/4.0;
  u= 2.0*len+len/4.0;
  for (i= 0; i< 2; i++)
    {
      Map_Coords (cent, dru,drv, u, 0.5*MARK_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u+len/2.0, 0.5*MARK_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+len/2.0, -0.5*MARK_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u, -0.5*MARK_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      u += len;
    }
}

static void
Add_South_Junction
(MAV_cityCell *c, MAV_vector *pos, int lgt)
{
  float vx[4],vy[4],vz[4];
  int i;
  MAV_matrix transform,shift;
  float len;
  MAV_vector dru,drv;
  MAV_vector cent;
  float u;
  float angle;

  dru.x= pos[10].x-pos[6].x;
  dru.y= pos[10].y-pos[6].y;
  dru.z= pos[10].z-pos[6].z;
  dru= mav_vectorNormalize(dru);
  drv.x= 0.5*(c->sw.x+c->se.x)-0.5*(c->nw.x+c->ne.x);
  drv.y= 0.5*(c->sw.y+c->se.y)-0.5*(c->nw.y+c->ne.y);
  drv.z= 0.5*(c->sw.z+c->se.z)-0.5*(c->nw.z+c->ne.z);
  drv= mav_vectorNormalize(drv);

  cent.x= 0.5*(pos[9].x+pos[7].x);
  cent.y= 0.5*(pos[9].y+pos[7].y);
  cent.z= 0.5*(pos[9].z+pos[7].z);

  len= road_width/3.0;
  u= 0.5*MARK_WIDTH;
  for (i= 0; i< 3; i++)
    {
      Map_Coords (cent, dru,drv, u, 0.0, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u, JUNC_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, JUNC_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 0.0, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      Map_Coords (cent, dru,drv, u, 3.0*JUNC_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u, 4.0*JUNC_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 4.0*JUNC_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 3.0*JUNC_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      Map_Coords (cent, dru,drv, -u, JUNC_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, -u, 0.0, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, -(u+2.0*len/3.0), 0.0, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, -(u+2.0*len/3.0), JUNC_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      u += len;
    }

  Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, 0.0, &vx[0],&vy[0],&vz[0]);
  Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, MARK_LENGTH, &vx[1],&vy[1],&vz[1]);
  Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, MARK_LENGTH, &vx[2],&vy[2],&vz[2]);
  Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, 0.0, &vx[3],&vy[3],&vz[3]);
  Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

  if (lgt || mav_random() < 0.5)
    {
#ifdef COMPOSITES
#ifndef MAV_GL
      angle= 180.0/MAV_PI*acos(dru.x);

      transform= mav_matrixSet (0.0,0.0,angle, c->cent.x,0.0,c->cent.z);
      shift= mav_matrixSet (0.0,0.0,0.0, pos[9].x+0.5, 0.0, pos[9].z-0.5);
      transform= mav_matrixMult (shift, transform);
      Add_Composite (c, COMP_TRAFFICLIGHT, &transform);
#endif
#endif
    }
}

static void
Add_North_Junction
(MAV_cityCell *c, MAV_vector *pos, int lgt)
{
  float vx[4],vy[4],vz[4];
  int i;
  MAV_matrix transform,shift;
  float len;
  MAV_vector dru,drv;
  MAV_vector cent;
  float u;
  float angle;

  dru.x= pos[2].x-pos[5].x;
  dru.y= pos[2].y-pos[5].y;
  dru.z= pos[2].z-pos[5].z;
  dru= mav_vectorNormalize(dru);
  drv.x= 0.5*(c->nw.x+c->ne.x)-0.5*(c->sw.x+c->se.x);
  drv.y= 0.5*(c->nw.y+c->ne.y)-0.5*(c->sw.y+c->se.y);
  drv.z= 0.5*(c->nw.z+c->ne.z)-0.5*(c->sw.z+c->se.z);
  drv= mav_vectorNormalize(drv);

  cent.x= 0.5*(pos[1].x+pos[4].x);
  cent.y= 0.5*(pos[1].y+pos[4].y);
  cent.z= 0.5*(pos[1].z+pos[4].z);

  len= road_width/3.0;
  u= 0.5*MARK_WIDTH;
  for (i= 0; i< 3; i++)
    {
      Map_Coords (cent, dru,drv, u, 0.0, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u, JUNC_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, JUNC_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 0.0, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      Map_Coords (cent, dru,drv, u, 3.0*JUNC_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u, 4.0*JUNC_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 4.0*JUNC_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 3.0*JUNC_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      Map_Coords (cent, dru,drv, -u, JUNC_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, -u, 0.0, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, -(u+2.0*len/3.0), 0.0, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, -(u+2.0*len/3.0), JUNC_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      u += len;
    }

  Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, 0.0, &vx[0],&vy[0],&vz[0]);
  Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, MARK_LENGTH, &vx[1],&vy[1],&vz[1]);
  Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, MARK_LENGTH, &vx[2],&vy[2],&vz[2]);
  Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, 0.0, &vx[3],&vy[3],&vz[3]);
  Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

  if (lgt || mav_random() < 0.5)
    {
#ifdef COMPOSITES
#ifndef MAV_GL
      angle= 180.0/MAV_PI*acos(dru.x);

      transform= mav_matrixSet (0.0,0.0,angle, c->cent.x,0.0,c->cent.z);
      shift= mav_matrixSet (0.0,0.0,0.0, pos[1].x-0.5, 0.0, pos[1].z+0.5);
      transform= mav_matrixMult (shift, transform);
      Add_Composite (c, COMP_TRAFFICLIGHT, &transform);
#endif
#endif
    }
}

static void
Add_West_Junction
(MAV_cityCell *c, MAV_vector *pos, int lgt)
{
  float vx[4],vy[4],vz[4];
  int i;
  MAV_matrix transform,shift;
  float len;
  MAV_vector dru,drv;
  MAV_vector cent;
  float u;
  float angle;

  dru.x= pos[8].x-pos[0].x;
  dru.y= pos[8].y-pos[0].y;
  dru.z= pos[8].z-pos[0].z;
  dru= mav_vectorNormalize(dru);
  drv.x= 0.5*(c->nw.x+c->sw.x)-0.5*(c->ne.x+c->se.x);
  drv.y= 0.5*(c->nw.y+c->sw.y)-0.5*(c->ne.y+c->se.y);
  drv.z= 0.5*(c->nw.z+c->sw.z)-0.5*(c->ne.z+c->se.z);
  drv= mav_vectorNormalize(drv);

  cent.x= 0.5*(pos[1].x+pos[7].x);
  cent.y= 0.5*(pos[1].y+pos[7].y);
  cent.z= 0.5*(pos[1].z+pos[7].z);

  len= road_width/3.0;
  u= 0.5*MARK_WIDTH;
  for (i= 0; i< 3; i++)
    {
      Map_Coords (cent, dru,drv, u, 0.0, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u, JUNC_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, JUNC_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 0.0, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      Map_Coords (cent, dru,drv, u, 3.0*JUNC_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u, 4.0*JUNC_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 4.0*JUNC_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 3.0*JUNC_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      Map_Coords (cent, dru,drv, -u, JUNC_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, -u, 0.0, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, -(u+2.0*len/3.0), 0.0, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, -(u+2.0*len/3.0), JUNC_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      u += len;
    }

  Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, 0.0, &vx[0],&vy[0],&vz[0]);
  Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, MARK_LENGTH, &vx[1],&vy[1],&vz[1]);
  Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, MARK_LENGTH, &vx[2],&vy[2],&vz[2]);
  Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, 0.0, &vx[3],&vy[3],&vz[3]);
  Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

  if (lgt || mav_random() < 0.5)
    {
#ifdef COMPOSITES
#ifndef MAV_GL
      angle= 180.0/MAV_PI*acos(dru.x);

      transform= mav_matrixSet (0.0,0.0,-angle, c->cent.x,0.0,c->cent.z);
      shift= mav_matrixSet (0.0,0.0,0.0, pos[7].x-0.5, 0.0, pos[7].z-0.5);
      transform= mav_matrixMult (shift, transform);
      Add_Composite (c, COMP_TRAFFICLIGHT, &transform);
#endif
#endif
    }
}

static void
Add_East_Junction
(MAV_cityCell *c, MAV_vector *pos, int lgt)
{
  float vx[4],vy[4],vz[4];
  int i;
  MAV_matrix transform,shift;
  float len;
  MAV_vector dru,drv;
  MAV_vector cent;
  float u;
  float angle;

  dru.x= pos[3].x-pos[11].x;
  dru.y= pos[3].y-pos[11].y;
  dru.z= pos[3].z-pos[11].z;
  dru= mav_vectorNormalize(dru);
  drv.x= 0.5*(c->ne.x+c->se.x)-0.5*(c->nw.x+c->sw.x);
  drv.y= 0.5*(c->ne.y+c->se.y)-0.5*(c->nw.y+c->sw.y);
  drv.z= 0.5*(c->ne.z+c->se.z)-0.5*(c->nw.z+c->sw.z);
  drv= mav_vectorNormalize(drv);

  cent.x= 0.5*(pos[4].x+pos[9].x);
  cent.y= 0.5*(pos[4].y+pos[9].y);
  cent.z= 0.5*(pos[4].z+pos[9].z);

  len= road_width/3.0;
  u= 0.5*MARK_WIDTH;
  for (i= 0; i< 3; i++)
    {
      Map_Coords (cent, dru,drv, u, 0.0, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u, JUNC_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, JUNC_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 0.0, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      Map_Coords (cent, dru,drv, u, 3.0*JUNC_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, u, 4.0*JUNC_WIDTH, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 4.0*JUNC_WIDTH, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, u+2.0*len/3.0, 3.0*JUNC_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      Map_Coords (cent, dru,drv, -u, JUNC_WIDTH, &vx[0],&vy[0],&vz[0]);
      Map_Coords (cent, dru,drv, -u, 0.0, &vx[1],&vy[1],&vz[1]);
      Map_Coords (cent, dru,drv, -(u+2.0*len/3.0), 0.0, &vx[2],&vy[2],&vz[2]);
      Map_Coords (cent, dru,drv, -(u+2.0*len/3.0), JUNC_WIDTH, &vx[3],&vy[3],&vz[3]);
      Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

      u += len;
    }

  Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, 0.0, &vx[0],&vy[0],&vz[0]);
  Map_Coords (cent, dru,drv, -0.5*MARK_WIDTH, MARK_LENGTH, &vx[1],&vy[1],&vz[1]);
  Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, MARK_LENGTH, &vx[2],&vy[2],&vz[2]);
  Map_Coords (cent, dru,drv, 0.5*MARK_WIDTH, 0.0, &vx[3],&vy[3],&vz[3]);
  Add_Poly (c, 4, MAT_MARK,-1, vx,vy,vz, NULL,NULL);

  if (lgt || mav_random() < 0.5)
    {
#ifdef COMPOSITES
#ifndef MAV_GL
      angle= 180.0/MAV_PI*acos(dru.x);

      transform= mav_matrixSet (0.0,0.0,angle, c->cent.x,0.0,c->cent.z);
      shift= mav_matrixSet (0.0,0.0,0.0, pos[4].x+0.5, 0.0, pos[4].z+0.5);
      transform= mav_matrixMult (shift, transform);
      Add_Composite (c, COMP_TRAFFICLIGHT, &transform);
#endif
#endif
    }
}

static void
Add_Top_Lights
(MAV_cityCell *c, MAV_vector *pos)
{
  MAV_matrix transform, shift;
  MAV_vector cent, drv;
  float angle;

  cent.x= 0.5*(pos[2].x+pos[5].x);
  cent.y= 0.5*(pos[2].y+pos[5].y);
  cent.z= 0.5*(pos[2].z+pos[5].z);

  drv.x= 0.5*(c->sw.x+c->se.x)-0.5*(c->nw.x+c->ne.x);
  drv.y= 0.5*(c->sw.y+c->se.y)-0.5*(c->nw.y+c->ne.y);
  drv.z= 0.5*(c->sw.z+c->se.z)-0.5*(c->nw.z+c->ne.z);
  drv= mav_vectorNormalize(drv);

  angle= 180.0/MAV_PI*acos(drv.x);

#ifdef COMPOSITES
  transform= mav_matrixSet (0.0,0.0,angle, c->cent.x,0.0,c->cent.z);
  shift= mav_matrixSet (0.0,0.0,0.0, cent.x,0.0,cent.z);
  transform= mav_matrixMult (shift, transform);
  Add_Composite (c, COMP_STREETLIGHT, &transform);
#endif
}

static void
Add_Bottom_Lights
(MAV_cityCell *c, MAV_vector *pos)
{
  MAV_matrix transform, shift;
  MAV_vector cent, drv;
  float angle;

  cent.x= 0.5*(pos[10].x+pos[6].x);
  cent.y= 0.5*(pos[10].y+pos[6].y);
  cent.z= 0.5*(pos[10].z+pos[6].z);

  drv.x= 0.5*(c->nw.x+c->ne.x)-0.5*(c->sw.x+c->se.x);
  drv.y= 0.5*(c->nw.y+c->ne.y)-0.5*(c->sw.y+c->se.y);
  drv.z= 0.5*(c->nw.z+c->ne.z)-0.5*(c->sw.z+c->se.z);
  drv= mav_vectorNormalize(drv);

  angle= 180.0/MAV_PI*acos(drv.x);

#ifdef COMPOSITES
  transform= mav_matrixSet (0.0,0.0,-angle, c->cent.x,0.0,c->cent.z);
  shift= mav_matrixSet (0.0,0.0,0.0, cent.x,0.0,cent.z);
  transform= mav_matrixMult (shift, transform);
  Add_Composite (c, COMP_STREETLIGHT, &transform);
#endif
}

static void
Add_Left_Lights
(MAV_cityCell *c, MAV_vector *pos)
{
  MAV_matrix transform, shift;
  MAV_vector cent, drv;
  float angle;

  cent.x= 0.5*(pos[0].x+pos[8].x);
  cent.y= 0.5*(pos[0].y+pos[8].y);
  cent.z= 0.5*(pos[0].z+pos[8].z);

  drv.x= 0.5*(c->nw.x+c->sw.x)-0.5*(c->ne.x+c->se.x);
  drv.y= 0.5*(c->nw.y+c->sw.y)-0.5*(c->ne.y+c->se.y);
  drv.z= 0.5*(c->nw.z+c->sw.z)-0.5*(c->ne.z+c->se.z);
  drv= mav_vectorNormalize(drv);

  angle= 180.0/MAV_PI*acos(drv.x);

#ifdef COMPOSITES
  transform= mav_matrixSet (0.0,0.0,-angle, c->cent.x,0.0,c->cent.z);
  shift= mav_matrixSet (0.0,0.0,0.0, cent.x,0.0,cent.z);
  transform= mav_matrixMult (shift, transform);
  Add_Composite (c, COMP_STREETLIGHT, &transform);
#endif
}

static void
Add_Right_Lights
(MAV_cityCell *c, MAV_vector *pos)
{
  MAV_matrix transform, shift;
  MAV_vector cent, drv;
  float angle;

  cent.x= 0.5*(pos[11].x+pos[3].x);
  cent.y= 0.5*(pos[11].y+pos[3].y);
  cent.z= 0.5*(pos[11].z+pos[3].z);

  drv.x= 0.5*(c->ne.x+c->se.x)-0.5*(c->nw.x+c->sw.x);
  drv.y= 0.5*(c->ne.y+c->se.y)-0.5*(c->nw.y+c->sw.y);
  drv.z= 0.5*(c->ne.z+c->se.z)-0.5*(c->nw.z+c->sw.z);
  drv= mav_vectorNormalize(drv);

  angle= 180.0/MAV_PI*acos(drv.x);

#ifdef COMPOSITES
  transform= mav_matrixSet (0.0,0.0,-angle, c->cent.x,0.0,c->cent.z);
  shift= mav_matrixSet (0.0,0.0,0.0, cent.x,0.0,cent.z);
  transform= mav_matrixMult (shift, transform);
  Add_Composite (c, COMP_STREETLIGHT, &transform);
#endif
}

static void
Generate_Waste
(MAV_cityCell *c)
{
  float x[4],y[4],z[4];
  float tu[4],tv[4];
 
  x[0]= c->nw.x; y[0]= PAVE_HEIGHT; z[0]= c->nw.z;
  x[1]= c->sw.x; y[1]= PAVE_HEIGHT; z[1]= c->sw.z;
  x[2]= c->se.x; y[2]= PAVE_HEIGHT; z[2]= c->se.z;
  x[3]= c->ne.x; y[3]= PAVE_HEIGHT; z[3]= c->ne.z;
  tu[0]= 0.0625*(x[0]+c->cent.x); tv[0]= 0.0625*(z[0]+c->cent.z);
  tu[1]= 0.0625*(x[1]+c->cent.x); tv[1]= 0.0625*(z[1]+c->cent.z);
  tu[2]= 0.0625*(x[2]+c->cent.x); tv[2]= 0.0625*(z[2]+c->cent.z);
  tu[3]= 0.0625*(x[3]+c->cent.x); tv[3]= 0.0625*(z[3]+c->cent.z);
#ifdef MAV_GL
  Add_Poly (c, 4, MAT_DIRT,-1, x,y,z, tu,tv);
#else
  Add_Poly (c, 4, MAT_WHITE,TEX_DIRT, x,y,z, tu,tv);
#endif
}

/* calculate corner positions for roads */
static void
Map
(MAV_cityCell *c, MAV_vector *pos)
{
  MAV_vector dr0,dr1,dr2,dr3;

  dr0.x= c->nw.x-0.5*(c->nw.x+c->ne.x);
  dr0.y= c->nw.y-0.5*(c->nw.y+c->ne.y);
  dr0.z= c->nw.z-0.5*(c->nw.z+c->ne.z);
  dr0= mav_vectorNormalize(dr0);
  dr1.x= c->sw.x-0.5*(c->nw.x+c->sw.x);
  dr1.y= c->sw.y-0.5*(c->nw.y+c->sw.y);
  dr1.z= c->sw.z-0.5*(c->nw.z+c->sw.z);
  dr1= mav_vectorNormalize(dr1);
  dr2.x= c->se.x-0.5*(c->sw.x+c->se.x);
  dr2.y= c->se.y-0.5*(c->sw.y+c->se.y);
  dr2.z= c->se.z-0.5*(c->sw.z+c->se.z);
  dr2= mav_vectorNormalize(dr2);
  dr3.x= c->ne.x-0.5*(c->se.x+c->ne.x);
  dr3.y= c->ne.y-0.5*(c->se.y+c->ne.y);
  dr3.z= c->ne.z-0.5*(c->se.z+c->ne.z);
  dr3= mav_vectorNormalize(dr3);

  pos[0].x= 0.5*(c->nw.x+c->ne.x)+road_width*dr0.x;
  pos[0].y= 0.5*(c->nw.y+c->ne.y)+road_width*dr0.y;
  pos[0].z= 0.5*(c->nw.z+c->ne.z)+road_width*dr0.z;
  pos[3].x= 0.5*(c->nw.x+c->ne.x)-road_width*dr0.x;
  pos[3].y= 0.5*(c->nw.y+c->ne.y)-road_width*dr0.y;
  pos[3].z= 0.5*(c->nw.z+c->ne.z)-road_width*dr0.z;

  pos[2].x= 0.5*(c->nw.x+c->sw.x)-road_width*dr1.x;
  pos[2].y= 0.5*(c->nw.y+c->sw.y)-road_width*dr1.y;
  pos[2].z= 0.5*(c->nw.z+c->sw.z)-road_width*dr1.z;
  pos[6].x= 0.5*(c->nw.x+c->sw.x)+road_width*dr1.x;
  pos[6].y= 0.5*(c->nw.y+c->sw.y)+road_width*dr1.y;
  pos[6].z= 0.5*(c->nw.z+c->sw.z)+road_width*dr1.z;

  pos[8].x= 0.5*(c->se.x+c->sw.x)-road_width*dr2.x;
  pos[8].y= 0.5*(c->se.y+c->sw.y)-road_width*dr2.y;
  pos[8].z= 0.5*(c->se.z+c->sw.z)-road_width*dr2.z;
  pos[11].x= 0.5*(c->se.x+c->sw.x)+road_width*dr2.x;
  pos[11].y= 0.5*(c->se.y+c->sw.y)+road_width*dr2.y;
  pos[11].z= 0.5*(c->se.z+c->sw.z)+road_width*dr2.z;

  pos[5].x= 0.5*(c->se.x+c->ne.x)+road_width*dr3.x;
  pos[5].y= 0.5*(c->se.y+c->ne.y)+road_width*dr3.y;
  pos[5].z= 0.5*(c->se.z+c->ne.z)+road_width*dr3.z;
  pos[10].x= 0.5*(c->se.x+c->ne.x)-road_width*dr3.x;
  pos[10].y= 0.5*(c->se.y+c->ne.y)-road_width*dr3.y;
  pos[10].z= 0.5*(c->se.z+c->ne.z)-road_width*dr3.z;

  dr0.x= pos[2].x-0.5*(pos[5].x+pos[2].x);
  dr0.y= pos[2].y-0.5*(pos[5].y+pos[2].y);
  dr0.z= pos[2].z-0.5*(pos[5].z+pos[2].z);
  dr0= mav_vectorNormalize(dr0);
  dr1.x= pos[8].x-0.5*(pos[8].x+pos[0].x);
  dr1.y= pos[8].y-0.5*(pos[8].y+pos[0].y);
  dr1.z= pos[8].z-0.5*(pos[8].z+pos[0].z);
  dr1= mav_vectorNormalize(dr1);
  dr2.x= pos[10].x-0.5*(pos[10].x+pos[6].x);
  dr2.y= pos[10].y-0.5*(pos[10].y+pos[6].y);
  dr2.z= pos[10].z-0.5*(pos[10].z+pos[6].z);
  dr2= mav_vectorNormalize(dr2);
  dr3.x= pos[3].x-0.5*(pos[11].x+pos[3].x);
  dr3.y= pos[3].y-0.5*(pos[11].y+pos[3].y);
  dr3.z= pos[3].z-0.5*(pos[11].z+pos[3].z);
  dr3= mav_vectorNormalize(dr3);

  pos[1].x= 0.5*(pos[2].x+pos[5].x)+road_width*dr0.x;
  pos[1].y= 0.5*(pos[2].y+pos[5].y)+road_width*dr0.y;
  pos[1].z= 0.5*(pos[2].z+pos[5].z)+road_width*dr0.z;
  pos[4].x= 0.5*(pos[2].x+pos[5].x)-road_width*dr0.x;
  pos[4].y= 0.5*(pos[2].y+pos[5].y)-road_width*dr0.y;
  pos[4].z= 0.5*(pos[2].z+pos[5].z)-road_width*dr0.z;
  pos[7].x= 0.5*(pos[0].x+pos[8].x)+road_width*dr1.x;
  pos[7].y= 0.5*(pos[0].y+pos[8].y)+road_width*dr1.y;
  pos[7].z= 0.5*(pos[0].z+pos[8].z)+road_width*dr1.z;
  pos[9].x= 0.5*(pos[6].x+pos[10].x)+road_width*dr2.x;
  pos[9].y= 0.5*(pos[6].y+pos[10].y)+road_width*dr2.y;
  pos[9].z= 0.5*(pos[6].z+pos[10].z)+road_width*dr2.z;
}

static void
Pave_Cell
(MAV_cityCell *c, int material, int texture)
{
  int i,j;
  MAV_vector nw,sw,se,ne;
  float x[4],y[4],z[4],tu[4],tv[4];

  /* pavement */
  for (j= c->miny; j<= c->maxy; j++)
    for (i= c->minx; i<= c->maxx; i++)
      {
	nw= Calc_Grid_Position (i*BLOCK_WIDTH/(size*BLOCK_WIDTH), (j+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
	sw= Calc_Grid_Position (i*BLOCK_WIDTH/(size*BLOCK_WIDTH), j*BLOCK_WIDTH/(size*BLOCK_WIDTH));
	se= Calc_Grid_Position ((i+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), j*BLOCK_WIDTH/(size*BLOCK_WIDTH));
	ne= Calc_Grid_Position ((i+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), (j+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
	nw.x -= c->cent.x; nw.y -= c->cent.y; nw.z -= c->cent.z;
	sw.x -= c->cent.x; sw.y -= c->cent.y; sw.z -= c->cent.z;
	se.x -= c->cent.x; se.y -= c->cent.y; se.z -= c->cent.z;
	ne.x -= c->cent.x; ne.y -= c->cent.y; ne.z -= c->cent.z;
	x[0]= nw.x; y[0]= PAVE_HEIGHT; z[0]= nw.z;
	x[1]= sw.x; y[1]= PAVE_HEIGHT; z[1]= sw.z;
	x[2]= se.x; y[2]= PAVE_HEIGHT; z[2]= se.z;
	x[3]= ne.x; y[3]= PAVE_HEIGHT; z[3]= ne.z;
	tu[0]= 0.5*(x[0]+c->cent.x); tv[0]= 0.5*(z[0]+c->cent.z);
	tu[1]= 0.5*(x[1]+c->cent.x); tv[1]= 0.5*(z[1]+c->cent.z);
	tu[2]= 0.5*(x[2]+c->cent.x); tv[2]= 0.5*(z[2]+c->cent.z);
	tu[3]= 0.5*(x[3]+c->cent.x); tv[3]= 0.5*(z[3]+c->cent.z);
	Add_Poly (c, 4, material,texture, x,y,z, tu,tv);
      }
}

static void
Generate_Type_0
(MAV_cityCell *c)
{
  Pave_Cell (c, MAT_WHITE,TEX_PAVEMENT);
}

static void
Generate_Type_1
(MAV_cityCell *c)
{
  float vx[4],vy[4],vz[4];
  float tu[4],tv[4];
  MAV_vector pos[12];

  Map(c,pos);

  switch (c->type)
    {
    case 1 :
      vx[0]= pos[0].x; vy[0]= 0.0; vz[0]= pos[0].z;
      vx[1]= pos[7].x; vy[1]= 0.0; vz[1]= pos[7].z;
      vx[2]= pos[9].x; vy[2]= 0.0; vz[2]= pos[9].z;
      vx[3]= pos[3].x; vy[3]= 0.0; vz[3]= pos[3].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Pavement (c, pos, 1+128+64+32+16+8+4);
      break;
    case 2 :
      vx[0]= pos[5].x; vy[0]= 0.0; vz[0]= pos[5].z;
      vx[1]= pos[1].x; vy[1]= 0.0; vz[1]= pos[1].z;
      vx[2]= pos[7].x; vy[2]= 0.0; vz[2]= pos[7].z;
      vx[3]= pos[10].x; vy[3]= 0.0; vz[3]= pos[10].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Pavement (c, pos, 4+2+1+128+64+32+16);
      break;
    case 4 :
      vx[0]= pos[1].x; vy[0]= 0.0; vz[0]= pos[1].z;
      vx[1]= pos[8].x; vy[1]= 0.0; vz[1]= pos[8].z;
      vx[2]= pos[11].x; vy[2]= 0.0; vz[2]= pos[11].z;
      vx[3]= pos[4].x; vy[3]= 0.0; vz[3]= pos[4].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Pavement (c, pos, 64+128+1+2+4+8+16);
      break;
    case 8 :
      vx[0]= pos[2].x; vy[0]= 0.0; vz[0]= pos[2].z;
      vx[1]= pos[6].x; vy[1]= 0.0; vz[1]= pos[6].z;
      vx[2]= pos[9].x; vy[2]= 0.0; vz[2]= pos[9].z;
      vx[3]= pos[4].x; vy[3]= 0.0; vz[3]= pos[4].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Pavement (c, pos, 1+2+4+8+16+32+64);
      break;
    }
}

static void
Generate_Type_2
(MAV_cityCell *c)
{
  float vx[4],vy[4],vz[4];
  float tu[4],tv[4];
  MAV_vector pos[12];

  Map(c,pos);

  switch (c->type)
    {
    case 5 :
      vx[0]= pos[0].x; vy[0]= 0.0; vz[0]= pos[0].z;
      vx[1]= pos[8].x; vy[1]= 0.0; vz[1]= pos[8].z;
      vx[2]= pos[11].x; vy[2]= 0.0; vz[2]= pos[11].z;
      vx[3]= pos[3].x; vy[3]= 0.0; vz[3]= pos[3].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Left_Lights (c, pos);
      Add_Right_Lights (c, pos);
      if (mav_random() < 0.5)
	{
	  Add_Horizontal_Crossing (c, pos);
	  Add_Vertical_CMarks (c, pos);
	}
      else
	Add_Vertical_Marks (c, pos);
      Add_Pavement (c, pos, 1+128+64+4+8+16);
      break;
    case 10 :
      vx[0]= pos[2].x; vy[0]= 0.0; vz[0]= pos[2].z;
      vx[1]= pos[6].x; vy[1]= 0.0; vz[1]= pos[6].z;
      vx[2]= pos[10].x; vy[2]= 0.0; vz[2]= pos[10].z;
      vx[3]= pos[5].x; vy[3]= 0.0; vz[3]= pos[5].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Top_Lights (c, pos);
      Add_Bottom_Lights (c, pos);
      if (mav_random() < 0.5)
	{
	  Add_Vertical_Crossing (c, pos);
	  Add_Horizontal_CMarks (c, pos);
	}
      else
	Add_Horizontal_Marks (c, pos);
      Add_Pavement (c, pos, 1+2+4+16+32+64);
      break;
    }
}

static void
Generate_Type_3
(MAV_cityCell *c)
{
  float vx[4],vy[4],vz[4];
  float tu[4],tv[4];
  MAV_vector pos[12];

  Map(c,pos);

  switch (c->type)
    {
    case 3 :
      vx[0]= pos[0].x; vy[0]= 0.0; vz[0]= pos[0].z;
      vx[1]= pos[7].x; vy[1]= 0.0; vz[1]= pos[7].z;
      vx[2]= pos[9].x; vy[2]= 0.0; vz[2]= pos[9].z;
      vx[3]= pos[3].x; vy[3]= 0.0; vz[3]= pos[3].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);

      vx[0]= pos[4].x; vy[0]= 0.0; vz[0]= pos[4].z;
      vx[1]= pos[9].x; vy[1]= 0.0; vz[1]= pos[9].z;
      vx[2]= pos[10].x; vy[2]= 0.0; vz[2]= pos[10].z;
      vx[3]= pos[5].x; vy[3]= 0.0; vz[3]= pos[5].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Pavement (c, pos, 1+128+64+32+16+4);
      Add_East_North_Marks (c, pos);
      break;
    case 6 :
      vx[0]= pos[1].x; vy[0]= 0.0; vz[0]= pos[1].z;
      vx[1]= pos[7].x; vy[1]= 0.0; vz[1]= pos[7].z;
      vx[2]= pos[10].x; vy[2]= 0.0; vz[2]= pos[10].z;
      vx[3]= pos[5].x; vy[3]= 0.0; vz[3]= pos[5].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);

      vx[0]= pos[7].x; vy[0]= 0.0; vz[0]= pos[7].z;
      vx[1]= pos[8].x; vy[1]= 0.0; vz[1]= pos[8].z;
      vx[2]= pos[11].x; vy[2]= 0.0; vz[2]= pos[11].z;
      vx[3]= pos[9].x; vy[3]= 0.0; vz[3]= pos[9].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Pavement (c, pos, 1+2+4+128+64+16);
      Add_East_South_Marks (c, pos);
      break;
    case 9 :
      vx[0]= pos[0].x; vy[0]= 0.0; vz[0]= pos[0].z;
      vx[1]= pos[7].x; vy[1]= 0.0; vz[1]= pos[7].z;
      vx[2]= pos[9].x; vy[2]= 0.0; vz[2]= pos[9].z;
      vx[3]= pos[3].x; vy[3]= 0.0; vz[3]= pos[3].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);

      vx[0]= pos[2].x; vy[0]= 0.0; vz[0]= pos[2].z;
      vx[1]= pos[6].x; vy[1]= 0.0; vz[1]= pos[6].z;
      vx[2]= pos[7].x; vy[2]= 0.0; vz[2]= pos[7].z;
      vx[3]= pos[1].x; vy[3]= 0.0; vz[3]= pos[1].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Pavement (c, pos, 1+4+8+16+32+64);
      Add_West_North_Marks (c, pos);
      break;
    case 12 :
      vx[0]= pos[1].x; vy[0]= 0.0; vz[0]= pos[1].z;
      vx[1]= pos[8].x; vy[1]= 0.0; vz[1]= pos[8].z;
      vx[2]= pos[11].x; vy[2]= 0.0; vz[2]= pos[11].z;
      vx[3]= pos[4].x; vy[3]= 0.0; vz[3]= pos[4].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);

      vx[0]= pos[2].x; vy[0]= 0.0; vz[0]= pos[2].z;
      vx[1]= pos[6].x; vy[1]= 0.0; vz[1]= pos[6].z;
      vx[2]= pos[7].x; vy[2]= 0.0; vz[2]= pos[7].z;
      vx[3]= pos[1].x; vy[3]= 0.0; vz[3]= pos[1].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_West_South_Marks (c, pos);
      Add_Pavement (c, pos, 1+2+4+8+16+64);
      break;
    }
}

static void
Generate_Type_4
(MAV_cityCell *c)
{
  float vx[4],vy[4],vz[4];
  float tu[4],tv[4];
  MAV_vector pos[12];

  Map(c,pos);

  switch (c->type)
    {
    case 7 :
      vx[0]= pos[0].x; vy[0]= 0.0; vz[0]= pos[0].z;
      vx[1]= pos[8].x; vy[1]= 0.0; vz[1]= pos[8].z;
      vx[2]= pos[11].x; vy[2]= 0.0; vz[2]= pos[11].z;
      vx[3]= pos[3].x; vy[3]= 0.0; vz[3]= pos[3].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);

      vx[0]= pos[4].x; vy[0]= 0.0; vz[0]= pos[4].z;
      vx[1]= pos[9].x; vy[1]= 0.0; vz[1]= pos[9].z;
      vx[2]= pos[10].x; vy[2]= 0.0; vz[2]= pos[10].z;
      vx[3]= pos[5].x; vy[3]= 0.0; vz[3]= pos[5].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Pavement (c, pos, 1+128+64+4+16);
      Add_Vertical_Marks (c, pos);
      Add_East_Junction (c,pos,0);
      break;
    case 11 :
      vx[0]= pos[2].x; vy[0]= 0.0; vz[0]= pos[2].z;
      vx[1]= pos[6].x; vy[1]= 0.0; vz[1]= pos[6].z;
      vx[2]= pos[10].x; vy[2]= 0.0; vz[2]= pos[10].z;
      vx[3]= pos[5].x; vy[3]= 0.0; vz[3]= pos[5].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);

      vx[0]= pos[0].x; vy[0]= 0.0; vz[0]= pos[0].z;
      vx[1]= pos[1].x; vy[1]= 0.0; vz[1]= pos[1].z;
      vx[2]= pos[4].x; vy[2]= 0.0; vz[2]= pos[4].z;
      vx[3]= pos[3].x; vy[3]= 0.0; vz[3]= pos[3].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Pavement (c, pos, 1+4+64+32+16);
      Add_Horizontal_Marks (c, pos);
      Add_North_Junction (c, pos,0);
      break;
    case 13 :
      vx[0]= pos[0].x; vy[0]= 0.0; vz[0]= pos[0].z;
      vx[1]= pos[8].x; vy[1]= 0.0; vz[1]= pos[8].z;
      vx[2]= pos[11].x; vy[2]= 0.0; vz[2]= pos[11].z;
      vx[3]= pos[3].x; vy[3]= 0.0; vz[3]= pos[3].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);

      vx[0]= pos[2].x; vy[0]= 0.0; vz[0]= pos[2].z;
      vx[1]= pos[6].x; vy[1]= 0.0; vz[1]= pos[6].z;
      vx[2]= pos[7].x; vy[2]= 0.0; vz[2]= pos[7].z;
      vx[3]= pos[1].x; vy[3]= 0.0; vz[3]= pos[1].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Pavement (c, pos, 1+64+4+8+16);
      Add_Vertical_Marks (c, pos);
      Add_West_Junction (c, pos,0);
      break;
    case 14 :
      vx[0]= pos[2].x; vy[0]= 0.0; vz[0]= pos[2].z;
      vx[1]= pos[6].x; vy[1]= 0.0; vz[1]= pos[6].z;
      vx[2]= pos[10].x; vy[2]= 0.0; vz[2]= pos[10].z;
      vx[3]= pos[5].x; vy[3]= 0.0; vz[3]= pos[5].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);

      vx[0]= pos[7].x; vy[0]= 0.0; vz[0]= pos[7].z;
      vx[1]= pos[8].x; vy[1]= 0.0; vz[1]= pos[8].z;
      vx[2]= pos[11].x; vy[2]= 0.0; vz[2]= pos[11].z;
      vx[3]= pos[9].x; vy[3]= 0.0; vz[3]= pos[9].z;
      tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
      tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
      tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
      tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
      Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);
      Add_Pavement (c, pos, 1+2+4+16+64);
      Add_Horizontal_Marks (c, pos);
      Add_South_Junction (c, pos,0);
      break;
    }
}

static void
Generate_Type_5
(MAV_cityCell *c)
{
  float vx[4],vy[4],vz[4];
  float tu[4],tv[4];
  MAV_vector pos[12];

  Map(c,pos);

  vx[0]= pos[2].x; vy[0]= 0.0; vz[0]= pos[2].z;
  vx[1]= pos[6].x; vy[1]= 0.0; vz[1]= pos[6].z;
  vx[2]= pos[10].x; vy[2]= 0.0; vz[2]= pos[10].z;
  vx[3]= pos[5].x; vy[3]= 0.0; vz[3]= pos[5].z;
  tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
  tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
  tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
  tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
  Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);

  vx[0]= pos[0].x; vy[0]= 0.0; vz[0]= pos[0].z;
  vx[1]= pos[1].x; vy[1]= 0.0; vz[1]= pos[1].z;
  vx[2]= pos[4].x; vy[2]= 0.0; vz[2]= pos[4].z;
  vx[3]= pos[3].x; vy[3]= 0.0; vz[3]= pos[3].z;
  tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
  tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
  tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
  tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
  Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);

  vx[0]= pos[7].x; vy[0]= 0.0; vz[0]= pos[7].z;
  vx[1]= pos[8].x; vy[1]= 0.0; vz[1]= pos[8].z;
  vx[2]= pos[11].x; vy[2]= 0.0; vz[2]= pos[11].z;
  vx[3]= pos[9].x; vy[3]= 0.0; vz[3]= pos[9].z;
  tu[0]= vx[0]+c->cent.x; tv[0]= vz[0]+c->cent.z;
  tu[1]= vx[1]+c->cent.x; tv[1]= vz[1]+c->cent.z;
  tu[2]= vx[2]+c->cent.x; tv[2]= vz[2]+c->cent.z;
  tu[3]= vx[3]+c->cent.x; tv[3]= vz[3]+c->cent.z;
  Add_Poly (c, 4, MAT_ASPHALT,TEX_ASPHALT, vx,vy,vz, tu,tv);

  Add_Pavement (c, pos, 1+4+16+64);

  Add_South_Junction (c, pos,1);
  Add_North_Junction (c, pos,1);
  Add_West_Junction (c, pos,1);
  Add_East_Junction (c, pos,1);
}

static void
Generate_Street_Polys
(MAV_cityCell *c)
{
  switch (c->type)
    {
    case 0 :
      Generate_Type_0 (c);
      break;
    case 1 :
    case 8 :
    case 4 :
    case 2 :
      Generate_Type_1 (c);
      break;
    case 5 :
    case 10 :
      Generate_Type_2 (c);
      break;
    case 12 :
    case 6 :
    case 9 :
    case 3 :
      Generate_Type_3 (c);
      break;
    case 14 :
    case 13 :
    case 11 :
    case 7 :
      Generate_Type_4 (c);
      break;
    case 15 :
      Generate_Type_5 (c);
      break;
    case 99 :
      Generate_Waste (c);
      break;
    }
}

static void
Create_Occluder
(float blx, float bly, float blz, float trx, float try, float trz)
{
  MAV_occluder *o;

  o= mav_malloc (sizeof(MAV_occluder));
  o->occluder.min.x= blx;
  o->occluder.min.y= bly;
  o->occluder.min.z= blz;
  o->occluder.max.x= trx;
  o->occluder.max.y= try;
  o->occluder.max.z= trz;
  o->matrix= MAV_ID_MATRIX;
  o->num_vertices= 0;

  mav_listItemAdd (list_of_objects, (void *)mav_objectNew (mav_class_occluder, o));
  total_objects++;
}

#if 0
static void
Generate_Empty_Lot
(MAV_cityCell *c)
{
  Pave_Cell (c, MAT_WHITE,TEX_PAVEMENT);
}
#endif

static void
Generate_Park_Features (void)
{
  MAV_cityCell *c;
  MAV_object *obj;
  int num_parks= 0;
  int sthenge_id;
  int red_fountain_id;
  int door_id;
  int easter_id;
  int ok;
  int i,j,n;
  float rnd;
  MAV_matrix transform;
  MAV_vector pt;
  float scale, u,v;
  MAV_vector r1,r2,r3,r4;
  MAV_vector nw,sw,se,ne;
  int num;
  MAV_vector cent;
  /*
  MAV_vector min,max;
  */

  mav_listPointerReset (list_of_objects);
  while (mav_listItemNext (list_of_objects, (void **)&obj))
    {
      if (obj->the_class == mav_class_citycell)
	{
	  c= (MAV_cityCell *) obj->the_data;

	  if (c->building == 1 && c->type == 5)
	    num_parks += (c->maxx-c->minx+1)*(c->maxy-c->miny+1);
	}
    }

  easter_id= (int)(num_parks*mav_random());
  sthenge_id= (int)(num_parks*mav_random());
  red_fountain_id= (int)(num_parks*mav_random());
  door_id= (int)(num_parks*mav_random());

  easter_id= 5;
  sthenge_id= 13;
  red_fountain_id= 29;
  door_id= 41;

#ifdef COMPOSITES
  ok= 1;
  num_parks= 0;
  mav_listPointerReset (list_of_objects);
  while (ok && mav_listItemNext (list_of_objects, (void **)&obj))
    {
      if (obj->the_class == mav_class_citycell)
	{
	  c= (MAV_cityCell *) obj->the_data;

	  if (c->building == 1 && c->type == 5)
	    {
	      for (j= c->miny; j<= c->maxy; j++)
		for (i= c->minx; i<= c->maxx; i++)
		  {
		    if (num_parks == sthenge_id)
		      {
			transform= mav_matrixSet (0.0,0.0,360.0*mav_random(),
				       c->cent.x,PAVE_HEIGHT,c->cent.z);
			Add_Feature (c, COMP_STHENGE, &transform);
			fprintf(stdout, "sthenge at %d,%d\n", i,j);
		      }
		    else if (num_parks == easter_id)
		      {
			transform= mav_matrixSet (0.0,0.0,360.0*mav_random(),
				       c->cent.x,PAVE_HEIGHT,c->cent.z);
			Add_Feature (c, COMP_EASTER, &transform);
			fprintf(stdout, "easter at %d,%d\n", i,j);
		      }
		    else if (num_parks == red_fountain_id)
		      {
			transform= mav_matrixSet (0.0,0.0,360.0*mav_random(),
				       c->cent.x,PAVE_HEIGHT,c->cent.z);
			Add_Feature (c, COMP_RED_FOUNTAIN, &transform);
			fprintf(stdout, "red fountain at %d,%d\n", i,j);
		      }
		    else if (num_parks == door_id)
		      {
			transform= mav_matrixSet (0.0,0.0,360.0*mav_random(),
				       c->cent.x,PAVE_HEIGHT,c->cent.z);
			Add_Feature (c, COMP_DOOR, &transform);
			fprintf(stdout, "door at %d,%d\n", i,j);
		      }
		    else
		      {
			rnd= mav_random();
			if (rnd < 0.9)
			  {
			    nw= Calc_Grid_Position (i*BLOCK_WIDTH/(size*BLOCK_WIDTH), (j+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
			    sw= Calc_Grid_Position (i*BLOCK_WIDTH/(size*BLOCK_WIDTH), j*BLOCK_WIDTH/(size*BLOCK_WIDTH));
			    se= Calc_Grid_Position ((i+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), j*BLOCK_WIDTH/(size*BLOCK_WIDTH));
			    ne= Calc_Grid_Position ((i+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), (j+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
			    nw.x -= c->cent.x; nw.y -= c->cent.y; nw.z -= c->cent.z;
			    sw.x -= c->cent.x; sw.y -= c->cent.y; sw.z -= c->cent.z;
			    se.x -= c->cent.x; se.y -= c->cent.y; se.z -= c->cent.z;
			    ne.x -= c->cent.x; ne.y -= c->cent.y; ne.z -= c->cent.z;

			    r1.x= c->cent.x+nw.x;
			    r1.y= c->cent.y+nw.y;
			    r1.z= c->cent.z+nw.z;
			    r2.x= ne.x-nw.x;
			    r2.y= ne.y-nw.y;
			    r2.z= ne.z-nw.z;
			    r3.x= sw.x-nw.x;
			    r3.y= sw.y-nw.y;
			    r3.z= sw.z-nw.z;
			    r4.x= se.x-ne.x;
			    r4.y= se.y-ne.y;
			    r4.z= se.z-ne.z;

			    num= (int)(4.0*mav_random());
			    for (n= 0; n< num; n++)
			      {
				scale= 0.5+0.5*mav_random();
				u= 0.1+0.8*mav_random();
				v= 0.1+0.8*mav_random();
				
				PAR_TO_WOR(u,v,r1,r2,r3,r4,pt);
				
				rnd= mav_random();
				if (rnd < 0.5)
#ifdef TEXTURES
				  Add_Billboard (TEX_TREE1, pt.x,PAVE_HEIGHT,pt.z, 6.66*scale,10.0*scale, 0.0,1.0,0.0);
#else
				  Add_Billboard (TEX_TREE3, pt.x,PAVE_HEIGHT,pt.z, 6.66*scale,10.0*scale, 0.0,1.0,0.0);
#endif
				else
				  Add_Billboard (TEX_TREE3, pt.x,PAVE_HEIGHT,pt.z, 6.66*scale,10.0*scale, 0.0,1.0,0.0);
			      }
			  }
			else
			  {
			    nw= Calc_Grid_Position (i*BLOCK_WIDTH/(size*BLOCK_WIDTH), (j+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
			    sw= Calc_Grid_Position (i*BLOCK_WIDTH/(size*BLOCK_WIDTH), j*BLOCK_WIDTH/(size*BLOCK_WIDTH));
			    se= Calc_Grid_Position ((i+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), j*BLOCK_WIDTH/(size*BLOCK_WIDTH));
			    ne= Calc_Grid_Position ((i+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), (j+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
			    cent.x= 0.25*(nw.x+sw.x+se.x+ne.x);
			    cent.y= 0.25*(nw.y+sw.y+se.y+ne.y);
			    cent.z= 0.25*(nw.z+sw.z+se.z+ne.z);

			    rnd= mav_random();
			    if (rnd < 0.5)
			      {
				transform= mav_matrixSet (0.0,0.0,360.0*mav_random(),
					       cent.x,PAVE_HEIGHT,cent.z);
				Add_Feature (c, COMP_FOUNTAIN, &transform);
			      }
			    else
			      {
				transform= mav_matrixSet (0.0,0.0,360.0*mav_random(),
					       cent.x,PAVE_HEIGHT,cent.z);
				Add_Feature (c, COMP_GAZEBO, &transform);
			      }
			  }
		      }
		    num_parks++;
		  }
	  
	      /* fence */
	      /*
	      if (mav_random() < 0 && (c->maxx-c->minx+1)*(c->maxy-c->miny+1) > 1)
		{
		  min.x= c->minx*BLOCK_WIDTH;
		  min.y= PAVE_HEIGHT;
		  min.z= c->miny*BLOCK_WIDTH;
	      
		  max.x= (c->maxx+1)*BLOCK_WIDTH;
		  max.y= 2.2;
		  max.z= (c->maxy+1)*BLOCK_WIDTH;

		  if (mav_random() < 0)
		    {
		      for (x= min.x; x< max.x; x+= 4.0)
			{
			  transform= mav_matrixSet (0.0,0.0,0.0, size*BLOCK_WIDTH-(x+2.0),1.1,min.z);
			  Add_Composite (c, COMP_FENCE2, &transform);	  
		      
			  transform= mav_matrixSet (0.0,0.0,0.0, size*BLOCK_WIDTH-(x+2.0),1.1,max.z);
			  Add_Composite (c, COMP_FENCE2, &transform);	  
			}
		  
		      for (z= min.z; z< max.z; z+= 4.0)
			{
			  transform= mav_matrixSet (0.0,0.0,90.0, size*BLOCK_WIDTH-min.x,1.1,z+2.0);
			  Add_Composite (c, COMP_FENCE2, &transform);	  
		      
			  transform= mav_matrixSet (0.0,0.0,90.0, size*BLOCK_WIDTH-max.x,1.1,z+2.0);
			  Add_Composite (c, COMP_FENCE2, &transform);	  
			}
		    }
		  else
		    {
		      for (x= min.x; x< max.x; x+= 4.0)
			{
			  transform= mav_matrixSet (0.0,0.0,0.0, size*BLOCK_WIDTH-(x+2.0),1.1,min.z);
			  Add_Composite (c, COMP_FENCE, &transform);	  
		      
			  transform= mav_matrixSet (0.0,0.0,0.0, size*BLOCK_WIDTH-(x+2.0),1.1,max.z);
			  Add_Composite (c, COMP_FENCE, &transform);	  
			}
		  
		      for (z= min.z; z< max.z; z+= 4.0)
			{
			  transform= mav_matrixSet (0.0,0.0,90.0, size*BLOCK_WIDTH-min.x,1.1,z+2.0);
			  Add_Composite (c, COMP_FENCE, &transform);	  
		      
			  transform= mav_matrixSet (0.0,0.0,90.0, size*BLOCK_WIDTH-max.x,1.1,z+2.0);
			  Add_Composite (c, COMP_FENCE, &transform);	  
			}
		    }
		    */
	    }
	}
    }
#endif
}

static void
Generate_Park
(MAV_cityCell *c)
{
  int i,j;
  MAV_vector nw,sw,se,ne;
  float x[4],y[4],z[4],tu[4],tv[4];

  for (j= c->miny; j<= c->maxy; j++)
    for (i= c->minx; i<= c->maxx; i++)
      {
	nw= Calc_Grid_Position (i*BLOCK_WIDTH/(size*BLOCK_WIDTH), (j+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
	sw= Calc_Grid_Position (i*BLOCK_WIDTH/(size*BLOCK_WIDTH), j*BLOCK_WIDTH/(size*BLOCK_WIDTH));
	se= Calc_Grid_Position ((i+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), j*BLOCK_WIDTH/(size*BLOCK_WIDTH));
	ne= Calc_Grid_Position ((i+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), (j+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
	nw.x -= c->cent.x; nw.y -= c->cent.y; nw.z -= c->cent.z;
	sw.x -= c->cent.x; sw.y -= c->cent.y; sw.z -= c->cent.z;
	se.x -= c->cent.x; se.y -= c->cent.y; se.z -= c->cent.z;
	ne.x -= c->cent.x; ne.y -= c->cent.y; ne.z -= c->cent.z;
	x[0]= nw.x; y[0]= PAVE_HEIGHT; z[0]= nw.z;
	x[1]= sw.x; y[1]= PAVE_HEIGHT; z[1]= sw.z;
	x[2]= se.x; y[2]= PAVE_HEIGHT; z[2]= se.z;
	x[3]= ne.x; y[3]= PAVE_HEIGHT; z[3]= ne.z;
	tu[0]= x[0]+c->cent.x; tv[0]= z[0]+c->cent.z;
	tu[1]= x[1]+c->cent.x; tv[1]= z[1]+c->cent.z;
	tu[2]= x[2]+c->cent.x; tv[2]= z[2]+c->cent.z;
	tu[3]= x[3]+c->cent.x; tv[3]= z[3]+c->cent.z;
#ifdef TEXTURES
	Add_Poly (c, 4, MAT_WHITE,TEX_GRASS, x,y,z, tu,tv);
#else
	Add_Poly (c, 4, MAT_GRASS,-1, x,y,z, tu,tv);
#endif
      }
}

static int
Pick_Texture
(float *tu, float *tv, float wid, float hei)
{
  float rnd;
  int texture;
  int numx,numz;

  rnd= mav_random();
  if (rnd < 1.0/6.0)
    {
      texture= TEX_BUILDING_SIDE1;
      numx= (int)(ceil(wid/2.0));
      tu[0]= 0.0; tv[0]= 1.0;
      tu[1]= 0.0; tv[1]= 0.0;
      tu[2]= (float)numx; tv[2]= 0.0;
      tu[3]= (float)numx; tv[3]= 1.0;
    }
  else if (rnd < 2.0/6.0)
    {
      texture= TEX_BUILDING_SIDE2;
      numx= (int)(ceil(wid/4.0));
      numz= (int)(ceil(hei/2.0));
      tu[0]= 0.0; tv[0]= (float)numz;
      tu[1]= 0.0; tv[1]= 0.0;
      tu[2]= (float)numx; tv[2]= 0.0;
      tu[3]= (float)numx; tv[3]= (float)numz;
    }
  else if (rnd < 3.0/6.0)
    {
      texture= TEX_BUILDING_SIDE3;
      numx= (int)(ceil(wid/3.0));
      numz= (int)(ceil(hei/2.0));
      tu[0]= 0.0; tv[0]= (float)numz;
      tu[1]= 0.0; tv[1]= 0.0;
      tu[2]= (float)numx; tv[2]= 0.0;
      tu[3]= (float)numx; tv[3]= (float)numz;
    }
  else if (rnd < 4.0/6.0)
    {
      texture= TEX_BUILDING_SIDE4;
      numx= (int)(ceil(wid/4.0));
      numz= (int)(ceil(hei/3.0));
      tu[0]= 0.0; tv[0]= (float)numz;
      tu[1]= 0.0; tv[1]= 0.0;
      tu[2]= (float)numx; tv[2]= 0.0;
      tu[3]= (float)numx; tv[3]= (float)numz;
    }
  else if (rnd < 5.0/6.0)
    {
      texture= TEX_BUILDING_SIDE5;
      numx= (int)(ceil(wid/3.0));
      numz= (int)(ceil(hei/2.0));
      tu[0]= 0.0; tv[0]= (float)numz;
      tu[1]= 0.0; tv[1]= 0.0;
      tu[2]= (float)numx; tv[2]= 0.0;
      tu[3]= (float)numx; tv[3]= (float)numz;
    }
  else
    {
      texture= TEX_BUILDING_SIDE6;
      numx= (int)(ceil(wid/3.0));
      numz= (int)(ceil(hei/2.0));
      tu[0]= 0.0; tv[0]= (float)numz;
      tu[1]= 0.0; tv[1]= 0.0;
      tu[2]= (float)numx; tv[2]= 0.0;
      tu[3]= (float)numx; tv[3]= (float)numz;
    }

  return texture;
}

static void
Generate_Textured_Building
(MAV_cityCell *c)
{
  float sx,sy,sz;
  float wid,len,hei;
  float owid,olen;
  float x[5],y[5],z[5];
  float tu[5],tv[5];
  float blx,blz,trx,trz;
  float rnd;
  int texture;
  float scx,scz;
  int roofx,roofz=0;
  float roof_hei;
  int col;
  float ex,ez;

  sx= c->nw.x > c->sw.x ? c->nw.x : c->sw.x;
  sy= PAVE_HEIGHT;
  sz= c->sw.z > c->se.z ? c->sw.z : c->se.z;
  ex= c->ne.x < c->se.x ? c->ne.x : c->se.x;
  ez= c->ne.z < c->nw.z ? c->ne.z : c->nw.z;

  owid= wid= ex-sx;
  olen= len= ez-sz;
  hei= c->height;

  if (c->maxx-c->minx == 0 && c->maxy-c->miny > 0)
    roofx= mav_random() < 0.5 ? 1 : 0;
  else if (c->maxx-c->minx > 0 && c->maxy-c->miny == 0)
    roofx= mav_random() < 0.5 ? 1 : 0;
  else
    roofx= roofz= mav_random() < 0.5 ? 1 : 0;

  roof_hei= hei*(0.1+0.1*mav_random());
  rnd= mav_random();
#ifdef TEXTURES
  if (rnd < 0.3) col= MAT_WHITE;
  else if (rnd < 0.6) col= MAT_WHITE2;
  else col= MAT_WHITE3;
#else
  if (rnd < 0.3) col= MAT_NOTEX;
  else if (rnd < 0.6) col= MAT_NOTEX2;
  else col= MAT_NOTEX3;
#endif

  /* randomize building size */
  scx= 0.6+0.4*mav_random();
  scz= 0.6+0.4*mav_random();
  wid= (wid-3.0)*scx;
  len= (len-3.0)*scz;
  blx= 1.5+((owid-3.0)-wid)*mav_random();
  blz= 1.5+((olen-3.0)-len)*mav_random();
  trx= blx+wid;
  trz= blz+len;

  /* LOD polys */
  Add_Relative_Lod_Box (sx,sy,sz, c, col,-1, blx,0.0,blz,trx,(roofx||roofz)?(hei+roof_hei):hei,trz,1.0);

  x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
  x[1]= sx+blx; y[1]= sy; z[1]= sz+blz;
  x[2]= sx+trx; y[2]= sy; z[2]= sz+blz;
  x[3]= sx+trx; y[3]= sy+hei; z[3]= sz+blz;
  texture= Pick_Texture (tu,tv, trx-blx,hei);
  Add_Poly (c, 4, col,texture, x,y,z, tu,tv);
  if (roofx)
    {
      x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+(trx+blx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+blz;
      x[2]= sx+blx; y[2]= sy+hei; z[2]= sz+blz;
      tu[0]= x[0]; tv[0]= 0.0;
      tu[1]= x[1]; tv[1]= roof_hei;
      tu[2]= x[2]; tv[2]= 0.0;
      Add_Poly (c, 3, col,TEX_BUILDING_SIDE7, x,y,z, tu,tv);
    }

  x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+blz;
  x[1]= sx+trx; y[1]= sy; z[1]= sz+blz;
  x[2]= sx+trx; y[2]= sy; z[2]= sz+trz;
  x[3]= sx+trx; y[3]= sy+hei; z[3]= sz+trz;
  texture= Pick_Texture (tu,tv, trz-blz,hei);
  Add_Poly (c, 4, col,texture, x,y,z, tu,tv);
  if (roofz)
    {
      x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+trz;
      x[1]= sx+trx; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+blz;
      tu[0]= z[0]; tv[0]= 0.0;
      tu[1]= z[1]; tv[1]= roof_hei;
      tu[2]= z[2]; tv[2]= 0.0;
      Add_Poly (c, 3, col,TEX_BUILDING_SIDE7, x,y,z, tu,tv);
    }

  x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+trz;
  x[1]= sx+trx; y[1]= sy; z[1]= sz+trz;
  x[2]= sx+blx; y[2]= sy; z[2]= sz+trz;
  x[3]= sx+blx; y[3]= sy+hei; z[3]= sz+trz;
  texture= Pick_Texture (tu,tv, trx-blx,hei);
  Add_Poly (c, 4, col,texture, x,y,z, tu,tv);
  if (roofx)
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+trz;
      x[1]= sx+(trx+blx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+trz;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+trz;
      tu[0]= x[0]; tv[0]= 0.0;
      tu[1]= x[1]; tv[1]= roof_hei;
      tu[2]= x[2]; tv[2]= 0.0;
      Add_Poly (c, 3, col,TEX_BUILDING_SIDE7, x,y,z, tu,tv);
    }

  x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+trz;
  x[1]= sx+blx; y[1]= sy; z[1]= sz+trz;
  x[2]= sx+blx; y[2]= sy; z[2]= sz+blz;
  x[3]= sx+blx; y[3]= sy+hei; z[3]= sz+blz;
  texture= Pick_Texture (tu,tv, trz-blz,hei);
  Add_Poly (c, 4, col,texture, x,y,z, tu,tv);
  if (roofz)
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+blx; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+blx; y[2]= sy+hei; z[2]= sz+trz;
      tu[0]= z[0]; tv[0]= 0.0;
      tu[1]= z[1]; tv[1]= roof_hei;
      tu[2]= z[2]; tv[2]= 0.0;
      Add_Poly (c, 3, col,TEX_BUILDING_SIDE7, x,y,z, tu,tv);
    }

  if (!roofx && !roofz)
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+trx; y[1]= sy+hei; z[1]= sz+blz;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+trz;
      x[3]= sx+blx; y[3]= sy+hei; z[3]= sz+trz;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      tu[3]= x[3]; tv[3]= z[3];
      Add_Poly (c, 4, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);
    }
  else if (roofx && !roofz)
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+(trx+blx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+blz;
      x[2]= sx+(trx+blx)/2.0; y[2]= sy+hei+roof_hei; z[2]= sz+trz;
      x[3]= sx+blx; y[3]= sy+hei; z[3]= sz+trz;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      tu[3]= x[3]; tv[3]= z[3];
      Add_Poly (c, 4, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);

      x[0]= sx+(trx+blx)/2.0; y[0]= sy+hei+roof_hei; z[0]= sz+blz;
      x[1]= sx+trx; y[1]= sy+hei; z[1]= sz+blz;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+trz;
      x[3]= sx+(trx+blx)/2.0; y[3]= sy+hei+roof_hei; z[3]= sz+trz;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      tu[3]= x[3]; tv[3]= z[3];
      Add_Poly (c, 4, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);
    }
  else if (roofz && !roofx)
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+trx; y[1]= sy+hei; z[1]= sz+blz;
      x[2]= sx+trx; y[2]= sy+hei+roof_hei; z[2]= sz+(blz+trz)/2.0;
      x[3]= sx+blx; y[3]= sy+hei+roof_hei; z[3]= sz+(blz+trz)/2.0;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      tu[3]= x[3]; tv[3]= z[3];
      Add_Poly (c, 4, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);

      x[0]= sx+blx; y[0]= sy+hei+roof_hei; z[0]= sz+(blz+trz)/2.0;
      x[1]= sx+trx; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+trz;
      x[3]= sx+blx; y[3]= sy+hei; z[3]= sz+trz;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      tu[3]= x[3]; tv[3]= z[3];
      Add_Poly (c, 4, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);
    }
  else
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+blx; y[2]= sy+hei+roof_hei; z[2]= sz+(blz+trz)/2.0;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      Add_Poly (c, 3, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);

      x[0]= sx+blx; y[0]= sy+hei+roof_hei; z[0]= sz+(blz+trz)/2.0;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+blx; y[2]= sy+hei; z[2]= sz+trz;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      Add_Poly (c, 3, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);

      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+trz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+(blx+trx)/2.0; y[2]= sy+hei+roof_hei; z[2]= sz+trz;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      Add_Poly (c, 3, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);

      x[0]= sx+(blx+trx)/2.0; y[0]= sy+hei+roof_hei; z[0]= sz+trz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+trz;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      Add_Poly (c, 3, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);

      x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+trz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+trx; y[2]= sy+hei+roof_hei; z[2]= sz+(blz+trz)/2.0;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      Add_Poly (c, 3, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);

      x[0]= sx+trx; y[0]= sy+hei+roof_hei; z[0]= sz+(blz+trz)/2.0;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+blz;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      Add_Poly (c, 3, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);

      x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+(blx+trx)/2.0; y[2]= sy+hei+roof_hei; z[2]= sz+blz;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      Add_Poly (c, 3, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);

      x[0]= sx+(blx+trx)/2.0; y[0]= sy+hei+roof_hei; z[0]= sz+blz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+blx; y[2]= sy+hei; z[2]= sz+blz;
      tu[0]= x[0]; tv[0]= z[0];
      tu[1]= x[1]; tv[1]= z[1];
      tu[2]= x[2]; tv[2]= z[2];
      Add_Poly (c, 3, MAT_WHITE,TEX_BUILDING_TOP, x,y,z, tu,tv);
    }

  /* add occluder */
  Create_Occluder (c->cent.x+sx+blx,sy,c->cent.z+sz+blz, c->cent.x+sx+trx,sy+hei,c->cent.z+sz+trz);
}

static void
Generate_Textured_Building2
(MAV_cityCell *c)
{
  float sx,sy,sz;
  float wid,len,hei;
  float owid,olen;
  float x[5],y[5],z[5];
  float tu[5],tv[5];
  float blx,blz,trx,trz;
  float scx,scz;
  int roofx,roofz;
  float roof_hei;
  int numx,numz;
  int texture= mav_random() < 0.5 ? TEX_WINDOW : TEX_WINDOW2;
  float ex,ez;

  sx= c->nw.x > c->sw.x ? c->nw.x : c->sw.x;
  sy= PAVE_HEIGHT;
  sz= c->sw.z > c->se.z ? c->sw.z : c->se.z;
  ex= c->ne.x < c->se.x ? c->ne.x : c->se.x;
  ez= c->ne.z < c->nw.z ? c->ne.z : c->nw.z;

  owid= wid= ex-sx;
  olen= len= ez-sz;
  hei= c->height;

  roofx= roofz= 1;
  roof_hei= 1.0;

  /* randomize building size */
  scx= 0.6+0.4*mav_random();
  scz= 0.6+0.4*mav_random();
  wid= (wid-3.0)*scx;
  len= (len-3.0)*scz;
  blx= 1.5+((owid-3.0)-wid)*mav_random();
  blz= 1.5+((olen-3.0)-len)*mav_random();
  trx= blx+wid;
  trz= blz+len;

  /* LOD polys */
  Add_Relative_Lod_Box (sx,sy,sz, c, MAT_CONCRETE,-1, blx,0.0,blz,trx,(roofx||roofz)?(hei+roof_hei):hei,trz,1.0);

  x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
  x[1]= sx+blx; y[1]= sy; z[1]= sz+blz;
  x[2]= sx+trx; y[2]= sy; z[2]= sz+blz;
  x[3]= sx+trx; y[3]= sy+hei; z[3]= sz+blz;
  numx= (int)(ceil((trx-blx)/2.0));
  numz= (int)(ceil(hei/2.0));
  tu[0]= 0.0; tv[0]= (float)numz;
  tu[1]= 0.0; tv[1]= 0.0;
  tu[2]= (float)numx; tv[2]= 0.0;
  tu[3]= (float)numx; tv[3]= (float)numz;
  Add_Poly (c, 4, MAT_NOTEX,texture, x,y,z, tu,tv);
  if (roofx)
    {
      x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+(trx+blx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+blz;
      x[2]= sx+blx; y[2]= sy+hei; z[2]= sz+blz;
      tu[0]= x[0]; tv[0]= 0.0;
      tu[1]= x[1]; tv[1]= roof_hei;
      tu[2]= x[2]; tv[2]= 0.0;
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_TOP, x,y,z, tu,tv);
    }

  x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+blz;
  x[1]= sx+trx; y[1]= sy; z[1]= sz+blz;
  x[2]= sx+trx; y[2]= sy; z[2]= sz+trz;
  x[3]= sx+trx; y[3]= sy+hei; z[3]= sz+trz;
  numx= (int)(ceil((trz-blz)/2.0));
  numz= (int)(ceil(hei/2.0));
  tu[0]= 0.0; tv[0]= (float)numz;
  tu[1]= 0.0; tv[1]= 0.0;
  tu[2]= (float)numx; tv[2]= 0.0;
  tu[3]= (float)numx; tv[3]= (float)numz;
  Add_Poly (c, 4, MAT_NOTEX,texture, x,y,z, tu,tv);
  if (roofz)
    {
      x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+trz;
      x[1]= sx+trx; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+blz;
      tu[0]= z[0]; tv[0]= 0.0;
      tu[1]= z[1]; tv[1]= roof_hei;
      tu[2]= z[2]; tv[2]= 0.0;
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_TOP, x,y,z, tu,tv);
    }

  x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+trz;
  x[1]= sx+trx; y[1]= sy; z[1]= sz+trz;
  x[2]= sx+blx; y[2]= sy; z[2]= sz+trz;
  x[3]= sx+blx; y[3]= sy+hei; z[3]= sz+trz;
  numx= (int)(ceil((trx-blx)/2.0));
  numz= (int)(ceil(hei/2.0));
  tu[0]= 0.0; tv[0]= (float)numz;
  tu[1]= 0.0; tv[1]= 0.0;
  tu[2]= (float)numx; tv[2]= 0.0;
  tu[3]= (float)numx; tv[3]= (float)numz;
  Add_Poly (c, 4, MAT_NOTEX,texture, x,y,z, tu,tv);
  if (roofx)
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+trz;
      x[1]= sx+(trx+blx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+trz;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+trz;
      tu[0]= x[0]; tv[0]= 0.0;
      tu[1]= x[1]; tv[1]= roof_hei;
      tu[2]= x[2]; tv[2]= 0.0;
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_TOP, x,y,z, tu,tv);
    }

  x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+trz;
  x[1]= sx+blx; y[1]= sy; z[1]= sz+trz;
  x[2]= sx+blx; y[2]= sy; z[2]= sz+blz;
  x[3]= sx+blx; y[3]= sy+hei; z[3]= sz+blz;
  numx= (int)(ceil((trz-blz)/2.0));
  numz= (int)(ceil(hei/2.0));
  tu[0]= 0.0; tv[0]= (float)numz;
  tu[1]= 0.0; tv[1]= 0.0;
  tu[2]= (float)numx; tv[2]= 0.0;
  tu[3]= (float)numx; tv[3]= (float)numz;
  Add_Poly (c, 4, MAT_NOTEX,texture, x,y,z, tu,tv);
  if (roofz)
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+blx; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+blx; y[2]= sy+hei; z[2]= sz+trz;
      tu[0]= z[0]; tv[0]= 0.0;
      tu[1]= z[1]; tv[1]= roof_hei;
      tu[2]= z[2]; tv[2]= 0.0;
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_TOP, x,y,z, tu,tv);
    }

  if (!roofx && !roofz)
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+trx; y[1]= sy+hei; z[1]= sz+blz;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+trz;
      x[3]= sx+blx; y[3]= sy+hei; z[3]= sz+trz;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      tu[3]= 0.25*x[3]; tv[3]= 0.25*z[3];
      Add_Poly (c, 4, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);
    }
  else if (roofx && !roofz)
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+(trx+blx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+blz;
      x[2]= sx+(trx+blx)/2.0; y[2]= sy+hei+roof_hei; z[2]= sz+trz;
      x[3]= sx+blx; y[3]= sy+hei; z[3]= sz+trz;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      tu[3]= 0.25*x[3]; tv[3]= 0.25*z[3];
      Add_Poly (c, 4, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);

      x[0]= sx+(trx+blx)/2.0; y[0]= sy+hei+roof_hei; z[0]= sz+blz;
      x[1]= sx+trx; y[1]= sy+hei; z[1]= sz+blz;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+trz;
      x[3]= sx+(trx+blx)/2.0; y[3]= sy+hei+roof_hei; z[3]= sz+trz;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      tu[3]= 0.25*x[3]; tv[3]= 0.25*z[3];
      Add_Poly (c, 4, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);
    }
  else if (roofz && !roofx)
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+trx; y[1]= sy+hei; z[1]= sz+blz;
      x[2]= sx+trx; y[2]= sy+hei+roof_hei; z[2]= sz+(blz+trz)/2.0;
      x[3]= sx+blx; y[3]= sy+hei+roof_hei; z[3]= sz+(blz+trz)/2.0;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      tu[3]= 0.25*x[3]; tv[3]= 0.25*z[3];
      Add_Poly (c, 4, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);

      x[0]= sx+blx; y[0]= sy+hei+roof_hei; z[0]= sz+(blz+trz)/2.0;
      x[1]= sx+trx; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+trz;
      x[3]= sx+blx; y[3]= sy+hei; z[3]= sz+trz;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      tu[3]= 0.25*x[3]; tv[3]= 0.25*z[3];
      Add_Poly (c, 4, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);
    }
  else
    {
      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+blx; y[2]= sy+hei+roof_hei; z[2]= sz+(blz+trz)/2.0;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);

      x[0]= sx+blx; y[0]= sy+hei+roof_hei; z[0]= sz+(blz+trz)/2.0;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+blx; y[2]= sy+hei; z[2]= sz+trz;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);

      x[0]= sx+blx; y[0]= sy+hei; z[0]= sz+trz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+(blx+trx)/2.0; y[2]= sy+hei+roof_hei; z[2]= sz+trz;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);

      x[0]= sx+(blx+trx)/2.0; y[0]= sy+hei+roof_hei; z[0]= sz+trz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+trz;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);

      x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+trz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+trx; y[2]= sy+hei+roof_hei; z[2]= sz+(blz+trz)/2.0;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);

      x[0]= sx+trx; y[0]= sy+hei+roof_hei; z[0]= sz+(blz+trz)/2.0;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+trx; y[2]= sy+hei; z[2]= sz+blz;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);

      x[0]= sx+trx; y[0]= sy+hei; z[0]= sz+blz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+(blx+trx)/2.0; y[2]= sy+hei+roof_hei; z[2]= sz+blz;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);

      x[0]= sx+(blx+trx)/2.0; y[0]= sy+hei+roof_hei; z[0]= sz+blz;
      x[1]= sx+(blx+trx)/2.0; y[1]= sy+hei+roof_hei; z[1]= sz+(blz+trz)/2.0;
      x[2]= sx+blx; y[2]= sy+hei; z[2]= sz+blz;
      tu[0]= 0.25*x[0]; tv[0]= 0.25*z[0];
      tu[1]= 0.25*x[1]; tv[1]= 0.25*z[1];
      tu[2]= 0.25*x[2]; tv[2]= 0.25*z[2];
      Add_Poly (c, 3, MAT_NOTEX,TEX_WINDOW_ROOF, x,y,z, tu,tv);
    }

  /* add occluder */
  Create_Occluder (c->cent.x+sx+blx,sy,c->cent.z+sz+blz, c->cent.x+sx+trx,sy+hei,c->cent.z+sz+trz);
}

static void
Generate_Small_Building
(MAV_cityCell *c)
{
  float sx,sy,sz;
  float wid,len,hei;
  float ex,ez;
  float scale;

  sx= c->nw.x > c->sw.x ? c->nw.x : c->sw.x;
  sy= PAVE_HEIGHT;
  sz= c->sw.z > c->se.z ? c->sw.z : c->se.z;
  ex= c->ne.x < c->se.x ? c->ne.x : c->se.x;
  ez= c->ne.z < c->nw.z ? c->ne.z : c->nw.z;

  wid= ex-sx;
  len= ez-sz;
  hei= c->height;
  scale= wid/(float)((c->maxx-c->minx+1)*BLOCK_WIDTH);

  /* building */
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*1.5,0.0,scale*1.5, wid-scale*1.5,hei,len-scale*1.5, 1.0);

  Create_Occluder (c->cent.x+sx+scale*1.5,sy,c->cent.z+sz+scale*1.5, c->cent.x+sx+wid-scale*1.5,sy+hei,c->cent.z+sz+len-scale*1.5);
}

static void
Generate_Parametrised_Building
(MAV_cityCell *c)
{
  float wid, len, hei;
  float lump_width;
  float window_width;
  float window_height;
  float window_gap;
  float window_bar;
  float first_len_end=0, last_len_end;
  float first_wid_end=0, last_wid_end;
  float x,y;
  float sx,sy,sz;
  int num, numy;
  int i,j;
  float vx[4],vy[4],vz[4];
  float ex,ez;
  float scale;

  sx= c->nw.x > c->sw.x ? c->nw.x : c->sw.x;
  sy= PAVE_HEIGHT;
  sz= c->sw.z > c->se.z ? c->sw.z : c->se.z;
  ex= c->ne.x < c->se.x ? c->ne.x : c->se.x;
  ez= c->ne.z < c->nw.z ? c->ne.z : c->nw.z;

  wid= ez-sz;
  len= ex-sx;
  hei= c->height;
  scale= wid/(float)((c->maxx-c->minx+1)*BLOCK_WIDTH);
  scale= 1.0;

  lump_width= 0.5;
  window_width= 1.0;
  window_height= 1.0;
  window_gap= 1.0;
  window_bar= 0.5;

  /* fancy top bit */
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, 0.0, hei-1.0, 0.0, len, hei, scale*0.5, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, 0.0, hei-1.0, scale*0.5, scale*0.5, hei, wid-scale*0.5, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, 0.0, hei-1.0, wid-scale*0.5, len, hei, wid, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, len-scale*0.5, hei-scale*1.0, scale*0.5, len, hei, wid-scale*0.5, 1.0);

  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*0.2, hei-scale*1.7, scale*0.2, len-scale*0.2, hei-scale*1.0, wid-scale*0.2, 1.0);

  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*0.4, hei-scale*2.7, scale*0.4, len-scale*0.4, hei-scale*1.7, wid-scale*0.4, 1.0);

  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*1.7, hei-scale*2.7-2.0*window_gap-window_height,
		    scale*0.7, len-scale*1.7, hei-scale*2.7-window_gap-scale*window_height, wid-scale*0.7, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*0.7, hei-scale*2.7-2.0*window_gap-window_height,
		    scale*1.7, len-scale*0.7, hei-scale*2.7-window_gap-scale*window_height, wid-scale*1.7, 1.0);

  num= (int)((len-scale*0.8)/(3*lump_width));
  x= scale*0.4+((len-scale*0.8)-num*3*lump_width)/2.0;
  for (i= 0; i< num; i++)
    {
      Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, x+lump_width, hei-scale*1.7-lump_width, scale*0.4-lump_width/2.0,
			x+2.0*lump_width, hei-scale*1.7, wid-scale*0.4+lump_width/2.0, 1.0);
      x += 3.0*lump_width;
    }

  num= (int)((wid-scale*0.8)/(3*lump_width));
  x= scale*0.4+((wid-scale*0.8)-num*3*lump_width)/2.0;
  for (i= 0; i< num; i++)
    {
      Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*0.4-lump_width/2.0, hei-scale*1.7-lump_width, x+lump_width,
			len-scale*0.4+lump_width/2.0, hei-scale*1.7, x+2.0*lump_width, 1.0);
      x += 3.0*lump_width;
    }

  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*1.6, hei-scale*2.7-window_gap, scale*0.6, len-scale*1.6, hei-scale*2.7, wid-scale*0.6, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*0.6, hei-scale*2.7-window_gap, scale*1.6, len-scale*0.6, hei-scale*2.7, wid-scale*1.6, 1.0);

  /* front/back concrete columns and windows */
  num= (int)((len-scale*3.2)/(window_width+window_gap));
  x= scale*1.6+((len-scale*3.2)-num*(window_width+window_gap))/2.0;
  numy= (int)((hei-scale*2.7-2.0*window_gap-window_height-scale*3.5)/(window_height+window_bar));
  y= hei-scale*2.7-2.0*window_gap-window_height;
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*1.65, scale*3.5, scale*0.65, x+window_gap/2.0, hei-2.7-window_gap, wid-scale*0.65, 1.0);

  for (i= 1; i< num; i++)
    {
      Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, x+window_gap/2.0+window_width, scale*3.5, scale*0.65,
			x+window_gap/2.0+window_width+window_gap, hei-scale*2.7-window_gap, wid-scale*0.65, 1.0);
      vx[0]= x+window_gap/2.0; vy[0]= hei-scale*2.7-window_gap-window_height; vz[0]= scale*0.8;
      vx[1]= x+window_gap/2.0+window_width; vy[1]= hei-scale*2.7-window_gap-window_height; vz[1]= scale*0.8;
      vx[2]= x+window_gap/2.0+window_width; vy[2]= hei-scale*2.7-window_gap; vz[2]= scale*0.8;
      vx[3]= x+window_gap/2.0; vy[3]= hei-scale*2.7-window_gap; vz[3]= scale*0.8;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);
      vx[0]= x+window_gap/2.0; vy[0]= hei-scale*2.7-window_gap-window_height; vz[0]= wid-scale*0.8;
      vx[1]= x+window_gap/2.0; vy[1]= hei-scale*2.7-window_gap; vz[1]= wid-scale*0.8;
      vx[2]= x+window_gap/2.0+window_width; vy[2]= hei-scale*2.7-window_gap; vz[2]= wid-scale*0.8;
      vx[3]= x+window_gap/2.0+window_width; vy[3]= hei-scale*2.7-window_gap-window_height; vz[3]= wid-scale*0.8;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

      y= hei-scale*2.7-2.0*window_gap-window_height;
      for (j= 0; j< numy-1; j++)
	{
	  vx[0]= x+window_gap/2.0; vy[0]= y-window_height; vz[0]= scale*0.8;
	  vx[1]= x+window_gap/2.0+window_width; vy[1]= y-window_height; vz[1]= scale*0.8;
	  vx[2]= x+window_gap/2.0+window_width; vy[2]= y; vz[2]= scale*0.8;
	  vx[3]= x+window_gap/2.0; vy[3]= y; vz[3]= scale*0.8;
	  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

	  vx[0]= x+window_gap/2.0; vy[0]= y-window_height; vz[0]= wid-scale*0.8;
	  vx[1]= x+window_gap/2.0; vy[1]= y; vz[1]= wid-scale*0.8;
	  vx[2]= x+window_gap/2.0+window_width; vy[2]= y; vz[2]= wid-scale*0.8;
	  vx[3]= x+window_gap/2.0+window_width; vy[3]= y-window_height; vz[3]= wid-scale*0.8;
	  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

	  y -= window_height+window_bar;
	}
      vx[0]= x+window_gap/2.0; vy[0]= y-window_height; vz[0]= scale*0.8;
      vx[1]= x+window_gap/2.0+window_width; vy[1]= y-window_height; vz[1]= scale*0.8;
      vx[2]= x+window_gap/2.0+window_width; vy[2]= y; vz[2]= scale*0.8;
      vx[3]= x+window_gap/2.0; vy[3]= y; vz[3]= scale*0.8;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

      vx[0]= x+window_gap/2.0; vy[0]= y-window_height; vz[0]= wid-scale*0.8;
      vx[1]= x+window_gap/2.0; vy[1]= y; vz[1]= wid-scale*0.8;
      vx[2]= x+window_gap/2.0+window_width; vy[2]= y; vz[2]= wid-scale*0.8;
      vx[3]= x+window_gap/2.0+window_width; vy[3]= y-window_height; vz[3]= wid-scale*0.8;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

      if (i == 1) first_len_end= x+window_gap/2.0+window_width+window_gap;
      x += window_width+window_gap;
    }

  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, x+window_gap/2.0+window_width, scale*3.5, scale*0.65, len-scale*1.6, hei-scale*2.7-window_gap, wid-scale*0.65, 1.0);

  vx[0]= x+window_gap/2.0; vy[0]= hei-scale*2.7-window_gap-window_height; vz[0]= scale*0.8;
  vx[1]= x+window_gap/2.0+window_width; vy[1]= hei-scale*2.7-window_gap-window_height; vz[1]= scale*0.8;
  vx[2]= x+window_gap/2.0+window_width; vy[2]= hei-scale*2.7-window_gap; vz[2]= scale*0.8;
  vx[3]= x+window_gap/2.0; vy[3]= hei-scale*2.7-window_gap; vz[3]= scale*0.8;
  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

  vx[0]= x+window_gap/2.0; vy[0]= hei-scale*2.7-window_gap-window_height; vz[0]= wid-scale*0.8;
  vx[1]= x+window_gap/2.0; vy[1]= hei-scale*2.7-window_gap; vz[1]= wid-scale*0.8;
  vx[2]= x+window_gap/2.0+window_width; vy[2]= hei-scale*2.7-window_gap; vz[2]= wid-scale*0.8;
  vx[3]= x+window_gap/2.0+window_width; vy[3]= hei-scale*2.7-window_gap-window_height; vz[3]= wid-scale*0.8;
  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

  y= hei-scale*2.7-2.0*window_gap-window_height;
  for (j= 0; j< numy-1; j++)
    {
      vx[0]= x+window_gap/2.0; vy[0]= y-window_height; vz[0]= scale*0.8;
      vx[1]= x+window_gap/2.0+window_width; vy[1]= y-window_height; vz[1]= scale*0.8;
      vx[2]= x+window_gap/2.0+window_width; vy[2]= y; vz[2]= scale*0.8;
      vx[3]= x+window_gap/2.0; vy[3]= y; vz[3]= scale*0.8;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

      vx[0]= x+window_gap/2.0; vy[0]= y-window_height; vz[0]= wid-scale*0.8;
      vx[1]= x+window_gap/2.0; vy[1]= y; vz[1]= wid-scale*0.8;
      vx[2]= x+window_gap/2.0+window_width; vy[2]= y; vz[2]= wid-scale*0.8;
      vx[3]= x+window_gap/2.0+window_width; vy[3]= y-window_height; vz[3]= wid-scale*0.8;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

      y -= window_height+window_bar;
    }
  vx[0]= x+window_gap/2.0; vy[0]= y-window_height; vz[0]= scale*0.8;
  vx[1]= x+window_gap/2.0+window_width; vy[1]= y-window_height; vz[1]= scale*0.8;
  vx[2]= x+window_gap/2.0+window_width; vy[2]= y; vz[2]= scale*0.8;
  vx[3]= x+window_gap/2.0; vy[3]= y; vz[3]= scale*0.8;
  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);
  
  vx[0]= x+window_gap/2.0; vy[0]= y-window_height; vz[0]= wid-scale*0.8;
  vx[1]= x+window_gap/2.0; vy[1]= y; vz[1]= wid-scale*0.8;
  vx[2]= x+window_gap/2.0+window_width; vy[2]= y; vz[2]= wid-scale*0.8;
  vx[3]= x+window_gap/2.0+window_width; vy[3]= y-window_height; vz[3]= wid-scale*0.8;
  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

  last_len_end= x-window_gap/2.0;

  /* front/back horizontal struts */
  y= hei-scale*2.7-2.0*window_gap-window_height;
  for (j= 0; j< numy-1; j++)
    {
      Add_Relative_Box (sx,sy,sz, c,MAT_STRUTS,-1, first_len_end-window_gap-window_width, y-window_height-window_bar, scale*0.7,
			last_len_end+window_gap+window_width, y-window_height, wid-scale*0.7, 1.0);
      y -= window_height+window_bar;
    }
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, first_len_end-window_gap-window_width, scale*2.5, scale*0.6,
		    last_len_end+window_gap+window_width, y-window_height, wid-scale*0.6, 1.0);

  /* side windows and columns */
  num= (int)((wid-scale*3.2)/(window_width+window_gap));
  x= scale*1.6+((wid-scale*3.2)-num*(window_width+window_gap))/2.0;
  numy= (int)((hei-scale*2.7-2.0*window_gap-window_height-scale*3.5)/(window_height+window_bar));
  y= hei-scale*2.7-2.0*window_gap-window_height;
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*0.65, scale*3.5, scale*1.65,
		    len-scale*0.65, hei-scale*2.7-window_gap, x+window_gap/2.0, 1.0);
  for (i= 1; i< num; i++)
    {
      Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*0.65, scale*3.5, x+window_gap/2.0+window_width,
			len-scale*0.65, hei-scale*2.7-window_gap, x+window_gap/2.0+window_width+window_gap, 1.0);

      vx[0]= scale*0.8; vy[0]= hei-scale*2.7-window_gap-window_height; vz[0]= x+window_gap/2.0+window_width;
      vx[1]= scale*0.8; vy[1]= hei-scale*2.7-window_gap-window_height; vz[1]= x+window_gap/2.0;
      vx[2]= scale*0.8; vy[2]= hei-scale*2.7-window_gap; vz[2]= x+window_gap/2.0;
      vx[3]= scale*0.8; vy[3]= hei-scale*2.7-window_gap; vz[3]= x+window_gap/2.0+window_width;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

      vx[0]= len-scale*0.8; vy[0]= hei-scale*2.7-window_gap-window_height; vz[0]= x+window_gap/2.0+window_width;
      vx[1]= len-scale*0.8; vy[1]= hei-scale*2.7-window_gap; vz[1]= x+window_gap/2.0+window_width;
      vx[2]= len-scale*0.8; vy[2]= hei-scale*2.7-window_gap; vz[2]= x+window_gap/2.0;
      vx[3]= len-scale*0.8; vy[3]= hei-scale*2.7-window_gap-window_height; vz[3]=  x+window_gap/2.0;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

      y= hei-scale*2.7-2.0*window_gap-window_height;
      for (j= 0; j< numy-1; j++)
	{
	  vx[0]= scale*0.8; vy[0]= y-window_height; vz[0]= x+window_gap/2.0+window_width;
	  vx[1]= scale*0.8; vy[1]= y-window_height; vz[1]= x+window_gap/2.0;
	  vx[2]= scale*0.8; vy[2]= y; vz[2]= x+window_gap/2.0;
	  vx[3]= scale*0.8; vy[3]= y; vz[3]= x+window_gap/2.0+window_width;
	  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);
      
	  vx[0]= len-scale*0.8; vy[0]= y-window_height; vz[0]= x+window_gap/2.0+window_width;
	  vx[1]= len-scale*0.8; vy[1]= y; vz[1]= x+window_gap/2.0+window_width;
	  vx[2]= len-scale*0.8; vy[2]= y; vz[2]= x+window_gap/2.0;
	  vx[3]= len-scale*0.8; vy[3]= y-window_height; vz[3]= x+window_gap/2.0;
	  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);
	  y -= window_height+window_bar;
	}
      vx[0]= scale*0.8; vy[0]= y-window_height; vz[0]= x+window_gap/2.0+window_width;
      vx[1]= scale*0.8; vy[1]= y-window_height; vz[1]= x+window_gap/2.0;
      vx[2]= scale*0.8; vy[2]= y; vz[2]= x+window_gap/2.0;
      vx[3]= scale*0.8; vy[3]= y; vz[3]= x+window_gap/2.0+window_width;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);
      
      vx[0]= len-scale*0.8; vy[0]= y-window_height; vz[0]= x+window_gap/2.0+window_width;
      vx[1]= len-scale*0.8; vy[1]= y; vz[1]= x+window_gap/2.0+window_width;
      vx[2]= len-scale*0.8; vy[2]= y; vz[2]= x+window_gap/2.0;
      vx[3]= len-scale*0.8; vy[3]= y-window_height; vz[3]= x+window_gap/2.0;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

      if (i == 1) first_wid_end= x+window_gap/2.0+window_width+window_gap;
      x += window_width+window_gap;
    }

  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*0.6, scale*3.5, x+window_gap/2.0+window_width,
		    len-scale*0.6, hei-scale*2.7-window_gap, wid-scale*1.6, 1.0);

  vx[0]= scale*0.8; vy[0]= hei-scale*2.7-window_gap-window_height; vz[0]= x+window_gap/2.0+window_width;
  vx[1]= scale*0.8; vy[1]= hei-scale*2.7-window_gap-window_height; vz[1]= x+window_gap/2.0;
  vx[2]= scale*0.8; vy[2]= hei-scale*2.7-window_gap; vz[2]= x+window_gap/2.0;
  vx[3]= scale*0.8; vy[3]= hei-scale*2.7-window_gap; vz[3]= x+window_gap/2.0+window_width;
  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

  vx[0]= len-scale*0.8; vy[0]= hei-scale*2.7-window_gap-window_height; vz[0]= x+window_gap/2.0+window_width;
  vx[1]= len-scale*0.8; vy[1]= hei-scale*2.7-window_gap; vz[1]= x+window_gap/2.0+window_width;
  vx[2]= len-scale*0.8; vy[2]= hei-scale*2.7-window_gap; vz[2]= x+window_gap/2.0;
  vx[3]= len-scale*0.8; vy[3]= hei-scale*2.7-window_gap-window_height; vz[3]=  x+window_gap/2.0;
  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

  y= hei-scale*2.7-2.0*window_gap-window_height;
  for (j= 0; j< numy-1; j++)
    {
      vx[0]= scale*0.8; vy[0]= y-window_height; vz[0]= x+window_gap/2.0+window_width;
      vx[1]= scale*0.8; vy[1]= y-window_height; vz[1]= x+window_gap/2.0;
      vx[2]= scale*0.8; vy[2]= y; vz[2]= x+window_gap/2.0;
      vx[3]= scale*0.8; vy[3]= y; vz[3]= x+window_gap/2.0+window_width;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

      vx[0]= len-scale*0.8; vy[0]= y-window_height; vz[0]= x+window_gap/2.0+window_width;
      vx[1]= len-scale*0.8; vy[1]= y; vz[1]= x+window_gap/2.0+window_width;
      vx[2]= len-scale*0.8; vy[2]= y; vz[2]= x+window_gap/2.0;
      vx[3]= len-scale*0.8; vy[3]= y-window_height; vz[3]= x+window_gap/2.0;
      Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

      y -= window_height+window_bar;
    }

  vx[0]= scale*0.8; vy[0]= y-window_height; vz[0]= x+window_gap/2.0+window_width;
  vx[1]= scale*0.8; vy[1]= y-window_height; vz[1]= x+window_gap/2.0;
  vx[2]= scale*0.8; vy[2]= y; vz[2]= x+window_gap/2.0;
  vx[3]= scale*0.8; vy[3]= y; vz[3]= x+window_gap/2.0+window_width;
  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

  vx[0]= len-scale*0.8; vy[0]= y-window_height; vz[0]= x+window_gap/2.0+window_width;
  vx[1]= len-scale*0.8; vy[1]= y; vz[1]= x+window_gap/2.0+window_width;
  vx[2]= len-scale*0.8; vy[2]= y; vz[2]= x+window_gap/2.0;
  vx[3]= len-scale*0.8; vy[3]= y-window_height; vz[3]= x+window_gap/2.0;
  Add_Relative_Poly (sx,sy,sz, c,4, MAT_GLASS,-1, vx,vy,vz, NULL,NULL);

  last_wid_end= x-window_gap/2.0;


  /* side horizontal struts */
  y= hei-scale*2.7-2.0*window_gap-window_height;
  for (j= 0; j< numy-1; j++)
    {
      Add_Relative_Box (sx,sy,sz, c,MAT_STRUTS,-1, scale*0.7, y-window_height-window_bar, first_wid_end-window_gap-window_width,
			len-scale*0.7, y-window_height, last_wid_end+window_gap+window_width, 1.0);
      y -= window_height+window_bar;
    }
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*0.6, scale*2.5, first_wid_end-window_gap-window_width,
		    len-scale*0.6, y-window_height, last_wid_end+window_gap+window_width, 1.0);

  /* base */
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, 0.0, 0.0, 0.0, first_len_end+window_gap/2.0, scale*2.5, first_wid_end+window_gap/2.0, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, last_len_end-window_gap/2.0, 0.0, 0.0, len, scale*2.5, first_wid_end+window_gap/2.0, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, 0.0, 0.0, last_wid_end-window_gap/2.0, first_len_end+window_gap/2.0, scale*2.5, wid, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, last_len_end-window_gap/2.0, 0.0, last_wid_end-window_gap/2.0, len, scale*2.5, wid, 1.0);

  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*0.65, 0.0, scale*0.65, first_len_end+window_width+window_gap, scale*3.5, first_wid_end+window_width+window_gap, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, last_len_end-window_width-window_gap, 0.0, scale*0.65, len-scale*0.65, scale*3.5, first_wid_end+window_width+window_gap, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*0.65, 0.0, last_wid_end-window_width-window_gap, first_len_end+window_width+window_gap, scale*3.5, wid-scale*0.65, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, last_len_end-window_width-window_gap, 0.0, last_wid_end-window_width-window_gap, len-scale*0.65, scale*3.5, wid-scale*0.65, 1.0);

  /* doorways */
  Add_Relative_Box (sx,sy,sz, c,MAT_GLASS,-1, first_len_end+window_width+window_gap, 0.0, scale*0.7, last_len_end-window_width-window_gap, scale*2.5, wid-scale*0.7, 1.0);
  Add_Relative_Box (sx,sy,sz, c,MAT_GLASS,-1, scale*0.7, 0.0, first_wid_end+window_width+window_gap, len-scale*0.7, scale*2.5, last_wid_end-window_width-window_gap, 1.0);

  /* lod */
  Add_Relative_Lod_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*1.5,PAVE_HEIGHT,scale*1.5, len-scale*1.5,hei,wid-scale*1.5,0.0);

  /* occluder */
  Create_Occluder (c->cent.x+sx+scale*1.6,sy,c->cent.z+sz+scale*1.6, c->cent.x+sx+len-scale*1.6,sy+hei,c->cent.z+sz+wid-scale*1.6);
}

static void
Generate_Skyscraper
(MAV_cityCell *c)
{
  float sx,sy,sz;
  float ex,ez;
  float wid,len,hei;
  int type;
  int i;
  float bw;
  float scale;

  if (c->maxx-c->minx > 0 || c->maxy-c->miny > 0)
    Generate_Parametrised_Building (c);
  else
    {
      sx= c->nw.x > c->sw.x ? c->nw.x : c->sw.x;
      sy= PAVE_HEIGHT;
      sz= c->sw.z > c->se.z ? c->sw.z : c->se.z;
      ex= c->ne.x < c->se.x ? c->ne.x : c->se.x;
      ez= c->ne.z < c->nw.z ? c->ne.z : c->nw.z;

      wid= ez-sz;
      len= ex-sx;
      hei= c->height;
      scale= len/BLOCK_WIDTH;

      type= (int)(4*mav_random());
      switch (type)
	{
	case 0 :
	case 1 :
	case 2 :
	case 3 :
	  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, 1.5*scale,PAVE_HEIGHT,1.5*scale, len-1.5*scale,hei,wid-1.5*scale, 1.0);
	  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, 3.5*scale,hei,3.5*scale, len-3.5*scale,hei+2.0,wid-3.5*scale, 1.0);
	  Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, 1.2*scale,0.0,1.2*scale, len-1.2*scale,4.0,wid-1.2*scale, 1.0);
	  bw= (len-3.0)/15.0;
	  for (i= 0; i< 15; i++)
	    Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, 1.5*scale+i*bw+bw/4.0,4.0,1.25*scale, 1.5*scale+i*bw+bw/4.0+bw/2.0,hei-0.5,wid-1.25*scale, 1.0);
	  bw= (wid-3.0)/15.0;
	  for (i= 0; i< 15; i++)
	    Add_Relative_Box (sx,sy,sz, c,MAT_CONCRETE,-1, 1.25*scale,4.0,1.5+i*bw+bw/4.0, len-1.25*scale,hei-0.5,1.5*scale+i*bw+bw/4.0+bw/2.0, 1.0);
	  Create_Occluder (c->cent.x+sx+scale*1.5,sy,c->cent.z+sz+scale*1.5, c->cent.x+sx+len-scale*1.5,sy+hei,c->cent.z+sz+wid-scale*1.5);
	  Add_Relative_Lod_Box (sx,sy,sz, c,MAT_CONCRETE,-1, scale*1.5,PAVE_HEIGHT,scale*1.5, len-scale*1.5,hei,wid-scale*1.5,0.0);
	  break;
	}
    }
}

static void
Classify_Building
(MAV_cityCell *c)
{
  if (c->height > 20.0)
    {
#ifdef MAV_GL
      c->type= 2+(int)(3.0*mav_random());
      if (c->type == 4)
	c->type= 2+(int)(2.0*mav_random());
#else
      c->type= 1;
#endif
    }
  else
    {
      if (mav_random() < 0.25)
	c->type= 5; /* park */
      else
	{
	  c->type= 2+(int)(3.0*mav_random());
	  if (c->type == 4 && c->height > 6.0)
	    c->type= 2+(int)(2.0*mav_random());
	}
    }
}

static void
Find_Cell_BBox
(MAV_cityCell *c)
{
  Poly *p;
  MAV_object *obj;
  MAV_BB bb;
#ifdef COMPOSITES
  MAV_composite *comp;
#endif
  int i;

  c->bb.min.x= 10000000.0;
  c->bb.min.y= 10000000.0;
  c->bb.min.z= 10000000.0;
  c->bb.max.x= -10000000.0;
  c->bb.max.y= -10000000.0;
  c->bb.max.z= -10000000.0;

  p= c->polys;
  while (p)
    {
      for (i= 0; i< p->num; i++)
	{
	  if (p->verts[i].x < c->bb.min.x) c->bb.min.x= p->verts[i].x;
	  if (p->verts[i].y < c->bb.min.y) c->bb.min.y= p->verts[i].y;
	  if (p->verts[i].z < c->bb.min.z) c->bb.min.z= p->verts[i].z;
	  if (p->verts[i].x > c->bb.max.x) c->bb.max.x= p->verts[i].x;
	  if (p->verts[i].y > c->bb.max.y) c->bb.max.y= p->verts[i].y;
	  if (p->verts[i].z > c->bb.max.z) c->bb.max.z= p->verts[i].z;
	}
      p= p->next;
    }

#ifdef COMPOSITES
  mav_listPointerReset (c->composites);
  while (mav_listItemNext (c->composites, (void **)&obj))
    {
      comp= (MAV_composite *) obj->the_data;
      mav_compositeCalcBB (comp);
      mav_callbackBBExec (mav_win_current, obj, &bb);

      if (comp->bb.min.x < c->bb.min.x) c->bb.min.x= comp->bb.min.x;
      if (comp->bb.min.y < c->bb.min.y) c->bb.min.y= comp->bb.min.y;
      if (comp->bb.min.z < c->bb.min.z) c->bb.min.z= comp->bb.min.z;
      if (comp->bb.max.x > c->bb.max.x) c->bb.max.x= comp->bb.max.x;
      if (comp->bb.max.y > c->bb.max.y) c->bb.max.y= comp->bb.max.y;
      if (comp->bb.max.z > c->bb.max.z) c->bb.max.z= comp->bb.max.z;
    }
#endif
}

#ifndef DISPLAY_LISTS
static void
Sort_Cell_Polys
(MAV_cityCell *c, Poly **polys)
{
  Poly *p, *pnext, *plast;
  Poly *newp;
  int ptr;
  int current_texture, current_material;
  int num_polys_left;
  int num_textured_polys_left;
  int num_textured_lit_polys_left;

  p= *polys;
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

  newp= mav_malloc(num_polys_left*sizeof(Poly));
  ptr= 0;
  while (num_polys_left > 0)
    {
      current_texture= -1;
      current_material= -1;
      p= *polys;
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

	      /* copy over poly data */
	      memcpy ((void *)&newp[ptr], (void *)p, sizeof(Poly));
	      newp[ptr].drawn= 0;
	      newp[ptr].next= NULL;
	      if (ptr > 0)
		newp[ptr-1].next= &newp[ptr];
	      ptr++;
	    }

	  p= p->next;
	}
    }

  p= *polys;
  while (p)
    {
      pnext= p->next;
      free (p);
      p= pnext;
    }

  *polys= &newp[0];

  p= *polys;
  plast= NULL;
  current_texture= p->sp.texture;
  current_material= p->sp.material;
  while (p)
    {
      if (p->sp.texture != current_texture || p->sp.material != current_material)
	{
	  if (plast)
	    plast->drawn= 1;
	  current_texture= p->sp.texture;
	  current_material= p->sp.material;
	}

      plast= p;
      p= p->next;
    }
}
#endif

static void
Add_Waste_Ground
(int x, int y)
{
  MAV_cityCell *c;
  int okay= 0;

  if (x > 0)
    {
      if (city_scape[y*size+x-1].type != -1)
	okay= 1;

      if (y > 0)
	{
	  if (city_scape[(y-1)*size+x-1].type != -1)
	    okay= 1;
	}

      if (y < size-1)
	{
	  if (city_scape[(y+1)*size+x-1].type != -1)
	    okay= 1;
	}
    }

  if (x < size-1)
    {
      if (city_scape[y*size+x+1].type != -1)
	okay= 1;

      if (y > 0)
	{
	  if (city_scape[(y-1)*size+x+1].type != -1)
	    okay= 1;
	}

      if (y < size-1)
	{
	  if (city_scape[(y+1)*size+x+1].type != -1)
	    okay= 1;
	}
    }

  if (y > 0)
    {
      if (city_scape[(y-1)*size+x].type != -1)
	okay= 1;
    }

  if (y < size-1)
    {
      if (city_scape[(y+1)*size+x].type != -1)
	okay= 1;
    }

  if (okay)
    {
      c= mav_malloc(sizeof(MAV_cityCell));
      c->id= ++num_cells;
      c->building= 0;
      c->type= 99;
      c->num_polys= c->num_lod_polys= 0;
      c->polys= c->lod_polys= NULL;
#ifdef COMPOSITES
      c->composites= mav_listNew();
      c->features= mav_listNew();
#endif
#ifdef DISPLAY_LISTS
      c->cell_display_list=
	c->lod_display_list=
	c->composite_display_list=
	c->feature_display_list= 0;
#endif
      c->minx= c->maxx= x; c->miny= c->maxy= y;
      c->nw= Calc_Grid_Position (x*BLOCK_WIDTH/(size*BLOCK_WIDTH), (y+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
      c->sw= Calc_Grid_Position (x*BLOCK_WIDTH/(size*BLOCK_WIDTH), y*BLOCK_WIDTH/(size*BLOCK_WIDTH));
      c->se= Calc_Grid_Position ((x+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), y*BLOCK_WIDTH/(size*BLOCK_WIDTH));
      c->ne= Calc_Grid_Position ((x+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), (y+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
      c->cent.x= (c->nw.x+c->sw.x+c->se.x+c->ne.x)/4.0;
      c->cent.y= (c->nw.y+c->sw.y+c->se.y+c->ne.y)/4.0;
      c->cent.z= (c->nw.z+c->sw.z+c->se.z+c->ne.z)/4.0;
      c->nw.x -= c->cent.x; c->nw.y -= c->cent.y; c->nw.z -= c->cent.z;
      c->sw.x -= c->cent.x; c->sw.y -= c->cent.y; c->sw.z -= c->cent.z;
      c->se.x -= c->cent.x; c->se.y -= c->cent.y; c->se.z -= c->cent.z;
      c->ne.x -= c->cent.x; c->ne.y -= c->cent.y; c->ne.z -= c->cent.z;
      c->height= 0.0;

      c->matrix= mav_matrixSet(0.0,0.0,0.0, c->cent.x,0.0,c->cent.z);

      mav_listItemAdd (list_of_objects, (void *)mav_objectNew (mav_class_citycell, c));
      total_objects++;
    }
}

static void
Add_Street
(int x, int y)
{
  MAV_cityCell *c;

  c= mav_malloc(sizeof(MAV_cityCell));
  c->id= ++num_cells;
  c->building= 0;
  c->type= 0;
  c->num_polys= c->num_lod_polys= 0;
  c->polys= c->lod_polys= NULL;
#ifdef COMPOSITES
  c->composites= mav_listNew();
  c->features= mav_listNew();
#endif
#ifdef DISPLAY_LISTS
  c->cell_display_list=
    c->lod_display_list=
    c->composite_display_list=
    c->feature_display_list= 0;
#endif
  c->minx= c->maxx= x; c->miny= c->maxy= y;
  c->nw= Calc_Grid_Position (x*BLOCK_WIDTH/(size*BLOCK_WIDTH), (y+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
  c->sw= Calc_Grid_Position (x*BLOCK_WIDTH/(size*BLOCK_WIDTH), y*BLOCK_WIDTH/(size*BLOCK_WIDTH));
  c->se= Calc_Grid_Position ((x+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), y*BLOCK_WIDTH/(size*BLOCK_WIDTH));
  c->ne= Calc_Grid_Position ((x+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), (y+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
  c->cent.x= (c->nw.x+c->sw.x+c->se.x+c->ne.x)/4.0;
  c->cent.y= (c->nw.y+c->sw.y+c->se.y+c->ne.y)/4.0;
  c->cent.z= (c->nw.z+c->sw.z+c->se.z+c->ne.z)/4.0;
  c->nw.x -= c->cent.x; c->nw.y -= c->cent.y; c->nw.z -= c->cent.z;
  c->sw.x -= c->cent.x; c->sw.y -= c->cent.y; c->sw.z -= c->cent.z;
  c->se.x -= c->cent.x; c->se.y -= c->cent.y; c->se.z -= c->cent.z;
  c->ne.x -= c->cent.x; c->ne.y -= c->cent.y; c->ne.z -= c->cent.z;
  c->height= 0.0;

  c->matrix= mav_matrixSet (0.0,0.0,0.0, c->cent.x,0.0,c->cent.z);

  mav_listItemAdd (list_of_objects, (void *)mav_objectNew (mav_class_citycell, c));
  total_objects++;
}


#ifdef NEVER
static MAV_composite*
Generate_Street_Pole
(MAV_cityCell *c, int num, float x, float y)
{
  MAV_composite *comp;
  MAV_cylinder *cyl;

  comp= mav_malloc(sizeof(MAV_composite));
  comp->numobj= 1+3*num;
  comp->matrix= mav_matrixSet (0.0,0.0,0.0, c->cent.x+x,PAVE_HEIGHT,c->cent.z+y);
  comp->obj= mav_malloc(comp->numobj*sizeof(MAV_object*));
  mav_listItemAdd (c->composites, (void*)mav_objectNew (mav_class_composite, comp));

  cyl= mav_malloc(sizeof(MAV_cylinder));
  cyl->radius= 0.02;
  cyl->height= 2.2;
  cyl->nverts= 6;
  cyl->colouring= MAV_MATERIAL;
  cyl->colour= MAT_METAL;
  cyl->endcap= MAV_TRUE;
  cyl->matrix= mav_matrixSet(0.0,90.0,0.0, 0.0,1.1,0.0);
  comp->obj[0]= mav_objectNew (mav_class_cylinder, cyl);
  comp->numobj= 1;

  return comp;
}
#endif

#ifdef NEVER
static void
Add_Street_Sign
(MAV_cityCell *c, MAV_composite *comp, char *name, float x, float y, float rot, float hgt)
{
  MAV_text *text;
  MAV_box *box;
  MAV_matrix transform;
  float slen;
  int d;

  slen= 0.075*mavlib_slen (name, MAV_STROKE);

  text= mav_malloc(sizeof(MAV_text));
  text->text= strdup(name);
  text->colour= MAV_COLOUR_WHITE;
  text->style= MAV_STROKE;
  text->justify= MAV_JUSTIFY_CENTER;
  text->matrix= mav_matrixSet (0.0,0.0,180.0, slen/2.0, 2.05-hgt, -0.008);
  transform= mav_matrixSet (0.0,0.0,rot, 0.0,0.0,0.0);
  text->matrix= mav_matrixMult (transform, text->matrix);
  mav_scaleMatrix (&text->matrix, 0.1);
  comp->obj[comp->numobj]= mav_objectNew (mav_class_text, text);
  comp->numobj++;

  text= mav_malloc(sizeof(MAV_text));
  text->text= strdup(name);
  text->colour= MAV_COLOUR_WHITE;
  text->style= MAV_STROKE;
  text->justify= MAV_JUSTIFY_CENTER;  
  text->matrix= mav_matrixSet (0.0,0.0,0.0, slen/2.0, 2.05-hgt, 0.008);
  transform= mav_matrixSet (0.0,0.0,rot, 0.0,0.0,0.0);
  text->matrix= mav_matrixMult (transform, text->matrix);
  mav_scaleMatrix (&text->matrix, 0.1);
  comp->obj[comp->numobj]= mav_objectNew (mav_class_text, text);
  comp->numobj++;

  box= mav_malloc(sizeof(MAV_box));
  box->min.x= 0.0;
  box->min.y= 1.99-hgt;
  box->min.z= -0.004;
  box->max.x= slen;
  box->max.y= 2.1-hgt;
  box->max.z= 0.004;
  box->colouring= MAV_MATERIAL;
  box->colour= MAT_DARK_BLUE;
  box->matrix= mav_matrixSet (0.0,0.0,rot, 0.0,0.0,0.0);
  comp->obj[comp->numobj]= mav_objectNew (mav_class_box, box);
  comp->numobj++;
}
#endif

static void
Classify_Street_Cell
(MAV_cityCell *c, int x, int y)
{
  c->type= 0;

  if (city_scape[y*size+x].type == 1)
    return;

  if (y < size-1)
    {
      if (city_scape[(y+1)*size+x].type == 0)
	c->type += 1;
    }

  if (x < size-1)
    {
      if (city_scape[y*size+x+1].type == 0)
	c->type += 2;
    }

  if (y > 0)
    {
      if (city_scape[(y-1)*size+x].type == 0)
	c->type += 4;
    }

  if (x > 0)
    {
      if (city_scape[y*size+x-1].type == 0)
	c->type += 8;
    }
}

static void
Add_Street_Signs
(MAV_cityCell *c)
{
  return;

#if 0
  MAV_vector pos[12];
  int x,y;
  char name[255];
  int len;
  MAV_composite *comp;

  x= c->minx;
  y= c->miny;

  Map(c, pos);

  switch (c->type)
    {
    case 1 :
      comp= Generate_Street_Pole (c, 1, pos[7].x,pos[7].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[7].x,pos[7].z, 90.0, 0.0);
      break;
    case 2 :
      comp= Generate_Street_Pole (c, 1, pos[1].x,pos[1].z);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[1].x,pos[1].z, 0.0, 0.0);
      break;
    case 3 :
      comp= Generate_Street_Pole (c, 2, pos[7].x,pos[7].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[7].x,pos[7].z, 90.0, 0.0);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[7].x,pos[7].z, 0.0, 0.12);
      break;
    case 4 :
      comp= Generate_Street_Pole (c, 1, pos[4].x,pos[4].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[4].x,pos[4].z, -90.0, 0.0);
      break;
    case 6 :
      comp= Generate_Street_Pole (c, 2, pos[1].x,pos[1].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[1].x,pos[1].z, -90.0, 0.0);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[1].x,pos[1].z, 0.0, 0.12);
      break;
    case 7 :
      comp= Generate_Street_Pole (c, 1, pos[9].x,pos[9].z);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[9].x,pos[9].z, 0.0, 0.0);

      comp= Generate_Street_Pole (c, 1, pos[4].x,pos[4].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[4].x,pos[4].z, 270.0, 0.0);
      break;
    case 8 :
      comp= Generate_Street_Pole (c, 1, pos[9].x,pos[9].z);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[9].x,pos[9].z, 180.0, 0.0);
      break;
    case 9 :
      comp= Generate_Street_Pole (c, 2, pos[9].x,pos[9].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[9].x,pos[9].z, 90.0, 0.0);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[9].x,pos[9].z, 180.0, 0.12);
      break;
    case 11 :
      comp= Generate_Street_Pole (c, 1, pos[1].x,pos[1].z);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[1].x,pos[1].z, 0.0, 0.0);

      comp= Generate_Street_Pole (c, 1, pos[4].x,pos[4].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[4].x,pos[4].z, 90.0, 0.0);
      break;
    case 12 :
      comp= Generate_Street_Pole (c, 2, pos[4].x,pos[4].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[4].x,pos[4].z, -90.0, 0.0);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[4].x,pos[4].z, 180.0, 0.12);
      break;
    case 13 :
      comp= Generate_Street_Pole (c, 1, pos[7].x,pos[7].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[7].x,pos[7].z, 90.0, 0.0);

      comp= Generate_Street_Pole (c, 1, pos[1].x,pos[1].z);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[1].x,pos[1].z, 180.0, 0.0);
      break;
    case 14 :
      comp= Generate_Street_Pole (c, 1, pos[9].x,pos[9].z);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[9].x,pos[9].z, 180.0, 0.0);

      comp= Generate_Street_Pole (c, 1, pos[7].x,pos[7].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[7].x,pos[7].z, -90.0, 0.0);
      break;
    case 15 :
      comp= Generate_Street_Pole (c, 1, pos[1].x,pos[1].z);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[1].x,pos[1].z, 180.0, 0.0);

      comp= Generate_Street_Pole (c, 1, pos[7].x,pos[7].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[7].x,pos[7].z, 90.0, 0.0);

      comp= Generate_Street_Pole (c, 1, pos[9].x,pos[9].z);
      strcpy(name, street_names[y]);
      len= strlen(street_names[y]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(5.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[9].x,pos[9].z, 0.0, 0.0);

      comp= Generate_Street_Pole (c, 1, pos[4].x,pos[4].z);
      strcpy(name, street_names[40+x]);
      len= strlen(street_names[40+x]);
      strcpy(&name[len], " ");
      strcpy(&name[len+1], street_endings[(int)(2.0*mav_random())]);
      name[len+4]= 0;
      Add_Street_Sign (c, comp, name, pos[4].x,pos[4].z, -90.0, 0.0);
      break;
    default :
      break;
    }
#endif
}

static int
Add_Building
(int sx, int sy, int ex, int ey, int *type)
{
  MAV_cityCell *c;
  int i,j;

  c= mav_malloc(sizeof(MAV_cityCell));
  c->id= ++num_cells;
  c->building= 1;
  c->type= 0;
  c->num_polys= c->num_lod_polys= 0;
  c->polys= c->lod_polys= NULL;
#ifdef COMPOSITES
  c->composites= mav_listNew();
  c->features= mav_listNew();
#endif
#ifdef DISPLAY_LISTS
  c->cell_display_list=
    c->lod_display_list=
    c->composite_display_list=
    c->feature_display_list= 0;
#endif
  c->minx= sx; c->miny= sy;
  c->maxx= ex; c->maxy= ey;
  c->nw= Calc_Grid_Position (sx*BLOCK_WIDTH/(size*BLOCK_WIDTH), (ey+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
  c->sw= Calc_Grid_Position (sx*BLOCK_WIDTH/(size*BLOCK_WIDTH), sy*BLOCK_WIDTH/(size*BLOCK_WIDTH));
  c->se= Calc_Grid_Position ((ex+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), sy*BLOCK_WIDTH/(size*BLOCK_WIDTH));
  c->ne= Calc_Grid_Position ((ex+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH), (ey+1)*BLOCK_WIDTH/(size*BLOCK_WIDTH));
  c->cent.x= (c->nw.x+c->sw.x+c->se.x+c->ne.x)/4.0;
  c->cent.y= (c->nw.y+c->sw.y+c->se.y+c->ne.y)/4.0;
  c->cent.z= (c->nw.z+c->sw.z+c->se.z+c->ne.z)/4.0;
  c->nw.x -= c->cent.x; c->nw.y -= c->cent.y; c->nw.z -= c->cent.z;
  c->sw.x -= c->cent.x; c->sw.y -= c->cent.y; c->sw.z -= c->cent.z;
  c->se.x -= c->cent.x; c->se.y -= c->cent.y; c->se.z -= c->cent.z;
  c->ne.x -= c->cent.x; c->ne.y -= c->cent.y; c->ne.z -= c->cent.z;

  /* find height */
  c->height= 0.0;
  for (j= sy; j<= ey; j++)
    for (i= sx; i<= ex; i++)
      {
	if (building_heights[j*size+i] > c->height)
	  c->height= building_heights[j*size+i];
      }

  Classify_Building (c);

  c->matrix= mav_matrixSet (0.0,0.0,0.0, c->cent.x,0.0,c->cent.z);

  mav_listItemAdd (list_of_objects, (void *)mav_objectNew (mav_class_citycell, c));
  total_objects++;

  *type= c->type;
  return (c->id);
}

static void
Output_Cityscape (void)
{
  int i,j;

  for (j= size-1; j >= 0; j--)
    {
      fprintf(stdout, "%2d: ", j);

      for (i= 0; i< size; i++)
	{
	  if (city_scape[j*size+i].type == 0)
	    fprintf(stdout, "S");
	  else if (city_scape[j*size+i].type == 1)
	    fprintf(stdout, "B");
	  else
	    fprintf(stdout, ".");
	}
      fprintf(stdout, "\n");
    }
}

void
Build_Cityscape (void)
{
  int i,j;
  int n;
  int x,y;
  int okay;
  int startx,starty;
  int new_starty;
  int endx,endy;
  MAV_cityCell *s;
  float hgt;
  int id;
  MAV_object *obj;
  int type;

  road_width= (BLOCK_WIDTH-2.0*PAVE_WIDTH)/2.0;
  /*  srand(0);*/

  n= vcb_main (3,500);
  size= SPACE_SIZE;

  Init_Warp_Grid();

  list_of_objects= mav_listNew();

  city_scape= mav_malloc(size*size*sizeof(Block));
  building_heights= mav_malloc(size*size*sizeof(float));

  /* blank city to begin with */
  for (j= 0; j< size; j++)
    for (i= 0; i< size; i++)
      {
	city_scape[j*size+i].type= -1; /* empty */
	city_scape[j*size+i].id= -1;
      }

  /* add buildings and streets */
  for (i= 1; i<= n; i++)
    {
      city_scape[cells[i][1]*size+cells[i][0]].type= 0; /* street */
      city_scape[cells[i][3]*size+cells[i][2]].type= 1; /* building */
      city_scape[cells[i][1]*size+cells[i][0]].id= -1;
      city_scape[cells[i][3]*size+cells[i][2]].id= -1;
      building_heights[cells[i][3]*size+cells[i][2]]= 5.0+pow((float)(n-i)/(float)(n-1),3.0)*35.0;
    }

  /* find buildings */
  for (y= 0; y< size; y++)
    for (x= 0; x< size; x++)
      {
	if (city_scape[y*size+x].type == 1 && city_scape[y*size+x].id == -1)
	  {
	    /* found a new building */
	    startx= endx= x; starty= endy= y;
	    hgt= building_heights[starty*size+endx];
	    
	    /* scan horizontally to find building length */
	    okay= 1;
	    while (okay && city_scape[starty*size+endx].type == 1 && city_scape[starty*size+endx].id == -1 &&
		   (fabs(building_heights[starty*size+endx]-hgt) <= MAX_HEIGHT_DIFF) && endx-startx <= MAX_BUILDING_LEN)
	      {
		endx++;

		if (endx == size || city_scape[starty*size+endx].type != 1 || city_scape[starty*size+endx].id != -1 ||
		    (fabs(building_heights[starty*size+endx]-hgt) > MAX_HEIGHT_DIFF) || endx-startx > MAX_BUILDING_LEN)
		  {
		    endx--;
		    okay= 0;
		  }
	      }

	    if (endx-startx >= 1)
	      {
		/* scan vertically down to find building height */
		okay= 1;
		for (j= starty; okay && j<= starty+MAX_BUILDING_LEN; j++)
		  {
		    okay= 1;
		    for (i= startx; okay && i<= endx; i++)
		      {
			if (city_scape[j*size+i].type != 1 || city_scape[j*size+i].id != -1 ||
			    (fabs(building_heights[j*size+i]-hgt) > MAX_HEIGHT_DIFF))
			  okay= 0;
		      }
		    
		    if (okay)
		      endy= j;

		    if (j == size-1)
		      okay= 0;
		  }

		/* scan vertically upwards if possible */
		okay= 1;
		new_starty= starty;
		for (j= starty; okay && j > endy-MAX_BUILDING_LEN; j--)
		  {
		    okay= 1;
		    for (i= startx; okay && i <= endx; i++)
		      {
			if (city_scape[j*size+i].type != 1 || city_scape[j*size+i].id != -1 ||
			    (fabs(building_heights[j*size+i]-hgt) > MAX_HEIGHT_DIFF))
			  okay= 0;
		      }

		    if (okay)
		      new_starty= j;

		    if (endy-new_starty > MAX_BUILDING_LEN || new_starty == 0)
		      okay= 0;
		  }

		starty= new_starty;
		
		if (endy-starty >= 1)
		  {
		    id= Add_Building (startx,starty,endx,endy,&type);
		    
		    for (j= starty; j<= endy; j++)
		      for (i= startx; i<= endx; i++)
			{
			  if (type != 5)
			    Add_Street (i,j);
			  city_scape[j*size+i].id= id;
			}
		  }
	      }
	  }
      }

  okay= 1;
  while (okay)
    {
      okay= 0;
      for (j= 0; okay == 0 && j< size; j++)
	for (i= 0; okay == 0 && i< size; i++)
	  {
	    if (city_scape[j*size+i].type == 1 && city_scape[j*size+i].id == -1)
	      okay= 1;
	  }
      
      if (okay)
	{
	  startx= endx= i-1; starty= endy= j-1;
	  hgt= building_heights[starty*size+endx];

	  okay= 1;
	  while (okay && city_scape[starty*size+endx].type == 1 && city_scape[starty*size+endx].id == -1 &&
		 (fabs(building_heights[starty*size+endx]-hgt) <= MAX_HEIGHT_DIFF) && endx-startx <= MAX_BUILDING_LEN)
	    {
	      endx++;
	      
	      if (endx == size || city_scape[starty*size+endx].type != 1 || city_scape[starty*size+endx].id != -1 ||
		  (fabs(building_heights[starty*size+endx]-hgt) > MAX_HEIGHT_DIFF) || endx-startx > MAX_BUILDING_LEN)
		{
		  endx--;
		  okay= 0;
		}
	    }

	  okay= 1;
	  for (j= starty; okay && j<= starty+MAX_BUILDING_LEN; j++)
	    {
	      okay= 1;
	      for (i= startx; okay && i<= endx; i++)
		{
		  if (city_scape[j*size+i].type != 1 || city_scape[j*size+i].id != -1 ||
		      (fabs(building_heights[j*size+i]-hgt) > MAX_HEIGHT_DIFF))
		    okay= 0;
		}

	      if (okay)
		endy= j;

	      if (j == size-1)
		okay= 0;
	    }

	  id= Add_Building (startx,starty,endx,endy,&type);

	  for (j= starty; j<= endy; j++)
	    for (i= startx; i<= endx; i++)
	      {
		city_scape[j*size+i].id= id;
		if (type != 5)
		  Add_Street (i,j);
	      }

	  okay= 1;
	}
    }

  /* add streets */
  for (y= 0; y< size; y++)
    for (x= 0; x< size; x++)
      {
	if (city_scape[y*size+x].type == 0)
	  Add_Street (x,y);
	else if (city_scape[y*size+x].type == -1)
	  Add_Waste_Ground (x,y);
      }

  Output_Cityscape();

  mav_listPointerReset (list_of_objects);
  while (mav_listItemNext (list_of_objects, (void **)&obj))
    {
      if (obj->the_class == mav_class_citycell)
	{
	  s= (MAV_cityCell *) obj->the_data;
	  
	  if (s->building == 0 && s->type != 99)
	    Classify_Street_Cell (s, s->minx,s->miny);
	}
    }

  /* generate cell contents */
  mav_listPointerReset (list_of_objects);
  while (mav_listItemNext (list_of_objects, (void **)&obj))
    {
      if (obj->the_class == mav_class_citycell)
	{
	  s= (MAV_cityCell *) obj->the_data;

	  if (s->building == 0)
	    {
	      Generate_Street_Polys (s);
#ifdef COMPOSITES
	      Add_Street_Signs (s);
#endif
	      if (bounce_cell == NULL)
		{
		  bounce_cell= s;
		  fprintf(stdout, "bounce_cell= %d\n", s->id);
		}
	    }
	  else
	    {
	      switch (s->type)
		{
		case 1 :
		  Generate_Skyscraper (s);
		  break;
		case 2 :
		  Generate_Textured_Building (s);
		  break;
		case 3 :
		  Generate_Textured_Building2 (s);
		  break;
		case 4 :
		  Generate_Small_Building (s);
		  break;
		case 5 :
		  Generate_Park (s);
		  break;
		default :
		  break;
		}
	    }
      
	  Find_Cell_BBox (s);
#ifndef DISPLAY_LISTS
	  Sort_Cell_Polys (s, &s->polys);
	  if (s->lod_polys)
	    Sort_Cell_Polys (s, &s->lod_polys);
#endif
	}
    }

  Generate_Park_Features();
}
