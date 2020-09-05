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

/*
  The Distributed Legible City

  Implemented by various people at ZKM (http://www.zkm.de) and 
  the Advanced Interfaces Group (http://aig.cs.man.ac.uk)

  Primary ZKM author:
   Andreas Schiffler,
   ZKM, Lorenzstr. 19, 76135 Karlsruhe, Germany
   schiffler@zkm.de

  AIG authors:
   See http://aig.cs.man.ac.uk


  AIG_UPDATE 2
  
  ZKM Nov 26: Bike-control with autocentering and conf file - added.
  ZKM Nov 26: Wrong texture beeing loaded - fixed.
*/

#include "maverik.h"
#include <stdio.h>
#include <stdlib.h>
#include "lcity2b.h"
#include "lcity2.h"
#include <math.h>
#include "lcity2_colour.h"
#include <string.h>
#if defined(WIN32) || defined(__CYGWIN__)
#include <windows.h>
#endif
#include <GL/glu.h>
#if 0
#include <sys/time.h>
#endif

#include "build.h"

#undef INSERT_STREETS

#define RATE_TIME (1.0/12.0)

#define USE_SKY 1
#define TEX_FEATURES 1
#define SQUARE_THING 1000000

MAV_class *mav_class_lcity;

int apply_lod=1;

#define fog_distance 200.0
#define box_distance 150.0
 
GLfloat fogColour[4]={ 0.0, 0.0, 0.0, 1.0 };
int white_index;

#define COL_OFFSET	50

/* Check if we are on a feature */

int current_feature=-1;

#define TEX_SKY 1
#define TEX_FLOOR 2
#define TEX_GRASS 3
#define TEX_CANAL 4
#define TEX_STONE 4

#define COL_WHITE 1


#define TEX_TRANNY 598

#define TEX_GRID_SCALE 10.0

MAV_surfaceParams *sky=NULL;
MAV_surfaceParams *lcfloor=NULL;
MAV_surfaceParams *tranny=NULL;
MAV_surfaceParams *map_white=NULL;
MAV_surfaceParams *grass=NULL;
MAV_surfaceParams *canal=NULL;
MAV_surfaceParams *stone=NULL;

#define MAP_ZOOM 	590
#define MAP_OFFSET 	-600.0
float map_scale=0.11;

int drawMap=1;

MAV_class *mav_class_character;
MAV_class *mav_class_feature;
MAV_class *mav_class_vector;

MAV_BB city_bb;
MAV_SMS *city_build;
MAV_SMS *city_sms;
MAV_SMS *city_map;

#define STREET_WIDTH 20.0

#define STRUCTURE_SCALE 0.75

#define ORIGIN_SCALE 0.01

#define MAX_CITY	3

Alphabet alphabet[255];

#define NUM_LETTERS 65

#define MAX_FEATURE_POINTS	14

/* Current city we are working on */
static int citycount;

/* Center of Mass of city */
static float comx[3],comy[3];

/* Database filenames */
static char basefilename[3][80]={"manhattan","amsterdam","karlsruhe"};

/* Rotational correction of cities */
static float cityangle[3]={0.0,-90.0,-200.0};
static float angle,cangle,sangle;

/* For some statistics */
static int num_polygons;

static int ascii[NUM_LETTERS] = {
  33, 38, 39, 40, 41, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,
  56, 57, 58, 61, 63, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
  77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 96, 128, 130,
  131, 133, 136, 138, 140, 141, 142, 147, 149, 150, 151, 153, 154, 225 };

static float letter_widths[NUM_LETTERS+1][2] = {
  { 32.0, 0.8 },
  { 33.0, 0.3277 },
  { 38.0, 1.0217 },
  { 39.0, 0.2121 },
  { 40.0, 0.3374 },
  { 41.0, 0.3374 },
  { 44.0, 0.2313 },
  { 45.0, 0.6169 },
  { 46.0, 0.2313 },
  { 47.0, 0.7615 },
  { 48.0, 0.7615 },
  { 49.0, 0.5783 },
  { 50.0, 0.7615 },
  { 51.0, 0.6458 },
  { 52.0, 0.9350 },
  { 53.0, 0.7711 },
  { 54.0, 0.7615 },
  { 55.0, 0.6651 },
  { 56.0, 0.7615 },
  { 57.0, 0.7615 },
  { 58.0, 0.2313 },
  { 61.0, 0.6169 },
  { 63.0, 0.7615 },
  { 65.0, 0.8000 },
  { 66.0, 0.7615 },
  { 67.0, 0.7615 },
  { 68.0, 0.7615 },
  { 69.0, 0.6169 },
  { 70.0, 0.5783 },
  { 71.0, 0.7615 },
  { 72.0, 0.7615 },
  { 73.0, 0.2410 },
  { 74.0, 0.7615 },
  { 75.0, 0.8000 },
  { 76.0, 0.5494 },
  { 77.0, 1.0217 },
  { 78.0, 0.8000 },
  { 79.0, 0.7615 },
  { 80.0, 0.7615 },
  { 81.0, 0.8771 },
  { 82.0, 0.7615 },
  { 83.0, 0.7615 },
  { 84.0, 0.8675 },
  { 85.0, 0.7615 },
  { 86.0, 0.8000 },
  { 87.0, 1.3302 },
  { 88.0, 0.8964 },
  { 89.0, 0.7904 },
  { 90.0, 0.6651 },
  { 96.0, 0.2121 },
  { 128.0, 0.7615 },
  { 130.0, 0.6169 },
  { 131.0, 0.8000 },
  { 133.0, 0.8000 },
  { 136.0, 0.6169 },
  { 138.0, 0.6169 },
  { 140.0, 0.6169 },
  { 141.0, 0.2410 },
  { 142.0, 0.8000 },
  { 147.0, 0.7615 },
  { 149.0, 0.7615 },
  { 150.0, 0.7615 },
  { 151.0, 0.7615 },
  { 153.0, 0.7615 },
  { 154.0, 0.7615 },
  { 225.0, 0.7615 }
};

/* Support routines: point inside polygon */
/* -------------------------------------- */

/* Determine if a point is inside a 2D polygon */
/* A. Schiffler, 1998 */

/* 
 Original code example from FAQ
*/

int pnpoly(int npol, float *xp, float *yp, float x, float y)
{
 int i, j, c = 0;

 for (i = 0, j = npol-1; i < npol; j = i++) {
  if ((((yp[i]<=y) && (y<yp[j])) || ((yp[j]<=y) && (y<yp[i]))) &&
     (x < (xp[j] - xp[i]) * (y - yp[i]) / (yp[j] - yp[i]) + xp[i])) {
   c = !c;
  } 
 }
 return c;
}

/* Maverik code using MAV_vector types */

int testpoly_xy(int npol, MAV_vector *polyverts, MAV_vector testpoint)
{
 int i, j, c = 0;

 for (i = 0, j = npol-1; i < npol; j = i++) {
  if ((((polyverts[i].y<=testpoint.y) && (testpoint.y<polyverts[j].y)) || ((polyverts[j].y<=testpoint.y) && (testpoint.y<polyverts[i].y))) &&
     (testpoint.x < (polyverts[j].x - polyverts[i].x) * (testpoint.y - polyverts[i].y) / (polyverts[j].y - polyverts[i].y) + polyverts[i].x)) {
   c = !c;
  } 
 }
 return c;
}

int testpoly_xz(int npol, MAV_vector *polyverts, MAV_vector testpoint)
{
 int i, j, c = 0;

 for (i = 0, j = npol-1; i < npol; j = i++) {
  if ((((polyverts[i].z<=testpoint.z) && (testpoint.z<polyverts[j].z)) || ((polyverts[j].z<=testpoint.z) && (testpoint.z<polyverts[i].z))) &&
     (testpoint.x < (polyverts[j].x - polyverts[i].x) * (testpoint.z - polyverts[i].z) / (polyverts[j].z - polyverts[i].z) + polyverts[i].x)) {
   c = !c;
  } 
 }
 return c;
}

int testpoly_yz(int npol, MAV_vector *polyverts, MAV_vector testpoint)
{
 int i, j, c = 0;

 for (i = 0, j = npol-1; i < npol; j = i++) {
  if ((((polyverts[i].z<=testpoint.z) && (testpoint.z<polyverts[j].z)) || ((polyverts[j].z<=testpoint.z) && (testpoint.z<polyverts[i].z))) &&
     (testpoint.y < (polyverts[j].y - polyverts[i].y) * (testpoint.z - polyverts[i].z) / (polyverts[j].z - polyverts[i].z) + polyverts[i].y)) {
   c = !c;
  } 
 }
 return c;
}

/* 

Check if input coordinates are inside or outside the boudary. 
If they are outside, coordinates are changed such that they lie on the 
boudary. The boundary consists of 4 circles defined in build.h.

*/

/* A. Schiffler, 1998 */

void LineCircleIntersect(float xp, float yp, float xc, float yc, float r, float *x, float *y)
{
 float angle;
 
 angle=atan2((yp-yc),(xp-xc));
 *x=xc+r*cos(angle);
 *y=yc+r*sin(angle);
}

/* Returns 0 if coordinates are inside and have not been changed. */
/* Returns 1 of coordinates are outside and have been modified. */

int keep_inside_bounds (MAV_vector *location)
{
/* Change -- ziegi@zkm.de
   let the bikes just peek over the rim of the city
   The RIM is a factor that just enlarges the city space 
   */
#define RIM 1.0   

 float dx,dz;
 float radius;
 float x,z,newx=0,newz=0,d,dmin;
 
 /* Test inner circle */ 
 dx= location->x;
 dz=-location->z;
 radius=RIM*global_city_mre;
 
 if ((dx*dx+dz*dz)<(radius*radius)) {
  return(0);
 } else {
  /* Test outer circles */
  /* City 0 */
  dx= location->x-centx[0];
  dz=-location->z-centy[0];
  radius=RIM*city_mre[0];
  if ((dx*dx+dz*dz)<(radius*radius)) {
   return(0);
  } else {
   /* City 1 */
   dx= location->x-centx[1];
   dz=-location->z-centy[1];
   radius=RIM*city_mre[1];
   if ((dx*dx+dz*dz)<(radius*radius)) {
    return(0);
   } else {
    /* City 2 */
    dx= location->x-centx[2];
    dz=-location->z-centy[2];
    radius=RIM*city_mre[2];
    if ((dx*dx+dz*dz)<(radius*radius)) {
     return(0);
    } else {
     dmin=1.0e20;
     /* Check against main circle */
     LineCircleIntersect(location->x,-location->z,0.0,0.0,global_city_mre,&x,&z);
     dx= location->x-x;
     dz=-location->z-z;
     d=dx*dx+dz*dz;
     if (d<dmin) {
      newx=x;
      newz=z;
      dmin=d;
     }
     /* Check against city 1 circle */
     LineCircleIntersect(location->x,-location->z,centx[0],centy[0],city_mre[0],&x,&z);
     dx= location->x-x;
     dz=-location->z-z;
     d=dx*dx+dz*dz;
     if (d<dmin) {
      newx=x;
      newz=z;
      dmin=d;
     }
     /* Check against city 2 circle */
     LineCircleIntersect(location->x,-location->z,centx[1],centy[1],city_mre[1],&x,&z);
     dx= location->x-x;
     dz=-location->z-z;
     d=dx*dx+dz*dz;
     if (d<dmin) {
      newx=x;
      newz=z;
      dmin=d;
     }
     /* Check against city 3 circle */
     LineCircleIntersect(location->x,-location->z,centx[2],centy[2],city_mre[2],&x,&z);
     dx= location->x-x;
     dz=-location->z-z;
     d=dx*dx+dz*dz;
     if (d<dmin) {
      newx=x;
      newz=z;
     }
     /* Update location */
     location->x= newx;
     location->z=-newz;
     return(1);
    } 
   } 
  } 
 } 
}

/* LC primites   */
/* ------------- */

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
	mav_BBDrawWithSurfaceParams(mav_win_current, c->character->bb, c->sp);
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

#if 0 /* JMC */
int mav_subdivideDraw(MAV_vector *triangle, int depth)
{
 MAV_vector norm;
 MAV_vector newtriangle[3];
 MAV_vector S;
 int newdepth;
 
 if (depth==0) {
  mav_gfxPolygonBegin();
  norm.x=0.0; norm.y=1.0; norm.z=0.0;
  mav_gfxNormal(norm);
  mav_gfxVertex(triangle[0]);
  mav_gfxVertex(triangle[1]);
  mav_gfxVertex(triangle[2]);
  mav_gfxPolygonEnd();
 } else {
  /* New level */
  newdepth=depth-1;
  /* Calculate center of mass */
  S.x=0.3333333*(triangle[0].x + triangle[1].x + triangle[2].x);
  S.y=triangle[0].y;
  S.z=0.3333333*(triangle[0].z + triangle[1].z + triangle[2].z);
  /* Generate three new triangles */
  newtriangle[0]=triangle[0];
  newtriangle[1]=triangle[1];
  newtriangle[2]=S;
  mav_subdivideDraw(newtriangle, newdepth);
  newtriangle[0]=S;
  newtriangle[1]=triangle[1];
  newtriangle[2]=triangle[2];
  mav_subdivideDraw(newtriangle, newdepth);
  newtriangle[0]=S;
  newtriangle[1]=triangle[2];
  newtriangle[2]=triangle[0];
  mav_subdivideDraw(newtriangle, newdepth);
 }
}
#else
void mav_subdivideDraw(MAV_vector a, MAV_vector b, MAV_vector c)
{
#if 0 /* JMC */
  float area;

  area= fabs(0.5*((b.x-a.x)*(c.z-a.z)-(c.x-a.x)*(b.z-a.z)));

  if (area>500.0) 
#else
  float l1, l2, l3;
  
  l1= fabs(mav_vectorDotProduct(mav_vectorSub(b, a), mav_vectorSub(b, a)));
  l2= fabs(mav_vectorDotProduct(mav_vectorSub(c, a), mav_vectorSub(c, a)));
  l3= fabs(mav_vectorDotProduct(mav_vectorSub(b, c), mav_vectorSub(b, c)));

  if (l1>SQUARE_THING || l2>SQUARE_THING || l3>SQUARE_THING)
#endif
  {
    /* subdivide into 4, calc midpoints of edges */
    MAV_vector ab= mav_vectorScalar(mav_vectorAdd(a,b),0.5);
    MAV_vector ac= mav_vectorScalar(mav_vectorAdd(a,c),0.5);
    MAV_vector bc= mav_vectorScalar(mav_vectorAdd(b,c),0.5);

    mav_subdivideDraw(a, ab, ac);
    mav_subdivideDraw(ab, bc, ac);
    mav_subdivideDraw(ab, b, bc);
    mav_subdivideDraw(ac, bc, c);
  }
  else
  {
    /* draw */
    MAV_texCoord t;

#ifdef TEX_FEATURES
    t.s= a.x/TEX_GRID_SCALE;
    t.t= a.z/TEX_GRID_SCALE;
    mav_gfxTexCoord(t);
#endif
    mav_gfxVertex(a);

#ifdef TEX_FEATURES
    t.s= b.x/TEX_GRID_SCALE;
    t.t= b.z/TEX_GRID_SCALE;
    mav_gfxTexCoord(t);
#endif
    mav_gfxVertex(b);

#ifdef TEX_FEATURES
    t.s= c.x/TEX_GRID_SCALE;
    t.t= c.z/TEX_GRID_SCALE;
    mav_gfxTexCoord(t);
#endif
    mav_gfxVertex(c);
  }
}
#endif

int mav_featureDraw(MAV_object *obj, MAV_drawInfo *di)
{
  Feature *f= (Feature *) mav_objectDataGet(obj);
  int i;
  MAV_vector dr;
  float dist;

  /* Calculate distance from edge of feature circle to eye point */
  dr=mav_vectorSub(f->center, mav_win_current->vp->eye);
  dist= sqrt(mav_vectorDotProduct(dr, dr)) - f->radius;

  if (dist < fog_distance)
  {
  
  /* Determine which feature type we are on */
  if (testpoly_xz(f->npoints,f->verts,mav_win_current->vp->eye)) {
   current_feature=f->type;
  }
    
  /* Use the correct colouring and colour */
  mav_surfaceParamsUse(f->sp);

  /* Store view matrix then apply transformation */
  mav_gfxMatrixPush();
  mav_gfxMatrixMult(f->matrix);

  /* Draw the polygons */
  glBegin(GL_TRIANGLES); /* JMC */
  glNormal3f(0,1,0); /* JMC */
  for (i=0; i<f->num_polys; i++) {
#if 0 /* JMC */
    mav_subdivideDraw(f->polys[i].verts,2);
#else
    mav_subdivideDraw(f->polys[i].verts[0], f->polys[i].verts[1], f->polys[i].verts[2]);   
#endif
  }
  glEnd(); /* JMC */
  
  /* Restore the matrix */
  mav_gfxMatrixPop();
  
  }
  
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
  if (drawMap)
  {
      mav_surfaceParamsUse(map_white);
  }
  else
  {
	mav_surfaceParamsUse(v->sp);
  }

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

/* Support routines: font setup */
/* ---------------------------- */

void Make_Font(void)
{
  MAV_vector r0, r1, *verts;
  FILE *file;
  char filename[100];
  int i, j, l, c, num_verts, index[100];

  /* store letter widths */
  for (l=0; l<NUM_LETTERS+1; l++) alphabet[(int)(letter_widths[l][0])].letter_width= letter_widths[l][1];

  /*  read letter description from file */
  for (l=0; l<NUM_LETTERS; l++) {
    c= ascii[l];

    sprintf(filename, "./data/font/%d.cdat", c);
    file=fopen(filename, "r");
    if (!file) {
      fprintf(stderr, "Failed to open font file %s\n", filename);
      exit(0);
    }

    /* read vertices */
    fscanf(file, "%d", &num_verts);
    verts= mav_malloc(num_verts*sizeof(MAV_vector));
    for (j=0; j<num_verts; j++) fscanf(file, "%g %g %g", &verts[j].x,&verts[j].y,&verts[j].z);

    /* initialise bounding box of alphabet character */
    mav_BBCompInit(&alphabet[c].bb);

    /* read number of polygons */
    fscanf(file, "%d", &alphabet[c].num_polys);
    alphabet[c].polys= mav_malloc(alphabet[c].num_polys*sizeof(LC_Polygon));

    /* read each polygon */
    for (i=0; i<alphabet[c].num_polys; i++) {
      
      /* read which vertices define the polygon */
      num_verts= 0;
      fscanf(file, "%d", &index[num_verts]);

      while (index[num_verts]!=-1) {
	num_verts++;
	fscanf(file, "%d", &index[num_verts]);
      }

      /* assign vertex and calculate BB of points */
      alphabet[c].polys[i].num_verts= num_verts;
      alphabet[c].polys[i].verts= mav_malloc(num_verts*sizeof(MAV_vector));

      for (j=0; j<num_verts; j++) {
	alphabet[c].polys[i].verts[j]= verts[index[j]];	
	mav_BBCompPt(alphabet[c].polys[i].verts[j], &alphabet[c].bb);
      }

      /* calculate normal */
      r0= mav_vectorNormalize(mav_vectorSub(alphabet[c].polys[i].verts[2], alphabet[c].polys[i].verts[1]));
      r1= mav_vectorNormalize(mav_vectorSub(alphabet[c].polys[i].verts[0], alphabet[c].polys[i].verts[1]));
      alphabet[c].polys[i].normal= mav_vectorNormalize(mav_vectorCrossProduct(r0,r1));
    }

    mav_free(verts);

    fclose(file);
  }
}

/* Support routines: tesselation of feature polygons */
/* ------------------------------------------------- */

/* Routines for polygon tessellation */

Feature *feature;
int feature_vertices;

/* For old versions of GLU */

#ifndef GLU_VERSION_1_2
#define GLUtesselator GLUtriangulatorObj
#define gluTessBeginPolygon(X,Y) gluBeginPolygon((X))
#define gluTessBeginContour(X) gluNextContour((X), GLU_UNKNOWN)
#define gluTessEndContour(X) 
#define gluTessEndPolygon(X) gluEndPolygon((X))
#endif

#ifndef CALLBACK
#define CALLBACK
#endif

typedef void (CALLBACK *MAVLIB_GLUCB)();

void CALLBACK my_begin(GLenum mode)
{
  feature->polys[feature->num_polys].num_verts= 3;
  feature->polys[feature->num_polys].verts= mav_malloc(3*sizeof(MAV_vector));
  feature->polys[feature->num_polys].num_verts= 0;
}

void CALLBACK my_error(GLenum en)
{
  printf("GLU error: %i\n",en);
  exit(0);
}

void CALLBACK my_vertex(void *v)
{
  MAV_vector r0,r1;

  feature->polys[feature->num_polys].verts[feature->polys[feature->num_polys].num_verts]= *((MAV_vector*)v);
  feature->polys[feature->num_polys].num_verts++;

  if (feature->polys[feature->num_polys].num_verts == 3) {
    /* calculate normal */
    r0= mav_vectorSub(feature->polys[feature->num_polys].verts[1], feature->polys[feature->num_polys].verts[0]);
    r1= mav_vectorSub(feature->polys[feature->num_polys].verts[feature->polys[feature->num_polys].num_verts-1], feature->polys[feature->num_polys].verts[0]);
    feature->polys[feature->num_polys].normal= mav_vectorNormalize(mav_vectorCrossProduct(r0,r1));
    feature->num_polys++;
  }
}

void CALLBACK dummyedge(GLboolean f)
{
}

void CALLBACK count_polygons(void *ignored)
{
  feature_vertices++;

  if (feature_vertices == 3) {
    feature->num_polys++;
    feature_vertices= 0;
  }
}

void CALLBACK dummybegin(GLenum mode)
{
}

void CALLBACK dummyend(void)
{
}

/* Support routines: setup of LC structures */
/* ---------------------------------------- */

void Build_Feature(float *xp, float *yp, float *zp, int num_verts, int type)
{
  GLdouble data[3];
  GLUtesselator *tobj;
  int i;
  float x1,y1,x1p,y1p,z1;
  float dx,dy,dz;
   
  /* Rotate according to position */
  angle=MAV_PI/180.0*cityangle[citycount];
  cangle=(float)cos((double)angle);
  sangle=(float)sin((double)angle);

  /* Create feature structure */
  feature= mav_malloc(sizeof(Feature));

  /* Store type */
  feature->npoints=num_verts;
  feature->type=type;
  
  /* read in vertices */
  feature->verts= mav_malloc(num_verts*sizeof(MAV_vector));

  /* initialise bounding box */
  mav_BBCompInit(&feature->bb);

  /* read vertices */
  for (i=0; i<num_verts; i++) {
    x1=xp[i];
    y1=yp[i];
    z1=zp[i];
    
    if ((type==CANAL) || (type==GRASS) || (type==PAVEMENT)) {
     /* Center city according to center of mass */
     x1 -= comx[citycount];
     y1 -= comy[citycount];
 
     /* Rotate vector according to location */
     x1p = x1*cangle - y1*sangle;
     y1p = x1*sangle + y1*cangle;

     /* Store location, adjusting for position on triangle */
     feature->verts[i].x= -(x1p-centx[citycount]);
     feature->verts[i].y=  z1;
     feature->verts[i].z=  (y1p-centy[citycount]);
    } else {
     /* Store location directly */
     feature->verts[i].x=  x1;
     feature->verts[i].y=  z1;
     feature->verts[i].z=  y1;
    }
    
    /* update bounding box */
    mav_BBCompPt(feature->verts[i], &feature->bb);
  }
  
  /* Calculate bounding circle */
  feature->center.x=(feature->bb.min.x+feature->bb.max.x)*0.5;
  feature->center.y=(feature->bb.min.y+feature->bb.max.y)*0.5;
  feature->center.z=(feature->bb.min.z+feature->bb.max.z)*0.5;
  dx=feature->bb.min.x-feature->center.x;
  dy=feature->bb.min.y-feature->center.y;
  dz=feature->bb.min.z-feature->center.z;
  feature->radius=sqrt(dx*dx+dy*dy+dz*dz);
  
  /* initialise a triangulator object */
  tobj= gluNewTess();
  if (!tobj) {
    fprintf(stderr, "Failed to allocate GLUtesselator\n");
    exit(0);
  }

  /* set triangulator callbacks to count triangles */
  feature->num_polys= 0;
  feature_vertices= 0;
  
  gluTessCallback(tobj, GLU_TESS_BEGIN, (MAVLIB_GLUCB) dummybegin);
  gluTessCallback(tobj, GLU_TESS_END, (MAVLIB_GLUCB) dummyend);
  gluTessCallback(tobj, GLU_TESS_ERROR, (MAVLIB_GLUCB) my_error);
  gluTessCallback(tobj, GLU_TESS_VERTEX, (MAVLIB_GLUCB) count_polygons);
  gluTessCallback(tobj, GLU_TESS_EDGE_FLAG, (MAVLIB_GLUCB) dummyedge);

  /* count polygons */
  gluTessBeginPolygon(tobj, NULL);
  gluTessBeginContour(tobj);
  for (i=0; i<num_verts; i++) {
    data[0]= (GLdouble)feature->verts[i].x;
    data[1]= (GLdouble)feature->verts[i].y;
    data[2]= (GLdouble)feature->verts[i].z;
    gluTessVertex(tobj, data, (GLvoid *)&(feature->verts[i]));
  }
  gluTessEndContour(tobj);
  gluTessEndPolygon(tobj);
  gluDeleteTess(tobj);

  /* allocate space for polygons */
  feature->polys= mav_malloc(feature->num_polys*sizeof(LC_Polygon));
  for (i=0; i<feature->num_polys; i++) {
    feature->polys[i].verts= mav_malloc(3*sizeof(MAV_vector));
    feature->polys[i].num_verts= 0;
  }
  num_polygons += feature->num_polys;

  /* re-initialise triangulatorObj for triangulation */
  tobj= gluNewTess();
  if (!tobj) {
    fprintf(stderr, "Failed to allocate GLUtesselator\n");
    exit(0);
  }

  feature->num_polys= 0;
  gluTessCallback(tobj, GLU_TESS_BEGIN, (MAVLIB_GLUCB) my_begin);
  gluTessCallback(tobj, GLU_TESS_END, (MAVLIB_GLUCB) dummyend);
  gluTessCallback(tobj, GLU_TESS_ERROR, (MAVLIB_GLUCB) my_error);
  gluTessCallback(tobj, GLU_TESS_VERTEX, (MAVLIB_GLUCB) my_vertex);
  gluTessCallback(tobj, GLU_TESS_EDGE_FLAG, (MAVLIB_GLUCB) dummyedge); /* JMC */

  /* triangulate */
  gluTessBeginPolygon(tobj, NULL);
  gluTessBeginContour(tobj);
  for (i=0; i<num_verts; i++) {
    data[0]= (double)feature->verts[i].x;
    data[1]= (double)feature->verts[i].y;
    data[2]= (double)feature->verts[i].z;

    gluTessVertex(tobj, data, (void *)&(feature->verts[i]));
  }
  gluTessEndContour(tobj);
  gluTessEndPolygon(tobj);
  gluDeleteTess(tobj);

  /* initialise matrix */
  feature->matrix= MAV_ID_MATRIX;

  /* initialise colour */
  feature->sp= mav_surfaceParamsNew(MAV_COLOUR, 0, 0, 0);
  switch (type) {
   case CANAL :
#ifdef TEX_FEATURES
     feature->sp= canal;
#else
     feature->sp->colour= CANAL_COLOR+COL_OFFSET; 
#endif
    break;
  case PAVEMENT :
#ifdef TEX_FEATURES
    feature->sp= stone;
#else
    feature->sp->colour= PAVEMENT_COLOR+COL_OFFSET; 
#endif
    break;
  case GRASS :
#ifdef TEX_FEATURES
    feature->sp= grass;
#else
    feature->sp->colour= GRASS_COLOR+COL_OFFSET; 
#endif
    break;
  default :
#ifdef TEX_FEATURES
    feature->sp= grass;
#else
    feature->sp->colour= GRASS_COLOR+COL_OFFSET; 
#endif
    break;
  }
  
  /* create Maverik object and insert in SMS */
  mav_SMSCallbackObjectAddExec(city_build, mav_objectNew(mav_class_feature, feature));
}

/* Vector variables */

static Vector *vector_list=NULL;
static int vectorcount;

Vector* Find_Vector(char *name)
{
  Vector *v;

  /* Linear search through the list */
  v=vector_list;
  while (v) {
   if (strcmp(v->name, name)==0) return v;
   v= v->next;
  }
              
  return NULL;
}

/* Scanner line length (overdimensioned) */

#define LINE_LEN	1024

/* Add a 2D vector to the scene */

void AddNewVector(char *vectorname,float x1, float y1, float x2, float y2, int ColorCode)
{
  Vector *v,*newv;
  MAV_object *obj;
 
  /* Add and malloc */
  vectorcount++;
  v= mav_malloc(sizeof(Vector));
  strcpy(v->name, vectorname);
  /* Store new coordinates */
  v->start.x= x1;
  v->start.y= 0.0;
  v->start.z= y1;
  v->end.x  = x2;
  v->end.y  = 0.0;
  v->end.z  = y2;
  /* Set colour */
  v->sp=mav_surfaceParamsNew(MAV_COLOUR, 0, 0, 0);
  v->sp->colour=ColorCode;
  /* Character params */
  v->phrase_length= 0.0;
  v->num_characters= 0;
  /* Calculate bounding box */
  v->bb.min.x= (v->start.x < v->end.x) ? v->start.x : v->end.x;
  v->bb.min.y= 0.0;
  v->bb.min.z= (v->start.z < v->end.z) ? v->start.z : v->end.z;
  v->bb.max.x= (v->start.x > v->end.x) ? v->start.x : v->end.x;
  v->bb.max.y= 0.0;
  v->bb.max.z= (v->start.z > v->end.z) ? v->start.z : v->end.z;
  /* Update city BB with vector BB */
  mav_BBCompBB(v->bb, &city_bb);
  /* Set transformation */
  v->matrix= MAV_ID_MATRIX;;
  /* Place in vector list using read order */
  v->next= vector_list;
  vector_list=v;

  newv= mav_malloc(sizeof(Vector));
  memcpy(newv,v,sizeof(Vector));
  newv->start.z -= MAP_OFFSET;
  newv->end.z -= MAP_OFFSET;
  newv->start=mav_vectorScalar(newv->start,map_scale);
  newv->end=mav_vectorScalar(newv->end,map_scale);
  obj= mav_objectNew(mav_class_vector, newv);
  mav_SMSCallbackObjectAddExec(city_map, obj);
  
}

void MakeAllStreets()
{
  int i;
  FILE *file;
  char strfile[LINE_LEN];
  char lstfile[LINE_LEN];
  char aline[LINE_LEN];
  float x1,y1,x2,y2,x1p,y1p,x2p,y2p;
  float conradius[3][6];
#ifdef INSERT_STREETS
  float conpx[3][6];
  float conpy[3][6];
  float origx[6];
  float origy[6];
#endif
  int count,numpoints;
  int coordinatecount=0;
  float xsum=0, ysum=0;
  float xorigin,yorigin,dx=0,dy=0;
  float radius,maxradius=0;
  char streetname[LINE_LEN];
  char longstreetname[LINE_LEN];
  int ColorCode;
  MAV_BB bb;
  
  MAV_vector newdr;
  Alphabet *letter;
  Character *character;
  Vector *v, *current_vector=NULL;
  float width, height, depth, spacing, letter_width;
  float current_spacing_x=0, current_spacing_y=0;
  float vx1=0, vy1=0, vx2, vy2, dlen, dir=0, newx, newy;
  float xp[MAX_FEATURE_POINTS],yp[MAX_FEATURE_POINTS],zp[MAX_FEATURE_POINTS];
  
  /* Reset Statistics */
  num_polygons = 0;
  
  /* Determine Center of Mass (COM) of City */
  /* -------------------------------------- */
  global_city_mre=0.0;
  for (citycount=0; citycount<MAX_CITY; citycount++) {  
   /* Create secondary filename */
   sprintf(strfile, "./data/");
   strcat(strfile,basefilename[citycount]);
   strcat(strfile,".str");
   
   /* open file */
   if (!(file=fopen(strfile, "r"))) {
    fprintf(stderr, "Failed to open street file %s\n", strfile);
   } else {
    /* reset sums */
    coordinatecount=0;
    xsum=0.0;
    ysum=0.0;
    /* read next line */
    fgets(aline,LINE_LEN,file);
    /* scan the datafile */
    while (!feof(file)) {
     if (aline[0]!=';') {
      /* not a comment but data */
      if ( (!strncmp(aline, "grass", 5)) || (!strncmp(aline, "canal", 5)) || (!strncmp(aline, "pavem", 5)) ) { 
       /* its a feature */
       sscanf(aline, "%*s %i", &numpoints);
       for (count=0; count<numpoints; count++) {
        fgets(aline,LINE_LEN,file);
        sscanf(aline,"%*c %f %*c %f %*c %*f %*c", &x1, &y1);
        xsum += x1;
        ysum += y1;
        coordinatecount++;

       } 
      } else {
       /* its a street */
 
       /* read start and end point of vector */
       sscanf(aline, "%*s %f %f %*f %f %f %*i", &x1, &y1, &x2, &y2);
       xsum += x1;
       xsum += x2;
       ysum += y1;
       ysum += y2;
       coordinatecount += 2;
      }
     }
     /* read next line */
     fgets(aline,LINE_LEN,file);
    } 
   }
   fclose(file);
   
   /* Calculate center of mass */
   comx[citycount]=xsum/(float)coordinatecount;
   comy[citycount]=ysum/(float)coordinatecount;
   
   printf ("Center of mass of %s = (%8.2f,%8.2f)\n",basefilename[citycount],comx[citycount],comy[citycount]); 
  }

  /* Determine Maximum Radial Extent (MRE) from COM of City */
  /* ------------------------------------------------------ */
  global_city_mre=0.0;
  for (citycount=0; citycount<MAX_CITY; citycount++) {  
   /* Create secondary filename */
   sprintf(strfile, "./data/"); 
   strcat(strfile,basefilename[citycount]);
   strcat(strfile,".str");
   
   /* open file */
   if (!(file=fopen(strfile, "r"))) {
    fprintf(stderr, "Failed to open street file %s\n", strfile);
   } else {
    /* reset maximum and origin */
    maxradius=0.0;
    xorigin=comx[citycount];
    yorigin=comy[citycount];
    /* read next line */
    fgets(aline,LINE_LEN,file);
    /* scan the datafile */
    while (!feof(file)) {
     if (aline[0]!=';') {
      /* not a comment but data */
      if ( (!strncmp(aline, "grass", 5)) || (!strncmp(aline, "canal", 5)) || (!strncmp(aline, "pavem", 5)) ) { 
       /* its a feature */
       sscanf(aline, "%*s %i", &numpoints);
       for (count=0; count<numpoints; count++) {
        fgets(aline,LINE_LEN,file);
        sscanf(aline,"%*c %f %*c %f %*c %*f %*c", &x1, &y1);
        /* update radius */
        dx=x1-xorigin;
        dy=y1-yorigin;
        radius=sqrt(dx*dx+dy*dy);
	if (radius>maxradius) maxradius=radius;
       } 
      } else {
       /* its a street */
 
       /* read start and end point of vector */
       sscanf(aline, "%*s %f %f %*f %f %f %*i", &x1, &y1, &x2, &y2);
       /* update radius */
       dx=x1-xorigin;
       dy=y1-yorigin;
       radius=sqrt(dx*dx+dy+dy);
       if (radius>maxradius) maxradius=radius;
       /* update radius */
       dx=x2-xorigin;
       dy=y2-yorigin;
       radius=sqrt(dx*dx+dy+dy);
       if (radius>maxradius) maxradius=radius;
      }
     }
     /* read next line */
     fgets(aline,LINE_LEN,file);
    } 
   }
   fclose(file);
   
   /* Store MRE */
   city_mre[citycount]=maxradius;
   printf ("Maximum Radial Extent of %s = %8.2f\n",basefilename[citycount],city_mre[citycount]); 
   
   /* Do a global maximum */
   if (maxradius>global_city_mre) global_city_mre=maxradius;
  }
  
  /* Calculate city centers on a triangle */
  /* ------------------------------------ */
  for (citycount=0; citycount<MAX_CITY; citycount++) {
   angle=MAV_PI*120.0/180.0*(float)citycount;
   cangle=(float)cos((double)angle);
   sangle=(float)sin((double)angle);
   centx[citycount] = STRUCTURE_SCALE*global_city_mre*cangle;
   centy[citycount] = STRUCTURE_SCALE*global_city_mre*sangle;
   printf ("Citycenter of %s = (%8.2f,%8.2f)\n",basefilename[citycount],centx[citycount],centy[citycount]); 
  } 
  
  /* Build vector list of all three cities */
  /* ------------------------------------- */
  /* Initialise BB of structure */
  mav_BBCompInit(&city_bb);
  /* Scan all files */
  for (citycount=0; citycount<MAX_CITY; citycount++) {  

   /* Rotate city according to position */
   angle=MAV_PI/180.0*cityangle[citycount];
   cangle=(float)cos((double)angle);
   sangle=(float)sin((double)angle);
   
   /* Create secondary filename */
   sprintf(strfile, "./data/");
   strcat(strfile,basefilename[citycount]);
   strcat(strfile,".str");
   
   /* open file */
   if (!(file=fopen(strfile, "r"))) {
    fprintf(stderr, "Failed to open street file %s\n", strfile);
   } else {
    /* read next line */
    fgets(aline,LINE_LEN,file);
    /* scan the datafile */
    while (!feof(file)) {
     if (aline[0]!=';') {
      /* not a comment but data */
      if ( (!strncmp(aline, "grass", 5)) || (!strncmp(aline, "canal", 5)) || (!strncmp(aline, "pavem", 5)) ) { 
       /* its a feature */
       sscanf(aline, "%*s %i", &numpoints);
       if (!strncmp(aline, "grass", 5)) {
        /* Read vertices-coordinates into temporary arrays */
        for (i=0; i<numpoints; i++) {
         fscanf(file, "(%f,%f,%f)\n", &xp[i], &yp[i], &zp[i]);
        }
        Build_Feature(xp,yp,zp,numpoints,GRASS);
       } else {
        if (!strncmp(aline, "canal", 5)) {
         /* Read vertices-coordinates into temporary arrays */
         for (i=0; i<numpoints; i++) {
          fscanf(file, "(%f,%f,%f)\n", &xp[i], &yp[i], &zp[i]);
         }
         Build_Feature(xp,yp,zp,numpoints,CANAL);
        } else {
         /* Read vertices-coordinates into temporary arrays */
         for (i=0; i<numpoints; i++) {
          fscanf(file, "(%f,%f,%f)\n", &xp[i], &yp[i], &zp[i]);
         }
         if (!strncmp(aline, "pavem", 5)) {
          Build_Feature(xp,yp,zp,numpoints,PAVEMENT);
         }
        }
       }  
      } else {
       /* its a street */
 
       /* read start and end point of vector */
       sscanf(aline, "%s %f %f %*f %f %f %*f %i",streetname, &x1, &y1, &x2, &y2, &ColorCode);
       
       /* Center city according to center of mass */
       x1 -= comx[citycount];
       y1 -= comy[citycount];
       x2 -= comx[citycount];
       y2 -= comy[citycount];
              
       /* Rotate vector according to location */
       x1p = x1*cangle - y1*sangle;
       y1p = x1*sangle + y1*cangle;
       
       x2p = x2*cangle - y2*sangle;
       y2p = x2*sangle + y2*cangle;

       /* Place vector adjusting for center location on the triangle */
       strcpy(longstreetname,basefilename[citycount]);
       strcat(longstreetname,streetname);
       AddNewVector(longstreetname,
       		-(x1p-centx[citycount]),
       		 (y1p-centy[citycount]),
       		-(x2p-centx[citycount]),
       		 (y2p-centy[citycount]),
       		 ColorCode /*white_index */);
      }
     }
     /* read next line */
     fgets(aline,LINE_LEN,file);
    } 
   }
   fclose(file);
  }

  /* Streets connecting cities as vectors and features */
  /* ------------------------------------------------- */
  
  /* Manhattan */
  conradius[0][0]=0.55;
  conradius[0][1]=0.55;
  conradius[0][2]=0.45;
  conradius[0][3]=0.45;
  conradius[0][4]=0.55;
  conradius[0][5]=0.55;
  
  /* Karlsruhe */
  conradius[1][0]=0.37;
  conradius[1][1]=0.37;
  conradius[1][2]=0.47;
  conradius[1][3]=0.47;
  conradius[1][4]=0.40;
  conradius[1][5]=0.40;
  
  /* Amsterdam */
  conradius[2][0]=0.56;
  conradius[2][1]=0.56;
  conradius[2][2]=0.57;
  conradius[2][3]=0.57;
  conradius[2][4]=0.65;
  conradius[2][5]=0.65;
  
  for (citycount=0; citycount<MAX_CITY; citycount++) {  
   for (count=0; count<6; count++) {
    angle=STREET_WIDTH/(global_city_mre*conradius[citycount][count]);
    switch (count) {
     case 0:
      angle=MAV_PI/180.0*(-180.0+(120.0*citycount-30.0))-angle;
      break;
     case 1:
      angle=MAV_PI/180.0*(-180.0+(120.0*citycount)-30.0)+angle;
      break;
     case 2:
      angle=MAV_PI/180.0*(-180.0+(120.0*citycount))-angle;
      break;
     case 3:
      angle=MAV_PI/180.0*(-180.0+(120.0*citycount))+angle;
      break;
     case 4:
      angle=MAV_PI/180.0*(-180.0+(120.0*citycount)+30.0)-angle;
      break;
     case 5:
      angle=MAV_PI/180.0*(-180.0+(120.0*citycount)+30.0)+angle;
      break;
    }
    cangle=(float)cos((double)angle);
    sangle=(float)sin((double)angle);
#ifdef INSERT_STREETS
    conpx[citycount][count]=centx[citycount]+global_city_mre*conradius[citycount][count]*cangle;
    conpy[citycount][count]=centy[citycount]+global_city_mre*conradius[citycount][count]*sangle;
#endif
   }
  } 

  for (count=0; count<6; count++) {
   angle=STREET_WIDTH/(ORIGIN_SCALE*global_city_mre);
   switch (count & 1) {
    case 0:
      angle=MAV_PI/180.0*(120.0*(count >> 1))+angle;
      break;
    case 1:
      angle=MAV_PI/180.0*(120.0*(count >> 1))-angle;
      break;
   }
   cangle=(float)cos((double)angle);
   sangle=(float)sin((double)angle);
#ifdef INSERT_STREETS
   origx[count]=ORIGIN_SCALE*global_city_mre*cangle;
   origy[count]=ORIGIN_SCALE*global_city_mre*sangle;
#endif
  } 

  /* Adjust global radius */
  global_city_mre *= STRUCTURE_SCALE;
  
  /* Add vectors and features for streets connecting cities */
  /* ------------------------------------------------------ */

#ifdef INSERT_STREETS
  
  AddNewVector("0to1a",conpx[0][0],conpy[0][0],conpx[1][5],conpy[1][5],1);
  AddNewVector("0to1b",conpx[0][1],conpy[0][1],conpx[1][4],conpy[1][4],1);
  xp[0]=conpx[0][0]; yp[0]=conpy[0][0]; zp[0]=0.0;
  xp[3]=conpx[1][5]; yp[3]=conpy[1][5]; zp[1]=0.0;
  xp[1]=conpx[0][1]; yp[1]=conpy[0][1]; zp[2]=0.0;
  xp[2]=conpx[1][4]; yp[2]=conpy[1][4]; zp[3]=0.0;
  Build_Feature(xp,yp,zp,4,STREET0);

  AddNewVector("1to2a",conpx[1][0],conpy[1][0],conpx[2][5],conpy[2][5],1);
  AddNewVector("1to2b",conpx[1][1],conpy[1][1],conpx[2][4],conpy[2][4],1);
  xp[0]=conpx[1][0]; yp[0]=conpy[1][0]; zp[0]=0.0;
  xp[3]=conpx[2][5]; yp[3]=conpy[2][5]; zp[1]=0.0;
  xp[1]=conpx[1][1]; yp[1]=conpy[1][1]; zp[3]=0.0;
  xp[2]=conpx[2][4]; yp[2]=conpy[2][4]; zp[2]=0.0;
  Build_Feature(xp,yp,zp,4,STREET1);

  AddNewVector("2to0a",conpx[2][0],conpy[2][0],conpx[0][5],conpy[0][5],1);
  AddNewVector("2to0b",conpx[2][1],conpy[2][1],conpx[0][4],conpy[0][4],1);
  xp[0]=conpx[2][0]; yp[0]=conpy[2][0]; zp[0]=0.0;
  xp[3]=conpx[0][5]; yp[3]=conpy[0][5]; zp[1]=0.0;
  xp[1]=conpx[2][1]; yp[1]=conpy[2][1]; zp[3]=0.0;
  xp[2]=conpx[0][4]; yp[2]=conpy[0][4]; zp[2]=0.0;
  Build_Feature(xp,yp,zp,4,STREET2);

  AddNewVector("Cto0a",origx[0],origy[0],conpx[0][2],conpy[0][2],1);
  AddNewVector("Cto0b",origx[1],origy[1],conpx[0][3],conpy[0][3],1);
  xp[0]=origx[0];    yp[0]=origy[0];    zp[0]=0.0;
  xp[1]=conpx[0][2]; yp[1]=conpy[0][2]; zp[1]=0.0;
  xp[3]=origx[1];    yp[3]=origy[1];    zp[3]=0.0;
  xp[2]=conpx[0][3]; yp[2]=conpy[0][3]; zp[2]=0.0;
  Build_Feature(xp,yp,zp,4,STREET3);

  AddNewVector("Cto1a",origx[2],origy[2],conpx[1][2],conpy[1][2],1);
  AddNewVector("Cto1b",origx[3],origy[3],conpx[1][3],conpy[1][3],1);
  xp[0]=origx[2];    yp[0]=origy[2];    zp[0]=0.0;
  xp[1]=conpx[1][2]; yp[1]=conpy[1][2]; zp[1]=0.0;
  xp[3]=origx[3];    yp[3]=origy[3];    zp[3]=0.0;
  xp[2]=conpx[1][3]; yp[2]=conpy[1][3]; zp[2]=0.0;
  Build_Feature(xp,yp,zp,4,STREET4);

  AddNewVector("Cto2a",origx[4],origy[4],conpx[2][2],conpy[2][2],1);
  AddNewVector("Cto2b",origx[5],origy[5],conpx[2][3],conpy[2][3],1);
  xp[0]=origx[4];    yp[0]=origy[4];    zp[0]=0.0;
  xp[1]=conpx[2][2]; yp[1]=conpy[2][2]; zp[1]=0.0;
  xp[3]=origx[5];    yp[3]=origy[5];    zp[3]=0.0;
  xp[2]=conpx[2][3]; yp[2]=conpy[2][3]; zp[2]=0.0;
  Build_Feature(xp,yp,zp,4,STREET5);

#endif

  /* Load and Position all Characters */
  /* -------------------------------- */
  
  /* Scan all files */
  for (citycount=0; citycount<MAX_CITY; citycount++) {  

   /* Create secondary filename */
   sprintf(lstfile, "./data/");
   strcat(lstfile,basefilename[citycount]);
   strcat(lstfile,".lst");

   /* Proceed to position and instance alphabet characters */
   file= fopen(lstfile, "r");
   if (!file) {
    fprintf(stderr, "Failed to open letters file %s\n", lstfile);
    exit(0);
   }

   /* ------ First find phrase lengths */
   /* read next line */
   fgets(aline,LINE_LEN,file);
   /* scan the datafile */
   while (!feof(file)) {
    if (aline[0]!=';') {
     if (strncmp(aline, ":", 1)) {
      /* Find the vector for the name */
      sscanf(aline,"%s",streetname);
      strcpy(longstreetname,basefilename[citycount]);
      strcat(longstreetname,streetname);
      current_vector= Find_Vector(longstreetname);
      if (!current_vector) {
       fprintf(stderr, "Unknown vector name %s\n", longstreetname);
       exit(0);
      }

      /* Initialise character spacing */
      current_spacing_x= 0.0;
      current_spacing_y= 0.0;

      /* Find length of vector */
      vx1= -current_vector->start.x;
      vy1= current_vector->start.z;
      vx2= -current_vector->end.x;
      vy2= current_vector->end.z;

      dx= vx2-vx1;
      dy= vy2-vy1;
      dlen= 1.0/sqrt(dx*dx+dy*dy);
      dx *= dlen;
      dy *= dlen;
      dir= atan2(dx,dy);
     }
     else /* Contents of vector */
     {
      if (!current_vector) {
       fprintf(stderr, "Can't have letters before a street definition \n");
       exit(0);
      }

      /* Read dimensions */
      sscanf(&aline[2], "%f %f %f %f", &width, &height, &depth, &spacing);

      if (strlen(aline) > 1) {
       letter= &alphabet[(unsigned char)aline[1]];
       letter_width= letter->letter_width;
      }
      else letter_width= 0.8;
	
      /* Account for character spacing and width */
      current_spacing_x += (letter_width+0.2)*width*dx;
      current_spacing_y += (letter_width+0.2)*width*dy;

      /* Calculate length of phrase */
      current_vector->phrase_length=(letter_width+0.1)*width+sqrt(current_spacing_x*current_spacing_x+current_spacing_y*current_spacing_y);
      current_vector->num_characters++;
     }
    }

    /* read next line */
    fgets(aline,LINE_LEN,file);
   }
  
   fclose(file);

   /* Now calculate extra character widths */
   v=vector_list;
   while (v != NULL) {
    if (v->phrase_length > 0.0) 
    {
      newdr.x= v->end.x-v->start.x;
      newdr.z= v->end.z-v->start.z;
      dlen= sqrt(newdr.x*newdr.x+newdr.z*newdr.z);

      v->extra_width= dlen/v->phrase_length;
    }
    else
      v->extra_width= 0.0;

    v= v->next;
   }

   /* Re-parse file to instance characters now we know their sizes */
   file= fopen(lstfile, "r");
   if (!file) {
    fprintf(stderr, "Failed to open letters file %s\n", lstfile);
    exit(0);
   }

   current_vector= NULL;
  
   /* read next line */
   fgets(aline,LINE_LEN,file);
   /* scan the datafile */
   while (!feof(file)) {
    if (aline[0]!=';') {
     if (strncmp(aline, ":", 1)) {
      /* Find the vector for the name */
      sscanf(aline,"%s",streetname);
      strcpy(longstreetname,basefilename[citycount]);
      strcat(longstreetname,streetname);
      current_vector= Find_Vector(longstreetname);
      if (!current_vector) {
       fprintf(stderr, "Unknown vector name %s\n", longstreetname);
       exit(0);
      }

      /* Initialise character spacing */
      current_spacing_x= 0.0;
      current_spacing_y= 0.0;

      /* Find length of vector */
      vx1= -current_vector->start.x;
      vy1= current_vector->start.z;
      vx2= -current_vector->end.x;
      vy2= current_vector->end.z;

      dx= vx2-vx1;
      dy= vy2-vy1;
      dlen= 1.0/sqrt(dx*dx+dy*dy);
      dx *= dlen;
      dy *= dlen;
      dir= atan2(dx,dy);
     }
     else /* Contents of vector */
     {
      if (!current_vector) {
       fprintf(stderr, "Can't have letters before a street definition \n");
       exit(0);
      }

      /* Read dimensions */
      sscanf(&aline[2], "%f %f %f %f", &width, &height, &depth, &spacing);

      width *= current_vector->extra_width;

      if (strlen(aline) > 1) {
       letter = &alphabet[(unsigned char)aline[1]];
       letter_width = letter->letter_width;
       num_polygons += letter->num_polys;

       /* create the character */ 
       character= mav_malloc(sizeof(Character));
       character->character= letter;

       /* Copy colour information */
       character->sp= mav_surfaceParamsNew(MAV_MATERIAL, MapColorCode(current_vector->sp->colour,citycount), MapColorCode(current_vector->sp->colour,citycount), 0);

       /* calculate its position */
       current_spacing_x += 0.1*width*dx;
       current_spacing_y += 0.1*width*dy;
       newx= -(vx1+current_spacing_x);
       newy= vy1+current_spacing_y;

       /* set up transformation matrix */
       character->matrix= mav_matrixSet(0.0, 0.0, -(180.0*dir/MAV_PI+90.0), newx, 0.0, newy);

       /* scale matrix */
       character->matrix.mat[0][0] *= width;
       character->matrix.mat[1][0] *= width;
       character->matrix.mat[2][0] *= width;
       character->matrix.mat[3][0] *= width;
	  
       character->matrix.mat[0][1] *= height;
       character->matrix.mat[1][1] *= height;
       character->matrix.mat[2][1] *= height;
       character->matrix.mat[3][1] *= height;
	  
       character->matrix.mat[0][2] *= depth;
       character->matrix.mat[1][2] *= depth;
       character->matrix.mat[2][2] *= depth;
       character->matrix.mat[3][2] *= depth;

       /* create Maverik object and place in SMS */
       mav_SMSCallbackObjectAddExec(city_build, mav_objectNew(mav_class_character, character));
 
       /* update city bounding box */
       mav_BBAlign(character->character->bb, character->matrix, &bb);
       mav_BBCompBB(bb, &city_bb);

       /* calculate center of character for LOD processing */
       character->centre= mav_vectorScalar(mav_vectorAdd(bb.min, bb.max), 0.5);
      } else {
       /* Space */
       letter_width= 0.8;
       current_spacing_x += 0.1*width*dx;
       current_spacing_y += 0.1*width*dy;
      }

      current_spacing_x += (letter_width+0.1)*width*dx;
      current_spacing_y += (letter_width+0.1)*width*dy;
     }
    }

    /* read next line */
    fgets(aline,LINE_LEN,file);
   }
  }
    
  fclose(file);
  
  fprintf(stdout, "Loaded %i polygons into static scene.\n", num_polygons);
  fprintf(stdout, "Loaded %i vectors into static scene.\n", vectorcount);
}



/* Tables */

int AmsterdamIndexConversionTable[(AMSTERDAM_MAX_COLOR_INDEX+1)]=
	{1,6,10,4,3,13,5,1,13,4,8,6,2,9,7,13,2,3,4,11,14,13,1,7,8,2,1,5,12,14,1,5,9,11,6,10,1,2,3,14,14,11,9,8,10,8,12,4,1,3};
int KarlsruheIndexConversionTable[KARLSRUHE_MAX_COLOR_INDEX+1]=
	{3,9,6,6,4,2,10,1,2,14,3,3,2,5,17,10,18,15,14,11,12,14,4,15,11,8,9,2,4,6,7,4,7,9,8,18,16,2,6,17,18,14,6,19,6,1,1,1,1,1};

#define NUM_COLORS	47
	
int TheColorTable[NUM_COLORS][3] = 
	{

	/* 14 colors basic set */
	
	{255,0,0},
	{255,255,0},
	{0,255,255},
	{0,255,0},
	{255,0,255},

	{255,128,0},
	{0,0,255},
	{255,0,128},
	{128,255,0},
	{0,255,128},

	{0,128,255},
	{7,66,96},
	{73,182,30},
	{100,100,100},
	
	/* 14 colors Amsterdam set */
	
	{255,169,66},
	{255,26,23},
	{255,45,12},
	{255,85,23},
	{255,23,6},
	
	{207,132,90},
	{252,44,58},
	{172,123,123},
	{255,130,21},
	{255,95,3},
	
	{253,2,2},
	{253,107,80},
	{242,114,34},
	{255,78,54},
	
	/* 19 colors Karlsruhe set */
	
	{216,155,155},
	{254,149,149},
	{254,254,113},
	{207,132,90},
	{255,143,57},

	{200,200,200},
	{125,219,220},
	{68,253,141},
	{211,100,30},
	{146,129,227},

	{189,113,55},
	{254,200,37},
	{250,133,83},
	{177,41,1},
	{182,202,68},

	{255,201,121},
	{255,172,222},
	{250,250,191},
	{203,254,201}
	};

/* Internal error reporting routine */

void PrintRangeError(int ColorCode) 
{
  /* fprintf (stderr,"GetLetterColor: Oops, color index out of range (%i).\n",ColorCode);*/
}

/* Internal color conversion routine */
	
void GetLetterColor (int ColorCode, int CityCode, float *RGB)
{
 float rgb[3];
 int index;
 
 switch (CityCode) {
  case MANHATTAN_COLOR_CODE:
   if (ColorCode<0) {
    PrintRangeError(ColorCode);
    ColorCode=0;
   } else if (ColorCode>MANHATTAN_MAX_COLOR_INDEX) {
    PrintRangeError(ColorCode);
    ColorCode=MANHATTAN_MAX_COLOR_INDEX;
   }
   rgb[0]=TheColorTable[ColorCode][0]/255.0; 
   rgb[1]=TheColorTable[ColorCode][1]/255.0; 
   rgb[2]=TheColorTable[ColorCode][2]/255.0; 
   break;
  case AMSTERDAM_COLOR_CODE:
   if (ColorCode<0) {
    PrintRangeError(ColorCode);
    ColorCode=0;
   } else if (ColorCode>AMSTERDAM_MAX_COLOR_INDEX) {
    PrintRangeError(ColorCode);
    ColorCode=AMSTERDAM_MAX_COLOR_INDEX;
   }
   index=AmsterdamIndexConversionTable[ColorCode]+13;
   rgb[0]=TheColorTable[index][0]/255.0; 
   rgb[1]=TheColorTable[index][1]/255.0; 
   rgb[2]=TheColorTable[index][2]/255.0; 
   break;
  case KARLSRUHE_COLOR_CODE:
   if (ColorCode<0) {
    PrintRangeError(ColorCode);
    ColorCode=0;
   } else if (ColorCode>KARLSRUHE_MAX_COLOR_INDEX) {
    PrintRangeError(ColorCode);
    ColorCode=KARLSRUHE_MAX_COLOR_INDEX;
   }
   index=KarlsruheIndexConversionTable[ColorCode]+27;
   rgb[0]=TheColorTable[index][0]/255.0; 
   rgb[1]=TheColorTable[index][1]/255.0; 
   rgb[2]=TheColorTable[index][2]/255.0; 
   break;
 }
 memcpy(RGB,rgb,3*sizeof(float));  
}

/* Call this to initialize the LC color table */

void BuildColorTable()
{
 int i,j;
 float RGB[3];
 float modulation;
 int ccount;

 /* Randomize for later */
 /* srand(); */
 
 /* Build table */ 
 fprintf (stderr,"Building color table");
 ccount=COL_OFFSET;
 for (i=0; i<=MANHATTAN_MAX_COLOR_INDEX; i++)
 {
  GetLetterColor(i,MANHATTAN_COLOR_CODE,RGB); 
  modulation=1.0;
  for (j=0; j<NUM_SHADES; j++) {
   mav_paletteMaterialSet(mav_palette_default,ccount, 0.3*RGB[0]*modulation,0.3*RGB[1]*modulation,0.3*RGB[2]*modulation, 1.0, RGB[0]*modulation,RGB[1]*modulation,RGB[2]*modulation, 1.0, 0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0);
   mav_paletteColourSet(mav_palette_default,ccount,RGB[0]*modulation,RGB[1]*modulation,RGB[2]*modulation,1.0);
   ccount++;
   modulation -= SHADE_DIFFERENCE;
  }
 }
 for (i=0; i<=AMSTERDAM_MAX_COLOR_INDEX; i++)
 {
  GetLetterColor(i,AMSTERDAM_COLOR_CODE,RGB); 
  modulation=1.0;
  for (j=0; j<NUM_SHADES; j++) {
   mav_paletteMaterialSet(mav_palette_default,ccount, 0.3*RGB[0]*modulation,0.3*RGB[1]*modulation,0.3*RGB[2]*modulation, 1.0, RGB[0]*modulation,RGB[1]*modulation,RGB[2]*modulation, 1.0, 0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0);
   mav_paletteColourSet(mav_palette_default,ccount,RGB[0]*modulation,RGB[1]*modulation,RGB[2]*modulation,1.0);
   ccount++;
   modulation -= SHADE_DIFFERENCE;
  }
 }
 for (i=0; i<=KARLSRUHE_MAX_COLOR_INDEX; i++)
 {
  GetLetterColor(i,KARLSRUHE_COLOR_CODE,RGB); 
  modulation=1.0;
  for (j=0; j<NUM_SHADES; j++) {
   mav_paletteMaterialSet(mav_palette_default,ccount, 0.3*RGB[0]*modulation,0.3*RGB[1]*modulation,0.3*RGB[2]*modulation, 1.0, RGB[0]*modulation,RGB[1]*modulation,RGB[2]*modulation, 1.0, 0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0);
   mav_paletteColourSet(mav_palette_default,ccount,RGB[0]*modulation,RGB[1]*modulation,RGB[2]*modulation,1.0);
   ccount++;
   modulation -= SHADE_DIFFERENCE;
  }
 }
 mav_paletteMaterialSet(mav_palette_default,ccount, 0.3*1.0,0.3*1.0,0.3*1.0, 1.0, 1.0,1.0,1.0, 1.0, 0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0);
 mav_paletteColourSet(mav_palette_default,ccount,1.0,1.0,1.0,1.0);
 white_index=ccount;


 ccount--;
 fprintf (stderr," (%i entries).\n",ccount);
}

/* For a give database code and a city code, return the color */
/* map index with applied random shading selection. */

int MapColorCode (int ColorCode, int CityCode)
{
 int index=0;

 switch (CityCode) {
  case MANHATTAN_COLOR_CODE:
   if (ColorCode<0) {
    PrintRangeError(ColorCode);
    ColorCode=0;
   } else if (ColorCode>MANHATTAN_MAX_COLOR_INDEX) {
    PrintRangeError(ColorCode);
    ColorCode=MANHATTAN_MAX_COLOR_INDEX;
   }
   index=ColorCode;
   break;
  case AMSTERDAM_COLOR_CODE:
   if (ColorCode<0) {
    PrintRangeError(ColorCode);
    ColorCode=0;
   } else if (ColorCode>AMSTERDAM_MAX_COLOR_INDEX) {
    PrintRangeError(ColorCode);
    ColorCode=AMSTERDAM_MAX_COLOR_INDEX;
   }
   index=AmsterdamIndexConversionTable[ColorCode]+13;
   break;
  case KARLSRUHE_COLOR_CODE:
   if (ColorCode<0) {
    PrintRangeError(ColorCode);
    ColorCode=0;
   } else if (ColorCode>KARLSRUHE_MAX_COLOR_INDEX) {
    PrintRangeError(ColorCode);
    ColorCode=KARLSRUHE_MAX_COLOR_INDEX;
   }
   index=KarlsruheIndexConversionTable[ColorCode]+27;
   break;
 }
 return(COL_OFFSET+index*NUM_SHADES+(rand() % NUM_SHADES));
}

MAV_LCity *mav_newLCity(MAV_surfaceParams **sp)
{
   MAV_LCity *lc= (MAV_LCity *) mav_malloc(sizeof(MAV_LCity));
   lc->matrix= MAV_ID_MATRIX;
   return lc;
}

int mav_initLCity()
{
 /*
  MAV_object *obj;
*/
  /* create character class */
  mav_class_character= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_character, mav_characterDraw);
  mav_callbackBBSet(mav_win_all, mav_class_character, mav_characterBBox);

  /* create feature class */
  mav_class_feature= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_feature, mav_featureDraw);
  mav_callbackBBSet(mav_win_all, mav_class_feature, mav_featureBBox);

  /* create vector class */
  mav_class_vector= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_vector, mav_vectorDraw);
  mav_callbackBBSet(mav_win_all, mav_class_vector, mav_vectorBBox);

  /* create 2 SMS's. One, an objList, to build the city, then construct
     the other, an HBB, from this (leads to a more efficient hierarchy)  */
  city_build= mav_SMSNew(mav_SMSClass_objList, mav_objListNew());
  city_sms= mav_SMSNew(mav_SMSClass_HBB, mav_HBBNew());
  city_map= mav_SMSNew(mav_SMSClass_objList, mav_objListNew());

  /* Initialization */
  BuildColorTable ();
  Make_Font();
  MakeAllStreets();
  
  /* construct HBB from objList */
  mav_HBBConstructFromSMS(city_sms, city_build);

  /* delete objList (but not the objects it contains) as its no longer needed */
  mav_SMSDelete(city_build, MAV_FALSE);

  /* make SMS unselectable since we are not intersected in intersection */
  mav_SMSSelectabilitySet(city_sms, mav_win_all, MAV_FALSE);

  return 0;
}

#ifdef USE_SKY

#define DIVN	3

float phi0[DIVN],phi1[DIVN];
float t0[4*DIVN],t1[4*DIVN];
float sint0[4*DIVN],cost0[4*DIVN],sint1[4*DIVN],cost1[4*DIVN];
float sinp0[DIVN],cosp0[DIVN],sinp1[DIVN],cosp1[DIVN];

void initDrawCitySky()
{
  int i;
  float theta0;
  float theta1;

  for (i= 0; i<DIVN; i++) {
      phi0[i]=i/(float)DIVN;
      phi1[i]=(i+1)/(float)DIVN;
      sinp0[i]=sin(MAV_PI_OVER_2*phi0[i]);
      sinp1[i]=sin(MAV_PI_OVER_2*phi1[i]);
      cosp0[i]=cos(MAV_PI_OVER_2*phi0[i]);
      cosp1[i]=cos(MAV_PI_OVER_2*phi1[i]);
  }  
  for (i= 0; i<4*DIVN; i++) {
      theta0= 0.25*i/(float)DIVN;
      theta1= 0.25*(i+1)/(float)DIVN;
      sint0[i]=sin(MAV_PI_OVER_2*theta0);
      sint1[i]=sin(MAV_PI_OVER_2*theta1);
      cost0[i]=cos(MAV_PI_OVER_2*theta0);
      cost1[i]=cos(MAV_PI_OVER_2*theta1);
          
      t0[i]= 0.01+0.99*fabs(0.99-1.98*theta0);
      t1[i]= 0.01+0.99*fabs(0.99-1.98*theta1);
  }      
}
 
#if 0 /* JMC */
int drawCitySky()
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

  int radius=900;

/*  sp.mode= MAV_TEXTURE;
  sp.texture= TEX_SKY;
*/
  mav_surfaceParamsUse(sky);

  cent.x= mav_win_current->vp->eye.x;
  cent.y= mav_win_current->vp->eye.y;
  cent.z= mav_win_current->vp->eye.z;

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
              mav_gfxPolygonBegin ();
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
              mav_gfxPolygonEnd ();
            }
          else
            {
              mav_gfxPolygonBegin ();
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
              mav_gfxPolygonEnd ();

              mav_gfxPolygonBegin ();
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
              mav_gfxPolygonEnd ();
            }
        }
    }        

  return 0;
}
#else
int drawCitySky()
{
  MAV_cylinder cyl;
  MAV_rectangle r;
  MAV_object o;
  MAV_surfaceParams s;

  cyl.radius=590;
  cyl.height=450;
  cyl.endcap= 0;
  cyl.nverts=30;
  cyl.sp= sky;
  cyl.matrix= mav_matrixSet(0,-90,0, mav_win_current->vp->eye.x, mav_win_current->vp->eye.y+cyl.height/2.0-1.0, mav_win_current->vp->eye.z);

  r.width=2000;
  r.height=2000;
  s.mode=MAV_COLOUR;
  s.colour= MAV_COLOUR_WHITE;
  r.sp= &s;
  r.matrix= mav_matrixSet(0,-90,0, mav_win_current->vp->eye.x, mav_win_current->vp->eye.y+450, mav_win_current->vp->eye.z);

  glDisable(GL_FOG);
  glDisable(GL_CULL_FACE);
    
  o.the_data= &cyl;
  o.the_class= mav_class_cylinder;
  mav_cylinderDraw(&o, NULL);

  o.the_data= &r;
  o.the_class= mav_class_rectangle;
  mav_rectangleDraw(&o, NULL);

  glEnable(GL_CULL_FACE);
  glEnable(GL_FOG);

  return 0;
}
#endif
#endif

#define FLOOR_STEPS	4
float floor_gridsize;

void initDrawCityFloor()
{
 floor_gridsize=fog_distance/(float)FLOOR_STEPS;
}

int drawCityFloor()
{
  int i,j;
  MAV_vector cent;
  MAV_vector p;
  MAV_texCoord t;
  int iorigx, iorigz;
  float curx, curxx, curz, curzz;
    
  mav_surfaceParamsUse(lcfloor);

  cent=mav_win_current->vp->eye;
  
  iorigx=(int)(cent.x/floor_gridsize);
  iorigz=(int)(cent.z/floor_gridsize);
  
  curx=(float)(iorigx-FLOOR_STEPS)*floor_gridsize;
  curxx=curx+floor_gridsize;
  for (i=(iorigx-FLOOR_STEPS); i<=(iorigx+FLOOR_STEPS); i++) {
   curz=(float)(iorigz-FLOOR_STEPS)*floor_gridsize;
   curzz=curz+floor_gridsize;
   for (j=(iorigz-FLOOR_STEPS); j<=(iorigz+FLOOR_STEPS); j++) {
    mav_gfxPolygonBegin ();

    t.s=0.0;
    t.t=0.0;
    mav_gfxTexCoord (t);
    p.x= curx;
    p.y= -0.1;
    p.z= curz;
    mav_gfxVertex (p);

    t.s= 0.0;
    t.t= 1.0;
    mav_gfxTexCoord (t);
    p.x= curx;
    p.y= -0.1;
    p.z= curzz;
    mav_gfxVertex (p);

    t.s= 1.0;
    t.t= 1.0;
    mav_gfxTexCoord (t);
    p.x= curxx;
    p.y= -0.1;
    p.z= curzz;
    mav_gfxVertex (p);

    t.s=1.0;
    t.t=0.0;
    mav_gfxTexCoord (t);
    p.x= curxx;
    p.y= -0.1;
    p.z= curz;
    mav_gfxVertex (p);

    mav_gfxPolygonEnd ();
   
    curz += floor_gridsize;
    curzz += floor_gridsize;
   }
   curx += floor_gridsize;
   curxx += floor_gridsize;
  }
  
  return 0;
}

int mav_LCityDraw(MAV_object *obj, MAV_drawInfo *di)
{
    MAV_LCity *lc= (MAV_LCity *) mav_objectDataGet(obj);
    MAV_matrix m;
    MAV_vector right, view, up, eye;
    MAV_vector p;
    
    eye= mav_win_current->vp->eye;
    eye.x= 0.0;
    eye.y= MAP_ZOOM;
    eye.z= 0.0;
    
    right.x=1;
    right.y=0;
    right.z=0;

    view.x= 0;
    view.y= -1;
    view.z= 0;

    up= mav_vectorScalar(mav_vectorCrossProduct(view, right),-1);

    mav_gfxMatrixPush();
    mav_gfxMatrixMult(lc->matrix);

    /* Reset feature detector */
    current_feature=-1;

    mav_SMSDisplay(mav_win_current, city_sms);

#ifdef USE_SKY
    drawCitySky();
#endif
    drawCityFloor();

    glDisable(GL_FOG);
    
    if (drawMap) {
        m.mat[0][0]= right.x;
        m.mat[0][1]= right.y;
        m.mat[0][2]= right.z;
        m.mat[0][3]= 0.0;

        m.mat[1][0]= up.x;
        m.mat[1][1]= up.y;
        m.mat[1][2]= up.z;
        m.mat[1][3]= 0.0;

        m.mat[2][0]= -view.x;
        m.mat[2][1]= -view.y;
        m.mat[2][2]= -view.z;
        m.mat[2][3]= 0.0;

        m.mat[3][0]= 0.0;
        m.mat[3][1]= 0.0;
        m.mat[3][2]= 0.0;
        m.mat[3][3]= 1.0;

        mav_gfxMatrixLoad(m);
        mav_gfxMatrixTranslate(mav_vectorScalar(eye, -1.0));

        mav_surfaceParamsUse(tranny);

        mav_gfxDepthTestSet(MAV_FALSE);
        mav_SMSDisplayUnCulled(mav_win_current, city_map); 

        mav_surfaceParamsUse(mav_sp_default);
 
        mav_gfxPolygonBegin();
        p.x= 0; p.y=1;p.z=0;
        mav_gfxNormal(p);

        p= mav_vectorAdd(mav_win_current->vp->eye,mav_vectorScalar(mav_win_current->vp->right, 50));
        p.z -= MAP_OFFSET;
        p=mav_vectorScalar(p,map_scale);
        mav_gfxVertex(p);
        p= mav_vectorAdd(mav_win_current->vp->eye,mav_vectorScalar(mav_win_current->vp->view, 150));
        p.z -= MAP_OFFSET;
        p=mav_vectorScalar(p,map_scale);
        mav_gfxVertex(p);
        p= mav_vectorAdd(mav_win_current->vp->eye,mav_vectorScalar(mav_win_current->vp->right, -50));
        p.z -= MAP_OFFSET;
        p=mav_vectorScalar(p,map_scale);
        mav_gfxVertex(p);

        mav_gfxPolygonEnd();

        mav_gfxDepthTestSet(MAV_TRUE);
    }

    glEnable(GL_FOG);

    mav_gfxMatrixPop();
    return 0;
}

int mav_LCityBB(MAV_object *obj, MAV_BB *bb)
{
    MAV_LCity *lc= (MAV_LCity*) mav_objectDataGet(obj);
    MAV_BB tmp;

    tmp.min.x=-1000000; 
    tmp.min.y=-1000000; 
    tmp.min.z=-1000000; 

    tmp.max.x=1000000 ;
    tmp.max.y=1000000; 
    tmp.max.z=1000000; 
 
    mav_BBAlign(tmp, lc->matrix, bb);

    return 1;
}

int mav_LCityGetMatrix(MAV_object *obj, MAV_matrix **m)
{
    MAV_LCity *lc= (MAV_LCity *) mav_objectDataGet(obj);
    *m= &lc->matrix;
    return 1;
}

int mav_LCityID(MAV_object *obj, char **id)
{
    *id="LCity";
    return 1;
}

int mav_LCityGetUserdef(MAV_object *obj, void ***ud)
{
    MAV_LCity *lc= (MAV_LCity*) mav_objectDataGet(obj);
    *ud= &lc->userdef;
    return 1;
}

char *mav_LCityModuleID(void)
{
  return "LCity";
}

/* LC Bike control */
/* --------------- */

/* 
  Adjustment method: 
   - remove conffilename (see below)
   - restart system with handle centered
   - after the program comes up, wait a few seconds (centering)
   - move handle all the way to the left and right (scaling)
   - press one of the buttons to save the configuration settings
  If the configuration file exists, the settings will get loaded
*/ 
   
/* Autoadjustment variables */

#define conffilename	"bikesetup.cfg"

float maxxdiff=0.1;
float xscale=10.0;

#define NUM_CENTERCOUNT	20
int centercount=0;
float centersum=0.0;
float maxydiff=0.0;
float ycenter=0.0;
float yscale=0.0;

FILE *conffile;
int saveflag;

#if 0
struct timeval prevTime;
struct timezone prevZone;
struct timeval thisTime;
struct timezone thisZone;
#endif
int firsttime = 1;
float timediff;


 
void LCjoyMove(void *ignored)
{
#if 0
 float xdiff,ydiff;
 float lasteyex, lasteyez;

 /* calc time since last call */
 if (firsttime) {
      gettimeofday(&prevTime,&prevZone);
      firsttime=0;
 } else {
     gettimeofday(&thisTime,&thisZone);
 
     timediff=(thisTime.tv_sec-prevTime.tv_sec)+(thisTime.tv_usec-prevTime.tv_usec)/1.0e+6;
     timediff=RATE_TIME-timediff;
     if ((timediff>0.0) && (timediff<RATE_TIME)) {
      usleep((unsigned long)(timediff*1000000.0));
     } 
     prevTime=thisTime;
} 
 
 /* Check for configuration file */
 if (centercount==0) {
  if ((conffile=fopen(conffilename,"r"))!=NULL) {
   fprintf (stderr,"Loading configuration data from %s ...\n",conffilename);
   fscanf(conffile,"%f %f",&ycenter,&yscale);
   fclose(conffile);
   saveflag=0;
   centercount=NUM_CENTERCOUNT;
  } else {
   saveflag=1;
  }
 } 

 /* Get speed of bike and adjust for AD range */
 xdiff=0.5-mav_FFJoy_pos.x;
 /* Autoscale speed to 0.0 */
 if (xdiff>maxxdiff) {
  maxxdiff=xdiff;
  xscale=1.0/maxxdiff;
 }
 xdiff *= xscale;
 /* Null small speeds */
 if (xdiff<0.05) {
  xdiff=0.0;
 }
 
 /* Get direction of bike */
 ydiff=0.5-mav_FFJoy_pos.y;
 /* Center bike for first measurements */
 if (centercount<NUM_CENTERCOUNT) {
  centercount++;
  centersum += ydiff;
  if (centercount==NUM_CENTERCOUNT) {
   if (saveflag) {
    ycenter = centersum*(1.0/NUM_CENTERCOUNT);
   } 
  }
  ydiff=0.0;
 } else {
  /* Adjust for center */
  ydiff -= ycenter;
  if (saveflag) {
   /* Autoscale */
   if (fabs(ydiff)>maxydiff) {
    maxydiff=fabs(ydiff);
    if (maxydiff>0.0) {
     yscale=1.0/maxydiff;
    }
   } 
  }
  ydiff *= yscale;
  /* Scale steering amount according to speed */
  ydiff *= xdiff;
 }
 
 lasteyex = mav_win_current->vp->eye.x;
 lasteyez = mav_win_current->vp->eye.z;

  /* Move location */ 
  mav_navigate(mav_navigateForwards,mav_win_current->vp,xdiff,1.0,0.1); 
  mav_navigate(mav_navigateYaw,mav_win_current->vp,ydiff     ,1.0,0.1);
 
  /* Check bounds */
  if (keep_inside_bounds(&mav_win_current->vp->eye)) {
   mav_win_current->vp->eye.x = lasteyex;	
   mav_win_current->vp->eye.z = lasteyez;		 
  }
  mav_win_current->vp->view=mav_vectorNormalize(mav_win_current->vp->view);

 /* HORRID HACK TO POSSIBLY KEEP BIKER ON THE FLAT */

 mav_win_current->vp->up.x=0;
 mav_win_current->vp->up.y=1;
 mav_win_current->vp->up.z=0;
 
 mav_win_current->vp->eye.y= 2.2;

 /* END OF HORRID HACK TO POSSIBLE KEEP BIKER ON THE FLAT */

#endif
 
}

#if 0
int LCjoyPress(MAV_object *o, MAV_FFJoyEvent *je)
{
 if (je->movement==MAV_PRESSED) {
  /* Key down event */
  if (je->button==MAV_FFJOY_TRIGGER) {
   /* Button 0 pressed */
   if (saveflag) {
    fprintf (stderr,"Saving configuration data to %s ...\n",conffilename);
    conffile=fopen(conffilename,"w");
    fprintf(conffile,"%f %f",ycenter,yscale);
    fclose(conffile); 
    saveflag=0;
   }
   drawMap=1;
  } else if (je->button==MAV_FFJOY_LOZENGE) {
   /* Button 1 pressed */
   if (saveflag) {
    fprintf (stderr,"Saving configuration data to %s ...\n",conffilename);
    conffile=fopen(conffilename,"w");
    fprintf(conffile,"%f %f",ycenter,yscale);
    fclose(conffile); 
    saveflag=0;
   }
   drawMap=1;
  } 
 } else {
  /* Key up event */
  if (je->button==MAV_FFJOY_TRIGGER) {
   /* Button 0 released */
   drawMap=0;
  } else if (je->button==MAV_FFJOY_LOZENGE) {
   /* Button 1 released */
   drawMap=0;
  } 
 }
 return 1;
}
#endif

void enfog(void)
{
  glFogi(GL_FOG_MODE,GL_LINEAR);
  glFogfv(GL_FOG_COLOR,fogColour);
  glFogf(GL_FOG_START,1.0);
  glFogf(GL_FOG_END,fog_distance);
  glEnable(GL_FOG);
}

int mav_paletteTextureSet2(MAV_palette *p, int i, char *f)
{
  if (mav_opt_stereo) {
    mav_windowSet(mav_win_right);
    mav_paletteTextureSet(p,i,f);
    mav_windowSet(mav_win_left);
  }

  return mav_paletteTextureSet(p,i,f);
}

void mav_LCityModuleInit(void)
{
  /* Add the new module */
  mav_moduleNew(mav_LCityModuleID);

  /* Create new object class and set callbacks */
  /* create character class */
  mav_class_character= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_character, mav_characterDraw);
  mav_callbackBBSet(mav_win_all, mav_class_character, mav_characterBBox);

  /* create feature class */
  mav_class_feature= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_feature, mav_featureDraw);
  mav_callbackBBSet(mav_win_all, mav_class_feature, mav_featureBBox);

  /* create vector class */
  mav_class_vector= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_vector, mav_vectorDraw);
  mav_callbackBBSet(mav_win_all, mav_class_vector, mav_vectorBBox);

  /* create 2 SMS's. One, an objList, to build the city, then construct
     the other, an HBB, from this (leads to a more efficient hierarchy)  */

  city_build= mav_SMSNew(mav_SMSClass_objList, mav_objListNew());
  city_map= mav_SMSNew(mav_SMSClass_objList, mav_objListNew());
  city_sms= mav_SMSNew(mav_SMSClass_HBB, mav_HBBNew());

  grass= mav_surfaceParamsNew(MAV_TEXTURE, 0, 1, TEX_GRASS); 
  canal= mav_surfaceParamsNew(MAV_TEXTURE, 0, 1, TEX_CANAL);
  stone= mav_surfaceParamsNew(MAV_TEXTURE, 0, 1, TEX_STONE);
  
  mav_paletteTextureSet2 (mav_palette_default, TEX_GRASS , "./data/textures/grass.ppm");
  mav_paletteTextureSet2 (mav_palette_default, TEX_CANAL , "./data/textures/water.ppm");
  mav_paletteTextureSet2 (mav_palette_default, TEX_STONE , "./data/textures/stone.ppm");

  /* Initialization */
  BuildColorTable ();
  Make_Font();
  MakeAllStreets();
#ifdef USE_SKY  
  initDrawCitySky();
#endif
  initDrawCityFloor();

  /* Define textures */
#ifdef USE_SKY  
  sky= mav_surfaceParamsNew(MAV_TEXTURE, 0, 1,TEX_SKY);
#endif
  lcfloor= mav_surfaceParamsNew(MAV_TEXTURE, 0, 1,TEX_FLOOR);

  mav_paletteColourSet(mav_palette_default, TEX_TRANNY, 0,0,0.5,0.8);
  tranny= mav_surfaceParamsNew(MAV_COLOUR, TEX_TRANNY, 0,0);

  mav_paletteTextureSet2 (mav_palette_default, TEX_FLOOR , "./data/textures/rock5.ppm");
 
#ifdef USE_SKY
  mav_paletteTextureSet2 (mav_palette_default, TEX_SKY , "./data/textures/lcsky.ppm");
#endif

  mav_paletteColourSet(mav_palette_default, COL_WHITE, 1,1,1,1);
  map_white= mav_surfaceParamsNew(MAV_COLOUR, COL_WHITE, 0,0);  

  /* Joystick */
/*
  mav_FFJoyModuleInit();
  mav_FFJoyForceClear();

  mav_callbackFFJoySet(mav_win_all,mav_class_world,LCjoyPress);
  mav_frameFn1Add(LCjoyMove, NULL);
*/
          
  /* construct HBB from objList */
  mav_HBBConstructFromSMS(city_sms, city_build);

  /* delete objList (but not the objects it contains) as its no longer needed */
  mav_SMSDelete(city_build, MAV_FALSE);

  /* make SMS unselectable since we are not intersected in intersection */

  /* Setup fogging */
  if (mav_opt_stereo) {
    mav_windowSet(mav_win_right);
    enfog();
    mav_windowSet(mav_win_left);
  } 
  enfog();

  mav_SMSSelectabilitySet(city_sms, mav_win_all, MAV_FALSE);

  mav_class_lcity=mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_lcity, mav_LCityDraw);
  mav_callbackBBSet(mav_win_all, mav_class_lcity, mav_LCityBB);
  mav_callbackIDSet(mav_win_all, mav_class_lcity, mav_LCityID);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_lcity, mav_LCityGetUserdef);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_lcity, mav_LCityGetMatrix);
}

int keyPress(MAV_object *o, MAV_keyboardEvent *ke)
{
  if (ke->movement==MAV_PRESSED) {
    switch (ke->key) {
    case 'm':
      mav_vp_default.eye= mav_vectorSet(1684.35, 2, 0.00);
      break;
    case 'a':
      mav_vp_default.eye= mav_vectorSet(-842.17, 2, 1458.69);
      break;
    case 'k':
      mav_vp_default.eye= mav_vectorSet(-842.17, 2, -1458.69);
      break; 
    case 't':
      drawMap=!drawMap;
      break;
    }
  }

  return 1;
}

int main(int argc, char *argv[])
{
  MAV_SMS *sms;
  MAV_object *o;
  MAV_LCity *lcity;

  mav_opt_maxMaterials=750;
  mav_opt_maxColours=750;
  if (argc>1) mav_opt_stereo= MAV_STEREO_TWO_WINS;
  mav_initialise(&argc, argv);
  mav_LCityModuleInit();

  /* Set background color */ 
  mav_windowBackgroundColourSet(mav_win_all, fogColour[0], fogColour[1], fogColour[2]);
  mav_windowBackfaceCullSet(mav_win_all, MAV_TRUE);

  lcity= mav_newLCity(&mav_sp_default);

  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  mav_navigationMouseDefaultParams(mav_win_all, MAV_LEFT_BUTTON, mav_navigateYawFixedUp, 0.01, -0.0005,
                              mav_navigateTransForwardsFixedUp, 0.01, 0.0005);
  mav_navigationMouseDefaultParams(mav_win_all, MAV_RIGHT_BUTTON, mav_navigateRight, 0.01, 0.0005,
                              mav_navigateUp, 0.01, 0.0005);
  mav_navigationKeyboard(mav_win_all, mav_navigationKeyboardDefault);

  sms= mav_SMSObjListNew();
  o= mav_objectNew(mav_class_lcity, lcity);
  mav_SMSObjectAdd(sms, o);

  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyPress);

  mav_stp_default.offset=2.7;
  mav_vp_default.eye= mav_vectorSet(-842.17, 2, -1458.69); 

  while (1) 
  {
    mav_eventsCheck();
    mav_frameBegin();
    mav_SMSDisplay(mav_win_all, sms);
    mav_frameEnd();
  }
}


