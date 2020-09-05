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
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#if defined(WIN32) || defined(__CYGWIN__)
#include <windows.h>
#endif
#include <GL/glu.h>

extern MAV_BB city_bb;
extern MAV_SMS *city_build;
Alphabet alphabet[255];

#define NUM_LETTERS 65
#define NUM_COLOURS 104
#define UNKNOWN 0
#define CANAL 1
#define PAVEMENT 2
#define GRASS 3

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

static float colours[NUM_COLOURS][3] = {
  { 1.0000, 0.6627, 0.2588 },
  { 0.8118, 0.5176, 0.3529 },
  { 1.0000, 0.3725, 0.0118 },
  { 1.0000, 0.3333, 0.0902 },
  { 1.0000, 0.1765, 0.0471 },
  { 0.9490, 0.4471, 0.1333 },
  { 1.0000, 0.0902, 0.0235 },
  { 1.0000, 0.6627, 0.2588 },
  { 0.9490, 0.4471, 0.1333 },
  { 1.0000, 0.3333, 0.0902 },
  { 0.6745, 0.4824, 0.4824 },
  { 0.8118, 0.5176, 0.3529 },
  { 1.0000, 0.1020, 0.0902 },
  { 1.0000, 0.5098, 0.0824 },
  { 0.9882, 0.1725, 0.2275 },
  { 0.9490, 0.4471, 0.1333 },
  { 1.0000, 0.1020, 0.0902 },
  { 1.0000, 0.1765, 0.0471 },
  { 1.0000, 0.3333, 0.0902 },
  { 0.9922, 0.0078, 0.0078 },
  { 1.0000, 0.3059, 0.2118 },
  { 0.9490, 0.4471, 0.1333 },
  { 1.0000, 0.6627, 0.2588 },
  { 0.9882, 0.1725, 0.2275 },
  { 0.6745, 0.4824, 0.4824 },
  { 1.0000, 0.1020, 0.0902 },
  { 1.0000, 0.6627, 0.2588 },
  { 1.0000, 0.0902, 0.0235 },
  { 0.9922, 0.4196, 0.3137 },
  { 1.0000, 0.3059, 0.2118 },
  { 1.0000, 0.6627, 0.2588 },
  { 1.0000, 0.0902, 0.0235 },
  { 1.0000, 0.5098, 0.0824 },
  { 0.9922, 0.0078, 0.0078 },
  { 0.8118, 0.5176, 0.3529 },
  { 1.0000, 0.3725, 0.0118 },
  { 1.0000, 0.6627, 0.2588 },
  { 1.0000, 0.1020, 0.0902 },
  { 1.0000, 0.1765, 0.0471 },
  { 1.0000, 0.3059, 0.2118 },
  { 1.0000, 0.3059, 0.2118 },
  { 0.9922, 0.0078, 0.0078 },
  { 1.0000, 0.5098, 0.0824 },
  { 0.6745, 0.4824, 0.4824 },
  { 1.0000, 0.3725, 0.0118 },
  { 0.6745, 0.4824, 0.4824 },
  { 0.9922, 0.4196, 0.3137 },
  { 1.0000, 0.3333, 0.0902 },
  { 1.0000, 0.6627, 0.2588 },
  { 1.0000, 0.1765, 0.0471 },
  { 0.9961, 0.9961, 0.4431 },
  { 0.8275, 0.3922, 0.1176 },
  { 0.7843, 0.7843, 0.7843 },
  { 0.7843, 0.7843, 0.7843 },
  { 0.8118, 0.5176, 0.3529 },
  { 0.9961, 0.5843, 0.5843 },
  { 0.5725, 0.5059, 0.8902 },
  { 0.8471, 0.6078, 0.6078 },
  { 0.9961, 0.5843, 0.5843 },
  { 0.6941, 0.1608, 0.0039 },
  { 0.9961, 0.9961, 0.4431 },
  { 0.9961, 0.9961, 0.4431 },
  { 0.9961, 0.5843, 0.5843 },
  { 1.0000, 0.5608, 0.2235 },
  { 1.0000, 0.6745, 0.8706 },
  { 0.5725, 0.5059, 0.8902 },
  { 0.9804, 0.9804, 0.7490 },
  { 0.7137, 0.7922, 0.2667 },
  { 0.6941, 0.1608, 0.0039 },
  { 0.7412, 0.4431, 0.2157 },
  { 0.9961, 0.7843, 0.1451 },
  { 0.6941, 0.1608, 0.0039 },
  { 0.8118, 0.5176, 0.3529 },
  { 0.7137, 0.7922, 0.2667 },
  { 0.7412, 0.4431, 0.2157 },
  { 0.2667, 0.9922, 0.5529 },
  { 0.8275, 0.3922, 0.1176 },
  { 0.9961, 0.5843, 0.5843 },
  { 0.8118, 0.5176, 0.3529 },
  { 0.7843, 0.7843, 0.7843 },
  { 0.4902, 0.8588, 0.8627 },
  { 0.8118, 0.5176, 0.3529 },
  { 0.4902, 0.8588, 0.8627 },
  { 0.8275, 0.3922, 0.1176 },
  { 0.2667, 0.9922, 0.5529 },
  { 0.9804, 0.9804, 0.7490 },
  { 1.0000, 0.7882, 0.4745 },
  { 0.9961, 0.5843, 0.5843 },
  { 0.7843, 0.7843, 0.7843 },
  { 1.0000, 0.6745, 0.8706 },
  { 0.9804, 0.9804, 0.7490 },
  { 0.6941, 0.1608, 0.0039 },
  { 0.7843, 0.7843, 0.7843 },
  { 0.7961, 0.9961, 0.7882 },
  { 0.7843, 0.7843, 0.7843 },
  { 0.8471, 0.6078, 0.6078 },
  { 0.8471, 0.6078, 0.6078 },
  { 0.8471, 0.6078, 0.6078 },
  { 0.8471, 0.6078, 0.6078 },
  { 0.8471, 0.6078, 0.6078 },
  { 0.0274, 0.2600, 0.3800 }, /* canal = 101 */
  { 0.4000, 0.4000, 0.4000 }, /* pavem = 102 */
  { 0.0274, 0.3800, 0.1600 }, /* grass = 103 */
  { 1.0000, 1.0000, 1.0000 }  /* unknown = 104 */
};



void Make_Font(void)
{
  MAV_vector r0, r1, *verts;
  FILE *file;
  char filename[100];
  int i, j, l, c, num_verts, index[100];

  /* define font colours */
  for (i=0; i<NUM_COLOURS; i++) {
    mav_paletteMaterialSet(mav_palette_default, i+1, 0.3*colours[i][0], 0.3*colours[i][1], 0.3*colours[i][2], 1.0, colours[i][0], colours[i][1], colours[i][2], 1.0, 0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0);
    mav_paletteColourSet(mav_palette_default, i+1, colours[i][0], colours[i][1], colours[i][2], 1.0);
  }

  /* store letter widths */
  for (l=0; l<NUM_LETTERS+1; l++) alphabet[(int)(letter_widths[l][0])].letter_width= letter_widths[l][1];

  /*  read letter description from file */
  for (l=0; l<NUM_LETTERS; l++) {
    c= ascii[l];

    sprintf(filename, "data/font/%d.cdat", c);
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



/* routines for polygon tessellation */

Feature *feature;
int feature_vertices;
int num_polygons;

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
  printf("GLU error: %s\n", gluErrorString(en));
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

void CALLBACK count_polygons(void *v)
{
  feature_vertices++;

  if (feature_vertices == 3) {
    feature->num_polys++;
    feature_vertices= 0;
  }
}

void CALLBACK my_edge_flag(GLboolean f)
{
}

void Build_Feature(FILE *file, int type)
{
  MAV_vector *verts;
  GLdouble data[3];
  GLUtesselator *tobj;
  int i, num_verts;

  feature= mav_malloc(sizeof(Feature));

  /* read in vertices */
  fscanf(file, "%d\n", &num_verts);
  verts= mav_malloc(num_verts*sizeof(MAV_vector));

  /* initialise bounding box */
  mav_BBCompInit(&feature->bb);

  /* read vertices */
  for (i=0; i<num_verts; i++) {
    fscanf(file, "(%f,%f,%*f)\n", &verts[i].x, &verts[i].z);
    verts[i].x= -verts[i].x;
    verts[i].y= 0.0;

    /* update bounding box */
    mav_BBCompPt(verts[i], &feature->bb);
  }

  /* features can be concave, need to tessellate in convex polygons. Use GLU for this */

  /* initialise a triangulator object */
  tobj= gluNewTess();
  if (!tobj) {
    fprintf(stderr, "Failed to allocate GLUtesselator\n");
    exit(0);
  }

  /* set triangulator callbacks to count triangles */
  feature->num_polys= 0;
  feature_vertices= 0;
  gluTessCallback(tobj, GLU_TESS_BEGIN, NULL);
  gluTessCallback(tobj, GLU_TESS_END, NULL);
  gluTessCallback(tobj, GLU_TESS_ERROR, (MAVLIB_GLUCB) my_error);
  gluTessCallback(tobj, GLU_TESS_VERTEX, (MAVLIB_GLUCB) count_polygons);
  gluTessCallback(tobj, GLU_TESS_EDGE_FLAG, (MAVLIB_GLUCB) my_edge_flag);

  /* count polygons */
  gluTessBeginPolygon(tobj, NULL);
  gluTessBeginContour(tobj);
  for (i=0; i<num_verts; i++) {
    data[0]= (GLdouble)verts[i].x;
    data[1]= (GLdouble)verts[i].y;
    data[2]= (GLdouble)verts[i].z;
      
    gluTessVertex(tobj, data, (GLvoid *)&verts[i]);
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

  /* re-initialise triangulatorObj for triangulation */
  tobj= gluNewTess();
  if (!tobj) {
    fprintf(stderr, "Failed to allocate GLUtesselator\n");
    exit(0);
  }

  feature->num_polys= 0;
  gluTessCallback(tobj, GLU_TESS_BEGIN, (MAVLIB_GLUCB) my_begin);
  gluTessCallback(tobj, GLU_TESS_END, NULL);
  gluTessCallback(tobj, GLU_TESS_ERROR, (MAVLIB_GLUCB) my_error);
  gluTessCallback(tobj, GLU_TESS_VERTEX, (MAVLIB_GLUCB) my_vertex);
  gluTessCallback(tobj, GLU_TESS_EDGE_FLAG, (MAVLIB_GLUCB) my_edge_flag);

  /* triangulate */
  gluTessBeginPolygon(tobj, NULL);
  gluTessBeginContour(tobj);
  for (i=0; i<num_verts; i++) {
    data[0]= (double)verts[i].x;
    data[1]= (double)verts[i].y;
    data[2]= (double)verts[i].z;
    
    gluTessVertex(tobj, data, (void *)&verts[i]);
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
    feature->sp->colour= 101;
    break;
  case PAVEMENT :
    feature->sp->colour= 102;
    break;
  case GRASS :
    feature->sp->colour= 103;
    break;
  default :
    feature->sp->colour= 104;
    break;
  }

  /* create Maverik object and insert in SMS */
  mav_SMSObjectAdd(city_build, mav_objectNew(mav_class_feature, feature));
}



Vector *vector_list;

Vector* Find_Vector(char *name)
{
  Vector *v;

  v= vector_list;
  while (v) {
    if (strcmp(v->name, name)==0) return v;
    v= v->next;
  }

  return NULL;
}

void Make_Streets(char *strfile, char *lstfile)
{
  MAV_BB bb;
  MAV_vector newdr;
  Alphabet *letter;
  Character *character;
  Vector *v, *current_vector;
  float width, height, depth, spacing, letter_width;
  float current_spacing_x=0, current_spacing_y=0;
  float vx1=0, vy1=0, vx2, vy2, dx=0, dy=0, dlen, dir=0, newx, newy;
  int num_polygons;
  FILE *file;
  unsigned char str[100], c;

  /* open file */
  file= fopen(strfile, "r");
  if (!file) {
    fprintf(stderr, "Failed to open street file %s\n", strfile);
    exit(0);
  }

  /* Initialise BB of city */
  mav_BBCompInit(&city_bb);

  /* read city layout description */
  vector_list= NULL;

  while (fscanf(file, "%s", str)!=EOF) {

    if (strncmp((const char *) str, ";", 1) == 0)  /* comment - read to end of line */
    {
      fread(&c, 1, 1, file);
      while ((int)c != 9 && (int)c != 10) fread(&c, 1, 1, file);
    }
    else
    {
      if (!strncmp((const char *) str, "canal", 5)) Build_Feature(file, CANAL);
      else if (!strncmp((const char *) str, "pavem", 5)) Build_Feature(file, PAVEMENT);
      else if (!strncmp((const char *) str, "grass", 5)) Build_Feature(file, GRASS);
      else 
      {
	/* its a street */
	v= mav_malloc(sizeof(Vector));
	strcpy(v->name, (const char *) str);
	
	/* read start and end point of vector */
	fscanf(file, "%f %f", &v->start.x, &v->start.z); 
	v->start.x= -v->start.x;
	v->start.y= 0.0;

	fscanf(file, "%*f %f %f", &v->end.x, &v->end.z);
	v->end.x= -v->end.x;
	v->end.y= 0.0;

	/* read colour data */
	v->sp= mav_surfaceParamsNew(MAV_COLOUR, 0, 0, 0);
	fscanf(file, "%*f %i", &v->sp->colour);
	v->sp->colour++;

	/* initialise characters */
	v->phrase_length= 0.0;
	v->num_characters= 0;
	      
	/* calculate bounding box */
	v->bb.min.x= (v->start.x < v->end.x) ? v->start.x : v->end.x;
	v->bb.min.y= 0.0;
	v->bb.min.z= (v->start.z < v->end.z) ? v->start.z : v->end.z;
	v->bb.max.x= (v->start.x > v->end.x) ? v->start.x : v->end.x;
	v->bb.max.y= 0.0;
	v->bb.max.z= (v->start.z > v->end.z) ? v->start.z : v->end.z;

	/* update city BB with vector BB */
	mav_BBCompBB(v->bb, &city_bb);

	/* set transformation */
	v->matrix= MAV_ID_MATRIX;;
	      
	/* place in vector list */
	v->next= vector_list;
	vector_list= v;
	      
	/* create Maverik object and insert in SMS */
	mav_SMSObjectAdd(city_build, mav_objectNew(mav_class_vector, v));
      }
    }
  }

  fclose(file);

  /* position and instance alphabet characters */
  file= fopen(lstfile, "r");
  if (!file) {
    fprintf(stderr, "Failed to open letters file %s\n", lstfile);
    exit(0);
  }

  /* first find phrase lengths */
  current_vector= NULL;
  while (fscanf(file, "%s", str)!=EOF) {

    if (strncmp((const char *) str, ";", 1) == 0)   /* comments - read to end of line */
    {
      fread(&c, 1, 1, file);
      while ((int)c != 9 && (int)c != 10) fread(&c, 1, 1, file);
    }
    else
    {
      if (strncmp((const char *) str, ":", 1))  /* start of new vector */
      {
	/* Find the vector for the name */
	current_vector= Find_Vector((char *) str);
	if (!current_vector) {
	  fprintf(stderr, "Unknown vector name %s\n", str);
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
	dlen= sqrt(dx*dx+dy*dy);
	dx /= dlen;
	dy /= dlen;
	dir= atan2(dx,dy);
      }
      else /* Contents of vector */
      {
	if (!current_vector) {
	  fprintf(stderr, "Can't have letters before a vector\n");
	  exit(0);
	}

	/* Read dimensions */
	fscanf(file, "%f %f %f %f", &width, &height, &depth, &spacing);

	if (strlen((const char *) str) > 1) 
	{
	  letter= &alphabet[(int)str[1]];
	  letter_width= letter->letter_width;
	}
	else
	  letter_width= 0.8;
	
	/* Account for character spacing and width */
	current_spacing_x += 0.1*width*dx;
	current_spacing_y += 0.1*width*dy;

	current_spacing_x += (letter_width+0.1)*width*dx;
	current_spacing_y += (letter_width+0.1)*width*dy;

	/* Calculate length of phrase */
	current_vector->phrase_length= (letter_width+0.1)*width+sqrt(current_spacing_x*current_spacing_x+current_spacing_y*current_spacing_y);
	current_vector->num_characters++;
      }
    }
  }

  fclose(file);

  /* calculate extra character widths */
  v= vector_list;
  while (v) {
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

  /* re-parse file to instance characters now we know their sizes */
  file= fopen(lstfile, "r");
  if (!file) {
    fprintf(stderr, "Failed to open letters file %s\n", lstfile);
    exit(0);
  }

  current_vector= NULL;
  num_polygons= 0;
  while (fscanf(file, "%s", str)!=EOF) {

    if (strncmp((const char *) str, ";", 1) == 0) /* comment - read to end of line */
    {
      fread(&c, 1, 1, file); 
      while ((int)c != 9 && (int)c != 10) fread(&c, 1, 1, file);
    }
    else
    {
      if (strncmp((const char *) str, ":", 1)) /* start of vector */
      {
	/* Find the vector for the name */
	current_vector= Find_Vector ((char *) str);
	if (!current_vector) {
	  fprintf(stderr, "Unknown vector name %s\n", str);
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
	dlen= sqrt(dx*dx+dy*dy);
	dx /= dlen;
	dy /= dlen;
	dir= atan2(dx,dy);
      }
      else /* contents of vector */
      {
	if (!current_vector) {
	  fprintf(stderr, "Can't have letters before a vector\n");
	  exit(0);
	}

	fscanf(file, "%g %g %g %g", &width, &height, &depth, &spacing);
	
	width *= current_vector->extra_width;

	if (strlen((const char *) str) > 1) /* Letter */
	{
	  letter= &alphabet[(int)str[1]];
	  letter_width= letter->letter_width;
	  num_polygons+=letter->num_polys;

	  /* create the character */ 
	  character= mav_malloc(sizeof(Character));
	  character->character= letter;

	  /* copy colour information */
	  character->sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, 0, 0);
	  character->sp->material= current_vector->sp->colour;

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
	  mav_SMSObjectAdd(city_build, mav_objectNew(mav_class_character, character));

	  /* update city bounding box */
	  mav_BBAlign(character->character->bb, character->matrix, &bb);
	  mav_BBCompBB(bb, &city_bb);

	  /* calculate center of character for LOD processing */
	  character->centre= mav_vectorScalar(mav_vectorAdd(bb.min, bb.max), 0.5);
	}
	else /* Space */
	{
	  letter_width= 0.8;
	  current_spacing_x += 0.1*width*dx;
	  current_spacing_y += 0.1*width*dy;
	}

	current_spacing_x += (letter_width+0.1)*width*dx;
	current_spacing_y += (letter_width+0.1)*width*dy;
      }
    }
  }

  fclose(file);
  fprintf(stdout, "%d polygons\n", num_polygons);
}

