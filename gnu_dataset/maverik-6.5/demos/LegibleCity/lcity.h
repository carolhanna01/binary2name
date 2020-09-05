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


#include <maverik.h>

/* Data structures, classes and routine prototypes for new objects */

typedef struct {
  MAV_vector normal;
  int num_verts;
  MAV_vector *verts;
} LC_Polygon;

typedef struct {
  float letter_width;
  int num_polys;
  MAV_BB bb;
  LC_Polygon *polys;
} Alphabet;

typedef struct {
  Alphabet *character;
  MAV_vector centre;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
} Character;

extern MAV_class *mav_class_character;
int mav_characterDraw(MAV_object *, MAV_drawInfo *di);
int mav_characterBBox(MAV_object *, MAV_BB *);


typedef struct {
  int num_polys;
  LC_Polygon *polys;
  MAV_BB bb;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
} Feature;

extern MAV_class *mav_class_feature;
int mav_featureDraw(MAV_object *, MAV_drawInfo *di);
int mav_featureBBox(MAV_object *, MAV_BB *);


typedef struct vector {
  char name[100];
  float phrase_length;
  float extra_width;
  int num_characters;
  MAV_vector start;
  MAV_vector end;
  MAV_BB bb;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  struct vector *next;
} Vector;

extern MAV_class *mav_class_vector;
int mav_vectorDraw(MAV_object *, MAV_drawInfo *di);
int mav_vectorBBox(MAV_object *, MAV_BB *);



/* Routines to build the city */

void Make_Font(void);
void Make_Streets(char *, char *);

/* LOD parameters */

extern int apply_lod;
extern float fog_distance;
extern float box_distance;

