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

typedef struct poly {
  unsigned char num;
  unsigned char drawn;
  MAV_surfaceParams sp;
  MAV_vector normal;
  MAV_vector verts[4];
  MAV_texCoord texcoords[4];
  struct poly *next;
} Poly;

typedef struct mav_billboard {
  MAV_surfaceParams sp;
  float width, height;
  MAV_vector rot;
  MAV_BB bb;
  MAV_matrix matrix;
  void *userdef;
  struct mav_billboard *next;
} MAV_billboard;

typedef struct {
  MAV_object *obj;
  MAV_vector pos;
  float dist;
} BB;

typedef struct {
  int num_vertices;
  MAV_vector vertices[6];
  MAV_BB bound;
} Occluder;

typedef struct {
  MAV_BB occluder;       /* axis-aligned bb of occluder */
  MAV_BB global_bb;      /* transformed bb */
  MAV_matrix matrix;
  int num_vertices;
  MAV_vector vertices[6];
  MAV_BB bound;
  MAV_surfaceParams sp;
  void *userdef;
} MAV_occluder;

typedef struct {
  MAV_vector start;
  MAV_vector end;
  int visible;
} Candidate_Edge;

typedef struct {
  int num_edges;
  int num_visible_edges;
  Candidate_Edge edges[MAX_CANDIDATE_EDGES];
  MAV_BB bound;
} Candidate;

typedef struct {
  MAV_vector p;
  float dist;
} Intersection;

typedef struct {
  int id;
  int building;
  int type;
  int minx,miny;
  int maxx,maxy;
  MAV_vector nw,sw,se,ne;
  MAV_vector cent;
  float height;
  unsigned int num_polys;
  Poly *polys;
  unsigned int num_lod_polys;
  Poly *lod_polys;
#ifdef DISPLAY_LISTS
  unsigned int cell_display_list;
  unsigned int lod_display_list;
  unsigned int composite_display_list;
  unsigned int feature_display_list;
#endif
#ifdef COMPOSITES
  MAV_list *composites;
  MAV_list *features;
#endif
  MAV_BB bb;
  MAV_matrix matrix;
  void *userdef;
} MAV_cityCell;

typedef struct {
  MAV_object *obj;
  float dist;
  int visible;
} Visible_Object;

typedef struct {
  int type;
  int id;
} Block;
