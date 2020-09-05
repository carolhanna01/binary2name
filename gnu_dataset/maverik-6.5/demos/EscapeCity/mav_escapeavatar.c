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

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "maverik.h"
#include "mav_escapeavatar.h"

#ifdef DEVA
#include <ThreeDObjParams.hxx>
extern ThreeDObjParams *deva_getTDP(MAV_object *);
MAV_class *mav_class_avatar;
#endif

MAV_timer Atimer;
extern int wire;

char files[NUM_PARTS][100] = {
  "./models/hips.ac",
  "./models/neck.ac",
  "./models/head.ac",
  "./models/left_clavicle.ac",
  "./models/right_clavicle.ac",
  "./models/right_hand.ac",
  "./models/right_lower_arm.ac",
  "./models/right_upper_arm.ac",
  "./models/left_hand.ac",
  "./models/left_lower_arm.ac",
  "./models/left_upper_arm.ac",
  "./models/lower_torso.ac",
  "./models/upper_torso.ac",
  "./models/left_upper_leg.ac",
  "./models/left_lower_leg.ac",
  "./models/left_foot.ac",
  "./models/right_upper_leg.ac",
  "./models/right_lower_leg.ac",
  "./models/right_foot.ac",
};

int part_colours[NUM_PARTS] = {
  COLOUR_BLUE, COLOUR_SKIN, COLOUR_HAIR, COLOUR_WHITE,
  COLOUR_WHITE, COLOUR_SKIN, COLOUR_SKIN, COLOUR_WHITE,
  COLOUR_SKIN, COLOUR_SKIN, COLOUR_WHITE, COLOUR_WHITE,
  COLOUR_WHITE, COLOUR_BLUE, COLOUR_BLUE, COLOUR_BLACK,
  COLOUR_BLUE, COLOUR_BLUE, COLOUR_BLACK
};

MAV_vector offsets[NUM_PARTS]= {
  { -0.072, 7.21202, -1.703075 },
  { -0.00084, 2.90589, 1.347075 },
  { -0.00367, 10.65, 1.9961 },
  { -8.31702, -5.29735, 0.037970 },
  { 8.31702, -5.29735, 0.037970 },
  { 0.970001, -7.85, -4.4 },
  { 0.0, -12.649045, 0.0 },
  { 0.488288, -17.43445, 0.0 },
  { -0.97, -7.85, -4.4 },
  { 0.0, -12.649045, 0.0 },
  { -1.063061, -17.43445, 0.0 },
  { 0.0, 8.124279, 5.2517 },
  { 0.483099, -13.877290, 2.426865 },
  { -1.068495, -17.356901, -1.86941 },
  { 0.0, -16.551956, 0.0 },
  { -0.164850, -6.564404, 6.861835 },
  { 0.90647, -17.356901, -1.84463 },
  { 0.0, -16.551956, 0.0 },
  { -0.164850, -6.564404, 6.861835 },
};

/* relates object to a parent */
int relationship[NUM_PARTS][2] = {
  { HIPS, -1 },
  { LEFT_UPPER_LEG, HIPS },
  { LEFT_LOWER_LEG, LEFT_UPPER_LEG },
  { LEFT_FOOT, LEFT_LOWER_LEG },
  { RIGHT_UPPER_LEG, HIPS },
  { RIGHT_LOWER_LEG, RIGHT_UPPER_LEG },
  { RIGHT_FOOT, RIGHT_LOWER_LEG },
  { LOWER_TORSO, HIPS },
  { UPPER_TORSO, LOWER_TORSO },
  { NECK, UPPER_TORSO },
  { HEAD, NECK },
  { LEFT_CLAVICLE, UPPER_TORSO },
  { RIGHT_CLAVICLE, UPPER_TORSO },
  { LEFT_UPPER_ARM, LEFT_CLAVICLE },
  { LEFT_LOWER_ARM, LEFT_UPPER_ARM },
  { LEFT_HAND, LEFT_LOWER_ARM },
  { RIGHT_UPPER_ARM, RIGHT_CLAVICLE },
  { RIGHT_LOWER_ARM, RIGHT_UPPER_ARM },
  { RIGHT_HAND, RIGHT_LOWER_ARM }
};

/* same order as relationships */
MAV_vector relative_positions[NUM_PARTS] = {
  { 0.000000, 0.000000, 0.000000 },
  { -8.400009, 0.000000, 0.000000 },
  { 0.000000, -40.720001, 0.000000 },
  { 0.000000, -34.529999, 0.000000 },
  { 8.399997, 0.000000, 0.000000 },
  { 0.000004, -40.720001, 0.000000 },
  { 0.000003, -34.529999, 0.000000 },
  { 0.000000, 13.099998, -5.500000 },
  { 0.000000, 47.055008, 1.463499 },
  { 0.000000, 2.174988, 0.807901 },
  { 0.000000, 4.340012, 0.000000 },
  { -3.680010, -3.215012, 0.807901 },
  { 3.680000, -3.215012, 0.807901 },
  { -16.14008, 1.029999, 0.000000 },
  { 0.000000, -31.339996, 0.000000 },
  { 0.969999, -27.020401, 4.650000 },
  { 16.14008, 1.029999, 0.000000 },
  { 0.000000, -31.339996, 0.000000 },
  { -0.970000, -27.020401, 4.650000 }
};

double
mav_getTimer (void)
{
  mav_timerStop(&Atimer);
  return Atimer.wall;
}

int
mav_avatarGetMatrix
(MAV_object *obj, MAV_matrix **mat)
{
  *mat= &(((MAV_avatar *)obj->the_data)->matrix);
  return 1;
}

int
mav_avatarGetUserdef
(MAV_object *obj, void ***ud)
{
  *ud= &(((MAV_avatar *)obj->the_data)->userdef);
  return 1;
}

void
mav_partBBox
(MAV_part *part, MAV_matrix mat, MAV_BB *bb)
{
  MAV_partptr *p;
  MAV_object *obj;
  MAV_BB objbb;
  MAV_BB bbox;

  mat= mav_matrixMult (mat,part->position);
  mat= mav_matrixMult (mat,part->rotation);

  p= part->children;
  while (p)
    {
      mav_partBBox (p->part, mat, bb);
      p= p->next;
    }

  mav_listPointerReset (part->objects);
  while (mav_listItemNext (part->objects, (void *)&obj))
    {
      mav_callbackBBExec(mav_win_current, obj, &objbb);
      mav_BBAlign (objbb, mat, &bbox);

      if (bbox.min.x < bb->min.x) bb->min.x= bbox.min.x;
      if (bbox.min.y < bb->min.y) bb->min.y= bbox.min.y;
      if (bbox.min.z < bb->min.z) bb->min.z= bbox.min.z;
      if (bbox.max.x > bb->max.x) bb->max.x= bbox.max.x;
      if (bbox.max.y > bb->max.y) bb->max.y= bbox.max.y;
      if (bbox.max.z > bb->max.z) bb->max.z= bbox.max.z;
    }
}

int
mav_avatarBBox
(MAV_object *obj, MAV_BB *bb)
{
  MAV_avatar *a= (MAV_avatar *) obj->the_data;
  MAV_matrix mat;
  MAV_BB bbox;

  mat= MAV_ID_MATRIX;

  bbox.min.x= bbox.min.y= bbox.min.z= 10000000.0;
  bbox.max.x= bbox.max.y= bbox.max.z= -10000000.0;
  mav_partBBox (a->root, mat, &bbox);
  mav_BBAlign (bbox, a->vertical, &bbox);
  mav_BBAlign (bbox, a->matrix, bb);
  a->bb= *bb;

  return 1;
}

void
mav_BBDisplayWireWithSurfaceParams
(MAV_window *w, MAV_BB bb, MAV_surfaceParams sp)
{
  MAV_vector vert[8];
  MAV_vector norm;

  vert[0].x=bb.min.x;
  vert[0].y=bb.min.y;
  vert[0].z=bb.min.z;

  vert[1].x=bb.min.x;
  vert[1].y=bb.max.y;
  vert[1].z=bb.min.z;
  
  vert[2].x=bb.max.x;
  vert[2].y=bb.min.y;
  vert[2].z=bb.min.z;
  
  vert[3].x=bb.max.x;
  vert[3].y=bb.max.y;
  vert[3].z=bb.min.z;
  
  vert[4].x=bb.min.x;
  vert[4].y=bb.min.y;
  vert[4].z=bb.max.z;
  
  vert[5].x=bb.min.x;
  vert[5].y=bb.max.y;
  vert[5].z=bb.max.z;
  
  vert[6].x=bb.max.x;
  vert[6].y=bb.min.y;
  vert[6].z=bb.max.z;
  
  vert[7].x=bb.max.x;
  vert[7].y=bb.max.y;
  vert[7].z=bb.max.z;

  sp.mode= MAV_MATERIAL;
  mav_surfaceParamsUse(&sp);

  norm.x=0.0;
  norm.y=0.0;
  norm.z=-1.0;
  mav_gfxNormal(norm);
  mav_gfxLineClosedBegin();
  mav_gfxVertex(vert[0]);
  mav_gfxVertex(vert[1]);
  mav_gfxVertex(vert[3]);
  mav_gfxVertex(vert[2]);
  mav_gfxLineClosedEnd();
  
  norm.z=1.0;
  mav_gfxNormal(norm);
  mav_gfxLineClosedBegin();
  mav_gfxVertex(vert[4]);
  mav_gfxVertex(vert[5]);
  mav_gfxVertex(vert[7]);
  mav_gfxVertex(vert[6]);
  mav_gfxLineClosedEnd();
  
  norm.x=-1.0;
  norm.z=0.0;
  mav_gfxNormal(norm);
  mav_gfxLineBegin();
  mav_gfxVertex(vert[0]);
  mav_gfxVertex(vert[4]);
  mav_gfxLineEnd();
  
  norm.x=0.0;
  norm.y=1.0;
  mav_gfxNormal(norm);
  mav_gfxLineBegin();
  mav_gfxVertex(vert[1]);
  mav_gfxVertex(vert[5]);
  mav_gfxLineEnd();
  
  mav_gfxLineBegin();
  mav_gfxVertex(vert[3]);
  mav_gfxVertex(vert[7]);
  mav_gfxLineEnd();
  
  norm.y=-1.0;  
  mav_gfxNormal(norm);
  mav_gfxLineBegin();
  mav_gfxVertex(vert[2]);
  mav_gfxVertex(vert[6]);
  mav_gfxLineEnd();
}

void
mav_partDraw
(MAV_part *part, float dist)
{
  MAV_partptr *p;
  MAV_object *obj;
  MAV_composite *comp;
  MAV_surfaceParams **sp;

  mav_gfxMatrixPush ();
  mav_gfxMatrixMult (part->position);
  mav_gfxMatrixMult (part->rotation);

  /* display objects and children */
  mav_listPointerReset (part->objects);
  while (mav_listItemNext (part->objects, (void *)&obj))
    {
      if (!wire && dist > 1000.0)
	{
	  comp= (MAV_composite *)obj->the_data;

	  if (comp->numobj > 0)
	    {
	      mav_callbackGetSurfaceParamsExec (mav_win_current, comp->obj[0], &sp);

	      mav_gfxMatrixPush ();
	      mav_gfxMatrixMult (comp->matrix);
	      mav_BBDisplayWireWithSurfaceParams (mav_win_current, comp->bb, **sp);
	      mav_gfxMatrixPop ();
	    }
	}
      else if (!wire && dist > 500.0)
	{
	  comp= (MAV_composite *)obj->the_data;

	  if (comp->numobj > 0)
	    {
	      mav_callbackGetSurfaceParamsExec (mav_win_current, comp->obj[0], &sp);

	      mav_gfxMatrixPush ();
	      mav_gfxMatrixMult (comp->matrix);
	      mav_BBDisplayWithSurfaceParams (mav_win_current, comp->bb, *sp);
	      mav_gfxMatrixPop ();
	    }
	}
      else
	mav_callbackDrawExec (mav_win_current, obj, NULL);
    }

  p= part->children;
  while (p)
    {
      mav_partDraw (p->part, dist);
      p= p->next;
    }

  mav_gfxMatrixPop ();
}

int
mav_avatarDraw
(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_avatar *a= (MAV_avatar *) obj->the_data;
  MAV_vector dr;
  float dd;

#ifdef DEVA
  ThreeDObjParams *tdp= deva_getTDP(obj);
  mav_avatarSetVelocity (a, mav_vectorSet(tdp->x,tdp->y,tdp->z), 1);
#endif /* DEVA */

  dr.x= mav_win_current->vp->eye.x-a->matrix.mat[MAV_MATRIX_XCOMP];
  dr.y= mav_win_current->vp->eye.y-a->matrix.mat[MAV_MATRIX_YCOMP];
  dr.z= mav_win_current->vp->eye.z-a->matrix.mat[MAV_MATRIX_ZCOMP];
  dd= dr.x*dr.x+dr.y*dr.y+dr.z*dr.z;

  a->drawn= 1;

  if (mav_win_current==mav_win_left) mav_avatarAnimate (a);

  mav_gfxMatrixPush ();
  mav_gfxMatrixMult (a->matrix);
  mav_gfxMatrixMult (a->vertical);
  mav_gfxMatrixMult (a->rotation);

  mav_partDraw (a->root, dd);

  mav_gfxMatrixPop ();

  return 1;
}

void
mav_avatarInitialise (void)
{
  mav_paletteColourSet (mav_palette_default, COLOUR_WHITE, 0.627,0.743,0.878,0.0);
  mav_paletteColourSet (mav_palette_default, COLOUR_BLACK, 0.267,0.267,0.267,0.0);
  mav_paletteColourSet (mav_palette_default, COLOUR_SKIN, 0.800,0.544,0.455,0.0);
  mav_paletteColourSet (mav_palette_default, COLOUR_HAIR, 0.267,0.145,0.020,0.0);
  mav_paletteColourSet (mav_palette_default, COLOUR_BLUE, 0.043,0.282,0.529,0.0);
  mav_timerStart(&Atimer);
}

MAV_avatar*
mav_avatarBuild
(MAV_avatarCurves *curves, MAV_surfaceParams *jumper, MAV_surfaceParams *trousers)
{
  int i,j,n;
  MAV_matrix matrix;
  MAV_facet *fc;
  MAV_avatar *avatar;
  MAV_BB bb;
  MAV_composite *avatarComps[NUM_PARTS];
  MAV_partptr *pptr;

  avatar= mav_malloc(sizeof(MAV_avatar));
  avatar->last_time= -1.0;
  avatar->curves= curves;
  avatar->offset= 0.0;
  avatar->speed= 0.0;
  avatar->drawn= 0;

  for (i= 0; i< NUM_PARTS; i++)
    {
      avatar->part_list[i]= mav_malloc(sizeof(MAV_part));
      avatar->part_list[i]->part_num= i;
      avatar->part_list[i]->colour= part_colours[i];
      avatar->part_list[i]->objects= mav_listNew();
      avatar->part_list[i]->position= MAV_ID_MATRIX;
      avatar->part_list[i]->rotation= MAV_ID_MATRIX;
      avatar->part_list[i]->children= NULL;

      /* take a copy of the composite */
      avatarComps[i]= mav_malloc(sizeof(MAV_composite));
      matrix= mav_matrixSet (0.0,0.0,0.0, offsets[i].x,offsets[i].y,offsets[i].z);
      mav_compositeReadAC3D (files[i], avatarComps[i], matrix);
      avatarComps[i]->matrix= MAV_ID_MATRIX;
      avatarComps[i]->matrix= mav_matrixScaleSet (avatarComps[i]->matrix, 0.010211582);

      for (j= 0; j< avatarComps[i]->numobj; j++)
	{
	  if (i == UPPER_TORSO ||
	      i == LOWER_TORSO ||
	      i == LEFT_CLAVICLE ||
	      i == RIGHT_CLAVICLE ||
	      i == LEFT_UPPER_ARM ||
	      i == RIGHT_UPPER_ARM ||
	      i == LEFT_LOWER_ARM ||
	      i == RIGHT_LOWER_ARM)
	    {
	      /* set jumper */
	      if (avatarComps[i]->obj[j]->the_class == mav_class_facet)
		{
		  fc= (MAV_facet *)avatarComps[i]->obj[j]->the_data;
		  for (n= 0; n< fc->npolys; n++)
		    fc->sp[n]= jumper;
		}
	    }
	  else if (i == HIPS ||
		   i == LEFT_UPPER_LEG ||
		   i == RIGHT_UPPER_LEG ||
		   i == LEFT_LOWER_LEG ||
		   i == RIGHT_LOWER_LEG)
	    {
	      /* set trousers */
	      if (avatarComps[i]->obj[j]->the_class == mav_class_facet)
		{
		  fc= (MAV_facet *)avatarComps[i]->obj[j]->the_data;
		  for (n= 0; n< fc->npolys; n++)
		    fc->sp[n]= trousers;
		}
	    }
	}
    }
     
  for (i= 0; i< NUM_PARTS; i++)
    {
      if (relationship[i][1] == -1)
	{
	  /* root part */
	  avatar->root= avatar->part_list[relationship[i][0]];

	  /* position root object at origin */
	  mav_listItemAdd (avatar->root->objects, (void *)mav_objectNew (mav_class_composite, avatarComps[relationship[i][0]]));
	  avatar->part_list[relationship[i][0]]->position= mav_matrixSet (0.0,0.0,0.0, 0.0058695, 0.901687, -0.0323005);
	}
      else
	{
	  /* add a part child pointer */
	  pptr= mav_malloc(sizeof(MAV_partptr));
	  pptr->part= avatar->part_list[relationship[i][0]];
	  pptr->next= avatar->part_list[relationship[i][1]]->children;
	  avatar->part_list[relationship[i][1]]->children= pptr;

	  mav_listItemAdd (avatar->part_list[relationship[i][0]]->objects, (void *)mav_objectNew (mav_class_composite, avatarComps[relationship[i][0]]));

	  /* position object w.r.t parent */
	  avatar->part_list[relationship[i][0]]->position= mav_matrixSet (0.0,0.0,0.0, 0.010211582*relative_positions[i].x, 0.010211582*relative_positions[i].y, 0.010211582*relative_positions[i].z);
	}
    }

  mav_partBBox (avatar->root, MAV_ID_MATRIX, &bb);

  avatar->matrix= MAV_ID_MATRIX;
  avatar->rotation= MAV_ID_MATRIX;
  avatar->vertical= MAV_ID_MATRIX;

  return avatar;
}

static int
Find_PartNum
(char *str)
{
  if (strcmp(str, "HIPS") == 0)
    return HIPS;
  if (strcmp(str, "NECK") == 0)
    return NECK;
  if (strcmp(str, "HEAD") == 0)
    return HEAD;
  if (strcmp(str, "LEFT_CLAVICLE") == 0)
    return LEFT_CLAVICLE;
  if (strcmp(str, "RIGHT_CLAVICLE") == 0)
    return RIGHT_CLAVICLE;
  if (strcmp(str, "RIGHT_HAND") == 0)
    return NECK;
  if (strcmp(str, "RIGHT_LOWER_ARM") == 0)
    return RIGHT_LOWER_ARM;
  if (strcmp(str, "RIGHT_UPPER_ARM") == 0)
    return RIGHT_UPPER_ARM;
  if (strcmp(str, "LEFT_HAND") == 0)
    return LEFT_HAND;
  if (strcmp(str, "LEFT_LOWER_ARM") == 0)
    return LEFT_LOWER_ARM;
  if (strcmp(str, "LEFT_UPPER_ARM") == 0)
    return LEFT_UPPER_ARM;
  if (strcmp(str, "LOWER_TORSO") == 0)
    return LOWER_TORSO;
  if (strcmp(str, "UPPER_TORSO") == 0)
    return UPPER_TORSO;
  if (strcmp(str, "LEFT_UPPER_LEG") == 0)
    return LEFT_UPPER_LEG;
  if (strcmp(str, "LEFT_LOWER_LEG") == 0)
    return LEFT_LOWER_LEG;
  if (strcmp(str, "LEFT_FOOT") == 0)
    return LEFT_FOOT;
  if (strcmp(str, "RIGHT_UPPER_LEG") == 0)
    return RIGHT_UPPER_LEG;
  if (strcmp(str, "RIGHT_LOWER_LEG") == 0)
    return RIGHT_LOWER_LEG;
  if (strcmp(str, "RIGHT_FOOT") == 0)
    return RIGHT_FOOT;

  return -1;
}

MAV_avatarCurves *
mav_avatarReadCurves
(char *filename)
{
  MAV_avatarCurves *curves;
  FILE *in;
  int i,j;
  int num_points;
  float xcoord,angle;
  int b,v;
  char part_name[100];
  char axis[100];

  in= fopen(filename, "r");
  if (in == NULL)
    {
      fprintf(stderr, "mav_readAvatarCurves(): can't open file %s\n", filename);
      return 0;
    }

  curves= mav_malloc(sizeof(MAV_avatarCurves));

  fscanf(in, "%d", &curves->num_joints);
  curves->joints= mav_malloc(curves->num_joints*sizeof(MAV_joint));

  for (i= 0; i< curves->num_joints; i++)
    {
      fscanf(in, "%s", part_name);
      curves->joints[i].part_num= Find_PartNum (part_name);

      if (curves->joints[i].part_num != -1)
	{
	  fscanf(in, "%f", &curves->joints[i].offset);
	  fscanf(in, "%s", axis);
	  if (strcmp(axis, "ROLL") == 0)
	    curves->joints[i].axis= ROLL;
	  if (strcmp(axis, "PITCH") == 0)
	    curves->joints[i].axis= PITCH;
	  if (strcmp(axis, "YAW") == 0)
	    curves->joints[i].axis= YAW;
	}
      else
	{
	  curves->joints[i].offset= 0.0;
	  curves->joints[i].axis= -1;
	}

      curves->joints[i].start= 0.0;
      curves->joints[i].end= 1.0;

      fscanf(in, "%d", &num_points);
      curves->joints[i].num_beziers= 1+(num_points-4)/3;

      curves->joints[i].beziers= mav_malloc(curves->joints[i].num_beziers*sizeof(MAV_bezier));

      v= 0; b= 0;
      for (j= 0; j< num_points; j++)
	{
	  fscanf(in, "%f", &xcoord);
	  curves->joints[i].beziers[b].value[v]= xcoord;

	  v++;
	  if (v == 4)
	    {
	      if (j < num_points-1)
		curves->joints[i].beziers[b+1].value[0]= curves->joints[i].beziers[b].value[3];
	      b++;
	      v= 1;
	    }
	}

      v= 0; b= 0;
      for (j= 0; j< num_points; j++)
	{
	  fscanf(in, "%f", &angle);
	  curves->joints[i].beziers[b].angle[v]= angle/90.0;

	  v++;
	  if (v == 4)
	    {
	      if (j < num_points-1)
		curves->joints[i].beziers[b+1].angle[0]= curves->joints[i].beziers[b].angle[3];
	      b++;
	      v= 1;
	    }
	}
    }

  fclose(in);
  return curves;
}

void
mav_avatarUpdatePosition
(MAV_avatar *avatar)
{
  MAV_vector position;
  float time;
  MAV_vector new_position;

  /* get last position from matrix */
  position.x= avatar->matrix.mat[MAV_MATRIX_XCOMP];
  position.y= avatar->matrix.mat[MAV_MATRIX_YCOMP];
  position.z= avatar->matrix.mat[MAV_MATRIX_ZCOMP];

  /* work out the new position */
  time= mav_getTimer();

  if (avatar->last_time < 0.0) avatar->last_time= time;

  new_position.x= position.x+(time-avatar->last_time)*avatar->velocity.x*avatar->speed;
  new_position.y= position.y+(time-avatar->last_time)*avatar->velocity.y*avatar->speed;
  new_position.z= position.z+(time-avatar->last_time)*avatar->velocity.z*avatar->speed;
  avatar->last_time= time;

  /* and update his matrix accordingly */
  avatar->matrix= mav_matrixSet (0.0,0.0,0.0, new_position.x,new_position.y,new_position.z);
}

void
mav_avatarAnimate
(MAV_avatar *avatar)
{
  int i,j;
  float t;
  float value;
  float v,val;
  MAV_vector direction;
  float s;
  float angle;
  float scl;
  float time;

  /* calculate angle scale for walk speed */
  scl= 0.85*avatar->speed*sqrt(avatar->velocity.x*avatar->velocity.x+avatar->velocity.y*avatar->velocity.y+avatar->velocity.z*avatar->velocity.z);

  /* work out the rotation angle */
  direction= avatar->velocity;
  s= sqrt(direction.x*direction.x+direction.y*direction.y+direction.z*direction.z);
  if (s > 0.0)
    {
      direction.x /= s;
      direction.y /= s;
      direction.z /= s;
      angle= 57.29577951*acos(direction.z);
      if (direction.x < 0.0) angle= -angle;
    }
  else
    angle= 0.0;
  avatar->rotation= mav_matrixSet (0.0,0.0,angle, 0.0,0.0,0.0);

  time= mav_getTimer();
  value= fmod(time,1.0);
  value -= avatar->offset;

  for (i= 0; i< avatar->curves->num_joints; i++)
    {
      /* account for joint cycle offset */
      v= value-avatar->curves->joints[i].offset;
      while (v < 0.0) v += 1.0;
      while (v > 1.0) v -= 1.0;
      
      /* move t to [start..end] */
      t= avatar->curves->joints[i].start+v*(avatar->curves->joints[i].end-avatar->curves->joints[i].start);
      
      for (j= 0; j< avatar->curves->joints[i].num_beziers; j++)
	{
	  if (t >= avatar->curves->joints[i].beziers[j].value[0] &&
	      t <= avatar->curves->joints[i].beziers[j].value[3])
	    {
	      /* move to [0..1] */
	      val= (t-avatar->curves->joints[i].beziers[j].value[0])/(avatar->curves->joints[i].beziers[j].value[3]-avatar->curves->joints[i].beziers[j].value[0]);

	      /* evaluate bezier */
	      angle= scl*90.0*((1.0-val)*(1.0-val)*(1.0-val)*avatar->curves->joints[i].beziers[j].angle[0]+
				 3.0*val*(1.0-val)*(1.0-val)*avatar->curves->joints[i].beziers[j].angle[1]+
				 3.0*val*val*(1.0-val)*avatar->curves->joints[i].beziers[j].angle[2]+
				 val*val*val*avatar->curves->joints[i].beziers[j].angle[3]);

	      if (avatar->curves->joints[i].part_num == -1)
		{
		  /* update vertical height */
		  avatar->vertical= mav_matrixSet (0.0,0.0,0.0, 0.0,-0.055*angle/105.0,0.0);
		}
	      else
		{
		  /* update rotation angle */
		  switch (avatar->curves->joints[i].axis)
		    {
		    case ROLL :
		      avatar->part_list[avatar->curves->joints[i].part_num]->rotation= mav_matrixSet (-angle,0.0,0.0, 0.0,0.0,0.0);
		      break;
		    case PITCH :
		      avatar->part_list[avatar->curves->joints[i].part_num]->rotation= mav_matrixSet (0.0,-angle,0.0, 0.0,0.0,0.0);
		      break;
		    case YAW :
		      avatar->part_list[avatar->curves->joints[i].part_num]->rotation= mav_matrixSet (0.0,0.0,-angle, 0.0,0.0,0.0);
		      break;
		    }
		}

	      j= avatar->curves->joints[i].num_beziers;
	    }
	}
    }
}

void
mav_avatarSetVelocity
(MAV_avatar *avatar, MAV_vector direction, float velocity)
{
  avatar->speed= velocity;
  avatar->velocity= direction;
}

#ifdef DEVA

int mav_avatarIdentify(MAV_object *obj, char **name)
{
   *name="avatar";
   return 1;
}

MAV_avatarCurves *curves;

int mav_avatarInitialiseDeva(MAV_object **obj)
{
  MAV_avatar *av;
  printf("Making an avatar instance\n");
  av= mav_avatarBuild(curves);
  *obj= mav_objectNew(mav_class_avatar, av);
  mav_avatarSetVelocity(av, mav_vectorSet(1,0,0), 1.25);
  printf("Done that instance thing\n");
  return 1;
}

int mav_avatarInitialiseDevaModule()
{
  MAV_avatar *av;

  printf("Trying to create an avatar class\n");

  mav_class_avatar= mav_classNew();
  mav_callbackDrawSet (mav_win_all, mav_class_avatar, mav_avatarDraw);
  mav_callbackBBSet (mav_win_all, mav_class_avatar, mav_avatarBBox);
  mav_callbackGetMatrixSet (mav_win_all, mav_class_avatar, mav_avatarGetMatrix);
  mav_callbackGetUserdefSet (mav_win_all, mav_class_avatar, mav_avatarGetUserdef);
  mav_avatarInitialise();
  curves= mav_avatarReadCurves("./curves/walking.cset");

  return 1;
}

int mav_avatarRemoveDeva(MAV_object *obj)
{
    printf("Horrible things! dearydearyme!\n");
    exit(1);
}

int mav_avatarUpdateDeva(MAV_object *obj, void *message)
{
    return 1;
}

#endif /* DEVA */



