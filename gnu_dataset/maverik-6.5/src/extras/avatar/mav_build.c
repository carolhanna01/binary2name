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


#include "maverik.h"
#include "mavlib_avatar.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define HAND_X 0.0
#define HAND_Y 1.5
#define HAND_Z -0.3

extern MAV_list *mavlib_avatarList;



char mavlib_avatarFileName[][100] = {
  "hips.ac",
  "neck.ac",
  "head.ac",
  "left_clavicle.ac",
  "right_clavicle.ac",
  "right_hand.ac",
  "right_lower_arm.ac",
  "right_upper_arm.ac",
  "left_hand.ac",
  "left_lower_arm.ac",
  "left_upper_arm.ac",
  "lower_torso.ac",
  "upper_torso.ac",
  "left_upper_leg.ac",
  "left_lower_leg.ac",
  "left_foot.ac",
  "right_upper_leg.ac",
  "right_lower_leg.ac",
  "right_foot.ac"
};

MAV_vector mavlib_avatarOffset[]= {
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
  { -0.164850, -6.564404, 6.861835 }
};

int mavlib_avatarRelationship[][2] = {
  { HIPS, -1 },
  { RIGHT_UPPER_LEG, HIPS },
  { RIGHT_LOWER_LEG, RIGHT_UPPER_LEG },
  { RIGHT_FOOT, RIGHT_LOWER_LEG },
  { LEFT_UPPER_LEG, HIPS },
  { LEFT_LOWER_LEG, LEFT_UPPER_LEG },
  { LEFT_FOOT, LEFT_LOWER_LEG },
  { LOWER_TORSO, HIPS },
  { UPPER_TORSO, LOWER_TORSO },
  { NECK, UPPER_TORSO },
  { HEAD, NECK },
  { RIGHT_CLAVICLE, UPPER_TORSO },
  { LEFT_CLAVICLE, UPPER_TORSO },
  { RIGHT_UPPER_ARM, RIGHT_CLAVICLE },
  { RIGHT_LOWER_ARM, RIGHT_UPPER_ARM },
  { RIGHT_HAND, RIGHT_LOWER_ARM },
  { LEFT_UPPER_ARM, LEFT_CLAVICLE },
  { LEFT_LOWER_ARM, LEFT_UPPER_ARM },
  { LEFT_HAND, LEFT_LOWER_ARM }
};

MAV_vector mavlib_avatarRelPos[] = {
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



/* Routine to create an avatar */

MAV_avatar *mav_avatarNew(MAV_surfaceParams **sp)
{
  MAV_avatar *avatar= (MAV_avatar *) mav_malloc(sizeof(MAV_avatar));
  MAV_composite *avatarComps[NUM_PARTS];
  MAV_matrix matrix;
  MAV_avatarPartPtr *pptr;
  char filename[200];
  int i, bk;

  if (!getenv("MAV_HOME")) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: MAV_HOME variable not set\n");
    exit(1);
  }

  bk= mav_opt_output;
  mav_opt_output= MAV_SILENT;
  /* create parts */
  for (i=0; i<NUM_PARTS; i++) {
    avatar->part[i]= (MAV_avatarPart *) mav_malloc(sizeof(MAV_avatarPart));
    avatar->part[i]->num= i;
    avatar->part[i]->position= MAV_ID_MATRIX;
    avatar->part[i]->rotation= MAV_ID_MATRIX;
    avatar->part[i]->children= NULL;

    /* read the part description */
    avatarComps[i]= (MAV_composite *) mav_malloc(sizeof(MAV_composite));

    sprintf(filename, "%s/src/extras/avatar/data/%s", getenv("MAV_HOME"), mavlib_avatarFileName[i]);
    matrix= mav_matrixSet(0,0,0, mavlib_avatarOffset[i].x, mavlib_avatarOffset[i].y, mavlib_avatarOffset[i].z);
    if (mav_compositeReadAC3D(filename, avatarComps[i], matrix)==MAV_FALSE) {
      mav_opt_output= bk;
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: failed to read avatar file %s\n", filename);
      exit(1);
    }
    avatarComps[i]->matrix= MAV_ID_MATRIX; 
    avatarComps[i]->matrix= mav_matrixScaleSet(avatarComps[i]->matrix, 0.010211582);
  }
  mav_opt_output= bk;

  /* define relationships joining parts */
  for (i=0; i<NUM_PARTS; i++) {
    if (mavlib_avatarRelationship[i][1] == -1)
    {
      /* root part */
      avatar->root= avatar->part[mavlib_avatarRelationship[i][0]];
      
      /* position root object at origin */
      avatar->root->obj= mav_objectNew(mav_class_composite, avatarComps[mavlib_avatarRelationship[i][0]]);
      avatar->part[mavlib_avatarRelationship[i][0]]->position= mav_matrixSet(0.0,0.0,0.0, 0.0058695, 0.901687, -0.0323005);
    }
    else
    {
      /* add a part child pointer */
      pptr= (MAV_avatarPartPtr *) mav_malloc(sizeof(MAV_avatarPartPtr));
      pptr->part= avatar->part[mavlib_avatarRelationship[i][0]];
      pptr->next= avatar->part[mavlib_avatarRelationship[i][1]]->children;
      
      avatar->part[mavlib_avatarRelationship[i][1]]->children= pptr;
      
      avatar->part[mavlib_avatarRelationship[i][0]]->obj= mav_objectNew(mav_class_composite, avatarComps[mavlib_avatarRelationship[i][0]]);
    
      /* position object w.r.t parent */
      avatar->part[mavlib_avatarRelationship[i][0]]->position= mav_matrixSet(0.0,0.0,0.0, 0.010211582*mavlib_avatarRelPos[i].x, 0.010211582*mavlib_avatarRelPos[i].y, 0.010211582*mavlib_avatarRelPos[i].z);
    }
  }

  /* set surface params */
  mav_avatarSetSurfaceParams(avatar, sp);

  avatar->offset= 0.0;
  avatar->vertical= mav_matrixSet(0,0,0,0,-1.7,0);
  avatar->rotation= mav_matrixSet(0,0,180,0,0,0);
  avatar->matrix= MAV_ID_MATRIX;
  mav_timerStart(&avatar->timer);
  avatar->speed=0.0;
  avatar->animate=0;
  avatar->move=0;
  avatar->animFromMat=0;
  avatar->last_pos=MAV_NULL_VECTOR;
  avatar->last_time=-1.0;

  avatar->right_hand= mav_vectorSet(HAND_X,HAND_Y,HAND_Z);
  avatar->left_hand= mav_vectorSet(HAND_X,HAND_Y,HAND_Z); 
  avatar->holding_right= 0;
  avatar->holding_left= 0;
  avatar->laser_sp= mav_sp_default;

  /* Maintain a list of all avatars */
  mav_listItemAdd(mavlib_avatarList, (void *) avatar);
  
  return avatar;
}



/* Routine for finding part names */

int mavlib_avatarFindPartNum(char *str)
{
  if (strcmp(str, "HIPS") == 0) return HIPS;
  if (strcmp(str, "NECK") == 0) return NECK;
  if (strcmp(str, "HEAD") == 0) return HEAD;
  if (strcmp(str, "RIGHT_CLAVICLE") == 0) return RIGHT_CLAVICLE;
  if (strcmp(str, "LEFT_CLAVICLE") == 0) return LEFT_CLAVICLE;
  if (strcmp(str, "LEFT_HAND") == 0) return NECK;
  if (strcmp(str, "LEFT_LOWER_ARM") == 0) return LEFT_LOWER_ARM;
  if (strcmp(str, "LEFT_UPPER_ARM") == 0) return LEFT_UPPER_ARM;
  if (strcmp(str, "RIGHT_HAND") == 0) return RIGHT_HAND;
  if (strcmp(str, "RIGHT_LOWER_ARM") == 0) return RIGHT_LOWER_ARM;
  if (strcmp(str, "RIGHT_UPPER_ARM") == 0) return RIGHT_UPPER_ARM;
  if (strcmp(str, "LOWER_TORSO") == 0) return LOWER_TORSO;
  if (strcmp(str, "UPPER_TORSO") == 0) return UPPER_TORSO;
  if (strcmp(str, "RIGHT_UPPER_LEG") == 0) return RIGHT_UPPER_LEG;
  if (strcmp(str, "RIGHT_LOWER_LEG") == 0) return RIGHT_LOWER_LEG;
  if (strcmp(str, "RIGHT_FOOT") == 0) return RIGHT_FOOT;
  if (strcmp(str, "LEFT_UPPER_LEG") == 0) return LEFT_UPPER_LEG;
  if (strcmp(str, "LEFT_LOWER_LEG") == 0) return LEFT_LOWER_LEG;
  if (strcmp(str, "LEFT_FOOT") == 0) return LEFT_FOOT;

  return -1;
}



/* Routine for reading a set of animation curves */

MAVLIB_avatarCurves *mavlib_avatarReadCurves(char *filename)
{
  MAVLIB_avatarCurves *curves;
  FILE *in;
  int i,j;
  int num_points;
  float xcoord,angle;
  int b,v;
  char part_name[100];
  char axis[100];
  char fn2[200];

  if (!getenv("MAV_HOME")) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: MAV_HOME variable not set\n");
    return NULL;
  }

  sprintf(fn2, "%s/src/extras/avatar/data/%s", getenv("MAV_HOME"), filename);
  in= fopen(fn2, "r");
  if (!in) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: can't open avatar curve file %s\n", filename);
    return NULL;
  }

  curves= (MAVLIB_avatarCurves *) mav_malloc(sizeof(MAVLIB_avatarCurves));

  fscanf(in, "%d", &curves->num_joints);
  curves->joints= (MAVLIB_avatarJoint *) mav_malloc(curves->num_joints*sizeof(MAVLIB_avatarJoint));

  for (i=0; i<curves->num_joints; i++) {
    fscanf(in, "%s", part_name);
    curves->joints[i].part_num= mavlib_avatarFindPartNum(part_name);

    if (curves->joints[i].part_num != -1)
    {
      fscanf(in, "%f", &curves->joints[i].offset);
      fscanf(in, "%s", axis);
      if (strcmp(axis, "ROLL") == 0) curves->joints[i].axis= ROLL;
      if (strcmp(axis, "PITCH") == 0) curves->joints[i].axis= PITCH;
      if (strcmp(axis, "YAW") == 0) curves->joints[i].axis= YAW;
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
    
    curves->joints[i].beziers= (MAVLIB_avatarBezier *) mav_malloc(curves->joints[i].num_beziers*sizeof(MAVLIB_avatarBezier));

    v= 0; b= 0;
    for (j=0; j<num_points; j++) {
      fscanf(in, "%f", &xcoord);
      curves->joints[i].beziers[b].value[v]= xcoord;

      v++;
      if (v==4) {
	if (j<num_points-1) curves->joints[i].beziers[b+1].value[0]= curves->joints[i].beziers[b].value[3];
	b++;
	v= 1;
      }
    }

    v= 0; b= 0;
    for (j=0; j<num_points; j++) {
      fscanf(in, "%f", &angle);
      curves->joints[i].beziers[b].angle[v]= angle/90.0;

      v++;
      if (v==4) {
	if (j < num_points-1) curves->joints[i].beziers[b+1].angle[0]= curves->joints[i].beziers[b].angle[3];
	b++;
	v= 1;
      }
    }
  }
  
  fclose(in);
  return curves;
}



/* Routine to set the avatars surface parameters */

void mav_avatarSetSurfaceParams(MAV_avatar *avatar, MAV_surfaceParams **sp)
{
  MAV_composite *c;
  MAV_facet *fc;
  int i,n;

  if (!sp) 
  {
    avatar->sp[0]= NULL;
    avatar->sp[1]= NULL;
    avatar->sp[2]= NULL;
    avatar->sp[3]= NULL;
    avatar->sp[4]= NULL;
  }
  else
  {
    /* store reference to surface parameters */
    avatar->sp[0]= sp[0]; /* skin */
    avatar->sp[1]= sp[1]; /* hair */
    avatar->sp[2]= sp[2]; /* face */
    avatar->sp[3]= sp[3]; /* jumper */
    avatar->sp[4]= sp[4]; /* trousers */

    /* set in composite */
    for (i=0; i<NUM_PARTS; i++) {
      c= (MAV_composite *) mav_objectDataGet(avatar->part[i]->obj);
      fc= (MAV_facet *) mav_objectDataGet(c->obj[0]);
      
      if (i == UPPER_TORSO || i == LOWER_TORSO || i == RIGHT_CLAVICLE ||
	  i == LEFT_CLAVICLE || i == RIGHT_UPPER_ARM || i == LEFT_UPPER_ARM || 
	  i == RIGHT_LOWER_ARM || i == LEFT_LOWER_ARM)
      {
	/* set jumper */
	for (n=0; n<fc->npolys; n++) if (sp[3]) fc->sp[n]= sp[3];
      }
      else if (i == HIPS || i == RIGHT_UPPER_LEG || i == LEFT_UPPER_LEG ||
	       i == RIGHT_LOWER_LEG || i == LEFT_LOWER_LEG)
      {
	/* set trousers */
	for (n= 0; n<fc->npolys; n++) if (sp[4]) fc->sp[n]= sp[4];
      }
      else if (i == NECK || i == RIGHT_HAND || i == LEFT_HAND ||
	       i == RIGHT_FOOT || i== LEFT_FOOT)
      {
	/* set skin */
	for (n= 0; n<fc->npolys; n++) if (sp[0]) fc->sp[n]= sp[0];
      }
      else 
      {
	/* set face and hair for head */
	for (n= 0; n<48; n++) if (sp[1]) fc->sp[n]= sp[1]; /* hair */
	for (n= 48; n<fc->npolys; n++) if (sp[2]) fc->sp[n]= sp[2]; /* face  */
      }
    }
  }
}
