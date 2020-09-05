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

#ifndef _MAV_AVATAR_H
#define _MAV_AVATAR_H

#define NUM_PARTS 19

#define HIPS 0
#define NECK 1
#define HEAD 2
#define LEFT_CLAVICLE 3
#define RIGHT_CLAVICLE 4
#define RIGHT_HAND 5
#define RIGHT_LOWER_ARM 6
#define RIGHT_UPPER_ARM 7
#define LEFT_HAND 8
#define LEFT_LOWER_ARM 9
#define LEFT_UPPER_ARM 10
#define LOWER_TORSO 11
#define UPPER_TORSO 12
#define LEFT_UPPER_LEG 13
#define LEFT_LOWER_LEG 14
#define LEFT_FOOT 15
#define RIGHT_UPPER_LEG 16
#define RIGHT_LOWER_LEG 17
#define RIGHT_FOOT 18

#define ROLL 0
#define PITCH 1
#define YAW 2

#define COLOUR_WHITE 149
#define COLOUR_BLUE 148
#define COLOUR_SKIN 147
#define COLOUR_BLACK 146
#define COLOUR_HAIR 145

typedef struct mav_part {
  int part_num;
  int colour;
  MAV_list *objects;              /* part has a list of objects */
  MAV_matrix position;            /* positional transformation */
  MAV_matrix rotation;            /* rotational transformation */
  struct mav_partptr *children;   /* list of child parts */
} MAV_part;

typedef struct mav_partptr {
  MAV_part *part;
  struct mav_partptr *next;
} MAV_partptr;

typedef struct bezier {
  float value[4];
  float angle[4];
} MAV_bezier;

typedef struct joint {
  int part_num;
  int axis;
  float offset;
  float start;
  float end;
  int num_beziers;
  MAV_bezier *beziers;
} MAV_joint;

typedef struct {
  int num_joints;
  MAV_joint *joints;
} MAV_avatarCurves;

typedef struct {
  MAV_part *root;
  MAV_part *part_list[NUM_PARTS];
  MAV_avatarCurves *curves;
  MAV_BB bb;
  MAV_matrix vertical;
  MAV_matrix rotation;
  MAV_matrix matrix;
  MAV_vector velocity;
  float speed;
  int drawn;
  float last_time;
  float offset;
  void *userdef;
} MAV_avatar;

int mav_avatarGetMatrix (MAV_object *obj, MAV_matrix **mat);
int mav_avatarGetUserdef (MAV_object *obj, void ***ud);
int mav_avatarBBox (MAV_object *obj, MAV_BB *bb);
int mav_avatarDraw (MAV_object *obj, MAV_drawInfo *di);

void mav_avatarInitialise (void);
MAV_avatar *mav_avatarBuild (MAV_avatarCurves *curves, MAV_surfaceParams *jumper, MAV_surfaceParams *trousers);
MAV_avatarCurves *mav_avatarReadCurves (char *filename);
void mav_avatarAnimate (MAV_avatar *avatar);
void mav_avatarUpdatePosition (MAV_avatar *avatar);
void mav_avatarSetVelocity (MAV_avatar *avatar, MAV_vector direction, float velocity);

#endif
