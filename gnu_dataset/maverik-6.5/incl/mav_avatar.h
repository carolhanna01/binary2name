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

#ifndef _MAV_AVATAR_INCLUDE
#define _MAV_AVATAR_INCLUDE

#ifdef MAVAPI
#undef MAVAPI
#endif
#if defined(WIN32) && !defined(__CYGWIN__) 
#ifdef LIBMAV_AVATAR_EXPORTS
#define MAVAPI __declspec(dllexport) 
#else
#define MAVAPI __declspec(dllimport) 
#endif
#else
#define MAVAPI 
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Avatar */

typedef struct {
  int num;
  MAV_object *obj;
  MAV_matrix position;
  MAV_matrix rotation;
  struct MAV_AVATARPARTPTR *children;
} MAV_avatarPart;

typedef struct MAV_AVATARPARTPTR {
  MAV_avatarPart *part;
  struct MAV_AVATARPARTPTR *next;
} MAV_avatarPartPtr;

typedef struct {
  MAV_avatarPart *root;
  MAV_avatarPart *part[19];
  MAV_surfaceParams *sp[5];
  int movement;
  float speed;
  float offset;
  int animFromMat;
  int animate;
  int move;
  MAV_timer timer;
  float time;
  float last_time;
  MAV_vector last_pos;
  MAV_matrix vertical;
  MAV_matrix rotation;
  MAV_matrix matrix;
  MAV_vector right_hand;
  MAV_vector left_hand;
  int holding_right;
  int holding_left;
  MAV_surfaceParams *laser_sp;
  void *userdef;
} MAV_avatar;

MAVAPI extern MAV_class *mav_class_avatar;
MAVAPI int mav_avatarDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_avatarBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_avatarBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_avatarIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_avatarID(MAV_object *o, char **id);
MAVAPI int mav_avatarGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_avatarGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_avatarGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_avatarDump(MAV_object *o);

MAVAPI MAV_avatar *mav_avatarNew(MAV_surfaceParams **sp);
MAVAPI void mav_avatarAnimate(MAV_avatar *avatar);
MAVAPI void mav_avatarMove(MAV_avatar *avatar);
MAVAPI void mav_avatarSetSurfaceParams(MAV_avatar *, MAV_surfaceParams **sp);



/* Module initialise */

MAVAPI char *mav_avatarModuleID(void);
MAVAPI int mav_avatarModuleInit(void);

#ifdef __cplusplus
}
#endif
#endif
