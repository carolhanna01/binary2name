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


#if defined(WIN32) && !defined(__CYGWIN__)
#pragma warning( disable : 4244 ) /* '=' : conversion from 'const double ' to 'float ', possible loss of data */
#pragma warning( disable : 4018 ) /* '<' : signed/unsigned mismatch */
#pragma warning( disable : 4305 ) /* '=' : truncation from 'const double ' to 'float ' */
#endif

#include "mav_avatar.h"

#define ROLL 0
#define PITCH 1
#define YAW 2

#define NUM_PARTS 19

#define HIPS 0
#define NECK 1
#define HEAD 2
#define RIGHT_CLAVICLE 3
#define LEFT_CLAVICLE 4
#define LEFT_HAND 5
#define LEFT_LOWER_ARM 6
#define LEFT_UPPER_ARM 7
#define RIGHT_HAND 8
#define RIGHT_LOWER_ARM 9
#define RIGHT_UPPER_ARM 10
#define LOWER_TORSO 11
#define UPPER_TORSO 12
#define RIGHT_UPPER_LEG 13
#define RIGHT_LOWER_LEG 14
#define RIGHT_FOOT 15
#define LEFT_UPPER_LEG 16
#define LEFT_LOWER_LEG 17
#define LEFT_FOOT 18


typedef struct {
  float value[4];
  float angle[4];
} MAVLIB_avatarBezier;

typedef struct {
  int part_num;
  int axis;
  float offset;
  float start;
  float end;
  int num_beziers;
  MAVLIB_avatarBezier *beziers;
} MAVLIB_avatarJoint;

typedef struct {
  int num_joints;
  MAVLIB_avatarJoint *joints;
} MAVLIB_avatarCurves;

extern MAVLIB_avatarCurves *mavlib_avatarCurve1;

MAVLIB_avatarCurves *mavlib_avatarReadCurves(char *filename);


