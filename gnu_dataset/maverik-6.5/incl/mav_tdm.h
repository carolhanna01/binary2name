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

#ifndef _MAV_TDM_INCLUDE
#define _MAV_TDM_INCLUDE

#ifdef MAVAPI
#undef MAVAPI
#endif
#if defined(WIN32) && !defined(__CYGWIN__) 
#ifdef LIBMAV_TDM_EXPORTS
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

/* Tracker position and orientation data structure */

#define TDM_OUT_OF_RANGE 1
#define TDM_CALIBRATED 2

typedef struct {
  MAV_vector pos;
  MAV_vector u;
  MAV_vector v;
  MAV_vector n;
  MAV_matrix matrix;
  MAV_quaternion quaternion;
  int status;
} MAV_TDMPos;

MAVAPI extern MAV_TDMPos mav_TDM_pos[];
MAVAPI extern MAV_matrix mav_TDM_matrix[];
MAVAPI extern MAV_viewModifierFn mav_TDM_vp[];



/* Tracker event data structure */

typedef struct {
  MAV_TDMPos pos;
  MAV_line line;
  int intersects;
  MAV_object *obj;
  MAV_objectIntersection objint;
  int button;
  int tracker;
  int movement;
} MAV_TDMEvent;

MAVAPI extern MAV_callback *mav_callback_TDM;
typedef int (*MAV_callbackTDMFn)(MAV_object *, MAV_TDMEvent *);
MAVAPI void mav_callbackTDMSet(MAV_window *w, MAV_class *c, MAV_callbackTDMFn fn);
MAVAPI int  mav_callbackTDMExec(MAV_window *w, MAV_object *o, MAV_TDMEvent *te);

/* Tracker events reserved for system, e.g navigation */

MAVAPI extern MAV_callback *mav_callback_sysTDM;
MAVAPI void mav_callbackSysTDMSet(MAV_window *w, MAV_class *c, MAV_callbackTDMFn fn);
MAVAPI int  mav_callbackSysTDMExec(MAV_window *w, MAV_object *o, MAV_TDMEvent *te);



/* Cursor object */

typedef struct {
  int tracker;
  int style;
  MAV_surfaceParams *sp;
  void *userdef;
} MAV_TDMCursor;

MAVAPI extern MAV_class *mav_class_TDMCursor;
MAVAPI int mav_TDMCursorDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_TDMCursorBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_TDMCursorID(MAV_object *o, char **id);
MAVAPI int mav_TDMCursorGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_TDMCursorGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_TDMCursorDump(MAV_object *o);



/* Site specific defines for the AIG's setup */

#define MAV_TDM_RED     0
#define MAV_TDM_BLUE    1
#define MAV_TDM_HMD     2
#define MAV_TDM_VEL     3

#define MAV_TDM_MIDDLE  0
#define MAV_TDM_TOP     1
#define MAV_TDM_RIGHT   2
#define MAV_TDM_LEFT    3
#define MAV_TDM_BOTTOM  4



/* Supporting routines */

MAVAPI MAV_TDMPos mav_TDMPosQuery(int trk);
MAVAPI int  mav_TDMButtonQuery(int trk, int button);
MAVAPI void mav_TDMXYZScaleSet(float x, float y, float z);
MAVAPI void mav_TDMXYZOriginSet(float x, float y, float z);
MAVAPI void mav_TDMOffsetSet(float off);
MAVAPI void mav_TDMDrawScaleSet(float sc);
MAVAPI void mav_TDMScaleSet(float sc);

MAVAPI void mav_TDMSingleXYZScaleSet(int trk, float x, float y, float z);
MAVAPI void mav_TDMSingleXYZOriginSet(int trk, float x, float y, float z);
MAVAPI void mav_TDMSingleOffsetSet(int trk, float offset);
MAVAPI void mav_TDMSingleDrawScaleSet(int trk, float sc);
MAVAPI void mav_TDMSingleScaleSet(int trk, float sc);



/* TDM library to use */

MAVAPI extern char *mav_opt_TDMLib;



/* Module initialise */

MAVAPI char *mav_TDMModuleID(void);
MAVAPI int mav_TDMModuleInit(void);

#ifdef __cplusplus
}
#endif
#endif
