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

#ifndef _MAV_WINDOWS_INCLUDE
#define _MAV_WINDOWS_INCLUDE

#ifdef MAVAPI
#undef MAVAPI
#endif
#if defined(WIN32) && !defined(__CYGWIN__) 
#ifdef LIBMAVERIK_EXPORTS
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


/* Window handles, classes and objects */

MAVAPI extern MAV_window *mav_win_left;
MAVAPI extern MAV_window *mav_win_right;
MAVAPI void mav_eyeLeft(MAV_window *w);
MAVAPI void mav_eyeRight(MAV_window *w);
MAVAPI extern MAV_window *mav_win_mono;
MAVAPI extern MAV_class *mav_class_world;
MAVAPI extern MAV_class *mav_class_any;
MAVAPI extern MAV_class *mav_class_none;
MAVAPI extern MAV_object *mav_object_world;
MAVAPI extern MAV_object *mav_object_any;
MAVAPI extern MAV_object *mav_object_none;
typedef void (*MAV_ctrlF)(MAV_window *);
MAVAPI extern MAV_ctrlF mav_ctrlF[];
MAVAPI extern char *mav_ctrlF_desc[];

/* Options */

MAVAPI extern int mav_opt_noWins;
MAVAPI extern int mav_opt_stereo;
MAVAPI extern int mav_opt_fullscreen;
MAVAPI extern int mav_opt_x;
MAVAPI extern int mav_opt_y;
MAVAPI extern int mav_opt_width;
MAVAPI extern int mav_opt_height;
MAVAPI extern char *mav_opt_name;
MAVAPI extern char *mav_opt_disp;
MAVAPI extern int mav_opt_right_x;
MAVAPI extern int mav_opt_right_y;
MAVAPI extern int mav_opt_right_width;
MAVAPI extern int mav_opt_right_height;
MAVAPI extern char *mav_opt_right_name;
MAVAPI extern char *mav_opt_right_disp;
MAVAPI extern int mav_opt_restrictMouse;

/* Stereo modes */

#define MAV_STEREO_TWO_WINS 1
#define MAV_STEREO_QUAD_BUFFERS 2
#define MAV_STEREO_QUAD_BUFFERS_SEPARATE_Z 3
MAVAPI extern MAV_viewModifierParams mav_stp_default;

/* Screen resolution */

MAVAPI extern int mav_xres;
MAVAPI extern int mav_yres;

/* Mouse related info */

MAVAPI extern int mav_mouse_x;
MAVAPI extern int mav_mouse_y;
MAVAPI extern int mav_mouse_root_x;
MAVAPI extern int mav_mouse_root_y;
MAVAPI extern int mav_mouse_button[];
MAVAPI extern MAV_window *mav_win_mouse;
MAVAPI extern MAV_vector mav_mouse_pos;
MAVAPI extern MAV_vector mav_mouse_dir;

/* Non-ASCII keys */

#define MAV_KEY_F1 300
#define MAV_KEY_F2 301
#define MAV_KEY_F3 302
#define MAV_KEY_F4 303
#define MAV_KEY_F5 304
#define MAV_KEY_F6 305
#define MAV_KEY_F7 306
#define MAV_KEY_F8 307
#define MAV_KEY_F9 308
#define MAV_KEY_F10 309
#define MAV_KEY_F11 310
#define MAV_KEY_F12 311
#define MAV_KEY_UP 312
#define MAV_KEY_DOWN 313
#define MAV_KEY_LEFT 314
#define MAV_KEY_RIGHT 315
#define MAV_KEY_PAGE_UP 316
#define MAV_KEY_PAGE_DOWN 317
#define MAV_KEY_SHIFT_L 318
#define MAV_KEY_SHIFT_R 319
#define MAV_KEY_ALT_L 320
#define MAV_KEY_ALT_R 321
#define MAV_KEY_META_L 322
#define MAV_KEY_META_R 323
#define MAV_KEY_HOME 324
#define MAV_KEY_END 325
#define MAV_KEY_INSERT 326
#define MAV_KEY_CTRL_L 327
#define MAV_KEY_CTRL_R 328
#define MAV_KEY_CAPS_LOCK 329

/* Modifiers */

#define MAV_MODIFIER_MAX   3
#define MAV_MODIFIER_SHIFT 0
#define MAV_MODIFIER_CTRL  1
#define MAV_MODIFIER_ALT   2

/* Window event based callbacks */

/* Keyboard event */

typedef struct {
  MAV_window *win;
  int x;
  int y;
  int root_x;
  int root_y;
  MAV_line line;
  int intersects;
  MAV_object *obj;
  MAV_objectIntersection objint;
  int key;
  int modifiers[MAV_MODIFIER_MAX];
  int movement;  
} MAV_keyboardEvent;

MAVAPI extern MAV_callback *mav_callback_keyboard;
typedef int (*MAV_callbackKeyboardFn)(MAV_object *, MAV_keyboardEvent *);
MAVAPI void mav_callbackKeyboardSet(MAV_window *w, MAV_class *c, MAV_callbackKeyboardFn fn);
MAVAPI int  mav_callbackKeyboardExec(MAV_window *w, MAV_object *o, MAV_keyboardEvent *ke);

/* Keyboard events reserved for system, e.g navigation */

MAVAPI extern MAV_callback *mav_callback_sysKeyboard;
MAVAPI void mav_callbackSysKeyboardSet(MAV_window *w, MAV_class *c, MAV_callbackKeyboardFn fn);
MAVAPI int  mav_callbackSysKeyboardExec(MAV_window *w, MAV_object *o, MAV_keyboardEvent *ke);

/* Mouse event */

typedef struct {
  MAV_window *win;
  int x;
  int y;
  int root_x;
  int root_y;
  MAV_line line;
  int intersects;
  MAV_object *obj;
  MAV_objectIntersection objint;
  int button;
  int modifiers[MAV_MODIFIER_MAX];
  int movement;
} MAV_mouseEvent;

MAVAPI extern MAV_callback *mav_callback_leftButton;
MAVAPI extern MAV_callback *mav_callback_middleButton;
MAVAPI extern MAV_callback *mav_callback_rightButton;
MAVAPI extern MAV_callback *mav_callback_wheelUpButton;
MAVAPI extern MAV_callback *mav_callback_wheelDownButton;
MAVAPI extern MAV_callback *mav_callback_anyButton;
typedef int (*MAV_callbackMouseFn)(MAV_object *, MAV_mouseEvent *);
MAVAPI void mav_callbackMouseSet(int but, MAV_window *w, MAV_class *c, MAV_callbackMouseFn fn);
MAVAPI int  mav_callbackMouseExec(int but, MAV_window *w, MAV_object *o, MAV_mouseEvent *me);

#define MAV_LEFT_BUTTON 0
#define MAV_MIDDLE_BUTTON 1
#define MAV_RIGHT_BUTTON 2
#define MAV_WHEELUP_BUTTON 3
#define MAV_WHEELDOWN_BUTTON 4
#define MAV_ANY_BUTTON 20
#define MAV_PRESSED 0
#define MAV_RELEASED 1

/* Mouse events reserved for system, e.g navigation */

MAVAPI extern MAV_callback *mav_callback_sysMouse;
MAVAPI void mav_callbackSysMouseSet(MAV_window *w, MAV_class *c, MAV_callbackMouseFn fn);
MAVAPI int  mav_callbackSysMouseExec(MAV_window *w, MAV_object *o, MAV_mouseEvent *me);

/* Resize event */

typedef struct {
  MAV_window *win;
  int width;
  int height;
} MAV_resizeEvent;

MAVAPI extern MAV_callback *mav_callback_resize;
typedef int (*MAV_callbackResizeFn) (MAV_object *, MAV_resizeEvent *);
MAVAPI void mav_callbackResizeSet(MAV_window *w, MAV_callbackResizeFn fn);
MAVAPI int  mav_callbackResizeExec(MAV_window *w, MAV_resizeEvent *re);
MAVAPI int  mav_resizeDefault(MAV_object *o, MAV_resizeEvent *re);

/* Map/Unmap event */

typedef struct {
  MAV_window *win;
  int map;
} MAV_mapEvent;

#define MAV_MAP 0
#define MAV_UNMAP 1

MAVAPI extern MAV_callback *mav_callback_map;
typedef int (*MAV_callbackMapFn) (MAV_object *, MAV_mapEvent *);
MAVAPI void mav_callbackMapSet(MAV_window *w, MAV_callbackMapFn fn);
MAVAPI int  mav_callbackMapExec(MAV_window *w, MAV_mapEvent *me);
MAVAPI int  mav_mapDefault(MAV_object *o, MAV_mapEvent *me);

/* Crossing (Enter/Leave) event */

typedef struct {
  MAV_window *win;
  int dir;
} MAV_crossingEvent;

#define MAV_ENTER 0
#define MAV_LEAVE 1

MAVAPI extern MAV_callback *mav_callback_crossing;
typedef int (*MAV_callbackCrossingFn) (MAV_object *, MAV_crossingEvent *);
MAVAPI void mav_callbackCrossingSet(MAV_window *w, MAV_callbackCrossingFn fn);
MAVAPI int  mav_callbackCrossingExec(MAV_window *w, MAV_crossingEvent *ce);

/* Expose event */

typedef struct {
  MAV_window *win;
} MAV_exposeEvent;

MAVAPI extern MAV_callback *mav_callback_expose;
typedef int (*MAV_callbackExposeFn) (MAV_object *, MAV_exposeEvent *);
MAVAPI void mav_callbackExposeSet(MAV_window *w, MAV_callbackExposeFn fn);
MAVAPI int  mav_callbackExposeExec(MAV_window *w, MAV_exposeEvent *ee);
MAVAPI int  mav_exposeDefault(MAV_object *o, MAV_exposeEvent *ee);


/* Supporting routines */

MAVAPI MAV_line mav_lineFrom2DPoint(MAV_window *w, int x, int y);
MAVAPI void mav_mouseGet(MAV_window *w, int *x, int *y, int *rx, int *ry, int *buts);
MAVAPI void mav_mouseSet(MAV_window *w, int x, int y);
MAVAPI int  mav_keyboardGet(int key);
MAVAPI extern int mav_drawingMouse;
MAVAPI void mav_mouseDraw(void *ignored);
MAVAPI void mav_mouseSurfaceParamsSet(MAV_surfaceParams *sp);



/* Module initialise */

MAVAPI int mav_windowsModuleInit(void);
MAVAPI char *mav_windowsModuleID(void);

#ifdef __cplusplus
}
#endif
#endif
