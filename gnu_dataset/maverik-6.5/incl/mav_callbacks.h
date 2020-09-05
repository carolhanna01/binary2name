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

#ifndef _MAV_CALLBACKS_INCLUDE
#define _MAV_CALLBACKS_INCLUDE

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

/* Common process callbacks */

/* Draw  */

typedef struct {
  MAV_clipPlanes cp;
  MAV_viewParams vp;
  void *userdef;
} MAV_drawInfo;

MAVAPI extern MAV_callback *mav_callback_draw;
typedef int (*MAV_callbackDrawFn)(MAV_object *, MAV_drawInfo *);
MAVAPI void mav_callbackDrawSet(MAV_window *w, MAV_class *c, MAV_callbackDrawFn fn);
MAVAPI int  mav_callbackDrawExec(MAV_window *w, MAV_object *o, MAV_drawInfo *di);

/* Axis aligned bounding box */

typedef struct {
  MAV_vector min;
  MAV_vector max;
} MAV_BB;

MAVAPI extern MAV_callback *mav_callback_BB;
typedef int (*MAV_callbackBBFn)(MAV_object *, MAV_BB *);
MAVAPI void mav_callbackBBSet(MAV_window *w, MAV_class *c, MAV_callbackBBFn fn);
MAVAPI int  mav_callbackBBExec(MAV_window *w, MAV_object *o, MAV_BB *bb);

/* Intersect */

typedef struct {
  MAV_vector pt;
  MAV_vector dir;
} MAV_line;

typedef struct {
  float pt1;
  float pt2;
  MAV_vector intpt;
  MAV_vector surnorm;
} MAV_objectIntersection;

MAVAPI extern MAV_callback *mav_callback_intersect;
typedef int (*MAV_callbackIntersectFn)(MAV_object *, MAV_line *, MAV_objectIntersection *);
MAVAPI void mav_callbackIntersectSet(MAV_window *w, MAV_class *c, MAV_callbackIntersectFn fn);
MAVAPI int  mav_callbackIntersectExec(MAV_window *w, MAV_object *o, MAV_line ln, MAV_objectIntersection *oi);

/* ID */

MAVAPI extern MAV_callback *mav_callback_id;
typedef int (*MAV_callbackIDFn)(MAV_object *, char **);
MAVAPI void mav_callbackIDSet(MAV_window *w, MAV_class *c, MAV_callbackIDFn fn);
MAVAPI int  mav_callbackIDExec(MAV_window *w, MAV_object *o, char **id);

/* Dump */

MAVAPI extern MAV_callback *mav_callback_dump;
typedef int (*MAV_callbackDumpFn)(MAV_object *);
MAVAPI void mav_callbackDumpSet(MAV_window *w, MAV_class *c, MAV_callbackDumpFn fn);
MAVAPI int  mav_callbackDumpExec(MAV_window *w, MAV_object *o);

/* Get userdef */ 

MAVAPI extern MAV_callback *mav_callback_getUserdef;
typedef int (*MAV_callbackGetUserdefFn)(MAV_object *, void ***);
MAVAPI void mav_callbackGetUserdefSet(MAV_window *w, MAV_class *c, MAV_callbackGetUserdefFn fn);
MAVAPI int  mav_callbackGetUserdefExec(MAV_window *w, MAV_object *o, void ***ud);

/* Get matrix */

MAVAPI extern MAV_callback *mav_callback_getMatrix;
typedef int (*MAV_callbackGetMatrixFn)(MAV_object *, MAV_matrix **);
MAVAPI void mav_callbackGetMatrixSet(MAV_window *w, MAV_class *c, MAV_callbackGetMatrixFn fn);
MAVAPI int  mav_callbackGetMatrixExec(MAV_window *w, MAV_object *o, MAV_matrix **m);

/* Get surface colouring parameter */

MAVAPI extern MAV_callback *mav_callback_getSurfaceParams;
typedef int (*MAV_callbackGetSurfaceParamsFn)(MAV_object *, MAV_surfaceParams ***);
MAVAPI void mav_callbackGetSurfaceParamsSet(MAV_window *w, MAV_class *c, MAV_callbackGetSurfaceParamsFn fn);
MAVAPI int  mav_callbackGetSurfaceParamsExec(MAV_window *w, MAV_object *o, MAV_surfaceParams ***sp);



/* Supporting routines */

MAVAPI MAV_clipPlanes mav_clipPlanesGet(MAV_window *w, float xmin, float xmax, float ymin, float ymax, float zmin, float zmax);
MAVAPI MAV_clipPlanes mav_clipPlanesGetFromPixels(MAV_window *w, int xmin, int xmax, int ymin, int ymax, float zmin, float zmax);
MAVAPI MAV_clipPlanes mav_clipPlanesGetFromBB(MAV_BB bb);
MAVAPI void mav_clipPlanePrint(char *s, MAV_clipPlane cp);
MAVAPI void mav_clipPlanesPrint(char *s, MAV_clipPlanes cp);

MAVAPI void mav_viewParamsAnimateToObject(MAV_window *w, MAV_viewParams *vp, MAV_object *o, float dist, float len, int unit);

MAVAPI int mav_BBCull(MAV_BB bb);
MAVAPI int mav_BBCullToClipPlanes(MAV_BB bb, MAV_clipPlanes cp);
MAVAPI int mav_BBGetCorner(MAV_vector v);
MAVAPI int mav_BBIntersectsLine(MAV_BB bb, MAV_line ln, MAV_objectIntersection *oi);
MAVAPI int mav_BBIntersectsBB(MAV_BB bb1, MAV_BB bb2);
MAVAPI int mav_BBInsideBB(MAV_BB bb1, MAV_BB bb2);
MAVAPI int mav_BBIntersectsClipPlanes(MAV_BB bb, int *clist, MAV_clipPlanes *cp);
MAVAPI void mav_BBAlign(MAV_BB in, MAV_matrix m, MAV_BB *out);
MAVAPI void mav_BBPrint(char *s, MAV_BB bb);
MAVAPI void mav_BBDisplay(MAV_window *w, MAV_BB bb);
MAVAPI void mav_BBDisplayWithColour(MAV_window *w, MAV_BB bb, int col);
MAVAPI void mav_BBDisplayWithSurfaceParams(MAV_window *w, MAV_BB bb, MAV_surfaceParams *sp);
MAVAPI void mav_BBCompInit(MAV_BB *bb);
MAVAPI void mav_BBCompBB(MAV_BB in, MAV_BB *out);
MAVAPI void mav_BBCompPt(MAV_vector v, MAV_BB *bb);

#define mav_BBDraw(A,B) mav_BBDisplay(A,B)
#define mav_BBDrawWithColour(A,B,C) mav_BBDisplayWithColour(A,B,C)
#define mav_BBDrawWithSurfaceParams(A,B,C) mav_BBDisplayWithSurfaceParams(A,B,C)
#define mav_BBIntersect(A,B,C) mav_BBIntersectsLine(A,B,C)

MAVAPI MAV_line mav_lineTransFrame(MAV_line in, MAV_matrix m);
MAVAPI MAV_drawInfo mav_drawInfoTransFrame(MAV_drawInfo in, MAV_matrix mat);
MAVAPI int  mav_lineInfPlaneIntersection(MAV_vector pt, MAV_vector norm, MAV_line ln, MAV_objectIntersection *oi);
MAVAPI int  mav_lineAxisPlaneIntersection(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax, 
					  MAV_vector pt, MAV_vector norm, MAV_line ln, MAV_objectIntersection *oi);
MAVAPI int  mav_objectIntersectionsSort(int nhits, MAV_objectIntersection *hits, float scale, MAV_objectIntersection *res);
MAVAPI void mav_objectIntersectionPrint(char *s, MAV_objectIntersection oi);
MAVAPI void mav_linePrint(char *s, MAV_line ln);

typedef struct {
  MAV_window *win;
  MAV_object *obj;
  MAV_callbackDrawFn fn;
  MAV_drawInfo di;
  MAV_drawInfo *dip;
  MAV_matrix mat;
  MAV_BB bb;
  float dist2;
} MAV_transObjData;

typedef struct {
  MAV_window *win;
  char *s;
  int col;
  int font;
  float x;
  float y;
  int justify;
} MAV_transTextData;

MAVAPI int  mav_objectIsTransparent(MAV_window *w, MAV_object *o);
MAVAPI void mav_transparentObjectsManage(MAV_window *w, MAV_object *o, MAV_drawInfo *di);
MAVAPI void mav_transparentObjectsRender(void *ignored);
MAVAPI void mav_transparentTextManage(MAV_window *w, char *s, int col, int font, float x, float y);
MAVAPI void mav_transparentTextRender(void *ignored);
MAVAPI extern MAV_list *mav_transObjList;
MAVAPI extern MAV_list *mav_transTextList;

typedef struct {
  MAV_window *win;
  MAV_object *obj;
  MAV_callbackDrawFn fn;
  MAV_drawInfo di;
  MAV_drawInfo *dip;
  MAV_matrix mat;
} MAV_texturedObjData;

MAVAPI int  mav_objectIsTextured(MAV_window *w, MAV_object *o);
MAVAPI void mav_texturedObjectsManage(MAV_window *w, MAV_object *o, MAV_drawInfo *di);
MAVAPI void mav_texturedObjectsRender(void *ignored);
MAVAPI extern MAV_list **mav_textureObjList;



/* Strings */

MAVAPI int  mav_stringLength(MAV_window *w, char *s, int font);
MAVAPI void mav_stringDisplay(MAV_window *w, char *s, int col, int font, float x, float y);
MAVAPI void mav_stringDisplayLeft(MAV_window *w, char *s, int col, int font, float x, float y);
MAVAPI void mav_stringDisplayCentre(MAV_window *w, char *s, int col, int font, float x, float y);
MAVAPI void mav_stringDisplayRight(MAV_window *w, char *s, int col, int font, float x, float y);

#define mav_stringDisplayCenter mav_stringDisplayCentre 



/* Options */

MAVAPI extern int mav_opt_trans;
MAVAPI extern int mav_opt_delayTexture;



/* Module initialise */

MAVAPI int mav_callbacksModuleInit(void);
MAVAPI char *mav_callbacksModuleID(void);

#ifdef __cplusplus
}
#endif
#endif
