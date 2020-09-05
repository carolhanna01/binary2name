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

#ifndef _MAV_SMS_INCLUDE
#define _MAV_SMS_INCLUDE

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

/* Callbacks for SMS's */

/* Add object to SMS */

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_objectAdd;
typedef int (*MAV_SMSCallbackObjectAddFn)(MAV_SMS *, MAV_object *);
MAVAPI void mav_SMSCallbackObjectAddSet(MAV_SMSClass *sc, MAV_SMSCallbackObjectAddFn fn);
MAVAPI int mav_SMSCallbackObjectAddExec(MAV_SMS *s, MAV_object *o);
MAVAPI int mav_SMSObjectAdd(MAV_SMS *s, MAV_object *o);

/* Intersect objects */

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_intersect;
typedef int (*MAV_SMSCallbackIntersectFn)(MAV_SMS *, MAV_window *, MAV_line *, MAV_objectIntersection *, MAV_object **);
MAVAPI void mav_SMSCallbackIntersectSet(MAV_SMSClass *sc, MAV_SMSCallbackIntersectFn fn);
MAVAPI int mav_SMSCallbackIntersectExec(MAV_SMS *s, MAV_window *w, MAV_line ln, MAV_objectIntersection *oi, MAV_object **o);

/* Reset pointer */

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_pointerReset;
typedef int (*MAV_SMSCallbackPointerResetFn)(MAV_SMS *);
MAVAPI void mav_SMSCallbackPointerResetSet(MAV_SMSClass *sc, MAV_SMSCallbackPointerResetFn fn);
MAVAPI int mav_SMSCallbackPointerResetExec(MAV_SMS *s);

/* Push pointer */

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_pointerPush;
typedef int (*MAV_SMSCallbackPointerPushFn)(MAV_SMS *);
MAVAPI void mav_SMSCallbackPointerPushSet(MAV_SMSClass *sc, MAV_SMSCallbackPointerPushFn fn);
MAVAPI int mav_SMSCallbackPointerPushExec(MAV_SMS *s);

/* Pop pointer */

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_pointerPop;
typedef int (*MAV_SMSCallbackPointerPopFn)(MAV_SMS *);
MAVAPI void mav_SMSCallbackPointerPopSet(MAV_SMSClass *sc, MAV_SMSCallbackPointerPopFn fn);
MAVAPI int mav_SMSCallbackPointerPopExec(MAV_SMS *s);

/* Next object */

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_objectNext;
typedef int (*MAV_SMSCallbackObjectNextFn)(MAV_SMS *, MAV_object **);
MAVAPI void mav_SMSCallbackObjectNextSet(MAV_SMSClass *sc, MAV_SMSCallbackObjectNextFn fn);
MAVAPI int mav_SMSCallbackObjectNextExec(MAV_SMS *s, MAV_object **o);

/* ExecuteFn */

typedef void (*MAV_SMSExecFnFn)(MAV_object *, MAV_drawInfo *, void *);

typedef struct {
  MAV_SMSExecFnFn fn;
  int nocalc;
  void *params;
} MAV_SMSExecFn;

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_execFn;
typedef int (*MAV_SMSCallbackExecFnFn)(MAV_SMS *, MAV_drawInfo *, MAV_SMSExecFn *);
MAVAPI void mav_SMSCallbackExecFnSet(MAV_SMSClass *sc, MAV_SMSCallbackExecFnFn fn);
MAVAPI int mav_SMSCallbackExecFnExec(MAV_SMS *s, MAV_drawInfo *di, MAV_SMSExecFn *fn);

/* Empty */

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_empty;
typedef int (*MAV_SMSCallbackEmptyFn)(MAV_SMS *, int *);
MAVAPI void mav_SMSCallbackEmptySet(MAV_SMSClass *sc, MAV_SMSCallbackEmptyFn fn);
MAVAPI int mav_SMSCallbackEmptyExec(MAV_SMS *s, int o);



/* Optional callbacks (if not defined are inefficiently simulated with reset and next) */

/*  Size */

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_size;
typedef int (*MAV_SMSCallbackSizeFn)(MAV_SMS *, int *);
MAVAPI void mav_SMSCallbackSizeSet(MAV_SMSClass *sc, MAV_SMSCallbackSizeFn fn);
MAVAPI int mav_SMSCallbackSizeExec(MAV_SMS *s, int *sz);

/* Contains object */

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_objectContains;
typedef int (*MAV_SMSCallbackObjectContainsFn)(MAV_SMS *, MAV_object *, int *);
MAVAPI void mav_SMSCallbackObjectContainsSet(MAV_SMSClass *sc, MAV_SMSCallbackObjectContainsFn fn);
MAVAPI int mav_SMSCallbackObjectContainsExec(MAV_SMS *s, MAV_object *o, int *cnt);




/* Types of SMS */

/* object list type of SMS */

typedef struct {
  MAV_list *list;
} MAV_objList;

MAVAPI extern MAV_SMSClass *mav_SMSClass_objList;
MAVAPI int mav_objListObjectAdd(MAV_SMS *s, MAV_object *o);
MAVAPI int mav_objListObjectRmv(MAV_SMS *s, MAV_object *o);
MAVAPI int mav_objListIntersect(MAV_SMS *s, MAV_window *w, MAV_line *ln, MAV_objectIntersection *oi, MAV_object **o);
MAVAPI int mav_objListPointerReset(MAV_SMS *s);
MAVAPI int mav_objListPointerPush(MAV_SMS *s);
MAVAPI int mav_objListPointerPop(MAV_SMS *s);
MAVAPI int mav_objListObjectNext(MAV_SMS *s, MAV_object **o);
MAVAPI int mav_objListExecFn(MAV_SMS *s, MAV_drawInfo *di, MAV_SMSExecFn *fn);
MAVAPI int mav_objListEmpty(MAV_SMS *s, int *o);
MAVAPI int mav_objListDelete(MAV_SMS *s, int *o);
MAVAPI int mav_objListSize(MAV_SMS *s, int *sz);

MAVAPI MAV_objList *mav_objListNew(void);
MAVAPI MAV_SMS *mav_SMSObjListNew(void);

/* HBB type of SMS */

typedef struct MAVLIB_HBBCluster {
  int status;
  MAV_BB box;
  MAV_vector pos;
  float size;
  float surface_area;
  int num_children;
  struct MAVLIB_HBBCluster *parent;
  struct MAVLIB_HBBChild *children;
  MAV_object *obj;
} MAV_HBBCluster;

typedef struct MAVLIB_HBBChild {
  struct MAVLIB_HBBCluster *child;
  struct MAVLIB_HBBChild *next;
} MAV_HBBChild;

typedef struct {
  MAV_HBBCluster *curr_cluster;
} MAV_HBBPointer;

typedef struct {
  MAV_HBBCluster *root;
  int size;
  MAV_HBBPointer *pointer;
} MAV_HBB;

MAVAPI extern MAV_SMSClass *mav_SMSClass_HBB;
MAVAPI int mav_HBBObjectAdd(MAV_SMS *s, MAV_object *o);
MAVAPI int mav_HBBObjectRmv(MAV_SMS *s, MAV_object *o);
MAVAPI int mav_HBBIntersect(MAV_SMS *s, MAV_window *w, MAV_line *ln, MAV_objectIntersection *oi, MAV_object **o);
MAVAPI int mav_HBBPointerReset(MAV_SMS *s);
MAVAPI int mav_HBBPointerPush(MAV_SMS *s);
MAVAPI int mav_HBBPointerPop(MAV_SMS *s);
MAVAPI int mav_HBBObjectNext(MAV_SMS *s, MAV_object **o);
MAVAPI int mav_HBBExecFn(MAV_SMS *s, MAV_drawInfo *di, MAV_SMSExecFn *fn);
MAVAPI int mav_HBBEmpty(MAV_SMS *s, int *o);
MAVAPI int mav_HBBDelete(MAV_SMS *s, int *o);
MAVAPI int mav_HBBSize(MAV_SMS *s, int *sz);

MAVAPI MAV_HBB *mav_HBBNew(void);
MAVAPI MAV_SMS *mav_SMSHBBNew(void);
MAVAPI void mav_HBBConstructFromSMS(MAV_SMS *target, MAV_SMS *from);



/* Supporting routines */

MAVAPI int mav_SMSDisplay(MAV_window *w, MAV_SMS *s);
MAVAPI int mav_SMSDisplayUsingDrawInfo(MAV_window *w, MAV_SMS *s, MAV_drawInfo di);
MAVAPI int mav_SMSDisplayUnCulled(MAV_window *w, MAV_SMS *s);
MAVAPI int mav_SMSIntersectLine(MAV_window *w, MAV_SMS *sms, MAV_line ln, MAV_objectIntersection *oi, MAV_object **o);
MAVAPI int mav_SMSIntersectLineAll(MAV_window *w, MAV_line ln, MAV_objectIntersection *oi, MAV_object **o);
MAVAPI int mav_SMSIntersectBB(MAV_window *w, MAV_SMS *sms, MAV_BB bb, MAV_SMS *objs);
MAVAPI int mav_SMSIntersectBBAll(MAV_window *w, MAV_BB bb, MAV_SMS *objs);

MAVAPI void mav_viewParamsAnimateToSMS(MAV_window *w, MAV_viewParams *vp, MAV_SMS *sms, float dist, float len, int unit);



/* The hooks into SMS display */

MAVAPI extern MAV_SMSExecFnFn mav_SMS_displayFn;
MAVAPI void mav_SMSDisplayFn(MAV_object *, MAV_drawInfo *, void *);



/* Module initialisation */

MAVAPI char *mav_SMSModuleID(void);
MAVAPI int mav_SMSModuleInit(void);

#ifdef __cplusplus
}
#endif
#endif
