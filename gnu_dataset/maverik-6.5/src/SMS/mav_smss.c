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


#include "mavlib_sms.h"
#include <stdio.h>
#include <math.h>



/* Routines to initialise this module */

char *mav_SMSModuleID(void)
{
  return "SMS's";
}

int mav_SMSModuleInit(void)
{
  /* add this module */
  mav_moduleNew(mav_SMSModuleID);

  /* define SMS callbacks */
  mav_SMSCallback_objectAdd= mav_SMSCallbackNew();
  mav_SMSCallback_intersect= mav_SMSCallbackNew();
  mav_SMSCallback_pointerReset= mav_SMSCallbackNew();
  mav_SMSCallback_pointerPush= mav_SMSCallbackNew();
  mav_SMSCallback_pointerPop= mav_SMSCallbackNew();
  mav_SMSCallback_objectNext= mav_SMSCallbackNew();
  mav_SMSCallback_execFn= mav_SMSCallbackNew();
  mav_SMSCallback_empty= mav_SMSCallbackNew();
  mav_SMSCallback_size= mav_SMSCallbackNew();
  mav_SMSCallback_objectContains= mav_SMSCallbackNew();

  /* define the objList type of SMS */
  mav_SMSClass_objList= mav_SMSClassNew();
  mav_SMSCallbackObjectAddSet(mav_SMSClass_objList, mav_objListObjectAdd);
  mav_SMSCallbackObjectRmvSet(mav_SMSClass_objList, mav_objListObjectRmv);
  mav_SMSCallbackIntersectSet(mav_SMSClass_objList, mav_objListIntersect);
  mav_SMSCallbackPointerResetSet(mav_SMSClass_objList, mav_objListPointerReset);
  mav_SMSCallbackPointerPushSet(mav_SMSClass_objList, mav_objListPointerPush);
  mav_SMSCallbackPointerPopSet(mav_SMSClass_objList, mav_objListPointerPop);
  mav_SMSCallbackObjectNextSet(mav_SMSClass_objList, mav_objListObjectNext);
  mav_SMSCallbackExecFnSet(mav_SMSClass_objList, mav_objListExecFn);
  mav_SMSCallbackEmptySet(mav_SMSClass_objList, mav_objListEmpty);
  mav_SMSCallbackDeleteSet(mav_SMSClass_objList, mav_objListDelete);
  mav_SMSCallbackSizeSet(mav_SMSClass_objList, mav_objListSize);
  mav_SMSCallbackObjectContainsSet(mav_SMSClass_objList, NULL);

  /* define the HBB type of SMS */
  mav_SMSClass_HBB= mav_SMSClassNew();
  mav_SMSCallbackObjectAddSet(mav_SMSClass_HBB, mav_HBBObjectAdd);
  mav_SMSCallbackObjectRmvSet(mav_SMSClass_HBB, mav_HBBObjectRmv);
  mav_SMSCallbackIntersectSet(mav_SMSClass_HBB, mav_HBBIntersect);
  mav_SMSCallbackPointerResetSet(mav_SMSClass_HBB, mav_HBBPointerReset);
  mav_SMSCallbackPointerPushSet(mav_SMSClass_HBB, mav_HBBPointerPush);
  mav_SMSCallbackPointerPopSet(mav_SMSClass_HBB, mav_HBBPointerPop);
  mav_SMSCallbackObjectNextSet(mav_SMSClass_HBB, mav_HBBObjectNext);
  mav_SMSCallbackExecFnSet(mav_SMSClass_HBB, mav_HBBExecFn);
  mav_SMSCallbackEmptySet(mav_SMSClass_HBB, mav_HBBEmpty);
  mav_SMSCallbackDeleteSet(mav_SMSClass_HBB, mav_HBBDelete);
  mav_SMSCallbackSizeSet(mav_SMSClass_HBB, mav_HBBSize);
  mav_SMSCallbackObjectContainsSet(mav_SMSClass_HBB, NULL);

  return 1;
}



/* Wrapper routine to executeFn callback in order to display an SMS */

MAV_SMSExecFnFn mav_SMS_displayFn= mav_SMSDisplayFn;

void mav_SMSDisplayFn(MAV_object *obj, MAV_drawInfo *di, void *params)
{
  mav_callbackDrawExec(mav_win_current, obj, di);
}

int mav_SMSDisplayUsingDrawInfo(MAV_window *w, MAV_SMS *sms, MAV_drawInfo di)
{
  MAV_window *win, *orig_win= mav_win_current;
  MAV_SMSExecFn fn;
  int rv=0;

  /* make an SMSExecFn to call the function held in the variable mav_SMS_displayFn */
  fn.fn= mav_SMS_displayFn;
  fn.nocalc= 1;  /* If unsure call fn anyway */
  fn.params= NULL;

  if (w==mav_win_all) 
  {
    /* trap for mav_win_all */
    mav_listPointerReset(mav_win_list);
    while (mav_listItemNext(mav_win_list, (void **) &win)) rv=mav_SMSDisplayUsingDrawInfo(win, sms, di);
  }
  else
  {
    /* set to correct window and call executeFn callback */
    if (w!=orig_win) mav_windowSet(w);
    rv= mav_SMSCallbackExecFnExec(sms, &di, &fn);
    if (w!=orig_win) mav_windowSet(orig_win);
  }
  
  return rv;
}

int mav_SMSDisplay(MAV_window *w, MAV_SMS *sms)
{
  MAV_window *win;
  MAV_drawInfo di;
  int rv=0;

  if (w==mav_win_all) 
  {
    /* trap for mav_win_all */
    mav_listPointerReset(mav_win_list);
    while (mav_listItemNext(mav_win_list, (void **) &win)) rv=mav_SMSDisplay(win, sms);
  }
  else
  {
    /* call above function with the appropriate draw information */
    di.vp= *(w->vp);
    di.cp= mav_clipPlanesGet(w, -1.0, 1.0, -1.0, 1.0, w->ncp/w->fcp, 1.0);
    rv= mav_SMSDisplayUsingDrawInfo(w, sms, di);
  }
  
  return rv;
}



/* Routine to step through an SMS displaying the contents. Does not perform culling */

int mav_SMSDisplayUnCulled(MAV_window *w, MAV_SMS *sms)
{
  MAV_window *win, *orig_win= mav_win_current;
  MAV_object *obj;
  int rv=0;

  if (w==mav_win_all) 
  {
    mav_listPointerReset(mav_win_list);
    while (mav_listItemNext(mav_win_list, (void **) &win)) mav_SMSDisplayUnCulled(win, sms);
  }
  else
  {
    if (w!=orig_win) mav_windowSet(w);
    mav_SMSCallbackPointerResetExec(sms);
    while (mav_SMSCallbackObjectNextExec(sms, &obj)) (*mav_SMS_displayFn)(obj, NULL, NULL);
    if (w!=orig_win) mav_windowSet(orig_win);
  }
  
  return rv;
}



/* Wrapper routine to execute the intersect callback on an SMS */

int mav_SMSIntersectLine(MAV_window *w, MAV_SMS *sms, MAV_line ln, MAV_objectIntersection *objint, MAV_object **obj)
{
  return mav_SMSCallbackIntersectExec(sms, w, ln, objint, obj);
}



/* Routine to call intersect callback on all SMSs which are selectable for a given window */

int mav_SMSIntersectLineAll(MAV_window *w, MAV_line ln, MAV_objectIntersection *objint, MAV_object **obj)
{
  MAV_SMS *sms;
  MAV_object *obj2;
  MAV_objectIntersection objint2;
  int rv= MAV_FALSE;
  
  /* initalise the return values */
  *obj= NULL;
  objint->pt1= MAV_INFINITY;

  /* step through list of SMS's */
  mav_listPointerReset(mav_sms_list);

  while (mav_listItemNext(mav_sms_list, (void **) &sms)) {
    
    /* only consider those which are selectable in a given window */
    if ((sms->selectable[mav_win_all->id]==MAV_TRUE) || (sms->selectable[w->id]==MAV_TRUE && sms->selectable[mav_win_all->id]!=MAV_FALSE)) {

      /* execute intersection callback noting closest intersection  */
      if (mav_SMSCallbackIntersectExec(sms, w, ln, &objint2, &obj2)) {
	rv= MAV_TRUE;
	if (objint2.pt1<objint->pt1) {
	  *objint=objint2;
	  *obj=obj2;
	}
      }
    }
  }
  
  return rv;
}



/* Routine to determine if a BB intersects SMS sms, objects which do 
   are added to SMS objs. */

int mavlib_SMSBBFlag;

void mavlib_SMSBBFn(MAV_object *obj, MAV_drawInfo *di, void *params)
{
  MAV_SMS *sms= (MAV_SMS *) params;

  mav_SMSObjectAdd(sms, obj);

  mavlib_SMSBBFlag= MAV_TRUE;
}

int mav_SMSIntersectBB(MAV_window *w, MAV_SMS *sms, MAV_BB bb, MAV_SMS *objs)
{
  MAV_SMSExecFn fn;
  MAV_drawInfo di;

  /* Make a set of clip planes out of the BB */
  di.cp= mav_clipPlanesGetFromBB(bb);
  /* Any old view params will do */
  di.vp= *(mav_win_current->vp);

  /* Cull SMS to this draw info calling a routine which adds them to SMS objs */
  mavlib_SMSBBFlag=MAV_FALSE;
  fn.fn= mavlib_SMSBBFn;
  fn.nocalc= 0;  /* If unsure dont call fn */
  fn.params= objs;

  if (sms!=objs) mav_SMSCallbackExecFnExec(sms, &di, &fn);

  return mavlib_SMSBBFlag;
}

int mav_SMSIntersectBBAll(MAV_window *w, MAV_BB bb, MAV_SMS *objs)
{
  MAV_SMS *sms;
  int rv= MAV_FALSE;

  /* step through list of SMS's */
  mav_listPointerReset(mav_sms_list);

  while (mav_listItemNext(mav_sms_list, (void **) &sms)) {
    
    /* only consider those which are selectable in a given window */
    if ((sms->selectable[mav_win_all->id]==MAV_TRUE) || (sms->selectable[w->id]==MAV_TRUE && sms->selectable[mav_win_all->id]!=MAV_FALSE)) {      
      rv|= mav_SMSIntersectBB(w, sms, bb, objs);
    }
  }

  return rv;
}



/* Routine to animate vp to an SMS */

void mav_viewParamsAnimateToSMS(MAV_window *w, MAV_viewParams *vp, MAV_SMS *sms, float dist, float tim, int style)
{
  MAV_BB bb, tmp;
  MAV_vector c;
  float d, r;
  MAV_viewParams init, targ;
  MAV_object *o;
  int cancalc=0;

  if (!vp) vp= w->vp;

  if (w->orthogonal) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: can not animate vp to object with an orthogonal view\n");    
  }
  else
  {
    /* Calculate composite BB of SMS */
    mav_BBCompInit(&bb);
    mav_SMSCallbackPointerResetExec(sms);
    while (mav_SMSCallbackObjectNextExec(sms, &o)) {
      if (mav_callbackBBExec(w, o, &tmp)) {
	mav_BBCompBB(tmp, &bb);
	cancalc=1;
      }
    }

    if (cancalc)
    {
      /* Calculate smallest fov */
      float fov, vfov, hfov;

      vfov= w->fov;
      hfov= MAV_RAD2DEG(tan(atan(MAV_DEG2RAD(vfov/2.0))*w->aspect)*2.0);
      
      if (vfov>hfov)
      {
	fov= hfov;
      }
      else
      {
	fov= vfov;
      }

      /* Calculate a bounding sphere */
      c= mav_vectorScalar(mav_vectorAdd(bb.min, bb.max), 0.5);
      r= mav_vectorMag(mav_vectorSub(bb.max, c));

      /* Calculate distance from center of sphere so that view completely encompasses it */
      d= (r/(atan(MAV_DEG2RAD(fov/2.0))))*dist;
      
      /* Initial view params */
      init= *(w->vp);
      
      /* Target view params */
      targ= init;
      targ.eye= mav_vectorAdd(c, mav_vectorScalar(w->vp->view, -d));

      /* Animate view params */
      mav_viewParamsAnimate(vp, init, targ, tim, style);
    }
    else
    {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: could not find object's BB to animate vp to object\n");    
    }
  }
}
