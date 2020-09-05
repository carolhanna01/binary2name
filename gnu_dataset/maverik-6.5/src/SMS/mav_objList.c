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
#include <stdlib.h>
#include <stdio.h>

MAV_SMSClass *mav_SMSClass_objList;



/* Add object */

int mav_objListObjectAdd(MAV_SMS *sms, MAV_object *obj)
{
  MAV_objList *objList= (MAV_objList *) mav_SMSDataGet(sms);

  /* add a mav_object to a list */
  mav_listItemAdd(objList->list, (void *) obj);

  /* update object tables */
  if (mav_opt_objectTables) mav_objectTablesSMSAdd(obj, sms);

  return 1;
}



/* Remove object */

int mav_objListObjectRmv(MAV_SMS *sms, MAV_object *obj)
{
  MAV_objList *objList= (MAV_objList *) mav_SMSDataGet(sms);
  int rv=0;

  if (mav_listItemContains(objList->list, (void *) obj))
  {
    /* remove a mav_object to a list */
    mav_listItemRmv(objList->list, (void *) obj);

    /* update object tables */
    if (mav_opt_objectTables) mav_objectTablesSMSRmv(obj, sms);

    rv=1;
  }
  else
  {
    rv=0;
  }

  return rv;
}



/* Intersect */

int mav_objListIntersect(MAV_SMS *sms, MAV_window *w, MAV_line *ln, MAV_objectIntersection *objint, MAV_object **obj)
{
  MAV_objList *objList= (MAV_objList *) mav_SMSDataGet(sms);
  MAV_object *o;
  MAV_objectIntersection objint2;
  int rv= MAV_FALSE;

/* initialise return values */
  *obj=NULL;
  objint->pt1= MAV_INFINITY;


/* step through list calling intersect callback and noting the closest */
  mav_listPointerReset(objList->list);

  while (mav_listItemNext(objList->list, (void **) &o)) {    
    if (mav_callbackIntersectExec(w, o, *ln, &objint2)) {
      rv= MAV_TRUE;
      if (objint2.pt1<objint->pt1) {
	*objint=objint2;
	*obj=o;
      }
    }
  }
  
  return rv;
}



/* Reset pointer */

int mav_objListPointerReset(MAV_SMS *sms)
{
  MAV_objList *objList= (MAV_objList *) mav_SMSDataGet(sms);

  mav_listPointerReset(objList->list);
  
  return 1;
}



/* Push pointer */

int mav_objListPointerPush(MAV_SMS *sms)
{
  MAV_objList *objList= (MAV_objList *) mav_SMSDataGet(sms);

  mav_listPointerPush(objList->list);

  return 1;
}



/* Pop pointer */

int mav_objListPointerPop(MAV_SMS *sms)
{
  MAV_objList *objList= (MAV_objList *) mav_SMSDataGet(sms);

  mav_listPointerPop(objList->list);
  
  return 1;
}



/* Next object */

int mav_objListObjectNext(MAV_SMS *sms, MAV_object **obj)
{
  MAV_objList *objList= (MAV_objList *) mav_SMSDataGet(sms);

  return (mav_listItemNext(objList->list, (void **) obj));
}



/* ExecuteFn */

int mav_objListExecFn(MAV_SMS *sms, MAV_drawInfo *di, MAV_SMSExecFn *fn)
{
  MAV_objList *objList= (MAV_objList *) mav_SMSDataGet(sms);
  MAV_BB bb;
  MAV_object *o;
  int n, corner_list[MAV_MAX_CLIP_PLANES];

  /* calc list of corners we need to check BB against */
  for (n=0; n<di->cp.num; n++) corner_list[n]= mav_BBGetCorner(di->cp.planes[n].norm);

  /* step through list calling function if object is not culled */
  mav_listPointerReset(objList->list);
  while (mav_listItemNext(objList->list, (void **) &o)) {
    if (mav_callbackBBExec(mav_win_current, o, &bb)) 
    {
      if (mav_BBIntersectsClipPlanes(bb, corner_list, &di->cp)) (*(fn->fn))(o, di, fn->params); 
    }
    else
    {
      if (fn->nocalc) (*(fn->fn))(o, di, fn->params);
    }
  }

  return 1;
}



/* Empty */

int mav_objListEmpty(MAV_SMS *sms, int *o)
{
  MAV_objList *objList= (MAV_objList *) mav_SMSDataGet(sms);
  MAV_object *obj;

  /* update object tables and/or delete objects */
  mav_listPointerReset(objList->list);
  while (mav_listItemNext(objList->list, (void **) &obj)) {
    if (mav_opt_objectTables) mav_objectTablesSMSRmv(obj, sms);
    if (*o) mav_objectDelete(obj);
  }

  /* now empty contents of list */
  mav_listEmpty(objList->list);

  return 1;
}



/* Delete */

int mav_objListDelete(MAV_SMS *sms, int *o)
{
  MAV_objList *objList= (MAV_objList *) mav_SMSDataGet(sms);

  /* empty the SMS */
  mav_objListEmpty(sms, o);

  /* free memory */
  mav_listDelete(objList->list);
  mav_free(objList);

  return 1;
}



/* Size */

int mav_objListSize(MAV_SMS *sms, int *sz)
{
  MAV_objList *objList= (MAV_objList *) mav_SMSDataGet(sms);

  *sz= mav_listSize (objList->list);

  return 1;
}



/* Routine for creating this class of SMS */

MAV_objList *mav_objListNew(void)
{
  MAV_objList *objList= (MAV_objList *) mav_malloc(sizeof(MAV_objList));

  objList->list= mav_listNew();

  return objList;
}



/* Routine to create an SMS instance of this class */

MAV_SMS *mav_SMSObjListNew(void)
{
  return mav_SMSNew(mav_SMSClass_objList, mav_objListNew());
}
