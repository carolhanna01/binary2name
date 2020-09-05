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

MAV_SMSClass *mav_SMSClass_HBB;

int allow_interrupts= 0;
int mav_render_aborted;

MAV_HBB *mav_HBBNew (void) {
  MAV_HBB *new_hbb;

  new_hbb= mav_malloc (sizeof (MAV_HBB));
  new_hbb->root= NULL;
  new_hbb->size= 0;
  new_hbb->pointer= NULL;

  return new_hbb;
}

/* Routine to create an SMS instance of this class */

MAV_SMS *mav_SMSHBBNew(void)
{
  return mav_SMSNew(mav_SMSClass_HBB, mav_HBBNew());
}

void mav_setFrameInterrupts (int mode)
{
  if (mode)
    allow_interrupts= 1;
  else
    allow_interrupts= 0;
}

int mav_HBBPointerReset (MAV_SMS *sms) {
  MAV_HBB *hbb= (MAV_HBB *) mav_SMSDataGet (sms);

  if (hbb->pointer == NULL)
	hbb->pointer= (MAV_HBBPointer *) mav_malloc (sizeof (MAV_HBBPointer));

  hbb->pointer->curr_cluster= hbb->root;

  return 1;
}

void mavlib_HBBUpLevel (MAV_HBBPointer *ptr) {
  MAV_HBBCluster *last_child;
  MAV_HBBChild *ch;

  if (ptr->curr_cluster->parent == NULL)
  /* completed traversal */
    ptr->curr_cluster= NULL;
  else {
    last_child= ptr->curr_cluster;
    ptr->curr_cluster= ptr->curr_cluster->parent;

  /* check for another child node */
    ch= ptr->curr_cluster->children;

    while (ch->child != last_child)
  /* if this fails something well wacky has gone on */
      ch= ch->next;

  /* move to next child */
    ch= ch->next;

    if (ch) ptr->curr_cluster= ch->child;
    else
  /* last child on this level so go up another level */
      mavlib_HBBUpLevel (ptr);
  }
}

void mavlib_HBBNextCluster (MAV_HBBPointer *ptr) {

  /* if current node has children go down 1 level */
  if (ptr->curr_cluster->children) {
    ptr->curr_cluster= ptr->curr_cluster->children->child;

  /* check there is an object at this level otherwise move to next cluster */
    if (ptr->curr_cluster->obj == NULL) mavlib_HBBNextCluster (ptr);
  } else {
  /* go back up one (or more) levels */
    mavlib_HBBUpLevel (ptr);

    if (ptr->curr_cluster)
      if (ptr->curr_cluster->obj == NULL) mavlib_HBBNextCluster (ptr);
  }
}

int mav_HBBObjectNext (MAV_SMS *sms, MAV_object **next_obj) {
  MAV_HBB *hbb= (MAV_HBB *) mav_SMSDataGet (sms);

  if (hbb->pointer->curr_cluster == NULL)
    return 0;
  else {
    if (hbb->pointer->curr_cluster->obj == NULL) {
      mavlib_HBBNextCluster (hbb->pointer);
      if (hbb->pointer->curr_cluster == NULL)
	return 0;
    }

    *next_obj= hbb->pointer->curr_cluster->obj;
    mavlib_HBBNextCluster (hbb->pointer);
  }

  return 1;
}

void mavlib_HBBDeleteFunc (MAV_HBBCluster *node) {
  MAV_HBBChild *ch;
  MAV_HBBChild *next_ch;

  /* delete node and all objects it contains */

  if (node != NULL) {
    ch= node->children;

  /* delete children first */
    while (ch != NULL) {
    next_ch= ch->next;
    mavlib_HBBDeleteFunc (ch->child);
    ch= next_ch;
  }

  /* if there is an object at this level then delete it */
  if (node->obj != NULL)
    mav_objectDelete (node->obj);

  /* free HBB node */
  mav_free (node);
}
}

#if 0
void
mav_SMSHBBDelete (MAV_SMS *sms) {
  MAV_HBB *hbb= (MAV_HBB *) mav_SMSDataGet(sms);

  mavlib_deletedSMS= sms;
  mavlib_HBBDeleteFunc (hbb->root);
  mav_free (sms);
  mavlib_deletedSMS= sms;
}

void
mavlib_HBBEmptyFunc (MAV_HBBCluster *node, MAV_SMS *sms) {
  MAV_HBBChild *ch;
  MAV_HBBChild *next_ch;

  /* delete node but keep its objects */

  if (node != NULL) {
    ch= node->children;

  /* delete children first */
    while (ch != NULL) {
      next_ch= ch->next;
      mavlib_HBBEmptyFunc (ch->child, sms);
      ch= next_ch;
    }

  /* make sure object tables are kept up to date */
    if ((node->obj != NULL)&&(mav_opt_objectTables))
      mavlib_objectTablesRemoveSMS (node->obj, sms);

  /* free node memory */
    mav_free (node);
  }
}

void
mav_HBBEmpty (MAV_SMS *sms) {
  MAV_HBB *hbb= (MAV_HBB *) mav_SMSDataGet (sms);

  mavlib_HBBEmptyFunc (hbb->root, sms);
  hbb->root= NULL;
  hbb->size= 0;
  hbb->pointer= NULL;
}
#endif

int mavlib_HBBRemoveObjectFunc (MAV_HBBCluster *node, MAV_object *mo) 
{
  int found= 0;
  MAV_HBBCluster *current;
  MAV_HBBCluster *parent_node;
  MAV_HBBChild *ch;
  MAV_HBBChild *last_ch;
  MAV_HBBChild *next_ch;

  current= node;

    /* check current object */
    if (current->obj == mo) {
      /* found it */
      found= 1;
      /* delete it from this node */
      current->obj= NULL;

      /* delete from parent's children list */
      parent_node= current->parent;
      if (parent_node == NULL) {
  	  /* root node */
      } else {
	ch= parent_node->children;
	last_ch= NULL;
	while (ch->child != current) {
	  last_ch= ch;
	  ch= ch->next;
	}
	if (last_ch == NULL)
	  parent_node->children= ch->next;
	else
	  last_ch->next= ch->next;
	mav_free (ch);
	parent_node->num_children --;
      }

      /* update children to point to correct parent */
      ch= current->children;
      while (ch != NULL) {
	next_ch= ch->next;
	ch->next= parent_node->children;
	parent_node->children= ch;
	parent_node->num_children ++;
	ch->child->parent= parent_node;
	ch= next_ch;
      }
    } else {
      ch= current->children;
      while (!found && (ch != NULL)) {
	found= mavlib_HBBRemoveObjectFunc (ch->child, mo); ch= ch->next;
      }
    }

    return found;
}

int mav_HBBObjectRmv (MAV_SMS *sms, MAV_object *mo) {
  MAV_HBB *hbb= (MAV_HBB *) mav_SMSDataGet (sms);
  int rv=0;

  if (hbb->size) {
    if (mavlib_HBBRemoveObjectFunc (hbb->root, mo)) {
      rv=1;
      hbb->size --;
      if (mav_opt_objectTables) mav_objectTablesSMSRmv (mo, sms);
    }
  }

  return rv;
}

int mavlib_HBBContainsObjectFunc (MAV_HBBCluster *node, MAV_object *mo) 
{
  MAV_HBBCluster *current;
  MAV_HBBChild *ch;
  int found= 0;

    /* start at root */
    current= node;

    if (current->obj == mo)  {
      /* found it */
      found= 1;
    } else {
      /* search this node's children */
      ch= current->children;
      while ((!found) && (ch != NULL)) {
	found= mavlib_HBBContainsObjectFunc (ch->child, mo);
	ch= ch->next;
      }
    }
    return (found);
}

int mav_HBBContainsObject (MAV_SMS *sms, MAV_object *mo) {
  MAV_HBB *hbb= (MAV_HBB *) mav_SMSDataGet (sms);

  if (hbb->size)
    return mavlib_HBBContainsObjectFunc (hbb->root, mo);
  else
    return 0;
}

int mav_HBBSize (MAV_SMS *sms, int *sz) {
  MAV_HBB *hbb= (MAV_HBB *) mav_SMSDataGet (sms);

  *sz= hbb->size;

  return 1;
}

void mavlib_HBBDoCluster (MAV_HBBCluster *node, MAV_drawInfo *di, MAV_SMSExecFn *fn) {
/* do complete cluster without culling */
  MAV_HBBChild *ch;

#if 0
  if (allow_interrupts && !mav_render_aborted)
    if (mav_eventWaiting ())
      mav_render_aborted= 1;
#endif

  if (node->obj != NULL) 
  /* do this node's object */
    (*(fn->fn)) (node->obj, di, fn->params);

  ch= node->children;
  while ((ch != NULL) && !mav_render_aborted) {
  /* draw children */
    mavlib_HBBDoCluster (ch->child, di, fn);
    ch= ch->next;
  }
}

void mavlib_HBBCullFunc (MAV_HBBCluster *node, int *corner_list, MAV_drawInfo *di, MAV_SMSExecFn *fn)
{
/* cull and draw cluster */
  MAV_HBBChild *ch;
  int test_result;

  /* check for interrupt */
#if 0
  if (allow_interrupts && !mav_render_aborted)
    if (mav_eventWaiting ());
      mav_render_aborted= 1;
#endif

  if (!mav_render_aborted) {
  /* check this node's bb against the frustum */
    test_result= mav_BBIntersectsClipPlanes(node->box, corner_list, &di->cp);

  /* set status (for redraw) */
    node->status= test_result;

    if (test_result) {
  /* definitely intersected */
      if (test_result==1)
  /* total enclosure so draw complete cluster */
        mavlib_HBBDoCluster (node, di, fn);
      else {
  /* partial enclosure so do this object and intersect */
  /* rest of cluster */
        if (node->obj != NULL)
	  (*(fn->fn)) (node->obj, di, fn->params);

  /* do children */
	ch= node->children;
	while (ch != NULL) {
	  mavlib_HBBCullFunc (ch->child, corner_list, di, fn);
	  ch= ch->next;
	}
      }
    }
  }
}

int mav_HBBExecFn (MAV_SMS *sms, MAV_drawInfo *di, MAV_SMSExecFn *fn) 
{
  MAV_HBB *hbb= (MAV_HBB *) mav_SMSDataGet (sms);
  int corner_list[MAV_MAX_CLIP_PLANES];
  int n;

  mav_render_aborted= 0;

  if (hbb->size) {
    for (n= 0; n <di->cp.num; n++)
      corner_list[n]= mav_BBGetCorner (di->cp.planes[n].norm);

    mavlib_HBBCullFunc (hbb->root, corner_list, di, fn);
  }

  return 1;
}

int mavlib_HBBIntersectHitBB (MAV_window *w, MAV_HBBCluster *mc, MAV_line ln, MAV_objectIntersection *objint, MAV_object **obj)
{
/* intersect a BB with a ray that we know hits it */
  MAV_objectIntersection objint2;
  MAV_HBBChild *ch;
  int rv= MAV_FALSE;
  int test_rv;

  if (mc->obj != NULL) {
  /* check this node's object */
    if (mav_callbackIntersectExec(w, mc->obj, ln, &objint2)) {
      rv= MAV_TRUE;

  /* check if valid hit */
      if (objint2.pt1 < objint->pt1) {
	*objint= objint2;
	*obj= mc->obj;
      }
    }
  }

  /* check this node's children */
  ch= mc->children;
  while (ch != NULL) {
  /* intersect child's bbox */
    if (mav_BBIntersectsLine (ch->child->box, ln, &objint2))
      if (objint2.pt1 < objint->pt1) {
  /* if a valid hit then recurse */
        test_rv= mavlib_HBBIntersectHitBB (w, ch->child, ln, objint, obj);
	if (!rv) rv= test_rv;
      }
    ch= ch->next;
  }

  return rv;
}

int mav_HBBIntersect (MAV_SMS *sms, MAV_window *w, MAV_line *ln, MAV_objectIntersection *objint, MAV_object **obj)
{
  MAV_HBB *hbb= (MAV_HBB *) mav_SMSDataGet (sms);
  MAV_objectIntersection objint2;
  int rv= MAV_FALSE;

  *obj= NULL;
  objint->pt1= MAV_INFINITY;

  if (hbb->size)
  /* intersect this node's bb */
    if (mav_BBIntersectsLine (hbb->root->box, *ln, &objint2))
      if (objint2.pt1 < objint->pt1)
        rv= mavlib_HBBIntersectHitBB (w, hbb->root, *ln, objint, obj);

  return rv;
}



int mav_HBBPointerPush(MAV_SMS *s)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: HBBPointerPush not implemented yet!\n");
  return 0;
}

int mav_HBBPointerPop(MAV_SMS *s)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: HBBPointerPop not implemented yet!\n");
  return 0;
}

int mav_HBBEmpty(MAV_SMS *s, int *o)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: HBBEmpty not implemented yet!\n");
  return 0;
}

int mav_HBBDelete(MAV_SMS *s, int *o)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: HBBDelete not implemented yet!\n");
  return 0;
}

