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


#include "mavlib_callbacks.h"
#include <stdlib.h>
#include <stdio.h>



/* Wrapper routines to set and execute draw callback */

MAV_callback *mav_callback_draw;

void mav_callbackDrawSet(MAV_window *w, MAV_class *c, MAV_callbackDrawFn fn)
{
  mav_callbackSet(mav_callback_draw, w, c, (MAV_callbackFn) fn);
}

int mav_callbackDrawExec(MAV_window *w, MAV_object *o, MAV_drawInfo *di)
{
  int rv;

  /* check if its textured */
  if (mav_opt_delayTexture && mav_objectIsTextured(mav_win_current, o))
  {
    mav_texturedObjectsManage(w, o, di);
    rv= 1;
  } /* check if its transparent */
  else if (mav_opt_trans && mav_objectIsTransparent(mav_win_current, o))
  {
    mav_transparentObjectsManage(w, o, di);
    rv= 1;
  }
  else
  {
    rv= mav_callbackExec(mav_callback_draw, w, o, di, NULL);
  }

  return rv;
}



/* Support routines for draw */

/* Routine to translate a drawInfo structure defined in one coordinate frame into another */

MAV_drawInfo mav_drawInfoTransFrame(MAV_drawInfo di, MAV_matrix mat)
{
  MAV_drawInfo rv;
  MAV_vector pt;
  MAV_matrix imat;
  int i;

/* Account for transformation */

  imat= mav_matrixInverse(mat);

  rv.vp.eye= mav_vectorMult(di.vp.eye, imat);
  rv.vp.view= mav_vectorNormalize(mav_vectorMult3x3(di.vp.view, imat));
  rv.vp.up= mav_vectorNormalize(mav_vectorMult3x3(di.vp.up, imat));
  rv.vp.right= mav_vectorNormalize(mav_vectorMult3x3(di.vp.right, imat));

  rv.cp.num= di.cp.num;
  for (i=0; i<rv.cp.num; i++) {
    rv.cp.planes[i].norm= mav_vectorNormalize(mav_vectorMult3x3(di.cp.planes[i].norm, imat));
    pt= mav_vectorScalar(di.cp.planes[i].norm, di.cp.planes[i].d);
    pt= mav_vectorMult(pt, imat);
    rv.cp.planes[i].d= mav_vectorDotProduct(pt, rv.cp.planes[i].norm);
  }

  return rv;
}



/* Routine to work out if an object is transparent */

int mav_objectIsTransparent(MAV_window *w, MAV_object *o)
{
  MAV_surfaceParams **sp;

  return (mav_callbackGetSurfaceParamsExec(w, o, &sp) && mav_surfaceParamsIsTransparent(w, *sp));
}



/* Routine to deal with transparent object (store and render later) */

void mav_transparentObjectsManage(MAV_window *w, MAV_object *o, MAV_drawInfo *di)
{
  MAV_transObjData *to= (MAV_transObjData *) mav_malloc(sizeof(MAV_transObjData));
  MAV_vector wpos;

  /* store the window */
  to->win= mav_win_current;

  /* store transparent object */
  to->obj= o;
  to->fn= (MAV_callbackDrawFn) mav_callbackQuery(mav_callback_draw, w, o);
  if (di) 
  {
    to->di= *di;
    to->dip= &to->di;
  }
  else 
  {
    to->dip= NULL;
  }

  /* store the pre-multiplying matrix */
  if (mav_opt_trackMatrix) 
  {
    to->mat= mav_win_current->viewMat;
  }
  else
  {
    to->mat= mav_gfxMatrixGet();
  }

  /* calc the bb */
  if (mav_callbackBBExec(mav_win_current, o, &to->bb)) 
  {
    mav_BBAlign(to->bb, to->mat, &to->bb);
    wpos= mav_vectorScalar(mav_vectorAdd(to->bb.min, to->bb.max), 0.5);
    to->dist2= mav_vectorDotProduct(wpos, wpos);

    /* add to transparent object list */
    mav_listItemAdd(mav_transObjList, (void *) to);
  }
  else
  {
    mav_free(to);
    fprintf(stderr, "Warning: transparent object has no BB callback defined, ignoring.\n");
  }
}



/* Routine for rendering transparencies at the end of frame */

void mav_transparentObjectsRender(void *ignored)
{
  MAV_transObjData *to, *targ;
  float larg;
  int more, org_opt_trans;
  MAV_window *cur_win, *orig_win= mav_win_current;

  if (!mav_opt_trans) return;

  if (mav_listSize(mav_transObjList)) {

    /* store then turn off transparency trap while rendering list */
    org_opt_trans= mav_opt_trans;
    mav_opt_trans= 0;

    /* for each window containing transparent objects */
    while (mav_listSize(mav_transObjList)) {

      mav_listPointerReset(mav_transObjList);
      mav_listItemNext(mav_transObjList, (void **) &to);

      /* set current window */
      cur_win= to->win;
      mav_windowSet(cur_win);

      /* switch on blending and push the matrix */
      mav_gfxBlendSet(MAV_BLEND_1);
      mav_gfxMatrixPush();

      /* for each transparent object in current window */
      do {
	/* find furthest object */
	larg= -1.0;
	targ= NULL;
	more= 0;
	mav_listPointerReset(mav_transObjList);
	while (mav_listItemNext(mav_transObjList, (void **) &to)) {
	  if ((to->win == cur_win) && (to->dist2>larg)) {
	    targ= to;
	    larg= to->dist2;
	    more=1;
	  }
	}

	if (more) {
#if 0
	  /* check if object intersects with any other */
	  mav_listPointerReset(mav_transObjList);
	  while (mav_listItemNext(mav_transObjList, (void **) &to)) {
	    if (to!=targ && to->win==targ->win && mav_BBIntersectsBB(to->bb, targ->bb)) {
	      printf("Int with %x\n", to);
	    }
	  }
#endif

	  /* draw the object */
	  mav_gfxMatrixLoad(targ->mat);
	  (targ->fn)(targ->obj, targ->dip);
      
	  /* remove item from list and free the memory used */
	  mav_listItemRmv(mav_transObjList, targ);
	  mav_free(targ);
	}

      } while (more);

      /* switch off blending and pop the matrix */
      mav_gfxBlendSet(MAV_BLEND_OFF);
      mav_gfxMatrixPop();
    }

    /* restore transparency trap again */
    mav_opt_trans= org_opt_trans;

    /* restore original window */
    mav_windowSet(orig_win);
  }
}



/* Routine to work out if an object is textured */

int mav_objectIsTextured(MAV_window *w, MAV_object *o)
{
  MAV_surfaceParams **sp;

  return (mav_callbackGetSurfaceParamsExec(w, o, &sp) && mav_surfaceParamsIsTextured(w, *sp));
}



/* Routine to deal with textured object (store and render later) */

void mav_texturedObjectsManage(MAV_window *w, MAV_object *o, MAV_drawInfo *di)
{
  MAV_texturedObjData *to= mav_malloc(sizeof(MAV_texturedObjData));
  MAV_surfaceParams **sp;

  /* store the window */
  to->win= mav_win_current;

  /* store textured object */
  to->obj= o;
  to->fn= (MAV_callbackDrawFn) mav_callbackQuery(mav_callback_draw, w, o);
  if (di) 
  {
    to->di= *di;
    to->dip= &to->di;
  }
  else 
  {
    to->dip= NULL;
  }

  /* store the pre-multiplying matrix */
  if (mav_opt_trackMatrix) 
  {
    to->mat= mav_win_current->viewMat;
  }
  else
  {
    to->mat= mav_gfxMatrixGet();
  }

  /* add to appropriate texture's object list */
  mav_callbackGetSurfaceParamsExec(mav_win_current, o, &sp);
  mav_listItemAdd(mav_textureObjList[(*sp)->texture], (void *) to);
}



/* Routine for rendering textured objects at the end of frame */


void mav_texturedObjectsRender(void *ignored)
{
  MAV_texturedObjData *targ;
  int i, org_opt_delayTexture;
  MAV_window *cur_win, *orig_win= mav_win_current;
  MAV_list *tmp;

  if (!mav_opt_delayTexture) return;

  /* create a tmp list to hold object to be removed */
  tmp= mav_listNew();

  /* store then turn off texture trap while rendering lists */
  org_opt_delayTexture= mav_opt_delayTexture;
  mav_opt_delayTexture= 0;

  /* for each window */
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &cur_win)) {

    /* set window */
    if (mav_win_current!=cur_win) mav_windowSet(cur_win);

    /* push the matrix */
    mav_gfxMatrixPush();

    /* for each texture */
    for (i=0; i<mav_opt_maxTextures; i++) {
      if (mav_listSize(mav_textureObjList[i])) {
	mav_listPointerReset(mav_textureObjList[i]);
	while (mav_listItemNext(mav_textureObjList[i], (void **)&targ)) {
	  /* draw the object and add it to tmp list if correct window */
	  if (targ->win==cur_win) {
	    mav_gfxMatrixLoad(targ->mat);
	    (targ->fn)(targ->obj, targ->dip);
	    mav_listItemAdd(tmp, targ);
	  }
	}

	/* remove objects in tmp list */
	mav_listPointerReset(tmp);
	while (mav_listItemNext(tmp, (void **)&targ)) {
	    mav_listItemRmv(mav_textureObjList[i], targ);
	    mav_free(targ);
	}
       
	/* empty list */
	mav_listEmpty(tmp);
      }
    }
  
    /* pop the matrix */
    mav_gfxMatrixPop();
  }

  /* restore original window */
  mav_windowSet(orig_win);

  /* Delete temporary list */
  mav_listDelete(tmp);

  /* restore texture trap again */
  mav_opt_delayTexture= org_opt_delayTexture;
}
