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
#include <math.h>
#include <stdio.h>
#include <string.h>

int mav_opt_trans= MAV_FALSE;
int mav_opt_delayTexture= MAV_FALSE;
MAV_list *mav_transObjList;
MAV_list *mav_transTextList;
MAV_list **mav_textureObjList;



/* Routines to initialise the common callbacks module */

char *mav_callbacksModuleID(void)
{
  return "Common callbacks";
}

int mav_callbacksModuleInit(void)
{
  int i;

  /* add the new module */
  mav_moduleNew(mav_callbacksModuleID);

  /* define new callbacks */
  mav_callback_draw= mav_callbackNew();
  mav_callback_BB= mav_callbackNew();
  mav_callback_intersect= mav_callbackNew();
  mav_callback_id= mav_callbackNew();
  mav_callback_dump= mav_callbackNew();
  mav_callback_getUserdef= mav_callbackNew();
  mav_callback_getMatrix= mav_callbackNew();
  mav_callback_getSurfaceParams= mav_callbackNew();

  /* support delayed rendering of textured objects */
  mav_textureObjList= mav_malloc(mav_opt_maxTextures*sizeof(MAV_list *));
  for (i=0; i<mav_opt_maxTextures; i++) mav_textureObjList[i]= mav_listNew();
  mav_frameFn3Add(mav_texturedObjectsRender, NULL);

  /* support for delayed rendering of transparent objects and text */
  mav_transObjList= mav_listNew();
  mav_transTextList= mav_listNew();
  mav_frameFn3Add(mav_transparentObjectsRender, NULL);
  mav_frameFn3Add(mav_transparentTextRender, NULL);

  return 1;
}



/* Routine to calculate the length of a string */

#ifdef WIN32
int mav_gfxWindowStringLength(int, char *, int);
#endif

int mav_stringLength(MAV_window *w, char *s, int font)
{
  int tot=0;

#ifdef WIN32
/* note - adding the individual widths together doesn't work */
/* because text characters overlap */
  tot= mav_gfxWindowStringLength(w->id, s, font);
#else
  int i;

  if (w->palette->fontlist[font].defined) 
  {
    for (i=0; i<strlen(s); i++) tot+= w->palette->fontlist[font].width[(int)s[i]];
  }
  else
  {
    if (mav_opt_output) fprintf(stderr, "Warning: font %i not defined\n", font);
  }
#endif

  return tot;
}



/* Routines to display a string on screen */

int mavlib_justify=0;

void mavlib_displayStringToAll(char *s, int col, int font, float x, float y)
{
  MAV_window *w;

  mav_listPointerReset(mav_win_list);
  
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_stringDisplay(w,s,col,font,x,y);
}

void mav_stringDisplay(MAV_window *w, char *s, int col, int font, float x, float y)
{
  MAV_window *orig= mav_win_current;
  MAV_surfaceParams sp;
#ifndef WIN32
  int c=0;
#endif

  if (w==mav_win_all) 
  {
    mavlib_displayStringToAll(s, col, font, x, y);
  }
  else
  {
    /* set the correct colouring */
    sp.mode= MAV_COLOUR;
    sp.colour= col;
    sp.material=0;
    sp.texture=0;

    if (mav_opt_trans && mav_surfaceParamsIsTransparent(w, &sp)) {
      mav_transparentTextManage(w, s, col, font, x, y);
      return;
    }

    if (mav_win_current!=w) mav_windowSet(w);

    mav_surfaceParamsUse(&sp);

    /* check font has been defined */
    if (!mav_win_current->palette->fontlist[font].defined) {
      if (mav_opt_output) fprintf(stderr, "Warning: font %i not defined\n", font);
    }

    /* use a projection matrix such that we talk in term of pixels */
    mav_gfxMatrixMode(MAV_PROJECTION);
    mav_gfxMatrixLoad(MAV_ID_MATRIX);    
    mav_gfxOrthogonalSet(0, mav_win_current->width, 0, mav_win_current->height, -1, 1);
    
    /* use an identity view matrix */
    mav_gfxMatrixMode(MAV_MODELVIEW);
    mav_gfxMatrixPush();
    mav_gfxMatrixLoad(MAV_ID_MATRIX);
    
    /* convert position (in range [-1:1]) into pixel values */
    x= (x+1)/2.0 * mav_win_current->width;
    y= (y+1)/2.0 * mav_win_current->height;

    /* account for justification */
    switch (mavlib_justify) {
    case 1: /* Center justify */
      x-= mav_stringLength(mav_win_current, s, font)/2;
      break;

    case 2: /* Right justify */
      x-= mav_stringLength(mav_win_current, s, font);
      break;
    }

    /* if position is invalid (x<0), ignore a chacter and retry */
#ifdef WIN32
/* don't try culling as character widths are invalid */
    mav_gfxRasterPos2DSet(x,y);
    mav_gfxWindowStringDisplay(s, font);
#else
    while (x<0 && c<strlen(s)) {
      x+= mav_win_current->palette->fontlist[font].width[(int)s[c]];
      c++;
    }

    /* set raster position and display text (unless its all culled out) */
    if (x>=0) {
      mav_gfxRasterPos2DSet(x, y);
      mav_gfxWindowStringDisplay(&s[c], font);
    }
#endif

    /* restore original projection and view matrices */
    mav_gfxMatrixMode(MAV_PROJECTION);
    mav_gfxMatrixLoad(mav_win_current->projMat);
    
    mav_gfxMatrixMode(MAV_MODELVIEW);
    mav_gfxMatrixPop();

    if (mav_win_current!=orig) mav_windowSet(orig);
  }
}



/* Routines to display justified text on screen */

void mav_stringDisplayLeft(MAV_window *w, char *s, int col, int font, float x, float y)
{
  mav_stringDisplay(w, s, col, font, x, y);
}

void mav_stringDisplayCentre(MAV_window *w, char *s, int col, int font, float x, float y)
{
  mavlib_justify= 1;
  mav_stringDisplay(w, s, col, font, x, y);
  mavlib_justify= 0;
}

void mav_stringDisplayRight(MAV_window *w, char *s, int col, int font, float x, float y)
{
  mavlib_justify= 2;
  mav_stringDisplay(w, s, col, font, x, y);
  mavlib_justify= 0;
}



/* Routine to deal with transparent text */

void mav_transparentTextManage(MAV_window *w, char *s, int col, int font, float x, float y)
{
  MAV_transTextData *tt= (MAV_transTextData *) mav_malloc(sizeof(MAV_transTextData));

  /* store transparent text */
  tt->win= w;
  tt->s= (char *) mav_malloc((strlen(s)+1)*sizeof(char));
  strcpy(tt->s, s);
  tt->col= col;
  tt->font= font;
  tt->x= x;
  tt->y= y;
  tt->justify= mavlib_justify;

  /* add to transparent text list */
  mav_listItemAdd(mav_transTextList, (void *) tt);
}

void mav_transparentTextRender(void *ignored)
{
  MAV_transTextData *tt;
  MAV_window *cur_win, *orig_win= mav_win_current;
  int org_opt_trans;

  if (!mav_opt_trans) return;

  if (mav_listSize(mav_transTextList)) {

    /* store then turn off transparency trap while rendering list */
    org_opt_trans= mav_opt_trans;
    mav_opt_trans= 0;

    /* for each window containing transparent text */
    while (mav_listSize(mav_transTextList)) {

      mav_listPointerReset(mav_transTextList);
      mav_listItemNext(mav_transTextList, (void **) &tt);

      /* set current window */
      cur_win= tt->win;
      mav_windowSet(cur_win);

      /* switch on blending */
      mav_gfxBlendSet(MAV_BLEND_1);

      /* for each transparent text in current window */
      mav_listPointerReset(mav_transTextList);
      while (mav_listItemNext(mav_transTextList, (void **) &tt)) {
	if (tt->win==cur_win) {
	  /* display the text */
	  mavlib_justify= tt->justify;
	  mav_stringDisplay(tt->win, tt->s, tt->col, tt->font, tt->x, tt->y);
	  
	  /* remove item from list and free the memory used */
	  mav_listItemRmv(mav_transTextList, tt);
	  mav_free(tt->s);
	  mav_free(tt);
	}
      }

      /* switch off blending */
      mav_gfxBlendSet(MAV_BLEND_OFF);
    }

    /* restore transparency trap again */
    mav_opt_trans= org_opt_trans;

    /* restore original window */
    mav_windowSet(orig_win);
  }
}



/* Routine to animate vp to an object */

void mav_viewParamsAnimateToObject(MAV_window *w, MAV_viewParams *vp, MAV_object *o, float dist, float tim, int style)
{
  MAV_BB bb;
  MAV_vector c;
  float d, r;
  MAV_viewParams init, targ;

  if (!vp) vp= w->vp;

  if (w->orthogonal) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: can not animate vp to object with an orthogonal view\n");    
  }
  else
  {
    /* Calculate BB of object */
    if (mav_callbackBBExec(w, o, &bb)==MAV_FALSE) 
    {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: could not find object's BB to animate vp to object\n");    
    }
    else
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
  }
}
