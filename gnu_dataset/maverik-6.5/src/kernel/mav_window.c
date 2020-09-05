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


#include "mavlib_kernel.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#if !defined(WIN32) || defined(__CYGWIN__)
char *strdup (const char *s1);
#endif

MAV_list *mav_win_list;
MAV_window *mav_win_current= NULL;
MAV_window *mav_win_orig= NULL;
extern int mavlib_usedWin[];
MAV_frameFn mav_windowChgFn=NULL;
MAV_surfaceParams *mav_sp_default=NULL;



/* Routine to set the active window for rendering */

void mav_windowSet(MAV_window *win)
{
  if (win) {
    mav_win_current= win;
    mav_surfaceParamsUndefine();
    mav_gfxWindowSet(win->id);
    mav_gfxMatrixLoad(win->viewMat);
    if (mav_windowChgFn) mav_windowChgFn(win);
  }
}



/* Routine to apply a null translation to the translated view parameters */

void mav_eyeMono(MAV_window *w)
{
  /* simply copy provided data into windows data structure */

  w->eye= w->vp->trans_eye;
  w->view= w->vp->trans_view;
  w->up= w->vp->trans_up;
  w->right= w->vp->trans_right;
}



/* Routine to create a new window */

MAV_window *mav_windowNew(int x, int y, int width, int height, char *name, char *disp) 
{
  MAV_window *win, *orig_win= mav_win_current;
  int i, id;
  
  win= (MAV_window *) mav_malloc(sizeof(MAV_window));

  /* give it an id */
  id= -1;
  for (i=1; (i<MAV_MAX_WIN && id==-1); i++) {
    if (mavlib_usedWin[i]==0) {
      id=i;
      mavlib_usedWin[id]= 1;
    }
  }

  if (id==-1) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Maximum of %i windows. Exiting\n", MAV_MAX_WIN-1);
    exit(1);
  }
  
  win->id= id;
  win->name= strdup(name);

  /* add to all windows list */
  mav_listItemAdd(mav_win_list, (void *) win);

  /* set window position */
  win->x= x;
  win->y= y;

  /* ask window manager to open window. Store actual width and height in window data structure */
  mav_gfxWindowOpen(win->id, x, y, width, height, name, disp, mav_opt_WMPlacement, mav_opt_singleBuf, mav_opt_quadBuf, mav_opt_multiSample, mav_opt_accumBuf, mav_opt_stencilBuf, mav_opt_destAlpha, &win->width, &win->height);
  mav_windowSet(win);

  if (win->width!=width && mav_opt_output==MAV_VERBOSE) {
    fprintf(stderr, "Warning: Requested width %i, actual %i\n", width, win->width);
  }

  if (win->height!=height && mav_opt_output==MAV_VERBOSE) {
    fprintf(stderr, "Warning: Requested height %i, actual %i\n", height, win->height);
  }

  /* the original window (cant be deleted) */
  if (win->id==1) {
    mav_win_orig= win;

    /* create the global palette handle (now we have a valid gfx context) */
    mav_palette_default= mav_paletteNew();
    mav_windowPaletteSet(mav_win_all, mav_palette_default);
  }

  /* set some default values for the window */
  win->viewMat= MAV_ID_MATRIX;
  win->vp= &mav_vp_default;
  win->mod= NULL;
  win->vmp= NULL;
  mav_windowBackgroundColourSet(win, 0.0, 0.0, 0.0);
  mav_windowPerspectiveSet(win, 0.1, 1000.0, 60.0, ((float) width)/height);
  
  if (mav_opt_multiSample) mav_gfxMultiSampleSet(MAV_TRUE);

  mav_gfxDepthTestSet(MAV_TRUE);
  mav_gfxNormalizeSet(MAV_TRUE);
  mav_gfxMatrixMode(MAV_MODELVIEW);
  mav_gfxMatrixLoad(MAV_ID_MATRIX);

  mav_windowPaletteSet(win, mav_palette_default);

  /* display background */
  mav_gfxClearCZ();
  mav_gfxWindowBuffersSwap();

  /* Sky blue is default background colour after an initial black clear buffers */
  /* beiHR Farbe geaendert von 0.0, 0.5, 1.0 */
  	mav_windowBackgroundColourSet(win, 0.5, 0.5, 1.0);

  if (orig_win)
	  mav_windowSet(orig_win);

  return win;
}



/* Routines to delete a window */

void mav_windowDeleteID(int id)
{
  MAV_window *win;

  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &win)) {
    if (win->id==id) {
      mav_windowDelete(win);
      return;
    }
  }
}

void mav_windowDelete(MAV_window *win)
{
  if (win==mav_win_all) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Can not delete all windows, ignoring\n");
  }
  else if (win==mav_win_orig)
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Can not delete original window, ignoring\n");
  }
  else
  {
    if (win==mav_win_current) mav_windowSet(mav_win_orig);

    /* remove from all windows list */
    mav_listItemRmv(mav_win_list, (void *) win);    

    /* ask window manager to close window */
    mav_gfxWindowClose(win->id);

    /* flag window as unused */
    mavlib_usedWin[win->id]=0;

    /* free up memory, and NULLify pointer */
    mav_free(win);
    win=NULL;
  }
}



/* Routines to set a windows background colour */

void mavlib_setBackgroundColourToAll(float r, float g, float b)
{
  MAV_window *w;

  /* step through window list setting background for windows */
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowBackgroundColourSet(w, r, g, b);
}

void mav_windowBackgroundColourSet(MAV_window *win, float r, float g, float b)
{
  MAV_window *curr_win= mav_win_current;

  if (win==mav_win_all) 
  {
    mavlib_setBackgroundColourToAll(r, g, b);
  }
  else
  {
    if (win!=curr_win) mav_windowSet(win);
    win->background_red= r;
    win->background_green= g;
    win->background_blue= b;
    mav_gfxBackgroundColourSet(r,g,b);
    if (win!=curr_win) mav_windowSet(curr_win);
  }
}



/* Routines to set a windows blending */

void mavlib_setBlendToAll(int i)
{
  MAV_window *w;

  /* step through window list setting blend*/
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowBlendSet(w, i);
}

void mav_windowBlendSet(MAV_window *win, int blend)
{
  MAV_window *curr_win= mav_win_current;

  if (win==mav_win_all) 
  {
    mavlib_setBlendToAll(blend);
  }
  else
  {
    if (win!=curr_win) mav_windowSet(win);
    mav_gfxBlendSet(blend);
    if (win!=curr_win) mav_windowSet(curr_win);
  }
}


		 
/* Routines to set a windows backface culling */

void mavlib_setBackfaceCullToAll(int i)
{
  MAV_window *w;

  /* step through window list setting backface cull*/
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowBackfaceCullSet(w, i);
}

void mav_windowBackfaceCullSet(MAV_window *win, int cull)
{
  MAV_window *curr_win= mav_win_current;

  if (win==mav_win_all) 
  {
    mavlib_setBackfaceCullToAll(cull);
  }
  else
  {
    if (win!=curr_win) mav_windowSet(win);
    mav_gfxBackfaceCullSet(cull);
    if (win!=curr_win) mav_windowSet(curr_win);
  }
}



/* Routine to get a windows backface culling status */

int mav_windowBackfaceCullGet(MAV_window *w)
{
  MAV_window *curr_win= mav_win_current;
  int rv=MAV_FALSE;

  if (w==mav_win_all) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: mav_windowBackfaceCullGet operation not permitted on mav_win_all, ignoring\n");
  }
  else
  {
    if (w!=curr_win) mav_windowSet(w);
    rv= mav_gfxBackfaceCullGet();
    if (w!=curr_win) mav_windowSet(curr_win);
  }
  
  return rv;
}



/* Routines to set a windows view parameters */

void mavlib_setViewParamsToAll(MAV_viewParams *vp) 
{
  MAV_window *w;

  /* step through window list setting viewparams */
  mav_listPointerReset (mav_win_list); 
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowViewParamsSet(w, vp);
}

void mav_windowViewParamsSet(MAV_window *win, MAV_viewParams *vp) 
{
  MAV_window *curr_win= mav_win_current;
  
  if (win==mav_win_all)
  {
    mavlib_setViewParamsToAll(vp);
  }
  else 
  {
    if (curr_win != win) mav_windowSet (win);
    win->vp= vp;
    if (curr_win != win) mav_windowSet (curr_win);
  }
}



/* Routines to set a windows view modifier function and parameters */

void mavlib_setViewModifierToAll(MAV_viewModifierParams *vmp, MAV_viewModifierFn fn) 
{
  MAV_window *w;

  /* step through window list setting stereoparams */
  mav_listPointerReset (mav_win_list); 
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowViewModifierSet(w, vmp, fn);
}

void mav_windowViewModifierSet(MAV_window *win, MAV_viewModifierParams *vmp, MAV_viewModifierFn fn) 
{
  MAV_window *curr_win= mav_win_current;
  
  if (win==mav_win_all)
  {
    mavlib_setViewModifierToAll(vmp, fn);
  }
  else 
  {
    if (curr_win != win) mav_windowSet (win);
    win->mod= fn;
    win->vmp= vmp;
    if (curr_win != win) mav_windowSet (curr_win);
  }
}



/* Routines to set a windows perspective */

void mavlib_setPerspectiveToAll(float ncp, float fcp, float fov, float aspect) 
{
  MAV_window *w;

  /* step through window list setting perspective */
  mav_listPointerReset (mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowPerspectiveSet(w, ncp, fcp, fov, aspect);
}

void mav_windowPerspectiveSet(MAV_window *win, float ncp, float fcp, float fov, float aspect) 
{
  MAV_window *curr_win= mav_win_current;
  
  if (win==mav_win_all)
  {
    mavlib_setPerspectiveToAll (ncp, fcp, fov, aspect);
  }
  else 
  {
    if (curr_win != win) mav_windowSet (win);

    /* set the perspective for the window */

    mav_gfxMatrixMode(MAV_PROJECTION);
    mav_gfxMatrixLoad(MAV_ID_MATRIX);
    mav_gfxPerspectiveSet(ncp, fcp, fov, aspect);
    win->projMat= mav_gfxMatrixGet();
    mav_gfxMatrixMode(MAV_MODELVIEW);

    /* record the values */
    win->fov= fov;
    win->aspect= aspect;
    win->ncp= ncp;
    win->fcp= fcp;

    /* flag as perspective projection */
    win->orthogonal= 0;

    if (curr_win != win) mav_windowSet (curr_win);
  }
}



/* Routines to set an orthogonal projection */

void mavlib_setOrthogonalToAll (float ncp, float fcp, float size, float aspect) 
{
  MAV_window *w;

  /* step through window list setting projection */
  mav_listPointerReset (mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowOrthogonalSet(w, ncp, fcp, size, aspect);
}

void mav_windowOrthogonalSet(MAV_window *win, float ncp, float fcp, float size, float aspect) 
{
  MAV_window *curr_win= mav_win_current;
  
  if (win==mav_win_all)
  {
    mavlib_setOrthogonalToAll (ncp, fcp, size, aspect);
  }
  else 
  {
    if (curr_win != win) mav_windowSet (win);

    /* set the projection for the window */
    mav_gfxMatrixMode(MAV_PROJECTION);
    mav_gfxMatrixLoad(MAV_ID_MATRIX);
    mav_gfxOrthogonalSet(-0.5*size*aspect, 0.5*size*aspect, -0.5*size, 0.5*size, ncp, fcp);
    win->projMat= mav_gfxMatrixGet();
    mav_gfxMatrixMode(MAV_MODELVIEW);

    /* record the values */
    win->ortho_size= size;
    win->aspect= aspect;
    win->ncp= ncp;
    win->fcp= fcp;

    /* flag as orthogonal */
    win->orthogonal= 1;

    if (curr_win != win) mav_windowSet (curr_win);
  }
}



/* Routines to set polygon mode */

void mavlib_setPolygonModeToAll(int v) 
{
  MAV_window *w;

  /* step through window list setting polygon mode */
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowPolygonModeSet(w, v);
}

void mav_windowPolygonModeSet(MAV_window *win, int v)
{
  MAV_window *curr_win= mav_win_current;
  
  if (win==mav_win_all)
  {
    mavlib_setPolygonModeToAll(v);
  }
  else 
  {
    if (curr_win != win) mav_windowSet (win);

    /* set the polygon mode for the window */

    mav_gfxPolygonModeSet(v);

    if (curr_win != win) mav_windowSet (curr_win);
  }
}



/* Routines to set multisample */

void mavlib_setMultiSampleToAll(int v) 
{
  MAV_window *w;

  /* step through window list setting multisample */
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowMultiSampleSet(w, v);
}

void mav_windowMultiSampleSet(MAV_window *win, int v)
{
  MAV_window *curr_win= mav_win_current;
  
  if (win==mav_win_all)
  {
    mavlib_setMultiSampleToAll(v);
  }
  else 
  {
    if (curr_win != win) mav_windowSet(win);

    /* set the multisample for the window */

    mav_gfxMultiSampleSet(v);

    if (curr_win != win) mav_windowSet(curr_win);
  }
}



/* Routines to set a windows line width */

void mavlib_setLineWidthToAll(float v)
{
  MAV_window *w;

  /* step through window list setting line width */
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowLineWidthSet(w, v);
}

void mav_windowLineWidthSet(MAV_window *win, float v)
{
  MAV_window *curr_win= mav_win_current;

  if (win==mav_win_all) 
  {
    mavlib_setLineWidthToAll(v);
  }
  else
  {
    if (win!=curr_win) mav_windowSet(win);
    mav_gfxLineWidthSet(v);
    if (win!=curr_win) mav_windowSet(curr_win);
  }
}



/* Routines to set a windows line stipple */

void mavlib_setLineStippleToAll(int factor, unsigned short pattern)
{
  MAV_window *w;

  /* step through window list setting line stipple */
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowLineStippleSet(w, factor, pattern);
}

void mav_windowLineStippleSet(MAV_window *win, int factor, unsigned short pattern)
{
  MAV_window *curr_win= mav_win_current;

  if (win==mav_win_all) 
  {
    mavlib_setLineStippleToAll(factor, pattern);
  }
  else
  {
    if (win!=curr_win) mav_windowSet(win);
    mav_gfxLineStippleSet(factor, pattern);
    if (win!=curr_win) mav_windowSet(curr_win);
  }
}



/* Routines to set a windows fog */

void mavlib_setFogToAll(int tp, float d1, float d2, float r, float g, float b)
{
  MAV_window *w;

  /* step through window list setting fog */
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_windowFogSet(w, tp, d1, d2, r, g, b);
}

void mav_windowFogSet(MAV_window *win, int tp, float d1, float d2, float r, float g, float b)
{
  MAV_window *curr_win= mav_win_current;

  if (win==mav_win_all) 
  {
    mavlib_setFogToAll(tp, d1, d2, r, g, b);
  }
  else
  {
    if (win!=curr_win) mav_windowSet(win);
    if (r<0 && g<0 && b<0)
    {
      mav_gfxFogSet(tp, d1, d2, win->background_red, win->background_green, win->background_blue);
    }
    else
    {
      mav_gfxFogSet(tp, d1, d2, r, g, b);
    }
    if (win!=curr_win) mav_windowSet(curr_win);
  }
}



/* Routine to dump the contents of a window as a ppm file */

void mav_windowDump(MAV_window *w, char *fn)
{
  MAV_window *orig= mav_win_current;
  unsigned char *mem= mav_malloc(w->width*w->height*3*sizeof(unsigned char));
  unsigned char *rowPtr;
  FILE *f;
  int i;

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "dumping %s...", fn);

  /* Switch to correct window */
  if (w!=orig) mav_windowSet(w);

  /* read the frame buffer into memory */
  mav_gfxBufferReadSet(MAV_FRONT);
  mav_gfxPixelReadUByte(0, 0, w->width, w->height, mem);

  /* open the file */
  f= fopen(fn, "w");
  if (!f) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Could not dump window to file %s\n", fn);
    mav_free(mem);
    return;
  }

  fprintf(f,"P6\n");
  fprintf(f,"# PPM-file created by Maverik\n");
  fprintf(f,"%i %i\n", w->width, w->height);
  fprintf(f,"255\n");
  fclose(f);

  f = fopen(fn, "ab");  /* now append binary data */
  if (!f) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Could not dump window to file %s\n", fn);
    mav_free(mem);
    return;
  }

  for (i=0; i<w->height; i++) {
    /* Remember, OpenGL images are bottom to top.  Have to reverse. */
    rowPtr = mem + (w->height-1-i) * (w->width*3);
    fwrite(rowPtr, 1, w->width*3, f);
  }

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "done\n");
  fclose(f);

  mav_free(mem);

  /* Switch to original window */
  if (w!=orig) mav_windowSet(orig);
}
