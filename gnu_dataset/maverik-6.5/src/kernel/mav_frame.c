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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

MAV_list *mavlib_frame0_list;
MAV_list *mavlib_frame1_list;
MAV_list *mavlib_frame2_list;
MAV_list *mavlib_frame3_list;
MAV_list *mavlib_frame4_list;
int mav_firstFrame=MAV_TRUE;
int mav_frameCount=0;
int mav_opt_finish=MAV_UNDEFINED;
int mav_opt_flush=MAV_UNDEFINED;

float mavlib_culTime=0;
int mavlib_culFrame=0;
MAV_timer mavlib_frameTimer;



/* Routines to set and remove frame0 (navigator) functions */

void mav_frameFn0Add(MAV_frameFn fn, void *d)
{
  mav_listItemsAdd(mavlib_frame0_list, (void *) fn, d);
}

void mav_frameFn0Rmv(MAV_frameFn fn, void *d) 
{
  mav_listItemsRmv(mavlib_frame0_list, (void *) fn, d);
}



/* Routines to set and remove frame1 functions */

void mav_frameFn1Add(MAV_frameFn fn, void *d)
{
  mav_listItemsAdd(mavlib_frame1_list, (void *) fn, d);
}

void mav_frameFn1Rmv(MAV_frameFn fn, void *d)
{
  mav_listItemsRmv(mavlib_frame1_list, (void *) fn, d);
}



/* As above but for frame2 */

void mav_frameFn2Add(MAV_frameFn fn, void *d)
{
  mav_listItemsAdd(mavlib_frame2_list, (void *) fn, d);
}

void mav_frameFn2Rmv(MAV_frameFn fn, void *d) 
{
  mav_listItemsRmv(mavlib_frame2_list, (void *) fn, d);
}



/* As above but for frame3 */

void mav_frameFn3Add(MAV_frameFn fn, void *d)
{
  mav_listItemsAdd(mavlib_frame3_list, (void *) fn, d);
}

void mav_frameFn3Rmv(MAV_frameFn fn, void *d) 
{
  mav_listItemsRmv(mavlib_frame3_list, (void *) fn, d);
}



/* As above but for frame4 */

void mav_frameFn4Add(MAV_frameFn fn, void *d)
{
  mav_listItemsAdd(mavlib_frame4_list, (void *) fn, d);
}

void mav_frameFn4Rmv(MAV_frameFn fn, void *d) 
{
  mav_listItemsRmv(mavlib_frame4_list, (void *) fn, d);
}



/* Routine to define the start of a frame */

void mav_frameBegin(void) 
{
  MAV_window *w, *orig_win= mav_win_current;
  MAV_matrix view_matrix;
  MAV_viewParams *vp;
  MAV_frameFn fn;  
  MAV_vector view_shift;
  float hdist, vdist;
  void *d;

  /* start framerate timer */

  mav_timerStart(&mavlib_frameTimer);

  /* Poll devices */

  mav_devicePoll();

  /* Do frame 0 functions */

  mav_listPointerReset(mavlib_frame0_list);  
  while (mav_listItemsNext(mavlib_frame0_list, (void **) &fn, &d)) (*fn)(d);

  /* Do frame 1 functions */

  mav_listPointerReset(mavlib_frame1_list);  
  while (mav_listItemsNext(mavlib_frame1_list, (void **) &fn, &d)) (*fn)(d);

  /* Clear buffers and set matrices for each window */

  mav_listPointerReset(mav_win_list);

  while (mav_listItemNext(mav_win_list, (void **) &w)) {
    if (w!=mav_win_current) mav_windowSet(w);

    /* clear colour, depth and, if enabled, accumulation buffers */
    mav_gfxClearCZ();
    if (mav_opt_accumBuf) mav_gfxClearA();
    mav_surfaceParamsUndefine();
    
    /* calculate view matrix orientation */
    vp= mav_win_current->vp;
    
    /* dont reply on view up being exact */
    vp->right= mav_vectorNormalize(mav_vectorCrossProduct(vp->view, vp->up));
    vp->up= mav_vectorNormalize(mav_vectorCrossProduct(vp->right, vp->view));

    /* calc translated view parameters */
    if (vp->mod)
    {
      (*(vp->mod))(w);
    }
    else
    {
      mav_viewParamsFixed(w);
    }

    /* calc stereo view parameters */
    if (w->mod)
    {
      (*(w->mod))(w);
    }
    else
    {
      mav_eyeMono(w);
    }

    /* define and load the orientation part of the matrix */

    view_matrix.mat[0][0]= w->right.x;
    view_matrix.mat[0][1]= w->right.y;
    view_matrix.mat[0][2]= w->right.z;
    view_matrix.mat[0][3]= 0;

    view_matrix.mat[1][0]= w->up.x;
    view_matrix.mat[1][1]= w->up.y;
    view_matrix.mat[1][2]= w->up.z;
    view_matrix.mat[1][3]= 0;

    view_matrix.mat[2][0]= -w->view.x;
    view_matrix.mat[2][1]= -w->view.y;
    view_matrix.mat[2][2]= -w->view.z;
    view_matrix.mat[2][3]= 0.0;

    view_matrix.mat[3][0]= 0;
    view_matrix.mat[3][1]= 0;
    view_matrix.mat[3][2]= 0;
    view_matrix.mat[3][3]= 1.0;

    mav_gfxMatrixLoad(view_matrix);

    /* account for eye position */ 

    view_shift= w->eye;
    mav_gfxMatrixTranslate(mav_vectorScalar(view_shift, -1.0));

    /* store resulting view and proj.view matrices  */

    w->viewMat= mav_gfxMatrixGet();
    w->pdvMat= mav_matrixMult(w->projMat, w->viewMat);

    /* store the 5 ncp vertices */
    if (w->orthogonal)
    {
      vdist= 0.5*w->ortho_size;
      hdist= vdist*w->aspect;
    } 
    else 
    {
      vdist= tan(w->fov*0.5*MAV_PI_OVER_180)*w->ncp;
      hdist= vdist*w->aspect;
    }

    w->ncpv[4]= mav_vectorAdd(w->eye, mav_vectorScalar(w->view, w->ncp));
    w->ncpv[0]= mav_vectorAdd(w->ncpv[4], mav_vectorAdd(mav_vectorScalar(w->right, -hdist), mav_vectorScalar(w->up, -vdist)));
    w->ncpv[1]= mav_vectorAdd(w->ncpv[4], mav_vectorAdd(mav_vectorScalar(w->right, +hdist), mav_vectorScalar(w->up, -vdist)));
    w->ncpv[2]= mav_vectorAdd(w->ncpv[4], mav_vectorAdd(mav_vectorScalar(w->right, +hdist), mav_vectorScalar(w->up, +vdist)));
    w->ncpv[3]= mav_vectorAdd(w->ncpv[4], mav_vectorAdd(mav_vectorScalar(w->right, -hdist), mav_vectorScalar(w->up, +vdist)));

    /* store the 5 fcp vertices */
    if (!w->orthogonal) {
      vdist= tan(w->fov*0.5*MAV_PI_OVER_180)*w->fcp;
      hdist= vdist*w->aspect;
    }

    w->fcpv[4]= mav_vectorAdd(w->eye, mav_vectorScalar(w->view, w->fcp));
    w->fcpv[0]= mav_vectorAdd(w->fcpv[4], mav_vectorAdd(mav_vectorScalar(w->right, -hdist), mav_vectorScalar(w->up, -vdist)));
    w->fcpv[1]= mav_vectorAdd(w->fcpv[4], mav_vectorAdd(mav_vectorScalar(w->right, +hdist), mav_vectorScalar(w->up, -vdist)));
    w->fcpv[2]= mav_vectorAdd(w->fcpv[4], mav_vectorAdd(mav_vectorScalar(w->right, +hdist), mav_vectorScalar(w->up, +vdist)));
    w->fcpv[3]= mav_vectorAdd(w->fcpv[4], mav_vectorAdd(mav_vectorScalar(w->right, -hdist), mav_vectorScalar(w->up, +vdist)));
  }

  if (mav_win_current!=orig_win) mav_windowSet(orig_win);  

  /* Calc world pos of devices */

  mav_deviceCalc();

  /* Update positions for absolute lights */

  mavlib_lightPosFix();

  /* Do frame 2 functions */

  mav_listPointerReset(mavlib_frame2_list);
  while (mav_listItemsNext(mavlib_frame2_list, (void **) &fn, &d)) (*fn)(d);
}



/* Routine to define the end of the frame */

void mav_frameEnd(void)
{
  MAV_window *w, *orig_win= mav_win_current;
  MAV_frameFn fn;
  void *d;

  /* Do frame 3 functions */

  mav_listPointerReset(mavlib_frame3_list);  
  while (mav_listItemsNext(mavlib_frame3_list, (void **) &fn, &d)) (*fn)(d);

  /* flush and/or finish (if applicable) to sync state */

  if (mav_opt_flush || mav_opt_finish) {
    mav_listPointerReset(mav_win_list);
    while (mav_listItemNext(mav_win_list, (void **) &w)) {
      if (w!=mav_win_current) mav_windowSet(w);    
      if (mav_opt_flush) mav_gfxFlush();
      if (mav_opt_finish) mav_gfxFinish();
    }
  }

  /* swap the buffers for each window */

  mav_listPointerReset(mav_win_list);

  while (mav_listItemNext(mav_win_list, (void **) &w)) {
    if (w!=mav_win_current) mav_windowSet(w);    
    mav_gfxWindowBuffersSwap();
  }

  if (mav_win_current!=orig_win) mav_windowSet(orig_win);

  /* calculate instant frame rate */
  
  mav_timerStop(&mavlib_frameTimer);  
  mav_fps= 1.0/mavlib_frameTimer.wall;

  /* calculate averaged frame rate */

  mavlib_culTime+=mavlib_frameTimer.wall;
  mavlib_culFrame++;

  if (mavlib_culTime>1.0) {
    mav_fps_avg= mavlib_culFrame/mavlib_culTime;
    mavlib_culTime=0.0;
    mavlib_culFrame=0;
  }

  mav_firstFrame=MAV_FALSE;

  /* Do frame 4 functions */

  mav_listPointerReset(mavlib_frame4_list);  
  while (mav_listItemsNext(mavlib_frame4_list, (void **) &fn, &d)) (*fn)(d);

  /* Increment frame count */

  mav_frameCount++;
}



/* Routine to apply a null translation to the eye parameters */

void mav_viewParamsFixed(MAV_window *w)
{
  /* simply copy provided data into translated part */
  
  w->vp->trans_view= w->vp->view;
  w->vp->trans_up= w->vp->up;
  w->vp->trans_right= w->vp->right;
  w->vp->trans_eye= w->vp->eye;
}



/* Routine to print a set of view parameters */

void mav_viewParamsPrint(char *s, MAV_viewParams f)
{
  printf("%s", s);
  mav_vectorPrint("eye ", f.eye);
  mav_vectorPrint("view ", f.view);
  mav_vectorPrint("up ", f.up);
}



/* Routines to interpolate view parameters */

MAV_viewParams mav_viewParamsInterpolate(MAV_viewParams st, MAV_viewParams fi, float val)
{
  MAV_viewParams rv;
  MAV_matrix mst, mfi, m;
  MAV_quaternion qst, qfi, q;

  /* Copy over things like fixed up and modifier fns */
  rv= st;

  /* Dont reply on view up being exact */
  st.right= mav_vectorNormalize(mav_vectorCrossProduct(st.view, st.up));
  st.up= mav_vectorNormalize(mav_vectorCrossProduct(st.right, st.view));

  fi.right= mav_vectorNormalize(mav_vectorCrossProduct(fi.view, fi.up));
  fi.up= mav_vectorNormalize(mav_vectorCrossProduct(fi.right, fi.view));

  /* Make up a matrix for the start and end orientations */
  mst= MAV_ID_MATRIX;
  mst= mav_matrixXAxisSet(mst, st.right);
  mst= mav_matrixYAxisSet(mst, st.up);
  mst= mav_matrixZAxisSet(mst, mav_vectorScalar(st.view, -1));

  mfi= MAV_ID_MATRIX;
  mfi= mav_matrixXAxisSet(mfi, fi.right);
  mfi= mav_matrixYAxisSet(mfi, fi.up);
  mfi= mav_matrixZAxisSet(mfi, mav_vectorScalar(fi.view, -1));

  /* Convert matrices to quaternions and interpolate */
  qst= mav_quaternionMatrixConvert(mst);
  qfi= mav_quaternionMatrixConvert(mfi);
  q= mav_quaternionInterpolate(qst, qfi, val);

  /* Convert interpolated quaternion back into a matrix and read out vectors */
  m= mav_matrixQuaternionConvert(q);
  rv.right= mav_matrixXAxisGet(m);
  rv.up= mav_matrixYAxisGet(m);
  rv.view= mav_vectorScalar(mav_matrixZAxisGet(m), -1);

  /* Interpolate eye position */
  rv.eye= mav_vectorAdd(st.eye, mav_vectorScalar(mav_vectorSub(fi.eye, st.eye), val));

  return rv;
}



typedef struct {
  MAV_viewParams *vp;
  MAV_viewParams st;
  MAV_viewParams fi;
  float val;
  int style;
  int frame;
  MAV_timer timer;
  float dist;
  float totdist;
} MAVLIB_vpAnim;

void mavlib_vpAnim(void *vvpi)
{
  MAVLIB_vpAnim *vpi= (MAVLIB_vpAnim *) vvpi;
  float iv;

  if ((vpi->style & 15) == MAV_ANIMATE_TIME)
  {
    mav_timerStop(&vpi->timer);
    iv= vpi->timer.wall/vpi->val;
  }
  else if ((vpi->style & 15) == MAV_ANIMATE_FRAME)
  {
    vpi->frame++;
    iv= ((float) vpi->frame)/vpi->val;
  }
  else if ((vpi->style & 15) == MAV_ANIMATE_DISTANCE)
  {
    vpi->dist+= vpi->val;
    iv= vpi->dist/vpi->totdist;
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Unknown animation style, using time\n");
    mav_timerStop(&vpi->timer);
    iv= (vpi->timer.wall)/(vpi->val);
  }

  if (iv>=1.0)
  {
    *(vpi->vp)= vpi->fi;
    mav_frameFn1Rmv(mavlib_vpAnim, vpi);
    mav_free(vpi);
  }
  else
  {
    if (vpi->style & MAV_ANIMATE_S) {
#define fn(x) atan(((x)-0.5)*10)
      iv= (fn(iv)-fn(0))/(fn(1)-fn(0));
#undef fn
    }

    *(vpi->vp)= mav_viewParamsInterpolate(vpi->st, vpi->fi, iv);
  }
}

void mav_viewParamsAnimate(MAV_viewParams *vp, MAV_viewParams st, MAV_viewParams fi, float val, int style)
{
  if (val>=0)
  {
    MAVLIB_vpAnim *vpi= (MAVLIB_vpAnim *) mav_malloc(sizeof(MAVLIB_vpAnim));
  
    vpi->vp= vp;
    vpi->st= st;
    vpi->fi= fi;
    vpi->val= val;
    vpi->style= style;
    vpi->frame= 0;
    vpi->dist= 0;
    vpi->totdist= mav_vectorMag(mav_vectorSub(fi.eye, st.eye));
    mav_timerStart(&vpi->timer);

    mav_frameFn1Add(mavlib_vpAnim, vpi);
  }
  else
  {
    *vp= fi;
  }
}



/* Routine to display in one window another window's frustum */

void mavlib_frustumDisplayToAll(MAV_window *f)
{
  MAV_window *w;

  mav_listPointerReset(mav_win_list);
  
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_frustumDisplay(w,f);
}

void mav_frustumDisplay(MAV_window *w, MAV_window *f)
{
  MAV_surfaceParams sp;
  MAV_window *orig=mav_win_current;
  int i;
 
  if (w==mav_win_all) 
  {
    mavlib_frustumDisplayToAll(f);
  }
  else
  {
    if (w!=orig) mav_windowSet(w);
  
    sp.mode= MAV_COLOUR;
    sp.colour= MAV_COLOUR_BLACK;
    sp.material= 0;
    sp.texture= 0;
    mav_surfaceParamsUse(&sp);
    
    mav_gfxLineClosedBegin();
    mav_gfxVertex(f->ncpv[0]);
    mav_gfxVertex(f->ncpv[1]);
    mav_gfxVertex(f->ncpv[2]);
    mav_gfxVertex(f->ncpv[3]);
    mav_gfxLineClosedEnd();
    
    mav_gfxLineClosedBegin();
    mav_gfxVertex(f->fcpv[0]);
    mav_gfxVertex(f->fcpv[1]);
    mav_gfxVertex(f->fcpv[2]);
    mav_gfxVertex(f->fcpv[3]);
    mav_gfxLineClosedEnd();
    
    for (i=0; i<5; i++) {
      mav_gfxLineClosedBegin();
      mav_gfxVertex(f->ncpv[i]);
      mav_gfxVertex(f->fcpv[i]);
      mav_gfxLineClosedEnd();
    }
    
    if (w!=orig) mav_windowSet(orig);
  }
}
