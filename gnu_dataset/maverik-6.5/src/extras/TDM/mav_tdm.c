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


#include "mavlib_tdm.h"
#include <stdio.h>
#include <math.h>

#ifdef MAV_TDM
#include <dlfcn.h>
MAV_TDMPos mav_TDM_pos[TDM_MAX_TRACKERS];
MAV_matrix mav_TDM_matrix[TDM_MAX_TRACKERS];
typedef int  (*MAVLIB_TDMInitFn)(void);
typedef void (*MAVLIB_TDMGetPositionFn)(int, TDM_position *);
typedef void (*MAVLIB_TDMGetButtonsFn)(int [TDM_MAX_TRACKERS][TDM_MAX_BUTTONS]);
typedef int  (*MAVLIB_TDMGetEventFn)(TDM_buttonEvent *);
MAVLIB_TDMInitFn mavlib_TDMInitFn=NULL;
MAVLIB_TDMGetPositionFn mavlib_TDMGetPositionFn=NULL;
MAVLIB_TDMGetButtonsFn mavlib_TDMGetButtonsFn=NULL;
MAVLIB_TDMGetEventFn mavlib_TDMGetEventFn=NULL;
#else
MAV_TDMPos mav_TDM_pos[4];
MAV_matrix mav_TDM_matrix[4];
#endif
char *mav_opt_TDMLib= NULL;

int mavlib_TDM= MAV_FALSE;

#ifdef MAV_TDM
float mavlib_TDM_xorigin[TDM_MAX_TRACKERS];
float mavlib_TDM_yorigin[TDM_MAX_TRACKERS];
float mavlib_TDM_zorigin[TDM_MAX_TRACKERS];
float mavlib_TDM_xscale[TDM_MAX_TRACKERS];
float mavlib_TDM_yscale[TDM_MAX_TRACKERS];
float mavlib_TDM_zscale[TDM_MAX_TRACKERS];
float mavlib_TDM_offset[TDM_MAX_TRACKERS];
float mavlib_TDM_drawScale[TDM_MAX_TRACKERS];
#else
float mavlib_TDM_xorigin[4];
float mavlib_TDM_yorigin[4];
float mavlib_TDM_zorigin[4];
float mavlib_TDM_xscale[4];
float mavlib_TDM_yscale[4];
float mavlib_TDM_zscale[4];
float mavlib_TDM_offset[4];
float mavlib_TDM_drawScale[4];
#endif

/* Routine to copy between the different data structures */

#ifdef MAV_TDM
void mavlib_tdm2mav(MAV_TDMPos *pos, TDM_position tdmpos)
{
  /* Copy the position */
  pos->pos.x= tdmpos.xpos;
  pos->pos.y= tdmpos.ypos;
  pos->pos.z= tdmpos.zpos;

  /* Copy the axes */
  pos->u.x= tdmpos.u[0];
  pos->u.y= tdmpos.u[1];
  pos->u.z= tdmpos.u[2];

  pos->v.x= tdmpos.v[0];
  pos->v.y= tdmpos.v[1];
  pos->v.z= tdmpos.v[2];

  pos->n.x= tdmpos.n[0];
  pos->n.y= tdmpos.n[1];
  pos->n.z= tdmpos.n[2];

  /* Make an orientation matrix */
  pos->matrix.mat[0][0]= pos->u.x;
  pos->matrix.mat[1][0]= pos->u.y;
  pos->matrix.mat[2][0]= pos->u.z;
  pos->matrix.mat[3][0]= 0.0;

  pos->matrix.mat[0][1]= pos->v.x;
  pos->matrix.mat[1][1]= pos->v.y;
  pos->matrix.mat[2][1]= pos->v.z;
  pos->matrix.mat[3][1]= 0.0;

  pos->matrix.mat[0][2]= pos->n.x;
  pos->matrix.mat[1][2]= pos->n.y;
  pos->matrix.mat[2][2]= pos->n.z;
  pos->matrix.mat[3][2]= 0.0;

  pos->matrix.mat[0][3]= pos->pos.x;
  pos->matrix.mat[1][3]= pos->pos.y;
  pos->matrix.mat[2][3]= pos->pos.z;
  pos->matrix.mat[3][3]= 1.0;

  /* Make a quaternion */
  pos->quaternion= mav_quaternionMatrixConvert(pos->matrix);

  /* Copy status */
  pos->status= tdmpos.status;
}
#endif



/* Routine to get a trackers button status */

int mav_TDMButtonQuery(int tracker, int button)
{
#ifdef MAV_TDM
  int bs[TDM_MAX_TRACKERS][TDM_MAX_BUTTONS];
  int rv;

  if (tracker>TDM_MAX_TRACKERS-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: tracker not valid\n");
    rv=MAV_RELEASED;
  } 
  else if (button>TDM_MAX_BUTTONS-1)
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: button not valid\n");
    rv=MAV_RELEASED;
  }
  else if (mavlib_TDM) 
  {
    /* Get TDM button status */
    (*mavlib_TDMGetButtonsFn)(bs);
    
    rv= !bs[tracker][button]; /* NB TDM reports pressed=1, the opposite to Maverik and X */
  }
  else
  {    
    /* Dummy values if TDM not available */
    rv= MAV_RELEASED;
  }

  return rv;
#else
  return MAV_RELEASED;
#endif
}



/* Routine to get a trackers position */

MAV_TDMPos mav_TDMPosQuery(int tracker)
{
  MAV_TDMPos rv;
  
  /* Fill in with a default set of values */
  rv.pos.x= mavlib_TDM_xorigin[tracker];
  rv.pos.y= mavlib_TDM_yorigin[tracker];
  rv.pos.z= mavlib_TDM_zorigin[tracker];

  rv.u.x=1;
  rv.u.y=0;
  rv.u.z=0;

  rv.v.x=0;
  rv.v.y=1;
  rv.v.z=0;

  rv.n.x=0;
  rv.n.y=0;
  rv.n.z=1;

  rv.quaternion= MAV_ID_QUATERNION;
  rv.matrix= mav_matrixXYZSet(MAV_ID_MATRIX, rv.pos);

  rv.status= 0;

  /* Override them with real values if we can */
#ifdef MAV_TDM
  if (tracker>TDM_MAX_TRACKERS-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: tracker not valid\n");
  } 
  else if (mavlib_TDM) 
  {  
    TDM_position tdmpos;

    /* Get TDM position */
    (*mavlib_TDMGetPositionFn)(tracker, &tdmpos);

    /* Convert between different structures */
    mavlib_tdm2mav(&rv, tdmpos);
  }
#endif

  return rv;
}



/* Routine to poll for the trackers positions */

void mavlib_TDM_poll(void)
{
  int i;
#if MAV_TDM
  for (i=0; i<TDM_MAX_TRACKERS; i++) mav_TDM_pos[i]= mav_TDMPosQuery(i);
#else
  for (i=0; i<4; i++) mav_TDM_pos[i]= mav_TDMPosQuery(i);
#endif
}



/* Routines to calculate the trackers world positions */

MAV_matrix mavlib_TDM_calcPos(int tk, MAV_TDMPos trk, MAV_matrix iv)
{
  MAV_vector shift;
  MAV_matrix rv;

  /* Calc trackers position, relative to eye, taking into account origin and scale */
  shift.x=(trk.pos.x-mavlib_TDM_xorigin[tk])*mavlib_TDM_xscale[tk];
  shift.y=(trk.pos.y-mavlib_TDM_yorigin[tk])*mavlib_TDM_yscale[tk];
  shift.z=(trk.pos.z-mavlib_TDM_zorigin[tk])*mavlib_TDM_zscale[tk];

  /* Give a bit of z shift to move the tracker origin away from the eye */
  shift.z-=mavlib_TDM_offset[tk];

  /* Calculate trackers orientation */
  rv= mav_matrixMult(iv, mav_matrixXYZSet(trk.matrix, shift));

  return rv;
}

MAV_matrix mavlib_TDM_iv(void)
{
#if 0
  /* Calculate the inverse of the view matrix for the *modified* view vectors, ie. trans_up ... */
  return (mav_matrixInverse(mav_gfxMatrixGet()));
#else
  /* Calculate the inverse of the view matrix for the un-modified view vectors, ie. up ... */
  MAV_viewParams *vp;
  MAV_matrix iv, tr;

  vp= mav_win_current->vp;

  iv.mat[0][0]= vp->right.x;
  iv.mat[0][1]= vp->right.y;
  iv.mat[0][2]= vp->right.z;
  iv.mat[0][3]= 0.0;

  iv.mat[1][0]= vp->up.x;
  iv.mat[1][1]= vp->up.y;
  iv.mat[1][2]= vp->up.z;
  iv.mat[1][3]= 0.0;

  iv.mat[2][0]= -vp->view.x;
  iv.mat[2][1]= -vp->view.y;
  iv.mat[2][2]= -vp->view.z;
  iv.mat[2][3]= 0.0;

  iv.mat[3][0]= 0.0;
  iv.mat[3][1]= 0.0;
  iv.mat[3][2]= 0.0;
  iv.mat[3][3]= 1.0;

  tr= mav_matrixXYZSet(MAV_ID_MATRIX, mav_vectorScalar(vp->eye, -1.0));
  iv= mav_matrixMult(iv, tr);

  return mav_matrixInverse(iv);
#endif
}

void mavlib_TDM_calc(void)
{
  MAV_matrix iv;
  int i;

  if (mav_win_current && mav_win_current->vp) {
    
    /* Calculate inverse of view matrix */
    iv= mavlib_TDM_iv();

    /* For each tracker */
#ifdef MAV_TDM
    for (i=0; i<TDM_MAX_TRACKERS; i++) mav_TDM_matrix[i]= mavlib_TDM_calcPos(i, mav_TDM_pos[i], iv);
#else
    for (i=0; i<4; i++) mav_TDM_matrix[i]= mavlib_TDM_calcPos(i, mav_TDM_pos[i], iv);
#endif
  }
}



/* Routine to check for, and act upon, tracker event */

int mavlib_TDM_event(void)
{
  int rv=0;

#ifdef MAV_TDM
  if (mavlib_TDM) {
    TDM_buttonEvent mavlib_tdm_event;
    if ((*mavlib_TDMGetEventFn)(&mavlib_tdm_event)) rv= mavlib_dealWithTDMEvent(&mavlib_tdm_event);
  }
#endif

  return rv;
}



/* Routines to initialise the module */

char *mav_TDMModuleID(void)
{
  return "TDM";
}

int mav_TDMModuleInit(void)
{
  int i;
#ifdef MAV_TDM
  int pinit=1;
#endif

  /* Add the new module */
  mav_moduleNew(mav_TDMModuleID);

  mavlib_TDM= MAV_FALSE;

#ifdef MAV_TDM
#if 0
  /* Statically linked library case could do something like this */
  mavlib_TDMInitFn= TDM_init;
  mavlib_TDMGetPositionFn= TDM_getPosition;
  mavlib_TDMGetButtonsFn= TDM_getButtons;
  mavlib_TDMGetEventFn= TDM_getEvent;
#else
  /* Dynamic loaded library case */
  if (mav_opt_TDMLib)
  {
    void *dlhnd;

    /* Open library */
#ifdef MAV_SUNOS4
    dlhnd= dlopen(mav_opt_TDMLib, 1);
#else
    dlhnd= dlopen(mav_opt_TDMLib, RTLD_NOW);
#endif  
    
    if (dlhnd)
    {
      /* Get symbols */
      mavlib_TDMInitFn= (MAVLIB_TDMInitFn) dlsym(dlhnd, "TDM_init");

      if (mavlib_TDMInitFn)
      {
	mavlib_TDMGetPositionFn= (MAVLIB_TDMGetPositionFn) dlsym(dlhnd, "TDM_getPosition");

	if (mavlib_TDMGetPositionFn)
	{
	  mavlib_TDMGetButtonsFn= (MAVLIB_TDMGetButtonsFn) dlsym(dlhnd, "TDM_getButtons");
	  
	  if (mavlib_TDMGetButtonsFn)
	  {
	    mavlib_TDMGetEventFn= (MAVLIB_TDMGetEventFn) dlsym(dlhnd, "TDM_getEvent");

	    if (!mavlib_TDMGetEventFn) {
	      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: could not find TDM_getEvent function in %s, ignoring\n", mav_opt_TDMLib);
	      pinit=0;
	    }
	  }
	  else
	  {
	    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: could not find TDM_getButtons function in %s, ignoring\n", mav_opt_TDMLib);
	    pinit=0;
	  }
	}
	else
	{
	  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: could not find TDM_getPosition function in %s, ignoring\n", mav_opt_TDMLib);
	  pinit=0;
	}
      }
      else
      {
	if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: could not find TDM_init function in %s, ignoring\n", mav_opt_TDMLib);
	pinit=0;
      }
    }
    else
    {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: failed to dlopen TDM library %s, ignoring\n", mav_opt_TDMLib);
      pinit=0;
    }
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: TDM library to dynamically load not specified, ignoring\n");
    pinit=0;
  }
#endif

  /* Connect to TDM */
  if (pinit && (*mavlib_TDMInitFn)()) mavlib_TDM= MAV_TRUE;

  if (mavlib_TDM==MAV_FALSE) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: TDM initialisation failed, ignoring\n");
  }
#else
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: code not compiled with TDM option, ignoring\n");
#endif

  /* Add the trackers as new devices */
  mav_deviceNew(mavlib_TDM_poll, mavlib_TDM_calc, mavlib_TDM_event);  

  /* Define new callbacks for events */
  mav_callback_TDM= mav_callbackNew();
  mav_callback_sysTDM= mav_callbackNew();

  /* Define new cursor object */
  mav_class_TDMCursor= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_TDMCursor, mav_TDMCursorDraw);
  mav_callbackBBSet(mav_win_all, mav_class_TDMCursor, mav_TDMCursorBB);
  mav_callbackIDSet(mav_win_all, mav_class_TDMCursor, mav_TDMCursorID);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_TDMCursor, mav_TDMCursorGetUserdef);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_TDMCursor, mav_TDMCursorGetSurfaceParams);
  mav_callbackDumpSet(mav_win_all, mav_class_TDMCursor, mav_TDMCursorDump);

  /* Initialise the cursor shapes */
  mavlib_TDM_cursorInit();

  /* Set defaults */
#ifdef MAV_TDM
  for (i=0; i<TDM_MAX_TRACKERS; i++) {
#else
  for (i=0; i<4; i++) {
#endif
    mavlib_TDM_xorigin[i]= 0.0;
    mavlib_TDM_yorigin[i]= 0.0;
    mavlib_TDM_zorigin[i]= 20.0;
    mavlib_TDM_xscale[i]= 1.0;
    mavlib_TDM_yscale[i]= 1.0;
    mavlib_TDM_zscale[i]= 1.0;
    mavlib_TDM_offset[i]= 20.0;
    mavlib_TDM_drawScale[i]= 1.0;
  }

  return mavlib_TDM;
}



/* Routines to set TDM origin and scale */

void mav_TDMXYZScaleSet(float x, float y, float z)
{
  int i;

#ifdef MAV_TDM
  for (i=0; i<TDM_MAX_TRACKERS; i++) {
#else
  for (i=0; i<4; i++) {
#endif
    mavlib_TDM_xscale[i]= x;
    mavlib_TDM_yscale[i]= y;
    mavlib_TDM_zscale[i]= z;
  }
}

void mav_TDMSingleXYZScaleSet(int trk, float x, float y, float z)
{
  mavlib_TDM_xscale[trk]= x;
  mavlib_TDM_yscale[trk]= y;
  mavlib_TDM_zscale[trk]= z;
}

void mav_TDMXYZOriginSet(float x, float y, float z)
{
  int i;

#ifdef MAV_TDM
  for (i=0; i<TDM_MAX_TRACKERS; i++) {
#else
  for (i=0; i<4; i++) {
#endif
    mavlib_TDM_xorigin[i]= x;
    mavlib_TDM_yorigin[i]= y;
    mavlib_TDM_zorigin[i]= z;
  }
}

void mav_TDMSingleXYZOriginSet(int trk, float x, float y, float z)
{
  mavlib_TDM_xorigin[trk]= x;
  mavlib_TDM_yorigin[trk]= y;
  mavlib_TDM_zorigin[trk]= z;
}

void mav_TDMOffsetSet(float off)
{
  int i;

#ifdef MAV_TDM
  for (i=0; i<TDM_MAX_TRACKERS; i++) {
#else
  for (i=0; i<4; i++) {
#endif
    mavlib_TDM_offset[i]= off;
  }
}

void mav_TDMSingleOffsetSet(int trk, float off)
{
  mavlib_TDM_offset[trk]= off;
}

void mav_TDMDrawScaleSet(float sc)
{
  int i;

#ifdef MAV_TDM
  for (i=0; i<TDM_MAX_TRACKERS; i++) {
#else
  for (i=0; i<4; i++) {
#endif
    mavlib_TDM_drawScale[i]= sc;
  }
}

void mav_TDMSingleDrawScaleSet(int trk, float sc)
{
  mavlib_TDM_drawScale[trk]= sc;
}

void mav_TDMScaleSet(float sc)
{
  int i;

#ifdef MAV_TDM
  for (i=0; i<TDM_MAX_TRACKERS; i++) {
#else
  for (i=0; i<4; i++) {
#endif
    mavlib_TDM_xscale[i]= sc;
    mavlib_TDM_yscale[i]= sc;
    mavlib_TDM_zscale[i]= sc;
    mavlib_TDM_drawScale[i]= sc;
    mavlib_TDM_offset[i]= 20.0*sc;
  }
}

void mav_TDMSingleScaleSet(int trk, float sc)
{
  mavlib_TDM_xscale[trk]= sc;
  mavlib_TDM_yscale[trk]= sc;
  mavlib_TDM_zscale[trk]= sc;
  mavlib_TDM_drawScale[trk]= sc;
  mavlib_TDM_offset[trk]= 20.0*sc;
}
