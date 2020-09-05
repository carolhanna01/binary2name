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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
#include "maverik.h"
#include "mav_escapeavatar.h"
#include "city_macros.h"
#include "city_types.h"

#ifdef DISPLAY_LISTS
void Build_Display_Lists (void);
#endif
void Render_Background (float radius);
void Display_Objects (void);
void Redisplay_Objects (void);
void mav_initialiseLensFlare (void);
void mav_displayLensFlare (MAV_vector sunpos);
#ifdef COMPOSITES
MAV_object *Add_Composite_To_List (MAV_SMS *list, int type, MAV_matrix *transform);
#endif
void initTime();
double calcTime();
double mav_getTimer (void);

extern MAV_class *mav_class_avatar;
extern MAV_callback *mav_callback_enlist;
extern int size;
extern float block_width;
extern MAV_SMS *city_sms;
extern int bb_pos;
extern int current_texture;
extern int num_visible_objects;
extern int num_polys;
extern int num_cells;
extern int num_occluders;
extern MAV_cityCell *bounce_cell;
extern int wire;
extern double cull_time, render_time;
extern double other_time;
extern int routereader;
extern int routewriter;
#ifdef DISPLAY_LISTS
extern int background_display_list;
#endif

float fog_distance= 600.0;
MAV_viewParams vp;
MAV_viewModifierParams sp;
unsigned int current_frame= 0;
int write_anim;
int anim_frame;
int info= 0;
int max_rendered_cells= 400;
float zoom= 250.0;
int culling= 1;
#ifdef STEREO
int show_flare= 0;
#else
int show_flare= 0;
#endif

static int
Keyboard_Function
(MAV_object *obj, MAV_keyboardEvent *event)
{
  if (event->movement == MAV_PRESSED)
    {
      if (event->key == 'c')
	{
	  culling= !culling;
	  fprintf(stdout, "culling %d\n", culling);
	  return 1;
	}
      else if (event->key == '.')
	{
	  zoom += 50.0;
	  fprintf(stdout, "zoom= %f\n", zoom);
	  return 1;
	}
      else if (event->key == ',')
	{
	  zoom -= 50.0;
	  fprintf(stdout, "zoom= %f\n", zoom);
	  return 1;
	}
      else if (event->key == 'x')
	{
	  fog_distance += 100.0;
	  glFogf (GL_FOG_END, fog_distance);
	  fprintf(stdout, "%f\n", fog_distance);
	  return 1;
	}
      else if (event->key == 'z')
	{
	  fog_distance -= 100.0;
	  glFogf (GL_FOG_END, fog_distance);
	  fprintf(stdout, "%f\n", fog_distance);
	  return 1;
	}
      else if (event->key == 'w')
	{
	  wire= !wire;
	  return 1;
	}
      else if (event->key == 'a')
	{
	  write_anim= !write_anim;
	  if (write_anim) anim_frame= 0;
	  return 1;
	}
      else if (event->key == 'i')
	{
	  info= !info;
	  return 1;
	}
      else if (event->key == 'f')
	{
	  show_flare= !show_flare;
	  return 1;
	}
    }

  return 0;
}

#ifdef HMD
MAV_vector origin;

void
nav_red (void)
{
  MAV_vector distance;

  distance= mav_vectorSub (mav_red_TDM_pos.pos,origin);
  mav_moveFrustum (mav_current_window->frustum, MAV_RIGHT_FIXED_UP, 0.25*distance.x);
  mav_moveFrustum (mav_current_window->frustum, MAV_FORWARDS_FIXED_UP, -0.25*distance.z);
}

void
nav_blue (void)
{
  MAV_vector distance;

  distance= mav_vectorSub (mav_blue_TDM_pos.pos,origin);
  mav_moveFrustum (mav_current_window->frustum, MAV_FIXED_UP, 0.25*distance.y);
}

int
navigate_3d_red
(MAV_3DMouseEvent *event, MAV_object *obj)
{
  if (event->cause.movement == MAV_PRESSED)
    {
      fprintf(stdout, "Starting red...\n");
      mav_setStartFrame (nav_red);
      origin= event->pos[event->cause.mouse].pos;
    }
  else
    mav_deleteStartFrame (nav_red);
}

int
navigate_3d_blue
(MAV_3DMouseEvent *event, MAV_object *obj)
{
  if (event->cause.movement == MAV_PRESSED)
    {
      fprintf(stdout, "Starting blue...\n");
      mav_setStartFrame (nav_blue);
      origin= event->pos[event->cause.mouse].pos;
    }
  else
    mav_deleteStartFrame (nav_blue);
}
#endif

/* sms callback display function */
void
mav_SMSEnlistFn
(MAV_object *obj, MAV_drawInfo *di, void *params)
{
  mav_callbackExec (mav_callback_enlist, mav_win_current, obj, NULL, NULL);
}

int
mav_SMSEnlist
(MAV_window *w, MAV_SMS *sms)
{
  MAV_window *win, *orig_win= mav_win_current;
  MAV_SMSExecFn fn;
  MAV_drawInfo di;
  int rv=0;

  fn.fn= mav_SMSEnlistFn;
  fn.params= NULL;

  if (w == mav_win_all) 
    {
      mav_listPointerReset(mav_win_list);
      while (mav_listItemNext(mav_win_list, (void **) &win)) mav_SMSEnlist(win, sms);
    }
  else
    {
      if (w != orig_win) mav_windowSet (w);
      di.vp= *(mav_win_current->vp);      
      di.cp= mav_clipPlanesGet (w, -1.0,1.0, -1.0,1.0, w->ncp/w->fcp, 1.0);
      rv= mav_SMSCallbackExecFnExec (sms, &di, &fn);
      if (w != orig_win) mav_windowSet (orig_win);
    }
  
  return rv;
}

void
WalkAround (void)
{
  MAV_vector sunpos;
  char fname[100];
  MAV_SMS *list;
  int num_legoman;
  MAV_avatarCurves *curves;
  MAV_object *legoman[10];
  MAV_vector start[10];
  MAV_vector dir[10];
  float height[10];
  int frames[10];
  int num_avatars= 50;
  MAV_avatar *avatar[100];
  int follow[100];
  float time[100];
  MAV_vector dr;
  float d;
  float t;
  int i;
  double tm;
  GLfloat fogColour[4]= { 0.85, 0.85, 1.0, 1.0 };
  MAV_surfaceParams jumper;
  MAV_surfaceParams trousers;
  float rnd;
  FILE *rfile=NULL;
  MAV_timer tim;

  mav_windowBackgroundColourSet (mav_win_all, 0.5, 0.5, 0.5);
  mav_windowPerspectiveSet (mav_win_all, 0.25, 1500.0, 50.0, ((float ) mav_win_current->width)/mav_win_current->height);

#ifdef HMD
  mav_setMouseSensitivity (4.0, 0.01, 0.05);
  mav_setMouseScale (0.0);
  mav_set3DMouseMethod (mav_class_world, MAV_RED_TOP, navigate_3d_red);
  mav_set3DMouseMethod (mav_class_world, MAV_BLUE_TOP, navigate_3d_blue);
#else
  mav_navigationMouse (mav_win_all, mav_navigationMouseDefault);
  mav_navigationMouseDefaultParams (mav_win_all, MAV_LEFT_BUTTON,
			       mav_navigateYawFixedUp, 0.01, -0.001,
			       mav_navigateForwardsFixedUp, 0.01, 0.001);
  mav_navigationMouseDefaultParams (mav_win_all, MAV_MIDDLE_BUTTON,
			       mav_navigateYawFixedUp, 0.01, -0.001,
			       mav_navigatePitchFixedUp, 0.01, 0.001);
  mav_navigationMouseDefaultParams (mav_win_all, MAV_RIGHT_BUTTON,
			       mav_navigateRightFixedUp, 0.01, -0.001,
			       mav_navigateUpFixedUp, 0.01, 0.001);
#endif
  mav_callbackKeyboardSet (mav_win_all, mav_class_world, Keyboard_Function);

  sunpos.x= 1000.0;
  sunpos.y= 4000.0;
  sunpos.z= -10000.0;

  vp.eye.x= 43.0;
  vp.eye.y= 1.8;
  vp.eye.z= 28.0;
  vp.view.x= 0.65;
  vp.view.y= 0.0;
  vp.view.z= 0.75;
  vp.view= mav_vectorNormalize(vp.view);
  vp.up.x= 0.0;
  vp.up.y= 1.0;
  vp.up.z= 0.0;
  vp.fixed_up= vp.up;
#ifdef HMD
  vp.mod= mav_vp_hmd;
#else
  vp.mod= NULL;
#endif

  mav_windowViewParamsSet (mav_win_all, &vp);
#ifdef STEREO
  mav_stp_default.offset= 0.1;
#endif

  mav_paletteLightingModelSet (mav_palette_default, 0.2,0.2,0.2,0.0, 1.0);
  mav_paletteLightSet (mav_palette_default, 1, 0.3,0.3,0.3,0.0, 1.0,1.0,1.0,0.0, 0.0,0.0,0.0,0.0);
  mav_paletteLightSet (mav_palette_default, 2, 0.0,0.0,0.0,0.0, 0.5,0.5,0.5,0.0, 0.0,0.0,0.0,0.0);
  mav_paletteLightPos (mav_palette_default, 1, sunpos);

  mav_windowBackfaceCullSet (mav_win_all, MAV_TRUE);

#if 0
  if (0)
    mav_initialiseLensFlare ();
#endif

  mav_avatarInitialise ();
  curves= mav_avatarReadCurves ("./models/walking.cset");

#ifdef COMPOSITES
  list= mav_SMSObjListNew();

  num_legoman= 0;

  /*
   start[0].x= 18.0; start[0].y= PAVE_HEIGHT; start[0].z= 33.0;
     dir[0].x= 84.0;    dir[0].y= 0.0;    dir[0].z= 2.0;
  height[0]= 3.0;
  frames[0]= 250;
  tr= mav_matrixSet (0.0,0.0,0.0, start[0].x,start[0].y,start[0].z);
  legoman[0]= Add_Composite_To_List (list, COMP_LEGOMAN, &tr);

   start[1].x= 226.0; start[1].y= 20.0; start[1].z= 197.0;
     dir[1].x= 8.0;   dir[1].y= 0.0;    dir[1].z= 129.0;
  height[1]= 0.0;
  frames[1]= 800;
  tr= mav_matrixSet (0.0,0.0,0.0, start[1].x,start[1].y,start[1].z);
  legoman[1]= Add_Composite_To_List (list, COMP_RECOGNIZER, &tr);

   start[2].x= 73.0; start[2].y= PAVE_HEIGHT; start[2].z= 107.0;
     dir[2].x= 5.0;   dir[2].y= 0.0;    dir[2].z= 95.0;
  height[2]= 15.0;
  frames[2]= 1000;
  tr= mav_matrixSet (0.0,0.0,0.0, start[2].x,start[2].y,start[2].z);
  legoman[2]= Add_Composite_To_List (list, COMP_STANDINGMAN, &tr);

   start[3].x= 245.0; start[3].y= PAVE_HEIGHT; start[3].z= 133.0;
     dir[3].x= -16.0;   dir[3].y= 0.0;    dir[3].z= 0.0;
  height[3]= 1.0;
  frames[3]= 50;
  tr= mav_matrixSet (0.0,0.0,0.0, start[3].x,start[3].y,start[3].z);
  legoman[3]= Add_Composite_To_List (list, COMP_FISH, &tr);

   start[4].x= 298.0; start[4].y= PAVE_HEIGHT; start[4].z= 239.0;
     dir[4].x= 45.0;   dir[4].y= 0.0;    dir[4].z= 1.0;
  height[4]= 0.0;
  frames[4]= 700;
  tr= mav_matrixSet (0.0,0.0,0.0, start[4].x,start[4].y,start[4].z);
  legoman[4]= Add_Composite_To_List (list, COMP_WOMAN, &tr);

   start[5].x= 56.0; start[5].y= PAVE_HEIGHT; start[5].z= 348.0;
     dir[5].x= 0.0;   dir[5].y= 0.0;    dir[5].z= 0.0;
  height[5]= 0.0;
  frames[5]= 30;
  tr= mav_matrixSet (0.0,0.0,0.0, start[5].x,start[5].y,start[5].z);
  legoman[5]= Add_Composite_To_List (list, COMP_CAROSEL, &tr);
  */

  /* avatars */
  for (i= 0; i< num_avatars; i++)
    {
      rnd= mav_random();
      if (rnd < 1.0/9.0)
	{
	  jumper.mode= MAV_LIT_TEXTURE;
	  jumper.texture= TEX_JUMPER1;
	  jumper.material= MAT_JUMPER1;
	}
      else if (rnd < 2.0/9.0)
	{
	  jumper.mode= MAV_LIT_TEXTURE;
	  jumper.texture= TEX_JUMPER1;
	  jumper.material= MAT_JUMPER2;
	}
      else if (rnd < 3.0/9.0)
	{
	  jumper.mode= MAV_LIT_TEXTURE;
	  jumper.texture= TEX_JUMPER1;
	  jumper.material= MAT_JUMPER3;
	}
      else if (rnd < 4.0/9.0)
	{
	  jumper.mode= MAV_LIT_TEXTURE;
	  jumper.texture= TEX_JUMPER2;
	  jumper.material= MAT_JUMPER1;
	}
      else if (rnd < 5.0/9.0)
	{
	  jumper.mode= MAV_LIT_TEXTURE;
	  jumper.texture= TEX_JUMPER2;
	  jumper.material= MAT_JUMPER2;
	}
      else if (rnd < 6.0/9.0)
	{
	  jumper.mode= MAV_LIT_TEXTURE;
	  jumper.texture= TEX_JUMPER2;
	  jumper.material= MAT_JUMPER3;
	}
      else if (rnd < 7.0/9.0)
	{
	  jumper.mode= MAV_MATERIAL;
	  jumper.material= MAT_JUMPER1;
	}
      else if (rnd < 8.0/9.0)
	{
	  jumper.mode= MAV_MATERIAL;
	  jumper.material= MAT_JUMPER2;
	}
      else
	{
	  jumper.mode= MAV_MATERIAL;
	  jumper.material= MAT_JUMPER3;
	}

      rnd= mav_random();
      if (rnd < 1.0/9.0)
	{
	  trousers.mode= MAV_LIT_TEXTURE;
	  trousers.texture= TEX_TROUSERS1;
	  trousers.material= MAT_TROUSERS1;
	}
      else if (rnd < 2.0/9.0)
	{
	  trousers.mode= MAV_LIT_TEXTURE;
	  trousers.texture= TEX_TROUSERS1;
	  trousers.material= MAT_TROUSERS2;
	}
      else if (rnd < 3.0/9.0)
	{
	  trousers.mode= MAV_LIT_TEXTURE;
	  trousers.texture= TEX_TROUSERS1;
	  trousers.material= MAT_TROUSERS3;
	}
      else if (rnd < 4.0/9.0)
	{
	  trousers.mode= MAV_LIT_TEXTURE;
	  trousers.texture= TEX_TROUSERS2;
	  trousers.material= MAT_TROUSERS1;
	}
      else if (rnd < 5.0/9.0)
	{
	  trousers.mode= MAV_LIT_TEXTURE;
	  trousers.texture= TEX_TROUSERS2;
	  trousers.material= MAT_TROUSERS2;
	}
      else if (rnd < 6.0/9.0)
	{
	  trousers.mode= MAV_LIT_TEXTURE;
	  trousers.texture= TEX_TROUSERS2;
	  trousers.material= MAT_TROUSERS3;
	}
      else if (rnd < 7.0/9.0)
	{
	  trousers.mode= MAV_MATERIAL;
	  trousers.material= MAT_TROUSERS1;
	}
      else if (rnd < 8.0/9.0)
	{
	  trousers.mode= MAV_MATERIAL;
	  trousers.material= MAT_TROUSERS2;
	}
      else
	{
	  trousers.mode= MAV_MATERIAL;
	  trousers.material= MAT_TROUSERS3;
	}

      avatar[i]= mav_avatarBuild (curves, &jumper, &trousers);
      avatar[i]->offset= mav_random();
      follow[i]= i%2;

      mav_SMSObjectAdd (list, mav_objectNew (mav_class_avatar, avatar[i]));

      dr.x= 0.5-mav_random();
      dr.y= 0.0;
      dr.z= 0.5-mav_random();
      dr= mav_vectorNormalize(dr);
      mav_avatarSetVelocity (avatar[i], dr, 0.5+mav_random());
      avatar[i]->matrix= mav_matrixSet (0.0,0.0,0.0, mav_random()*400.0,PAVE_HEIGHT,mav_random()*400.0);

      time[i]= mav_random()*120.0;
    }
#endif

#ifdef DISPLAY_LISTS
  Render_Background (1000.0);
  Build_Display_Lists ();
#endif

  /* activate fog */
  mav_windowSet (mav_win_left);
  glFogi (GL_FOG_MODE, GL_LINEAR);
  glFogfv (GL_FOG_COLOR, fogColour);
  glFogf (GL_FOG_DENSITY, 0.01);
  glHint (GL_FOG_HINT, GL_FASTEST);
  glFogf (GL_FOG_START, 1.0);
  glFogf (GL_FOG_END, fog_distance);
  glEnable (GL_FOG);

#ifdef STEREO
  mav_windowSet (mav_win_right);
  glFogi (GL_FOG_MODE, GL_LINEAR);
  glFogfv (GL_FOG_COLOR, fogColour);
  glFogf (GL_FOG_DENSITY, 0.01);
  glHint (GL_FOG_HINT, GL_FASTEST);
  glFogf (GL_FOG_START, 1.0);
  glFogf (GL_FOG_END, fog_distance);
  glEnable (GL_FOG);
#endif

  if (routereader) {
    rfile= fopen("city_route1","r");
    if (!rfile) {
      printf("cant open route file\n");
      exit(1);
    }
    mav_timerStart(&tim);
  }

  current_frame= 0;
  while (1)
    {
#ifdef COMPOSITES
      for (i= 0; i< num_legoman; i++)
	{
	  t= fabs(((float)(current_frame%(2*frames[i])-(float)frames[i]))/(float)frames[i]);
	  ((MAV_composite*)(legoman[i]->the_data))->matrix= mav_matrixSet (0.0,0.0,360.0*(float)current_frame/(float)frames[i], start[i].x+t*dir[i].x,height[i]*fabs(sin(10.0*MAV_PI*current_frame/(float)frames[i])),start[i].z+t*dir[i].z);
	}

      tm= mav_getTimer();
      for (i= 0; i< num_avatars; i++)
	{
	  if (follow[i] && avatar[i]->drawn)
	    {
	      /* just drawn */
	      dr.x= mav_win_left->vp->eye.x-avatar[i]->matrix.mat[MAV_MATRIX_XCOMP];
	      dr.y= 0.0;
	      dr.z= mav_win_left->vp->eye.z-avatar[i]->matrix.mat[MAV_MATRIX_ZCOMP];
	      d= sqrt(dr.x*dr.x+dr.z*dr.z);
	      dr.x /= d;
	      dr.z /= d;

	      if (d < 2.0)
		{
		  mav_avatarSetVelocity (avatar[i], dr, 0.0);
		  time[i]= tm+10.0*mav_random();
		  follow[i]= 0;
		  follow[(int)(mav_random()*num_avatars)]= 1;
		}
	      else
		mav_avatarSetVelocity (avatar[i], dr, avatar[i]->speed);
	    }

	  avatar[i]->drawn= 0;

	  if (tm > time[i])
	    {
	      dr.x= 0.5-mav_random();
	      dr.y= 0.0;
	      dr.z= 0.5-mav_random();
	      dr= mav_vectorNormalize(dr);
	      mav_avatarSetVelocity (avatar[i], dr, 0.5+mav_random());
	      time[i]= tm+120.0*mav_random(); 
	    }
	  mav_avatarUpdatePosition (avatar[i]);
	}
#endif

      mav_eventsCheck ();

      if (routereader) {
	if (feof(rfile)) {
	  mav_timerStop(&tim);
	  mav_timerPrint("", tim);
	  printf("all over\n");
	  exit(1);
	}

	fscanf(rfile, "%*s %f %f %f", &mav_win_current->vp->eye.x, &mav_win_current->vp->eye.y, &mav_win_current->vp->eye.z);
	fscanf(rfile, "%*s %f %f %f", &mav_win_current->vp->view.x, &mav_win_current->vp->view.y, &mav_win_current->vp->view.z);
	fscanf(rfile, "%*s %f %f %f", &mav_win_current->vp->up.x, &mav_win_current->vp->up.y, &mav_win_current->vp->up.z);
      }

      mav_frameBegin ();

      if (routewriter) mav_viewParamsPrint("", *mav_win_current->vp);

      mav_paletteLightPos (mav_palette_default, 2, vp.eye);

      bb_pos= 0;
      num_visible_objects= 0;

      /* LEFT VIEW */
      mav_windowSet (mav_win_left);

      if (!wire)
	{
#ifdef DISPLAY_LISTS
#ifdef MAV_GL
	  callobj (background_display_list);
#else
	  glCallList (background_display_list);
#endif
#else
	  Render_Background (1000.0);
#endif
	}

      initTime();

      /* generate list of objects to be displayed */
      mav_SMSEnlist (mav_win_left, city_sms);

#ifdef COMPOSITES
      mav_SMSEnlist (mav_win_left, list);
#endif

      /* sort and display objects */
      Display_Objects ();

#if 0
      if (!wire && show_flare)
	mav_displayLensFlare (sunpos);
#endif

      if (info)
	fprintf(stdout, "%.2f culltime %.2f rendertime\n", cull_time, render_time);

#ifdef STEREO
      /* RIGHT VIEW */
      mav_windowSet (mav_win_right);

      if (!wire)
	{
#ifdef DISPLAY_LISTS
#ifdef MAV_GL
	  callobj (background_display_list);
#else
	  glCallList (background_display_list);
#endif
#else
	  Render_Background (1000.0);
#endif
	}

      Redisplay_Objects ();
#endif

      mav_frameEnd();

      if (write_anim)
	{
	  sprintf(fname, "/usr/tmp/movie/image%.4d.rgb", anim_frame++);
	  mav_windowDump (mav_win_left, fname);
	}

      current_frame++;
    }
}
