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


#include "mavlib_navigation.h"
#include <stdio.h>



/* Wrapper routine to set mouse navigation callback */

void mav_navigationMouse(MAV_window *win, MAV_callbackMouseFn fn)
{
  /* function is set for mav_class_world */
  mav_callbackSysMouseSet(win, mav_class_world, fn);
}



/* Default mouse navigation */

char mavlib_mouseNavName[][7]={"left","middle","right"};

/* Data structure to store the parameters to use for each mouse button in each window */

typedef struct {
  int defined;
  MAV_navigatorFn x;
  float xls;
  float xas;
  MAV_navigatorFn y;
  float yls;
  float yas;
} MAVLIB_mouseNavParams;

MAVLIB_mouseNavParams mavlib_mouseNavParams[MAV_MAX_WIN][4];

/* Data structure to store which mouse button are actively begin used for navigation */ 

typedef struct {
  MAVLIB_mouseNavParams *params;
  MAV_window *win;
  int xorig;
  int yorig;
} MAVLIB_mouseNavButton;

MAVLIB_mouseNavButton mavlib_mouseNavButton[3];



/* Routine to actually perform the navigation every frame */

void mavlib_mouseNav(int but)
{
  float x, y;

  /* calculate amount mouse has moved from navigation origin */
  x= (mav_mouse_root_x-mavlib_mouseNavButton[but].xorig);
  y= -(mav_mouse_root_y-mavlib_mouseNavButton[but].yorig);
  
  /* call appropriate navigators */
  mav_navigate(mavlib_mouseNavButton[but].params->x, mavlib_mouseNavButton[but].win->vp, x,
	       mavlib_mouseNavButton[but].params->xls, mavlib_mouseNavButton[but].params->xas);
  mav_navigate(mavlib_mouseNavButton[but].params->y, mavlib_mouseNavButton[but].win->vp, y,
	       mavlib_mouseNavButton[but].params->yls, mavlib_mouseNavButton[but].params->yas);
}

void mavlib_mouseNavMove0(void *ignored)
{
  mavlib_mouseNav(0);
}

void mavlib_mouseNavMove1(void *ignored)
{
  mavlib_mouseNav(1);
}

void mavlib_mouseNavMove2(void *ignored)
{
  mavlib_mouseNav(2);
}

MAV_frameFn mavlib_mouseNavMove[]={mavlib_mouseNavMove0, mavlib_mouseNavMove1, mavlib_mouseNavMove2};



/* Routine which gets called for mouse navigation events */

int mav_navigationMouseDefault(MAV_object *obj, MAV_mouseEvent *ev)
{
  MAVLIB_mouseNavParams *params=NULL;
  int rv=1;

  /* Find which set of navigation parameters to use for this button 
     taking into account mav_win_all and mav_any_button precedences */

  if (mavlib_mouseNavParams[0][MAV_ANY_BUTTON].defined) {
    params= &mavlib_mouseNavParams[0][MAV_ANY_BUTTON];
  } else if (mavlib_mouseNavParams[0][ev->button].defined) {
    params= &mavlib_mouseNavParams[0][ev->button];
  } else if (mavlib_mouseNavParams[ev->win->id][MAV_ANY_BUTTON].defined) {
    params= &mavlib_mouseNavParams[ev->win->id][MAV_ANY_BUTTON];
  } else if (mavlib_mouseNavParams[ev->win->id][ev->button].defined) {
    params= &mavlib_mouseNavParams[ev->win->id][ev->button];
  } else {
    rv=0;
  }

  if (rv) {
    if (ev->movement==MAV_PRESSED)
    {
      /* navigation activated - store the data for this button */ 
      rv= !mav_opt_navPassEvents;
      mavlib_mouseNavButton[ev->button].params= params;
      mavlib_mouseNavButton[ev->button].win= ev->win;
      mavlib_mouseNavButton[ev->button].xorig= ev->root_x;
      mavlib_mouseNavButton[ev->button].yorig= ev->root_y;
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Mouse navigation activated (%s button)\n", mavlib_mouseNavName[ev->button]);

      /* insert a function to be called every frame to implement navigation for this button */
      mav_frameFn0Add(mavlib_mouseNavMove[ev->button], NULL);

      /* keep a track of how many navigators are active for frame control purposes */
      mav_navigating++;
      mav_needFrameDraw++;
    }
    else
    {
      /* navigation deactivated */
      rv= !mav_opt_navPassEvents;
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Mouse navigation deactivated (%s button)\n", mavlib_mouseNavName[ev->button]);
      
      /* remove start frame function */
      mav_frameFn0Rmv(mavlib_mouseNavMove[ev->button], NULL);

      /* keep a track of how many navigators are active */
      mav_navigating--;
      mav_needFrameDraw--;
    }
  }

  return rv;
}



/* Routine to customize mouse navigation */

void mav_navigationMouseDefaultParams(MAV_window *w, int but, MAV_navigatorFn x, float xls, float xas, MAV_navigatorFn y, float yls, float yas)
{
  /* store the parameters to be used for this button in this window */
  if (x && y) 
  {
    mavlib_mouseNavParams[w->id][but].defined= 1;
    mavlib_mouseNavParams[w->id][but].x= x;
    mavlib_mouseNavParams[w->id][but].xls= xls;
    mavlib_mouseNavParams[w->id][but].xas= xas;
    mavlib_mouseNavParams[w->id][but].y= y;
    mavlib_mouseNavParams[w->id][but].yls= yls;
    mavlib_mouseNavParams[w->id][but].yas= yas;
  }
  else
  {
    mavlib_mouseNavParams[w->id][but].defined= 0;
  }
}



extern float mavlib_keyNavLS[];
extern float mavlib_keyNavAmount[];

void mavlib_cf2(MAV_window *w)
{
  int i, j;

  for (i=0; i<MAV_MAX_WIN; i++) {
    
    if (mavlib_keyNavAmount[i]!=0.0) {
      mavlib_keyNavLS[i]*=0.9;
      printf("Keyboard - win %i linear navigation scaling factor now at %f\n", i, mavlib_keyNavLS[i]);
    }

    for (j=0; j<4; j++) {
      if (mavlib_mouseNavParams[i][j].defined) {
	mavlib_mouseNavParams[i][j].xls*=0.9;
	mavlib_mouseNavParams[i][j].yls*=0.9;
	printf("Mouse - win %i, button %i XY linear navigation scaling factor now at %f %f\n", i, j, mavlib_mouseNavParams[i][j].xls, mavlib_mouseNavParams[i][j].yls);
      }
    }
  }
}

void mavlib_cf3(MAV_window *w)
{
  int i, j;

  for (i=0; i<MAV_MAX_WIN; i++) {

    if (mavlib_keyNavAmount[i]!=0.0) {
      mavlib_keyNavLS[i]*=1.1;
      printf("Keyboard - win %i linear navigation scaling factor now at %f\n", i, mavlib_keyNavLS[i]);
    }

    for (j=0; j<4; j++) {
      if (mavlib_mouseNavParams[i][j].defined) {
	mavlib_mouseNavParams[i][j].xls*=1.1;
	mavlib_mouseNavParams[i][j].yls*=1.1;
	printf("Win %i, button %i XY linear navigation scaling factor now at %f %f\n", i, j, mavlib_mouseNavParams[i][j].xls, mavlib_mouseNavParams[i][j].yls);
      }
    }
  }
}



/* Routine to initalise mouse navigation */

void mavlib_mouseNavigationInit(void)
{
  int i,j;

  /* Initialise navigation parameters */
  for (i=0; i<MAV_MAX_WIN; i++) {
    for (j=0; j<4; j++) mavlib_mouseNavParams[i][j].defined= 0;
  }

  mav_ctrlF[2]= mavlib_cf2;
  mav_ctrlF_desc[2]= "Ctrl-F2 decrease linear navigation scaling factor by 10%";

  mav_ctrlF[3]= mavlib_cf3;
  mav_ctrlF_desc[3]= "Ctrl-F3 increase linear navigation scaling factor by 10%";

  /* Set navigation parameters to some useful default */

  mav_navigationMouseDefaultParams(mav_win_all, MAV_LEFT_BUTTON, mav_navigateYawFixedUp, 0.001, -0.00005,
                              mav_navigateForwardsFixedUp, 0.001, 0.00005);
  mav_navigationMouseDefaultParams(mav_win_all, MAV_RIGHT_BUTTON, mav_navigateRight, 0.001, 0.00005,
                              mav_navigateUp, 0.001, 0.00005);
}
