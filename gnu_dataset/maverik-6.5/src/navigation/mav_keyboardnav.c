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



/* Wrapper routine to set keyboard navigation callback */

void mav_navigationKeyboard(MAV_window *win, MAV_callbackKeyboardFn fn)
{
  /* function is set for mav_class_world */
  mav_callbackSysKeyboardSet(win, mav_class_world, fn);
}



/* Default key navigation */

char mavlib_keyNavName[][8]={"forward","back","left","right","up","down"};

/* Data structures to store navigation parameters */

MAV_window *mavlib_keyNavWin;
float mavlib_keyNavAmount[MAV_MAX_WIN];
float mavlib_keyNavLS[MAV_MAX_WIN];
float mavlib_keyNavAS[MAV_MAX_WIN];



/* Routine to actually perform the navigtaion every frame */

void mavlib_keyNav(int key)
{
  MAV_navigatorFn method=NULL;
  int wid, dir=1, alt=0;
  float amount;

  /* account for global win keyboard navigation parameters */
  if (mavlib_keyNavAmount[0]!=0.0) 
  {
    wid= 0;
  }
  else
  {
    wid= mavlib_keyNavWin->id;
  }

  /* find the amount of movement for navigation in this window */
  amount= mavlib_keyNavAmount[wid];

  /* check for shift and alt key modifiers */
  if (mav_keyboardGet(MAV_KEY_ALT_L)==MAV_PRESSED || mav_keyboardGet(MAV_KEY_ALT_R)==MAV_PRESSED) alt=1;
  if (mav_keyboardGet(MAV_KEY_SHIFT_L)==MAV_PRESSED || mav_keyboardGet(MAV_KEY_SHIFT_R)==MAV_PRESSED) amount*=2;

  /* determine navigator function to use for this key */
  switch (key) {
  case 0:
    method=mav_navigateForwardsFixedUp;
    dir=1;
    break;
  case 1:
    method=mav_navigateForwardsFixedUp;
    dir=-1;
    break;
  case 2:
    if (alt) 
    {
      method=mav_navigateRight;
      dir=-1;
    }
    else
    {
      method=mav_navigateYawFixedUp;
      dir=1;
    }
    break;
  case 3:
    if (alt) 
    {
      method=mav_navigateRight;
      dir=1;
    }
    else
    {
      method=mav_navigateYawFixedUp;
      dir=-1;
    }
    break;
  case 4:
    if (alt) 
    {
      method=mav_navigatePitch;
    }
    else
    {
      method=mav_navigateUpFixedUp;
    }
    dir=1;
    break;
  case 5:
    if (alt) 
    {
      method=mav_navigatePitch;
    }
    else
    {
      method=mav_navigateUpFixedUp;
    }
    dir=-1;
    break;
  }
  
  /* account for direction and the navigator */
  amount*=dir;
  mav_navigate(method, mavlib_keyNavWin->vp, amount, mavlib_keyNavLS[wid], mavlib_keyNavAS[wid]);
}

void mavlib_keyNavMove0(void *ignored)
{
  mavlib_keyNav(0);
}

void mavlib_keyNavMove1(void *ignored)
{
  mavlib_keyNav(1);
}

void mavlib_keyNavMove2(void *ignored)
{
  mavlib_keyNav(2);
}

void mavlib_keyNavMove3(void *ignored)
{
  mavlib_keyNav(3);
}

void mavlib_keyNavMove4(void *ignored)
{
  mavlib_keyNav(4);
}

void mavlib_keyNavMove5(void *ignored)
{
  mavlib_keyNav(5);
}

MAV_frameFn mavlib_keyNavMove[]={mavlib_keyNavMove0, mavlib_keyNavMove1, mavlib_keyNavMove2, mavlib_keyNavMove3, mavlib_keyNavMove4, mavlib_keyNavMove5};



/* Routine which gets called for keyboard navigation events */

int mav_navigationKeyboardDefault(MAV_object *o, MAV_keyboardEvent *ev)
{
  int key= ev->key-MAV_KEY_UP;
  int rv=0;

  if (key>=0 && key<=(MAV_KEY_PAGE_DOWN-MAV_KEY_UP)) {
    /* valid key for navigation */
    if (ev->movement==MAV_PRESSED) 
    {
      /* navigation activated */ 
      rv= !mav_opt_navPassEvents;
      mavlib_keyNavWin= ev->win;
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Keyboard navigation activated (%s key)\n", mavlib_keyNavName[key]);

      /* insert a function to be called every frame to implement navigation for this key */
      mav_frameFn0Add(mavlib_keyNavMove[key], NULL);

      /* keep a track of how many navigators are active for frame control purposes */
      mav_navigating++;
      mav_needFrameDraw++;
    }
    else
    {
      /* navigation deactivated */ 
      rv= !mav_opt_navPassEvents;
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Keyboard navigation deactivated (%s key)\n", mavlib_keyNavName[key]);

      /* remove start frame function */
      mav_frameFn0Rmv(mavlib_keyNavMove[key], NULL);

      /* keep a track of how many navigators are active */
      mav_navigating--;
      mav_needFrameDraw--;
    }
  }

  return rv;
}



/* Routine to customize keyboard navigation */

void mav_navigationKeyboardDefaultParams(MAV_window *w, float am, float ls, float as)
{
  /* store the parameters to be used for this window */

  mavlib_keyNavAmount[w->id]=am;
  mavlib_keyNavLS[w->id]=ls;
  mavlib_keyNavAS[w->id]=as;
}



/* Routine to initalise keyboard navigation */

void mavlib_keyboardNavigationInit(void)
{
  int i;

  for (i=0; i<MAV_MAX_WIN; i++) {
    mavlib_keyNavAmount[i]=0;
    mavlib_keyNavLS[i]=0;
    mavlib_keyNavAS[i]=0;
  }

  /* Set navigation parameters to some useful default */
  mav_navigationKeyboardDefaultParams(mav_win_all, 50, 0.001, 0.00005);
}
