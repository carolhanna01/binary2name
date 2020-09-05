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

int mav_navigating;
int mav_opt_navPassEvents= MAV_TRUE;
MAV_vector mav_nav_center={0,0,0};



/* Routines to initialise the navigation module */

char *mav_navigationModuleID(void)
{
  return "Navigation";
}

int mav_navigationModuleInit(void)
{
  /* add the new module */
  mav_moduleNew(mav_navigationModuleID);

  /* initialise mouse and keyboard navigation */
  mavlib_mouseNavigationInit();
  mavlib_keyboardNavigationInit();

  mav_navigating=0;

  return 1;
}



/* Routine to execute a navigator function on a set of view parameters */

void mav_navigate(MAV_navigatorFn fn, MAV_viewParams *vp, float amount, float ls, float as)
{
  (*fn)(vp, amount, ls, as);
}
