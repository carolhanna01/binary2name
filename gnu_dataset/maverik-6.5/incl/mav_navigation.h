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

#ifndef _MAV_NAVIGATION_INCLUDE
#define _MAV_NAVIGATION_INCLUDE

#ifdef MAVAPI
#undef MAVAPI
#endif
#if defined(WIN32) && !defined(__CYGWIN__) 
#ifdef LIBMAVERIK_EXPORTS
#define MAVAPI __declspec(dllexport) 
#else
#define MAVAPI __declspec(dllimport) 
#endif
#else
#define MAVAPI
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Navigators */

typedef void (*MAV_navigatorFn)(MAV_viewParams *, float, float, float);

MAVAPI void mav_navigate(MAV_navigatorFn fn, MAV_viewParams *vp, float am, float ls, float as);

MAVAPI void mav_navigateNull(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateTransX(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateTransY(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateTransZ(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateRotRight(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateRotUp(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateRotFixedUp(MAV_viewParams *vp, float amount, float ls, float as);
MAVAPI void mav_navigateForwards(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateUp(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateUpFixedUp(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateRight(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateForwardsFixedUp(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateTransForwardsFixedUp(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateRightFixedUp(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateRoll(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigatePitch(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateYaw(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigateYawFixedUp(MAV_viewParams *vp, float am, float ls, float as);
MAVAPI void mav_navigatePitchFixedUp(MAV_viewParams *vp, float am, float ls, float as);



/* Mouse and keyboard navigation */

MAVAPI void mav_navigationMouse(MAV_window *w, MAV_callbackMouseFn fn);
MAVAPI int  mav_navigationMouseDefault(MAV_object *obj, MAV_mouseEvent *ev);
MAVAPI void mav_navigationMouseDefaultParams(MAV_window *w, int but, MAV_navigatorFn x, float xls, float xas, 
					     MAV_navigatorFn y, float yls, float yas);

MAVAPI void mav_navigationKeyboard(MAV_window *w, MAV_callbackKeyboardFn fn);
MAVAPI int  mav_navigationKeyboardDefault(MAV_object *obj, MAV_keyboardEvent *ke);
MAVAPI void mav_navigationKeyboardDefaultParams(MAV_window *w, float am, float ls, float as);

MAVAPI extern int mav_navigating;
MAVAPI extern int mav_opt_navPassEvents;
MAVAPI extern MAV_vector mav_nav_center;



/* Module initialise */

MAVAPI int mav_navigationModuleInit(void);
MAVAPI char *mav_navigationModuleID(void);

#ifdef __cplusplus
}
#endif
#endif
