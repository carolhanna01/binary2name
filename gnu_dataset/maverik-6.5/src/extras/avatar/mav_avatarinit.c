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


#include "maverik.h"
#include "mavlib_avatar.h"
#include <stdlib.h>

MAVLIB_avatarCurves *mavlib_avatarCurve1;
extern MAV_list *mavlib_avatarList;
void mavlib_avatarUpdateFn(void *ignored);



/* Routines to initialise the module */

char *mav_avatarModuleID(void)
{
  return "Avatar";
}

int mav_avatarModuleInit(void)
{
  /* Add the new module */
  mav_moduleNew(mav_avatarModuleID);

  /* Create new object class and set callbacks */
  mav_class_avatar= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_avatar, mav_avatarDraw);
  mav_callbackBBSet(mav_win_all, mav_class_avatar, mav_avatarBB2);
  mav_callbackIntersectSet(mav_win_all, mav_class_avatar, mav_avatarIntersect);
  mav_callbackIDSet(mav_win_all, mav_class_avatar, mav_avatarID);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_avatar, mav_avatarGetUserdef);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_avatar, mav_avatarGetMatrix);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_avatar, mav_avatarGetSurfaceParams);
  mav_callbackDumpSet(mav_win_all, mav_class_avatar, mav_avatarDump);

  /* Read in avatar animation curves */
  mavlib_avatarCurve1= mavlib_avatarReadCurves("walking.cset");
  if (!mavlib_avatarCurve1) exit(1);
  
  /* A list and function to update the position of all avatars */
  mavlib_avatarList= mav_listNew();
  mav_frameFn1Add(mavlib_avatarUpdateFn, NULL);

  return 1;
}



