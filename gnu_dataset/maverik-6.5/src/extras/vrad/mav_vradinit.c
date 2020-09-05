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
#include "mavlib_vrad.h"



/* Routines to initialise the module */

char *mav_vradModuleID(void)
{
  return "VRAD";
}

int mav_vradModuleInit(void)
{
  /* Add the new module */
  mav_moduleNew(mav_vradModuleID);

  /* Create new object class and set callbacks */
  mav_class_vrad= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mav_class_vrad, mav_vradDraw);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_vrad, mav_vradGetMatrix);
  mav_callbackGetUserdefSet(mav_win_all, mav_class_vrad, mav_vradGetUserdef);

  /* Internal element class */
  mavlib_vradElemClass= mav_classNew();
  mav_callbackDrawSet(mav_win_all, mavlib_vradElemClass, mavlib_vradElemDraw);
  mav_callbackBBSet(mav_win_all, mavlib_vradElemClass, mavlib_vradElemBB);

  return 1;
}
