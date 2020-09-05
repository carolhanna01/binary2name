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


#include "mavlib_callbacks.h"
#include <stdlib.h>
#include <stdio.h>



/* Wrapper routines to set and execute the get family of callbacks */

MAV_callback *mav_callback_getUserdef;
MAV_callback *mav_callback_getMatrix;
MAV_callback *mav_callback_getSurfaceParams;

void mav_callbackGetUserdefSet(MAV_window *w, MAV_class *c, MAV_callbackGetUserdefFn fn)
{
  mav_callbackSet(mav_callback_getUserdef, w, c, (MAV_callbackFn) fn);
}

int  mav_callbackGetUserdefExec(MAV_window *w, MAV_object *o, void ***d)
{
  return (mav_callbackExec(mav_callback_getUserdef, w, o, (void *) d, NULL));
}



void mav_callbackGetMatrixSet(MAV_window *w, MAV_class *c, MAV_callbackGetMatrixFn fn)
{
  mav_callbackSet(mav_callback_getMatrix, w, c, (MAV_callbackFn) fn);
}

int  mav_callbackGetMatrixExec(MAV_window *w, MAV_object *o, MAV_matrix **m)
{
  return (mav_callbackExec(mav_callback_getMatrix, w, o, (void *) m, NULL));
}



void mav_callbackGetSurfaceParamsSet(MAV_window *w, MAV_class *c, MAV_callbackGetSurfaceParamsFn fn)
{
  mav_callbackSet(mav_callback_getSurfaceParams, w, c, (MAV_callbackFn) fn);
}

int  mav_callbackGetSurfaceParamsExec(MAV_window *w, MAV_object *o, MAV_surfaceParams ***sp)
{
  return (mav_callbackExec(mav_callback_getSurfaceParams, w, o, (void *) sp, NULL));
}
