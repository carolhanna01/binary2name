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
#include "mav_vrml97.h"
#include <stdio.h>

int mav_opt_VRML97CleanUp= MAV_TRUE;
int mav_opt_VRML97HBBThreshold= 0;


/* Routine to define a composite from a VRML97 file */

int mav_compositeReadVRML97(char *filename, MAV_composite *c, MAV_matrix m)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: code not compiled with VRML97 option, ignoring\n");
  
  return MAV_FALSE;
}



/* Routines to initialise the module */

char *mav_VRML97ModuleID(void)
{
  return "VRML97 (dummy)";
}

int mav_VRML97ModuleInit(void)
{
  /* Add the new module */
  mav_moduleNew(mav_VRML97ModuleID);

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: code not compiled with VRML97 option, ignoring\n");

  return 1;
}
