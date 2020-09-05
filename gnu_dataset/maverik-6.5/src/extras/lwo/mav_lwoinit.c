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
#include "mav_lwo.h"



/* Routines to initialise the module */

char *mav_LWOModuleID(void)
{
  return "Lightwave parser";
}

int mav_LWOModuleInit(void)
{
  /* Add the new module */
  mav_moduleNew(mav_LWOModuleID);

  /* Add lwo to list of supported composite file formats */
  if (mav_compositeFormat[3].defined && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: file format 3 already reserved, overwriting\n"); 
  mav_compositeFormat[3].defined= MAV_TRUE;
  mav_compositeFormat[3].ext= ".lwo";
  mav_compositeFormat[3].fn= mav_compositeReadLWO;

  return 1;
}
