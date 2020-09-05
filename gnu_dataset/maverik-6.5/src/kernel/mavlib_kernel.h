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


#if defined(WIN32) && !defined(__CYGWIN__)
#pragma warning( disable : 4244 ) /* '=' : conversion from 'const double ' to 'float ', possible loss of data */
#pragma warning( disable : 4018 ) /* '<' : signed/unsigned mismatch */
#pragma warning( disable : 4305 ) /* '=' : truncation from 'const double ' to 'float ' */
#endif

#include "mav_kernel.h"
#include "mav_gfx.h"

#define MAVLIB_OBJECT_TABLE_SIZE 113 /* make it prime */

extern MAV_list **mavlib_table_list;
extern MAV_list *mavlib_frame0_list;
extern MAV_list *mavlib_frame1_list;
extern MAV_list *mavlib_frame2_list;
extern MAV_list *mavlib_frame3_list;
extern MAV_list *mavlib_frame4_list;

extern MAV_list *mavlib_devicePoll_list;
extern MAV_list *mavlib_deviceCalc_list;
extern MAV_list *mavlib_deviceEvent_list;

void mavlib_setUpObjectTables(void); 
void mavlib_objectTablesAddSMS(MAV_object *, MAV_SMS *);
void mavlib_objectTablesRemoveSMS(MAV_object *, MAV_SMS *);
void mavlib_lightPosFix(void);

typedef struct {
  MAV_object *the_obj;
  MAV_list *SMS_list;
} MAVLIB_objectTableEntry;

