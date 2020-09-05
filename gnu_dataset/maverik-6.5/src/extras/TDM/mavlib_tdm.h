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

#include "maverik.h"
#include "mav_tdm.h"

#ifdef MAV_TDM
#include <tdm.h>
int mavlib_dealWithTDMEvent(TDM_buttonEvent *);
void mavlib_tdm2mav(MAV_TDMPos *, TDM_position);

extern float mavlib_TDM_xorigin[TDM_MAX_TRACKERS];
extern float mavlib_TDM_yorigin[TDM_MAX_TRACKERS];
extern float mavlib_TDM_zorigin[TDM_MAX_TRACKERS];
extern float mavlib_TDM_xscale[TDM_MAX_TRACKERS];
extern float mavlib_TDM_yscale[TDM_MAX_TRACKERS];
extern float mavlib_TDM_zscale[TDM_MAX_TRACKERS];
extern float mavlib_TDM_offset[TDM_MAX_TRACKERS];
extern float mavlib_TDM_drawScale[TDM_MAX_TRACKERS];

#else

extern float mavlib_TDM_xorigin[4];
extern float mavlib_TDM_yorigin[4];
extern float mavlib_TDM_zorigin[4];
extern float mavlib_TDM_xscale[4];
extern float mavlib_TDM_yscale[4];
extern float mavlib_TDM_zscale[4];
extern float mavlib_TDM_offset[4];
extern float mavlib_TDM_drawScale[4];

#endif

void mavlib_TDM_cursorInit(void);
MAV_matrix mavlib_TDM_calcPos(int, MAV_TDMPos, MAV_matrix);
MAV_matrix mavlib_TDM_iv(void);

