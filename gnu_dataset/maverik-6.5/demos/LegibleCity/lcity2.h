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

#ifndef _MAV_LCITY_INCLUDE
#define _MAV_LCITY_INCLUDE

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef struct {
    MAV_surfaceParams *sp;
    MAV_matrix matrix;
    MAV_BB bb;
    void *userdef;
} MAV_LCity;

extern void mav_LCityModuleInit(void);
extern MAV_LCity *mav_newLCity(MAV_surfaceParams **sp);
extern MAV_class *mav_class_lcity;

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* _MAV_LCITY_INCLUDE */
