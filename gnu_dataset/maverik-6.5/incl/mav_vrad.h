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

#ifndef _MAV_VRAD_INCLUDE
#define _MAV_VRAD_INCLUDE

#ifdef MAVAPI
#undef MAVAPI
#endif
#if defined(WIN32) && !defined(__CYGWIN__) 
#ifdef LIBMAV_VRAD_EXPORTS
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

/* Vrad */

typedef struct {
  MAV_SMS *sms;
  MAV_matrix matrix;
  void *userdef;
} MAV_vrad;

MAVAPI extern MAV_class *mav_class_vrad;
MAVAPI int mav_vradDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_vradGetMatrix(MAV_object *o, MAV_matrix **mat);
MAVAPI int mav_vradGetUserdef(MAV_object *o, void ***ud);

MAVAPI int  mav_vradRead(char *filename, MAV_vrad *vrad, MAV_matrix m);
MAVAPI void mav_vradGamma(MAV_vrad *vrad, float gam);



/* Options */

MAVAPI extern int mav_opt_vradSetMatrix;



/* Module initialise */

MAVAPI char *mav_vradModuleID(void);
MAVAPI int mav_vradModuleInit(void);

#ifdef __cplusplus
}
#endif
#endif
