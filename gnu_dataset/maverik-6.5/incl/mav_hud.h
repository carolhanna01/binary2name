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

#ifndef MAV_HUD_H
#define MAV_HUD_H

#ifdef MAVAPI
#undef MAVAPI
#endif
#if defined(WIN32) && !defined(__CYGWIN__) 
#ifdef LIBMAV_HUD_EXPORTS
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

typedef struct {
  int nolines;
  char **line;
  int x;
  int y;
  int w;
  int h;
  float depthfac;
  int textXOffset;
  int textYOffset;
  int textYStep;
  int textFont;
  int textColour;
  int selectable;
  MAV_rectangle rect;
  MAV_object *obj;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_hud;

MAVAPI extern MAV_class *mav_class_hud;

MAVAPI int mav_hudDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_hudBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_hudIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_hudID(MAV_object *o, char **id);
MAVAPI int mav_hudGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_hudGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_hudGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_hudDump(MAV_object *o);

MAVAPI MAV_hud *mav_hudNew(int nolines, int x, int y, int w, int h, MAV_surfaceParams *sp);



/* Module initialise */

MAVAPI char *mav_hudModuleID(void);
MAVAPI int mav_hudModuleInit(void);

#ifdef __cplusplus
}
#endif
#endif /* MAV_HUD_H */
