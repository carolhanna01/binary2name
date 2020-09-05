/*
 * // SpeedX //
 *
 *  medernac@isty-info.uvsq.fr
 *
 *  Copyright (C) 2000 
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#ifndef __road_H

#define __road_H



#include "const.h"

#include "course.h"



int delta_relatif [HVISION] ;



void Draw_road(char *screen_buffer, int view,int dep);





extern char *road;
extern char *border;
extern char *depart;



extern int ProjectiveDistance[HVISION];

extern int conversion[WIDTH * HVISION];

extern int Rightroad[HVISION];

extern int Leftroad[HVISION];

extern unsigned int depth;

extern int inv_radius;



#endif /* __road_H */

