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

#ifndef __JEU_H

#define __JEU_H



#include "const.h"

#include "Xh.h"

#include "course.h"


/*
dx and dy are the x and y dimension
of sprites images
*/

int dx;

int dy;



int vitesse = 0;

int Road_type = 0 ; // type of the road



char *road = NULL ;
char *border = NULL ;
char *depart = NULL ;



extern int mit_shm ;


extern unsigned int depth;

extern Display *display;

extern Window win;

extern Window win2;

extern XImage *xim;

extern GC gc;

extern char *screen_buffer, *screen_buffer2 ;



extern int dep_alea;



extern void Draw_road(char *, int,int);



extern int num_v,num_l ;

extern char *number[10] ;

extern int SkyCylinder [WIDTH] ;

extern int multi ;



#endif /* __JEU_H */

