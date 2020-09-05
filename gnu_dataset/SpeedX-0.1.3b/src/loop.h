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

#ifndef __BOUCLE_H

#define __BOUCLE_H



#include "Xh.h"

#include "const.h"

#include "course.h"

#include "music.h"

/*
A mask defines which keys are being pushed
 */

#define UP 		1

#define DOWN  		2

#define RIGHT 	4

#define LEFT	8



int loop (Display * dpy) ;

int auto_run(void);





extern int multi ;



extern int mit_shm ;



extern Display * display ;

extern Display * display2 ;



extern Window win ;

extern Window win2 ;



extern KeyCode KEY_UP;

extern KeyCode KEY_DOWN;

extern KeyCode KEY_RIGHT;

extern KeyCode KEY_LEFT;



extern KeyCode KEY_UP2;

extern KeyCode KEY_DOWN2;

extern KeyCode KEY_RIGHT2;

extern KeyCode KEY_LEFT2;



extern KeyCode KEY_ESC;

extern KeyCode KEY_QUIT;



//extern int num_veh;

extern int vitesse;



extern int dble ;



extern char * screen_buffer , *sky  ;

extern int multi ;



extern int event_x(XEvent *);

extern void menu (char * buffer, Display * dpy) ;


extern void refresh(char *,char *, int,int);

extern void aff(Window window);

extern void aff2(Window window);



/*

extern int physics(Vehicule Cl[]);

extern void collision (Vehicule Cl[],int);

*/

extern int Draw_car(char *, int, int, int, float);



extern void init_course(void);



#endif /* __BOUCLE_H */

