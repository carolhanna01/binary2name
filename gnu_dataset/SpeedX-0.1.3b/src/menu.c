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

#include "menu.h"
#include <unistd.h>

extern unsigned int depth;
extern int factor;
extern Window win;



void menu(char *buffer, Display * dpy)
{

/*
   TO DO:
   a menu of choice.
   change_keys(&KEY_UP);

 */

	char *_buffer;
	char *_menu;
	int dx, dy;

	Read_PNG(&_menu, "./road/menu.png", depth,
		 &dx, &dy);




	_buffer = buffer;

	/* OPTIMISATION !!! */

	memcpy((char *) _buffer, (char *) (_menu),
	       factor * dx * dy);
	sleep(2);
	aff(win);
	XPending(dpy);

	sleep(1);
}
