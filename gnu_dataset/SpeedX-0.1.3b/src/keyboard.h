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

#ifndef __CLAVIER_H

#define __CLAVIER_H



#include "Xh.h"




void init_keyboard(Display * dpy);

void change_keys(KeyCode * touche);





KeyCode KEY_UP;

KeyCode KEY_DOWN;

KeyCode KEY_RIGHT;

KeyCode KEY_LEFT;

KeyCode KEY_UP2;

KeyCode KEY_DOWN2;

KeyCode KEY_RIGHT2;

KeyCode KEY_LEFT2;

KeyCode KEY_ESC ;

KeyCode KEY_SPEED ;

KeyCode KEY_QUIT;



extern Display *display;



#endif

