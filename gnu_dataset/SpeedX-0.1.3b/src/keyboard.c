
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

/*
   Version : 0.1.2c

   License : GPL  

   keys variables
 */

#include "keyboard.h"

void init_keyboard(Display * dpy)
{

	/* First player */

	KEY_LEFT = XKeysymToKeycode(dpy, XK_KP_4);	// XK_Left

	KEY_UP = XKeysymToKeycode(dpy, XK_KP_8);	// XK_Up

	KEY_RIGHT = XKeysymToKeycode(dpy, XK_KP_6);	// XK_Right

	KEY_DOWN = XKeysymToKeycode(dpy, XK_KP_2);	// XK_Down

	/* second player (is this QWERTY keyboard OK ?) */

	KEY_LEFT2 = XKeysymToKeycode(dpy, XK_S);	// XK_Left

	KEY_UP2 = XKeysymToKeycode(dpy, XK_E);	// XK_Up

	KEY_RIGHT2 = XKeysymToKeycode(dpy, XK_F);	// XK_Right

	KEY_DOWN2 = XKeysymToKeycode(dpy, XK_C);	// XK_Down

	/* control */

	KEY_ESC = XKeysymToKeycode(dpy, XK_Escape);

	KEY_SPEED = XKeysymToKeycode(dpy, XK_Control_R);

	KEY_QUIT = XKeysymToKeycode(dpy, XK_q);

}

void change_keys(KeyCode * touche)
{

	XEvent XEv;

	while (XCheckMaskEvent(display, KeyPressMask, &XEv) != True) {

	}

	(*touche) = XEv.xkey.keycode;

}
