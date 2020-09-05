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

   Sprites and sky initialisation
 */

#include "std.h"

#include "init.h"

extern int factor;

/*
   OUT : image size
 */

int init(char **sky, int numsky, int _depth)
{

	int height, width;

	switch (numsky) {

	case 0:

		Read_PNG(sky, "./background/sky0.png", _depth, &width,
			 &height);

		break;

	case 1:

		Read_PNG(sky, "./background/sky1.png", _depth, &width,
			 &height);

		break;

	}

	return factor * height * width * sizeof(char);

}
