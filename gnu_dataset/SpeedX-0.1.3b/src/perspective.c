
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

   Precomputation of perspective and initialisation
 */

#include "perspective.h"
#include <math.h>

#define PI 3.1415926

/*

   perspective : For a line of vision we give the distance

   The Projective Distance is  H / ( H - z )

 */

void init_table_perspective(void)
{

	int i, j;

	int conversion_x[256 * HVISION], conversion_y[256 * HVISION];

	for (i = 0; i < HVISION; i++) {

		ProjectiveDistance[i] =
		    (HVISION * HVISION / (HVISION - i));

		perspective[i] =
		    (HVISION * HVISION / (HVISION - i)) % HEIGHT;

	}

	for (i = 0; i < HVISION; i++)
		for (j = 0; j < WIDTH; j++) {

			conversion_x[i * WIDTH + j] =
			    ((j) * HVISION / (HVISION - i)) -
			    ((WIDTH / 2) * HVISION / (HVISION - i));

			conversion_y[i * WIDTH + j] = perspective[i];

			conversion[i * WIDTH + j] =
			    conversion_x[i * WIDTH + j] +
			    WIDTH * conversion_y[i * WIDTH + j];

		}

	for (i = 0; i < HVISION; i++) {

		Rightroad[i] =
		    WIDTH / 2 + 4 * WIDTH * (HVISION - i) / (HVISION * 2);
/*
   if (Rightroad[i] >= WIDTH)
   Rightroad[i] = WIDTH;
 */
/*
   if (Rightroad[i] >= WIDTH)
   Rightroad[i] = WIDTH;
 */

		Leftroad[i] = WIDTH - Rightroad[i];

	}
	for (i = 0; i < WIDTH; i++) {
		double x;
		x = (PI / 3) * (i - WIDTH / 2) / WIDTH;
		SkyCylinder[i] = WIDTH * tan(x);
	}

}
