
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

   sprite : Car drawing according to the distance
 */

#include <math.h>

#include "std.h"

#include "sprite.h"

#include "read_image.h"

#include "world.h"

#include "mem.h"

extern unsigned int depth;

extern World *world;

extern int factor;

#define DX 100
#define DY 100

void load_sprites(int *xsize, int *ysize)
{

	int i;

	char nom[128];

	char *noms_images[] =
	{"3a", "2a", "1a", "0", "1", "2", "3"};

	for (i = 0; i < 10; i++) {

		sprintf(nom, "./num/%1d.png", i);

		Read_PNG(number + i, nom, depth, &num_l, &num_v);

	}

	switch (world->num_veh = vehicule) {

	case 0:

		for (i = 0; i < 7; i++) {

			sprintf(nom, "./cars/f1_1/f1_1_%s.png",
				noms_images[i]);

			Read_PNG(m_v + i, nom, depth, xsize, ysize);

		}

		for (i = 0; i < 7; i++) {

			sprintf(nom, "./cars/f1_1/f1_1_%s_mask.png",
				noms_images[i]);

			Read_PNG(v + i, nom, depth, xsize, ysize);

		}

		break;

	case 1:

		for (i = 0; i < 7; i++) {

			sprintf(nom, "./cars/jeep/jeep_%s.png",
				noms_images[i]);

			Read_PNG(m_v + i, nom, depth, xsize, ysize);

		}

		for (i = 0; i < 7; i++) {

			sprintf(nom, "./cars/jeep/jeep_%s_mask.png",
				noms_images[i]);

			Read_PNG(v + i, nom, depth, xsize, ysize);

		}

		break;

	case 2:

		for (i = 0; i < 7; i++) {

			sprintf(nom, "./cars/camion/camion_%s.png",
				noms_images[i]);

			Read_PNG(m_v + i, nom, depth, xsize, ysize);

		}

		for (i = 0; i < 7; i++) {

			sprintf(nom,
				"./cars/camion/camion_%s_mask.png",
				noms_images[i]);

			Read_PNG(v + i, nom, depth, xsize, ysize);

		}

		break;

	}

	/* calculate a boolean mask from the sprite and its mask */
	/* Then, we won't have to compare the sprite with the mask during the execution */
	for (i = 0; i < 7; i++) {
		int x;
		int y;
		int k;
		char *tmp = v[i];
		char *m_tmp = m_v[i];
		for (y = 0; y < dy; y++)
			for (x = 0; x < dx; x++) {
				int offset = factor * (y * dx + x);
				int ok = 1;
				for (k = 0; k < factor; k++) {
					if (m_tmp[offset + k] != tmp[offset + k]) {
						ok = 0;
						break;
					}
				}
				if (!ok)
					m_tmp[offset] = 0;
				else
					m_tmp[offset] = 1;
			}
	}
	{

		int xsize_d, ysize_d;

		xsize_d = ysize_d = 256;

		Read_PNG(&panel, "./scene/arrivee.png", depth, &xsize_d,
			 &ysize_d);

	}

}

int Draw_car(char *screen_buffer, int num_veh, int dep, int posx,
	     float gros)
{

/* SPRITE */

/*
   We use _posx to simulate the curve effect
 */

	int posy;
	int _posx;
	int i, j;
	int end_i, end_j;
	char *tmp = NULL;
	char *m_tmp = NULL;
	int offset;
	int x, y;
	int rx, ry;
	float invgros = 1 / gros;

	i = ((dep) / vmin[num_veh]) + 3;
	if (i < 0)
		i = 0;
	else if (i > 6)
		i = 6;

	tmp = v[i];
	m_tmp = m_v[i];

	posy = HVISION * (1 + invgros) - 50 * invgros;
	/* Curve */
	/* We have to correct  with the sprite height */
	_posx = (50 + posx - HVISION) * invgros + HVISION + (delta_relatif[2 * HVISION - posy] * 3  /*(depth >> 3)*/)  - dep;

	end_i = DX * invgros;
	end_j = DY * invgros;

	for (j = 0, y = posy; j < end_j; j++, y++) {
		if (y < HEIGHT) {
			ry = j * gros;
			for (i = 0, x = _posx; i < end_i; i++, x++) {
				if ((x >= 0) && (x < WIDTH)) {
					rx = i * gros;
					offset = rx + DX * ry;
					if (offset < DX * DY) {
						offset *= factor;
						if (m_tmp[offset]) {
							S_memcpy(&screen_buffer[factor * (x + WIDTH * y)], &tmp[offset], factor);
						}
					}
				}
			}
		} else
			return 0;
	}
	return 0;

}

void Free_sprite(void)
{

	int i;

	for (i = 0; i < 7; i++) {

		free(m_v[i]);

		free(v[i]);

	}

	free(panel);

}
