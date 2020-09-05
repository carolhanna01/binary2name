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

#include "effects.h"

#include "const.h"

#include "color.h"

#include "mem.h"

extern char *screen_buffer;

extern int factor;

extern int color;

extern int bitmap_bit_order;

void fog(unsigned int r, unsigned int g, unsigned int b)
{

	unsigned int i, j;

	unsigned int Color = 0;

	unsigned int background = 0;

	unsigned int rc, gc, bc, alpha;

	unsigned int ofs1, ofs2;

	ofs1 = factor * (HEIGHT / 2) * WIDTH;

	ofs2 = ofs1 - factor;

	for (j = 0; j < HEIGHT / 4; j++) {

		alpha = (((HEIGHT / 4) - j) * 255) / (HEIGHT / 4);

		for (i = 0; i < WIDTH; i++) {

			/* to Bottom */

			memcpy(&background, &screen_buffer[ofs1], factor);

			if (bitmap_bit_order)
				background >>= (8 * (4 - factor));

			rc =
			    (alpha * r +
			     (255 - alpha) * red(background)) >> 8;

			gc =
			    (alpha * g +
			     (255 - alpha) * green(background)) >> 8;

			bc =
			    (alpha * b +
			     (255 - alpha) * blue(background)) >> 8;

			Color = rgb(rc, gc, bc);

			memcpy(&screen_buffer[ofs1], &Color, factor);

			ofs1 += factor;

			/* to Up */

			memcpy(&background, &screen_buffer[ofs2], factor);

			if (bitmap_bit_order)
				background >>= (8 * (4 - factor));

			rc =
			    (alpha * r +
			     (255 - alpha) * red(background)) >> 8;

			gc =
			    (alpha * g +
			     (255 - alpha) * green(background)) >> 8;

			bc =
			    (alpha * b +
			     (255 - alpha) * blue(background)) >> 8;

			Color = rgb(rc, gc, bc);

			memcpy(&screen_buffer[ofs2], &Color, factor);

			ofs2 -= factor;

		}

	}

}
