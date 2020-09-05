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

#ifndef __COLOR_H

#define __COLOR_H

extern unsigned int depth;
extern int factor;
extern int bitmap_bit_order;

#define color_shift ( bitmap_bit_order ? (8 * (4 - factor)) : 0)

#define red(color) ( \
			depth ==  8 ? (((color) >> 6) * 255) / 3 : \
			depth == 15 ? ((((color) >> 10) & 31) * 255) / 31 : \
			depth == 16 ? ((((color) >> 11) & 31) * 255) / 31 : \
			/* else */    ((color) >> 16) & 255 \
		   )

#define green(color) ( \
			depth ==  8 ? ((((color) >> 3) & 7) * 255) / 7 : \
			depth == 15 ? ((((color) >> 5) & 31) * 255) / 31 : \
			depth == 16 ? ((((color) >> 5) & 63) * 255) / 63 : \
			/* else */    ((color) >> 8) & 255 \
		     )

#define blue(color) ( \
			depth ==  8 ? (((color) & 7) * 255) / 7 : \
			depth == 15 ? (((color) & 31) * 255) / 31 : \
			depth == 16 ? (((color) & 31) * 255) / 31 : \
			/* else */    (color) & 255 \
                    )

#define grey2(r, g, b) ( (42 * r + 71 * g + 14 * b) >> 7 )

#define grey(color) ( \
			{ \
				unsigned int dep_color = color >> color_shift; \
				grey2(red(dep_color), green(dep_color), blue(dep_color));\
			} \
		    )

#define rgb(r, g, b) ( \
			depth ==  8 ? ((((r) >> 6) <<  6) + (((g) >> 5) << 3) + ((b) >> 5)) << color_shift : \
			depth == 15 ? ((((r) >> 3) << 10) + (((g) >> 3) << 5) + ((b) >> 3)) << color_shift : \
			depth == 16 ? ((((r) >> 3) << 11) + (((g) >> 5) << 3) + ((b) >> 3)) << color_shift : \
			/* else */    ( ((r)       << 16) +  ((g)       << 8) +  (b)      ) << color_shift \
		     )
#endif /* __COLOR_H */

