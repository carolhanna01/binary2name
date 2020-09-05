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

   Version : 0.1
   License : GPL
 */



#ifndef __LIRE_PNG_H

#define __LIRE_PNG_H



#include <png.h>

#include <zlib.h>



int Read_PNG(char **, char *, int, int *, int *);


#define RGB16(p,r,g,b) *((unsigned short *) (p))=(r<<11) | \
(g<<5) | (b);



#endif /* __LIRE_PNG_H */

