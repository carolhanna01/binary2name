/*
 *  acm : an aerial combat simulator for X
 *  Copyright (C) 1991-1998  Riley Rainey
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundaation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#include "Vlib.h"

VObject  *
VCopyObject(VObject * obj)
{

	register int i;
	register VObject *newObj;

	newObj = (VObject *) Vmalloc(sizeof(VObject));
	newObj->name = obj->name;
	newObj->extent = obj->extent;
	newObj->center = obj->center;
	newObj->numPolys = obj->numPolys;
	newObj->polygon =
		(VPolygon **) Vmalloc(sizeof(VPolygon *) * newObj->numPolys);
	if (obj->order) {
		newObj->order = (unsigned short *)
			Vmalloc(sizeof(unsigned short) * NUM_ASPECTS *
					newObj->numPolys);

		memcpy((char *) newObj->order, (char *) obj->order,
			   sizeof(unsigned short) * NUM_ASPECTS *
			   newObj->numPolys);
	}
	else {
		newObj->order = (unsigned short *) NULL;
	}

	for (i = 0; i < obj->numPolys; ++i) {
		if ((newObj->polygon[i] = VCopyPolygon(obj->polygon[i]))
			== (VPolygon *) NULL) {
			fprintf(stderr, "VCopyObject: can't copy polygons\n");
			exit(1);
		}
	}

	return newObj;
}
