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

#include <Vlib.h>

extern VPolygon *ScalePolygon(VPolygon * in, VPoint * origin,
			      VPoint * scale, double);

VObject *
VExtrudeObject(VObject * obj, VPoint * vec)
{
#ifdef notdef
    VPoint tmp[4];
	int k, other;
#endif
    VPolygon *poly_tmp[65536], *poly;
    int i, j;
    VObject *newObj;
    VPoint scale =
    {1, 1, 1};

#ifdef DEBUG
    fprintf(stderr, "creating an extrusion based on \"%s\"\n", obj->name);
    fprintf(stderr, "starting with %d polygons\n", obj->numPolys);
#endif
    newObj = (VObject *) Vmalloc(sizeof(VObject));
    newObj->name = obj->name;
    newObj->extent = obj->extent;
    newObj->center = obj->center;

    for (i = 0; i < obj->numPolys; ++i) {
	if ((poly_tmp[obj->numPolys - i - 1] = VCopyPolygon(obj->polygon[i]))
	    == (VPolygon *) NULL) {
	    fprintf(stderr, "VExtrudeObject: can't copy polygons\n");
	    exit(1);
	}
    }

/*
 *  If clipping is enabled, then we should be reversing the vertices
 *  of this polygon
 */
    for (i = 0, j = obj->numPolys; i < obj->numPolys; ++i, ++j) {
	if ((poly_tmp[j] = ScalePolygon(obj->polygon[i], vec, &scale, 0.0))
	    == (VPolygon *) NULL) {
	    fprintf(stderr, "VExtrudeObject: can't copy polygons\n");
	    exit(1);
	}
    }

/*
 *  Now create extrusion polygons by marching through the original polygon
 *  vertices and creating new polygons that connect the edges of the "upper"
 *  and "lower" polygons as we go.
 */

#ifdef notdef
    for (i = 0; i < obj->numPolys; ++i) {
	poly = obj->polygon[i];
	for (k = 0; k < poly->numVtces; ++k) {
	    other = (k == 0) ? poly->numVtces - 1 : k - 1;
	    tmp[0] = poly->vertex[other];
	    tmp[1] = poly->vertex[k];
	    tmp[2] = poly->vertex[k];
	    tmp[2].x += vec->x;
	    tmp[2].y += vec->y;
	    tmp[2].z += vec->z;
	    tmp[3] = poly->vertex[other];
	    tmp[3].x += vec->x;
	    tmp[3].y += vec->y;
	    tmp[3].z += vec->z;
	    poly_tmp[j++] = VCreatePolygonFromTemplate(4, tmp, poly);
	}
    }
#endif

/*
 *  Complete filling the object structure
 */

#ifdef DEBUG
    fprintf(stderr, "%d polygons in the extruded object\n", j);
#endif
    newObj->polygon = (VPolygon **) Vmalloc(sizeof(VPolygon *) * j);
    newObj->numPolys = j;
    for (i = 0; i < j; ++i) {
	newObj->polygon[i] = poly_tmp[i];
    }
    return newObj;
}
