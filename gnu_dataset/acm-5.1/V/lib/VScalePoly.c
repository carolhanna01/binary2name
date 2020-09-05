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
#include <math.h>

void ComputeRotationMatrix(double r, VPoint * e, VMatrix * m);

VPolygon *
ScalePolygon(VPolygon * in, VPoint * offset, VPoint * scale, VPoint *e, double r)
{
    int numVtces = in->numVtces, i;
    VPoint *vert;
    VPolygon *p;
    VPoint a, b, tmp, offset1;
    VMatrix m;

    p = (VPolygon *) Vmalloc(sizeof(VPolygon));
#ifdef DEBUG
    fprintf(stderr, "scaling %d-point polygon %g, %g, %g; rotation %g; offset %g, %g, %g\n",
	    in->numVtces, scale->x, scale->y, scale->z, r,
	    offset->x, offset->y, offset->z);
#endif

    *p = *in;
    p->numVtces = numVtces;
    vert = p->vertex = (VPoint *) Vmalloc(sizeof(VPoint) * numVtces);

    ComputeRotationMatrix(r, e, &m);
    
    VTransform_ (offset, &m, &offset1);

    for (i = 0; i < numVtces; ++i) {
    	p->vertex[i] = in->vertex[i];
#ifdef notdef
    	p->vertex[i].x -= offset->x;
	p->vertex[i].y -= offset->y;
	p->vertex[i].z -= offset->z;
#endif
        VTransform_(&p->vertex[i], &m, &tmp);
#ifdef notdef
	tmp.x += offset1.x;
	tmp.y += offset1.y;
	tmp.z += offset1.z;
#endif
	p->vertex[i].x = tmp.x * scale->x;
	p->vertex[i].y = tmp.y * scale->y;
	p->vertex[i].z = tmp.z * scale->z;
#ifdef notdef
	p->vertex[i].x += offset->x;
	p->vertex[i].y += offset->y;
	p->vertex[i].z += offset->z;
	p->vertex[i].x += offset1.x;
	p->vertex[i].y += offset1.y;
	p->vertex[i].z += offset1.z;
#endif
    }

    if ((p->flags & PolyNormalValid) == 0) {
	if ((p->flags & PolyClipBackface) != 0 ||
	    p->backColor != (VColor *) NULL) {
	    a.x = vert[0].x - vert[1].x;
	    a.y = vert[0].y - vert[1].y;
	    a.z = vert[0].z - vert[1].z;
	    b.x = vert[2].x - vert[1].x;
	    b.y = vert[2].y - vert[1].y;
	    b.z = vert[2].z - vert[1].z;
	    VCrossProd(&a, &b, &p->normal);
	    p->flags |= PolyNormalValid;
	}
    }

    return p;
}

void
ComputeRotationMatrix(double r, VPoint * e, VMatrix * m)
{
    double one64th = 1.0 / 64.0, ma;
    VPoint Ax, Ay, Wy =
    {0, 1, 0}, Wz =
    {0, 0, 1};
    VMatrix tm, tm1;

    VIdentMatrix(&tm);
    if (r != 0.0) {
	VRotate(&tm, ZRotation, r * M_PI / 180.0);
    }

    if (fabs(e->x) < one64th && fabs(e->y) < one64th) {
	VCrossProd(&Wy, e, &Ax);
    }
    else {
	VCrossProd(&Wz, e, &Ax);
    }

    ma = sqrt(Ax.x * Ax.x + Ax.y * Ax.y + Ax.z * Ax.z);
    Ax.x /= ma;
    Ax.y /= ma;
    Ax.z /= ma;

    VCrossProd(e, &Ax, &Ay);

    ma = sqrt(Ay.x * Ay.x + Ay.y * Ay.y + Ay.z * Ay.z);
    Ay.x /= ma;
    Ay.y /= ma;
    Ay.z /= ma;

    VIdentMatrix(m);
    m->m[0][0] = Ax.x;
    m->m[1][0] = Ax.y;
    m->m[2][0] = Ax.z;

    m->m[0][1] = Ay.x;
    m->m[1][1] = Ay.y;
    m->m[2][1] = Ay.z;

    m->m[0][2] = e->x;
    m->m[1][2] = e->y;
    m->m[2][2] = e->z;

    VMatrixMultByRank(&tm, m, &tm1, 3);
    *m = tm1;
}
