/*
 *  acm : an aerial combat simulator for X
 *  Copyright (C) 1991-1997  Riley Rainey
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
 *  along with this program; if not, write to the Free Software Foundation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#include "pm.h"
#include <math.h>

extern int acm_rand();

#if defined(NEEDS_COPYSIGN)

#if defined(HAS_IEEE_NAN)
#include <ieeefp.h>
#endif

double
copysign(double x, double y)
{

#if defined(HAS_IEEE_NAN)
	if (isnand(y))
		return fabs(x);
#endif

	return (y < 0.0) ? -fabs(x) : fabs(x);
}

#endif

#define SPIKES			16
#define FLAME_SPIKES	8
#define SMOKE_INNER		0.2
#define SMOKE_RADIUS	1.0
#define SMOKE_MIN_RADIUS 0.5
#define SMOKE_VARIATION (SMOKE_RADIUS - SMOKE_MIN_RADIUS)
#define FLAME_RADIUS	0.5
#define FLAME_MIN_RADIUS 0.3
#define FLAME_VARIATION	(FLAME_RADIUS - FLAME_MIN_RADIUS)

/*
 *  return a double value from -1.0 .. 1.0
 */

double
efrandom(void)
{
	return ((acm_rand() % 32767) - 16384) / 16384.0;
}

static VObject *explosionTemplate;

int       placeExplosion(Viewport * v, craft * obj, VMatrix * mtx, VPolygon ** poly, long *cnt);

static craftType expcraft;

void
newExplosion(VPoint * loc, VPoint *vel, double s_meters, double dur1, double dur2)
{

	register int i;
	register craft *e;

	for (i = 0; i < MAXPROJECTILES; ++i) {
		if (mtbl[i].type == CT_FREE) {
			e = &mtbl[i];
			e->type = CT_EXPLOSION;
			e->Sg = *loc;
			DISGeocentricToWorldCoordinates(
							   (dis_world_coordinates *) & e->Sg, &e->w);
			e->Cg = *vel;
			e->escale = s_meters;
			e->duration = (int) (dur1 / deltaT + 0.5);
			e->flameDuration = (int) (dur2 / deltaT + 0.5);
			e->cinfo = &expcraft;
			e->cinfo->placeProc = placeExplosion;
			e->curHeading = e->curRoll = e->curPitch = 0.0;
			break;
		}
	}
}

static VColor *effectBlackColor;

void
buildExplosion(void)
{

	register int i, numSpikes, numFlame, numRed, poly;
	register VObject *obj;
	VColor   *redFlameColor, *orangeFlameColor, *color;
	VPoint    vp[3];
	double    x, s;

	numSpikes = SPIKES;
	numFlame = FLAME_SPIKES;
	numRed = numFlame / 2;

	effectBlackColor = VAllocColor("black");
	redFlameColor = VAllocColor("red");
#ifndef EIGHT_COLORS
	orangeFlameColor = VAllocColor("#ffc900");
#endif

	obj = (VObject *) Vmalloc(sizeof(VObject));
	obj->name = strdup("explosion");
	obj->numPolys = numSpikes + numFlame;
	obj->polygon = (VPolygon **) Vmalloc(obj->numPolys * sizeof(VPolygon *));
	obj->order = (unsigned short *) NULL;

	poly = 0;

	for (i = 0; i < numSpikes; ++i) {
		x = efrandom();
		s = copysign(1.0, x);
		x = fabs(x);
		vp[0].x = (SMOKE_MIN_RADIUS + x * SMOKE_VARIATION) * s;
		x = efrandom();
		s = copysign(1.0, x);
		x = fabs(x);
		vp[0].y = (SMOKE_MIN_RADIUS + x * SMOKE_VARIATION) * s;
		x = efrandom();
		s = copysign(1.0, x);
		x = fabs(x);
		vp[0].z = (SMOKE_MIN_RADIUS + x * SMOKE_VARIATION) * s;
		vp[1].x = efrandom() * SMOKE_INNER;
		vp[1].y = efrandom() * SMOKE_INNER;
		vp[1].z = efrandom() * SMOKE_INNER;
		vp[2].x = efrandom() * SMOKE_INNER;
		vp[2].y = efrandom() * SMOKE_INNER;
		vp[2].z = efrandom() * SMOKE_INNER;
		obj->polygon[poly++] = VCreatePolygon(3, vp, effectBlackColor);
	}

	for (i = 0; i < numFlame; ++i) {
		x = efrandom();
		s = copysign(1.0, x);
		x = fabs(x);
		vp[0].x = (FLAME_MIN_RADIUS + x * FLAME_RADIUS) * s;
		x = efrandom();
		s = copysign(1.0, x);
		x = fabs(x);
		vp[0].y = (FLAME_MIN_RADIUS + x * FLAME_RADIUS) * s;
		x = efrandom();
		s = copysign(1.0, x);
		x = fabs(x);
		vp[0].z = (FLAME_MIN_RADIUS + x * FLAME_RADIUS) * s;
		vp[1].x = efrandom() * SMOKE_INNER;
		vp[1].y = efrandom() * SMOKE_INNER;
		vp[1].z = efrandom() * SMOKE_INNER;
		vp[2].x = efrandom() * SMOKE_INNER;
		vp[2].y = efrandom() * SMOKE_INNER;
		vp[2].z = efrandom() * SMOKE_INNER;
#ifndef EIGHT_COLORS
		if (i < numRed)
			color = redFlameColor;
		else
			color = orangeFlameColor;
#else
		color = redFlameColor;
#endif
		obj->polygon[poly++] = VCreatePolygon(3, vp, color);
	}

	explosionTemplate = obj;

}

int								/*ARGSUSED */
placeExplosion(Viewport * v, craft * obj, VMatrix * mtx, VPolygon ** poly, long *cnt)
{

	register int i, j, k, n;
	register VPolygon **p;
	VPoint    tmp, *q;

	j = *cnt;

	n = explosionTemplate->numPolys;
	p = explosionTemplate->polygon;

	for (i = 0; i < n; ++i) {
		if (obj->flameDuration > 0 || p[i]->color == effectBlackColor) {
			poly[j] = VCopyPolygon(p[i]);
			for ((k = 0, q = poly[j]->vertex); k < poly[j]->numVtces; (++k, ++q)) {
				tmp.x = q->x * obj->escale + obj->Sg.x;
				tmp.y = q->y * obj->escale + obj->Sg.y;
				tmp.z = q->z * obj->escale + obj->Sg.z;
				*q = tmp;
			}
			VTransformPolygon(poly[j], &v->eyeSpace);
			++j;
		}
	}

	*cnt = j;

	return 0;
}

void
freeEffects(void)
{
	if (explosionTemplate) {
		VDestroyObject ( explosionTemplate );
	}
}
