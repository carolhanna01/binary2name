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

#if !((defined(__GNUC__) || defined(__STDC__) || defined(_WINDOWS)) && !defined(_NO_INLINE))

/*
 * VTransform: transform a point from one coordinate system to another.
 */

void
VTransform(pt, mt, newPt)
VPoint   *pt;
VMatrix  *mt;
VPoint   *newPt;
{

	newPt->x = pt->x * mt->m[0][0] + pt->y * mt->m[0][1]
		+ pt->z * mt->m[0][2] + mt->m[0][3];

	newPt->y = pt->x * mt->m[1][0] + pt->y * mt->m[1][1]
		+ pt->z * mt->m[1][2] + mt->m[1][3];

	newPt->z = pt->x * mt->m[2][0] + pt->y * mt->m[2][1]
		+ pt->z * mt->m[2][2] + mt->m[2][3];
}

void
VReverseTransform(pt, mt, newPt)
VPoint   *pt;
VMatrix  *mt;
VPoint   *newPt;
{
	VPoint    tmp;

	tmp.x = pt->x - mt->m[0][3];
	tmp.y = pt->y - mt->m[1][3];
	tmp.z = pt->z - mt->m[2][3];

	newPt->x = tmp.x * mt->m[0][0] + tmp.y * mt->m[1][0]
		+ tmp.z * mt->m[2][0];

	newPt->y = tmp.x * mt->m[0][1] + tmp.y * mt->m[1][1]
		+ tmp.z * mt->m[2][1];

	newPt->z = tmp.x * mt->m[0][2] + tmp.y * mt->m[1][2]
		+ tmp.z * mt->m[2][2];
}

void
VTransform_(pt, mt, newPt)
VPoint   *pt;
VMatrix  *mt;
VPoint   *newPt;
{

	newPt->x = pt->x * mt->m[0][0] + pt->y * mt->m[0][1]
		+ pt->z * mt->m[0][2];

	newPt->y = pt->x * mt->m[1][0] + pt->y * mt->m[1][1]
		+ pt->z * mt->m[1][2];

	newPt->z = pt->x * mt->m[2][0] + pt->y * mt->m[2][1]
		+ pt->z * mt->m[2][2];
}

/*
 *  Apply the reverse of a given transformation
 */

void
VReverseTransform_(pt, mt, newPt)
VPoint   *pt;
VMatrix  *mt;
VPoint   *newPt;
{

	newPt->x = pt->x * mt->m[0][0] + pt->y * mt->m[1][0]
		+ pt->z * mt->m[2][0];

	newPt->y = pt->x * mt->m[0][1] + pt->y * mt->m[1][1]
		+ pt->z * mt->m[2][1];

	newPt->z = pt->x * mt->m[0][2] + pt->y * mt->m[1][2]
		+ pt->z * mt->m[2][2];
}

#endif
