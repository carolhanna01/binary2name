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
#include <math.h>

VMatrix  *
VRotate(VMatrix * Mt1, int operation, double angle)
{

	VMatrix   m, s;

	VIdentMatrix(&m);

	switch (operation) {
	case XRotation:
		m.m[1][1] = m.m[2][2] = cos(angle);
		m.m[2][1] = sin(angle);
		m.m[1][2] = -m.m[2][1];
		break;
	case YRotation:
		m.m[0][0] = m.m[2][2] = cos(angle);
		m.m[2][0] = sin(angle);
		m.m[0][2] = -m.m[2][0];
		break;
	case ZRotation:
		m.m[0][0] = m.m[1][1] = cos(angle);
		m.m[1][0] = sin(angle);
		m.m[0][1] = -m.m[1][0];
		break;
	}

	s = *Mt1;

	VMatrixMult(&s, &m, Mt1);
	return Mt1;
}

VMatrix  *
VTranslatePoint(VMatrix * Mt, VPoint loc)
{

	Mt->m[0][3] += loc.x;
	Mt->m[1][3] += loc.y;
	Mt->m[2][3] += loc.z;
	return Mt;

}

VMatrix  *
VTranslate(VMatrix * Mt, double x, double y, double z)
{

	Mt->m[0][3] += x;
	Mt->m[1][3] += y;
	Mt->m[2][3] += z;
	return Mt;
}
