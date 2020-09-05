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

void
VMatrixMult(Mt1, Mt2, R)
VMatrix  *Mt1, *Mt2, *R;
{

	register int I, J, K;
	register double x;

	for (I = 0; I < 4; ++I)
		for (J = 0; J < 4; ++J) {
			x = 0.0;
			for (K = 0; K < 4; ++K)
				x += Mt1->m[K][J] * Mt2->m[I][K];
			R->m[I][J] = x;
		}
}

#endif

#define ZEROFOURTH

void
VMatrixMultByRank(VMatrix * Mt1, VMatrix * Mt2, VMatrix * R, int rank)
{

	register int I, J, K, r = rank;
	register double x;

	for (I = 0; I < r; ++I)
		for (J = 0; J < r; ++J) {
			x = 0.0;
			for (K = 0; K < r; ++K)
				x += Mt1->m[K][J] * Mt2->m[I][K];
			R->m[I][J] = x;
		}

#ifdef ZEROFOURTH
	R->m[0][3] = R->m[1][3] = R->m[2][3] = 0.0;
	R->m[3][0] = R->m[3][1] = R->m[3][2] = 0.0;
	R->m[3][3] = 1.0;
#endif
}
