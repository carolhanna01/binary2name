/*
 *  acm : an aerial combat simulator for X
 *  Copyright (C) 1991-1994,1997  Riley Rainey
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

#include <math.h>

#define	GM	1.4					/* ratio of specific heats for air */
#define P0	2116.22				/* sea-level pressure */
#define TA	459.67				/* CONVERSION TEMPERATURE FROM F TO R */
#define T0	(TA + 59.0)			/* sea-level temperature */
#define	G	32.1741				/* gravitational acceleration [ft/sec^2] */
#define RU	(1545.31 * G)		/* universal gas constant */
#define MA	28.9644				/* molecular weight for air [lbm/lbm-mole] */
#define	RA	(RU / MA)			/* gas constant for air [ft^2/sec^2/R] */

/*
 *  airProperties :  Compute rho and the speed of sound for the standard
 *              atmosphere up to 60,000 feet.
 */

void
airProperties(double h, double *rho, double *mach1)
{

	double    t, p;

	if (h < 36089.2) {
		t = T0 - 3.56616e-3 * h;
		p = P0 * pow(t / T0, 5.255912);
	}
	else {
		t = 389.97;
		p = 472.6773 * exp(-G * (h - 36089.2) / (RA * t));
	}

	*rho = p / (RA * t);
	*mach1 = sqrt(GM * RA * t);
}
