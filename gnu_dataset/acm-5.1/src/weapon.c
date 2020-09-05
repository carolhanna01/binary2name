/*
 *  acm : an aerial combat simulator for X
 *  Copyright (C) 1991-1996  Riley Rainey
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

#include <string.h>
#include "pm.h"

/*
 *  countOrdinance : count a particular type of ordinance on an
 *      aircraft.
 */

int
countOrdinance(craft * c, char *type)
{

	register int i, count = 0;

	for (i = 0; i < c->cinfo->sCount; ++i) {
		if (strcmp(c->station[i].type, type) == 0)
			++count;
	}

	return count;
}

/*
 *  readyStation:  return the index of the first station with
 *         a particular type of ordinance.
 */

int
readyStation(craft * c, char *type)
{

	register int i;

	for (i = 0; i < c->cinfo->sCount; ++i) {
		if (strcmp(c->station[i].type, type) == 0)
			return i;
	}

	return -1;
}

int
selectWeapon(craft * c)
{

	register int n, m;

	m = c->curWeapon;
	n = (c->curWeapon + 1) % WEAPONTYPES;
	for (; n != m; n = (n + 1) % WEAPONTYPES) {
		if (wtbl[n].select != NULL)
			if ((*wtbl[n].select) (c) == 1) {
				c->curWeapon = n;
				return 1;
			}
	}

	return 0;
}

int
selectNamedWeapon(craft * c, int id)
{
	int       n;

	for (n = 0; n < WEAPONTYPES; ++n) {
		if (wtbl[n].mask == id && (*wtbl[n].select) (c) == 1) {
			c->curWeapon = n;
			return 1;
		}
	}
	return 0;
}

int
fireWeapon(craft * c)
{

	if (wtbl[c->curWeapon].firePress != NULL)
		return (*wtbl[c->curWeapon].firePress) (c);

	return 0;
}

int
ceaseFireWeapon(craft * c)
{

	if (wtbl[c->curWeapon].fireRelease != NULL)
		return (*wtbl[c->curWeapon].fireRelease) (c);

	return 0;
}

int
doWeaponDisplay(craft * c, viewer * u, int *x, int *y)
{

	int       i;

	if (wtbl[c->curWeapon].display != NULL)
		return (*wtbl[c->curWeapon].display) (c, wtbl[c->curWeapon].w, u, x, y);
	else {
		for (i = 0; i < 6; i++)
			strcpy(c->leftHUD[i], "");
	}

	return 0;
}

int
doWeaponUpdate(craft * c)
{

	if (wtbl[c->curWeapon].update != NULL)
		return (*wtbl[c->curWeapon].update) (c);

	return 0;
}
