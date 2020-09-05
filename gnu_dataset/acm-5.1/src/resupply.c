/*
 *  acm : an aerial combat simulator for X
 *  Copyright (C) 1991,1992  Riley Rainey
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

#include <math.h>
#include "pm.h"
#include "alarm.h"

/*
 *  resupplyCheck :  If a player is on the airport grounds and motionless,
 *           then invoke the plane's resupply procedure.
 */

/* ARGSUSED */
void
resupplyCheck(char *arg1, char *arg2)
{

	craft    *c;
	int       i;
	double    d;

#ifdef FLAT_WORLD
	double    x, y, z;

#endif
	alarm_id_t id;

	for ((i = 0, c = &ptbl[0]); i < MAXPLAYERS; (++i, ++c)) {

		if (c->type != CT_PLANE)
			continue;

		if (mag(c->Cg) < 5.0) {
#ifdef FLAT_WORLD
			x = c->Sg.x - teamLoc[c->team].x;
			y = c->Sg.y - teamLoc[c->team].y;
			z = c->Sg.z - teamLoc[c->team].z;
			d = sqrt(x * x + y * y + z * z);
#else
			d = 0.0;
#endif
			if (d <= MAX_GROUND_DISTANCE)
				(*c->cinfo->resupply) (c);
			(*wtbl[c->curWeapon].select) (c);
		}
	}

	id = addAlarm(RESUPPLY_INTERVAL, resupplyCheck, NULL, NULL);
}
