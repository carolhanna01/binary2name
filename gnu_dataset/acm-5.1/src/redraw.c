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

#include "pm.h"

int
redrawItem(drawnItem * p)
{
	p->redraw = 4;
	return 0;
}

_BOOL
isRedrawRequired(drawnItem * p)
{
	_BOOL      result = FALSE;

	if (p->redraw > 0) {
		--p->redraw;
		result = TRUE;
	}
	return result;
}

void
initializeDrawnItemController(drawnItemController * c)
{
	c->head = NULL;
}

void
addDrawnItem(drawnItemController * c, drawnItem * p)
{
	p->next = c->head;
	p->redraw = 0;
	c->head = p;
}

void
redrawAllItems(drawnItemController * c)
{
	drawnItem *p;

	for (p = c->head; p; p = p->next) {
		redrawItem(p);
	}
}
