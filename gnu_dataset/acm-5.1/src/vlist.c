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

#include <pm.h>

/*
 *  Get the number of local viewers in the simulation
 */

int
getViewerCount(void)
{
	return vl_count;
}

/*
 *  Add a new view to the head of the viewer list
 */

void
addViewer ( viewer *v )
{
	v->vl_next = vl_head;
	v->vl_prev = NULL;

	if (vl_head == NULL) {
		vl_tail = v;
	}

	vl_head = v;
	++vl_count;
}

/*
 *   Remove a viewer from the viewer list
 *   Returns zero on success; one if the entry wasn't found.
 */

int
removeViewer ( viewer *v )
{
	viewer *cur, *last = NULL;

	for (cur=vl_head; cur != NULL; cur=cur->vl_next) {
		if (cur == v) {
			/* not the first entry on the list ? */
			if (last) {
				last->vl_next = cur->vl_next;
			}
			/* first entry on the list */
			else {
				vl_head = cur->next;
			}
			/* not the last entry on the list */
			if (cur->vl_next) {
				cur->vl_next->vl_prev = last;
			}
			/* last entry on the list */
			else {
				vl_tail = last;
			}
			-- vl_count;
			return 0;
		}
		last = cur;
	}
	return 1;
}
