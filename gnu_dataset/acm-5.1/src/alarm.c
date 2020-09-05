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
#include "alarm.h"
#include <sys/param.h>
#include <stdlib.h>

static alarm_descriptor_t *alarm_list = NULL;
static alarm_id_t next_alarm_id = 0;

void
alarmCheck(double delta)
{
	double    d = delta;
	alarm_descriptor_t *p, *list = NULL, *list1;

	while (alarm_list != (alarm_descriptor_t *) NULL && d > 0.0) {
		if ((d = alarm_list->delta - d) <= 0.0) {
			p = alarm_list;
			alarm_list = alarm_list->next;
			if (list) {
				p->next = list;
				list = p;
			}
			else {
				p->next = NULL;
				list = p;
			}
			d = (d < 0.0) ? -d : 0.0;
		}
		else {
			alarm_list->delta = d;
			break;
		}
	}

	while (list) {
		(*list->proc) (list->arg1, list->arg2);
		list1 = list->next;
		free((char *) list);
		list = list1;
	}

}

alarm_id_t
addAlarm(double delta, void (*proc) (char *, char *), char *arg1, char *arg2)
{
	alarm_descriptor_t *n, *prev, *p;

	n = (alarm_descriptor_t *) malloc(sizeof(alarm_descriptor_t));
	/* per PREfix */
	if (n == NULL) {
		printf("unable to allocate alarm descriptor\n");
		exit(1);
	}
	n->alarm_id = next_alarm_id++;
	n->delta = delta;
	n->proc = proc;
	n->arg1 = arg1;
	n->arg2 = arg2;
	n->next = NULL;

	if ((p = alarm_list) == NULL) {
		alarm_list = n;
	}
	else {
		prev = NULL;

		while (n->delta > p->delta) {
			n->delta -= p->delta;
			prev = p;
			if ((p = p->next) == NULL) {
				prev->next = n;
				return n->alarm_id;
			}
		}

		if (prev == NULL) {
			n->next = alarm_list;
			n->next->delta -= n->delta;
			alarm_list = n;
		}
		else {
			n->next = prev->next;
			n->next->delta -= n->delta;
			prev->next = n;
		}
	}

	return n->alarm_id;
}

/*
 *  cancelAlarm returns the amount of time remaining until the cancelled
 *  alarm would have "gone-off" (or -1.0, if the alarm was not found).
 */

double
cancelAlarm(alarm_id_t id)
{
	alarm_descriptor_t *p, *last = NULL;
	double    result = 0.0;

/*
 *  Attempt to locate the specified alarm by its alarm_id.
 */

	for (p = alarm_list; p;) {
		if (p->alarm_id == id) {

/*
 *  Unlink this alarm descriptor
 */

			if (last) {
				last->next = p->next;
			}
			else {
				alarm_list = p->next;
			}

/*
 *  Correct the next decriptor's delta value, free this descriptor and return.
 */

			if (p->next) {
				p->next->delta += p->delta;
			}
			result += p->delta;
			free((char *) p);
			return result;
		}

		result += p->delta;
		last = p;
		p = p->next;
	}

/*
 *  Alarm not found, return -1.0.
 */

	return -1.0;
}
