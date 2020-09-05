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
 *  along with this program; if not, write to the Free Software Foundation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifndef _alarm_h
#define _alarm_h

typedef long alarm_id_t;

typedef struct _alarm_descriptor_t {
	struct _alarm_descriptor_t *next;
	double    delta;
	alarm_id_t alarm_id;
	void      (*proc) ();
	char     *arg1;
	char     *arg2;
} alarm_descriptor_t;

extern alarm_id_t addAlarm(double delta, void (*proc) (char *, char *), char *arg1, char *arg2);
extern double cancelAlarm(alarm_id_t id);

#endif
