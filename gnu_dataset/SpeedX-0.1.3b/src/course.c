/*
 * // SpeedX //
 *
 *  medernac@isty-info.uvsq.fr
 *
 *  Copyright (C) 2000 
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

/*
   Version : 0.1.2c

   Course parameters 
 */

#include <stdio.h>

#include "const.h"

#include "course.h"

#include "world.h"

extern World *world;

void init_course()
{

	int i;

	int j;


	for (i = 0; i < NB_ADV; i++) {
		world->Sorting[i].num_car = i;
		world->Sorting[i].type = 0;
		world->Sorting[i].autopilot = 1;
		world->Sorting[i].ypos = STARTLINE * i;
		world->Sorting[i].xpos =
		    (((i % 2) == 1) ? (RANK1) : (RANK2));
		world->Sorting[i].turn = 0;
		world->Sorting[i].position = NB_ADV - i;
		world->Sorting[i].xvit = \
		    world->Sorting[i].yvit = 0;
	}

	//j = rand() % NB_ADV ;
	j = 0 ;
	world->Sorting[j].ypos = 0;
	world->Sorting[j].xpos = 0;

	world->Sorting[0].ypos = STARTLINE * j;
	world->Sorting[0].xpos =
		(((j % 2) == 1) ? (RANK1) : (RANK2));

	

	for (i = 0; i < multi + 1; i++)
		world->Sorting[i].autopilot = 0;

	/* Creation of the road */
	/* Horizontal curve */
	/* 64 */

	for (i = 0; i < LENGTH - 1; i++) {
		for (j = 0; j < 64; j++) {
			world->trace[64 * i + j] =
			    ((_trace[i + 1] * j) +
			     (_trace[i] * (64 - j))) / 64;
		}
	}

	for (j = 0; j < 64; j++) {
		world->trace[64 * LENGTH + j - 64] =
		    ((_trace[0] * j) +
		     (_trace[LENGTH - 1] * (64 - j))) / 64;
	}
	/* vertical curve */

	for (i = 0; i < LENGTH - 1; i++) {
		for (j = 0; j < 64; j++) {
			world->curve[64 * i + j] =
			    ((_trace2[i + 1] * j) +
			     (_trace2[i] * (64 - j))) / 64;
		}
	}

	for (j = 0; j < 64; j++) {
		world->curve[64 * LENGTH + j - 64] =
		    ((_trace2[0] * j) +
		     (_trace2[LENGTH - 1] * (64 - j))) / 64;
	}
}
