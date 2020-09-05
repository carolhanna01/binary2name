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

#ifndef __WORLD_H

#define __WORLD_H



#include "car.h"

#include "const.h"



void init_world(void);

void start_world(void);

void stop_world(void);

void pause_world(void);

void continue_world(void);



typedef struct World {
	int running;
	int finish;
	int multi;

	Vehicule Sorting[NB_ADV];
	int accel[NVEHICULES];
	int vmax[NVEHICULES];
	int num_veh;
	int omega[NVEHICULES];
	int vrmax[NVEHICULES];
	int trace[LENGTH * 64];
	int curve[LENGTH * 64];
	//int position;
	int  mask[NB_ADV];
	int refresh;

} World;



#define CAN_REFRESH 0

#define REFRESH_BUSY 1

#define REFRESH_DONE 2



#endif /* __WORLD_H */

