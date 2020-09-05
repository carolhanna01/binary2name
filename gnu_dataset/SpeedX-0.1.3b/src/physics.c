
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

   License : GPL

   Movement Law
 */

#include "physics.h"

#include "const.h"

#include "car.h"

#include "world.h"

extern int autorun;

extern World *world;

int dep_alea = 0;

int level = 1;

int vrmax[NVEHICULES] =
{25, 50, 15};

int vmax[NVEHICULES] =		/* 100 80 60 */
{(1 << ACCEL) * 60, (1 << ACCEL) * 50, (1 << ACCEL) * 40};

int ralenti[NVEHICULES] = /* {1, 2, 3}; */
{4, 2, 3};

int accel[NVEHICULES] =
{8, 7, 6};

int vmin[NVEHICULES] =
{1, 1, 1};

int omega[NVEHICULES] =
{5, 5, 5};

int posxmax[NVEHICULES] =
{170, 170, 170};

int posxmin[NVEHICULES] =
{-120, -120, -120};

int width[NVEHICULES] =
{70, 60, 70};

int _length[NVEHICULES] =
{80, 60, 40};

void collision(Vehicule * Sorting, int j)
{

	int i;
	int p;

	{
	/* Position */
	p = NB_ADV - 1;

	for (i = 0; i < NB_ADV; i++)
		if ((Sorting[j].turn > Sorting[i].turn)
		    || ((Sorting[j].turn == Sorting[i].turn)
			&& (Sorting[j].ypos > Sorting[i].ypos)))
			//      world->Sorting[j].position--;
			p--;

	world->Sorting[j].position = p;
	/* /Position */
	}

	if (!autorun) {

		for (i = 0; i < NB_ADV; i++)
			if ((Sorting[j].ypos <= Sorting[i].ypos)
			    && (i != j)) {

				if (abs
				    ((Sorting[j].ypos) -
				     (Sorting[i].ypos)) <
				    (_length[Sorting[j].type] +
				     _length[Sorting[i].type]) / 2)
					if (abs
					    ((Sorting[j].xpos) -
					     (Sorting[i].xpos)) <
					    (width[Sorting[j].type] +
					   width[Sorting[i].type]) / 2) {

						int m, M;

						if (Sorting[i].ypos <
						    Sorting[j].ypos) {

							m = i;

							M = j;

						} else {

							m = j;

							M = i;

						}

						Sorting[M].xpos +=
						    Sorting[m].xvit;
						Sorting[m].xpos -=
						    Sorting[M].xvit
						  +(rand() % 32); 
						Sorting[m].yvit =
						    Sorting[m].yvit >> 3;
						Sorting[m].xvit +=
						    Sorting[M].xvit;
						Sorting[M].xvit +=
						    (rand() % 32) - 16;
						Sorting[M].ypos += 5;

					}
			}
	}
}

int physics(Vehicule * Sorting)
{

	int i;

	int inv_radius;


	/* Rotation and Inertia */

	for (i = 0; i < NB_ADV; i++) {

		inv_radius = world->trace[Sorting[i].ypos >> 6];

		Sorting[i].xpos += Sorting[i].xvit >> 2;

		Sorting[i].xpos -=
		    (((Sorting[i].yvit >> ACCEL) * inv_radius) >>
		     world->omega[world->num_veh]);


		if (Sorting[i].yvit != 0)
			Sorting[i].ypos +=
			    (Sorting[i].yvit >> ACCEL) -
			    abs(Sorting[i].xvit >> 3);
		
		//Sorting[i].ypos += (Sorting[i].yvit >> ACCEL);

		if (Sorting[i].ypos > ((64 * LENGTH) << 6)) {

			Sorting[i].ypos =
			    Sorting[i].ypos - ((64 * LENGTH) << 6);

			Sorting[i].turn++;

		}
		if (Sorting[i].ypos < 0) {

			Sorting[i].ypos =
			    Sorting[i].ypos + ((64 * LENGTH) << 6);

			Sorting[i].turn--;

		}
		/*

		   if (Sorting[i].ypos < 0)

		   Sorting[i].ypos = 0;

		 */

		if (Sorting[i].turn == 3) {
		  world->finish = 2;
		  //printf("End of the course !!\n");
		}

		if (dep_alea != 0)
			Sorting[i].xvit -= (((((rand() % 3) - 1) *
					      16 + Sorting[i].xvit) * (Sorting[i].yvit >> ACCEL)) >> 7);

		/*
		   Road limits
		 */

		if ((Sorting[i].xpos) < posxmin[Sorting[i].type]) {

			(Sorting[i].xpos) =
			    posxmin[Sorting[i].type] + 20 - Sorting[i].xvit;	// / 2;

			(Sorting[i].xvit) = 1;

			(Sorting[i].yvit) = 0;	//(Sorting[i].yvit) >> 4;

		}
		if ((Sorting[i].xpos) > posxmax[Sorting[i].type]) {

			(Sorting[i].xpos) =
			    posxmax[Sorting[i].type] - 20 + Sorting[i].xvit;	// / 2;

			(Sorting[i].xvit) = -1;

			(Sorting[i].yvit) = 0;	//(Sorting[i].yvit) >> 4;

		}
		/*
		   Speed Max
		 */

		if ((Sorting[i].xvit) < -world->vrmax[Sorting[i].type])
			(Sorting[i].xvit) = -world->vrmax[Sorting[i].type];

		if ((Sorting[i].xvit) > world->vrmax[Sorting[i].type])
			(Sorting[i].xvit) = world->vrmax[Sorting[i].type];

		if (Sorting[i].autopilot == 0) {
			if ((Sorting[i].yvit) >
			    world->vmax[Sorting[i].type])
				(Sorting[i].yvit) =
				    world->vmax[Sorting[i].type];
		} else {
			if ((Sorting[i].yvit) > (14 + level) *
			    (world->vmax[Sorting[i].type] >> 4) + (1 + level) * (i - 6))
				(Sorting[i].yvit) = (14 + level) * (world->vmax[Sorting[i].type] >> 4)
				    + (1 + level) * (i - 6);
			/*
			if ((Sorting[i].yvit) > (14 + level) *
			    (world->vmax[Sorting[i].type] >> 4) + 6 - i)
				(Sorting[i].yvit) = (14 + level) * (world->vmax[Sorting[i].type] >> 4)
				    + 6 - i ;
			if ((Sorting[i].yvit) > 
			   (14 + level) * world->vmax[Sorting[i].type] - 16*(5 -i - level))
				(Sorting[i].yvit) = (14 + level) *world->vmax[Sorting[i].type] - 16*(5 -i - level);
			*/
		}

		if ((Sorting[i].yvit) < -10)
			(Sorting[i].yvit) = -10;

		/*
		   Slow down
		 */

		if ((Sorting[i].xvit) < 0) {

			(Sorting[i].xvit) += ralenti[Sorting[i].type];

			if ((Sorting[i].xvit) > 0)
				(Sorting[i].xvit) = 0;

		} else if ((Sorting[i].xvit) > 0) {

			(Sorting[i].xvit) -= ralenti[Sorting[i].type];

			if ((Sorting[i].xvit) < 0)
				(Sorting[i].xvit) = 0;

		}

		if ((Sorting[i].yvit) < 0) {

			(Sorting[i].yvit) += ralenti[Sorting[i].type];

			if ((Sorting[i].yvit) > 0)
				(Sorting[i].yvit) = 0;

		} else if ((Sorting[i].yvit) > 0) {

			(Sorting[i].yvit) -= ralenti[Sorting[i].type];

			if ((Sorting[i].yvit) < 0)
				(Sorting[i].yvit) = 0;

		}
	}

	//collision (Sorting,0);

	return 0;

}
