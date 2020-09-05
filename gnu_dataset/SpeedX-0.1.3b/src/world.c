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

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/shm.h>
#include <sys/ipc.h>

//#include <sys/timex.h>

#include "world.h"
#include "param.h"
#include "course.h"
#include "physics.h"
#include "const.h"
#include "loop.h"

extern int accel[];
extern int vmax[];
extern int vrmax[];
extern int omega[];
World *world;
extern int player;

XEvent XEv;

unsigned int etat;

int num_dpy = -1;

struct shmid_ds buf;

int shmid;

void run_world(void)
{

	int i;

	world->finish = 0;
	while (!world->finish) {

		if (world->refresh == REFRESH_DONE)
			world->refresh = CAN_REFRESH;

		/* Pause */

		while (!world->running) {

			usleep(1000);

		}

		for (i = 0; i < NB_ADV; i++) {	
		  //fprintf(stderr,"speed %d = %d\n",i,world->Sorting[i].yvit);

			if (world->Sorting[i].autopilot) {

				world->Sorting[i].xvit +=
				    (15 + ((i % 4) * 8) -
				     world->Sorting[i].xpos) * C0
				    - (C1 -
				     i * 0.001) * world->Sorting[i].xvit;

				if (abs
				    (world->Sorting[i].xpos -
				     world->Sorting[i - 1].xpos) < 32)
					world->Sorting[i].xvit +=
					   (world->Sorting[i].xpos -
					   world->Sorting[i -
					   1].xpos) * C0 +
					    (rand() % 32) - 16;
				world->Sorting[i].yvit +=
				    (world->accel[world->num_veh]);

				if (world->Sorting[i].yvit >
				    world->vmax[world->num_veh])
					world->Sorting[i].yvit =
					    world->vmax[world->num_veh];

			} else {

				if ((world->mask[i] & LEFT) != 0) {

					world->Sorting[i].xvit -=
					    world->accel[world->num_veh];

				}
				if ((world->mask[i] & RIGHT) != 0) {

					world->Sorting[i].xvit +=
					    world->accel[world->num_veh];

				}
				if ((world->mask[i] & UP) != 0) 
				{
					world->Sorting[i].yvit +=
						world->accel[world->num_veh];
				}
				if ((world->mask[i] & DOWN) != 0) {

					world->Sorting[i].yvit -=
					    world->accel[world->num_veh];

				}
			}

		}

		//      printf("ypos = %d\n",world->Sorting[0].ypos);
		physics(world->Sorting);

		for (i = 0; i < NB_ADV; i++)
			collision(world->Sorting, i);

		usleep(1000000 / TPS);

	}
	shmdt(world);

}

void start_world(void)
{

	printf("World birth ...\n");

	world->running = 1;

	world->refresh = CAN_REFRESH;

}

void pause_world(void)
{

	printf("World Pause ...\n");

	world->running = 0;

}

void continue_world(void)
{

	printf("World wake up...\n");

	world->running = 1;

}

void init_world(void)
{

	pid_t pid;

	key_t cle;


	int i;

	printf(". World Initialisation ...");

	fflush(stdout);

	/* Code to run multiple independant speedx  */
/*
   {
   int key_id = 0;
   do {
   cle = ftok(".", key_id);
   key_id++;
   } while ((shmid = shmget(cle, sizeof(World), 0666 | IPC_CREAT)) == -1);
   }
 */
	/* Code to run multiple dependant speedx */
	cle = ftok(".", 0);
	shmid = shmget(cle, sizeof(World), 0777);
	if (shmid != -1) {
		/* The shared memory already exists */

		/* We must verify if there are processus attached.
		 * If not, it's surely due to a game crash.
		 * So, we have to reinitialize it to be a server.
		 */

		shmctl(shmid, IPC_STAT, &buf);
		shmctl(shmid, IPC_RMID /*| SHM_UNLOCK*/, 0);

		world = shmat(shmid, 0, 0);
		if (buf.shm_nattch != 0) {
			/* If we are dependant of a world , we can exit */
			player = -1;
			for (i = 0; i < NB_ADV; i++) {
				if (world->Sorting[i].autopilot == 1) {
					world->Sorting[player = i].autopilot = 0;
					break;
				}
			}
			return;
		} else {
		}
	} else {
		shmid = shmget(cle, sizeof(World), 0777 | IPC_CREAT);
		if (shmid == -1) {
			printf("Error : (Bad Allocation of a shared memory segment)\n");
			shmctl(shmid, IPC_RMID, 0);
			exit(1);
		}
		/* End of dependant code */
		world = shmat(shmid, 0, 0);
		/* We have to destroy the shared memory at the end */
		//shmctl(shmid, SHM_UNLOCK, 0);
	}
	if (world == (World *) - 1) {

		printf("Error (Bad Attached memory shared)\n");

		exit(1);

	}
	world->running = 0;
	world->finish = 0;
	for (i = 0; i < NVEHICULES; i++) {
		world->accel[i] = accel[i];
		world->vmax[i] = vmax[i];
		world->vrmax[i] = vrmax[i];
		world->omega[i] = omega[i];
	}

	init_course();

	//world->refresh = REFRESH_BUSY;

	pid = fork();

	switch (pid) {

	case -1:

		printf("Error (fork)\n");

		exit(1);

	case 0:

		/* Son */
		sleep(4);

		run_world();

		exit(0);

		break;

	default:

		/* Father */

		printf("OK\n");


		break;

	}

}

void stop_world(void)
{
	if (player == 0) {	/* This is the server */
		printf("World stopped...");
		fflush(stdout);

		world->finish = 1;
		shmctl(shmid, IPC_RMID, 0);
		shmdt(world);

		printf("OK\n");
	} else {		/* this is a client */
		shmdt(world);
	}

}
