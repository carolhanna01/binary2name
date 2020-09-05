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

   Event Loop
 */

#include "std.h"

#include "loop.h"

#include "param.h"

#include "const.h"

#if LIBMIKMOD

#include <mikmod.h>

#endif

#include "world.h"

#include "effects.h"

#include "perf.h"

int player;

/*
   keyboard event are managed with KeyPress and KeyRelease
 */

XEvent XEv;

int inv_radius = 0;

int counter = 0;

int drawing = 1;

/*
   Road drawing .. (with inverse curvure radius ...)
 */

/* vertical */
int _trace2[] =
//{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
{30, 40, 60, 40, 30, 50, 60, 30, 20, 30, 20, 20, 10, 40, 20, 50,
 30, 20, 30, 40, 30, 50, 60, 30, 20, 30, 20, 20, 30, 40, 20, 50};

/* horizontal */
int _trace[] =
{0, 0, -3, 6, -5, 4, -3, 0, 0, -4, -5, 3, 0, -5, 3, -5,
 3, 0, -4, 7, 0, -5, 2, 0, -5, 0, -5, 5, -2, 0, -1, 0};

/*

   {0, 5, -10, 5, 0, 15, -10, 5, -15, 10, 0, 10, -15, 5, -10, 0};

 */

int road_length = LENGTH;	//128;

/*
   Camera is located at 50 units back.
 */

float gros = 1.4;

int End = 0;

extern int CompletionType;

extern int mikmod;

extern World *world;

unsigned int touche;

int loop(Display * dpy)
{
  int finish = 0;

  unsigned int etat;	// meta keys see ControlMask in XKeyEvent

#if LIBMIKMOD

	MODULE *module = NULL;

#endif

	int num_dpy = -1;

	int i;
	int refresh_ready;

	float g;

#if LIBMIKMOD

	/* Music loading */

	if (mikmod) {

		fprintf(stderr, "Music Loading ...");

		fflush(stdout);
		{
			char *music_name = NULL;
			music_name = load_music();
			if (music_name == NULL) {
				fprintf(stderr, "No music found\n");
			} else {
				module = Player_Load(music_name, 64, 0);
				free(music_name);
			}
		}
		if (module) {

			printf("OK\n");

			/* start module */

			module->wrap = 1;

			Player_Start(module);

			fprintf(stderr, " Music Starting\n");

		} else
			printf("Error: Player_Load \n");

	}
#endif

	for (i = 0; i < NB_ADV; i++)
		world->mask[i] = 0;

	fprintf(stderr, "Player : %d\n", player);


/*
   Every 10 seconds, we compute the Frame Per Seconds
 */

	if (DEBUG) {

		signal(SIGALRM, fps);

		alarm(10);

	}
	refresh_ready = CAN_REFRESH;


	menu(screen_buffer, dpy);
	start_world();

/* EVENTS LOOP */

	while (!finish) {
		/* we wait a little */
#if WAIT
		{
			struct timeval tv;

			tv.tv_sec = 0;
			tv.tv_usec = WAIT;	//WAIT

			select(1, NULL, NULL, NULL, &tv);
		}
#endif
		/* drawing OK */

		if (refresh_ready == CAN_REFRESH) {

			counter++;

			refresh_ready = REFRESH_BUSY;

#if LIBMIKMOD

			if (module && Player_Active())
				MikMod_Update();

#endif

			inv_radius =
			    world->trace[(world->Sorting[player].ypos >> 6) %
					 (64 * road_length)];

			refresh(screen_buffer, sky, inv_radius, player);	//#75

			for (i = 0; i < NB_ADV; i++) {

				/* 
				   We manage the rotation in the case of a
				   reverse gear
				 */

				int sens = 0;

				if (world->Sorting[i].yvit >= 0)
					sens = 1;

				else if (world->Sorting[i].yvit <= 0)
					sens = -1;
				/*  Cars are drawned according to their position and with their
				   acceleration */
				g = ((world->Sorting[i].ypos -
				      world->Sorting[player].ypos + (world->Sorting[player].yvit >> 5) +
				      CAMERA)) % ((64 * LENGTH) << 6);

				g = (g / HVISION) + 1;

				if (g > 1.2)
					Draw_car(screen_buffer, world->num_veh,
						 ((world->Sorting[i].xvit >> 3) - ((world->Sorting[i].xpos -
										    world->Sorting[player].xpos) / 64)) * sens, world->Sorting[i].xpos
						 ,g);
			}
			/* { int fog_power = 255; fog(fog_power,fog_power,fog_power); } */
			if (dble == 0) {

				aff(win);

			} else
				aff2(win);

		}
		/*
		   A loss of focus results in a Pause
		 */
/*
   if (XCheckMaskEvent(dpy, FocusChangeMask, &XEv))
   if (XEv.type == FocusOut) {

   perror("Focus OUT\n");

   //pause_world();

   for (;;) {

   XNextEvent(dpy, &XEv);

   if (XEv.type == FocusIn) {

   perror("Focus IN\n");

   //continue_world();

   world->refresh =
   CAN_REFRESH;

   break;

   }
   }

   }
 */
		if (!mit_shm)
			refresh_ready = CAN_REFRESH;

		while (XPending(dpy)) {
			XNextEvent(dpy, &XEv);
#ifdef MITSHM
			if (mit_shm) {
				if (XEv.type == CompletionType) {
					refresh_ready = CAN_REFRESH;
					break;
				}
			}
#endif				/* MITSHM */

			touche = XEv.xkey.keycode;
			etat = XEv.xkey.state;

			/*
			   We have to know in which window the event occur.
			 */

			if (XEv.xkey.display == dpy)
				num_dpy = 0;

			else
				num_dpy = 1;

			switch (XEv.xkey.type) {

			case KeyPress:

				if (touche == KEY_UP) {

					world->mask[player] =
					    world->mask[player] & (~DOWN);

					world->mask[player] =
					    world->mask[player] | UP;

				}
				if (touche == KEY_DOWN) {

					world->mask[player] =
					    world->mask[player] & (~UP);

					world->mask[player] =
					    world->mask[player] | DOWN;
				}
				if (touche == KEY_RIGHT) {

					world->mask[player] =
					    world->mask[player] & (~LEFT);

					world->mask[player] =
					    world->mask[player] | RIGHT;

				}
				if (touche == KEY_LEFT) {

					world->mask[player] =
					    world->mask[player] &
					    (~RIGHT);

					world->mask[player] =
					    world->mask[player] | LEFT;

				}
				if (touche == KEY_UP2) {

					world->mask[1] =
					    world->mask[1] & (~DOWN);

					world->mask[1] =
					    world->mask[1] | UP;

				}
				if (touche == KEY_DOWN2) {

					world->mask[1] =
					    world->mask[1] & (~UP);

					world->mask[1] =
					    world->mask[1] | DOWN;

				}
				if (touche == KEY_RIGHT2) {

					world->mask[1] =
					    world->mask[1] & (~LEFT);

					world->mask[1] =
					    world->mask[1] | RIGHT;

				}
				if (touche == KEY_LEFT2) {

					world->mask[1] =
					    world->mask[1] & (~RIGHT);

					world->mask[1] =
					    world->mask[1] | LEFT;

				}
				if (touche == KEY_ESC) {

					/* Draw a menu or erase it */
				  

					perror("Esc : Menu");

				}
				break;

			case KeyRelease:

				if (touche == KEY_UP)
					world->mask[player] =
					    world->mask[player] & (~UP);

				if (touche == KEY_DOWN)
					world->mask[player] =
					    world->mask[player] & (~DOWN);

				if (touche == KEY_RIGHT)
					world->mask[player] =
					    world->mask[player] &
					    (~RIGHT);

				if (touche == KEY_LEFT)
					world->mask[player] =
					    world->mask[player] & (~LEFT);

				if (touche == KEY_UP2)
					world->mask[1] =
					    world->mask[1] & (~UP);

				if (touche == KEY_DOWN2)
					world->mask[1] =
					    world->mask[1] & (~DOWN);

				if (touche == KEY_RIGHT2)
					world->mask[1] =
					    world->mask[1] & (~RIGHT);

				if (touche == KEY_LEFT2)
					world->mask[1] =
					    world->mask[1] & (~LEFT);

				break;

				if (touche == KEY_ESC) {

					perror("Esc");

				}
				break;

			}

			if ((touche == KEY_QUIT) || (world->finish)) {
				finish = 1;
				if (player == 0)
					world->finish = 1;
			}
		}

	}
	/* menu score */
	printf("Position = %d\n",world->Sorting[0].position);
	/* */
	if (world->finish == 1) {
		printf("The server quit the game.\n");
		printf("Everybody must quit !\n");
	}
	stop_world();

#if LIBMIKMOD

	Player_Stop();

	Player_Free(module);

	MikMod_Exit();

#endif

	return 0;

}

int auto_run(void)
{
	int refresh_ready;

#if LIBMIKMOD

	MODULE *module = NULL;

#endif

	world->Sorting[0].autopilot = 1;

#if LIBMIKMOD

	/* Music loading */

	if (mikmod) {

		fprintf(stderr, "Music loading ...");

		fflush(stdout);
		{
			char *music_name = NULL;
			music_name = load_music();
			if (music_name == NULL) {
				fprintf(stderr, "No music found\n");
			} else {
				module = Player_Load("music.mod", 64, 0);
				free(music_name);
			}
		}
		if (module) {

			printf("OK\n");

			/* start module */

			module->wrap = 1;

			Player_Start(module);

			fprintf(stderr, "Démarrage de la music\n");

		} else
			printf("Error : Player_Load\n");

	}
#endif

	start_world();

	if (DEBUG) {

		signal(SIGALRM, fps);

		alarm(10);

	}
	refresh_ready = CAN_REFRESH;

	for (;;) {

#if LIBMIKMOD

		if (module && Player_Active())
			MikMod_Update();

#endif

		if (refresh_ready == CAN_REFRESH) {
			refresh_ready = REFRESH_BUSY;
			counter++;

			inv_radius =
			    world->trace[(world->Sorting[0].ypos >> 6) %
					 (64 * road_length)];

			refresh(screen_buffer, sky, inv_radius, 0);
			Draw_car(screen_buffer, world->num_veh,
				 world->Sorting[0].xvit >> 3,
				 world->Sorting[0].xpos, gros);
			fog(255, 255, 255);

			if (dble == 0)
				aff(win);
			else
				aff2(win);

			if (!mit_shm)
				refresh_ready = CAN_REFRESH;

			while (XPending(display)) {
				XNextEvent(display, &XEv);
#ifdef MITSHM
				if (mit_shm) {
					if (XEv.type == CompletionType) {
						refresh_ready = CAN_REFRESH;
						break;
					}
				}
#endif
			}
		}
	}
	stop_world();

#if LIBMIKMOD
	Player_Stop();
	Player_Free(module);
	MikMod_Exit();

#endif

	return 0;
}
