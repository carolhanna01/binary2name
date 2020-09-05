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

   Game management
   Sky
 */

#include "std.h"

#include "game.h"

#include "const.h"

#include "read_image.h"

#include "world.h"

#include "color.h"

#include "mem.h"

#include "config.h"

extern World *world;

extern int factor;

extern int bpp;

extern void load_sprites(int *, int *);

extern int CompletionType;

extern int drawing;
extern int player;

/*inline*/
 void refresh(char *buffer, char *sky, int inv_radius, int view)
{
	int j;
	static int boo = 1;
	static int road_dx;
	static int road_dy;
	static int depart_dx;
	static int depart_dy;
	static int angle[2] = {256, 256};

	if (boo == 1) {
		boo = 0;
		fprintf(stderr, "Sprites and Road Loading ..\n");
		load_sprites(&dx, &dy);
		Read_PNG(&depart, "./road/depart.png", depth, &depart_dx,
			 &depart_dy);
		Read_PNG(&border, "./road/border.png", depth,
			 &road_dx, &road_dy);
		switch (Road_type) {
		case 0:
			Read_PNG(&road, "./road/road1.png", depth,
				 &road_dx, &road_dy);
			break;

		case 1:	/* Snow */

			Read_PNG(&road, "./road/road2.png", depth,
				 &road_dx, &road_dy);

			world->omega[0]--;

			world->omega[1]--;

			world->omega[2]--;

			world->vrmax[0] += 5;

			world->vrmax[1] += 5;

			world->vrmax[2] += 5;

			break;

		case 2:

			Read_PNG(&road, "./road/road3.png", depth,
				 &road_dx, &road_dy);

			dep_alea = 1;

			world->accel[0]++;

			world->accel[1]++;

			world->accel[2]++;


			break;

		}

	}
	angle[view] +=
	    (inv_radius * world->Sorting[view].yvit >> (SKY_TURN + ACCEL));
	/*dep OR world->Sorting[view].xvit >> 4 */

	while (angle[view] < 0) {

		angle[view] += 3 * WIDTH;

	}

	while (angle[view] >= 3 * WIDTH) {

		angle[view] -= 3 * WIDTH;

	}

/*

   SCROLLING HORIZONTAL

 */
	{
		char *_buffer;
		char *_sky;
		//unsigned int sky_offset;
		
		_buffer = buffer;

		_sky = sky;

		/* OPTIMISATION !!! */

		_sky += angle[view] * factor;

		/* 
		   Point of view 
		   0 : high
		   HVISION/2 : low
		 */
		{
			int derive = 0;

			derive = (world->Sorting[view].ypos >> 6);
			derive = derive % (64 * LENGTH);
			derive = world->curve[derive];

			_sky += 6 * (HVISION / 2 + derive) * factor * WIDTH;

		}
		/* Sky loop drawing */
		for (j = 0; j < HEIGHT - HVISION + 1 ; j++) {
			memcpy((char *) _buffer, (char *) (_sky),
			       factor * WIDTH);
			_buffer += factor * WIDTH;
			_sky += 6 * factor * WIDTH;
		}

		/*
		   for (j = 0; j < WIDTH; j++) 
		   {
		   int i;
		   int courant;
		   courant = (factor * WIDTH) * j;
		   for (i = 0; i < HEIGHT - HVISION + 1; i++) 
		   { 
		   int pos ;

		   pos = factor * ((SkyCylinder[i] +
		   angle[view]) + courant);
		   //~~~~~
		   S_memcpy((char *) _buffer,(char *) (_sky + pos*factor),factor);
		   // *(e_++) = ciel[pos];
		   _buffer += factor;

		   }
		   }
		 */
	}

	/* road */

	Draw_road(buffer, view, world->Sorting[view].xpos >> 2);	//#65

/* 
   SPEED & POSITION
 */

	{

		int ii, jj;

		char *_im_;

		{

			unsigned int color;
			unsigned ofs1, ofs2, ofs3, ofs4;

			color = rgb(255, 0, 0);
			ofs1 = factor * (1 + WIDTH);
			ofs2 = ofs1 + factor * WIDTH;
			ofs3 = ofs2 + factor * WIDTH;
			ofs4 = ofs3 + factor * WIDTH;

			for (ii = 0;
			     ii <
			     (world->Sorting[view].yvit >> (ACCEL)) + 20;
			     ii++) {

				memcpy(&buffer[ofs1], &color, factor);
				memcpy(&buffer[ofs2], &color, factor);
				memcpy(&buffer[ofs3], &color, factor);
				memcpy(&buffer[ofs4], &color, factor);

				ofs1 += factor;
				ofs2 += factor;
				ofs3 += factor;
				ofs4 += factor;
			}

			color = rgb(192 + world->Sorting[view].turn * 8, 0, 0);

			ofs1 = factor * (1 + 5 * WIDTH);

			ofs2 = ofs1 + factor * WIDTH;

			ofs3 = ofs2 + factor * WIDTH;

			ofs4 = ofs3 + factor * WIDTH;

			for (ii = 0;
			     ii <
			     (world->Sorting[view].ypos >> 10) % (64 *
								  LENGTH);
			     ii++) {

				memcpy(&buffer[ofs1], &color, factor);

				memcpy(&buffer[ofs2], &color, factor);

				memcpy(&buffer[ofs3], &color, factor);

				memcpy(&buffer[ofs4], &color, factor);

				ofs1 += factor;

				ofs2 += factor;

				ofs3 += factor;

				ofs4 += factor;

			}

			color = rgb(128, 0, 0);

			ofs1 = factor * (1 + 9 * WIDTH);

			ofs2 = ofs1 + factor * WIDTH;

			ofs3 = ofs2 + factor * WIDTH;

			ofs4 = ofs3 + factor * WIDTH;

			for (ii = 0;
			     ii <
			     (world->Sorting[1 /*- view*/ ].ypos >> 10) %
			     (64 * LENGTH); ii++) {

				memcpy(&buffer[ofs1], &color, factor);

				memcpy(&buffer[ofs2], &color, factor);

				memcpy(&buffer[ofs3], &color, factor);

				memcpy(&buffer[ofs4], &color, factor);

				ofs1 += factor;

				ofs2 += factor;

				ofs3 += factor;

				ofs4 += factor;

			}

		}

/*
   PLAYER POSITION (Right number)
 */

		//_im_ = number[(world->Sorting[player].position + 1) % 10];
		_im_ = number[(world->Sorting[view].position + 1) % 10];

		for (ii = 0; ii < num_l; ii++) {

			for (jj = 0; jj < num_v; jj++) {

				unsigned int color = 0;

				unsigned int alpha;

				memcpy(&color,
				       &_im_[factor * (jj + ii * num_v)],
				       factor);

				alpha = grey(color);

				if (alpha > 64)
					memcpy(&buffer
					       [factor *
						(jj + (3 * WIDTH) / 4 +
						 ii * WIDTH)], &color,
					       factor);

			}

		}

		if (world->Sorting[player].position + 1 >= 10) {

			_im_ = number[(world->Sorting[player].position + 1) / 10];

			for (ii = 0; ii < num_l; ii++) {

				for (jj = 0; jj < num_v; jj++) {

					unsigned int color = 0;

					/*

					   unsigned int background = 0;

					   unsigned int r,g,b;

					 */

					unsigned int alpha;

					memcpy(&color,
					       &_im_[factor *
						     (jj + ii * num_v)],
					       factor);

					/*

					   memcpy(&background, &buffer[factor * (jj + WIDTH / 2 + WIDTH / 10 + ii * WIDTH)], factor);

					 */

					alpha = grey(color);

					//alpha = 0;

/*

   if (alpha <= 128) {

   r = (alpha *   red(color)) / 128 + ((128 - alpha) *   red(background)) / 128;

   g = (alpha * green(color)) / 128 + ((128 - alpha) * green(background)) / 128;

   b = (alpha *  blue(color)) / 128 + ((128 - alpha) *  blue(background)) / 128;

   color = rgb(r, g, b);

   }

 */

					if (alpha > 64)
						/*if (color != 0) */

						memcpy(&buffer
						       [factor *
							(jj + WIDTH / 2 +
							 WIDTH / 10 +
							 ii * WIDTH)],
						       &color, factor);

				}

			}

		}
	}

}

int event_x(XEvent * XEv)
{

	int pressure = 0;
	/*
	   pressure need to be fixed.
	   pressure is needed for fast computer to
	   keep control on the keyboard.
	 */

	/*

	   XEvent xev_tmp;

	 */

	while (XPending(display))
		XNextEvent(display, XEv);

	pressure = 1;

/*
   Here some try ..


   while (XCheckMaskEvent(display, KeyPressMask | KeyReleaseMask, XEv)) //XCheckWindowEvent

   {

   pressure++;

   }

 */

/*

   if (XCheckMaskEvent(display, KeyPressMask | KeyReleaseMask, XEv) == True)

   {

   if (XEv -> xkey.type == KeyPress)

   while (XCheckMaskEvent(display, KeyPressMask, &xev_tmp))

   {

   pressure++;

   }    

   else

   while (XCheckMaskEvent(display, KeyReleaseMask, &xev_tmp))

   {

   pressure++;

   }    

   }

 */

	return pressure;

}

void aff(Window w)
{
/*
   __ Image is drawn after being entirely build __
   ********************************
   !! NO DOUBLE BUFFERING NEEDED !!
   ********************************
 */
	if (xim != NULL) {
#if MITSHM
		if (mit_shm)
			XShmPutImage(display, w, gc, xim, 0, 0, 0,
				     0, WIDTH, HEIGHT, True);
		else
			XPutImage(display, w, gc, xim, 0, 0, 0, 0,
				  WIDTH, HEIGHT);
#else
		XPutImage(display, w, gc, xim, 0, 0, 0, 0, WIDTH,
			  HEIGHT);
#endif
	} else {
		// printf("aff : xim NULL\n");
	}
}

void aff2(Window w)
{

	int i, j;

	int tmp, tmp1, tmp2, tmp3, tmp4;

	/* OPTIMISATION */

	tmp = 0;

	for (j = 0; j < HEIGHT; j++) {

		tmp1 = factor * (j << 1) * (WIDTH * 2);

		tmp2 = tmp1 + factor;

		tmp3 = tmp1 + (factor * (WIDTH * 2));

		tmp4 = tmp3 + factor;

		for (i = 0; i < WIDTH; i++) {

			S_memcpy(&screen_buffer2[tmp1],
				 &screen_buffer[tmp], factor);

			S_memcpy(&screen_buffer2[tmp2],
				 &screen_buffer[tmp], factor);

			S_memcpy(&screen_buffer2[tmp3],
				 &screen_buffer[tmp], factor);

			S_memcpy(&screen_buffer2[tmp4],
				 &screen_buffer[tmp], factor);

			/* Shifting */

			tmp += factor;

			tmp1 += 2 * factor;
			tmp2 += 2 * factor;
			tmp3 += 2 * factor;
			tmp4 += 2 * factor;

		}

	}

	if ( /*drawing */ 1) {

		if (xim != NULL) {

#if MITSHM

			if (mit_shm) {

				XShmPutImage(display, w, gc, xim, 0, 0, 0,
					     0, WIDTH * 2, HEIGHT * 2,
					     True);


			} else
				XPutImage(display, w, gc, xim, 0, 0, 0, 0,
					  WIDTH * 2, HEIGHT * 2);

#else

			XPutImage(display, w, gc, xim, 0, 0, 0, 0,
				  WIDTH * 2, HEIGHT * 2);

#endif

		}
		//drawing = 0;

	}
}

void liberation(void)
{

	int i;

	//free(road); core dump ??

	for (i = 0; i < 10; i++)
		free(number[i]);

}
