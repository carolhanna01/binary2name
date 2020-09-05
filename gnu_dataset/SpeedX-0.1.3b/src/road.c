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
  */

 #include "std.h"
 #include "road.h"
 #include "read_image.h"
 #include "const.h"
 #include "world.h"
 #include "mem.h"
 #include <stdio.h>

 extern World *world;
 extern int factor;

 int y_panel;

//#65
 inline void Draw_road(char *screen_buffer, int view, int dep)
 {
   int i, _j, j;
   int ctmp, ctmp2;
   int delta = 0;
   int d_el = 0;
   int inv_radius_local = 0;
   int delta_e;
   int diff;

   //   int distance = 0;

   dep = 0 ;
   delta = world->Sorting[view].xvit >> 3;

   /* Scenery : TO DO (Voxel ?) */
   y_panel = -1;
   delta_e = world->Sorting[view].ypos;
   ctmp2 = factor * WIDTH * (HEIGHT - 1);

   for (_j = 0; _j < HVISION - 1; _j++) {

     int limite = WIDTH - delta;

     int _conversion_;
     int position;
     char *buffer;


     j = _j;

     if (j < 0)
       continue;
     if (j > HVISION - 1)
       continue;

     diff = (world->Sorting[view].ypos + ProjectiveDistance[j] - HVISION);
     position = (diff >> 6) % (64 * LENGTH);
     if (position == 0)
       buffer = depart;
     else
       /* We draw the road */
       buffer = road;

     d_el = ProjectiveDistance[j + 1] - ProjectiveDistance[j];

     if (d_el < MINV)
       d_el = 0;

     inv_radius_local = world->trace[position];
     _conversion_ = (HEIGHT / 4) + j * WIDTH;



     /* the left border of the road */
     for (i = -delta; (i < Leftroad[j]) && (i < limite); i++) {
	     /*
	       ctmp = ((conversion[_conversion_ + (i >> 1)]
	       + world->Sorting[view].ypos * WIDTH)
	       % (WIDTH * HEIGHT));
	     */

	     ctmp = (i + delta) + (j /* + (rand() % 4) */) * HEIGHT;
	     while (ctmp < 0)
		     ctmp += WIDTH;

	     ctmp *= factor;

	     //S_memcpy(&screen_buffer[ctmp2], &border[ctmp], factor);
	     S_memcpy(&screen_buffer[ctmp2], &screen_buffer[ctmp], factor);

	     ctmp2 += factor;
     }

     /* the road itself */
     for (; (i < Rightroad[j]) && (i < limite); i++) {
       ctmp = ((conversion[_conversion_ + (i >> 1)]
		+ world->Sorting[view].ypos * WIDTH
		+ dep)
	       % (WIDTH * HEIGHT));

       while (ctmp < 0)
	 ctmp += WIDTH;
       ctmp *= factor;

       S_memcpy(&screen_buffer[ctmp2], &buffer[ctmp], factor);
       ctmp2 += factor;
     }

     /* the right border of the road */
     for (; i < WIDTH - delta; i++) {
       /*
	 ctmp = ((conversion[_conversion_ + (i >> 1)]
	 + world->Sorting[view].ypos * WIDTH)
	 % (WIDTH * HEIGHT));
       */

       ctmp = (i + delta) + (j /* + (rand() % 4) */) * HEIGHT;

       while (ctmp < 0)
	 ctmp += WIDTH;
       ctmp *= factor;

       //S_memcpy(&screen_buffer[ctmp2], &border[ctmp], factor);
       S_memcpy(&screen_buffer[ctmp2], &screen_buffer[ctmp], factor);
       ctmp2 += factor;
     }
     ctmp2 -= factor * 2 * WIDTH;
     delta_relatif[j] = (delta >> 2);
     delta += ((d_el * inv_radius_local) >> 8);

     /*
       ARRIVAL PANEL
     */
     if (diff % (64 * 64 * LENGTH) < delta_e) {

       delta_e = diff % (64 * 64 * LENGTH);

       y_panel = j;

     }
   }

   j = HVISION - 1;

   diff =
     (world->Sorting[view].ypos + ProjectiveDistance[j] -
      ProjectiveDistance[0]) % (64 * 64 * LENGTH);

   /*
     if (diff < delta_e) {

     delta_e = diff;

     y_panel = j;
     }
   */
   if (y_panel != -1) {
     int x, y;
     int cx, cy;
     int d;
     int px, py;
     extern char *panel;
     int color = 0;

     d = Rightroad[y_panel] - Leftroad[y_panel];

     d_el = ProjectiveDistance[y_panel + 1] - ProjectiveDistance[y_panel];
     if (d_el < MINV)
       d_el = 0;

     inv_radius_local = world->trace[((world->Sorting[view].ypos +
				       ProjectiveDistance[y_panel] -
				       ProjectiveDistance[0]) / 64) %
				    (64 * LENGTH)];
     cx = ((d_el * inv_radius_local) / 256);
     cx += Leftroad[y_panel];
     for (x = 0; x < d; x++) {
       if ((cx >= 0) && (cx < WIDTH)) {
	 cy = HEIGHT - y_panel;
	 px = (x * 256) / d;
	 for (y = 0; y < d; y++) {
	   if ((cy >= 0) && (cy < HEIGHT)) {
	     py = ((d - y) * 256) / d;
	     S_memcpy((char *) &color, &panel[factor * (px + py * 256)], factor);
	     if (color != 0)
	       S_memcpy((char *) &screen_buffer[factor * (cx + cy * WIDTH)], &color, factor);
	   }
	   cy--;
	 }
       }
       cx++;
     }
   }
 }
