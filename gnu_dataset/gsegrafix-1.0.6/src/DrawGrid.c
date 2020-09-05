/*******************************************************************************
*
* DrawGrid.c
*
* Draws linear or logarithmic grid lines.
*
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* This file is part of GSEGrafix, a scientific and engineering plotting program.
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*******************************************************************************/


#include <math.h>
#include "gsegraf.h"


void DrawGrid ( char *axis_type,
                double x11_screen, double y11_screen, double x12_screen, double y12_screen,
                double x21_screen, double y21_screen, int nticks, double *tick_values,
                double offset1, double offset2 )
   {
   /* Declare variables */
   int i, ndecades;
   double xscale, yscale, axis_min, axis_max;
   GnomeCanvasPoints *points;


   /* Draw linear-axis grid lines */
   if ( strcmp(axis_type, "linear") == 0 )
      {
      axis_min = tick_values[0] - offset1;
      axis_max = tick_values[nticks-1] + offset2;
      xscale = (x12_screen - x11_screen)/(axis_max - axis_min);
      yscale = (y12_screen - y11_screen)/(axis_max - axis_min);

      points = gnome_canvas_points_new(2);
      for ( i=1; i<=nticks; i++ )
         {
         points->coords[0] = x11_screen + (tick_values[i-1] - axis_min)*xscale;
         points->coords[1] = y11_screen + (tick_values[i-1] - axis_min)*yscale;
         points->coords[2] = x21_screen + (tick_values[i-1] - axis_min)*xscale;
         points->coords[3] = y21_screen + (tick_values[i-1] - axis_min)*yscale;
         if ( gridchar1 == 'l' )
            DrawLine(points, gridcolor, 1);
         else if ( gridchar1 == 'd' )
            DrawDashedLine(points, gridcolor, 1);
         else if ( gridchar1 == '.' )
            DrawDottedLine(points, gridcolor, 1);
         }
      gnome_canvas_points_unref(points);
      }

   /* Draw logarithmic-axis grid lines */
   else if ( strcmp(axis_type, "log") == 0 )
      {
      ndecades = roundint(ceil(tick_values[nticks-1]) - floor(tick_values[0]));
      if ( ndecades <= 10 )
         nticks = ndecades + 1;

      xscale = (x12_screen - x11_screen)/(nticks - 1);
      yscale = (y12_screen - y11_screen)/(nticks - 1);

      points = gnome_canvas_points_new(2);
      for ( i=1; i<=nticks; i++ )
         {
         points->coords[0] = x11_screen + (i - 1)*xscale;
         points->coords[1] = y11_screen + (i - 1)*yscale;
         points->coords[2] = x21_screen + (i - 1)*xscale;
         points->coords[3] = y21_screen + (i - 1)*yscale;
         if ( gridchar1 == 'l' )
            DrawLine(points, gridcolor, 1);
         else if ( gridchar1 == 'd' )
            DrawDashedLine(points, gridcolor, 1);
         else if ( gridchar1 == '.' )
            DrawDottedLine(points, gridcolor, 1);
         }
      gnome_canvas_points_unref(points);
      }

   return;
   }

