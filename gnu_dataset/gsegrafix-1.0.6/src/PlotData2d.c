/*******************************************************************************
*
* PlotData2d.c
*
* Plots data points.
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


void PlotData2d ( void )
   {
   /* Declare variables */
   int i, ifunc, nx, ny, iplot, nplots, npts,
       index_plot_types, index_stemflags, index,
       ipoints, ihist, icontour, icolor,
       nxcontourplot, nycontourplot, xindex_contour, yindex_contour, zindex_contour,
       nxcolorplot, nycolorplot, xindex_color, yindex_color, zindex_color;
   double x1_box, x2_box, y1_box, y2_box, xmin, xmax, ymin, ymax,
          xscale, yscale, x, y;
   char *pchar;
   extern char symbol_string1[];   /* symbol-specification characters "cCtTsSiIpPhH" */
   extern char symbol_string2[];   /* symbol-specification characters "+xra" */
   GnomeCanvasPoints *points;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Get minimum and maximum axis values */
   if ( strcmp(p_plot_param->axis_type, "linear") == 0 )
      {
      nx = p_ticklabels->nxvalues;
      xmin = p_ticklabels->xvalues[0];
      xmax = p_ticklabels->xvalues[nx-1];
      ny = p_ticklabels->nyvalues;
      ymin = p_ticklabels->yvalues[0];
      ymax = p_ticklabels->yvalues[ny-1];
      xmin = xmin - p_ticklabels->xoffset1;
      xmax = xmax + p_ticklabels->xoffset2;
      ymin = ymin - p_ticklabels->yoffset1;
      ymax = ymax + p_ticklabels->yoffset2;
      }

   else if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
      {
      nx = p_ticklabels->nxvalues;
      xmin = floor(p_ticklabels->xvalues[0]);
      xmax = ceil(p_ticklabels->xvalues[nx-1]);
      nx = roundint(xmax - xmin + 1.0);
      ny = p_ticklabels->nyvalues;
      ymin = p_ticklabels->yvalues[0];
      ymax = p_ticklabels->yvalues[ny-1];
      ymin = ymin - p_ticklabels->yoffset1;
      ymax = ymax + p_ticklabels->yoffset2;
      }

   else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
      {
      nx = p_ticklabels->nxvalues;
      xmin = p_ticklabels->xvalues[0];
      xmax = p_ticklabels->xvalues[nx-1];
      ny = p_ticklabels->nyvalues;
      ymin = floor(p_ticklabels->yvalues[0]);
      ymax = ceil(p_ticklabels->yvalues[ny-1]);
      ny = roundint(ymax - ymin + 1.0);
      xmin = xmin - p_ticklabels->xoffset1;
      xmax = xmax + p_ticklabels->xoffset2;
      }

   else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
      {
      nx = p_ticklabels->nxvalues;
      xmin = floor(p_ticklabels->xvalues[0]);
      xmax = ceil(p_ticklabels->xvalues[nx-1]);
      nx = roundint(xmax - xmin + 1.0);
      ny = p_ticklabels->nyvalues;
      ymin = floor(p_ticklabels->yvalues[0]);
      ymax = ceil(p_ticklabels->yvalues[ny-1]);
      ny = roundint(ymax - ymin + 1.0);
      }


   /* Calculate axis scale factors */
   xscale = (x2_box - x1_box)/(xmax - xmin);
   yscale = (y2_box - y1_box)/(ymax - ymin);


   /* Plot data */
   nplots = p_plot_param->nplots;
   index_plot_types = 0;
   index = 0;
   ipoints = 0;
   ihist = 0;
   icontour = 0;
   icolor = 0;
   xindex_contour = 0;
   yindex_contour = 0;
   zindex_contour = 0;
   xindex_color = 0;
   yindex_color = 0;
   zindex_color = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
         {
         ipoints++;
         npts = ndata[ipoints+ihist-1];

         /* Draw stem lines */
         index_stemflags = (iplot - 1)*4;
         if ( strcmp(&stemflags[index_stemflags],  "on") == 0 ||
              strcmp(&stemflags[index_stemflags], "num") == 0 )
            {
            points = gnome_canvas_points_new(2);

            /* Calculate y coordinate of stem point 1 */
            if ( strcmp(&stemflags[index_stemflags], "on") == 0 )
               y = y2_box;
            else if ( strcmp(&stemflags[index_stemflags], "num") == 0 )
               {
               if ( ymin <= stemvalues[iplot-1] && stemvalues[iplot-1] <= ymax )
                  y = y2_box - (stemvalues[iplot-1] - ymin)*yscale;
               else if ( stemvalues[iplot-1] < ymin )
                  y = y2_box;
               else if ( stemvalues[iplot-1] > ymax )
                  y = y1_box;
               }
            points->coords[1] = y;

            for ( i=1; i<=npts; i++ )
               if ( xmin <= xdata[index+i-1] && xdata[index+i-1] <= xmax &&
                    ymin <= ydata[index+i-1] && ydata[index+i-1] <= ymax )
                  {
                  /* Calculate x coordinate of stem point 1 */
                  x = x1_box + (xdata[index+i-1] - xmin)*xscale;
                  points->coords[0] = x;

                  /* Calculate x and y coordinates of stem point 2 */
                  x = x1_box + (xdata[index+i-1] - xmin)*xscale;
                  y = y2_box - (ydata[index+i-1] - ymin)*yscale;
                  points->coords[2] = x;
                  points->coords[3] = y;

                  DrawLine(points, outline_colors_rgba[iplot-1], 1);
                  }

            gnome_canvas_points_unref(points);
            }


         /* Draw lines */
         if ( stylechar1[iplot-1] == 'l' )
            DrawLineSegments(iplot, index, npts, xmin, xmax, ymin, ymax,
                             xscale, yscale, 'l');


         /* Draw dashed lines */
         else if ( stylechar1[iplot-1] == 'd' )
            DrawLineSegments(iplot, index, npts, xmin, xmax, ymin, ymax,
                             xscale, yscale, 'd');


         /* Draw dotted lines */
         else if ( stylechar1[iplot-1] == '.' )
            DrawLineSegments(iplot, index, npts, xmin, xmax, ymin, ymax,
                             xscale, yscale, '.');


         /* Draw symbols in symbol_string1 ("cCtTsSiIpPhH") */
         else if ( (pchar = strchr(symbol_string1, stylechar1[iplot-1])) != NULL )
            {
            ifunc = pchar - symbol_string1;
            for ( i=1; i<=npts; i++ )
               if ( xmin <= xdata[index+i-1] && xdata[index+i-1] <= xmax &&
                    ymin <= ydata[index+i-1] && ydata[index+i-1] <= ymax )
                  {
                  x = x1_box + (xdata[index+i-1] - xmin)*xscale;
                  y = y2_box - (ydata[index+i-1] - ymin)*yscale;
                  symbol_func1[ifunc](x, y, fill_colors_rgba[iplot-1], outline_colors_rgba[iplot-1],
                                      stylesizes[iplot-1]);
                  }
            }


         /* Draw symbols in symbol_string2 ("+xra") */
         else if ( (pchar = strchr(symbol_string2, stylechar1[iplot-1])) != NULL )
            {
            ifunc = pchar - symbol_string2;
            for ( i=1; i<=npts; i++ )
               if ( xmin <= xdata[index+i-1] && xdata[index+i-1] <= xmax &&
                    ymin <= ydata[index+i-1] && ydata[index+i-1] <= ymax )
                  {
                  x = x1_box + (xdata[index+i-1] - xmin)*xscale;
                  y = y2_box - (ydata[index+i-1] - ymin)*yscale;
                  symbol_func2[ifunc](x, y, fill_colors_rgba[iplot-1], stylesizes[iplot-1]);
                  }
            }

         index = index + ndata[ipoints+ihist-1];
         }


      else if ( strcmp(&plot_types[index_plot_types], "histogram") == 0 )
         {
         ihist++;
         Histogram(iplot, ihist);
         index = index + ndata[ipoints+ihist-1];
         }


      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 )
         {
         icontour++;
         nxcontourplot = nxcontour[icontour-1];
         nycontourplot = nycontour[icontour-1];
         ContourPlot2d(iplot, icontour, xindex_contour, yindex_contour, zindex_contour, nxcontourplot, nycontourplot);
         xindex_contour = xindex_contour + nxcontourplot;
         yindex_contour = yindex_contour + nycontourplot;
         zindex_contour = zindex_contour + nxcontourplot*nycontourplot;
         }


      else if ( strcmp(&plot_types[index_plot_types], "color")  == 0 )
         {
         icolor++;
         nxcolorplot = nxcolor[icolor-1];
         nycolorplot = nycolor[icolor-1];
         ColorPlot2d(iplot, icolor, xindex_color, yindex_color, zindex_color, nxcolorplot, nycolorplot);
         xindex_color = xindex_color + nxcolorplot;
         yindex_color = yindex_color + nycolorplot;
         zindex_color = zindex_color + nxcolorplot*nycolorplot;
         }


      /* Increment indices */
      index_plot_types = index_plot_types + 10;
      }

   return;
   }


void DrawLineSegments ( int iplot, int index, int npts,
                        double xmin, double xmax, double ymin, double ymax,
                        double xscale, double yscale, int linechar )
   {
   /* Declare variables */
   int idraw, iseg, nseg, npts_seg, iseg1;
   char linetype[7];


   /* Create linetype string */
   memset(linetype, 0, sizeof(linetype));
   if ( linechar == 'l' )
      strcpy(linetype, "solid");
   else if ( linechar == 'd' )
      strcpy(linetype, "dashed");
   else if ( linechar == '.' )
      strcpy(linetype, "dotted");


   /* Draw all line segments except last */
   idraw = 0;
   nseg = nlinebreaks;
   iseg1 = index;
   for (iseg=1; iseg<=nseg; iseg++ )
      {
      if ( index < nlinebreak[iseg-1] && nlinebreak[iseg-1] < index + npts )
         {
         idraw++;
         npts_seg = nlinebreak[iseg-1] - iseg1;
         DrawLines2d(npts_seg, &xdata[iseg1], &ydata[iseg1], xmin, xmax, ymin, ymax,
                     xscale, yscale, fill_colors_rgba[iplot-1], stylesizes[iplot-1],
                     linetype);
         iseg1 = nlinebreak[iseg-1];
         }
      }


   /* Draw last line segment */
   if ( idraw > 0 )
      {
      npts_seg = index + npts - iseg1;
      DrawLines2d(npts_seg, &xdata[iseg1], &ydata[iseg1], xmin, xmax, ymin, ymax,
                  xscale, yscale, fill_colors_rgba[iplot-1], stylesizes[iplot-1],
                  linetype);
      }

   /* Draw continuous line */
   else
      DrawLines2d(npts, &xdata[index], &ydata[index], xmin, xmax, ymin, ymax,
                  xscale, yscale, fill_colors_rgba[iplot-1], stylesizes[iplot-1],
                  linetype);

   return;
   }
