/*******************************************************************************
*
* Histogram.c
*
* Calculates histogram of input data.
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


void Histogram ( int iplot, int ihist )
   {
   /* Declare variables */
   int i, j, index, npts, nx, ny, n1, n2, nbins, index_bin_values, index_bin_refs;
   double data_min, data_max,
          sum, sumsq, mean, std, q[5],
          binmin, binmax, binwidth, bin1, bin2,
          x1_box, y1_box, x2_box, y2_box,
          xmin, xmax, ymin, ymax, xscale, yscale,
          *yhist, x1_bar, y1_bar, x2_bar, y2_bar,
          x1, y1, x2, y2, xline;
   GnomeCanvasPoints *points;


   /* Calculate data index */
   index = 0;
   if ( iplot > 1 )
      for ( i=1; i<iplot; i++ )
         index = index + ndata[i-1];


   /* Calculate data mean and standard deviation */
   sum = 0.0;
   npts = ndata[iplot-1];
   for ( i=1; i<=npts; i++ )
      sum = sum + xdata[index+i-1];
   mean = sum/npts;

   sumsq = 0.0;
   for ( i=1; i<=npts; i++ )
      sumsq = sumsq + (xdata[index+i-1] - mean)*(xdata[index+i-1] - mean);
   std = sqrt(sumsq/(npts - 1.0));


   /* Calculate data quartile values */
   data_min = xdata[index];
   data_max = xdata[index+npts-1];
   q[0] = data_min;
   q[4] = data_max;
   for ( i=1; i<=3; i++ )
      {
      j = roundint(i*0.25*npts);
      q[i] = xdata[index+j-1];
      }


   /* Estimate optimal histogram bin width */
   if ( bin_widths[ihist-1] <= 0.0 )
      {
      /* binwidth = 3.49*std/pow((double) npts, 1.0/3.0); */   /* Scott */
      binwidth = 2.0*(q[3] - q[1])/pow((double) npts, 1.0/3.0);   /* Freedman, Diaconis */
      bin_widths[ihist-1] = binwidth;
      }
   else
      binwidth = bin_widths[ihist-1];


   /* Calculate number of bins, binmin, and binmax */
   index_bin_refs = (ihist - 1)*9;
   if ( strcmp(&bin_refs[index_bin_refs], "mean") == 0 )
      {
      n1 = ceil(((mean - binwidth/2.0) - data_min)/binwidth);
      n2 = ceil((data_max - (mean + binwidth/2.0))/binwidth);
      nbins = n1 + n2 + 1;
      binmin = (mean - binwidth/2.0) - n1*binwidth;
      binmax = (mean + binwidth/2.0) + n2*binwidth;
      }
   else if ( strcmp(&bin_refs[index_bin_refs], "zero") == 0 )
      {
      if ( data_max > 0 )
         {
         nbins = ceil(data_max/binwidth);
         binmin = 0.0;
         binmax = nbins*binwidth;
         }
      else
         return;
      }
   else if ( strcmp(&bin_refs[index_bin_refs], "integers") == 0 )
      {
      binwidth = 1.0;
      nbins = roundint(data_max) - roundint(data_min) + 1.0;
      binmin = (double) roundint(data_min) - 0.5;
      binmax = (double) roundint(data_max) + 0.5;
      }


   /* Calculate number of samples in each bin */
   yhist = xmalloc(nbins*sizeof(double));
   for ( i=1; i<=nbins; i++ )
      yhist[i-1] = 0.0;
   j = 1;
   for ( i=1; i<=nbins; i++ )
      {
      bin1 = binmin + (i - 1.0)/(nbins - 1.0)*(binmax - binmin - binwidth);
      bin2 = bin1 + binwidth;
      while ( (j <= npts) && (bin1 <= xdata[index+j-1]) && (xdata[index+j-1] < bin2) )
         {
         yhist[i-1] = yhist[i-1] + 1.0;
         j++;
         }
      }
   if ( xdata[index+npts-1] == bin2 )
      yhist[nbins-1] = yhist[nbins-1] + 1.0;


   /* Specify axis minimum and maximum values */
   nx = p_ticklabels->nxvalues;
   ny = p_ticklabels->nyvalues;
   xmin = p_ticklabels->xvalues[0];
   xmax = p_ticklabels->xvalues[nx-1];
   ymin = p_ticklabels->yvalues[0];
   ymax = p_ticklabels->yvalues[ny-1];
   xmin = xmin - p_ticklabels->xoffset1;
   xmax = xmax + p_ticklabels->xoffset2;
   ymin = ymin - p_ticklabels->yoffset1;
   ymax = ymax + p_ticklabels->yoffset2;


   /* Get plot box minimum and maximum values */
   x1_box = p_plot_box_data->xmin;
   x2_box = p_plot_box_data->xmax;
   y1_box = p_plot_box_data->ymin;
   y2_box = p_plot_box_data->ymax;


   /* Calculate axis scale factors */
   xscale = (x2_box - x1_box)/(xmax - xmin);
   yscale = (y2_box - y1_box)/(ymax - ymin);


   /* Plot histogram */
   index_bin_values = (ihist - 1)*9;
   points = gnome_canvas_points_new(2);
   for ( i=1; i<=nbins; i++ )
      {
      y1_bar = 0.0;
      if ( strcmp(&bin_values[index_bin_values], "number") == 0 )
         y2_bar = yhist[i-1];
      else if ( strcmp(&bin_values[index_bin_values], "fraction") == 0 )
         y2_bar = yhist[i-1]/npts;
      else if ( strcmp(&bin_values[index_bin_values], "percent") == 0 )
         y2_bar = 100.0*yhist[i-1]/npts;

      if ( y1_bar >= ymax || y2_bar <= ymin )
         continue;
      if ( y1_bar < ymin )
          y1_bar = ymin;
      if ( y2_bar > ymax )
          y2_bar = ymax;

      y1 = y2_box - (y1_bar - ymin)*yscale;
      y2 = y2_box - (y2_bar - ymin)*yscale;

      if ( stylechar1[iplot-1] == 'b' || stylechar1[iplot-1] == 'B' )
         {
         /* Draw bars */
         x1_bar = binmin + (i - 1.0)/(nbins - 1.0)*(binmax - binwidth - binmin);
         x2_bar = x1_bar + binwidth;

         if ( x1_bar >= xmax || x2_bar <= xmin )
            continue;
         if ( x1_bar < xmin )
             x1_bar = xmin;
         if ( x2_bar > xmax )
             x2_bar = xmax;

         x1 = x1_box + (x1_bar - xmin)*xscale;
         x2 = x1_box + (x2_bar - xmin)*xscale;

         DrawBar(x1+1.0, y1, x2-1.0, y2, fill_colors_rgba[iplot-1], outline_colors_rgba[iplot-1]);
         DrawBar(x1, y1+1.0, x2, y2-1.0, 0xFFFFFF00, canvas_bg_color);
         }
      else if ( stylechar1[iplot-1] == 'l' )
         {
         /* Draw lines */
         xline = binmin + (i - 1.0)/(nbins - 1.0)*(binmax - binwidth - binmin) + binwidth/2.0;
         if ( xmin < xline && xline < xmax )
            {
            points->coords[0] = x1_box + (xline - xmin)*xscale;
            points->coords[1] = y1;
            points->coords[2] = points->coords[0];
            points->coords[3] = y2;
            DrawLine(points, fill_colors_rgba[iplot-1], stylesizes[iplot-1]);
            }
         }
      }

   gnome_canvas_points_unref(points);
   free(yhist);

   return;
   }

