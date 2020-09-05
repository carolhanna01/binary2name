/*******************************************************************************
*
* DataMinMax.c
*
* Finds minimum and maximum values of 2d-plot data.
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


#include <float.h>
#include <math.h>
#include "gsegraf.h"


void DataMinMax ( void )
   {
   /* Declare variables */
   int i, j, iplot, nplots, npts, index, index_plot_types,
       ipoints, ihist, icontour, icolor,
       n1, n2, nbins, count, index_bin_values,
       nx, ny, xindex_contour, yindex_contour, zindex_contour,
       xindex_color, yindex_color, zindex_color,
       nlines, nsymbols;
   double xmin, xmax, ymin, ymax, zmin, zmax, sum, mean,
          data_min, data_max, q[5], binwidth, binmin, binmax, bin1, bin2,
          histmin, histmax, line_coords[4], symbol_coords[2];
   const char *error_str[] =
      { "Invalid line coordinates.",
        "Invalid symbol coordinates." };
   FILE *fptr;


   /* Initialize min-max data */
   xmin =  DBL_MAX;
   xmax = -DBL_MAX;
   ymin =  DBL_MAX;
   ymax = -DBL_MAX;
   zmin =  DBL_MAX;
   zmax = -DBL_MAX;


   /* Find minimum and maximum values of data files */
   nplots = p_plot_param->nplots;
   index_plot_types = 0;
   index = 0;
   ipoints = 0;
   ihist = 0;
   icontour = 0;
   icolor = 0;
   index_bin_values = 0;
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

         npts = ndata[ipoints + ihist -1];
         for ( i=1; i<=npts; i++ )
            {
            if ( xdata[index+i-1] < xmin )
               xmin = xdata[index+i-1];

            if ( xdata[index+i-1] > xmax )
               xmax = xdata[index+i-1];

            if ( ydata[index+i-1] < ymin )
               ymin = ydata[index+i-1];

            if ( ydata[index+i-1] > ymax )
               ymax = ydata[index+i-1];
            }

         index = index + ndata[ipoints+ihist-1];
         }

      else if ( strcmp(&plot_types[index_plot_types], "histogram") == 0 )
         {
         ihist++;

         /* Calculate data mean */
         npts = ndata[ipoints+ihist-1];
         sum = 0.0;
         for ( i=1; i<=npts; i++ )
            sum = sum + xdata[index+i-1];
         mean = sum/npts;

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

         /* Calculate bin minimum and maximum */
         n1 = ceil(((mean - binwidth/2.0) - data_min)/binwidth);
         n2 = ceil((data_max - (mean + binwidth/2.0))/binwidth);
         nbins = n1 + n2 + 1;
         binmin = (mean - binwidth/2.0) - n1*binwidth;
         binmax = (mean + binwidth/2.0) + n2*binwidth;

         /* Find maximum histogram value */
         histmin = 0.0;
         histmax = 0.0;
         for ( i=1; i<=nbins; i++ )
            {
            bin1 = binmin + (i - 1.0)/(nbins - 1.0)*(binmax - binmin - binwidth);
            bin2 = bin1 + binwidth;
            count = 0.0;
            for ( j=1; j<=npts; j++ )
               if ( (xdata[index+j-1] >= bin1)  && (xdata[index+j-1] < bin2) )
                  count = count + 1;

            if ( count > histmax )
               histmax = count;
            }

         if ( strcmp(&bin_values[index_bin_values], "fraction") == 0 )
            histmax = histmax/npts;
         else if ( strcmp(&bin_values[index_bin_values], "percent") == 0 )
            histmax = 100.0*histmax/npts;
         index_bin_values = index_bin_values + 9;

         if ( binmin < xmin )
            xmin = binmin;

         if ( binmax > xmax )
            xmax = binmax;

         if ( histmin < ymin )
            ymin = histmin;

         if ( histmax > ymax )
            ymax = histmax;

         index = index + ndata[ipoints + ihist -1];
         }

      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 )
         {
         icontour++;

         nx = nxcontour[icontour-1];
         ny = nycontour[icontour-1];

         if ( xcontour[xindex_contour] < xmin )
            xmin = xcontour[xindex_contour];

         if ( xcontour[xindex_contour+nx-1] > xmax )
            xmax = xcontour[xindex_contour+nx-1];

         if ( ycontour[yindex_contour] < ymin )
            ymin = ycontour[yindex_contour];

         if ( ycontour[yindex_contour+ny-1] > ymax )
            ymax = ycontour[yindex_contour+ny-1];

         for ( i=1; i<=nx; i++ )
            for ( j=1; j<=ny; j++ )
               {
               if ( zcontour[zindex_contour+ny*(i-1)+j-1] < zmin )
                  zmin = zcontour[zindex_contour+ny*(i-1)+j-1];
               if ( zcontour[zindex_contour+ny*(i-1)+j-1] > zmax )
                  zmax = zcontour[zindex_contour+ny*(i-1)+j-1];
               }

         xindex_contour = xindex_contour + nx;
         yindex_contour = yindex_contour + ny;
         zindex_contour = zindex_contour + nx*ny;
         }

      else if ( strcmp(&plot_types[index_plot_types], "color")  == 0 )
         {
         icolor++;

         nx = nxcolor[icolor-1];
         ny = nycolor[icolor-1];

         if ( xcolor[xindex_color] < xmin )
            xmin = xcolor[xindex_color];

         if ( xcolor[xindex_color+nx-1] > xmax )
            xmax = xcolor[xindex_color+nx-1];

         if ( ycolor[yindex_color] < ymin )
            ymin = ycolor[yindex_color];

         if ( ycolor[yindex_color+ny-1] > ymax )
            ymax = ycolor[yindex_color+ny-1];

         for ( i=1; i<=nx; i++ )
            for ( j=1; j<=ny; j++ )
               {
               if ( zcolor[zindex_color+ny*(i-1)+j-1] < zmin )
                  zmin = zcolor[zindex_color+ny*(i-1)+j-1];
               if ( zcolor[zindex_color+ny*(i-1)+j-1] > zmax )
                  zmax = zcolor[zindex_color+ny*(i-1)+j-1];
               }

         xindex_color = xindex_color + nx;
         yindex_color = yindex_color + ny;
         zindex_color = zindex_color + nx*ny;
         }

      /* Increment indices */
      index_plot_types = index_plot_types + 10;
      }


   /* Find minimum and maximum values of line data */
   fptr = fopen(p_param_file, "r");
   nlines = 0;
   while ( fgets(line, maxline, fptr) != NULL )
      {
      /* Get line coordinates */
      if ( strncmp(line, "line_coords", 11) == 0 )
         {
         nlines++;

         if ( sscanf(line, "%*s %lf %lf %lf %lf",
                     &line_coords[0], &line_coords[2],
                     &line_coords[1], &line_coords[3]) != 4 )
            {
            ErrorDialog(error_str[0]);
            exit(1);
            }

         /* Modify line coordinates for logarithmic and polar axes */
         if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
            for ( i=1; i<=3; i=i+2 )
               line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
            for ( i=2; i<=4; i=i+2 )
               line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
            for ( i=1; i<=4; i++ )
               line_coords[i-1] = log10(fabs(line_coords[i-1]));

         else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
            for ( i=1; i<=3; i=i+2 )
               line_coords[i-1] = line_coords[i-1]*deg2rad;

         /* Modify axis minimum and maximum values */
         if ( line_coords[0] < xmin )
            xmin = line_coords[0];

         if ( line_coords[0] > xmax )
            xmax = line_coords[0];

         if ( line_coords[2] < xmin )
            xmin = line_coords[2];

         if ( line_coords[2] > xmax )
            xmax = line_coords[2];

         if ( line_coords[1] < ymin )
            ymin = line_coords[1];

         if ( line_coords[1] > ymax )
            ymax = line_coords[1];

         if ( line_coords[3] < ymin )
            ymin = line_coords[3];

         if ( line_coords[3] > ymax )
            ymax = line_coords[3];
         }

      else if ( strncmp(line, "#####", 5) == 0 )
         break;
      }
   fclose(fptr);


   /* Find minimum and maximum values of symbol data */
   fptr = fopen(p_param_file, "r");
   nsymbols = 0;
   while ( fgets(line, maxline, fptr) != NULL )
      {
      /* Get symbol coordinates */
      if ( strncmp(line, "symbol_coords", 13) == 0 )
         {
         nsymbols++;

         if ( sscanf(line, "%*s %lf %lf",
                     &symbol_coords[0], &symbol_coords[1]) != 2 )
            {
            ErrorDialog(error_str[1]);
            exit(1);
            }

         /* Modify symbol coordinates for logarithmic and polar axes */
         if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
            symbol_coords[0] = log10(fabs(symbol_coords[0]));

         else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
            symbol_coords[1] = log10(fabs(symbol_coords[1]));

         else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
            {
            symbol_coords[0] = log10(fabs(symbol_coords[0]));
            symbol_coords[1] = log10(fabs(symbol_coords[1]));
            }

         else if ( strcmp(p_plot_param->axis_type, "polar") == 0 )
            symbol_coords[0] = symbol_coords[0]*deg2rad;

         /* Modify axis minimum and maximum values */
         if ( symbol_coords[0] < xmin )
            xmin = symbol_coords[0];

         if ( symbol_coords[0] > xmax )
            xmax = symbol_coords[0];

         if ( symbol_coords[1] < ymin )
            ymin = symbol_coords[1];

         if ( symbol_coords[1] > ymax )
            ymax = symbol_coords[1];
         }

      else if ( strncmp(line, "#####", 5) == 0 )
         break;
      }
   fclose(fptr);


   /* Save data */
   if ( nplots > 0 || nlines > 0 || nsymbols > 0 )
      {
      p_data_min_max->xmin = xmin;
      p_data_min_max->xmax = xmax;
      p_data_min_max->ymin = ymin;
      p_data_min_max->ymax = ymax;
      }

   if ( icontour > 0 || icolor > 0 )
      {
      p_data_min_max->zmin = zmin;
      p_data_min_max->zmax = zmax;
      }

   return;
   }
