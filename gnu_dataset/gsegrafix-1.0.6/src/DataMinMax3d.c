/*******************************************************************************
*
* DataMinMax3d.c
*
* Finds minimum and maximum values of 3d-plot data.
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


#include "gsegraf.h"


void DataMinMax3d ( void )
   {
   /* Declare variables */
   int i, j, iplot, nplots, index_plot_types,
       npts, nx, ny, index, ipoints,
       imesh, xindex_mesh, yindex_mesh, zindex_mesh,
       icontour, xindex_contour, yindex_contour, zindex_contour,
       icolor, xindex_color, yindex_color, zindex_color,
       nlines, nsymbols;
   double xmin, xmax, ymin, ymax, zmin, zmax, line_coords[6], symbol_coords[3];
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
   xindex_mesh = 0;
   yindex_mesh = 0;
   zindex_mesh = 0;
   xindex_contour = 0;
   yindex_contour = 0;
   zindex_contour = 0;
   xindex_color = 0;
   yindex_color = 0;
   zindex_color = 0;
   ipoints = 0;
   imesh = 0;
   icontour = 0;
   icolor = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
         {
         ipoints++;

         npts = ndata[ipoints-1];
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

            if ( zdata[index+i-1] < zmin )
               zmin = zdata[index+i-1];

            if ( zdata[index+i-1] > zmax )
               zmax = zdata[index+i-1];
            }

         index = index + ndata[ipoints-1];
         }

      else if ( strcmp(&plot_types[index_plot_types], "mesh") == 0 )
         {
         imesh++;

         nx = nxmesh[imesh-1];
         if ( xmesh[xindex_mesh] < xmin )
            xmin = xmesh[xindex_mesh];
         if ( xmesh[xindex_mesh+nx-1] > xmax )
            xmax = xmesh[xindex_mesh+nx-1];

         ny = nymesh[imesh-1];
         if ( ymesh[yindex_mesh] < ymin )
            ymin = ymesh[yindex_mesh];
         if ( ymesh[yindex_mesh+ny-1] > ymax )
            ymax = ymesh[yindex_mesh+ny-1];

         for ( i=1; i<=nx; i++ )
            for ( j=1; j<=ny; j++ )
               {
               if ( zmesh[zindex_mesh+ny*(i-1)+j-1] < zmin )
                  zmin = zmesh[zindex_mesh+ny*(i-1)+j-1];
               if ( zmesh[zindex_mesh+ny*(i-1)+j-1] > zmax )
                  zmax = zmesh[zindex_mesh+ny*(i-1)+j-1];
               }

         xindex_mesh = xindex_mesh + nx;
         yindex_mesh = yindex_mesh + ny;
         zindex_mesh = zindex_mesh + nx*ny;
         }

      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 )
         {
         icontour++;

         nx = nxcontour[icontour-1];
         if ( xcontour[xindex_contour] < xmin )
            xmin = xcontour[xindex_contour];
         if ( xcontour[xindex_contour+nx-1] > xmax )
            xmax = xcontour[xindex_contour+nx-1];

         ny = nycontour[icontour-1];
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

      else if ( strcmp(&plot_types[index_plot_types], "color") == 0 )
         {
         icolor++;

         nx = nxcolor[icolor-1];
         if ( xcolor[xindex_color] < xmin )
            xmin = xcolor[xindex_color];
         if ( xcolor[xindex_color+nx-1] > xmax )
            xmax = xcolor[xindex_color+nx-1];

         ny = nycolor[icolor-1];
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

         if ( sscanf(line, "%*s %lf %lf %lf %lf %lf %lf",
                     &line_coords[0], &line_coords[3],
                     &line_coords[1], &line_coords[4],
                     &line_coords[2], &line_coords[5]) != 6 )
            {
            ErrorDialog(error_str[0]);
            exit(1);
            }

         /* Modify axis minimum and maximum values */
         if ( line_coords[0] < xmin )
            xmin = line_coords[0];

         if ( line_coords[0] > xmax )
            xmax = line_coords[0];

         if ( line_coords[3] < xmin )
            xmin = line_coords[3];

         if ( line_coords[3] > xmax )
            xmax = line_coords[3];

         if ( line_coords[1] < ymin )
            ymin = line_coords[1];

         if ( line_coords[1] > ymax )
            ymax = line_coords[1];

         if ( line_coords[4] < ymin )
            ymin = line_coords[4];

         if ( line_coords[4] > ymax )
            ymax = line_coords[4];

         if ( line_coords[2] < zmin )
            zmin = line_coords[2];

         if ( line_coords[2] > zmax )
            zmax = line_coords[2];

         if ( line_coords[5] < zmin )
            zmin = line_coords[5];

         if ( line_coords[5] > zmax )
            zmax = line_coords[5];
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

         if ( sscanf(line, "%*s %lf %lf %lf",
                     &symbol_coords[0], &symbol_coords[1], &symbol_coords[2]) != 3 )
            {
            ErrorDialog(error_str[1]);
            exit(1);
            }

         /* Modify axis minimum and maximum values */
         if ( symbol_coords[0] < xmin )
            xmin = symbol_coords[0];

         if ( symbol_coords[0] > xmax )
            xmax = symbol_coords[0];

         if ( symbol_coords[1] < ymin )
            ymin = symbol_coords[1];

         if ( symbol_coords[1] > ymax )
            ymax = symbol_coords[1];

         if ( symbol_coords[2] < zmin )
            ymin = symbol_coords[2];

         if ( symbol_coords[2] > zmax )
            ymax = symbol_coords[2];
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
      p_data_min_max->zmin = zmin;
      p_data_min_max->zmax = zmax;
      }

   return;
   }
