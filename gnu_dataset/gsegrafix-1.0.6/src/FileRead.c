/*******************************************************************************
*
* FileRead.c
*
* Contains functions:
*    FileRead
*    compare
*
* Reads 2d data files.
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


void FileRead ( void )
   {
   /* Declare variables */
   int i, j, iplot, ipoints, ihist, icontour, icolor, nplots, npts, npts_total,
       npts_total_xcontour, npts_total_ycontour, npts_total_zcontour,
       npts_total_xcolor, npts_total_ycolor, npts_total_zcolor,
       index_filenames, index_formats, index_plot_types, index,
       nx, ny, xindex_contour, yindex_contour, zindex_contour,
       xindex_color, yindex_color, zindex_color,
       ndouble, nchar, ichar;
   unsigned int size;
   double x, y;
   char *string = NULL, ast1, ast2;
   const char *error_str[] =
      { "Error reading contour-plot data file.",
        "Error reading color-plot data file.",
        "Not enough memory for data.",
        "Not enough memory for line-break data." };
   FILE *fptr;


   /* Get sizes of data files */
   nplots = p_plot_param->nplots;
   npts_total = 0;
   npts_total_xcontour = 0;
   npts_total_ycontour = 0;
   npts_total_zcontour = 0;
   npts_total_xcolor = 0;
   npts_total_ycolor = 0;
   npts_total_zcolor = 0;
   index_filenames = 0;
   index_formats = 0;
   index_plot_types = 0;
   ipoints = 0;
   ihist = 0;
   icontour = 0;
   icolor = 0;
   nlinebreaks = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      /* Open data file */
      if ( (fptr = fopen( &filenames[index_filenames], "r")) == NULL )
         {
         size = strlen("Cannot open data file:\n") + strlen(&filenames[index_filenames]);
         string = xmalloc(size + 1);
         sprintf(string, "%s%s", "Cannot open data file:\n", &filenames[index_filenames]);
         ErrorDialog(string);
         free(string);
         exit(1);
         }

      /* Get size of data file */
      i = 0;
      if ( strcmp(p_plot_param->axis_type, "linear")   == 0 ||
           strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
           strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
           strcmp(p_plot_param->axis_type, "loglog")   == 0 ||
           strcmp(p_plot_param->axis_type, "polar")    == 0 )
         {
         if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
            {
            ipoints++;
            ndouble = 0;
            nchar = 0;
            while ( (ndouble = fscanf(fptr, &formats[index_formats], &x, &y)) == 2 ||
                    ((nchar = fscanf(fptr, &formats_mod[index_formats], &ast1, &ast2)) == 2 &&
                      ast1 == '*' && ast2 == '*') )
               {
               if ( ndouble == 2 )
                  {
                  i++;
                  ndouble = 0;
                  }
               else if ( nchar == 2 )
                  {
                  nlinebreaks++;
                  nchar = 0;
                  }
               }
            index_formats = index_formats + nformats[ipoints+ihist-1];
            npts = i;
            ndata[ipoints+ihist-1] = npts;
            npts_total = npts_total + npts;
            }

         else if ( strcmp(&plot_types[index_plot_types], "histogram") == 0 )
            {
            ihist++;
            while ( fscanf(fptr, &formats[index_formats], &x) == 1 )
               i++;
            index_formats = index_formats + nformats[ipoints+ihist-1];
            npts = i;
            ndata[ipoints+ihist-1] = npts;
            npts_total = npts_total + npts;
            }

         else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 )
            {
            icontour++;
            if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 )
               {
               ErrorDialog(error_str[0]);
               exit(1);
               }
            npts_total_xcontour = npts_total_xcontour + nx;
            npts_total_ycontour = npts_total_ycontour + ny;
            npts_total_zcontour = npts_total_zcontour + nx*ny;
            }

         else if ( strcmp(&plot_types[index_plot_types], "color")  == 0 )
            {
            icolor++;
            if ( fscanf(fptr, "%d %d", &nx, &ny) != 2 )
               {
               ErrorDialog(error_str[1]);
               exit(1);
               }
            npts_total_xcolor = npts_total_xcolor + nx;
            npts_total_ycolor = npts_total_ycolor + ny;
            npts_total_zcolor = npts_total_zcolor + nx*ny;
            }
         }

      /* Increment indices */
      fclose(fptr);
      index_filenames = index_filenames + nfilenames[iplot-1];
      index_plot_types = index_plot_types + 10;
      }


   /* Allocate memory for points- and histogram-data arrays */
   if ( (xdata = xmalloc(npts_total*sizeof(double))) == NULL ||
        (ydata = xmalloc(npts_total*sizeof(double))) == NULL )
      {
      ErrorDialog(error_str[2]);
      exit(1);
      }


   /* Allocate memory for contour-plot data arrays */
   if ( icontour > 0 )
      if ( (nxcontour = xmalloc(icontour*sizeof(int))) == NULL ||
           (nycontour = xmalloc(icontour*sizeof(int))) == NULL ||
           (xcontour = xmalloc(npts_total_xcontour*sizeof(double))) == NULL ||
           (ycontour = xmalloc(npts_total_ycontour*sizeof(double))) == NULL ||
           (zcontour = xmalloc(npts_total_zcontour*sizeof(double))) == NULL )
         {
         ErrorDialog(error_str[3]);
         exit(1);
         }


   /* Allocate memory for color-plot data arrays */
   if ( icolor > 0 )
      if ( (nxcolor = xmalloc(icolor*sizeof(int))) == NULL ||
           (nycolor = xmalloc(icolor*sizeof(int))) == NULL ||
           (xcolor = xmalloc(npts_total_xcolor*sizeof(double))) == NULL ||
           (ycolor = xmalloc(npts_total_ycolor*sizeof(double))) == NULL ||
           (zcolor = xmalloc(npts_total_zcolor*sizeof(double))) == NULL )
         {
         ErrorDialog(error_str[3]);
         exit(1);
         }


   /* Allocate memory for line-break index array */
   if ( nlinebreaks > 0 )
      if ( (nlinebreak = xmalloc(nlinebreaks*sizeof(int))) == NULL )
         {
         ErrorDialog(error_str[3]);
         exit(1);
         }


   /* Read data files */
   index_filenames = 0;
   index_formats = 0;
   index_plot_types = 0;
   index = 0;
   xindex_contour = 0;
   yindex_contour = 0;
   zindex_contour = 0;
   xindex_color = 0;
   yindex_color = 0;
   zindex_color = 0;
   ipoints = 0;
   ihist = 0;
   icontour = 0;
   icolor = 0;
   ichar = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      /* Open data file */
      fptr = fopen( &filenames[index_filenames], "r");

      /* Read data file */
      if ( strcmp(p_plot_param->axis_type, "linear")   == 0 ||
           strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
           strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
           strcmp(p_plot_param->axis_type, "loglog")   == 0 ||
           strcmp(p_plot_param->axis_type, "polar")    == 0 )
         {
         if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
            {
            ipoints++;
            npts = ndata[ipoints+ihist-1];
            nchar = 0;
            for ( i=1; i<=npts; i++ )
               if ( (ndouble = fscanf(fptr, &formats[index_formats], &xdata[index+i-1], &ydata[index+i-1])) == 2 ||
                    (nchar = fscanf(fptr, &formats_mod[index_formats], &ast1, &ast2)) == 2 )
                  if ( ndouble != 2 && nchar == 2 )
                     {
                     ichar++;
                     nlinebreak[ichar-1] = index + i - 1;
                     nchar = 0;
                     i--;
                     }

            /* Modify data for logarithmic axes */
            if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 ||
                 strcmp(p_plot_param->axis_type, "loglog")   == 0 )
               for ( i=1; i<=npts; i++ )
                  {
                  if ( xdata[index+i-1] != 0.0 )
                     xdata[index+i-1] = log10(fabs(xdata[index+i-1]));
                  else
                     xdata[index+i-1] = log10(DBL_MIN);
                  }

            if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
                 strcmp(p_plot_param->axis_type, "loglog")   == 0 )
               for ( i=1; i<=npts; i++ )
                  {
                  if ( ydata[index+i-1] != 0.0 )
                     ydata[index+i-1] = log10(fabs(ydata[index+i-1]));
                  else
                     ydata[index+i-1] = log10(DBL_MIN);
                  }

            index_formats = index_formats + nformats[ipoints+ihist-1];
            index = index + ndata[ipoints+ihist-1];
            }

         else if ( strcmp(&plot_types[index_plot_types], "histogram") == 0 )
            {
            ihist++;
            npts = ndata[ipoints+ihist-1];
            for ( i=1; i<=npts; i++ )
               {
               fscanf(fptr, &formats[index_formats], &xdata[index+i-1]);
               ydata[index+i-1] = 0.0;
               }

            qsort(&xdata[index], npts, sizeof(double), sort_compare_double);
            index_formats = index_formats + nformats[ipoints+ihist-1];
            index = index + ndata[ipoints+ihist-1];
            }

         else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 )
            {
            icontour++;
            if ( fscanf(fptr, "%d %d", &nxcontour[icontour-1], &nycontour[icontour-1]) != 2 )
               {
               ErrorDialog(error_str[0]);
               exit(1);
               }
            nx = nxcontour[icontour-1];
            ny = nycontour[icontour-1];

            for ( i=1; i<=nx; i++ )
               if ( fscanf(fptr, "%lf", &xcontour[xindex_contour+i-1]) != 1 )
                  {
                  ErrorDialog(error_str[0]);
                  exit(1);
                  }

            for ( j=1; j<=ny; j++ )
               if ( fscanf(fptr, "%lf", &ycontour[yindex_contour+j-1]) != 1 )
                  {
                  ErrorDialog(error_str[0]);
                  exit(1);
                  }

            for ( i=1; i<=nx; i++ )
               for ( j=1; j<=ny; j++ )
                  if ( fscanf(fptr, "%lf", &zcontour[zindex_contour+ny*(i-1)+j-1]) != 1 )
                     {
                     ErrorDialog(error_str[0]);
                     exit(1);
                     }

            xindex_contour = xindex_contour + nx;
            yindex_contour = yindex_contour + ny;
            zindex_contour = zindex_contour + nx*ny;
            }

         else if ( strcmp(&plot_types[index_plot_types], "color")  == 0 )
            {
            icolor++;
            if ( fscanf(fptr, "%d %d", &nxcolor[icolor-1], &nycolor[icolor-1]) != 2 )
               {
               ErrorDialog(error_str[1]);
               exit(1);
               }
            nx = nxcolor[icolor-1];
            ny = nycolor[icolor-1];

            for ( i=1; i<=nx; i++ )
               if ( fscanf(fptr, "%lf", &xcolor[xindex_color+i-1]) != 1 )
                  {
                  ErrorDialog(error_str[1]);
                  exit(1);
                  }

            for ( j=1; j<=ny; j++ )
               if ( fscanf(fptr, "%lf", &ycolor[yindex_color+j-1]) != 1 )
                  {
                  ErrorDialog(error_str[1]);
                  exit(1);
                  }

            for ( i=1; i<=nx; i++ )
               for ( j=1; j<=ny; j++ )
                  if ( fscanf(fptr, "%lf", &zcolor[zindex_color+ny*(i-1)+j-1]) != 1 )
                     {
                     ErrorDialog(error_str[1]);
                     exit(1);
                     }

            xindex_color = xindex_color + nx;
            yindex_color = yindex_color + ny;
            zindex_color = zindex_color + nx*ny;
            }
         }


      /* Increment indices */
      fclose(fptr);
      index_filenames = index_filenames + nfilenames[iplot-1];
      index_plot_types = index_plot_types + 10;
      }

   return;
   }


int sort_compare_double ( const void *p1, const void *p2 )
   {
   double key = *(const double *) p1,
          x   = *(const double *) p2;

   if ( key < x )
      return -1;
   else if ( key == x )
      return 0;
   else
      return 1;
   }
