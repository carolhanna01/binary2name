/*******************************************************************************
*
* CheckParamData.c
*
* Checks plot-parameter data read from plot-parameter file.
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


#include <ctype.h>
#include "gsegraf.h"


void CheckParamData ( void )
   {
   /* Declare variables */
   int iplot, nplots, iformat, index_filenames, index_formats, index_plot_types,
       index_bin_values, index_bin_refs, icount, i, j, k;
   unsigned int size;
   char *pchar, *string;
   extern char symbol_string[];   /* symbol-specification characters "ldcCtTsSiIpPhH+xra" */
   extern char color_string[];    /* color-specification characters "kaswrylqbfmogtnpx" */
   const char *error_str[] =
      { "Window size greater than screen size.",
        "Invalid or missing axis_type parameter.",
        "Invalid or missing plot_type parameter.",
        "Only points, histogram, contour, and color plot_type's\ncompatible with linear axis_type.",
        "Only points plot_type compatible with semilogx axis_type.",
        "Only points plot_type compatible with semilogy axis_type.",
        "Only points plot_type compatible with loglog axis_type.",
        "Only points plot_type compatible with polar axis_type.",
        "Only points, mesh, contour, and color plot_type's\ncompatible with 3d axis_type.",
        "Invalid or missing plot_style parameter.",
        "Invalid axis_scale parameter.",
        "Invalid grid parameter.",
        "Invalid bin_value parameter.",
        "Invalid bin_ref parameter.",
        "View-direction elevation angle out of range.",
        "Invalid date_time parameter.",
        "Invalid plot_box parameter.",
        "Invalid x_tick_marks parameter.",
        "Invalid y_tick_marks parameter.",
        "Invalid z_tick_marks parameter.",
        "Invalid x_tick_labels parameter.",
        "Invalid y_tick_labels parameter.",
        "Invalid z_tick_labels parameter.",
        "Invalid or missing data format;\nformat must specify double data type;\none data column read for histogram plot type.",
        "Invalid or missing data format;\nformat must specify double data type;\ntwo data columns read for 2d points plot type.",
        "Invalid or missing data format;\nformat must specify double data type;\nthree data columns read for 3d points plot type."};
   FILE *fptr;


   /* Check window size */
   if ( p_window_data->width  > width_screen ||
        p_window_data->height > height_screen )
      {
      ErrorDialog(error_str[0]);
      exit(1);
      }


   /* Get number of plots */
   nplots = p_plot_param->nplots;


   /* Check data filenames */
   index_filenames = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( (fptr = fopen(&filenames[index_filenames], "r")) == NULL )
         {
         size = strlen("Cannot open data file:\n") + strlen(&filenames[index_filenames]);
         string = xmalloc(size + 1);
         sprintf(string, "%s%s", "Cannot open data file:\n", &filenames[index_filenames]);
         ErrorDialog(string);
         free(string);
         exit(1);
         }
      else
         {
         fclose(fptr);
         index_filenames = index_filenames + nfilenames[iplot-1];
         }
      }


   /* Check axis_type parameter */
   if ( strcmp(p_plot_param->axis_type, "linear")   != 0 &&
        strcmp(p_plot_param->axis_type, "semilogx") != 0 &&
        strcmp(p_plot_param->axis_type, "semilogy") != 0 &&
        strcmp(p_plot_param->axis_type, "loglog")   != 0 &&
        strcmp(p_plot_param->axis_type, "polar")    != 0 &&
        strcmp(p_plot_param->axis_type, "3d")       != 0 )
      {
      ErrorDialog(error_str[1]);
      exit(1);
      }


   /* Check plot_type parameters */
   index_plot_types = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points")    != 0 &&
           strcmp(&plot_types[index_plot_types], "histogram") != 0 &&
           strcmp(&plot_types[index_plot_types], "mesh")      != 0 &&
           strcmp(&plot_types[index_plot_types], "contour")   != 0 &&
           strcmp(&plot_types[index_plot_types], "color")     != 0 )
         {
         ErrorDialog(error_str[2]);
         exit(1);
         }

      /* Increment index */
      index_plot_types = index_plot_types + 10;
      }


   /* Check compatibility of axis_type and plot_type parameters */
   index_plot_types = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(p_plot_param->axis_type, "linear") == 0 &&
           strcmp(&plot_types[index_plot_types], "points")    != 0 &&
           strcmp(&plot_types[index_plot_types], "histogram") != 0 &&
           strcmp(&plot_types[index_plot_types], "contour")   != 0 &&
           strcmp(&plot_types[index_plot_types], "color")     != 0 )
         {
         ErrorDialog(error_str[3]);
         exit(1);
         }

      else if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 &&
                strcmp(&plot_types[index_plot_types], "points") != 0 )
         {
         ErrorDialog(error_str[4]);
         exit(1);
         }

      else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 &&
                strcmp(&plot_types[index_plot_types], "points") != 0 )
         {
         ErrorDialog(error_str[5]);
         exit(1);
         }

      else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 &&
                strcmp(&plot_types[index_plot_types], "points") != 0 )
         {
         ErrorDialog(error_str[6]);
         exit(1);
         }

      else if ( strcmp(p_plot_param->axis_type, "polar") == 0 &&
                strcmp(&plot_types[index_plot_types], "points") != 0 )
         {
         ErrorDialog(error_str[7]);
         exit(1);
         }

      else if ( strcmp(p_plot_param->axis_type, "3d") == 0 &&
                strcmp(&plot_types[index_plot_types], "points")  != 0 &&
                strcmp(&plot_types[index_plot_types], "mesh")    != 0 &&
                strcmp(&plot_types[index_plot_types], "contour") != 0 &&
                strcmp(&plot_types[index_plot_types], "color")   != 0 )
         {
         ErrorDialog(error_str[8]);
         exit(1);
         }

      /* Increment index */
      index_plot_types = index_plot_types + 10;
      }


   /* Check compatibility of plot_type and plot_style parameters */
   index_plot_types = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
         {
         if ( styleflags[iplot-1] != 2 &&
              styleflags[iplot-1] != 4 )
            {
            ErrorDialog(error_str[9]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "histogram") == 0 )
         {
         if ( styleflags[iplot-1] != 2 &&
              styleflags[iplot-1] != 4 )
            {
            ErrorDialog(error_str[9]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 &&
                strcmp(p_plot_param->axis_type, "linear") == 0 )
         {
         if ( styleflags[iplot-1] != 1 &&
              styleflags[iplot-1] != 3 &&
              styleflags[iplot-1] != 7 )
            {
            ErrorDialog(error_str[9]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "color") == 0 &&
                strcmp(p_plot_param->axis_type, "linear") == 0 )
         {
         if ( styleflags[iplot-1] != 8 &&
              styleflags[iplot-1] != 9 )
            {
            ErrorDialog(error_str[9]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "mesh") == 0 )
         {
         if ( styleflags[iplot-1] != 2 &&
              styleflags[iplot-1] != 4 &&
              styleflags[iplot-1] != 5 &&
              styleflags[iplot-1] != 6 &&
              styleflags[iplot-1] != 7 )
            {
            ErrorDialog(error_str[9]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 &&
                strcmp(p_plot_param->axis_type, "3d") == 0 )
         {
         if ( styleflags[iplot-1] != 2 &&
              styleflags[iplot-1] != 4 &&
              styleflags[iplot-1] != 5 &&
              styleflags[iplot-1] != 6 )
            {
            ErrorDialog(error_str[9]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "color") == 0 &&
                strcmp(p_plot_param->axis_type, "3d") == 0 )
         {
         if ( styleflags[iplot-1] != 7 )
            {
            ErrorDialog(error_str[9]);
            exit(1);
            }
         }

      /* Increment index */
      index_plot_types = index_plot_types + 10;
      }


   /* Check stylechar1 data */
   index_filenames = 0;
   index_plot_types = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
         {
         if ( (pchar = strchr(symbol_string, stylechar1[iplot-1])) == NULL )
            {
            size = strlen("Invalid or missing plot symbol for file:\n") + strlen(&filenames[index_filenames]);
            string = xmalloc(size + 1);
            sprintf(string, "%s%s", "Invalid or missing plot symbol for file:\n", &filenames[index_filenames]);
            ErrorDialog(string);
            free(string);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "histogram") == 0 )
         {
         if ( (pchar = strchr("lbB", stylechar1[iplot-1])) == NULL )
            {
            size = strlen("Invalid or missing plot symbol for file:\n") + strlen(&filenames[index_filenames]);
            string = xmalloc(size + 1);
            sprintf(string, "%s%s", "Invalid or missing plot symbol for file:\n", &filenames[index_filenames]);
            ErrorDialog(string);
            free(string);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "mesh") == 0 )
         {
         if ( styleflags[iplot-1] ==  2 ||
              styleflags[iplot-1] ==  4 )
            {
            if ( (pchar = strchr(color_string, stylechar1[iplot-1])) == NULL )
               {
               size = strlen("Invalid or missing plot color for file:\n") + strlen(&filenames[index_filenames]);
               string = xmalloc(size + 1);
               sprintf(string, "%s%s", "Invalid or missing plot color for file:\n", &filenames[index_filenames]);
               ErrorDialog(string);
               free(string);
               exit(1);
               }
            }
         }

      else if ( strcmp(p_plot_param->axis_type, "3d") == 0 &&
                strcmp(&plot_types[index_plot_types], "contour") == 0 )
         {
         if ( styleflags[iplot-1] ==  2 ||
              styleflags[iplot-1] ==  4 )
            {
            if ( (pchar = strchr(color_string, stylechar1[iplot-1])) == NULL )
               {
               size = strlen("Invalid or missing plot color for file:\n") + strlen(&filenames[index_filenames]);
               string = xmalloc(size + 1);
               sprintf(string, "%s%s", "Invalid or missing plot color for file:\n", &filenames[index_filenames]);
               ErrorDialog(string);
               free(string);
               exit(1);
               }
            }
         }

      /* Increment indices */
      index_filenames = index_filenames + nfilenames[iplot-1];
      index_plot_types = index_plot_types + 10;
      }


   /* Check stylechar2 data */
   index_filenames = 0;
   index_plot_types = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points")    == 0 ||
           strcmp(&plot_types[index_plot_types], "histogram") == 0 ||
           strcmp(&plot_types[index_plot_types], "mesh")      == 0 ||
           strcmp(&plot_types[index_plot_types], "contour")   == 0 )
         {
         if ( styleflags[iplot-1] ==  2 ||
              styleflags[iplot-1] ==  5 )
            if ( (pchar = strchr(color_string, stylechar2[iplot-1])) == NULL )
               {
               size = strlen("Invalid or missing plot color for file:\n") + strlen(&filenames[index_filenames]);
               string = xmalloc(size + 1);
               sprintf(string, "%s%s", "Invalid or missing plot color for file:\n", &filenames[index_filenames]);
               ErrorDialog(string);
               free(string);
               exit(1);
               }
         }

      index_filenames = index_filenames + nfilenames[iplot-1];
      index_plot_types = index_plot_types + 10;
      }


   /* Check axis_scale parameter */
   if ( strcmp(p_plot_param->axis_scale, "auto")  != 0 &&
        strcmp(p_plot_param->axis_scale, "equal") != 0 )
      {
      ErrorDialog(error_str[10]);
      exit(1);
      }


   /* Check grid parameters */
   if ( strcmp(p_plot_param->grid, "on1") != 0 &&
        strcmp(p_plot_param->grid, "on2") != 0 &&
        strcmp(p_plot_param->grid, "off") != 0 )
      {
      ErrorDialog(error_str[11]);
      exit(1);
      }

   if ( strcmp(p_plot_param->grid, "on1") == 0 )
      {
      if ( (gridchar1 != 'l' && gridchar1 != 'd' && gridchar1 != '.') ||
           (pchar = strchr(color_string, gridchar2)) == NULL )
         {
         ErrorDialog(error_str[11]);
         exit(1);
         }
      }
   else if ( strcmp(p_plot_param->grid, "on2") == 0 )
      {
      if ( gridchar1 != 'l' && gridchar1 != 'd' )
         {
         ErrorDialog(error_str[11]);
         exit(1);
         }
      }


   /* Check bin_value parameter for histograms */
   index_bin_values = 0;
   for ( i=1; i<=nplots; i++ )
      {
      if ( strcmp(&bin_values[index_bin_values], "number")   != 0 &&
           strcmp(&bin_values[index_bin_values], "fraction") != 0 &&
           strcmp(&bin_values[index_bin_values], "percent")  != 0 )
         {
         ErrorDialog(error_str[12]);
         exit(1);
         }

      index_bin_values = index_bin_values + 9;
      }


   /* Check bin_refs parameter for histograms */
   index_bin_refs = 0;
   for ( i=1; i<=nplots; i++ )
      {
      if ( strcmp(&bin_refs[index_bin_refs], "mean")     != 0 &&
           strcmp(&bin_refs[index_bin_refs], "zero")     != 0 &&
           strcmp(&bin_refs[index_bin_refs], "integers") != 0 )
         {
         ErrorDialog(error_str[13]);
         exit(1);
         }

      index_bin_refs = index_bin_refs + 9;
      }


   /* Check view-direction angles for 3d plots */
   if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
      if ( p_plot_param_3d->theta <  0.0 ||
           p_plot_param_3d->theta > 90.0 )
         {
         ErrorDialog(error_str[14]);
         exit(1);
         }


   /* Check date_time parameter */
   if ( strcmp(p_plot_param->date_time_anchor, "off")       != 0 &&
        strcmp(p_plot_param->date_time_anchor, "north")     != 0 &&
        strcmp(p_plot_param->date_time_anchor, "northeast") != 0 &&
        strcmp(p_plot_param->date_time_anchor, "southeast") != 0 &&
        strcmp(p_plot_param->date_time_anchor, "south")     != 0 &&
        strcmp(p_plot_param->date_time_anchor, "southwest") != 0 &&
        strcmp(p_plot_param->date_time_anchor, "northwest") != 0 )
      {
      ErrorDialog(error_str[15]);
      exit(1);
      }


   /* Check plot_box parameter */
   if ( strcmp(p_plot_param->plot_box, "on")  != 0 &&
        strcmp(p_plot_param->plot_box, "off") != 0 )
      {
      ErrorDialog(error_str[16]);
      exit(1);
      }


   /* Check x_tick_marks parameter */
   if ( strcmp(p_plot_param->x_tick_marks, "on")  != 0 &&
        strcmp(p_plot_param->x_tick_marks, "off") != 0 )
      {
      ErrorDialog(error_str[17]);
      exit(1);
      }


   /* Check y_tick_marks parameter */
   if ( strcmp(p_plot_param->y_tick_marks, "on")  != 0 &&
        strcmp(p_plot_param->y_tick_marks, "off") != 0 )
      {
      ErrorDialog(error_str[18]);
      exit(1);
      }


   /* Check z_tick_marks parameter */
   if ( strcmp(p_plot_param->z_tick_marks, "on")  != 0 &&
        strcmp(p_plot_param->z_tick_marks, "off") != 0 )
      {
      ErrorDialog(error_str[19]);
      exit(1);
      }


   /* Check x_tick_labels parameter */
   if ( strcmp(p_plot_param->x_tick_labels, "on")  != 0 &&
        strcmp(p_plot_param->x_tick_labels, "off") != 0 )
      {
      ErrorDialog(error_str[20]);
      exit(1);
      }


   /* Check y_tick_labels parameter */
   if ( strcmp(p_plot_param->y_tick_labels, "on")  != 0 &&
        strcmp(p_plot_param->y_tick_labels, "off") != 0 )
      {
      ErrorDialog(error_str[21]);
      exit(1);
      }


   /* Check z_tick_labels parameter */
   if ( strcmp(p_plot_param->z_tick_labels, "on")  != 0 &&
        strcmp(p_plot_param->z_tick_labels, "off") != 0 )
      {
      ErrorDialog(error_str[22]);
      exit(1);
      }


   /* Check data formats for points and histogram plot types */
   index_plot_types = 0;
   index_formats = 0;
   iformat = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points")    == 0 ||
           strcmp(&plot_types[index_plot_types], "histogram") == 0 )
         {
         iformat++;

         /* Check that format is for double data type */
         icount = 0;
         i = 0;
         while ( (pchar = strchr(&formats[index_formats+i], '%')) != NULL )
            {
            if ( *(pchar+1) != '*' )
               {
               j = 0;
               while ( isdigit(*(pchar+j+1)) != 0 )
                  j++;
               if ( *(pchar+j+1) == 'l' &&
                    (*(pchar+j+2) == 'e' || *(pchar+j+2) == 'f' || *(pchar+j+2) == 'g') )
                  icount++;
               }
            i = i + pchar - &formats[index_formats+i] + j + 3;
            }

         if ( strcmp(&plot_types[index_plot_types], "histogram") == 0 )
            {
            if ( icount != 1 )
               {
               ErrorDialog(error_str[23]);
               exit(1);
               }
            }
         else if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
            {
            if ( strcmp(p_plot_param->axis_type, "linear")    == 0 ||
                 strcmp(p_plot_param->axis_type, "semilogx")  == 0 ||
                 strcmp(p_plot_param->axis_type, "semilogy")  == 0 ||
                 strcmp(p_plot_param->axis_type, "loglog")    == 0 ||
                 strcmp(p_plot_param->axis_type, "polar")     == 0 )
               {
               if ( icount != 2 )
                  {
                  ErrorDialog(error_str[24]);
                  exit(1);
                  }
               }
            else if ( strcmp(p_plot_param->axis_type, "3d") == 0 )
               {
               if ( icount != 3 )
                  {
                  ErrorDialog(error_str[25]);
                  exit(1);
                  }
               }
            }

         /* Increment indices */
         index_formats = index_formats + nformats[iformat-1];
         index_plot_types = index_plot_types + 10;
         }
      }


   /* Create modified data formats to read asterisks instead of numbers for points plot types */
   index_plot_types = 0;
   index_formats = 0;
   iformat = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points")    == 0 ||
           strcmp(&plot_types[index_plot_types], "histogram") == 0 )
         {
         iformat++;

         strcpy(&formats_mod[index_formats], &formats[index_formats]);

         i = 0;
         while ( (pchar = strchr(&formats[index_formats+i], '%')) != NULL )
            {
            if ( *(pchar+1) != '*' )
               {
               j = pchar - &formats[index_formats+i];
               k = 0;
               while ( isdigit(*(pchar+k+1)) != 0 )
                  {
                  formats_mod[index_formats+i+j+k+1] = ' ';
                  k++;
                  }
               formats_mod[index_formats+i+j+k+1] = ' ';
               formats_mod[index_formats+i+j+k+2] = ' ';
               formats_mod[index_formats+i+j+1] = 'c';
               }
            i = i + j + k + 3;
            }

         index_formats = index_formats + nformats[iformat-1];
         index_plot_types = index_plot_types + 10;
         }
      }

   return;
   }
