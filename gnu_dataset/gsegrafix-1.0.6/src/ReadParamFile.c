/*******************************************************************************
*
* ReadParamFile.c
*
* Reads plot-parameter file.
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
#include <math.h>
#include "gsegraf.h"


void ReadParamFile ( void )
   {
   /* Declare variables */
   int i, j, ch, iplot, nplots, iformat, nformat, nfilenames_total, nformats_total,
       index, index_filenames, index_formats, index_plot_types, index_bin_values,
       index_bin_refs, ibinwidth, index_stemflags, ibinvalue, ibinref, ininterp,
       istemflag, imeshcolor, icontourcolor, stylesize;
   unsigned int i1_str, i2_str, size;
   char *string, *pchar, character;
   extern char color_string[];   /* color-specification characters "kaswrylqbfmogtnpx" */
   const char *error_str[] =
      { "Missing file_name data.",
        "Missing file_format data.",
        "Invalid or missing axis_type parameter.",
        "Invalid or missing plot_type parameter.",
        "Invalid axis_limits parameter.",
        "Invalid or incomplete view direction angles.",
        "Invalid grid parameter.",
        "Invalid histogram bin width.",
        "Invalid ninterp value.",
        "Invalid contours value.",
        "Invalid or missing contour color.",
        "Invalid or missing mesh color.",
        "Invalid window-size data;\ntwo integers required.",
        "Invalid or missing date-time font size.",
        "Invalid or missing legend font size.",
        "Invalid or missing text font size.",
        "Invalid or missing tick-label font size.",
        "Invalid or missing axis-label font size.",
        "Invalid or missing title font size.",
        "Invalid x-axis limit.",
        "Invalid y-axis limit.",
        "Invalid axis limit." };
   FILE *fptr;


   /* Open plot-parameter file */
   if ( (fptr = fopen(p_param_file, "r")) == NULL )
      {
      size = strlen("Cannot open plot-parameter file:\n") + strlen(p_param_file);
      string = xmalloc(size + 1);
      sprintf(string, "%s%s", "Cannot open plot-parameter file:\n", p_param_file);
      ErrorDialog(string);
      free(string);
      exit(1);
      }


   /* Get maximum line length */
   maxline = 0;
   i = 0;
   while ( (ch = fgetc(fptr)) != EOF )
      {
      if ( ch != '\n' )
         i++;
      else
         {
         if ( maxline < i )
            maxline = i;
         i = 0;
         }
      }
   maxline++;


   /****************************************************************************
   * Increase maxline for use with char *fgets(char *s, int n, FILE *stream).
   * fgets reads at most the next n-1 characters into the array s. It is desired
   * that fgets read the terminating character so that following uses of fgets
   * start with the next line in the file.
   ****************************************************************************/
   maxline++;
   line = xmalloc(maxline*sizeof(char));


   /* Get number of file_name entries */
   iplot = 0;
   rewind(fptr);
   while ( fgets(line, maxline, fptr) != NULL )
      {
      if ( strncmp(line, "file_name", 9) == 0 )
         iplot++;
      else if ( strncmp(line, "#####", 5) == 0 )
         break;
      }
   nplots = iplot;
   p_plot_param->nplots = nplots;
   if ( nplots > 0 )
      nfilenames = xmalloc(nplots*sizeof(int));


   /* Get number of file_format entries */
   nformat = 0;
   if ( nplots > 0 )
      {
      iformat = 0;
      rewind(fptr);
      while ( fgets(line, maxline, fptr) != NULL )
         {
         if ( strncmp(line, "file_format", 11) == 0 )
            iformat++;
         else if ( strncmp(line, "#####", 5) == 0 )
            break;
         }
      nformat = iformat;
      if ( nformat > 0 )
         nformats = xmalloc(nformat*sizeof(int));
      }


   /* Get filename sizes */
   nfilenames_total = 0;
   iplot = 0;
   rewind(fptr);
   if ( nplots > 0 )
      {
      while ( fgets(line, maxline, fptr) != NULL )
         {
         if ( strncmp(line, "file_name", 9) == 0 )
            {
            iplot++;
            size = strlen(&line[9]);
            if ( (string = get_string(&line[9], &i1_str, &i2_str, &size, 0)) != NULL )
               nfilenames[iplot-1] = size + 1;
            else
               {
               ErrorDialog(error_str[0]);
               exit(1);
               }

            nfilenames_total = nfilenames_total + nfilenames[iplot-1];
            }

         else if ( strncmp(line, "#####", 5) == 0 )
            break;
         }
      }


   /* Get format sizes */
   nformats_total = 0;
   iformat = 0;
   rewind(fptr);
   if ( nformat > 0 )
      {
      while ( fgets(line, maxline, fptr) != NULL )
         {
         if ( strncmp(line, "file_format", 11) == 0 )
            {
            iformat++;
            size = strlen(&line[11]);
            if ( (string = get_string(&line[11], &i1_str, &i2_str, &size, 0)) != NULL )
               nformats[iformat-1] = size + 1;
            else
               {
               ErrorDialog(error_str[1]);
               exit(1);
               }

            nformats_total = nformats_total + nformats[iformat-1];
            }

         else if ( strncmp(line, "#####", 5) == 0 )
            break;
         }
      }


   /* Get background color */
   rewind(fptr);
   while ( fgets(line, maxline, fptr) != NULL )
      {
      if ( strncmp(line, "background_color", 16) == 0 )
         {
         size = strlen(&line[16]);
         if ( (string = get_string(&line[16], &i1_str, &i2_str, &size, 0)) != NULL )
            if ( strcmp(string, "black") == 0 )
               {
               canvas_bg_color = 0x000000FF;   /* black */
               canvas_fg_color = 0xFFFFFFFF;   /* white */
               }
         }

      else if ( strncmp(line, "background_image", 16) == 0 )
         {
         size = strlen(&line[16]);
         if ( (string = get_string(&line[16], &i1_str, &i2_str, &size, 0)) != NULL )
            {
            background_image_file = xmalloc(size + 1);
            strcpy(background_image_file, string);
            }
         index = 16 + i2_str + 2;
         size = strlen(&line[index]);
         if ( (string = get_string(&line[index], &i1_str, &i2_str, &size, 0)) != NULL )
            {
            if ( strcmp(string, "center") == 0 )
               background_image_style = 1;
            else if ( strcmp(string, "fill") == 0 )
               background_image_style = 2;
            else if ( strcmp(string, "scale") == 0 )
               background_image_style = 3;
            else if ( strcmp(string, "zoom") == 0 )
               background_image_style = 4;
            }
         }

      else if ( strncmp(line, "#####", 5) == 0 )
         break;
      }


   /* Allocate memory for plot parameters */
   filenames           = xmalloc(nfilenames_total*sizeof(char));
   formats             = xmalloc(nformats_total*sizeof(char));
   formats_mod         = xmalloc(nformats_total*sizeof(char));
   plot_types          = xmalloc(10*nplots*sizeof(char));
   styleflags          = xmalloc(nplots*sizeof(int));
   stylechar1          = xmalloc(nplots*sizeof(char));
   stylechar2          = xmalloc(nplots*sizeof(char));
   stylecolor1         = xmalloc(nplots*sizeof(guint32));
   stylecolor2         = xmalloc(nplots*sizeof(guint32));
   alphacolor          = xmalloc(nplots*sizeof(guint32));
   stylesizes          = xmalloc(nplots*sizeof(unsigned int));
   zblack              = xmalloc(nplots*sizeof(double));
   zwhite              = xmalloc(nplots*sizeof(double));
   fill_colors_rgba    = xmalloc(nplots*sizeof(guint32));
   outline_colors_rgba = xmalloc(nplots*sizeof(guint32));
   bin_widths          = xmalloc(nplots*sizeof(double));
   bin_values          = xmalloc(9*nplots*sizeof(char));
   bin_refs            = xmalloc(9*nplots*sizeof(char));
   ninterp             = xmalloc(nplots*sizeof(int));
   stemflags           = xmalloc(4*nplots*sizeof(char));
   stemvalues          = xmalloc(nplots*sizeof(double));
   meshcolors          = xmalloc(nplots*sizeof(guint32));
   contourcolors       = xmalloc(nplots*sizeof(guint32));
   ndata               = xmalloc(nplots*sizeof(int));
   memset(filenames,   0, nfilenames_total*sizeof(char));
   memset(formats,     0, nformats_total*sizeof(char));
   memset(formats_mod, 0, nformats_total*sizeof(char));
   memset(plot_types,  0, 10*nplots*sizeof(char));
   memset(stylechar1,  0, nplots*sizeof(char));
   memset(stylechar2,  0, nplots*sizeof(char));
   memset(bin_values,  0, 9*nplots*sizeof(char));
   memset(bin_refs,    0, 9*nplots*sizeof(char));
   memset(stemflags,   0, 4*nplots*sizeof(char));


   /* Set default style parameters */
   for ( i=1; i<=nplots; i++ )
      {
      styleflags[i-1] = -1;
      stylechar1[i-1] = '*';
      stylechar2[i-1] = '*';
      stylesizes[i-1] = 1;
      alphacolor[i-1] = 0xFF;
      zblack[i-1] = -DBL_MAX;
      zwhite[i-1] =  DBL_MAX;
      }


   /* Set default histogram bin_widths and bin_values */
   for ( i=1; i<=nplots; i++ )
      {
      bin_widths[i-1] = -1.0;
      strcpy(&bin_values[(i-1)*9], "percent");
      strcpy(&bin_refs[(i-1)*9], "mean");
      }


   /* Set default stem flags and stem values */
   for ( i=1; i<=nplots; i++ )
      {
      strcpy(&stemflags[(i-1)*4], "off");
      stemvalues[i-1] = 0.0;   /* not used */
      }


   /* Set default number of contours */
   /* (2d and 3d contour plots) */
   ncontours = 0;


   /* Set default number of interpolation intervals */
   /* (3d color plots) */
   for ( i=1; i<=nplots; i++ )
      ninterp[i-1] = 20;


   /* Set default 3d-mesh colors and 3d-contour colors */
   for ( i=1; i<=nplots; i++ )
      {
      meshcolors[i-1] = canvas_fg_color;
      contourcolors[i-1] = canvas_fg_color;
      }


   /* Read filenames */
   iplot = 0;
   index_filenames = 0;
   rewind(fptr);
   if ( nplots > 0 )
      {
      while ( fgets(line, maxline, fptr) != NULL )
         {
         if ( strncmp(line, "file_name", 9) == 0 )
            {
            iplot++;
            size = strlen(&line[9]);
            if ( (string = get_string(&line[9], &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strcpy(&filenames[index_filenames], string);
               index_filenames = index_filenames + nfilenames[iplot-1];
               }
            }

         else if ( strncmp(line, "#####", 5) == 0 )
            break;
         }
      }


   /* Read file formats */
   iformat = 0;
   index_formats = 0;
   index_plot_types = 0;
   rewind(fptr);
   if ( nformat > 0 )
      {
      while ( fgets(line, maxline, fptr) != NULL )
         {
         if ( strncmp(line, "file_format", 11) == 0 )
            {
            iformat++;
            size = strlen(&line[11]);
            if ( (string = get_string(&line[11], &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strcpy(&formats[index_formats], string);
               index_formats = index_formats + nformats[iformat-1];
               }
            }

         else if ( strncmp(line, "#####", 5) == 0 )
            break;
         }
      }


   /* Read axis type */
   rewind(fptr);
   while ( fgets(line, maxline, fptr) != NULL )
      {
      if ( strncmp(line, "axis_type", 9) == 0 )
         {
         size = strlen(&line[9]);
         if ( (string = get_string(&line[9], &i1_str, &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->axis_type[0], string, 8);
         }

      else if ( strncmp(line, "#####", 5) == 0 )
         break;
      }

   if ( strcmp(p_plot_param->axis_type, "linear")   != 0 &&
        strcmp(p_plot_param->axis_type, "semilogx") != 0 &&
        strcmp(p_plot_param->axis_type, "semilogy") != 0 &&
        strcmp(p_plot_param->axis_type, "loglog")   != 0 &&
        strcmp(p_plot_param->axis_type, "polar")    != 0 &&
        strcmp(p_plot_param->axis_type, "3d")       != 0 )
      {
      ErrorDialog(error_str[2]);
      exit(1);
      }


   /* Read plot types */
   iplot = 0;
   index_plot_types = 0;
   rewind(fptr);
   if ( nplots > 0 )
      {
      while ( fgets(line, maxline, fptr) != NULL )
         {
         if ( strncmp(line, "plot_type", 9) == 0 )
            {
            iplot++;
            size = strlen(&line[9]);
            if ( (string = get_string(&line[9], &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strncpy(&plot_types[index_plot_types], string, 9);
               index_plot_types = index_plot_types + 10;
               }
            }

         else if ( strncmp(line, "#####", 5) == 0 )
            break;
         }
      }

   index_plot_types = 0;
   for ( iplot=1; iplot<=nplots; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points")    != 0 &&
           strcmp(&plot_types[index_plot_types], "histogram") != 0 &&
           strcmp(&plot_types[index_plot_types], "color")     != 0 &&
           strcmp(&plot_types[index_plot_types], "contour")   != 0 &&
           strcmp(&plot_types[index_plot_types], "mesh")      != 0 )
         {
         ErrorDialog(error_str[3]);
         exit(1);
         }

      /* Increment index */
      index_plot_types = index_plot_types + 10;
      }


   /* Read plot styles */
   iplot = 0;
   index_plot_types = 0;
   rewind(fptr);
   if ( nplots > 0 )
      {
      while ( fgets(line, maxline, fptr) != NULL )
         {
         if ( strncmp(line, "plot_style", 10) == 0 )
            {
            iplot++;

            if ( strcmp(&plot_types[index_plot_types], "points")    == 0 ||
                 strcmp(&plot_types[index_plot_types], "histogram") == 0 )
               {
               if ( (sscanf(&line[10], " %c 0x%x", &stylechar1[iplot-1], (unsigned int *) &stylecolor2[iplot-1]) == 2 ||
                     sscanf(&line[10], " %c 0X%x", &stylechar1[iplot-1], (unsigned int *) &stylecolor2[iplot-1]) == 2) &&
                    isdigit(stylechar1[iplot-1]) == 0 )
                  {
                  /************************************************************
                  *
                  * 2d points and 3d points plot types
                  * Read symbol character, hexadecimal color, and symbol size
                  *
                  * 2d histogram
                  * Read symbol character and hexadecimal color
                  *
                  ************************************************************/
                  styleflags[iplot-1] = 4;
                  fill_colors_rgba[iplot-1]    = stylecolor2[iplot-1];         /* set specified fill color */
                  outline_colors_rgba[iplot-1] = stylecolor2[iplot-1];         /* set specified outline color */
                  if ( (pchar = strchr("ctsiphb", stylechar1[iplot-1])) != NULL )
                     fill_colors_rgba[iplot-1] = canvas_bg_color;             /* set fill color to background color */
                  if ( (pchar = strchr("cCtTsSiIpPhH+xra", stylechar1[iplot-1])) != NULL )
                     stylesizes[iplot-1] = 6;                                  /* set default symbol size */
                  if ( sscanf(&line[10], " %*c %*s %d", &stylesize) == 1 )
                     stylesizes[iplot-1] = abs(stylesize);                     /* get symbol size */
                  }

               else if ( sscanf(&line[10], " %c %c", &stylechar1[iplot-1], &stylechar2[iplot-1]) == 2 &&
                         (pchar = strchr(color_string, stylechar2[iplot-1])) != NULL )
                  {
                  /************************************************************
                  *
                  * 2d points and 3d points plot types
                  * Read symbol character, color character, and symbol size
                  *
                  * 2d histogram
                  * Read symbol character and color character
                  *
                  ************************************************************/
                  styleflags[iplot-1] = 2;
                  index = pchar - &color_string[0];                            /* get index to color character */
                  stylecolor2[iplot-1] = color_rgba[index];                    /* set specified color */
                  fill_colors_rgba[iplot-1]    = stylecolor2[iplot-1];         /* set specified fill color */
                  outline_colors_rgba[iplot-1] = stylecolor2[iplot-1];         /* set specified outline color */
                  if ( (pchar = strchr("ctsiphb", stylechar1[iplot-1])) != NULL )
                     fill_colors_rgba[iplot-1] = canvas_bg_color;              /* set fill color to background color */
                  if ( (pchar = strchr("cCtTsSiIpPhH+xra", stylechar1[iplot-1])) != NULL )
                     stylesizes[iplot-1] = 6;                                  /* set default symbol size */
                  if ( sscanf(&line[10], " %*c %*c %d", &stylesize) == 1 )
                     stylesizes[iplot-1] = abs(stylesize);                     /* get symbol size */
                  }
               }

            else if ( strcmp(&plot_types[index_plot_types], "color") == 0 &&
                      strcmp(p_plot_param->axis_type, "linear") == 0 )
               {
               size = strlen(&line[10]);
               if ( (string = get_string(&line[10], &i1_str, &i2_str, &size, 0)) != NULL )
                  {
                  /************************************************************
                  *
                  * 2d color plot type
                  * Read "nearest" or "bilinear" string and two decimal numbers
                  *
                  ************************************************************/
                  if ( strcmp(string, "nearest") == 0 )
                     {
                     styleflags[iplot-1] = 9;
                     sscanf(&string[size+1], " %lf %lf", &zblack[iplot-1], &zwhite[iplot-1]);
                     }
                  else if ( strcmp(string, "bilinear") == 0 )
                     {
                     styleflags[iplot-1] = 8;
                     sscanf(&string[size+1], " %lf %lf", &zblack[iplot-1], &zwhite[iplot-1]);
                     }
                  }
               }

            else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 &&
                      strcmp(p_plot_param->axis_type, "linear") == 0 )
               {
               size = strlen(&line[10]);
               if ( (string = get_string(&line[10], &i1_str, &i2_str, &size, 0)) != NULL )
                  {
                  /************************************************************
                  *
                  * 2d contour plot type
                  * Read "auto" string and line width
                  *
                  ************************************************************/
                  if ( strcmp(string, "auto") == 0 )
                     {
                     styleflags[iplot-1] = 7;
                     if ( sscanf(&string[size+1], " %d", &stylesize) == 1 )
                        stylesizes[iplot-1] = abs(stylesize);                  /* get line width */
                     }
                  }

               else if ( sscanf(&line[10], " 0x%x", (unsigned int *) &stylecolor1[iplot-1]) == 1 ||
                         sscanf(&line[10], " 0X%x", (unsigned int *) &stylecolor1[iplot-1]) == 1 )
                  {
                  /************************************************************
                  *
                  * 2d contour plot type
                  * Read hexadecimal color and line width
                  *
                  ************************************************************/
                  styleflags[iplot-1] = 3;
                  if ( sscanf(&line[10], " %*s %d", &stylesize) == 1 )
                     stylesizes[iplot-1] = abs(stylesize);                     /* get line width */
                  }

               else if ( sscanf(&line[10], " %c", &stylechar1[iplot-1]) == 1 &&
                         (pchar = strchr(color_string, stylechar1[iplot-1])) != NULL )
                  {
                  /************************************************************
                  *
                  * 2d contour plot type
                  * Read color character and line width
                  *
                  ************************************************************/
                  styleflags[iplot-1] = 1;
                  index = pchar - &color_string[0];                            /* get index to color character */
                  stylecolor1[iplot-1] = color_rgba[index];                    /* set specified color */
                  if ( sscanf(&line[10], " %*c %d", &stylesize) == 1 )
                     stylesizes[iplot-1] = abs(stylesize);                     /* get line width */
                  }
               }

            else if ( (strcmp(&plot_types[index_plot_types], "contour") == 0 ||
                       strcmp(&plot_types[index_plot_types], "color")   == 0 ||
                       strcmp(&plot_types[index_plot_types], "mesh")    == 0) &&
                      strcmp(p_plot_param->axis_type, "3d") == 0 )
               {
               size = strlen(&line[10]);
               if ( (strcmp(&plot_types[index_plot_types], "color") == 0 ||
                     strcmp(&plot_types[index_plot_types], "mesh")  == 0) &&
                    (string = get_string(&line[10], &i1_str, &i2_str, &size, 0)) != NULL )
                  {
                  /************************************************************
                  *
                  * 3d color and 3d mesh plot types
                  * Read "auto" string and color alpha value
                  *
                  ************************************************************/
                  if ( strcmp(string, "auto") == 0 )
                     {
                     styleflags[iplot-1] = 7;
                     if ( sscanf(&string[size+1], " 0x%x", (unsigned int *) &alphacolor[iplot-1]) == 1 ||
                          sscanf(&string[size+1], " 0X%x", (unsigned int *) &alphacolor[iplot-1]) == 1 )
                        {
                        if ( alphacolor[iplot-1] > 0xFF )                      /* check alpha value */
                           alphacolor[iplot-1] = 0xFF;
                        }
                     }
                  }

               else if ( sscanf(&line[10], " 0x%x 0x%x", (unsigned int *) &stylecolor1[iplot-1], (unsigned int *) &stylecolor2[iplot-1]) == 2 ||
                         sscanf(&line[10], " 0X%x 0X%x", (unsigned int *) &stylecolor1[iplot-1], (unsigned int *) &stylecolor2[iplot-1]) == 2 ||
                         sscanf(&line[10], " 0x%x 0X%x", (unsigned int *) &stylecolor1[iplot-1], (unsigned int *) &stylecolor2[iplot-1]) == 2 ||
                         sscanf(&line[10], " 0X%x 0x%x", (unsigned int *) &stylecolor1[iplot-1], (unsigned int *) &stylecolor2[iplot-1]) == 2 )
                  {
                  /************************************************************
                  *
                  * 3d contour and 3d mesh plot types
                  * Read two hexadecimal colors
                  *
                  ************************************************************/
                  styleflags[iplot-1] = 6;
                  }

               else if ( sscanf(&line[10], " 0x%x %c", (unsigned int *) &stylecolor1[iplot-1], &stylechar2[iplot-1]) == 2 ||
                         sscanf(&line[10], " 0X%x %c", (unsigned int *) &stylecolor1[iplot-1], &stylechar2[iplot-1]) == 2 )
                  {
                  /************************************************************
                  *
                  * 3d contour and 3d mesh plot types
                  * Read hexadecimal color and color character
                  *
                  ************************************************************/
                  styleflags[iplot-1] = 5;
                  }

               else if ( sscanf(&line[10], " %c 0x%x", &stylechar1[iplot-1], (unsigned int *) &stylecolor2[iplot-1]) == 2 ||
                         sscanf(&line[10], " %c 0X%x", &stylechar1[iplot-1], (unsigned int *) &stylecolor2[iplot-1]) == 2 )
                  {
                  /************************************************************
                  *
                  * 3d contour and 3d mesh plot types
                  * Read color character and hexadecimal color
                  *
                  ************************************************************/
                  styleflags[iplot-1] = 4;
                  }

               else if ( sscanf(&line[10], " %c %c", &stylechar1[iplot-1], &stylechar2[iplot-1]) == 2 )
                  {
                  /************************************************************
                  *
                  * 3d contour and 3d mesh plot types
                  * Read two color characters
                  *
                  ************************************************************/
                  styleflags[iplot-1] = 2;
                  }
               }

            /* Increment index */
            index_plot_types = index_plot_types + 10;
            }

         else if ( strncmp(line, "#####", 5) == 0 )
            break;
         }
      }


   /* Read remainder of plot-parameter file */
   ibinwidth = 0;
   ibinvalue = 0;
   ibinref = 0;
   ininterp = 0;
   istemflag = 0;
   imeshcolor = 0;
   icontourcolor = 0;
   index_bin_values = 0;
   index_bin_refs = 0;
   index_stemflags = 0;
   rewind(fptr);
   while ( fgets(line, maxline, fptr) != NULL )
      {
      if ( strncmp(line, "axis_scale", 10) == 0 )
         {
         size = strlen(&line[10]);
         if ( (string = get_string(&line[10], &i1_str, &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->axis_scale[0], string, 5);
         }

      else if ( strncmp(line, "axis_limits", 11) == 0 )
         {
         size = strlen(line);
         i = 11;
         for ( j=1; j<=6; j++ )
            {
            while ( line[i] == ' ' || line[i] == '\t' )
               i++;

            if ( sscanf(&line[i], "%lf", &p_plot_param->axis_limits[j-1]) == 1 )
               axis_limits[j-1] = 1;
            else if ( sscanf(&line[i], " %c", &character ) == 1 )
               {
               if ( character == '#' )
                  break;
               else if ( character != '*' )
                  {
                  ErrorDialog(error_str[4]);
                  exit(1);
                  }
               }

            if ( (pchar = strchr(&line[i], ' ')) == NULL )
               break;

            i = i + pchar - &line[i];
            }
         }

      else if ( strncmp(line, "view3d", 6) == 0 )
         {
         if ( sscanf(&line[6], "%lf %lf", &p_plot_param_3d->phi, &p_plot_param_3d->theta) != 2 )
            {
            ErrorDialog(error_str[5]);
            exit(1);
            }
         }

      else if ( strncmp(line, "minor_ticks", 11) == 0 )
         {
         size = strlen(&line[11]);
         if ( (string = get_string(&line[11], &i1_str, &i2_str, &size, 0)) != NULL )
            {
            strncpy(&p_plot_param->minor_ticks[0], string, 3);
            if ( strcmp(string, "on") == 0 )
               minor_ticks_flag = 1;
            }
         }

      else if ( strncmp(line, "grid", 4) == 0 )
         {
         size = strlen(&line[4]);
         if ( (string = get_string(&line[4], &i1_str, &i2_str, &size, 0)) != NULL &&
              strcmp(string, "off") == 0 )
            {
            strncpy(&p_plot_param->grid[0], string, 3);
            }
         else if ( sscanf(&line[4], " %c 0x%x", &gridchar1, &gridcolor) == 2 ||
                   sscanf(&line[4], " %c 0X%x", &gridchar1, &gridcolor) == 2 )
            {
            strcpy(&p_plot_param->grid[0], "on2");
            }
         else if ( sscanf(&line[4], " %c %c", &gridchar1, &gridchar2) == 2 &&
                   (pchar = strchr(color_string, gridchar2)) != NULL )
            {
            index = pchar - &color_string[0];   /* get index to color character */
            gridcolor = color_rgba[index];      /* set specified color */
            strcpy(&p_plot_param->grid[0], "on1");
            }
         else
            {
            ErrorDialog(error_str[6]);
            exit(1);
            }
         }

      else if ( strncmp(line, "xlabel", 6) == 0 )
         {
         size = strlen(&line[6]);
         if ( (string = get_string(&line[6], &i1_str, &i2_str, &size, 1)) != NULL )
            {
            xlabel = xmalloc(size + 1);
            strcpy(xlabel, string);
            }
         }

      else if ( strncmp(line, "ylabel", 6) == 0 )
         {
         size = strlen(&line[6]);
         if ( (string = get_string(&line[6], &i1_str, &i2_str, &size, 1)) != NULL )
            {
            ylabel = xmalloc(size + 1);
            strcpy(ylabel, string);
            }
         }

      else if ( strncmp(line, "zlabel", 6) == 0 )
         {
         size = strlen(&line[6]);
         if ( (string = get_string(&line[6], &i1_str, &i2_str, &size, 1)) != NULL )
            {
            zlabel = xmalloc(size + 1);
            strcpy(zlabel, string);
            }
         }

      else if ( strncmp(line, "title", 5) == 0 )
         {
         size = strlen(&line[5]);
         if ( (string = get_string(&line[5], &i1_str, &i2_str, &size, 1)) != NULL )
            {
            title = xmalloc(size + 1);
            strcpy(title, string);
            }
         }

      else if ( strncmp(line, "bin_width", 9) == 0 )
         {
         ibinwidth++;
         if ( ibinwidth <= nplots )
            if ( sscanf(&line[9], "%lf", &bin_widths[ibinwidth-1]) != 1 )
               {
               ErrorDialog(error_str[7]);
               exit(1);
               }
         }

      else if ( strncmp(line, "bin_value", 9) == 0 )
         {
         ibinvalue++;
         if ( ibinvalue <= nplots )
            {
            size = strlen(&line[9]);
            if ( (string = get_string(&line[9], &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strncpy(&bin_values[index_bin_values], string, 8);
               index_bin_values = index_bin_values + 9;
               }
            }
         }

      else if ( strncmp(line, "bin_ref", 7) == 0 )
         {
         ibinref++;
         if ( ibinref <= nplots )
            {
            size = strlen(&line[7]);
            if ( (string = get_string(&line[7], &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strncpy(&bin_refs[index_bin_refs], string, 8);
               index_bin_refs = index_bin_refs + 9;
               }
            }
         }

      else if ( strncmp(line, "ninterp", 7) == 0 )
         {
         ininterp++;
         if ( ininterp <= nplots )
            if ( sscanf(&line[7], "%d", &ninterp[ininterp-1]) != 1 )
               {
               ErrorDialog(error_str[8]);
               exit(1);
               }
         }

      else if ( strncmp(line, "contours", 8) == 0 )
         {
         if ( sscanf(&line[8], "%d", &ncontours) != 1 )
            {
            ErrorDialog(error_str[9]);
            exit(1);
            }
         }

      else if ( strncmp(line, "contour_color", 13) == 0 )
         {
         icontourcolor++;
         if ( icontourcolor <= nplots )
            {
            if ( sscanf(&line[13], " 0x%x", (unsigned int *) &contourcolors[icontourcolor-1]) != 1 &&
                 sscanf(&line[13], " 0X%x", (unsigned int *) &contourcolors[icontourcolor-1]) != 1 )
               {
               if ( sscanf(&line[13], " %c", &character) == 1 &&
                    (pchar = strchr(color_string, character)) != NULL )
                  {
                  index = pchar - &color_string[0];                     /* get index to color character */
                  contourcolors[icontourcolor-1] = color_rgba[index];   /* set specified color */
                  }
               else
                  {
                  ErrorDialog(error_str[10]);
                  exit(1);
                  }
               }
            }
         }

      else if ( strncmp(line, "mesh_color", 10) == 0 )
         {
         imeshcolor++;
         if ( imeshcolor <= nplots )
            {
            if ( sscanf(&line[10], " 0x%x", (unsigned int *) &meshcolors[imeshcolor-1]) != 1 &&
                 sscanf(&line[10], " 0X%x", (unsigned int *) &meshcolors[imeshcolor-1]) != 1 )
               {
               if ( sscanf(&line[10], " %c", &character) == 1 &&
                    (pchar = strchr(color_string, character)) != NULL )
                  {
                  index = pchar - &color_string[0];               /* get index to color character */
                  meshcolors[imeshcolor-1] = color_rgba[index];   /* set specified color */
                  }
               else
                  {
                  ErrorDialog(error_str[11]);
                  exit(1);
                  }
               }
            }
         }

      else if ( strncmp(line, "stems", 5) == 0 )
         {
         istemflag++;
         if ( istemflag <= nplots )
            {
            if ( sscanf(&line[5], "%lf", &stemvalues[istemflag-1]) == 1 )
               {
               strcpy(&stemflags[index_stemflags], "num");
               if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 ||
                    strcmp(p_plot_param->axis_type, "loglog")   == 0 )
                  {
                  if ( stemvalues[istemflag-1] != 0.0 )
                     stemvalues[istemflag-1] = log10(fabs(stemvalues[istemflag-1]));
                  else
                     stemvalues[istemflag-1] = log10(DBL_MIN);
                  }
               index_stemflags = index_stemflags + 4;
               }
            else
               {
               size = strlen(&line[5]);
               if ( (string = get_string(&line[5], &i1_str, &i2_str, &size, 0)) != NULL )
                  {
                  strncpy(&stemflags[index_stemflags], string, 3);
                  index_stemflags = index_stemflags + 4;
                  }
               }
            }
         }

      else if ( strncmp(line, "date_time", 9) == 0 )
         {
         size = strlen(&line[9]);
         if ( (string = get_string(&line[9], &i1_str, &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->date_time_anchor[0], string, 9);
         }

      else if ( strncmp(line, "save_close", 10) == 0 )
         {
         size = strlen(&line[10]);
         if ( (string = get_string(&line[10], &i1_str, &i2_str, &size, 0)) != NULL )
            {
            save_filename = xmalloc(size + 1);
            strcpy(save_filename, string);
            close_flag = 1;
            }
         }

      else if ( strncmp(line, "save", 4) == 0 )
         {
         size = strlen(&line[4]);
         if ( (string = get_string(&line[4], &i1_str, &i2_str, &size, 0)) != NULL )
            {
            save_filename = xmalloc(size + 1);
            strcpy(save_filename, string);
            }
         }

      else if ( strncmp(line, "window_size", 11) == 0 )
         {
         if ( sscanf(&line[11], "%d %d", &p_window_data->width, &p_window_data->height ) != 2 )
            {
            ErrorDialog(error_str[12]);
            exit(1);
            }
         }

      else if ( strncmp(line, "plot_box", 8) == 0 )
         {
         size = strlen(&line[8]);
         if ( (string = get_string(&line[8], &i1_str, &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->plot_box[0], string, 3);
         }

      else if ( strncmp(line, "x_tick_marks", 12) == 0 )
         {
         size = strlen(&line[12]);
         if ( (string = get_string(&line[12], &i1_str, &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->x_tick_marks[0], string, 3);
         }

      else if ( strncmp(line, "y_tick_marks", 12) == 0 )
         {
         size = strlen(&line[12]);
         if ( (string = get_string(&line[12], &i1_str, &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->y_tick_marks[0], string, 3);
         }

      else if ( strncmp(line, "z_tick_marks", 12) == 0 )
         {
         size = strlen(&line[12]);
         if ( (string = get_string(&line[12], &i1_str, &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->z_tick_marks[0], string, 3);
         }

      else if ( strncmp(line, "x_tick_labels", 13) == 0 )
         {
         size = strlen(&line[13]);
         if ( (string = get_string(&line[13], &i1_str, &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->x_tick_labels[0], string, 3);
         }

      else if ( strncmp(line, "y_tick_labels", 13) == 0 )
         {
         size = strlen(&line[13]);
         if ( (string = get_string(&line[13], &i1_str, &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->y_tick_labels[0], string, 3);
         }

      else if ( strncmp(line, "z_tick_labels", 13) == 0 )
         {
         size = strlen(&line[13]);
         if ( (string = get_string(&line[13], &i1_str, &i2_str, &size, 0)) != NULL )
            strncpy(&p_plot_param->z_tick_labels[0], string, 3);
         }

      else if ( strncmp(line, "font_name", 9) == 0 )
         {
         size = strlen(&line[9]);
         if ( (string = get_string(&line[9], &i1_str, &i2_str, &size, 0)) != NULL )
            {
            free(font_name);
            font_name = xmalloc(size + 1);
            strcpy(font_name, string);
            }
         }

      else if ( strncmp(line, "font_size_date_time", 19) == 0 )
         {
         if ( sscanf(&line[19], "%lf", &font_size_date_time) != 1 )
            {
            ErrorDialog(error_str[13]);
            exit(1);
            }
         }

      else if ( strncmp(line, "font_size_legend", 16) == 0 )
         {
         if ( sscanf(&line[16], "%lf", &font_size_legend) != 1 )
            {
            ErrorDialog(error_str[14]);
            exit(1);
            }
         }

      else if ( strncmp(line, "font_size_text", 14) == 0 )
         {
         if ( sscanf(&line[14], "%lf", &font_size_text) != 1 )
            {
            ErrorDialog(error_str[15]);
            exit(1);
            }
         }

      else if ( strncmp(line, "font_size_tick_labels", 21) == 0 )
         {
         if ( sscanf(&line[21], "%lf", &font_size_tick_labels) != 1 )
            {
            ErrorDialog(error_str[16]);
            exit(1);
            }
         }

      else if ( strncmp(line, "font_size_axis_labels", 21) == 0 )
         {
         if ( sscanf(&line[21], "%lf", &font_size_axis_labels) != 1 )
            {
            ErrorDialog(error_str[17]);
            exit(1);
            }
         }

      else if ( strncmp(line, "font_size_title", 15) == 0 )
         {
         if ( sscanf(&line[15], "%lf", &font_size_title) != 1 )
            {
            ErrorDialog(error_str[18]);
            exit(1);
            }
         }

      else if ( strncmp(line, "#####", 5) == 0 )
         break;
      }

   fclose(fptr);


   /* Modify data for logarithmic axes */
   if ( strcmp(p_plot_param->axis_type, "semilogx") == 0 )
      {
      for ( i=1; i<=2; i++ )
         if ( axis_limits[i-1] == 1 )
            {
            if ( p_plot_param->axis_limits[i-1] <= 0.0 )
               {
               ErrorDialog(error_str[19]);
               exit(1);
               }
            p_plot_param->axis_limits[i-1] = log10(fabs(p_plot_param->axis_limits[i-1]));
            }
      }

   else if ( strcmp(p_plot_param->axis_type, "semilogy") == 0 )
      {
      for ( i=3; i<=4; i++ )
         if ( axis_limits[i-1] == 1 )
            {
            if ( p_plot_param->axis_limits[i-1] <= 0.0 )
               {
               ErrorDialog(error_str[20]);
               exit(1);
               }
            p_plot_param->axis_limits[i-1] = log10(fabs(p_plot_param->axis_limits[i-1]));
            }
      }

   else if ( strcmp(p_plot_param->axis_type, "loglog") == 0 )
      {
      for ( i=1; i<=4; i++ )
         if ( axis_limits[i-1] == 1 )
            {
            if ( p_plot_param->axis_limits[i-1] <= 0.0 )
               {
               ErrorDialog(error_str[21]);
               exit(1);
               }
            p_plot_param->axis_limits[i-1] = log10(fabs(p_plot_param->axis_limits[i-1]));
            }
      }


   /* Set font family */
   pango_font_description_set_family(font_date_time,   font_name);
   pango_font_description_set_family(font_legend,      font_name);
   pango_font_description_set_family(font_text,        font_name);
   pango_font_description_set_family(font_tick_labels, font_name);
   pango_font_description_set_family(font_axis_labels, font_name);
   pango_font_description_set_family(font_title,       font_name);


   /* Set font sizes */
   pango_font_description_set_absolute_size(font_date_time,   font_size_date_time*PANGO_SCALE);
   pango_font_description_set_absolute_size(font_legend,      font_size_legend*PANGO_SCALE);
   pango_font_description_set_absolute_size(font_text,        font_size_text*PANGO_SCALE);
   pango_font_description_set_absolute_size(font_tick_labels, font_size_tick_labels*PANGO_SCALE);
   pango_font_description_set_absolute_size(font_axis_labels, font_size_axis_labels*PANGO_SCALE);
   pango_font_description_set_absolute_size(font_title,       font_size_title*PANGO_SCALE);

   return;
   }

