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
   Version : 0.1 
   based on the example from libpng

   License : GPL
   conversion for XImage
 */

#include <stdlib.h>

#include "Xh.h"

#include "read_image.h"

extern int factor;


#define RGB16(p,r,g,b) *((unsigned short *) (p))=(r<<11) | (g<<5) | (b);

#define RGB15(p,r,g,b) *((unsigned short *) (p))=(r<<10) | (g<<5) | (b);

#define RED8   192

#define BLUE8  56

#define GREEN8 7


int Read_PNG(char **img, char *name, int depth, int *height, int *width)
{

	png_structp png_ptr;

	png_infop info_ptr;

	png_uint_32 png_width, png_height;

	FILE *fp;

	int bitdepth, colortype, interlacetype;

	unsigned int row;

	*img = NULL;

	if ((fp = fopen(name, "r")) == NULL)
		fprintf(stderr, "Error opening %s\n", name);

	if ((png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,
					      NULL, NULL, NULL)) == NULL)
		perror("png_create_read_struct");

	if ((info_ptr = png_create_info_struct(png_ptr)) == NULL)
		perror("info_ptr");

	if (setjmp(png_ptr->jmpbuf)) {

		/* Free all of the memory associated with the png_ptr and info_ptr */

		png_destroy_read_struct(&png_ptr, &info_ptr,
					(png_infopp) NULL);

		fclose(fp);

		/* If we get here, we had a problem reading the file */

		return -1;

	}
	png_init_io(png_ptr, fp);

	png_read_info(png_ptr, info_ptr);

	png_get_IHDR(png_ptr, info_ptr, &png_width, &png_height,
		     &bitdepth, &colortype, &interlacetype, NULL, NULL);

	*height = png_height;

	*width = png_width;

/* True RGB */


	{

/* Image reading */

		png_bytep *row_pointers;

		row_pointers =
		    (png_bytep *) malloc(png_height * sizeof(png_bytep));

		for (row = 0; row < png_height; row++) {

			row_pointers[row] = (png_bytep)
			    malloc(png_get_rowbytes(png_ptr, info_ptr));

		}

		png_read_image(png_ptr, row_pointers);

		(*img) =
		    (char *) malloc(factor * png_width * png_height *
				    sizeof(char));

		{

			unsigned int i, j;

			char *p;

			for (i = 0; i < png_height; i++)
				for (j = 0; j < png_width; j++) {

					switch (depth) {

					case 32:

						(*img)[factor *
						       (j +
							i *
							png_width) +
						       2] =
						    row_pointers[i][j * 3];

						(*img)[factor *
						       (j +
							i *
							png_width) +
						       1] =
						    row_pointers[i][j * 3 +
								    1];

						(*img)[factor *
						       (j +
							i *
							png_width)] =
						    row_pointers[i][j * 3 +
								    2];

						(*img)[factor *
						       (j +
							i *
							png_width) +
						       3] = 0;

						break;

						/*

						   }

						   else

						 */

					case 24:

						(*img)[factor *
						       (j +
							i *
							png_width) +
						       2] =
						    row_pointers[i][j * 3];

						(*img)[factor *
						       (j +
							i *
							png_width) +
						       1] =
						    row_pointers[i][j * 3 +
								    1];

						(*img)[factor *
						       (j +
							i *
							png_width)] =
						    row_pointers[i][j * 3 +
								    2];

						break;

						/*

						   }

						   else

						 */

					case 16:

						p =
						    &(*img)[factor *
							    (j +
							     i *
							     png_width)];

						RGB16(p,
						      row_pointers[i][j *
								    3] >>
						      3,
						      row_pointers[i][j *
								      3 +
								    1] >>
						      2,
						      row_pointers[i][j *
								      3 +
								    2] >>
						      3);

						break;

					case 15:

						p =
						    &(*img)[factor *
							    (j +
							     i *
							     png_width)];

						RGB15(p,
						      row_pointers[i][j *
								    3] >>
						      3,
						      row_pointers[i][j *
								      3 +
								    1] >>
						      3,
						      row_pointers[i][j *
								      3 +
								    2] >>
						      3);

						break;

					case 8:

						(*img)[j +
						       i * png_width] =
						    (char) (
							    (row_pointers
							     [i][j *
								 3] &
							     RED8));

						(*img)[j +
						       i *
						       png_width] +=
						    (char) (
							    (row_pointers
							     [i][j * 3 +
								 1] >> 5)
								   << 3);

						(*img)[j +
						       i *
						       png_width] +=
						    ((row_pointers
						      [i][j * 3 +
							  2]) >> 5);

						break;

					}

				}

		}

		for (row = 0; row < png_height; row++) {

			free(row_pointers[row]);

		}

		free(row_pointers);

	}

	png_read_end(png_ptr, info_ptr);

	png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp) NULL);

	fclose(fp);

	return 0;

}
