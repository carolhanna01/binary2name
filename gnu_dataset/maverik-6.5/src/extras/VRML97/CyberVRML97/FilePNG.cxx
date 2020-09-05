/*
    GNU Maverik - a system for managing display and interaction in 
              Virtual Environment applications.
   Copyright (C) 2008  Advanced Interfaces Group

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   The authors can be contacted via:
   www   - http://aig.cs.man.ac.uk
   email - maverik@aig.cs.man.ac.uk
   mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
        University of Manchester, Manchester, M13 9PL, UK
*/

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1997
*
*	File:	FilePNG.cpp
*
******************************************************************/

#ifdef SUPPORT_PNG

#include <stdio.h>
#include "png.h"
#include "FilePNG.h"

static RGBColor	gDefaultTransparencyColor = {1, 1, 1};

static bool EqualColor(
RGBColor color1,
RGBColor color2)
{
	if (color1[0] == color2[0] && color1[1] == color2[1] && color1[2] == color2[2])
		return true;
	return false;
}

FilePNG::FilePNG(char *filename)
{	
	mImgBuffer = NULL;
	mWidth = mHeight = 0;
	mHasTransparencyColor = false;
	
	load(filename);
}

bool FilePNG::load(char *filename)
{	
	FILE	*fp;
	int		n, i, j;

	if (!(fp = fopen(filename, "rb")))
		return false;

	png_struct *pngRead = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	
	if (pngRead == NULL)
		return false;

	png_info *pngInfo = png_create_info_struct(pngRead);

	if (pngInfo == NULL) {
		fclose(fp);
		png_destroy_read_struct(&pngRead, NULL, NULL);
		return false;
	}

	png_init_io(pngRead, fp);

	png_read_info(pngRead, pngInfo);

	png_uint_32	width,height;
	int			bitDepth, colorType, interlaceType;

	png_get_IHDR(pngRead, pngInfo, &width, &height, &bitDepth, &colorType, &interlaceType, NULL, NULL);

	if (!(colorType & PNG_COLOR_MASK_COLOR) || bitDepth != 8) {
		fclose(fp);
		return false;
	}
/*
	png_color	*pngPalette	= NULL;
	int			numPalette	= 0;

	if (colorType & PNG_COLOR_MASK_PALETTE) 
		png_get_PLTE(pngRead, pngInfo, &pngPalette, & numPalette);
*/
    if (colorType == PNG_COLOR_TYPE_PALETTE && bitDepth <= 8) 
		png_set_expand(pngRead);

    if (png_get_valid(pngRead, pngInfo, PNG_INFO_tRNS))
		png_set_expand(pngRead);

	png_get_IHDR(pngRead, pngInfo, &width, &height, &bitDepth, &colorType, &interlaceType, NULL, NULL);

	int	pixelByte = 0;
	if (colorType & PNG_COLOR_TYPE_RGB_ALPHA)
		pixelByte = bitDepth * 4;
	else 
		pixelByte = bitDepth * 3;


	png_bytep	*imgData;

	imgData = new png_bytep[height];
	for (n=0; n<(int)height; n++)
		imgData[n] = new unsigned char[pixelByte * width];

	png_read_image(pngRead, imgData); 

	mImgBuffer = new RGBColor[width*height];
	
	int offset;
	for (i=0; i<(int)width; i++) {
		for (j=0; j<(int)height; j++) {
			unsigned char *color = mImgBuffer[(width * j) + i];
			if (colorType & PNG_COLOR_TYPE_RGB_ALPHA)
				offset = i * 4 * (bitDepth/8);
			else
				offset = i * 3 * (bitDepth/8);

			color[0] = imgData[j][offset + 0];
			color[1] = imgData[j][offset + 1];
			color[2] = imgData[j][offset + 2];

			if ((colorType & PNG_COLOR_TYPE_RGB_ALPHA) && imgData[j][offset + 3] == 0) {
				color[0] = gDefaultTransparencyColor[0];
				color[1] = gDefaultTransparencyColor[1];
				color[2] = gDefaultTransparencyColor[2];
			}
		}
	}

	mWidth = width;
	mHeight = height;

	if (colorType & PNG_COLOR_TYPE_RGB_ALPHA) {
		mHasTransparencyColor = true;
		setTransparencyColor(gDefaultTransparencyColor);
	}

	for (n=0; n<(int)height; n++)
		delete []imgData[n];
	delete []imgData;

	png_read_end(pngRead, pngInfo);
	png_destroy_read_struct(&pngRead, NULL, NULL);

	fclose(fp);

	return true;
}

FilePNG::~FilePNG()
{
	if (mImgBuffer)
		delete []mImgBuffer;
}

#endif //JMC
