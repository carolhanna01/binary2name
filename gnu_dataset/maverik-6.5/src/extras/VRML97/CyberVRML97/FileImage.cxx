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
*	File:	FileImage.cpp
*
******************************************************************/

#include <stdio.h>
#include <memory.h>
#include "FileImage.h"

FileImage::FileImage()
{
}

FileImage::~FileImage()
{
}

RGBColor *FileImage::getImage(int newx, int newy)
{
	float xscale = (float)getWidth() / (float)newx;
	float yscale = (float)getHeight() / (float)newy;

	RGBColor *color = getImage();
	if (color == NULL)
		return NULL;

	RGBColor *newColor = new RGBColor[newx*newy];

	int width = getWidth();

	for (int y=0; y<newy; y++) {
		for (int x=0; x<newx; x++) {
			int xIndex = (int)((float)x*xscale);
			int yIndex = (int)((float)y*yscale);
			memcpy(newColor[x + y*newx], color[xIndex + yIndex*width], sizeof(RGBColor));
		}
	}

	return newColor;
}

RGBAColor *FileImage::getRGBAImage()
{
	int width	= getWidth();
	int height	= getHeight();

	RGBColor *color = getImage(width, height);
	if (color == NULL)
		return NULL;

	RGBAColor *newColor = new RGBAColor[width*height];

	RGBColor bgColor;
	if (hasTransparencyColor())
		getTransparencyColor(bgColor);

	for (int y=0; y<height; y++) {
		for (int x=0; x<width; x++) {
			int index = x + y*width;
			memcpy(newColor[index], color[index], sizeof(RGBColor));
			if (hasTransparencyColor() && bgColor[0] == color[index][0] && bgColor[1] == color[index][1] && bgColor[2] == color[index][2])
				newColor[index][3] = 0x00;
			else
				newColor[index][3] = 0xff;
		}
	}

	return newColor;
}

RGBAColor *FileImage::getRGBAImage(int newx, int newy)
{
	RGBColor	*color = getImage(newx, newy);
	if (color == NULL)
		return NULL;

	RGBAColor	*newColor = new RGBAColor[newx*newy];

	RGBColor	bgColor;
	if (hasTransparencyColor())
		getTransparencyColor(bgColor);

	for (int y=0; y<newy; y++) {
		for (int x=0; x<newx; x++) {
			int index = x + y*newx;
			memcpy(newColor[index], color[index], sizeof(RGBColor));
			if (hasTransparencyColor() && bgColor[0] == color[index][0] && bgColor[1] == color[index][1] && bgColor[2] == color[index][2])
				newColor[index][3] = 0x00;
			else
				newColor[index][3] = 0xff;
		}
	}

	delete []color;

	return newColor;
}
