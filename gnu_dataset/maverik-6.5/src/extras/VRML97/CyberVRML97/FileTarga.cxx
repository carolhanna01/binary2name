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
*	File:	FileTarga.h
*
******************************************************************/
#include	<stdio.h>
#include	<stdlib.h>
#include	"FileTarga.h"

void FileTarga::initialize()
{
	idLength	= 0;
	coMapType	= 0;
	imgType		= 2;
	index		= 0;	
	length		= 0;	
	coSize		= 0;	
	xOrg		= 0;	
	yOrg		= 0;	
	width		= 0;	
	height		= 0;	
	pixelSize	= 0;
	attBits		= 0;
	imageBuffer = NULL;
}

FileTarga::FileTarga(char *filename)
{
	load(filename);
}

FileTarga::FileTarga(int cx, int cy, RGBColor *color)
{
	initialize();

	width		= cx;	
	height		= cy;	
	pixelSize	= 24;
	imageBuffer = color;
}

FileTarga::~FileTarga()
{
	if (imageBuffer)
		free(imageBuffer);
}

bool FileTarga::load(char *filename)
{
	initialize();

	FILE			*fp;
	
	if (!(fp = fopen(filename, "rb")))
		return false;
			
	fread(&idLength, sizeof(char), sizeof(char), fp);
	fread(&coMapType, sizeof(char), sizeof(char), fp);
	fread(&imgType, sizeof(char), sizeof(char), fp);
	fread(&index, sizeof(char), sizeof(short), fp);
	fread(&length, sizeof(char), sizeof(short), fp);
	fread(&coSize, sizeof(char), sizeof(char), fp);
	fread(&xOrg, sizeof(char), sizeof(short), fp);
	fread(&yOrg, sizeof(char), sizeof(short), fp);
	fread(&width, sizeof(char), sizeof(short), fp);
	fread(&height, sizeof(char), sizeof(short), fp);
	fread(&pixelSize, sizeof(char), sizeof(char), fp);
	fread(&attBits, sizeof(char), sizeof(char), fp);

	if (pixelSize != 24)
		return false;
	
	if (0 < idLength) {
		fseek(fp, idLength, SEEK_CUR);
		idLength = 0;
	}
	
	imageBuffer = (RGBColor *)malloc(sizeof(RGBColor)*(height*width));
	for (int y=0; y<height; y++) {
		for (int x=0; x<width; x++)
			fread(&imageBuffer[x+y*width], sizeof(char), sizeof(RGBColor), fp);
	}
	
	fclose(fp);

	return true;
}

bool FileTarga::save(char *filename)
{
	FILE			*fp;
	
	if (!imageBuffer)
		return false;

	if (!(fp = fopen(filename, "wb")))
		return false;
			
	fwrite(&idLength, sizeof(char), sizeof(char), fp);
	fwrite(&coMapType, sizeof(char), sizeof(char), fp);
	fwrite(&imgType, sizeof(char), sizeof(char), fp);
	fwrite(&index, sizeof(char), sizeof(short), fp);
	fwrite(&length, sizeof(char), sizeof(short), fp);
	fwrite(&coSize, sizeof(char), sizeof(char), fp);
	fwrite(&xOrg, sizeof(char), sizeof(short), fp);
	fwrite(&yOrg, sizeof(char), sizeof(short), fp);
	fwrite(&width, sizeof(char), sizeof(short), fp);
	fwrite(&height, sizeof(char), sizeof(short), fp);
	fwrite(&pixelSize, sizeof(char), sizeof(char), fp);
	fwrite(&attBits, sizeof(char), sizeof(char), fp);

	for (int y=0; y<height; y++) {
		for (int x=0; x<width; x++) {
			fwrite(&imageBuffer[x+y*width][0], sizeof(char), sizeof(char), fp);
			fwrite(&imageBuffer[x+y*width][1], sizeof(char), sizeof(char), fp);
			fwrite(&imageBuffer[x+y*width][2], sizeof(char), sizeof(char), fp);
		}
	}
	
	fclose(fp);

	return true;
}
