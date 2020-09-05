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


#include "mavlib_cvcomp.h"

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1997
*
*	File:	FileTarga.h
*
******************************************************************/

#ifndef _FILETARGA_H_
#define _FILETARGA_H_

#include "FileImage.h"

typedef struct {
	unsigned char		IDLength;
	unsigned char		CoMapType;
	unsigned char		ImgType;
	unsigned short int	Index;	
	unsigned short int	Length;	
	unsigned char		CoSize;	
	unsigned short int	XOrg;	
	unsigned short int	YOrg;	
	unsigned short int	Width;	
	unsigned short int	Height;	
	unsigned char		PixelSize;
	unsigned char		AttBits;
} TargaHeadeInfor;

class FileTarga : public FileImage {
	unsigned char		idLength;
	unsigned char		coMapType;
	unsigned char		imgType;
	unsigned short int	index;	
	unsigned short int	length;	
	unsigned char		coSize;	
	unsigned short int	xOrg;	
	unsigned short int	yOrg;	
	unsigned short int	width;	
	unsigned short int	height;	
	unsigned char		pixelSize;
	unsigned char		attBits;
	RGBColor			*imageBuffer;
public:
	FileTarga(char *filename);
	FileTarga(int cx, int cy, RGBColor *color);
	~FileTarga();

	void		initialize();
	bool		load(char *filename);
	bool		save(char *filename);

	int			getFileType() { return FILETYPE_TARGA; }

	int			getWidth()	{ return width; }
	int			getHeight()	{ return height; }
	RGBColor	*getImage()	{ return imageBuffer; }
};

#endif
