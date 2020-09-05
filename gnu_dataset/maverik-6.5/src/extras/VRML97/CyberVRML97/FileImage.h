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
*	File:	FileImage.h
*
******************************************************************/

#ifndef _FILEIMAGE_H_
#define _FILEIMAGE_H_

#if !R && !G && !B
#define R	0
#define G	1
#define B	2
#endif

typedef unsigned char RGBColor[3];
typedef unsigned char RGBAColor[4];

enum {
FILETYPE_NONE,
FILETYPE_GIF,
FILETYPE_JPEG,
FILETYPE_TARGA,
FILETYPE_PNG
};

class FileImage {

public:

			FileImage();
	virtual ~FileImage();

	bool isOk() {
		if (0 < getWidth() && 0 < getHeight() && getImage())
			return true;
		else
			return false;
	}
	
	virtual int			getFileType() = 0;

	virtual int			getWidth() = 0;
	virtual int			getHeight() = 0;
	virtual RGBColor	*getImage() = 0;

	virtual bool hasTransparencyColor() {
		return false;
	}

	virtual void getTransparencyColor(RGBColor color) {
	};

	RGBColor	*getImage(int newx, int newy);
	RGBAColor	*getRGBAImage();
	RGBAColor	*getRGBAImage(int newx, int newy);
};

#endif
