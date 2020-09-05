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
*	File:	FilePNG.h
*
******************************************************************/

#ifndef _FILEPNG_H_
#define _FILEPNG_H_

#include "FileImage.h"

#ifdef SUPPORT_PNG

class FilePNG : public FileImage {
	int			mWidth;
	int			mHeight;
	RGBColor	*mImgBuffer;
	bool		mHasTransparencyColor;
	RGBColor	mTransparencyColor;
public:	

	FilePNG(char *filename);
	~FilePNG();
	
	bool load(char *filename);

	int getFileType() {
		return FILETYPE_PNG;
	}

	int getWidth() {
		return mWidth;
	}

	int getHeight() {
		return mHeight;
	}
	
	RGBColor *getImage() {
		return mImgBuffer;
	}

	bool hasTransparencyColor() {
		return mHasTransparencyColor;
	}

	void setTransparencyColor(RGBColor color) {
		mTransparencyColor[0] = color[0];
		mTransparencyColor[1] = color[1];
		mTransparencyColor[2] = color[2];
	}

	void getTransparencyColor(RGBColor color) {
		color[0] = mTransparencyColor[0];
		color[1] = mTransparencyColor[1];
		color[2] = mTransparencyColor[2];
	}

};

#endif

#endif
