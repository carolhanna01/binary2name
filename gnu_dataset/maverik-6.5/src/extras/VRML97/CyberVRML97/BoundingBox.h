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
*	Copyright (C) Satoshi Konno 1996-1997
*
*	File:	BoundingBox.h
*
******************************************************************/

#ifndef _BOUNDINGBOX_H_
#define _BOUNDINGBOX_H_

class BoundingBox {

	float	mMaxPosition[3];
	float	mMinPosition[3];
	int		mNPoints;

public:

	BoundingBox();
	BoundingBox(BoundingBox *bbox);
	BoundingBox(float center[3], float size[3]);

	~BoundingBox();

	void	initialize();

	void	addPoint(float point[3]);
	void	addPoint(float x, float y, float z);
	void	addBoundingBox(float center[3], float size[3]);
	void	addBoundingBox(BoundingBox *bbox);

	void	setNPoints(int npoints);
	int		getNPoints();

	void	setMinPosition(float pos[3]);
	void	setMaxPosition(float pos[3]);

	void	setMinPosition(float x, float y, float z);
	void	setMaxPosition(float x, float y, float z);

	void	getMinPosition(float pos[3]);
	void	getMaxPosition(float pos[3]);

	void	set(float center[3], float size[3]);
	void	set(BoundingBox *bbox);

	void	getCenter(float center[3]);
	void	getSize(float size[3]);

};

#endif
