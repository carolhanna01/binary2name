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
*	Copyright (C) Satoshi Konno 1996-1997
*
*	File:	BoundingBox.cpp
*
******************************************************************/

#include <float.h>
#include <math.h>
#include "BoundingBox.h"

BoundingBox::BoundingBox()
{
	initialize();
}

BoundingBox::BoundingBox(BoundingBox *bbox)
{
	set(bbox);
}

BoundingBox::BoundingBox(float center[3], float size[3])
{
	set(center, size);
}

BoundingBox::~BoundingBox()
{
}

void BoundingBox::initialize()
{
	setMinPosition(FLT_MAX, FLT_MAX, FLT_MAX);
	setMaxPosition(FLT_MIN, FLT_MIN, FLT_MIN);
	setNPoints(0);
}

void BoundingBox::addPoint(float point[3])
{
	for (int n=0; n<3; n++) {
		if (point[n] < mMinPosition[n])
			mMinPosition[n] = point[n];
		if (mMaxPosition[n] < point[n]) 
			mMaxPosition[n] = point[n];
	}
	setNPoints(getNPoints()+1);
}

void BoundingBox::addPoint(float x, float y, float z)
{
	float point[] = {x, y, z};
	addPoint(point);
}

void BoundingBox::addBoundingBox(float center[3], float size[3])
{
	float	point[3];
	for (int n=0; n<8; n++) {
		point[0] = (n < 4)			? center[0] - size[0] : center[0] + size[0];
		point[1] = (n % 2)			? center[1] - size[1] : center[1] + size[1];
		point[2] = ((n % 4) < 2)	? center[2] - size[2] : center[2] + size[2];
		addPoint(point);
	}
}

void BoundingBox::addBoundingBox(BoundingBox *bbox)
{
	float	center[3];
	float	size[3];
	bbox->getCenter(center);
	bbox->getSize(size);
	addBoundingBox(center, size);
}

void BoundingBox::setNPoints(int npoints)
{
	mNPoints = npoints;
}

int BoundingBox::getNPoints()
{
	return mNPoints;
}

void BoundingBox::setMinPosition(float x, float y, float z)
{
	mMinPosition[0] = x;
	mMinPosition[1] = y;
	mMinPosition[2] = z;
}

void BoundingBox::setMaxPosition(float x, float y, float z)
{
	mMaxPosition[0] = x;
	mMaxPosition[1] = y;
	mMaxPosition[2] = z;
}

void BoundingBox::setMinPosition(float pos[3])
{
	setMinPosition(pos[0], pos[1], pos[2]);
}

void BoundingBox::setMaxPosition(float pos[3])
{
	setMaxPosition(pos[0], pos[1], pos[2]);
}

void BoundingBox::getMinPosition(float pos[3])
{
	pos[0] = mMinPosition[0];
	pos[1] = mMinPosition[1];
	pos[2] = mMinPosition[2];
}

void BoundingBox::getMaxPosition(float pos[3])
{
	pos[0] = mMaxPosition[0];
	pos[1] = mMaxPosition[1];
	pos[2] = mMaxPosition[2];
}

void BoundingBox::set(float center[3], float size[3])
{
	for (int n=0; n<3; n++) {
		mMinPosition[n] = center[n] - size[n];
		mMaxPosition[n] = center[n] + size[n];;
	}
	setNPoints(1);
}

void BoundingBox::set(BoundingBox *bbox)
{
	float	center[3];
	float	size[3];
	bbox->getCenter(center);
	bbox->getSize(size);
	set(center, size);
}

void BoundingBox::getCenter(float center[3])
{
	if (0 < getNPoints()) {
		center[0] = (mMaxPosition[0] + mMinPosition[0]) / 2.0f;
		center[1] = (mMaxPosition[1] + mMinPosition[1]) / 2.0f;
		center[2] = (mMaxPosition[2] + mMinPosition[2]) / 2.0f;
	}
	else {
		center[0] = 0.0f;
		center[1] = 0.0f;
		center[2] = 0.0f;
	}
}

void BoundingBox::getSize(float size[3])
{
	if (0 < getNPoints()) {
		size[0] = (float)fabs(mMaxPosition[0] - mMinPosition[0]) / 2.0f;
		size[1] = (float)fabs(mMaxPosition[1] - mMinPosition[1]) / 2.0f;
		size[2] = (float)fabs(mMaxPosition[2] - mMinPosition[2]) / 2.0f;
	}
	else {
		size[0] = -1.0f;
		size[1] = -1.0f;
		size[2] = -1.0f;
	}
}
