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
*	File:	Node.cpp
*
******************************************************************/

#include "TransformNode.h"

////////////////////////////////////////////////
//	Matrix
////////////////////////////////////////////////

void TransformNode::getSFMatrix(SFMatrix *mOut)
{
	float	center[3];
	float	rotation[4];
	float	scale[3];
	float	scaleOri[4];
	float	trans[3];
	SFMatrix	mSRI;
	SFMatrix	mSR;
	SFMatrix	mCI;
	SFMatrix	mC;
	SFMatrix	mT;
	SFMatrix	mR;
	SFMatrix	mS;

	getTranslation(trans); 
	mT.setTranslation(trans);

	getCenter(center); 
	mC.setTranslation(center);

	getRotation(rotation);
	mR.setRotation(rotation);

	getScaleOrientation(scaleOri); 
	mSR.setRotation(scaleOri);

	getScale(scale);
	mS.setScaling(scale);

	getScaleOrientation(scaleOri); 
	scaleOri[3] = -scaleOri[3]; 
	mSRI.setRotation(scaleOri);

	getCenter(center); 
	center[0] = -center[0]; 
	center[1] = -center[1]; 
	center[2] = -center[2]; 
	mCI.setTranslation(center);

	mOut->init();
	mOut->add(&mT);
	mOut->add(&mC);
	mOut->add(&mR);
	mOut->add(&mSR);
	mOut->add(&mS);
	mOut->add(&mSRI);
	mOut->add(&mCI);
}
