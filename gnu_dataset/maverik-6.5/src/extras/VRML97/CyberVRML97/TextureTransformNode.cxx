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

#include "TextureTransformNode.h"


////////////////////////////////////////////////
//	Node::getTransformMatrix(SFMatrix *matrix)
////////////////////////////////////////////////

void TextureTransformNode::getSFMatrix(SFMatrix *mOut)
{
	float		center[3];
	float		rotation[4];
	float		scale[3];
	float		translation[3];

	SFMatrix	mSRI;
	SFMatrix	mSR;
	SFMatrix	mCI;
	SFMatrix	mC;
	SFMatrix	mT;
	SFMatrix	mR;
	SFMatrix	mS;

	getTranslation(translation); 
	translation[2] = 0.0f;
	mT.setTranslation(translation);

	getCenter(center); 
	center[2] = 0.0f;
	mC.setTranslation(center);

	rotation[0] = 0.0f;
	rotation[0] = 0.0f;
	rotation[0] = 1.0f;
	rotation[0] = getRotation();
	mR.setRotation(rotation);

	getScale(scale);
	center[2] = 1.0f;
	mS.setScaling(scale);

	getCenter(center); 
	center[0] = -center[0]; 
	center[1] = -center[1]; 
	center[2] = 0.0f; 
	mCI.setTranslation(center);

	mOut->init();
	mOut->add(&mCI);
	mOut->add(&mS);
	mOut->add(&mR);
	mOut->add(&mC);
	mOut->add(&mT);
}
