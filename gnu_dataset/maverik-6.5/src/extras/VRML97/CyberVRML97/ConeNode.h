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
*	File:	ConeNode.h
*
******************************************************************/

#ifndef _CONE_H_
#define _CONE_H_

#include "GeometryNode.h"

class ConeNode : public GeometryNode {

public:

	ConeNode() {

		setHeaderFlag(false);
		setType(coneNodeString);

		// bottomRadius field
		SFFloat *bottomRadius = new SFFloat(1.0f);
		addExposedField(bottomRadiusFieldString, bottomRadius);

		// height field
		SFFloat *height = new SFFloat(2.0f);
		addExposedField(heightFieldString, height);

		// side field
		SFBool *side = new SFBool(true);
		addExposedField(sideFieldString, side);

		// bottom field
		SFBool *bottom = new SFBool(true);
		addExposedField(bottomFieldString, bottom);
	}

	~ConeNode() {
	}

	////////////////////////////////////////////////
	//	bottomRadius
	////////////////////////////////////////////////

	void setBottomRadius(float value) {
		SFFloat *bottomRadius = (SFFloat *)getExposedField(bottomRadiusFieldString);
		bottomRadius->setValue(value);
	}
	float getBottomRadius() {
		SFFloat *bottomRadius = (SFFloat *)getExposedField(bottomRadiusFieldString);
		return bottomRadius->getValue();
	}

	////////////////////////////////////////////////
	//	height
	////////////////////////////////////////////////

	void setHeight(float value) {
		SFFloat *height = (SFFloat *)getExposedField(heightFieldString);
		height->setValue(value);
	}
	float getHeight() {
		SFFloat *height = (SFFloat *)getExposedField(heightFieldString);
		return height->getValue();
	}

	////////////////////////////////////////////////
	//	side
	////////////////////////////////////////////////

	void setSide(bool value) {
		SFBool *side = (SFBool *)getExposedField(sideFieldString);
		side->setValue(value);
	}
	void setSide(int value) {
		setSide(value ? true : false);
	}
	bool getSide() {
		SFBool *side = (SFBool *)getExposedField(sideFieldString);
		return side->getValue();
	}

	////////////////////////////////////////////////
	//	bottom
	////////////////////////////////////////////////

	void setBottom(bool value) {
		SFBool *bottom = (SFBool *)getExposedField(bottomFieldString);
		bottom->setValue(value);
	}
	void setBottom(int value) {
		setBottom(value ? true : false);
	}
	bool getBottom() {
		SFBool *bottom = (SFBool *)getExposedField(bottomFieldString);
		return bottom->getValue();
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	ConeNode *next() {
		return (ConeNode *)Node::next(getType());
	}

	ConeNode *nextTraversal() {
		return (ConeNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		return false;
	}

	void initialize() {
		recomputeBoundingBox();
#ifdef SUPPORT_OPENGL
		recomputeDisplayList();
#endif
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	BoundingBox
	////////////////////////////////////////////////

	void recomputeBoundingBox() {
		setBoundingBoxCenter(0.0f, 0.0f, 0.0f);
		setBoundingBoxSize(getBottomRadius(), getHeight()/2.0f, getBottomRadius());
	}

	////////////////////////////////////////////////
	//	recomputeDisplayList
	////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL
	void recomputeDisplayList();
#endif

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *side = (SFBool *)getExposedField(sideFieldString);
		SFBool *bottom = (SFBool *)getExposedField(bottomFieldString);

		printStream << indentString << "\t" << "bottomRadius " << getBottomRadius() << endl;
		printStream << indentString << "\t" << "height " << getHeight() << endl;
		printStream << indentString << "\t" << "side " << side << endl;
		printStream << indentString << "\t" << "bottom " << bottom << endl;
	}
};

#endif

