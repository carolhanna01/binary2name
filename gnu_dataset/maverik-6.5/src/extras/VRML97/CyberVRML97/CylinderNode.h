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
*	File:	CylinderNode.h
*
******************************************************************/

#ifndef _CYLINDER_H_
#define _CYLINDER_H_

#include "GeometryNode.h"

class CylinderNode : public GeometryNode {

public:

	CylinderNode() {

		setHeaderFlag(false);
		setType(cylinderNodeString);

		// radius field
		SFFloat *radius = new SFFloat(1.0f);
		addExposedField(radiusFieldString, radius);

		// height field
		SFFloat *height = new SFFloat(2.0f);
		addExposedField(heightFieldString, height);

		// top field
		SFBool *top = new SFBool(true);
		addExposedField(topFieldString, top);

		// side field
		SFBool *side = new SFBool(true);
		addExposedField(sideFieldString, side);

		// bottom field
		SFBool *bottom = new SFBool(true);
		addExposedField(bottomFieldString, bottom);
	}

	~CylinderNode() {
	}

	////////////////////////////////////////////////
	//	radius
	////////////////////////////////////////////////

	void setRadius(float value) {
		SFFloat *radius = (SFFloat *)getExposedField(radiusFieldString);
		radius->setValue(value);
	}
	float getRadius() {
		SFFloat *radius = (SFFloat *)getExposedField(radiusFieldString);
		return radius->getValue();
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
	//	top
	////////////////////////////////////////////////

	void setTop(bool value) {
		SFBool *top = (SFBool *)getExposedField(topFieldString);
		top->setValue(value);
	}
	void setTop(int value) {
		setTop(value ? true : false);
	}
	bool getTop() {
		SFBool *top = (SFBool *)getExposedField(topFieldString);
		return top->getValue();
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

	void setBottom(bool  value) {
		SFBool *bottom = (SFBool *)getExposedField(bottomFieldString);
		bottom->setValue(value);
	}
	void setBottom(int value) {
		setBottom(value ? true : false);
	}
	bool  getBottom() {
		SFBool *bottom = (SFBool *)getExposedField(bottomFieldString);
		return bottom->getValue();
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	CylinderNode *next() {
		return (CylinderNode *)Node::next(getType());
	}

	CylinderNode *nextTraversal() {
		return (CylinderNode *)Node::nextTraversalByType(getType());
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
		setBoundingBoxSize(getRadius(), getHeight()/2.0f, getRadius());
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
		SFBool *top = (SFBool *)getExposedField(topFieldString);
		SFBool *side = (SFBool *)getExposedField(sideFieldString);
		SFBool *bottom = (SFBool *)getExposedField(bottomFieldString);

		printStream << indentString << "\t" << "radius " << getRadius() << endl;
		printStream << indentString << "\t" << "height " << getHeight() << endl;
		printStream << indentString << "\t" << "side " << side << endl;
		printStream << indentString << "\t" << "top " << top << endl;
		printStream << indentString << "\t" << "bottom " << bottom << endl;
	}
};

#endif

