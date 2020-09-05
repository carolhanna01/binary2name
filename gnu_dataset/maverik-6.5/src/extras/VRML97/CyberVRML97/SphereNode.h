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
*	File:	SphereNode.h
*
******************************************************************/

#ifndef _SPHERE_H_
#define _SPHERE_H_

#include "GeometryNode.h"

class SphereNode : public GeometryNode {
	
public:

	SphereNode() {
		setHeaderFlag(false);
		setType(sphereNodeString);

		///////////////////////////
		// Exposed Field 
		///////////////////////////

		// radius exposed field
		SFFloat *radius = new SFFloat(1.0f);
		addExposedField(radiusFieldString, radius);
	}

	~SphereNode() {
	}

	////////////////////////////////////////////////
	//	Radius
	////////////////////////////////////////////////
	
	void setRadius(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(radiusFieldString);
		sffloat->setValue(value);
	}
	float getRadius() {
		SFFloat *sffloat = (SFFloat *)getExposedField(radiusFieldString);
		return sffloat->getValue();
	} 

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	SphereNode *next() {
		return (SphereNode *)Node::next(getType());
	}

	SphereNode *nextTraversal() {
		return (SphereNode *)Node::nextTraversalByType(getType());
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
		setBoundingBoxSize(getRadius(), getRadius(), getRadius());
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
		printStream << indentString << "\t" << "radius " << getRadius() << endl;
	}
};

#endif

