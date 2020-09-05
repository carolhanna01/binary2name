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
*	File:	BoxNode.h
*
******************************************************************/

#ifndef _BOX_H_
#define _BOX_H_

#include "vrmlfields.h"
#include "GeometryNode.h"

class BoxNode : public GeometryNode {
	
public:

	BoxNode() {
		setHeaderFlag(false);
		setType(boxNodeString);

		// size exposed field
		SFVec3f *size = new SFVec3f(2.0f, 2.0f, 2.0f);
		size->setName(sizeFieldString);
		addExposedField(size);
	}

	~BoxNode() {
	}

	////////////////////////////////////////////////
	//	size
	////////////////////////////////////////////////

	void setSize(float value[]) {
		SFVec3f *size = (SFVec3f *)getExposedField(sizeFieldString);
		size->setValue(value);
	}
	void setSize(float x, float y, float z) {
		SFVec3f *size = (SFVec3f *)getExposedField(sizeFieldString);
		size->setValue(x, y, z);
	}
	void getSize(float value[]) {
		SFVec3f *size = (SFVec3f *)getExposedField(sizeFieldString);
		size->getValue(value);
	}

	float getX() {
		return ((SFVec3f *)getExposedField(sizeFieldString))->getX();
	}
	float getY() {
		return ((SFVec3f *)getExposedField(sizeFieldString))->getY();
	}
	float getZ() {
		return ((SFVec3f *)getExposedField(sizeFieldString))->getZ();
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	BoxNode *next() {
		return (BoxNode *)Node::next(getType());
	}

	BoxNode *nextTraversal() {
		return (BoxNode *)Node::nextTraversalByType(getType());
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

	void recomputeBoundingBox();

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
		SFVec3f *size = (SFVec3f *)getExposedField(sizeFieldString);
		printStream << indentString << "\t" << "size " << size << endl;
	}
};

#endif
