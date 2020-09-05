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
*	File:	LodNode.h
*
******************************************************************/

#ifndef _LOD_H_
#define _LOD_H_

#include "vrmlfields.h"
#include "Node.h"
#include "JVector.h"

class LodNode : public Node {

public:

	LodNode() {
		setHeaderFlag(false);
		setType(lodNodeString);

		// center field
		SFVec3f *center = new SFVec3f(0.0f, 0.0f, 0.0f);
		addField(centerFieldString, center);

		// range field
		MFFloat *range = new MFFloat();
		addField(rangeFieldString, range);
	}

	~LodNode() {
	}
	
	////////////////////////////////////////////////
	//	center
	////////////////////////////////////////////////

	void setCenter(float value[]) {
		SFVec3f *center = (SFVec3f *)getField(centerFieldString);
		center->setValue(value);
	}

	void setCenter(float x, float y, float z) {
		SFVec3f *center = (SFVec3f *)getField(centerFieldString);
		center->setValue(x, y, z);
	}

	void getCenter(float value[]) {
		SFVec3f *center = (SFVec3f *)getField(centerFieldString);
		center->getValue(value);
	}

	////////////////////////////////////////////////
	//	range 
	////////////////////////////////////////////////

	void addRange(float value) {
		MFFloat *range = (MFFloat *)getField(rangeFieldString);
		range->addValue(value);
	}

	int getNRanges() {
		MFFloat *range = (MFFloat *)getField(rangeFieldString);
		return range->getSize();
	}

	float getRange(int index) {
		MFFloat *range = (MFFloat *)getField(rangeFieldString);
		return range->get1Value(index);
	}


	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	LodNode *next() {
		return (LodNode *)Node::next(getType());
	}

	LodNode *nextTraversal() {
		return (LodNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		if (node->isCommonNode() || node->isBindableNode() ||node->isInterpolatorNode() || node->isSensorNode() || node->isGroupingNode() || node->isSpecialGroupNode())
			return true;
		else
			return false;
	}

	void initialize();

	void uninitialize();

	void update();

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFVec3f *center = (SFVec3f *)getField(centerFieldString);
		printStream << indentString << "\t" << "center " << center << endl;

		if (0 < getNRanges()) {
			MFFloat *range = (MFFloat *)getField(rangeFieldString);
			printStream << indentString << "\t" << "range [" << endl;
			range->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}
};

void UpdateLod(LodNode *lod);
void InitializeLod(LodNode *lod);
void UninitializeLod(LodNode *lod);

#endif

