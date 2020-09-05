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
*	File:	BillboardNode.h
*
******************************************************************/

#ifndef _BILLBOARD_H_
#define _BILLBOARD_H_

#include "vrmlfields.h"
#include "Node.h"
	
class BillboardNode : public GroupingNode {

public:

	BillboardNode() {
		setHeaderFlag(false);
		setType(billboardNodeString);

		// axisOfRotation exposed field
		SFVec3f *axisOfRotation = new SFVec3f(0.0f, 1.0f, 0.0f);
		addExposedField(axisOfRotationFieldString, axisOfRotation);
	}

	~BillboardNode() {
	}

	////////////////////////////////////////////////
	//	axisOfRotation
	////////////////////////////////////////////////

	void setAxisOfRotation(float value[]) {
		SFVec3f *axisOfRotation = (SFVec3f *)getExposedField(axisOfRotationFieldString);
		axisOfRotation->setValue(value);
	}

	void setAxisOfRotation(float x, float y, float z) {
		SFVec3f *axisOfRotation = (SFVec3f *)getExposedField(axisOfRotationFieldString);
		axisOfRotation->setValue(x, y, z);
	}

	void getAxisOfRotation(float value[]) {
		SFVec3f *axisOfRotation = (SFVec3f *)getExposedField(axisOfRotationFieldString);
		axisOfRotation->getValue(value);
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	BillboardNode *next() {
		return (BillboardNode *)Node::next(getType());
	}

	BillboardNode *nextTraversal() {
		return (BillboardNode *)Node::nextTraversalByType(getType());
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

	void initialize() {
		recomputeBoundingBox();
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	actions
	////////////////////////////////////////////////

	void	getBillboardToViewerVector(float vector[3]);
	void	getAxisOfRotationAndBillboardToViewerPlaneVector(float vector[3]);
	void	getZAxisVectorOnAxisRotationAndBillboardToViewerPlane(float vector[3]);
	void	getSFMatrix(SFMatrix *mOut);

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFVec3f *axisOfRotation = (SFVec3f *)getExposedField(axisOfRotationFieldString);
		printStream << indentString << "\t" << "axisOfRotation " << axisOfRotation << endl;
	}
};

#endif

