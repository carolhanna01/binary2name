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
*	File:	CoordinateNode.h
*
******************************************************************/

#ifndef _COOORDINATE_H_
#define _COOORDINATE_H_

#include "vrmlfields.h"
#include "Node.h"

class CoordinateNode : public Node {

public:

        MFVec3f *mf; // JMC

	CoordinateNode() {
	        mf=NULL; // JMC

		setHeaderFlag(false);
		setType(coordinateNodeString);

		// point exposed field
		MFVec3f *mfpoint = new MFVec3f();
		mfpoint->setName(pointFieldString);
		addExposedField(mfpoint);
	}

	~CoordinateNode() {
	}

	////////////////////////////////////////////////
	//	point 
	////////////////////////////////////////////////

	void addPoint(float point[]) {
		MFVec3f *mfpoint = (MFVec3f *)getExposedField(pointFieldString);
		mfpoint->addValue(point);
	}

	void addPoint(float x, float y, float z) {
		MFVec3f *mfpoint = (MFVec3f *)getExposedField(pointFieldString);
		mfpoint->addValue(x, y, z);
	}

	int getNPoints() {
		MFVec3f *mfpoint = (MFVec3f *)getExposedField(pointFieldString);
		return mfpoint->getSize();
	}

	void getPoint(int index, float point[]) {
		MFVec3f *mfpoint = (MFVec3f *)getExposedField(pointFieldString);
		mfpoint->get1Value(index, point);
	}
	void getPointNext(float point[]) { // JMC
	  if (!mf) mf= (MFVec3f *)getExposedField(pointFieldString); // JMC
	  mf->get1ValueNext(point); // JMC
	} // JMC

	void setPoint(int index, float point[]) {
		MFVec3f *mfpoint = (MFVec3f *)getExposedField(pointFieldString);
		mfpoint->set1Value(index, point);
	}

	void setPoint(int index, float x, float y, float z) {
		MFVec3f *mfpoint = (MFVec3f *)getExposedField(pointFieldString);
		mfpoint->set1Value(index, x, y, z);
	}

	void removePoint(int index) {
		MFVec3f *mfpoint = (MFVec3f *)getExposedField(pointFieldString);
		mfpoint->remove(index);
	}

	void removeLastPoint() {
		MFVec3f *mfpoint = (MFVec3f *)getExposedField(pointFieldString);
		mfpoint->removeLastObject();
	}

	void removeFirstPoint() {
		MFVec3f *mfpoint = (MFVec3f *)getExposedField(pointFieldString);
		mfpoint->removeFirstObject();
	}

	void removeAllPoints() {
		MFVec3f *mfpoint = (MFVec3f *)getExposedField(pointFieldString);
		mfpoint->clear();
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		return false;
	}

	void initialize() {
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		if (0 < getNPoints()) {
			MFVec3f *mfpoint = (MFVec3f *)getExposedField(pointFieldString);
			printStream <<  indentString << "\t" << "point ["  << endl;
			mfpoint->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	CoordinateNode *next() {
		return (CoordinateNode *)Node::next(getType());
	}

	CoordinateNode *nextTraversal() {
		return (CoordinateNode *)Node::nextTraversalByType(getType());
	}

};

#endif

