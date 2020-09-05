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
*	File:	TextureCoordinateNode.h
*
******************************************************************/

#ifndef _TEXTURECOORDINATE_H_
#define _TEXTURECOORDINATE_H_

#include "vrmlfields.h"
#include "Node.h"

class TextureCoordinateNode : public Node {
	
public:

	MFVec2f *mf; // JMC
	TextureCoordinateNode() {
	        mf= NULL; // JMC
	        setHeaderFlag(false);
		setType(textureCoordinateNodeString);

		// point exposed field
		MFVec2f *mfpoint = new MFVec2f();
		mfpoint->setName(pointFieldString);
		addExposedField(mfpoint);
	}

	~TextureCoordinateNode() {
	}

	////////////////////////////////////////////////
	//	point 
	////////////////////////////////////////////////

	void addPoint(float point[]) {
		MFVec2f *mfpoint = (MFVec2f *)getExposedField(pointFieldString);
		mfpoint->addValue(point);
	}

	int getNPoints() {
		MFVec2f *mfpoint = (MFVec2f *)getExposedField(pointFieldString);
		return mfpoint->getSize();
	}

	void getPoint(int index, float point[]) {
		MFVec2f *mfpoint = (MFVec2f *)getExposedField(pointFieldString);
		mfpoint->get1Value(index, point);
	}

	void getPointNext(float point[]) { // JMC
	  if (!mf) mf = (MFVec2f *)getExposedField(pointFieldString); // JMC
	  mf->get1ValueNext(point); // JMC
	} // JMC

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
			MFVec2f *mfpoint = (MFVec2f *)getExposedField(pointFieldString);
			printStream <<  indentString << "\t" << "point ["  << endl;
			mfpoint->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}
	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	TextureCoordinateNode *next() {
		return (TextureCoordinateNode *)Node::next(getType());
	}

	TextureCoordinateNode *nextTraversal() {
		return (TextureCoordinateNode *)Node::nextTraversalByType(getType());
	}

};

#endif

