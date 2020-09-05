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
*	File:	ColorNode.h
*
******************************************************************/

#ifndef _COLOR_H_
#define _COLOR_H_

#include "vrmlfields.h"
#include "Node.h"

class ColorNode : public Node {

public:

	ColorNode() {
		setHeaderFlag(false);
		setType(colorNodeString);

		// color exposed field
		MFColor *mfcolor = new MFColor();
		mfcolor->setName(colorFieldString);
		addExposedField(mfcolor);
	}

	~ColorNode() {
	}

	////////////////////////////////////////////////
	//	color
	////////////////////////////////////////////////
	
	void addColor(float color[]) {
		MFColor *mfcolor = (MFColor *)getExposedField(colorFieldString);
		mfcolor->addValue(color);
	}
	int getNColors() {
		MFColor *mfcolor = (MFColor *)getExposedField(colorFieldString);
		return mfcolor->getSize();
	}
	void getColor(int index, float color[]) {
		MFColor *mfcolor = (MFColor *)getExposedField(colorFieldString);
		mfcolor->get1Value(index, color);
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
		if (0 < getNColors()) { 
			MFColor *mfcolor = (MFColor *)getExposedField(colorFieldString);
			printStream <<  indentString << "\t" << "color ["  << endl;
			mfcolor->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	ColorNode *next() {
		return (ColorNode *)Node::next(getType());
	}

	ColorNode *nextTraversal() {
		return (ColorNode *)Node::nextTraversalByType(getType());
	}

};

#endif
