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
*	File:	FogNode.h
*
******************************************************************/

#ifndef _FOG_H_
#define _FOG_H_

#include "BindableNode.h"

class FogNode : public BindableNode {

public:

	FogNode() {

		setHeaderFlag(false);
		setType(fogNodeString);

		///////////////////////////
		// Exposed Field 
		///////////////////////////
		
		// color exposed field
		SFColor *color = new SFColor(1.0f, 1.0f, 1.0f);
		addExposedField(colorFieldString, color);

		// fogType exposed field
		SFString *fogType = new SFString("LINEAR");
		addExposedField(fogTypeFieldString, fogType);

		// visibilityRange exposed field
		SFFloat *visibilityRange = new SFFloat(0.0f);
		addExposedField(visibilityRangeFieldString, visibilityRange);
	}

	~FogNode() {
	}

	////////////////////////////////////////////////
	//	Color
	////////////////////////////////////////////////

	void setColor(float value[]) {
		SFColor *color = (SFColor *)getExposedField(colorFieldString);
		color->setValue(value);
	}
	void setColor(float r, float g, float b) {
		SFColor *color = (SFColor *)getExposedField(colorFieldString);
		color->setValue(r, g, b);
	}
	void getColor(float value[]) {
		SFColor *color = (SFColor *)getExposedField(colorFieldString);
		color->getValue(value);
	}

	////////////////////////////////////////////////
	//	FogType
	////////////////////////////////////////////////

	void setFogType(String value) {
		SFString *fogType = (SFString *)getExposedField(fogTypeFieldString);
		fogType->setValue(value);
	}
	String getFogType() {
		SFString *fogType = (SFString *)getExposedField(fogTypeFieldString);
		return fogType->getValue();
	}

	////////////////////////////////////////////////
	//	VisibilityRange
	////////////////////////////////////////////////

	void setVisibilityRange(float value) {
		SFFloat *visibilityRange = (SFFloat *)getExposedField(visibilityRangeFieldString);
		visibilityRange->setValue(value);
	}
	float getVisibilityRange() {
		SFFloat *visibilityRange = (SFFloat *)getExposedField(visibilityRangeFieldString);
		return visibilityRange->getValue();
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	FogNode *next() {
		return (FogNode *)Node::next(getType());
	}

	FogNode *nextTraversal() {
		return (FogNode *)Node::nextTraversalByType(getType());
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
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFColor *color = (SFColor *)getExposedField(colorFieldString);
		SFString *fogType = (SFString *)getExposedField(fogTypeFieldString);

		printStream << indentString << "\t" << "color " << color << endl;
		printStream << indentString << "\t" << "fogType " << fogType << endl;
		printStream << indentString << "\t" << "visibilityRange " << getVisibilityRange() << endl;
	}
};

#endif

