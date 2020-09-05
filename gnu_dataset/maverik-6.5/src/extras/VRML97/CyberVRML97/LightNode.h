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
*	File:	LightNode.h
*
******************************************************************/

#ifndef _LIGHTNODE_H_
#define _LIGHTNODE_H_

#include "vrmlfields.h"
#include "Node.h"

class LightNode : public Node {
	
public:

	LightNode() {
		setHeaderFlag(false);

		// on exposed field
		SFBool *bon = new SFBool(true);
		bon->setName(onFieldString);
		addExposedField(bon);

		// intensity exposed field
		SFFloat *intensity = new SFFloat(1.0f);
		intensity->setName(intensityFieldString);
		addExposedField(intensity);

		// color exposed field
		SFColor *color = new SFColor(1.0f, 1.0f, 1.0f);
		color->setName(colorFieldString);
		addExposedField(color);
	}

	virtual ~LightNode() {
	}

	////////////////////////////////////////////////
	//	On
	////////////////////////////////////////////////
	
	void setOn(bool on) {
		SFBool *bon = (SFBool *)getExposedField(onFieldString);
		bon->setValue(on);
	}
	void setOn(int value) {
		setOn(value ? true : false);
	}
	bool isOn() {
		SFBool *bon = (SFBool *)getExposedField(onFieldString);
		return bon->getValue();
	}

	////////////////////////////////////////////////
	//	Intensity
	////////////////////////////////////////////////
	
	void setIntensity(float value) {
		SFFloat *intensity = (SFFloat *)getExposedField(intensityFieldString);
		intensity->setValue(value);
	}
	float getIntensity() {
		SFFloat *intensity = (SFFloat *)getExposedField(intensityFieldString);
		return intensity->getValue();
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
	//	functions
	////////////////////////////////////////////////
	
	virtual bool isChildNodeType(Node *node) = 0;

	virtual void initialize() = 0;

	virtual void uninitialize() = 0;

	virtual void update() = 0;

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	virtual void outputContext(ostream &printStream, String indentString) = 0;
};

#endif

