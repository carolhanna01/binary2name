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
*	File:	DirectionalLightNode.h
*
******************************************************************/

#ifndef _DIRECTIONALLIGHT_H_
#define _DIRECTIONALLIGHT_H_

#include "vrmlfields.h"
#include "LightNode.h"

class DirectionalLightNode : public LightNode {
	
public:

	DirectionalLightNode() {
		setType(directionalLightNodeString);

		// ambient intensity exposed field
		SFFloat *ambientIntensity = new SFFloat(0.0f);
		ambientIntensity->setName(ambientIntensityFieldString);
		addExposedField(ambientIntensity);

		// direction exposed field
		SFVec3f *direction = new SFVec3f(0.0f, 0.0f, -1.0f);
		direction->setName(directionFieldString);
		addExposedField(direction);
	}

	~DirectionalLightNode() {
	}

	////////////////////////////////////////////////
	//	AmbientIntensity
	////////////////////////////////////////////////
	
	void setAmbientIntensity(float value) {
		SFFloat *intensity = (SFFloat *)getExposedField(ambientIntensityFieldString);
		intensity->setValue(value);
	}
	float getAmbientIntensity() {
		SFFloat *intensity = (SFFloat *)getExposedField(ambientIntensityFieldString);
		return intensity->getValue();
	}

	////////////////////////////////////////////////
	//	Direction
	////////////////////////////////////////////////

	void setDirection(float value[]) {
		SFVec3f *direction = (SFVec3f *)getExposedField(directionFieldString);
		direction->setValue(value);
	}
	void setDirection(float x, float y, float z) {
		SFVec3f *direction = (SFVec3f *)getExposedField(directionFieldString);
		direction->setValue(x, y, z);
	}
	void getDirection(float value[]) {
		SFVec3f *direction = (SFVec3f *)getExposedField(directionFieldString);
		direction->getValue(value);
	}

	////////////////////////////////////////////////
	//	Diffuse Color
	////////////////////////////////////////////////

	void getDiffuseColor(float value[]) {
		getColor(value);
		float	intensity = getIntensity();
		value[0] *= intensity;
		value[1] *= intensity;
		value[2] *= intensity;
	}

	////////////////////////////////////////////////
	//	Ambient Color
	////////////////////////////////////////////////

	void getAmbientColor(float value[]) {
		getColor(value);
		float	intensity = getIntensity();
		float	ambientIntensity = getAmbientIntensity();
		value[0] *= intensity * ambientIntensity;
		value[1] *= intensity * ambientIntensity;
		value[2] *= intensity * ambientIntensity;
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	DirectionalLightNode *next() {
		return (DirectionalLightNode *)Node::next(getType());
	}

	DirectionalLightNode *nextTraversal() {
		return (DirectionalLightNode *)Node::nextTraversalByType(getType());
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
		SFBool *bon = (SFBool *)getExposedField(onFieldString);
		SFVec3f *direction = (SFVec3f *)getExposedField(directionFieldString);
		SFColor *color = (SFColor *)getExposedField(colorFieldString);

		printStream << indentString << "\t" << "on " << bon  << endl;
		printStream << indentString << "\t" << "intensity " << getIntensity()  << endl;
		printStream << indentString << "\t" << "ambientIntensity " << getAmbientIntensity()  << endl;
		printStream << indentString << "\t" << "color " << color  << endl;
		printStream << indentString << "\t" << "direction " << direction  << endl;
	}
};

#endif

