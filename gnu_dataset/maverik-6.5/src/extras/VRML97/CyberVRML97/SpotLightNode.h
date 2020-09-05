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
*	File:	SpotLightNode.h
*
******************************************************************/

#ifndef _SPOTLIGHT_H_
#define _SPOTLIGHT_H_

#include "LightNode.h"

class SpotLightNode : public LightNode {
	
public:

	SpotLightNode() {
		setType(spotLightNodeString);

		// ambient intensity exposed field
		SFFloat *ambientIntensity = new SFFloat(0.0f);
		ambientIntensity->setName(ambientIntensityFieldString);
		addExposedField(ambientIntensity);

		// location exposed field
		SFVec3f *location = new SFVec3f(0.0f, 0.0f, 0.0f);
		location->setName(locationFieldString);
		addExposedField(location);

		// direction exposed field
		SFVec3f *direction = new SFVec3f(0.0f, 0.0f, -1.0f);
		direction->setName(directionFieldString);
		addExposedField(direction);

		// radius exposed field
		SFFloat *radius = new SFFloat(100.0f);
		radius->setName(radiusFieldString);
		addExposedField(radius);

		// attenuation exposed field
		SFVec3f *attenuation = new SFVec3f(1.0f, 0.0f, 0.0f);
		attenuation->setName(attenuationFieldString);
		addExposedField(attenuation);

		// beamWidth exposed field
		SFFloat *beamWidth = new SFFloat(1.570796f);
		beamWidth->setName(beamWidthFieldString);
		addExposedField(beamWidth);

		// cutOffAngle exposed field
		SFFloat *cutOffAngle = new SFFloat(0.785398f);
		cutOffAngle->setName(cutOffAngleFieldString);
		addExposedField(cutOffAngle);
	}

	~SpotLightNode() {
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
	//	Location
	////////////////////////////////////////////////

	void setLocation(float value[]) {
		SFVec3f *location = (SFVec3f *)getExposedField(locationFieldString);
		location->setValue(value);
	}
	void setLocation(float x, float y, float z) {
		SFVec3f *location = (SFVec3f *)getExposedField(locationFieldString);
		location->setValue(x, y, z);
	}
	void getLocation(float value[]) {
		SFVec3f *location = (SFVec3f *)getExposedField(locationFieldString);
		location->getValue(value);
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
	//	Radius
	////////////////////////////////////////////////
	
	void setRadius(float value) {
		SFFloat *radius = (SFFloat *)getExposedField(radiusFieldString);
		radius->setValue(value);
	}
	float getRadius() {
		SFFloat *radius = (SFFloat *)getExposedField(radiusFieldString);
		return radius->getValue();
	}

	////////////////////////////////////////////////
	//	Attenuation
	////////////////////////////////////////////////

	void setAttenuation(float value[]) {
		SFVec3f *attenuation = (SFVec3f *)getExposedField(attenuationFieldString);
		attenuation->setValue(value);
	}
	void setAttenuation(float x, float y, float z) {
		SFVec3f *attenuation = (SFVec3f *)getExposedField(attenuationFieldString);
		attenuation->setValue(x, y, z);
	}
	void getAttenuation(float value[]) {
		SFVec3f *attenuation = (SFVec3f *)getExposedField(attenuationFieldString);
		attenuation->getValue(value);
	}

	////////////////////////////////////////////////
	//	BeamWidth
	////////////////////////////////////////////////
	
	void setBeamWidth(float value) {
		SFFloat *bwidth = (SFFloat *)getExposedField(beamWidthFieldString);
		bwidth->setValue(value);
	}
	float getBeamWidth() {
		SFFloat *bwidth = (SFFloat *)getExposedField(beamWidthFieldString);
		return bwidth->getValue();
	}


	////////////////////////////////////////////////
	//	CutOffAngle
	////////////////////////////////////////////////
	
	void setCutOffAngle(float value) {
		SFFloat *angle = (SFFloat *)getExposedField(cutOffAngleFieldString);
		angle->setValue(value);
	}
	float getCutOffAngle() {
		SFFloat *angle = (SFFloat *)getExposedField(cutOffAngleFieldString);
		return angle->getValue();
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

	SpotLightNode *next() {
		return (SpotLightNode *)Node::next(getType());
	}

	SpotLightNode *nextTraversal() {
		return (SpotLightNode *)Node::nextTraversalByType(getType());
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
		SFColor *color = (SFColor *)getExposedField(colorFieldString);
		SFVec3f *direction = (SFVec3f *)getExposedField(directionFieldString);
		SFVec3f *location = (SFVec3f *)getExposedField(locationFieldString);
		SFVec3f *attenuation = (SFVec3f *)getExposedField(attenuationFieldString);

		printStream << indentString << "\t" << "on " << bon << endl;
		printStream << indentString << "\t" << "intensity " << getIntensity() << endl;
		printStream << indentString << "\t" << "ambientIntensity " << getAmbientIntensity() << endl;
		printStream << indentString << "\t" << "color " << color << endl;
		printStream << indentString << "\t" << "direction " << direction << endl;
		printStream << indentString << "\t" << "location " << location << endl;
		printStream << indentString << "\t" << "beamWidth " << getBeamWidth() << endl;
		printStream << indentString << "\t" << "cutOffAngle " << getCutOffAngle() << endl;
		printStream << indentString << "\t" << "radius " << getRadius() << endl;
		printStream << indentString << "\t" << "attenuation " << attenuation << endl;
	}
};

#endif
