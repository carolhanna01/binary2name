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
*	File:	MaterialNode.h
*
******************************************************************/

#ifndef _MATERIAL_H_
#define _MATERIAL_H_

#include "vrmlfields.h"
#include "Node.h"

class MaterialNode : public Node {
	
public:

	MaterialNode() {
		setHeaderFlag(false);
		setType(materialNodeString);

		// tranparency exposed field
		SFFloat *transparency = new SFFloat(0.0f);
		transparency->setName(transparencyFieldString);
		addExposedField(transparency);

		// ambientIntesity exposed field
		SFFloat *ambientIntensity = new SFFloat(0.2f);
		ambientIntensity->setName(ambientIntensityFieldString);
		addExposedField(ambientIntensity);

		// shininess exposed field
		SFFloat *shininess = new SFFloat(0.2f);
		shininess->setName(shininessFieldString);
		addExposedField(shininess);

		// diffuseColor exposed field
		SFColor *diffuseColor = new SFColor(0.8f, 0.8f, 0.8f);
		diffuseColor->setName(diffuseColorFieldString);
		addExposedField(diffuseColor);

		// specularColor exposed field
		SFColor *specularColor = new SFColor(0.0f, 0.0f, 0.0f);
		specularColor->setName(specularColorFieldString);
		addExposedField(specularColor);

		// emissiveColor exposed field
		SFColor *emissiveColor = new SFColor(0.0f, 0.0f, 0.0f);
		emissiveColor->setName(emissiveColorFieldString);
		addExposedField(emissiveColor);
	}

	~MaterialNode() {
	}

	////////////////////////////////////////////////
	//	Transparency
	////////////////////////////////////////////////
	
	void setTransparency(float value) {
		SFFloat *transparency = (SFFloat *)getExposedField(transparencyFieldString);
		transparency->setValue(value);
	}
	float getTransparency() {
		SFFloat *transparency = (SFFloat *)getExposedField(transparencyFieldString);
		return transparency->getValue();
	}

	////////////////////////////////////////////////
	//	AmbientIntensity
	////////////////////////////////////////////////
	
	void setAmbientIntensity(float intensity) {
		SFFloat *ambientIntensity = (SFFloat *)getExposedField(ambientIntensityFieldString);
		ambientIntensity->setValue(intensity);
	}
	float getAmbientIntensity() {
		SFFloat *ambientIntensity = (SFFloat *)getExposedField(ambientIntensityFieldString);
		return ambientIntensity->getValue();
	}

	////////////////////////////////////////////////
	//	Shininess
	////////////////////////////////////////////////
	
	void setShininess(float value) {
		SFFloat *shininess = (SFFloat *)getExposedField(shininessFieldString);
		shininess->setValue(value);
	}
	float getShininess() {
		SFFloat *shininess = (SFFloat *)getExposedField(shininessFieldString);
		return shininess->getValue();
	}

	////////////////////////////////////////////////
	//	DiffuseColor
	////////////////////////////////////////////////

	void setDiffuseColor(float value[]) {
		SFColor *color = (SFColor *)getExposedField(diffuseColorFieldString);
		color->setValue(value);
	}
	void setDiffuseColor(float r, float g, float b) {
		SFColor *color = (SFColor *)getExposedField(diffuseColorFieldString);
		color->setValue(r, g, b);
	}
	void getDiffuseColor(float value[]) {
		SFColor *color = (SFColor *)getExposedField(diffuseColorFieldString);
		color->getValue(value);
	}

	////////////////////////////////////////////////
	//	SpecularColor
	////////////////////////////////////////////////

	void setSpecularColor(float value[]) {
		SFColor *color = (SFColor *)getExposedField(specularColorFieldString);
		color->setValue(value);
	}
	void setSpecularColor(float r, float g, float b) {
		SFColor *color = (SFColor *)getExposedField(specularColorFieldString);
		color->setValue(r, g, b);
	}
	void getSpecularColor(float value[]) {
		SFColor *color = (SFColor *)getExposedField(specularColorFieldString);
		color->getValue(value);
	}

	////////////////////////////////////////////////
	//	EmissiveColor
	////////////////////////////////////////////////

	void setEmissiveColor(float value[]) {
		SFColor *color = (SFColor *)getExposedField(emissiveColorFieldString);
		color->setValue(value);
	}
	void setEmissiveColor(float r, float g, float b) {
		SFColor *color = (SFColor *)getExposedField(emissiveColorFieldString);
		color->setValue(r, g, b);
	}
	void getEmissiveColor(float value[]) {
		SFColor *color = (SFColor *)getExposedField(emissiveColorFieldString);
		color->getValue(value);
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	MaterialNode *next() {
		return (MaterialNode *)Node::next(getType());
	}

	MaterialNode *nextTraversal() {
		return (MaterialNode *)Node::nextTraversalByType(getType());
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
		SFColor *dcolor = (SFColor *)getExposedField(diffuseColorFieldString);
		SFColor *scolor = (SFColor *)getExposedField(specularColorFieldString);
		SFColor *ecolor = (SFColor *)getExposedField(emissiveColorFieldString);
		printStream << indentString << "\t" << "diffuseColor " << dcolor << endl;
		printStream << indentString << "\t" << "ambientIntensity " << getAmbientIntensity() << endl;
		printStream << indentString << "\t" << "specularColor " << scolor << endl;
		printStream << indentString << "\t" << "emissiveColor " << ecolor << endl;
		printStream << indentString << "\t" << "shininess " << getShininess() << endl;
		printStream << indentString << "\t" << "transparency " << getTransparency() << endl;
	}
};

#endif

