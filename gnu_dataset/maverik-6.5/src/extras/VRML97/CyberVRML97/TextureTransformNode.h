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
*	File:	TextureTransformNode.h
*
******************************************************************/

#ifndef _TEXTURETRANSFORM_H_
#define _TEXTURETRANSFORM_H_

#include "vrmlfields.h"
#include "Node.h"

class TextureTransformNode : public Node {

public:

	TextureTransformNode() {
		setHeaderFlag(false);
		setType(textureTransformNodeString);

		// translation exposed field
		SFVec2f *translation = new SFVec2f(0.0f, 0.0f);
		translation->setName(translationFieldString);
		addExposedField(translation);

		// scale exposed field
		SFVec2f *scale = new SFVec2f(1.0f, 1.0f);
		scale->setName(scaleFieldString);
		addExposedField(scale);

		// center exposed field
		SFVec2f *center = new SFVec2f(0.0f, 0.0f);
		center->setName(centerFieldString);
		addExposedField(center);

		// rotation exposed field
		SFFloat *rotation = new SFFloat(0.0f);
		rotation->setName(rotationFieldString);
		addExposedField(rotation);
	}

	~TextureTransformNode() {
	}

	////////////////////////////////////////////////
	//	Translation
	////////////////////////////////////////////////

	void setTranslation(float value[]) {
		SFVec2f *translation = (SFVec2f *)getExposedField(translationFieldString);
		translation->setValue(value);
	}
	void setTranslation(float x, float y) {
		SFVec2f *translation = (SFVec2f *)getExposedField(translationFieldString);
		translation->setValue(x, y);
	}
	void getTranslation(float value[]) {
		SFVec2f *translation = (SFVec2f *)getExposedField(translationFieldString);
		translation->getValue(value);
	}

	////////////////////////////////////////////////
	//	Scale
	////////////////////////////////////////////////

	void setScale(float value[]) {
		SFVec2f *scale = (SFVec2f *)getExposedField(scaleFieldString);
		scale->setValue(value);
	}
	void setScale(float x, float y) {
		SFVec2f *scale = (SFVec2f *)getExposedField(scaleFieldString);
		scale->setValue(x, y);
	}
	void getScale(float value[]) {
		SFVec2f *scale = (SFVec2f *)getExposedField(scaleFieldString);
		scale->getValue(value);
	}

	////////////////////////////////////////////////
	//	Center
	////////////////////////////////////////////////

	void setCenter(float value[]) {
		SFVec2f *center = (SFVec2f *)getExposedField(centerFieldString);
		center->setValue(value);
	}
	void setCenter(float x, float y) {
		SFVec2f *center = (SFVec2f *)getExposedField(centerFieldString);
		center->setValue(x, y);
	}
	void getCenter(float value[]) {
		SFVec2f *center = (SFVec2f *)getExposedField(centerFieldString);
		center->getValue(value);
	}

	////////////////////////////////////////////////
	//	Rotation
	////////////////////////////////////////////////

	void setRotation(float value) {
		SFFloat *rotation = (SFFloat *)getExposedField(rotationFieldString);
		rotation->setValue(value);
	}
	float getRotation() {
		SFFloat *rotation = (SFFloat *)getExposedField(rotationFieldString);
		return rotation->getValue();
	}

	////////////////////////////////////////////////
	//	Texture Matrix
	////////////////////////////////////////////////

	void getSFMatrix(SFMatrix *matrix);

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	TextureTransformNode *next() {
		return (TextureTransformNode *)Node::next(getType());
	}

	TextureTransformNode *nextTraversal() {
		return (TextureTransformNode *)Node::nextTraversalByType(getType());
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
		SFVec2f *translation = (SFVec2f *)getExposedField(translationFieldString);
		SFVec2f *center = (SFVec2f *)getExposedField(centerFieldString);
		SFVec2f *scale = (SFVec2f *)getExposedField(scaleFieldString);
		printStream << indentString  <<  "\t"  <<  "translation " << translation << endl;
		printStream << indentString  <<  "\t"  <<  "rotation "  << getRotation() << endl;
		printStream << indentString  <<  "\t"  <<  "scale "  << scale << endl;
		printStream << indentString  <<  "\t"  <<  "center "  << center << endl;
	}
};

#endif

