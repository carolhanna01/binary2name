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
*	File:	TransformNode.h
*
******************************************************************/

#ifndef _TRANSFORM_H_
#define _TRANSFORM_H_

#include "vrmlfields.h"
#include "Node.h"
#include "GroupingNode.h"

class TransformNode : public GroupingNode {

public:

	TransformNode() {
		setHeaderFlag(false);
		setType(transformNodeString);

		// translation exposed field
		SFVec3f *translation = new SFVec3f(0.0f, 0.0f, 0.0f);
		translation->setName(translationFieldString);
		addExposedField(translation);

		// scale exposed field
		SFVec3f *scale = new SFVec3f(1.0f, 1.0f, 1.0f);
		scale->setName(scaleFieldString);
		addExposedField(scale);

		// center exposed field
		SFVec3f *center = new SFVec3f(0.0f, 0.0f, 0.0f);
		center->setName(centerFieldString);
		addExposedField(center);

		// rotation exposed field
		SFRotation *rotation = new SFRotation(0.0f, 0.0f, 1.0f, 0.0f);
		rotation->setName(rotationFieldString);
		addExposedField(rotation);

		// scaleOrientation exposed field
		SFRotation *scaleOrientation = new SFRotation(0.0f, 0.0f, 1.0f, 0.0f);
		scaleOrientation->setName(scaleOrientationFieldString);
		addExposedField(scaleOrientation);
	}

	~TransformNode() {
	}

	////////////////////////////////////////////////
	//	Translation
	////////////////////////////////////////////////

	void setTranslation(float value[]) {
		SFVec3f *translation = (SFVec3f *)getExposedField(translationFieldString);
		translation->setValue(value);
	}
	void setTranslation(float x, float y, float z) {
		SFVec3f *translation = (SFVec3f *)getExposedField(translationFieldString);
		translation->setValue(x, y, z);
	}
	void getTranslation(float value[]) {
		SFVec3f *translation = (SFVec3f *)getExposedField(translationFieldString);
		translation->getValue(value);
	}

	////////////////////////////////////////////////
	//	Scale
	////////////////////////////////////////////////

	void setScale(float value[]) {
		SFVec3f *scale = (SFVec3f *)getExposedField(scaleFieldString);
		scale->setValue(value);
	}
	void setScale(float x, float y, float z) {
		SFVec3f *scale = (SFVec3f *)getExposedField(scaleFieldString);
		scale->setValue(x, y, z);
	}
	void getScale(float value[]) {
		SFVec3f *scale = (SFVec3f *)getExposedField(scaleFieldString);
		scale->getValue(value);
	}

	////////////////////////////////////////////////
	//	Center
	////////////////////////////////////////////////

	void setCenter(float value[]) {
		SFVec3f *center = (SFVec3f *)getExposedField(centerFieldString);
		center->setValue(value);
	}
	void setCenter(float x, float y, float z) {
		SFVec3f *center = (SFVec3f *)getExposedField(centerFieldString);
		center->setValue(x, y, z);
	}
	void getCenter(float value[]) {
		SFVec3f *center = (SFVec3f *)getExposedField(centerFieldString);
		center->getValue(value);
	}

	////////////////////////////////////////////////
	//	Rotation
	////////////////////////////////////////////////

	void setRotation(float value[]) {
		SFRotation *rotation = (SFRotation *)getExposedField(rotationFieldString);
		rotation->setValue(value);
	}
	void setRotation(float x, float y, float z, float w) {
		SFRotation *rotation = (SFRotation *)getExposedField(rotationFieldString);
		rotation->setValue(x, y, z, w);
	}
	void getRotation(float value[]) {
		SFRotation *rotation = (SFRotation *)getExposedField(rotationFieldString);
		rotation->getValue(value);
	}

	////////////////////////////////////////////////
	//	ScaleOrientation
	////////////////////////////////////////////////

	void setScaleOrientation(float value[]) {
		SFRotation *scaleOrientation = (SFRotation *)getExposedField(scaleOrientationFieldString);
		scaleOrientation->setValue(value);
	}
	void setScaleOrientation(float x, float y, float z, float w) {
		SFRotation *scaleOrientation = (SFRotation *)getExposedField(scaleOrientationFieldString);
		scaleOrientation->setValue(x, y, z, w);
	}
	void getScaleOrientation(float value[]) {
		SFRotation *scaleOrientation = (SFRotation *)getExposedField(scaleOrientationFieldString);
		scaleOrientation->getValue(value);
	}

	////////////////////////////////////////////////
	//	Matrix
	////////////////////////////////////////////////

	void getSFMatrix(SFMatrix *mOut);

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	TransformNode *next() {
		return (TransformNode *)Node::next(getType());
	}

	TransformNode *nextTraversal() {
		return (TransformNode *)Node::nextTraversalByType(getType());
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
		//recomputeBoundingBox();JMC
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		float vec[3];
		float rot[4];
		getTranslation(vec);		printStream << indentString << "\t" << "translation " << vec[0] << " "<< vec[1] << " " << vec[2] << endl;
		getRotation(rot);			printStream << indentString << "\t" << "rotation " << rot[0] << " "<< rot[1] << " " << rot[2] << " " << rot[3] << endl;
		getScale(vec);				printStream << indentString << "\t" << "scale " << vec[0] << " "<< vec[1] << " " << vec[2] << endl;
		getScaleOrientation(rot);	printStream << indentString << "\t" << "scaleOrientation " << rot[0] << " "<< rot[1] << " " << rot[2] << " " << rot[3] << endl;
		getCenter(vec);				printStream << indentString << "\t" << "center " << vec[0] << " "<< vec[1] << " " << vec[2] << endl;
	}
};

#endif

