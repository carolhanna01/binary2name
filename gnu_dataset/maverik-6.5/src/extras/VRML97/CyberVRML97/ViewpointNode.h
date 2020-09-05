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
*	File:	ViewpointNode.h
*
******************************************************************/

#ifndef _VIEWPOINT_H_
#define _VIEWPOINT_H_

#include "BindableNode.h"

class ViewpointNode : public BindableNode {

public:

	ViewpointNode() {
		setHeaderFlag(false);
		setType(viewpointNodeString);

		// position exposed field
		SFVec3f *position = new SFVec3f(0.0f, 0.0f, 0.0f);
		position->setName(positionFieldString);
		addExposedField(position);

		// orientation exposed field
		SFRotation *orientation = new SFRotation(0.0f, 0.0f, 1.0f, 0.0f);
		orientation->setName(orientationFieldString);
		addExposedField(orientation);

		// description exposed field
		SFString *description = new SFString("");
		description->setName(descriptionFieldString);
		addField(description);

		// fov exposed field
		SFFloat *fov = new SFFloat(0.785398f);
		fov->setName(fieldOfViewFieldString);
		addExposedField(fov);

		// jump exposed field
		SFBool *jump = new SFBool(true);
		jump->setName(jumpFieldString);
		addExposedField(jump);
	}

	~ViewpointNode() {
	}

	////////////////////////////////////////////////
	//	Jump
	////////////////////////////////////////////////
	
	void setJump(bool value) {
		SFBool *jump = (SFBool *)getExposedField(jumpFieldString);
		jump->setValue(value);
	}
	void setJump(int value) {
		setJump(value ? true : false);
	}
	bool getJump() {
		SFBool *jump = (SFBool *)getExposedField(jumpFieldString);
		return jump->getValue();
	}

	////////////////////////////////////////////////
	//	FieldOfView
	////////////////////////////////////////////////
	
	void setFieldOfView(float value) {
		SFFloat *fov = (SFFloat *)getExposedField(fieldOfViewFieldString);
		fov->setValue(value);
	}
	float getFieldOfView() {
		SFFloat *fov = (SFFloat *)getExposedField(fieldOfViewFieldString);
		return fov->getValue();
	}

	////////////////////////////////////////////////
	//	Description
	////////////////////////////////////////////////
	
	void setDescription(String value) {
		SFString *description = (SFString *)getField(descriptionFieldString);
		description->setValue(value);
	}
	String getDescription() {
		SFString *description = (SFString *)getField(descriptionFieldString);
		return description->getValue();
	}

	////////////////////////////////////////////////
	//	Position
	////////////////////////////////////////////////

	void setPosition(float value[]) {
		SFVec3f *position = (SFVec3f *)getExposedField(positionFieldString);
		position->setValue(value);
	}
	void setPosition(float x, float y, float z) {
		SFVec3f *position = (SFVec3f *)getExposedField(positionFieldString);
		position->setValue(x, y, z);
	}
	void getPosition(float value[]) {
		SFVec3f *position = (SFVec3f *)getExposedField(positionFieldString);
		position->getValue(value);
	}
	SFVec3f *getPosition() {
		SFVec3f *position = (SFVec3f *)getExposedField(positionFieldString);
		return position;
	}

	////////////////////////////////////////////////
	//	Orientation
	////////////////////////////////////////////////

	void setOrientation(float value[]) {
		SFRotation *orientation = (SFRotation *)getExposedField(orientationFieldString);
		orientation->setValue(value);
	}
	void setOrientation(float x, float y, float z, float w) {
		SFRotation *orientation = (SFRotation *)getExposedField(orientationFieldString);
		orientation->setValue(x, y, z, w);
	}
	void getOrientation(float value[]) {
		SFRotation *orientation = (SFRotation *)getExposedField(orientationFieldString);
		orientation->getValue(value);
	}
	SFRotation *getOrientation() {
		SFRotation *orientation = (SFRotation *)getExposedField(orientationFieldString);
		return orientation;
	}

	////////////////////////////////////////////////
	//	Add position
	////////////////////////////////////////////////

	void addPosition(float worldTranslation[3]) { 
		SFVec3f *position = (SFVec3f *)getExposedField(positionFieldString);
		position->add(worldTranslation);
	}

	void addPosition(float worldx, float worldy, float worldz) { 
		SFVec3f *position = (SFVec3f *)getExposedField(positionFieldString);
		position->add(worldx, worldy, worldz);
	}

	void addPosition(float localTranslation[3], float frame[3][3]) { 
		SFVec3f *position = (SFVec3f *)getExposedField(positionFieldString);
		float	translation[3];
		for (int axis=0; axis<3; axis++) {
			SFVec3f vector(frame[axis]);
			vector.scale(localTranslation[axis]);
			vector.getValue(translation);
			position->add(translation);
		}
	}

	void addPosition(float x, float y, float z, float frame[3][3]) { 
		float localTranslation[3];
		localTranslation[0] = x;
		localTranslation[1] = y;
		localTranslation[2] = z;
		addPosition(localTranslation, frame);
	}

	////////////////////////////////////////////////
	//	Add orientation
	////////////////////////////////////////////////

	void addOrientation(SFRotation *rot) {
		SFRotation *orientation = (SFRotation *)getExposedField(orientationFieldString);
		orientation->add(rot);
	}

	void addOrientation(float rotationValue[4]) {
		SFRotation *orientation = (SFRotation *)getExposedField(orientationFieldString);
		orientation->add(rotationValue);
	}

	void addOrientation(float x, float y, float z, float rot) {
		SFRotation *orientation = (SFRotation *)getExposedField(orientationFieldString);
		orientation->add(x, y, z, rot);
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	ViewpointNode *next() {
		return (ViewpointNode *)Node::next(getType());
	}

	ViewpointNode *nextTraversal() {
		return (ViewpointNode *)Node::nextTraversalByType(getType());
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

	void outputContext(ostream& printStream, String indentString) {
		SFVec3f *position = (SFVec3f *)getExposedField(positionFieldString);
		SFRotation *orientation = (SFRotation *)getExposedField(orientationFieldString);
		SFBool *jump = (SFBool *)getExposedField(jumpFieldString);
		SFString *description = (SFString *)getField(descriptionFieldString);

		printStream << indentString << "\t" << "fieldOfView " << getFieldOfView() << endl;
		printStream << indentString << "\t" << "jump " << jump << endl;
		printStream << indentString << "\t" << "position " << position << endl;
		printStream << indentString << "\t" << "orientation " << orientation << endl;
		printStream << indentString << "\t" << "description " << description << endl;
	}

	////////////////////////////////////////////////
	//	Local frame
	////////////////////////////////////////////////

	void getFrame(float frame[3][3]) {

		SFRotation *orientation = getOrientation();

		// local x frame
		frame[0][0] = 1.0f;
		frame[0][1] = 0.0f;
		frame[0][2] = 0.0f;
		orientation->multi(frame[0]);

		// local 0 frame
		frame[1][0] = 0.0f;
		frame[1][1] = 1.0f;
		frame[1][2] = 0.0f;
		orientation->multi(frame[1]);

		// local 0 frame
		frame[2][0] = 0.0f;
		frame[2][1] = 0.0f;
		frame[2][2] = 1.0f;
		orientation->multi(frame[2]);
	}

	void translate(float vector[3]) {
		float frame[3][3];
		getFrame(frame);
		addPosition(vector, frame);
	}

	void translate(SFVec3f vec) {
		float frame[3][3];
		float vector[3];
		getFrame(frame);
		vec.getValue(vector);
		addPosition(vector, frame);
	}

	void rotate(float rotation[4]) {
		addOrientation(rotation);
	}

	void rotate(SFRotation rot) {
		float rotation[4];
		rot.getValue(rotation);
		addOrientation(rotation);
	}

	////////////////////////////////////////////////
	//	ViewpointNode Matrix
	////////////////////////////////////////////////

	void getMatrix(SFMatrix *matrix) {
		float	position[3];
		float	rotation[4];
		
		getPosition(position);
		SFVec3f	transView(position);
		transView.invert();

		getOrientation(rotation);
		SFRotation rotView(rotation);
		rotView.invert();

		SFMatrix	mxTrans, mxRot;
		mxTrans.setTranslation(&transView);
		mxRot.setRotation(&rotView);

		matrix->init();
		matrix->add(&mxRot);
		matrix->add(&mxTrans);
	}

	void getMatrix(float value[4][4]) {
		SFMatrix	mx;
		getMatrix(&mx);
		mx.getValue(value);
	}
};

#endif

