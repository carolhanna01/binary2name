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
*	File:	CylinderSensorNode.h
*
******************************************************************/

#ifndef _CYLINDERSENSOR_H_
#define _CYLINDERSENSOR_H_

#include "vrmlfields.h"
#include "Node.h"

class CylinderSensorNode : public Node {
	
public:

	CylinderSensorNode() {
		setHeaderFlag(false);
		setType(cylinderSensorNodeString);

		// enabled exposed field
		SFBool *enabled = new SFBool(true);
		addExposedField(enabledFieldString, enabled);

		// autoOffset exposed field
		SFBool *autoOffset = new SFBool(true);
		addExposedField(autoOffsetFieldString, autoOffset);

		// diskAngle exposed field
		SFFloat *diskAngle = new SFFloat(0.262f);
		addExposedField(diskAngleFieldString, diskAngle);

		// minAngle exposed field
		SFFloat *minAngle = new SFFloat(0.0f);
		addExposedField(minAngleFieldString, minAngle);

		// maxAngle exposed field
		SFFloat *maxAngle = new SFFloat(-1.0f);
		addExposedField(maxAngleFieldString, maxAngle);

		// offset exposed field
		SFFloat *offset = new SFFloat(0.0f);
		addExposedField(offsetFieldString, offset);

	
		// isActive eventOut field
		SFBool *isActive = new SFBool(false);
		addEventOut(isActiveFieldString, isActive);

		// rotation eventOut field
		SFRotation *rotation = new SFRotation(0.0f, 0.0f, 1.0f, 0.0f);
		addEventOut(rotationFieldString, rotation);

		// trackPoint eventOut field
		SFVec3f *trackPoint = new SFVec3f(0.0f, 0.0f, 0.0f);
		addEventOut(trackPointFieldString, trackPoint);
	}

	~CylinderSensorNode() {
	}

	////////////////////////////////////////////////
	//	Enabled
	////////////////////////////////////////////////
	
	void setEnabled(bool  value) {
		SFBool *bEnabled = (SFBool *)getExposedField(enabledFieldString);
		bEnabled->setValue(value);
	}
	void setEnabled(int value) {
		setEnabled(value ? true : false);
	}
	bool  getEnabled() {
		SFBool *bEnabled = (SFBool *)getExposedField(enabledFieldString);
		return bEnabled->getValue();
	}
	bool  isEnabled() {
		return getEnabled();
	}

	////////////////////////////////////////////////
	//	AutoOffset
	////////////////////////////////////////////////
	
	void setAutoOffset(bool  value) {
		SFBool *sfbool = (SFBool *)getExposedField(autoOffsetFieldString);
		sfbool->setValue(value);
	}
	void setAutoOffset(int value) {
		setAutoOffset(value ? true : false);
	}
	bool  getAutoOffset() {
		SFBool *sfbool = (SFBool *)getExposedField(autoOffsetFieldString);
		return sfbool->getValue();
	}
	bool  isAutoOffset() {
		return getAutoOffset();
	}

	////////////////////////////////////////////////
	//	DiskAngle
	////////////////////////////////////////////////
	
	void setDiskAngle(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(diskAngleFieldString);
		sffloat->setValue(value);
	}
	float getDiskAngle() {
		SFFloat *sffloat = (SFFloat *)getExposedField(diskAngleFieldString);
		return sffloat->getValue();
	}

	////////////////////////////////////////////////
	//	MinAngle
	////////////////////////////////////////////////
	
	void setMinAngle(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(minAngleFieldString);
		sffloat->setValue(value);
	}
	float getMinAngle() {
		SFFloat *sffloat = (SFFloat *)getExposedField(minAngleFieldString);
		return sffloat->getValue();
	}

	////////////////////////////////////////////////
	//	MaxAngle
	////////////////////////////////////////////////
	
	void setMaxAngle(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(maxAngleFieldString);
		sffloat->setValue(value);
	}
	float getMaxAngle() {
		SFFloat *sffloat = (SFFloat *)getExposedField(maxAngleFieldString);
		return sffloat->getValue();
	}

	////////////////////////////////////////////////
	//	Offset
	////////////////////////////////////////////////
	
	void setOffset(float value) {
		SFFloat *sffloat = (SFFloat *)getExposedField(offsetFieldString);
		sffloat->setValue(value);
	}
	float getOffset() {
		SFFloat *sffloat = (SFFloat *)getExposedField(offsetFieldString);
		return sffloat->getValue();
	}

	////////////////////////////////////////////////
	//	isActive
	////////////////////////////////////////////////
	
	void setIsActive(bool  value) {
		SFBool *sfbool = (SFBool *)getEventOut(isActiveFieldString);
		sfbool->setValue(value);
	}
	void setIsActive(int value) {
		setIsActive(value ? true : false);
	}
	bool  getIsActive() {
		SFBool *sfbool = (SFBool *)getEventOut(isActiveFieldString);
		return sfbool->getValue();
	}
	bool  isActive() {
		return getIsActive();
	}

	////////////////////////////////////////////////
	//	Rotation
	////////////////////////////////////////////////
	
	void setRotationChanged(float value[]) {
		SFRotation *time = (SFRotation *)getEventOut(rotationFieldString);
		time->setValue(value);
	}
	void setRotationChanged(float x, float y, float z, float rot) {
		SFRotation *time = (SFRotation *)getEventOut(rotationFieldString);
		time->setValue(x, y, z, rot);
	}
	void getRotationChanged(float value[]) {
		SFRotation *time = (SFRotation *)getEventOut(rotationFieldString);
		time->getValue(value);
	}

	////////////////////////////////////////////////
	//	TrackPoint
	////////////////////////////////////////////////
	
	void setTrackPointChanged(float value[]) {
		SFVec3f *sfvec3f = (SFVec3f *)getEventOut(trackPointFieldString);
		sfvec3f->setValue(value);
	}
	void setTrackPointChanged(float x, float y, float z) {
		SFVec3f *sfvec3f = (SFVec3f *)getEventOut(trackPointFieldString);
		sfvec3f->setValue(x, y, z);
	}
	void getTrackPointChanged(float value[]) {
		SFVec3f *sfvec3f = (SFVec3f *)getEventOut(trackPointFieldString);
		sfvec3f->getValue(value);
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	CylinderSensorNode *next() {
		return (CylinderSensorNode *)Node::next(getType());
	}

	CylinderSensorNode *nextTraversal() {
		return (CylinderSensorNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		return false;
	}

	void initialize() {
		setIsActive(false);
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *autoOffset = (SFBool *)getExposedField(autoOffsetFieldString);
		SFBool *enabled = (SFBool *)getExposedField(enabledFieldString);

		printStream << indentString << "\t" << "autoOffset " << autoOffset << endl;
		printStream << indentString << "\t" << "diskAngle " << getDiskAngle() << endl;
		printStream << indentString << "\t" << "enabled " << enabled << endl;
		printStream << indentString << "\t" << "maxAngle " << getMaxAngle() << endl;
		printStream << indentString << "\t" << "minAngle " << getMinAngle() << endl;
		printStream << indentString << "\t" << "offset " << getOffset() << endl;
	}
};

#endif

