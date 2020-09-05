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
*	File:	ProximitySensorNode.h
*
******************************************************************/

#ifndef _PROXIMITYSENSOR_H_
#define _PROXIMITYSENSOR_H_

#include "vrmlfields.h"
#include "Node.h"

#define	inRegionPrivateFieldString	"inRegion"

class ProximitySensorNode : public Node {
	
public:

	ProximitySensorNode() {
		setHeaderFlag(false);
		setType(proximitySensorNodeString);

		// enabled exposed field
		SFBool *enabled = new SFBool(true);
		addExposedField(enabledFieldString, enabled);

		// center exposed field
		SFVec3f *center = new SFVec3f(0.0f, 0.0f, 0.0f);
		addExposedField(centerFieldString, center);

		// size exposed field
		SFVec3f *size = new SFVec3f(0.0f, 0.0f, 0.0f);
		addExposedField(sizeFieldString, size);

		
		// isActive eventOut field
		SFBool *isActive = new SFBool(false);
		addEventOut(isActiveFieldString, isActive);

		// position eventOut field
		SFVec3f *position = new SFVec3f(0.0f, 0.0f, 0.0f);
		addEventOut(positionFieldString, position);

		// orientation eventOut field
		SFRotation *orientation = new SFRotation(0.0f, 0.0f, 1.0f, 0.0f);
		addEventOut(orientationFieldString, orientation);

		// enterTime eventOut field
		SFTime *enterTime = new SFTime(0.0f);
		addEventOut(enterTimeFieldString, enterTime);

		// exitTime eventOut field
		SFTime *exitTime = new SFTime(0.0f);
		addEventOut(exitTimeFieldString, exitTime);

	
		// display list field
		SFBool *inRegion = new SFBool(false);
		inRegion->setName(inRegionPrivateFieldString);
		addPrivateField(inRegion);
	}

	~ProximitySensorNode() {
	}

	////////////////////////////////////////////////
	//	Enabled
	////////////////////////////////////////////////
	
	void setEnabled(bool value) {
		SFBool *bEnabled = (SFBool *)getExposedField(enabledFieldString);
		bEnabled->setValue(value);
	}
	void setEnabled(int value) {
		setEnabled(value ? true : false);
	}
	bool getEnabled() {
		SFBool *bEnabled = (SFBool *)getExposedField(enabledFieldString);
		return bEnabled->getValue();
	}
	bool  isEnabled() {
		return getEnabled();
	}

	////////////////////////////////////////////////
	//	Center
	////////////////////////////////////////////////
	
	void setCenter(float value[]) {
		SFVec3f *sfvec3f = (SFVec3f *)getExposedField(centerFieldString);
		sfvec3f->setValue(value);
	}
	void setCenter(float x, float y, float z) {
		SFVec3f *sfvec3f = (SFVec3f *)getExposedField(centerFieldString);
		sfvec3f->setValue(x, y, z);
	}
	void getCenter(float value[]) {
		SFVec3f *sfvec3f = (SFVec3f *)getExposedField(centerFieldString);
		sfvec3f->getValue(value);
	}

	////////////////////////////////////////////////
	//	Size
	////////////////////////////////////////////////
	
	void setSize(float value[]) {
		SFVec3f *sfvec3f = (SFVec3f *)getExposedField(sizeFieldString);
		sfvec3f->setValue(value);
	}
	void setSize(float x, float y, float z) {
		SFVec3f *sfvec3f = (SFVec3f *)getExposedField(sizeFieldString);
		sfvec3f->setValue(x, y, z);
	}
	void getSize(float value[]) {
		SFVec3f *sfvec3f = (SFVec3f *)getExposedField(sizeFieldString);
		sfvec3f->getValue(value);
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
	//	Position
	////////////////////////////////////////////////
	
	void setPositionChanged(float value[]) {
		SFVec3f *position = (SFVec3f *)getEventOut(positionFieldString);
		position->setValue(value);
	}
	void setPositionChanged(float x, float y, float z) {
		SFVec3f *position = (SFVec3f *)getEventOut(positionFieldString);
		position->setValue(x, y, z);
	}
	void getPositionChanged(float value[]) {
		SFVec3f *position = (SFVec3f *)getEventOut(positionFieldString);
		position->getValue(value);
	}

	////////////////////////////////////////////////
	//	Orientation
	////////////////////////////////////////////////
	
	void setOrientationChanged(float value[]) {
		SFRotation *orientation = (SFRotation *)getEventOut(orientationFieldString);
		orientation->setValue(value);
	}
	void setOrientationChanged(float x, float y, float z, float rot) {
		SFRotation *orientation = (SFRotation *)getEventOut(orientationFieldString);
		orientation->setValue(x, y, z, rot);
	}
	void getOrientationChanged(float value[]) {
		SFRotation *orientation = (SFRotation *)getEventOut(orientationFieldString);
		orientation->getValue(value);
	}

	////////////////////////////////////////////////
	//	EnterTime
	////////////////////////////////////////////////
	
	void setEnterTime(double value) {
		SFTime *time = (SFTime *)getEventOut(enterTimeFieldString);
		time->setValue(value);
	}
	double getEnterTime() {
		SFTime *time = (SFTime *)getEventOut(enterTimeFieldString);
		return time->getValue();
	}

	////////////////////////////////////////////////
	//	ExitTime
	////////////////////////////////////////////////
	
	void setExitTime(double value) {
		SFTime *time = (SFTime *)getEventOut(exitTimeFieldString);
		time->setValue(value);
	}
	double getExitTime() {
		SFTime *time = (SFTime *)getEventOut(exitTimeFieldString);
		return time->getValue();
	}

	////////////////////////////////////////////////
	//	inRegion
	////////////////////////////////////////////////

	void setInRegion(bool value) {
		SFBool *inRegion = (SFBool *)getPrivateField(inRegionPrivateFieldString);
		inRegion->setValue(value);
	}

	bool inRegion() {
		SFBool *inRegion = (SFBool *)getPrivateField(inRegionPrivateFieldString);
		return inRegion->getValue();
	} 

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	ProximitySensorNode *next() {
		return (ProximitySensorNode *)Node::next(getType());
	}

	ProximitySensorNode *nextTraversal() {
		return (ProximitySensorNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		return false;
	}

	void initialize();

	void uninitialize();

	void update();

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *enabled = (SFBool *)getExposedField(enabledFieldString);
		SFVec3f *center = (SFVec3f *)getExposedField(centerFieldString);
		SFVec3f *size = (SFVec3f *)getExposedField(sizeFieldString);

		printStream << indentString << "\t" << "enabled " << enabled << endl;
		printStream << indentString << "\t" << "center " << center << endl;
		printStream << indentString << "\t" << "size " << size << endl;
	}
};

#endif

