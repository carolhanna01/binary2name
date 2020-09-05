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
*	File:	TouchSensorNode.h
*
******************************************************************/

#ifndef _TOUCHSENSOR_H_
#define _TOUCHSENSOR_H_

#include "vrmlfields.h"
#include "Node.h"

class TouchSensorNode : public Node {
	
public:

	TouchSensorNode() {
		setHeaderFlag(false);
		setType(touchSensorNodeString);

		// enabled exposed field
		SFBool *enabled = new SFBool(true);
		addExposedField(enabledFieldString, enabled);

		// isActive eventOut field
		SFBool *isActive = new SFBool(false);
		addEventOut(isActiveFieldString, isActive);

		// hitNormal eventOut field
		SFVec3f *hitNormal = new SFVec3f(0.0f, 0.0f, 0.0f);
		addEventOut(hitNormalFieldString, hitNormal);

		// hitTexCoord eventOut field
		SFVec2f *hitTexCoord = new SFVec2f(0.0f, 0.0f);
		addEventOut(hitTexCoordFieldString, hitTexCoord);

		// hitPoint eventOut field
		SFVec3f *hitPoint = new SFVec3f(0.0f, 0.0f, 0.0f);
		addEventOut(hitPointFieldString, hitPoint);

		// isOver eventOut field
		SFBool *isOver = new SFBool(false);
		addEventOut(isOverFieldString, isOver);

		// exitTime eventOut field
		SFTime *time = new SFTime(0.0f);
		addEventOut(touchTimeFieldString, time);
	}

	~TouchSensorNode() {
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
	bool isEnabled() {
		return getEnabled();
	}
	SFBool *getEnabledField() {
		return (SFBool *)getExposedField(enabledFieldString);
	}

	////////////////////////////////////////////////
	//	isActive
	////////////////////////////////////////////////
	
	void setIsActive(bool value) {
		SFBool *sfbool = (SFBool *)getEventOut(isActiveFieldString);
		sfbool->setValue(value);
	}
	bool getIsActive() {
		SFBool *sfbool = (SFBool *)getEventOut(isActiveFieldString);
		return sfbool->getValue();
	}
	bool isActive() {
		return getIsActive();
	}
	SFBool *getIsActiveField() {
		return (SFBool *)getEventOut(isActiveFieldString);
	}

	////////////////////////////////////////////////
	//	isOver
	////////////////////////////////////////////////
	
	void setIsOver(bool  value) {
		SFBool *sfbool = (SFBool *)getEventOut(isOverFieldString);
		sfbool->setValue(value);
	}
	void setIsOver(int value) {
		setIsOver(value ? true : false);
	}
	bool  getIsOver() {
		SFBool *sfbool = (SFBool *)getEventOut(isOverFieldString);
		return sfbool->getValue();
	}
	bool  isOver() {
		return getIsOver();
	}
	SFBool *getIsOverField() {
		return (SFBool *)getEventOut(isOverFieldString);
	}

	////////////////////////////////////////////////
	//	hitNormal
	////////////////////////////////////////////////
	
	void setHitNormalChanged(float value[]) {
		SFVec3f *normal = (SFVec3f *)getEventOut(hitNormalFieldString);
		normal->setValue(value);
	}
	void setHitNormalChanged(float x, float y, float z) {
		SFVec3f *normal = (SFVec3f *)getEventOut(hitNormalFieldString);
		normal->setValue(x, y, z);
	}
	void getHitNormalChanged(float value[]) {
		SFVec3f *normal = (SFVec3f *)getEventOut(hitNormalFieldString);
		normal->getValue(value);
	}
	SFVec3f *getHitNormalChanged() {
		return (SFVec3f *)getEventOut(hitNormalFieldString);
	}

	////////////////////////////////////////////////
	//	hitPoint
	////////////////////////////////////////////////
	
	void setHitPointChanged(float value[]) {
		SFVec3f *point = (SFVec3f *)getEventOut(hitPointFieldString);
		point->setValue(value);
	}
	void setHitPointChanged(float x, float y, float z) {
		SFVec3f *point = (SFVec3f *)getEventOut(hitPointFieldString);
		point->setValue(x, y, z);
	}
	void getHitPointChanged(float value[]) {
		SFVec3f *point = (SFVec3f *)getEventOut(hitPointFieldString);
		point->getValue(value);
	}
	SFVec3f *getHitPointChanged() {
		return (SFVec3f *)getEventOut(hitPointFieldString);
	}

	////////////////////////////////////////////////
	//	hitTexCoord
	////////////////////////////////////////////////
	
	void setHitTexCoord(float value[]) {
		SFVec2f *point = (SFVec2f *)getEventOut(hitTexCoordFieldString);
		point->setValue(value);
	}
	void setHitTexCoord(float x, float y) {
		SFVec2f *point = (SFVec2f *)getEventOut(hitTexCoordFieldString);
		point->setValue(x, y);
	}
	void getHitTexCoord(float value[]) {
		SFVec2f *point = (SFVec2f *)getEventOut(hitTexCoordFieldString);
		point->getValue(value);
	}
	SFVec2f *getHitTexCoord() {
		return (SFVec2f *)getEventOut(hitTexCoordFieldString);
	}

	////////////////////////////////////////////////
	//	ExitTime
	////////////////////////////////////////////////
	
	void setTouchTime(double value) {
		SFTime *time = (SFTime *)getEventOut(touchTimeFieldString);
		time->setValue(value);
	}
	double getTouchTime() {
		SFTime *time = (SFTime *)getEventOut(touchTimeFieldString);
		return time->getValue();
	}
	SFTime *getTouchTimeField() {
		return (SFTime *)getEventOut(touchTimeFieldString);
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	TouchSensorNode *next() {
		return (TouchSensorNode *)Node::next(getType());
	}

	TouchSensorNode *nextTraversal() {
		return (TouchSensorNode *)Node::nextTraversalByType(getType());
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
		SFBool *enabled = (SFBool *)getExposedField(enabledFieldString);
		printStream << indentString << "\t" << "enabled " << enabled << endl;
	}
};

#endif
