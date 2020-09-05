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
*	File:	VisibilitySensorNode.h
*
******************************************************************/

#ifndef _VISIBILITYSENSOR_H_
#define _VISIBILITYSENSOR_H_

#include "vrmlfields.h"
#include "Node.h"

class VisibilitySensorNode : public Node {

public:

	VisibilitySensorNode() {
		setHeaderFlag(false);
		setType(visibilitySensorNodeString);

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

		// enterTime eventOut field
		SFTime *enterTime = new SFTime(0.0f);
		addEventOut(enterTimeFieldString, enterTime);

		// exitTime eventOut field
		SFTime *exitTime = new SFTime(0.0f);
		addEventOut(exitTimeFieldString, exitTime);
	}

	~VisibilitySensorNode() {
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
		sfvec3f->getValue();
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
		sfvec3f->getValue();
	}

	////////////////////////////////////////////////
	//	isActive
	////////////////////////////////////////////////
	
	void setIsActive(bool value) {
		SFBool *sfbool = (SFBool *)getExposedField(isActiveFieldString);
		sfbool->setValue(value);
	}
	void setIsActive(int value) {
		setIsActive(value ? true : false);
	}
	bool getIsActive() {
		SFBool *sfbool = (SFBool *)getExposedField(isActiveFieldString);
		return sfbool->getValue();
	}
	bool isActive() {
		return getIsActive();
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
	//	List
	////////////////////////////////////////////////

	VisibilitySensorNode *next() {
		return (VisibilitySensorNode *)Node::next(getType());
	}

	VisibilitySensorNode *nextTraversal() {
		return (VisibilitySensorNode *)Node::nextTraversalByType(getType());
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
		SFVec3f *center = (SFVec3f *)getExposedField(centerFieldString);
		SFVec3f *size = (SFVec3f *)getExposedField(sizeFieldString);

		printStream << indentString << "\t" << "enabled " << enabled << endl;
		printStream << indentString << "\t" << "center " << center << endl;
		printStream << indentString << "\t" << "size " << size << endl;
	}
};

#endif

