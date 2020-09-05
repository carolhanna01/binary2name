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
*	File:	PlaneSensorNode.h
*
******************************************************************/

#ifndef _PLANESENSOR_H_
#define _PLANESENSOR_H_

#include "vrmlfields.h"
#include "Node.h"

class PlaneSensorNode : public Node {
	
public:

	PlaneSensorNode() {
		setHeaderFlag(false);
		setType(planeSensorNodeString);

		// enabled exposed field
		SFBool *enabled = new SFBool(true);
		addExposedField(enabledFieldString, enabled);

		// autoOffset exposed field
		SFBool *autoOffset = new SFBool(true);
		addExposedField(autoOffsetFieldString, autoOffset);

		// minPosition exposed field
		SFVec2f *minPosition = new SFVec2f(0.0f, 0.0f);
		addExposedField(minPositionFieldString, minPosition);

		// maxAngle exposed field
		SFVec2f *maxPosition = new SFVec2f(-1.0f, -1.0f);
		addExposedField(maxPositionFieldString, maxPosition);

		// offset exposed field
		SFVec3f *offset = new SFVec3f(0.0f, 0.0f, 0.0f);
		addExposedField(offsetFieldString, offset);
	
		// isActive eventOut field
		SFBool *isActive = new SFBool(false);
		addEventOut(isActiveFieldString, isActive);

		// translation eventOut field
		SFVec3f *translation = new SFVec3f(0.0f, 0.0f, 0.0f);
		addEventOut(translationFieldString, translation);

		// trackPoint eventOut field
		SFVec3f *trackPoint = new SFVec3f(0.0f, 0.0f, 0.0f);
		addEventOut(trackPointFieldString, trackPoint);
	}

	~PlaneSensorNode() {
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
	SFBool *getEnabledField() {
		return (SFBool *)getExposedField(enabledFieldString);
	}

	////////////////////////////////////////////////
	//	AutoOffset
	////////////////////////////////////////////////
	
	void setAutoOffset(bool value) {
		SFBool *sfbool = (SFBool *)getExposedField(autoOffsetFieldString);
		sfbool->setValue(value);
	}
	void setAutoOffset(int value) {
		setAutoOffset(value ? true : false);
	}
	bool getAutoOffset() {
		SFBool *sfbool = (SFBool *)getExposedField(autoOffsetFieldString);
		return sfbool->getValue();
	}
	bool  isAutoOffset() {
		return getAutoOffset();
	}
	SFBool *getAutoOffsetField() {
		return (SFBool *)getExposedField(autoOffsetFieldString);
	}

	////////////////////////////////////////////////
	//	MinPosition
	////////////////////////////////////////////////
	
	void setMinPosition(float value[]) {
		SFVec2f *sfvec2f = (SFVec2f *)getExposedField(minPositionFieldString);
		sfvec2f->setValue(value);
	}
	void setMinPosition(float x, float y) {
		SFVec2f *sfvec2f = (SFVec2f *)getExposedField(minPositionFieldString);
		sfvec2f->setValue(x, y);
	}
	void getMinPosition(float value[]) {
		SFVec2f *sfvec2f = (SFVec2f *)getExposedField(minPositionFieldString);
		sfvec2f->getValue(value);
	}
	void getMinPosition(float *x, float *y) {
		SFVec2f *sfvec2f = (SFVec2f *)getExposedField(minPositionFieldString);
		*x = sfvec2f->getX();
		*y = sfvec2f->getY();
	}
	SFVec2f *getMinPositionField() {
		return (SFVec2f *)getExposedField(minPositionFieldString);
	}

	////////////////////////////////////////////////
	//	MaxPosition
	////////////////////////////////////////////////
	
	void setMaxPosition(float value[]) {
		SFVec2f *sfvec2f = (SFVec2f *)getExposedField(maxPositionFieldString);
		sfvec2f->setValue(value);
	}
	void setMaxPosition(float x, float y) {
		SFVec2f *sfvec2f = (SFVec2f *)getExposedField(maxPositionFieldString);
		sfvec2f->setValue(x, y);
	}
	void getMaxPosition(float value[]) {
		SFVec2f *sfvec2f = (SFVec2f *)getExposedField(maxPositionFieldString);
		sfvec2f->getValue(value);
	}
	void getMaxPosition(float *x, float *y) {
		SFVec2f *sfvec2f = (SFVec2f *)getExposedField(maxPositionFieldString);
		*x = sfvec2f->getX();
		*y = sfvec2f->getY();
	}
	SFVec2f *getMaxPositionField() {
		return (SFVec2f *)getExposedField(maxPositionFieldString);
	}

	////////////////////////////////////////////////
	//	Offset
	////////////////////////////////////////////////
	
	void setOffset(float value[]) {
		SFVec3f *vector = (SFVec3f *)getExposedField(offsetFieldString);
		vector->setValue(value);
	}
	void getOffset(float value[]) {
		SFVec3f *vector = (SFVec3f *)getExposedField(offsetFieldString);
		vector->getValue(value);
	}
	SFVec3f *getOffsetField() {
		return (SFVec3f *)getExposedField(offsetFieldString);
	}

	////////////////////////////////////////////////
	//	isActive
	////////////////////////////////////////////////
	
	void setIsActive(bool value) {
		SFBool *sfbool = (SFBool *)getEventOut(isActiveFieldString);
		sfbool->setValue(value);
	}
	bool  getIsActive() {
		SFBool *sfbool = (SFBool *)getEventOut(isActiveFieldString);
		return sfbool->getValue();
	}
	bool isActive() {
		return getAutoOffset();
	}
	SFBool *getIsActiveField() {
		return (SFBool *)getEventOut(isActiveFieldString);
	}

	////////////////////////////////////////////////
	//	Translation
	////////////////////////////////////////////////
	
	void setTranslationChanged(float value[]) {
		SFVec3f *translation = (SFVec3f *)getEventOut(translationFieldString);
		translation->setValue(value);
	}
	void setTranslationChanged(float x, float y, float z) {
		SFVec3f *translation = (SFVec3f *)getEventOut(translationFieldString);
		translation->setValue(x, y, z);
	}
	void getTranslationChanged(float value[]) {
		SFVec3f *translation = (SFVec3f *)getEventOut(translationFieldString);
		translation->getValue(value);
	}
	SFVec3f *getTranslationChangedField() {
		return (SFVec3f *)getEventOut(translationFieldString);
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
	SFVec3f *getTrackPointChangedField() {
		return (SFVec3f *)getEventOut(trackPointFieldString);
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	PlaneSensorNode *next() {
		return (PlaneSensorNode *)Node::next(getType());
	}

	PlaneSensorNode *nextTraversal() {
		return (PlaneSensorNode *)Node::nextTraversalByType(getType());
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
		SFVec2f *maxpos = (SFVec2f *)getExposedField(maxPositionFieldString);
		SFVec2f *minpos = (SFVec2f *)getExposedField(maxPositionFieldString);
		SFVec3f *offset = (SFVec3f *)getExposedField(offsetFieldString);

		printStream << indentString << "\t" << "autoOffset " << autoOffset  << endl;
		printStream << indentString << "\t" << "enabled " << enabled  << endl;
		printStream << indentString << "\t" << "maxPosition " << maxpos  << endl;
		printStream << indentString << "\t" << "minPosition " << minpos  << endl;
		printStream << indentString << "\t" << "offset " << offset << endl;
	}
};

#endif

