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
*	File:	NavigationInfoNode.h
*
******************************************************************/

#ifndef _NAVIGATIONINFO_H_
#define _NAVIGATIONINFO_H_

#include "BindableNode.h"

class NavigationInfoNode : public BindableNode {
	
public:

	NavigationInfoNode() {
		setHeaderFlag(false);
		setType(navigationInfoNodeString);

		///////////////////////////
		// Exposed Field 
		///////////////////////////

		// visibilityLimit exposed field
		SFFloat *visibilityLimit = new SFFloat(0.0f);
		addExposedField(visibilityLimitFieldString, visibilityLimit);

		// avatarSize exposed field
		MFFloat *avatarSize = new MFFloat();
		addExposedField(avatarSizeFieldString, avatarSize);

		// type exposed field
		MFString *type = new MFString();
		addExposedField(typeFieldString, type);

		// headlight exposed field
		SFBool *headlight = new SFBool(false);
		addExposedField(headlightFieldString, headlight);

		// speed exposed field
		SFFloat *speed = new SFFloat(1.0f);
		addExposedField(speedFieldString, speed);
	}

	~NavigationInfoNode() {
	}

	////////////////////////////////////////////////
	// Type
	////////////////////////////////////////////////

	void addType(String value) {
		MFString *type = (MFString *)getExposedField(typeFieldString);
		type->addValue(value);
	}
	int getNTypes() {
		MFString *type = (MFString *)getExposedField(typeFieldString);
		return type->getSize();
	}
	String getType(int index) {
		MFString *type = (MFString *)getExposedField(typeFieldString);
		return type->get1Value(index);
	}

	////////////////////////////////////////////////
	// avatarSize
	////////////////////////////////////////////////

	void addAvatarSize(float value) {
		MFFloat *avatarSize = (MFFloat *)getExposedField(avatarSizeFieldString);
		avatarSize->addValue(value);
	}
	int getNAvatarSizes() {
		MFFloat *avatarSize = (MFFloat *)getExposedField(avatarSizeFieldString);
		return avatarSize->getSize();
	}
	float getAvatarSize(int index) {
		MFFloat *avatarSize = (MFFloat *)getExposedField(avatarSizeFieldString);
		return avatarSize->get1Value(index);
	}

	////////////////////////////////////////////////
	//	Headlight
	////////////////////////////////////////////////
	
	void setHeadlight(bool value) {
		SFBool *headlight = (SFBool *)getExposedField(headlightFieldString);
		headlight->setValue(value);
	}
	void setHeadlight(int value) {
		setHeadlight(value ? true : false);
	}
	bool getHeadlight() {
		SFBool *headlight = (SFBool *)getExposedField(headlightFieldString);
		return headlight->getValue();
	}

	////////////////////////////////////////////////
	//	VisibilityLimit
	////////////////////////////////////////////////

	void setVisibilityLimit(float value) {
		SFFloat *visibilityLimit = (SFFloat *)getExposedField(visibilityLimitFieldString);
		visibilityLimit->setValue(value);
	}
	float getVisibilityLimit() {
		SFFloat *visibilityLimit = (SFFloat *)getExposedField(visibilityLimitFieldString);
		return visibilityLimit->getValue();
	}

	////////////////////////////////////////////////
	//	Speed
	////////////////////////////////////////////////
	
	void setSpeed(float value) {
		SFFloat *time = (SFFloat *)getExposedField(speedFieldString);
		time->setValue(value);
	}
	float getSpeed() {
		SFFloat *time = (SFFloat *)getExposedField(speedFieldString);
		return time->getValue();
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	bool isChildNodeType(Node *node){
		return false;
	}

	NavigationInfoNode *next() {
		return (NavigationInfoNode *)Node::next(Node::getType());
	}

	NavigationInfoNode *nextTraversal() {
		return (NavigationInfoNode *)Node::nextTraversalByType(Node::getType());
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	void initialize() {
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *headlight = (SFBool *)getExposedField(headlightFieldString);

		printStream << indentString << "\t" << "visibilityLimit " << getVisibilityLimit() << endl;
		printStream << indentString << "\t" << "headlight " << headlight << endl;
		printStream << indentString << "\t" << "speed " << getSpeed() << endl;

		if (0 < getNTypes()) {
			MFString *type = (MFString *)getExposedField(typeFieldString);
			printStream << indentString << "\t" << "type [" << endl;
			type->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNAvatarSizes()) {
			MFFloat *avatarSize = (MFFloat *)getExposedField(avatarSizeFieldString);
			printStream << indentString << "\t" << "avatarSize [" << endl;
			avatarSize->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}
};

#endif

