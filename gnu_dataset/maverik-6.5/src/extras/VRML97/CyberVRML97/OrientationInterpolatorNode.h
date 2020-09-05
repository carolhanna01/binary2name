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
*	File:	OrientationInterpolatorNode.h
*
******************************************************************/

#ifndef _ORIENTATIONINTERPOLATOR_H_
#define _ORIENTATIONINTERPOLATOR_H_

#include "vrmlfields.h"
#include "Node.h"

class OrientationInterpolatorNode : public Node {

public:

	OrientationInterpolatorNode() {
		setHeaderFlag(false);
		setType(orientationInterpolatorNodeString);

		// key exposed field
		MFFloat *key = new MFFloat();
		addExposedField(keyFieldString, key);

		// keyValue exposed field
		MFRotation *keyValue = new MFRotation();
		addExposedField(keyValueFieldString, keyValue);

		// set_fraction eventIn field
		SFFloat *setFraction = new SFFloat(0.0f);
		addEventIn(fractionFieldString, setFraction);

		// value_changed eventOut field
		SFRotation *valueChanged = new SFRotation(0.0f, 0.0f, 1.0f, 0.0f);
		addEventOut(valueFieldString, valueChanged);
	}

	~OrientationInterpolatorNode() {
	}

	////////////////////////////////////////////////
	//	key
	////////////////////////////////////////////////
	
	void addKey(float value) {
		MFFloat *key = (MFFloat *)getExposedField(keyFieldString);
		key->addValue(value);
	}
	int getNKeys() {
		MFFloat *key = (MFFloat *)getExposedField(keyFieldString);
		return key->getSize();
	}
	float getKey(int index) {
		MFFloat *key = (MFFloat *)getExposedField(keyFieldString);
		return key->get1Value(index);
	}
	Field *getKeyField() {
		return getExposedField(keyFieldString);
	}

	////////////////////////////////////////////////
	//	keyValue
	////////////////////////////////////////////////
	
	void addKeyValue(float rotation[]) {
		MFRotation *keyValue = (MFRotation *)getExposedField(keyValueFieldString);
		keyValue->addValue(rotation);
	}

	int getNKeyValues() {
		MFRotation *keyValue = (MFRotation *)getExposedField(keyValueFieldString);
		return keyValue->getSize();
	}
	
	void getKeyValue(int index, float rotation[]) {
		MFRotation *keyValue = (MFRotation *)getExposedField(keyValueFieldString);
		keyValue->get1Value(index, rotation);
	}

	Field *getKeyValueField() {
		return getExposedField(keyValueFieldString);
	}

	////////////////////////////////////////////////
	//	fraction
	////////////////////////////////////////////////
	
	void setFraction(float value) {
		SFFloat *fraction = (SFFloat *)getEventIn(fractionFieldString);
		fraction->setValue(value);
	}

	float getFraction() {
		SFFloat *fraction = (SFFloat *)getEventIn(fractionFieldString);
		return fraction->getValue();
	}

	Field *getFractionField() {
		return getEventIn(fractionFieldString);
	}

	////////////////////////////////////////////////
	//	value
	////////////////////////////////////////////////
	
	void setValue(float rotation[]) {
		SFRotation *value = (SFRotation *)getEventOut(valueFieldString);
		value->setValue(rotation);
	}

	void getValue(float rotation[]) {
		SFRotation *value = (SFRotation *)getEventOut(valueFieldString);
		value->getValue(rotation);
	}

	Field *getValueField() {
		return getEventOut(valueFieldString);
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
		int	n;

		float fraction = getFraction();
		int index = -1;
		int nKey = getNKeys();
		for (n=0; n<(nKey-1); n++) {
			if (getKey(n) <= fraction && fraction <= getKey(n+1)) {
				index = n;
				break;
			}
		}
		if (index == -1)
			return;

		float scale = (fraction - getKey(index)) / (getKey(index+1) - getKey(index));

		float rotation1[4];
		float rotation2[4];
		float rotationOut[4];

		getKeyValue(index, rotation1);
		getKeyValue(index+1, rotation2);
		for (n=0; n<4; n++)
			rotationOut[n] = rotation1[n] + (rotation2[n] - rotation1[n])*scale;

		setValue(rotationOut);
		sendEvent(getValueField());
	}

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	void outputContext(ostream& printStream, String indentString) {
		if (0 < getNKeys()) {
			MFFloat *key = (MFFloat *)getExposedField(keyFieldString);
			printStream << indentString << "\tkey [" << endl;
			key->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t]" << endl;
		}

		if (0 < getNKeyValues()) {
			MFRotation *keyValue = (MFRotation *)getExposedField(keyValueFieldString);
			printStream << indentString << "\tkeyValue [" << endl;
			keyValue->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t]" << endl;
		}
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	OrientationInterpolatorNode *next() {
		return (OrientationInterpolatorNode *)Node::next(getType());
	}

	OrientationInterpolatorNode *nextTraversal() {
		return (OrientationInterpolatorNode *)Node::nextTraversalByType(getType());
	}

};

#endif

