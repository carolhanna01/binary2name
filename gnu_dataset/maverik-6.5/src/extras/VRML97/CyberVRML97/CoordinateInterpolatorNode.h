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
*	File:	CoordinateInterpolatorNode.h
*
******************************************************************/

#ifndef _COORDINATEINTERPOLATOR_H_
#define _COORDINATEINTERPOLATOR_H_

#include "vrmlfields.h"
#include "Node.h"

class CoordinateInterpolatorNode : public Node {

public:

	CoordinateInterpolatorNode() {
		setHeaderFlag(false);
		setType(coordinateInterpolatorNodeString);

		// key exposed field
		MFFloat *key = new MFFloat();
		addExposedField(keyFieldString, key);

		// keyValue exposed field
		MFVec3f *keyValue = new MFVec3f();
		addExposedField(keyValueFieldString, keyValue);

		// set_fraction eventIn field
		SFFloat *setFraction = new SFFloat(0.0f);
		addEventIn(fractionFieldString, setFraction);

		// value_changed eventOut field
		SFVec3f *valueChanged = new SFVec3f(0.0f, 0.0f, 0.0f);
		addEventOut(valueFieldString, valueChanged);
	}

	~CoordinateInterpolatorNode() {
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
	
	void addKeyValue(float vector[]) {
		MFVec3f *keyValue = (MFVec3f *)getExposedField(keyValueFieldString);
		keyValue->addValue(vector);
	}

	int getNKeyValues() {
		MFVec3f *keyValue = (MFVec3f *)getExposedField(keyValueFieldString);
		return keyValue->getSize();
	}
	
	void getKeyValue(int index, float vector[]) {
		MFVec3f *keyValue = (MFVec3f *)getExposedField(keyValueFieldString);
		keyValue->get1Value(index, vector);
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
	
	void setValue(float vector[]) {
		SFVec3f *value = (SFVec3f *)getEventOut(valueFieldString);
		value->setValue(vector);
	}

	void getValue(float vector[]) {
		SFVec3f *value = (SFVec3f *)getEventOut(valueFieldString);
		value->getValue(vector);
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
		int n;

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

		float vector1[3];
		float vector2[3];
		float vectorOut[3];

		getKeyValue(index, vector1);
		getKeyValue(index+1, vector2);
		for (n=0; n<3; n++)
			vectorOut[n] = vector1[n] + (vector2[n] - vector1[n])*scale;

		setValue(vectorOut);
		sendEvent(getValueField());
	}

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		if (0 < getNKeys()) {
			MFFloat *key = (MFFloat *)getExposedField(keyFieldString);
			printStream << indentString << "\tkey [" << endl;
			key->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t]" << endl;
		}

		if (0 < getNKeyValues()) {
			MFVec3f *keyValue = (MFVec3f *)getExposedField(keyValueFieldString);
			printStream << indentString << "\tkeyValue [" << endl;
			keyValue->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t]" << endl;
		}
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	CoordinateInterpolatorNode *next() {
		return (CoordinateInterpolatorNode *)Node::next(getType());
	}

	CoordinateInterpolatorNode *nextTraversal() {
		return (CoordinateInterpolatorNode *)Node::nextTraversalByType(getType());
	}

};

#endif

