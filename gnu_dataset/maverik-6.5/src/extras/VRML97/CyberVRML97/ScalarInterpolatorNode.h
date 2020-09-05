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
*	File:	ScalarInterpolatorNode.h
*
******************************************************************/

#ifndef _SCALARINTERPOLATOR_H_
#define _SCALARINTERPOLATOR_H_

#include "vrmlfields.h"
#include "Node.h"

class ScalarInterpolatorNode : public Node {

public:

	ScalarInterpolatorNode() {
		setHeaderFlag(false);
		setType(scalarInterpolatorNodeString);

		// key exposed field
		MFFloat *key = new MFFloat();
		addExposedField(keyFieldString, key);

		// keyValue exposed field
		MFFloat *keyValue = new MFFloat();
		addExposedField(keyValueFieldString, keyValue);

		// set_fraction eventIn field
		SFFloat *setFraction = new SFFloat(0.0f);
		addEventIn(fractionFieldString, setFraction);

		// value_changed eventOut field
		SFFloat *valueChanged = new SFFloat(0.0f);
		addEventOut(valueFieldString, valueChanged);
	}

	~ScalarInterpolatorNode() {
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
	
	void addKeyValue(float value) {
		MFFloat *keyValue = (MFFloat *)getExposedField(keyValueFieldString);
		keyValue->addValue(value);
	}

	int getNKeyValues() {
		MFFloat *keyValue = (MFFloat *)getExposedField(keyValueFieldString);
		return keyValue->getSize();
	}
	
	float getKeyValue(int index) {
		MFFloat *keyValue = (MFFloat *)getExposedField(keyValueFieldString);
		return keyValue->get1Value(index);
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

	Field *getValueField() {
		return getEventOut(valueFieldString);
	}

	////////////////////////////////////////////////
	//	value
	////////////////////////////////////////////////
	
	void setValue(float vector) {
		SFFloat *value = (SFFloat *)getEventOut(valueFieldString);
		value->setValue(vector);
	}

	float getValue() {
		SFFloat *value = (SFFloat *)getEventOut(valueFieldString);
		return value->getValue();
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

		float fraction = getFraction();
		int index = -1;
		int nKey = getNKeys();
		for (int n=0; n<(nKey-1); n++) {
			if (getKey(n) <= fraction && fraction <= getKey(n+1)) {
				index = n;
				break;
			}
		}
		if (index == -1)
			return;

		float scale = (fraction - getKey(index)) / (getKey(index+1) - getKey(index));


		float value1 = getKeyValue(index);
		float value2 = getKeyValue(index+1);
		float valueOut = value1 + (value2 - value1)*scale;

		setValue(valueOut);
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
			MFFloat *keyValue = (MFFloat *)getExposedField(keyValueFieldString);
			printStream << indentString << "\tkeyValue [" << endl;
			keyValue->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t]" << endl;
		}
	}

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	ScalarInterpolatorNode *next() {
		return (ScalarInterpolatorNode *)Node::next(getType());
	}

	ScalarInterpolatorNode *nextTraversal() {
		return (ScalarInterpolatorNode *)Node::nextTraversalByType(getType());
	}
};

#endif
