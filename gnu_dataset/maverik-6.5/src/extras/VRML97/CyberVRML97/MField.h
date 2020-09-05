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
*	File:	MFiled.h
*
******************************************************************/

#ifndef _MFIELD_H_
#define _MFIELD_H_

#include <iostream.h>

#include "Field.h" 
#include "JVector.h"

class MField : public Field {

	JVector<Field>	mFieldVector;

public:

	~MField() {
	}

	int getSize() {
		return mFieldVector.size();
	}

	void add(Field *object) {
		mFieldVector.addElement(object);
	}

	void insert(Field *object, int index) {
		mFieldVector.insertElementAt(object, index);
	}

	void clear() {
		mFieldVector.removeAllElements();
	}

	void remove(int index) {
		mFieldVector.removeElementAt(index);
	}

	void removeLastObject() {
		mFieldVector.removeElement(lastObject());
	}

	void removeFirstObject() {
		mFieldVector.removeElement(firstObject());
	}

	Field *lastObject() {
		return (Field *)mFieldVector.lastElement();
	}

	Field *firstObject() {
		return (Field *)mFieldVector.firstElement();
	}

	Field *getObject(int index) {
		return (Field *)mFieldVector.elementAt(index);
	}

	Field *getNextObject() { // JMC
	  return (Field *)mFieldVector.nextElement(); // JMC
	} // JMC

	void setObject(int index, Field *object) {
		mFieldVector.setElementAt(object, index);
	}

	void copy(MField *srcMField) {
		clear();
		for (int n=0; n<srcMField->getSize(); n++) {
			add(srcMField->getObject(n));
		}
	}

	void setValue(char *buffer);
	char *getValue(char *buffer, int bufferLen);

	virtual void setValue(MField *mfield) = 0;
	virtual void outputContext(ostream& printStream, char *indentString) = 0;

	void outputContext(ostream& printStream, char *indentString1, char *indentString2) {
		char *indentString = new char[strlen(indentString1)+strlen(indentString2)+1];
		strcpy(indentString, indentString1);
		strcat(indentString, indentString2);
		outputContext(printStream, indentString);
		delete indentString;
	};

	////////////////////////////////////////////////
	//	Java
	////////////////////////////////////////////////

#ifdef SUPPORT_JSAI

	virtual jobject toJavaObject(int bConstField = 0) = 0;
	virtual void setValue(jobject field, int bConstField = 0) = 0;
	virtual void getValue(jobject field, int bConstField = 0) = 0;

#endif
};

#endif
