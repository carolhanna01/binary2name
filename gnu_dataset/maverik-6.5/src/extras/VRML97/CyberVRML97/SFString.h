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
*	File:	SFString.h
*
******************************************************************/

#ifndef _SFSTRING_H_
#define _SFSTRING_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include "Field.h"
#include "JString.h"

class SFString : public Field {

	static	int	mInit;

	JString	mValue;

public:

	SFString() {
		setType(fieldTypeSFString);
		setValue((char *)NULL);
		InitializeJavaIDs();
	}

	SFString(char *value) {
		setType(fieldTypeSFString);
		setValue(value);
		InitializeJavaIDs();
	}

	SFString(SFString *value) {
		setType(fieldTypeSFString);
		setValue(value);
		InitializeJavaIDs();
	}

	void InitializeJavaIDs() {
#ifdef SUPPORT_JSAI
		setJavaIDs();
#endif
	}

	~SFString() {
	}

	void setValue(char *value) {
		mValue.setValue(value);
	}

	void setValue(SFString *value) {
		mValue.setValue(value->getValue());
	}

	char *getValue() {
		return mValue.getValue();
	}

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	friend ostream& operator<<(ostream &s, SFString &string) {
		if (string.getValue())
			return s << "\"" << string.getValue() << "\"";
		else
			return s << "\"" << "\"";
	}

	friend ostream& operator<<(ostream &s, SFString *string) {
		if (string->getValue())
			return s << "\"" << string->getValue() << "\"";
		else
			return s << "\"" << "\"";
	}

	////////////////////////////////////////////////
	//	String
	////////////////////////////////////////////////

	char *getValue(char *buffer, int bufferLen) {
		sprintf(buffer, "%s", getValue());
		return buffer;
	}

	////////////////////////////////////////////////
	//	Compare
	////////////////////////////////////////////////

	bool equals(Field *field) {
		SFString *stringField = (SFString *)field;

		if (!getValue() && !stringField->getValue())
			return true;

		if (getValue() && stringField->getValue())
			return (!strcmp(getValue(), stringField->getValue()) ? true : false);
		else
			return false;
	}

	////////////////////////////////////////////////
	//	Java
	////////////////////////////////////////////////

#ifdef SUPPORT_JSAI

private:

	static jclass		mFieldClassID;
	static jclass		mConstFieldClassID;

	static jmethodID	mInitMethodID;
	static jmethodID	mSetValueMethodID;
	static jmethodID	mGetValueMethodID;
	static jmethodID	mSetNameMethodID;

	static jmethodID	mConstInitMethodID;
	static jmethodID	mConstSetValueMethodID;
	static jmethodID	mConstGetValueMethodID;
	static jmethodID	mConstSetNameMethodID;

public:

	void		setJavaIDs();

	jclass		getFieldID()				{return mFieldClassID;}
	jclass		getConstFieldID()			{return mConstFieldClassID;}

	jmethodID	getInitMethodID()			{return mInitMethodID;}
	jmethodID	getSetValueMethodID()		{return mSetValueMethodID;}
	jmethodID	getGetValueMethodID()		{return mGetValueMethodID;}
	jmethodID	getSetNameMethodID()		{return mSetNameMethodID;}

	jmethodID	getConstInitMethodID()		{return mConstInitMethodID;}
	jmethodID	getConstSetValueMethodID()	{return mConstSetValueMethodID;}
	jmethodID	getConstGetValueMethodID()	{return mConstGetValueMethodID;}
	jmethodID	getConstSetNameMethodID()	{return mConstSetNameMethodID;}

	jobject toJavaObject(int bConstField = 0);
	void setValue(jobject field, int bConstField = 0);
	void getValue(jobject field, int bConstField = 0);

#endif

};

#endif //JMC
