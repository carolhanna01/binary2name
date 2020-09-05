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
*	File:	SFTime.h
*
******************************************************************/

#ifndef _SFTIME_H_
#define _SFTIME_H_

#include <stdio.h>
#include <iostream.h>
#include "Field.h"

double GetCurrentSystemTime();

class SFTime : public Field {

	static	int	mInit;

	double mValue; 

public:

	SFTime() {
		setType(fieldTypeSFTime);
		setValue(-1);
		InitializeJavaIDs();
	}

	SFTime(double value) {
		setType(fieldTypeSFTime);
		setValue(value);
		InitializeJavaIDs();
	}

	SFTime(SFTime *value) {
		setType(fieldTypeSFTime);
		setValue(value);
		InitializeJavaIDs();
	}

	void InitializeJavaIDs() {
#ifdef SUPPORT_JSAI
		setJavaIDs();
#endif
	}

	void setValue(double value) {
		mValue = value;
	}

	void setValue(SFTime *value) {
		setValue(value->getValue());
	}

	double getValue() {
		return mValue;
	}

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	friend ostream& operator<<(ostream &s, SFTime &time) {
		return s << time.getValue();
	}

	friend ostream& operator<<(ostream &s, SFTime *time) {
		return s << time->getValue();
	}

	////////////////////////////////////////////////
	//	String
	////////////////////////////////////////////////

	void setValue(char *value) {
		if (!value)
			return;
		setValue(atof(value));
	}

	char *getValue(char *buffer, int bufferLen) {
		sprintf(buffer, "%f", getValue());
		return buffer;
	}

	////////////////////////////////////////////////
	//	Compare
	////////////////////////////////////////////////

	bool equals(Field *field) {
		SFTime *time = (SFTime *)field;
		if (getValue() == time->getValue())
			return true;
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

#endif
