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
*	File:	MFVec2f.h
*
******************************************************************/

#ifndef _MFVEC2F_H_
#define _MFVEC2F_H_

#include "MField.h"
#include "SFVec2f.h"

class MFVec2f : public MField {

	static	int	mInit;

public:

	MFVec2f() {
		setType(fieldTypeMFVec2f);
		InitializeJavaIDs();
	}

	void InitializeJavaIDs() {
#ifdef SUPPORT_JSAI
		setJavaIDs();
#endif
	}

	void addValue(float x, float y) {
		SFVec2f *vector = new SFVec2f(x, y);
		add(vector);
	}

	void addValue(float value[]) {
		SFVec2f *vector = new SFVec2f(value);
		add(vector);
	}

	void addValue(SFVec2f *vector) {
		add(vector);
	}

	void insertValue(int index, float x, float y) {
		SFVec2f *vector = new SFVec2f(x, y);
		insert(vector, index);
	}

	void insertValue(int index, float value[]) {
		SFVec2f *vector = new SFVec2f(value);
		insert(vector, index);
	}

	void insertValue(int index, SFVec2f *vector) {
		insert(vector, index);
	}

	void get1Value(int index, float value[]) {
		SFVec2f *vector = (SFVec2f *)getObject(index);
		if (vector)
			vector->getValue(value);
	}
	void get1ValueNext(float value[]) { // JMC
	  SFVec2f *vector = (SFVec2f *)getNextObject(); // JMC
	  if (vector) // JMC
	    vector->getValue(value); // JMC
	}

	void set1Value(int index, float value[]) {
		SFVec2f *vector = (SFVec2f *)getObject(index);
		if (vector)
			vector->setValue(value);
	}

	void setValue(MField *mfield);
	void setValue(MFVec2f *vectors);
	void setValue(int size, float vectors[][2]);

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	void outputContext(ostream& printStream, char *indentString) {
		float value[2];
		for (int n=0; n<getSize(); n++) {
			get1Value(n, value);
			if (n < getSize()-1)
				printStream << indentString << value[0] << " " << value[1] << "," << endl;
			else	
				printStream << indentString << value[0] << " " << value[1] << endl;
		}
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
