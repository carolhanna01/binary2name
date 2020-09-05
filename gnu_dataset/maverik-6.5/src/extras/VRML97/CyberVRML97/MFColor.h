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
*	File:	MFColor.h
*
******************************************************************/

#ifndef _MFCOLOR_H_
#define _MFCOLOR_H_

#include "MField.h"
#include "SFColor.h"

class MFColor : public MField {

	static	int	mInit;

public:

	MFColor() {
		setType(fieldTypeMFColor);
		InitializeJavaIDs();
	}

	void InitializeJavaIDs() {
#ifdef SUPPORT_JSAI
		setJavaIDs();
#endif
	}

	void addValue(float r, float g, float b) {
		SFColor *color = new SFColor(r, g, b);
		add(color);
	}

	void addValue(float value[]) {
		SFColor *color = new SFColor(value);
		add(color);
	}

	void addValue(SFColor *color) {
		add(color);
	}

	void insertValue(int index, float r, float g, float b) {
		SFColor *color = new SFColor(r, g, b);
		insert(color, index);
	}

	void insertValue(int index, float value[]) {
		SFColor *color = new SFColor(value);
		insert(color, index);
	}

	void insertValue(int index, SFColor *color) {
		insert(color, index);
	}

	void get1Value(int index, float value[]) {
		SFColor *color = (SFColor *)getObject(index);
		if (color)
			color->getValue(value);
	}

	void set1Value(int index, float value[]) {
		SFColor *color = (SFColor *)getObject(index);
		if (color)
			color->setValue(value);
	}

	void setValue(MField *mfield);
	void setValue(MFColor *colors);
	void setValue(int size, float colors[][3]);

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	void outputContext(ostream& printStream, char *indentString) {
		float value[3];
		for (int n=0; n<getSize(); n++) {
			get1Value(n, value);
			if (n < getSize()-1)
				printStream << indentString << value[0] << " " << value[1] << " " << value[2] << "," << endl;
			else	
				printStream << indentString << value[0] << " " << value[1] << " " << value[2] << endl;
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

#endif //JMC
