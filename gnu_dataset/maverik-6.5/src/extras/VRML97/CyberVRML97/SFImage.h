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
*	File:	SFImage.h
*
******************************************************************/

#ifndef _SFIMAGE_H_
#define _SFIMAGE_H_

#include "MField.h"
#include "SFInt32.h"

class SFImage : public MField {

	static	int	mInit;

public:

	SFImage() {
		setType(fieldTypeSFImage);
		InitializeJavaIDs();
	}

	void InitializeJavaIDs() {
#ifdef SUPPORT_JSAI
		setJavaIDs();
#endif
	}

	void addValue(int value) {
		SFInt32 *sfvalue = new SFInt32(value);
		add(sfvalue);
	}

	void addValue(SFInt32 *sfvalue) {
		add(sfvalue);
	}

	void insertValue(int index, int value) {
		SFInt32 *sfvalue = new SFInt32(value);
		insert(sfvalue, index);
	}

	int get1Value(int index) {
		SFInt32 *sfvalue = (SFInt32 *)getObject(index);
		return sfvalue->getValue();
	}

	int get1ValueNext() { // JMC
	  SFInt32 *sfvalue = (SFInt32 *)getNextObject(); // JMC
	  if (sfvalue) //JMC
	    return sfvalue->getValue(); //JMC
	  else // JMC
	    return 0; // JMC
	} // JMC

	void set1Value(int index, int value) {
		SFInt32 *sfvalue = (SFInt32 *)getObject(index);
		sfvalue->setValue(value);
	}

	void setValue(MField *mfield) {
	}

	////////////////////////////////////////////////
	//	Output
	////////////////////////////////////////////////

	void outputContext(ostream& printStream, char *indentString) {
		int	nOutput = 0;
		printStream << indentString;
		for (int n=0; n<getSize(); n++) {
			printStream << get1Value(n) << " ";
			nOutput++;
			if (32 < nOutput) {
				printStream << endl;
				printStream << indentString;
				nOutput = 0;
			}
		}
		printStream << endl;
	}

	////////////////////////////////////////////////
	//	toString
	////////////////////////////////////////////////

	void setValue(char *value) {
	}

	char *getValue(char *buffer, int bufferLen) {
		buffer[0] = '\0';
		return buffer;
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
