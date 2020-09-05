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

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1996-1997
*
*	File:	SFVec2f.cpp
*
******************************************************************/

#include "SFVec2f.h"

#ifdef SUPPORT_JSAI

int			SFVec2f::mInit = 0;

jclass		SFVec2f::mFieldClassID = 0;
jclass		SFVec2f::mConstFieldClassID = 0;

jmethodID	SFVec2f::mInitMethodID = 0;
jmethodID	SFVec2f::mSetValueMethodID = 0;
jmethodID	SFVec2f::mGetXMethodID = 0;
jmethodID	SFVec2f::mGetYMethodID = 0;
jmethodID	SFVec2f::mSetNameMethodID = 0;

jmethodID	SFVec2f::mConstInitMethodID = 0;
jmethodID	SFVec2f::mConstSetValueMethodID = 0;
jmethodID	SFVec2f::mConstGetXMethodID = 0;
jmethodID	SFVec2f::mConstGetYMethodID = 0;
jmethodID	SFVec2f::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	SFVec2f::setJavaIDs
////////////////////////////////////////////////

void SFVec2f::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/SFVec2f");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstSFVec2f");

		assert(mFieldClassID && mConstFieldClassID);

		// MethodIDs
		jclass classid = getFieldID();
		mInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(FF)V");
		mGetXMethodID		= jniEnv->GetMethodID(classid, "getX", "()F");
		mGetYMethodID		= jniEnv->GetMethodID(classid, "getY", "()F");
		mSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(FF)V");
		mSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mInitMethodID && mGetXMethodID && mGetYMethodID && mSetValueMethodID && mSetNameMethodID);

		// Const MethodIDs
		classid = getConstFieldID();
		mConstInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(FF)V");
		mConstGetXMethodID		= jniEnv->GetMethodID(classid, "getX", "()F");
		mConstGetYMethodID		= jniEnv->GetMethodID(classid, "getY", "()F");
		mConstSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(FF)V");
		mConstSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mConstInitMethodID && mConstGetXMethodID && mConstGetYMethodID && mConstSetValueMethodID && mConstSetNameMethodID);

		mInit = 1;
	}
}

////////////////////////////////////////////////
//	SFVec2f::toJavaObject
////////////////////////////////////////////////

jobject SFVec2f::toJavaObject(int bConstField) {
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	initMethod		= bConstField ? getConstInitMethodID() : getInitMethodID();
	jfloat		x				= getX();
	jfloat		y				= getY();
	jobject		eventField		= jniEnv->NewObject(classid, initMethod, x, y);
	jmethodID	setNameMethod	= bConstField ? getConstSetNameMethodID() : getSetNameMethodID();

	char		*fieldName		= getName();
	jstring		jfieldName		= NULL;
	if (fieldName && strlen(fieldName))
		jfieldName = jniEnv->NewStringUTF(getName());
	jniEnv->CallVoidMethod(eventField, setNameMethod, jfieldName);
	if (jfieldName)
		jniEnv->DeleteLocalRef(jfieldName);
		
	return eventField;
}

////////////////////////////////////////////////
//	SFVec2f::setValue
////////////////////////////////////////////////

void SFVec2f::setValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	getXMethod		= bConstField ? getConstGetXMethodID() : getGetXMethodID();
	jmethodID	getYMethod		= bConstField ? getConstGetYMethodID() : getGetYMethodID();
	assert(classid && getXMethod && getYMethod);
	jfloat		x				= jniEnv->CallFloatMethod(field, getXMethod);
	jfloat		y				= jniEnv->CallFloatMethod(field, getYMethod);
	setValue(x, y);
}

////////////////////////////////////////////////
//	SFVec2f::getValue
////////////////////////////////////////////////

void SFVec2f::getValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	setValueMethod	= bConstField ? getConstSetValueMethodID() : getSetValueMethodID();
	assert(classid && setValueMethod);
	jfloat		x				= getX();
	jfloat		y				= getY();
	jniEnv->CallVoidMethod(field, setValueMethod, x, y);
}

#endif
