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
*	File:	SFColor.cpp
*
******************************************************************/

#include "SFColor.h"

#ifdef SUPPORT_JSAI

int			SFColor::mInit = 0;

jclass		SFColor::mFieldClassID = 0;
jclass		SFColor::mConstFieldClassID = 0;

jmethodID	SFColor::mInitMethodID = 0;
jmethodID	SFColor::mSetValueMethodID = 0;
jmethodID	SFColor::mGetRedMethodID = 0;
jmethodID	SFColor::mGetGreenMethodID = 0;
jmethodID	SFColor::mGetBlueMethodID = 0;
jmethodID	SFColor::mSetNameMethodID = 0;

jmethodID	SFColor::mConstInitMethodID = 0;
jmethodID	SFColor::mConstSetValueMethodID = 0;
jmethodID	SFColor::mConstGetRedMethodID = 0;
jmethodID	SFColor::mConstGetGreenMethodID = 0;
jmethodID	SFColor::mConstGetBlueMethodID = 0;
jmethodID	SFColor::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	SFColor::setJavaIDs
////////////////////////////////////////////////

void SFColor::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/SFColor");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstSFColor");

		assert(mFieldClassID && mConstFieldClassID);

		// MethodIDs
		jclass classid = getFieldID();
		mInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(FFF)V");
		mGetRedMethodID		= jniEnv->GetMethodID(classid, "getRed", "()F");
		mGetGreenMethodID		= jniEnv->GetMethodID(classid, "getGreen", "()F");
		mGetBlueMethodID		= jniEnv->GetMethodID(classid, "getBlue", "()F");
		mSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(FFF)V");
		mSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mInitMethodID && mGetRedMethodID && mGetGreenMethodID && mGetBlueMethodID && mSetValueMethodID && mSetNameMethodID);

		// Const MethodIDs
		classid = getConstFieldID();
		mConstInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(FFF)V");
		mConstGetRedMethodID		= jniEnv->GetMethodID(classid, "getRed", "()F");
		mConstGetGreenMethodID		= jniEnv->GetMethodID(classid, "getGreen", "()F");
		mConstGetBlueMethodID		= jniEnv->GetMethodID(classid, "getBlue", "()F");
		mConstSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(FFF)V");
		mConstSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mConstInitMethodID && mConstGetRedMethodID && mConstGetGreenMethodID && mConstGetBlueMethodID && mConstSetValueMethodID && mConstSetNameMethodID);

		mInit = 1;
	}
}

////////////////////////////////////////////////
//	SFColor::toJavaObject
////////////////////////////////////////////////

jobject SFColor::toJavaObject(int bConstField) {
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	initMethod		= bConstField ? getConstInitMethodID() : getInitMethodID();
	jfloat		r				= getRed();
	jfloat		g				= getGreen();
	jfloat		b				= getBlue();
	jobject		eventField		= jniEnv->NewObject(classid, initMethod, r, g, b);
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
//	SFColor::setValue
////////////////////////////////////////////////

void SFColor::setValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	getRedMethod	= bConstField ? getConstGetRedMethodID() : getGetRedMethodID();
	jmethodID	getGreenMethod	= bConstField ? getConstGetGreenMethodID() : getGetGreenMethodID();
	jmethodID	getBlueMethod	= bConstField ? getConstGetBlueMethodID() : getGetBlueMethodID();
	assert(classid && getRedMethod && getGreenMethod && getBlueMethod);
	jfloat		r				= jniEnv->CallFloatMethod(field, getRedMethod);
	jfloat		g				= jniEnv->CallFloatMethod(field, getGreenMethod);
	jfloat		b				= jniEnv->CallFloatMethod(field, getBlueMethod);
	setValue(r, g, b);
}

////////////////////////////////////////////////
//	SFColor::getValue
////////////////////////////////////////////////

void SFColor::getValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	setValueMethod	= bConstField ? getConstSetValueMethodID() : getSetValueMethodID();
	assert(classid && setValueMethod);
	jfloat		r				= getRed();
	jfloat		g				= getGreen();
	jfloat		b				= getBlue();
	jniEnv->CallVoidMethod(field, setValueMethod, r, g, b);
}

#endif
