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
*	File:	SFFloat.cpp
*
******************************************************************/

#include "SFFloat.h"

#ifdef SUPPORT_JSAI

int			SFFloat::mInit = 0;

jclass		SFFloat::mFieldClassID = 0;
jclass		SFFloat::mConstFieldClassID = 0;

jmethodID	SFFloat::mInitMethodID = 0;
jmethodID	SFFloat::mSetValueMethodID = 0;
jmethodID	SFFloat::mGetValueMethodID = 0;
jmethodID	SFFloat::mSetNameMethodID = 0;

jmethodID	SFFloat::mConstInitMethodID = 0;
jmethodID	SFFloat::mConstSetValueMethodID = 0;
jmethodID	SFFloat::mConstGetValueMethodID = 0;
jmethodID	SFFloat::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	SFFloat::toJavaObject
////////////////////////////////////////////////

void SFFloat::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/SFFloat");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstSFFloat");

		assert(mFieldClassID && mConstFieldClassID);

		// MethodIDs
		jclass classid = getFieldID();
		mInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(F)V");
		mGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()F");
		mSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(F)V");
		mSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mInitMethodID && mGetValueMethodID && mSetValueMethodID && mSetNameMethodID);

		// MethodIDs
		classid	 = getConstFieldID();
		mConstInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(F)V");
		mConstGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()F");
		mConstSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(F)V");
		mConstSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mConstInitMethodID && mConstGetValueMethodID && mConstSetValueMethodID && mConstSetNameMethodID);

		mInit = 1;
	}
}

////////////////////////////////////////////////
//	SFFloat::toJavaObject
////////////////////////////////////////////////

jobject SFFloat::toJavaObject(int bConstField) {
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	initMethod		= bConstField ? getConstInitMethodID() : getInitMethodID();
	jfloat		value			= getValue();
	jobject		eventField		= jniEnv->NewObject(classid, initMethod, value);
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
//	SFFloat::setValue
////////////////////////////////////////////////

void SFFloat::setValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	getValueMethod	= bConstField ? getConstGetValueMethodID() : getGetValueMethodID();
	assert(classid && getValueMethod);
	jfloat		value			= jniEnv->CallFloatMethod(field, getValueMethod);
	setValue(value);
}

////////////////////////////////////////////////
//	SFFloat::getValue
////////////////////////////////////////////////

void SFFloat::getValue(jobject field, int bConstField){
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	setValueMethod	= bConstField ? getConstSetValueMethodID() : getSetValueMethodID();
	assert(classid && setValueMethod);
	jfloat		value			= getValue();
	jniEnv->CallVoidMethod(field, setValueMethod, value);
}

#endif
