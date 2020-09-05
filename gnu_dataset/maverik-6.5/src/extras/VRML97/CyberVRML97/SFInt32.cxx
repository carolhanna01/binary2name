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
*	File:	SFInt32.cpp
*
******************************************************************/

#include "SFInt32.h"

#ifdef SUPPORT_JSAI

int			SFInt32::mInit = 0;

jclass		SFInt32::mFieldClassID = 0;
jclass		SFInt32::mConstFieldClassID = 0;

jmethodID	SFInt32::mInitMethodID = 0;
jmethodID	SFInt32::mSetValueMethodID = 0;
jmethodID	SFInt32::mGetValueMethodID = 0;
jmethodID	SFInt32::mSetNameMethodID = 0;

jmethodID	SFInt32::mConstInitMethodID = 0;
jmethodID	SFInt32::mConstSetValueMethodID = 0;
jmethodID	SFInt32::mConstGetValueMethodID = 0;
jmethodID	SFInt32::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	SFInt32::setJavaIDs
////////////////////////////////////////////////

void SFInt32::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/SFInt32");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstSFInt32");

		assert(mFieldClassID && mConstFieldClassID);

		// MethodIDs
		jclass classid = getFieldID();
		mInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(I)V");
		mGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()I");
		mSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(I)V");
		mSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mInitMethodID && mGetValueMethodID && mSetValueMethodID && mSetNameMethodID);

		// MethodIDs
		classid	 = getConstFieldID();
		mConstInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(I)V");
		mConstGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()I");
		mConstSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(I)V");
		mConstSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mConstInitMethodID && mConstGetValueMethodID && mConstSetValueMethodID && mConstSetNameMethodID);

		mInit = 1;
	}
}


////////////////////////////////////////////////
//	SFInt32::toJavaObject
////////////////////////////////////////////////

jobject SFInt32::toJavaObject(int bConstField) {
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	initMethod		= bConstField ? getConstInitMethodID() : getInitMethodID();
	jint		value			= getValue();
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
//	SFInt32::setValue
////////////////////////////////////////////////

void SFInt32::setValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	getValueMethod	= bConstField ? getConstGetValueMethodID() : getGetValueMethodID();
	assert(classid && getValueMethod);
	jint		value			= jniEnv->CallIntMethod(field, getValueMethod);
	setValue(value);
}

////////////////////////////////////////////////
//	SFInt32::getValue
////////////////////////////////////////////////

void SFInt32::getValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	setValueMethod	= bConstField ? getConstSetValueMethodID() : getSetValueMethodID();
	assert(classid && setValueMethod);
	jint		value			= getValue();
	jniEnv->CallVoidMethod(field, setValueMethod, value);
}

#endif
