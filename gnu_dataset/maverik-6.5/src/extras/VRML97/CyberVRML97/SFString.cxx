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
*	File:	SFString.cpp
*
******************************************************************/

#include "SFString.h"

#ifdef SUPPORT_JSAI

int			SFString::mInit = 0;

jclass		SFString::mFieldClassID = 0;
jclass		SFString::mConstFieldClassID = 0;

jmethodID	SFString::mInitMethodID = 0;
jmethodID	SFString::mSetValueMethodID = 0;
jmethodID	SFString::mGetValueMethodID = 0;
jmethodID	SFString::mSetNameMethodID = 0;

jmethodID	SFString::mConstInitMethodID = 0;
jmethodID	SFString::mConstSetValueMethodID = 0;
jmethodID	SFString::mConstGetValueMethodID = 0;
jmethodID	SFString::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	SFString::setJavaIDs
////////////////////////////////////////////////

void SFString::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/SFString");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstSFString");

		assert(mFieldClassID && mConstFieldClassID);

		// MethodIDs
		jclass classid = getFieldID();
		mInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(Ljava/lang/String;)V");
		mGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()Ljava/lang/String;");
		mSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(Ljava/lang/String;)V");
		mSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mInitMethodID && mGetValueMethodID && mSetValueMethodID && mSetNameMethodID);

		// Const MethodIDs
		classid	 = getConstFieldID();
		mConstInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(Ljava/lang/String;)V");
		mConstGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()Ljava/lang/String;");
		mConstSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(Ljava/lang/String;)V");
		mConstSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mConstInitMethodID && mConstGetValueMethodID && mConstSetValueMethodID && mConstSetNameMethodID);

		mInit = 1;
	}
}

////////////////////////////////////////////////
//	SFString::toJavaObject
////////////////////////////////////////////////

jobject SFString::toJavaObject(int bConstField) {
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	initMethod		= bConstField ? getConstInitMethodID() : getInitMethodID();
	jstring		value			= NULL;
	char		*string			= getValue();
		
	if (string && strlen(string))
		value = jniEnv->NewStringUTF(string);
	jobject		eventField		= jniEnv->NewObject(classid, initMethod, value);
	jmethodID	setNameMethod	= bConstField ? getConstSetNameMethodID() : getSetNameMethodID();
	if (value)
		jniEnv->ReleaseStringUTFChars(value, jniEnv->GetStringUTFChars(value, NULL));

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
//	SFString::setValue
////////////////////////////////////////////////

void SFString::setValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	getValueMethod	= bConstField ? getConstGetValueMethodID() : getGetValueMethodID();
	assert(classid && getValueMethod);
	jstring		value			= (jstring)jniEnv->CallObjectMethod(field, getValueMethod);
	if (value) {
		const char	*string		= jniEnv->GetStringUTFChars(value, NULL);
		setValue((char *)string);
		jniEnv->ReleaseStringUTFChars(value, string);
		jniEnv->DeleteLocalRef(value);
	}
	else
		setValue((char *)NULL);

	jniEnv->DeleteLocalRef(classid);
}

////////////////////////////////////////////////
//	SFString::getValue
////////////////////////////////////////////////

void SFString::getValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	setValueMethod	= bConstField ? getConstSetValueMethodID() : getSetValueMethodID();
	assert(classid && setValueMethod);
	jstring		value			= NULL;
	char		*string			= getValue();
	if (string && strlen(string))
		value = jniEnv->NewStringUTF(string);
	jniEnv->CallVoidMethod(field, setValueMethod, value);
	if (value)
		jniEnv->DeleteLocalRef(value);
}

#endif
