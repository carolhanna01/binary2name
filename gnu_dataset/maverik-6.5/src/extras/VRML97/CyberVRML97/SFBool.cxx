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
*	File:	SFBool.cpp
*
******************************************************************/

#include "SFBool.h"

#ifdef SUPPORT_JSAI

int			SFBool::mInit = 0;

jclass		SFBool::mFieldClassID = 0;
jclass		SFBool::mConstFieldClassID = 0;

jmethodID	SFBool::mInitMethodID = 0;
jmethodID	SFBool::mSetValueMethodID = 0;
jmethodID	SFBool::mGetValueMethodID = 0;
jmethodID	SFBool::mSetNameMethodID = 0;

jmethodID	SFBool::mConstInitMethodID = 0;
jmethodID	SFBool::mConstSetValueMethodID = 0;
jmethodID	SFBool::mConstGetValueMethodID = 0;
jmethodID	SFBool::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	SFBool::toJavaObject
////////////////////////////////////////////////

void SFBool::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/SFBool");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstSFBool");

		assert(mFieldClassID && mConstFieldClassID);

		// MethodIDs
		jclass classid = getFieldID();
		mInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(Z)V");
		mGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()Z");
		mSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(Z)V");
		mSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mInitMethodID && mGetValueMethodID && mSetValueMethodID && mSetNameMethodID);

		// MethodIDs
		classid	 = getConstFieldID();
		mConstInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(Z)V");
		mConstGetValueMethodID	= jniEnv->GetMethodID(classid, "getValue", "()Z");
		mConstSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(Z)V");
		mConstSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mConstInitMethodID && mConstGetValueMethodID && mConstSetValueMethodID && mConstSetNameMethodID);

		mInit = 1;
	}
}

////////////////////////////////////////////////
//	SFBool::toJavaObject
////////////////////////////////////////////////

jobject SFBool::toJavaObject(int bConstField) {
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	initMethod		= bConstField ? getConstInitMethodID() : getInitMethodID();
	jboolean	value			= getValue();
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
//	SFBool::setValue
////////////////////////////////////////////////

void SFBool::setValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	getValueMethod	= bConstField ? getConstGetValueMethodID() : getGetValueMethodID();
	assert(classid && getValueMethod);
	jboolean	value			= jniEnv->CallBooleanMethod(field, getValueMethod);
	setValue(value ? true : false);
}

////////////////////////////////////////////////
//	SFBool::getValue
////////////////////////////////////////////////

void SFBool::getValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	setValueMethod	= bConstField ? getConstSetValueMethodID() : getSetValueMethodID();
	assert(classid && setValueMethod);
	jboolean	value			= getValue();
	jniEnv->CallVoidMethod(field, setValueMethod, value);
}

#endif
