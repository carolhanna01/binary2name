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
*	File:	SFVec3f.cpp
*
******************************************************************/

#include "SFVec3f.h"
#include "SFRotation.h"

#ifdef SUPPORT_JSAI

int			SFVec3f::mInit = 0;

jclass		SFVec3f::mFieldClassID = 0;
jclass		SFVec3f::mConstFieldClassID = 0;

jmethodID	SFVec3f::mInitMethodID = 0;
jmethodID	SFVec3f::mSetValueMethodID = 0;
jmethodID	SFVec3f::mGetXMethodID = 0;
jmethodID	SFVec3f::mGetYMethodID = 0;
jmethodID	SFVec3f::mGetZMethodID = 0;
jmethodID	SFVec3f::mSetNameMethodID = 0;

jmethodID	SFVec3f::mConstInitMethodID = 0;
jmethodID	SFVec3f::mConstSetValueMethodID = 0;
jmethodID	SFVec3f::mConstGetXMethodID = 0;
jmethodID	SFVec3f::mConstGetYMethodID = 0;
jmethodID	SFVec3f::mConstGetZMethodID = 0;
jmethodID	SFVec3f::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	SFVec3f::setJavaIDs
////////////////////////////////////////////////

void SFVec3f::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/SFVec3f");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstSFVec3f");

		assert(mFieldClassID && mConstFieldClassID);

		// MethodIDs
		jclass classid = getFieldID();
		mInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(FFF)V");
		mGetXMethodID		= jniEnv->GetMethodID(classid, "getX", "()F");
		mGetYMethodID		= jniEnv->GetMethodID(classid, "getY", "()F");
		mGetZMethodID		= jniEnv->GetMethodID(classid, "getZ", "()F");
		mSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(FFF)V");
		mSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mInitMethodID && mGetXMethodID && mGetYMethodID && mGetZMethodID && mSetValueMethodID && mSetNameMethodID);

		// Const MethodIDs
		classid = getConstFieldID();
		mConstInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "(FFF)V");
		mConstGetXMethodID		= jniEnv->GetMethodID(classid, "getX", "()F");
		mConstGetYMethodID		= jniEnv->GetMethodID(classid, "getY", "()F");
		mConstGetZMethodID		= jniEnv->GetMethodID(classid, "getZ", "()F");
		mConstSetValueMethodID	= jniEnv->GetMethodID(classid, "setValue", "(FFF)V");
		mConstSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mConstInitMethodID && mConstGetXMethodID && mConstGetYMethodID && mConstGetZMethodID && mConstSetValueMethodID && mConstSetNameMethodID);

		mInit = 1;
	}
}

////////////////////////////////////////////////
//	SFVec3f::toJavaObject
////////////////////////////////////////////////

jobject SFVec3f::toJavaObject(int bConstField) {
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	initMethod		= bConstField ? getConstInitMethodID() : getInitMethodID();
	jfloat		x				= getX();
	jfloat		y				= getY();
	jfloat		z				= getZ();
	jobject		eventField		= jniEnv->NewObject(classid, initMethod, x, y, z);
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
//	SFVec3f::setValue
////////////////////////////////////////////////

void SFVec3f::setValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	getXMethod		= bConstField ? getConstGetXMethodID() : getGetXMethodID();
	jmethodID	getYMethod		= bConstField ? getConstGetYMethodID() : getGetYMethodID();
	jmethodID	getZMethod		= bConstField ? getConstGetZMethodID() : getGetZMethodID();
	assert(classid && getXMethod && getYMethod && getZMethod);
	jfloat		x				= jniEnv->CallFloatMethod(field, getXMethod);
	jfloat		y				= jniEnv->CallFloatMethod(field, getYMethod);
	jfloat		z				= jniEnv->CallFloatMethod(field, getZMethod);
	setValue(x, y, z);
}

////////////////////////////////////////////////
//	SFVec3f::getValue
////////////////////////////////////////////////

void SFVec3f::getValue(jobject field, int bConstField) {
	assert(field);
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	setValueMethod	= bConstField ? getConstSetValueMethodID() : getSetValueMethodID();
	assert(classid && setValueMethod);
	jfloat		x				= getX();
	jfloat		y				= getY();
	jfloat		z				= getZ();
	jniEnv->CallVoidMethod(field, setValueMethod, x, y, z);
}

#endif




////////////////////////////////////////////////
//	SFVec3f::rotate
////////////////////////////////////////////////

void SFVec3f::rotate(SFRotation *rotation) 
{
	rotation->multi(mValue);
}

void SFVec3f::rotate(float x, float y, float z, float angle) 
{
	SFRotation rotation(x, y, z, angle);
	rotate(&rotation);
}
