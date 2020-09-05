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
*	File:	MFRotation.cpp
*
******************************************************************/

#include "MFRotation.h"

#ifdef SUPPORT_JSAI

int			MFRotation::mInit = 0;

jclass		MFRotation::mFieldClassID = 0;
jclass		MFRotation::mConstFieldClassID = 0;

jmethodID	MFRotation::mInitMethodID = 0;
jmethodID	MFRotation::mSetValueMethodID = 0;
jmethodID	MFRotation::mGetValueMethodID = 0;
jmethodID	MFRotation::mSetNameMethodID = 0;

jmethodID	MFRotation::mConstInitMethodID = 0;
jmethodID	MFRotation::mConstSetValueMethodID = 0;
jmethodID	MFRotation::mConstGetValueMethodID = 0;
jmethodID	MFRotation::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	MFRotation::setJavaIDs
////////////////////////////////////////////////

void MFRotation::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/MFRotation");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstMFRotation");

		assert(mFieldClassID && mConstFieldClassID);

		// MethodIDs
		jclass classid		= getFieldID();
		mInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "()V");
		mSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mInitMethodID && mSetNameMethodID);

		// Const MethodIDs
		classid = getConstFieldID();
		mConstInitMethodID		= jniEnv->GetMethodID(classid, "<init>", "()V");
		mConstSetNameMethodID	= jniEnv->GetMethodID(classid, "setName", "(Ljava/lang/String;)V");

		assert(mConstInitMethodID && mConstSetNameMethodID);

		mInit = 1;
	}
}

////////////////////////////////////////////////
//	MFRotation::toJavaObject
////////////////////////////////////////////////

jobject MFRotation::toJavaObject(int bConstField) {
	JNIEnv		*jniEnv			= getJniEnv();
	jclass		classid			= bConstField ? getConstFieldID() : getFieldID();
	jmethodID	initMethod		= bConstField ? getConstInitMethodID() : getInitMethodID();
	jobject		eventField		= jniEnv->NewObject(classid, initMethod);
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
//	MFRotation::setValue
////////////////////////////////////////////////

void MFRotation::setValue(jobject field, int bConstField) {
}

////////////////////////////////////////////////
//	MFRotation::getValue
////////////////////////////////////////////////

void MFRotation::getValue(jobject field, int bConstField) {
}

#endif 





////////////////////////////////////////////////
//	MFRotation::setValue
////////////////////////////////////////////////

void MFRotation::setValue(MFRotation *rotations)
{
	clear();

	float value[3];
	int size = rotations->getSize();
	for (int n=0; n<size; n++) {
		rotations->get1Value(n, value);
		addValue(value);
	}
}

void MFRotation::setValue(MField *mfield)
{
	if (mfield->getType() == fieldTypeMFRotation)
		setValue((MFRotation *)mfield);
}

void MFRotation::setValue(int size, float rotations[][4])
{
	clear();

	for (int n=0; n<size; n++)
		addValue(rotations[n]);
}

