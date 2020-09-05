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
*	File:	MFVec2f.cpp
*
******************************************************************/

#include "MFVec2f.h"

#ifdef SUPPORT_JSAI

int			MFVec2f::mInit = 0;

jclass		MFVec2f::mFieldClassID = 0;
jclass		MFVec2f::mConstFieldClassID = 0;

jmethodID	MFVec2f::mInitMethodID = 0;
jmethodID	MFVec2f::mSetValueMethodID = 0;
jmethodID	MFVec2f::mGetValueMethodID = 0;
jmethodID	MFVec2f::mSetNameMethodID = 0;

jmethodID	MFVec2f::mConstInitMethodID = 0;
jmethodID	MFVec2f::mConstSetValueMethodID = 0;
jmethodID	MFVec2f::mConstGetValueMethodID = 0;
jmethodID	MFVec2f::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	MFVec2f::setJavaIDs
////////////////////////////////////////////////

void MFVec2f::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/MFVec2f");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstMFVec2f");

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
//	MFVec2f::toJavaObject
////////////////////////////////////////////////

jobject MFVec2f::toJavaObject(int bConstField) {
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
//	MFVec2f::setValue
////////////////////////////////////////////////

void MFVec2f::setValue(jobject field, int bConstField) {
}

////////////////////////////////////////////////
//	MFVec2f::getValue
////////////////////////////////////////////////

void MFVec2f::getValue(jobject field, int bConstField) {
}

#endif 





////////////////////////////////////////////////
//	MFVec2f::setValue
////////////////////////////////////////////////

void MFVec2f::setValue(MFVec2f *vectors)
{
	clear();

	float value[3];
	int size = vectors->getSize();
	for (int n=0; n<size; n++) {
		vectors->get1Value(n, value);
		addValue(value);
	}
}

void MFVec2f::setValue(MField *mfield)
{
	if (mfield->getType() == fieldTypeMFVec2f)
		setValue((MFVec2f *)mfield);
}

void MFVec2f::setValue(int size, float vectors[][2])
{
	clear();

	for (int n=0; n<size; n++)
		addValue(vectors[n]);
}

