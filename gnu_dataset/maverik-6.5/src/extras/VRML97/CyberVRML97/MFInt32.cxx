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
*	File:	MFInt32.cpp
*
******************************************************************/

#include "MFInt32.h"

#ifdef SUPPORT_JSAI

int			MFInt32::mInit = 0;

jclass		MFInt32::mFieldClassID = 0;
jclass		MFInt32::mConstFieldClassID = 0;

jmethodID	MFInt32::mInitMethodID = 0;
jmethodID	MFInt32::mSetValueMethodID = 0;
jmethodID	MFInt32::mGetValueMethodID = 0;
jmethodID	MFInt32::mSetNameMethodID = 0;

jmethodID	MFInt32::mConstInitMethodID = 0;
jmethodID	MFInt32::mConstSetValueMethodID = 0;
jmethodID	MFInt32::mConstGetValueMethodID = 0;
jmethodID	MFInt32::mConstSetNameMethodID = 0;

////////////////////////////////////////////////
//	MFInt32::setJavaIDs
////////////////////////////////////////////////

void MFInt32::setJavaIDs() {

	if (!mInit) {
		JNIEnv *jniEnv = getJniEnv();

		if (jniEnv == NULL)
			return;

		// Class IDs
		mFieldClassID		= jniEnv->FindClass("vrml/field/MFInt32");
		mConstFieldClassID	= jniEnv->FindClass("vrml/field/ConstMFInt32");

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
//	MFInt32::toJavaObject
////////////////////////////////////////////////

jobject MFInt32::toJavaObject(int bConstField) {
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
//	MFInt32::setValue
////////////////////////////////////////////////

void MFInt32::setValue(jobject field, int bConstField) {
}

////////////////////////////////////////////////
//	MFInt32::getValue
////////////////////////////////////////////////

void MFInt32::getValue(jobject field, int bConstField) {
}

#endif 





////////////////////////////////////////////////
//	MFInt32::setValue
////////////////////////////////////////////////

void MFInt32::setValue(MFInt32 *values)
{
	clear();

	int size = values->getSize();
	for (int n=0; n<size; n++) {
		addValue(values->get1Value(n));
	}
}

void MFInt32::setValue(MField *mfield)
{
	if (mfield->getType() == fieldTypeMFInt32)
		setValue((MFInt32 *)mfield);
}

void MFInt32::setValue(int size, int values[])
{
	clear();

	for (int n=0; n<size; n++)
		addValue(values[n]);
}


